#!/usr/bin/env Rscript
# Contratos de cache: arquivos físicos de teste, nenhuma UI/pipeline/rede.
args <- commandArgs(TRUE)
out <- if(length(args))args[[1L]]else tempfile("v307_cache_")
dir.create(out, recursive=TRUE, showWarnings=FALSE)
source("tools/v307/cache.R",encoding="UTF-8")
source("tools/v307/justificativas.R",encoding="UTF-8")
stopifnot(requireNamespace("data.table",quietly=TRUE),requireNamespace("digest",quietly=TRUE))
cfg <- list(versao_modulo="2.5.4",validar_espacial=TRUE,raio_m=150)
results <- list()
test <- function(name, f) {
  t <- proc.time()[["elapsed"]]
  f()
  results[[length(results)+1L]] <<- list(test=name,status="PASS",seconds=proc.time()[["elapsed"]]-t)
  cat("PASS",name,"\n")
}
reject <- function(f, pattern="Cache") {
  err <- tryCatch({f();NULL},error=function(e)conditionMessage(e))
  stopifnot(is.character(err),length(err)==1L,grepl(pattern,err,ignore.case=TRUE))
}
valid <- function(...)isTRUE(monitora_v307_cache_validar(...)$ok)
base <- file.path(out,"origem","input")
dir.create(file.path(base,"linhagem"),recursive=TRUE)
writeLines(c("COLETA,valor","C1,a","C2,b"),file.path(base,"registros_corrig.csv"))
writeLines(c("evento_id,atributo","E1,valor"),file.path(base,"linhagem","ledger.csv"))
writeLines('{"schema":"teste"}',file.path(base,"linhagem","manifesto.json"))
legacy_signature <- function(man,config) {
  txt<-paste(c(paste0("script_versao_modulo=",config$versao_modulo),paste0("validar_espacial=",config$validar_espacial),paste0("raio=",config$raio_m),paste0(man$arquivo,"|",man$tamanho,"|",man$mtime,"|",man$md5)),collapse="\n")
  paste0("cache_",abs(sum(utf8ToInt(txt)))%%1000000000L)
}
make <- function(input=base,config=cfg,explicit=TRUE) {
  files <- monitora_v307_cache_arquivos(input,legado=TRUE)
  man <- data.frame(arquivo=files$arquivo,tamanho=files$bytes,mtime=rep("2026-11-11 10:00:00",nrow(files)),md5=unname(as.character(tools::md5sum(files$arquivo))),stringsAsFactors=FALSE)
  obj <- list(versao_cache="painel_cache_v2",manifesto_inputs=man,
    registros_corrig=data.table::data.table(COLETA=c("C1","C2"),valor=c("espécie","NA")),
    meta_xls=list(schema="teste",campos=data.table::data.table(nome="habito",tipo="select_one")),
    relatorios_pre=list(indices=data.frame(tipo="teste",n=1)),
    validacao_espacial_pre=list(validacao=data.frame(status="ok",raio=150)))
  if(explicit)obj$configuracao_espacial <- config
  obj$assinatura <- legacy_signature(man,config)
  obj
}
legacy <- make()
modern <- monitora_v307_cache_assinar(legacy,base,cfg)
test("novo_mesmo_input",function()stopifnot(valid(modern,base,cfg)))
test("novo_save_reopen",function(){p<-file.path(out,"cache.rds");saveRDS(modern,p);stopifnot(valid(readRDS(p),base,cfg))})
test("selfref_transitorio_sem_alterar_semantica",function(){x<-make();before<-unserialize(serialize(x$registros_corrig,NULL));data.table::setattr(x$registros_corrig,".internal.selfref",new("externalptr"));s<-monitora_v307_cache_assinar(x,base,cfg);h<-monitora_v307_cache_payload_hash(s);stopifnot(valid(s,base,cfg),valid(s,base,cfg),identical(h,monitora_v307_cache_payload_hash(s)),all(vapply(seq_len(ncol(before)),function(i)identical(before[[i]],s$registros_corrig[[i]]),logical(1L))));p<-file.path(out,"cache_selfref.rds");saveRDS(s,p);stopifnot(valid(readRDS(p),base,cfg))})
portable <- file.path(out,"raiz_portatil","input")
dir.create(portable,recursive=TRUE)
stopifnot(all(file.copy(list.files(base,full.names=TRUE),portable,recursive=TRUE)))
for(p in list.files(portable,recursive=TRUE,full.names=TRUE))Sys.setFileTime(p,Sys.time()+3600)
test("novo_raiz_portatil_mtime_diferente",function()stopifnot(valid(modern,portable,cfg)))
test("legado_explicito_sem_prova_payload_recusado",function()reject(function()monitora_v307_cache_validar(legacy,base,cfg),"regenere"))
test("legado_explicito_portatil_recusado",function()reject(function()monitora_v307_cache_validar(legacy,portable,cfg),"regenere"))
test("prova_memoria_usa_helper_comum",function(){x<-monitora_v307_cache_validar(modern,base,cfg);stopifnot(identical(x$memoria_sha256,monitora_v307_justificativas_memoria_sha256(modern$registros_corrig)),grepl("^[a-f0-9]{64}$",x$input_sha256),identical(x$brutos_sha256,""),identical(x$config_sha256,monitora_v307_cache_configuracao_sha256(cfg)))})
test("incremental_somente_corrig_linhagem_registra_prova",function(){MONITORA_EXEC_ID<<-"qa_cache_incremental";x<-monitora_v307_cache_validar(modern,base,cfg);monitora_v307_justificativas_registrar_cache_validado(x$memoria_sha256,x$input_sha256,x$config_sha256,validado=TRUE);p<-get("MONITORA_V307_CACHE_VALIDADO",.GlobalEnv);stopifnot(identical(p$exec_id,MONITORA_EXEC_ID),identical(p$input_sha256,x$input_sha256))})
test("config_hash_independente_modo",function(){a<-cfg;b<-cfg;a$modo<-"completo";b$modo<-"abrir_painel_cache";stopifnot(identical(monitora_v307_cache_configuracao_sha256(a),monitora_v307_cache_configuracao_sha256(b)))})
test("legado_real_sem_config_recusa_sem_reescrever",function(){x<-make(explicit=FALSE);before<-serialize(x,NULL);reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere");stopifnot(identical(before,serialize(x,NULL)))})
test("dados_alterados",function(){x<-modern;data.table::set(x$registros_corrig,1L,"valor","alterado");reject(function()monitora_v307_cache_validar(x,base,cfg),"dados/schema")})
modern <- monitora_v307_cache_assinar(make(),base,cfg)
test("legado_payload_alheio_recusado",function(){x<-make();data.table::set(x$registros_corrig,1L,"COLETA","COLETA_ALHEIA");reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("meta_xls_alterado",function(){x<-unserialize(serialize(modern,NULL));x$meta_xls$schema<-"alterado";reject(function()monitora_v307_cache_validar(x,base,cfg),"payload")})
test("relatorios_pre_alterados",function(){x<-unserialize(serialize(modern,NULL));x$relatorios_pre$indices$n<-999;reject(function()monitora_v307_cache_validar(x,base,cfg),"payload")})
test("validacao_espacial_pre_alterada",function(){x<-unserialize(serialize(modern,NULL));x$validacao_espacial_pre$validacao$status<-"outro";reject(function()monitora_v307_cache_validar(x,base,cfg),"payload")})
test("payload_hash_ausente",function(){x<-unserialize(serialize(modern,NULL));x$monitora_cache_v307$payload_sha256<-NULL;reject(function()monitora_v307_cache_validar(x,base,cfg),"prova")})
# A alteração por referência anterior não deve contaminar as outras fixtures.
modern <- monitora_v307_cache_assinar(make(),base,cfg)
test("novo_prova_ausente",function(){x<-modern;x$monitora_cache_v307<-NULL;reject(function()monitora_v307_cache_validar(x,base,cfg),"prova")})
test("novo_prova_incompleta",function(){x<-modern;x$monitora_cache_v307$inputs_sha256<-NULL;reject(function()monitora_v307_cache_validar(x,base,cfg),"prova")})
test("novo_sha_inputs_adulterado",function(){x<-modern;x$monitora_cache_v307$inputs_sha256<-paste(rep("0",64),collapse="");reject(function()monitora_v307_cache_validar(x,base,cfg),"SHA256")})
test("novo_chave_duplicada",function(){x<-modern;x$monitora_cache_v307$inputs<-c(x$monitora_cache_v307$inputs,x$monitora_cache_v307$inputs[1]);reject(function()monitora_v307_cache_validar(x,base,cfg),"duplicad")})
test("novo_traversal",function(){x<-modern;x$monitora_cache_v307$inputs[[1]]$path<-"../fora.csv";reject(function()monitora_v307_cache_validar(x,base,cfg),"traversal")})
test("novo_chave_absoluta",function(){x<-modern;x$monitora_cache_v307$inputs[[1]]$path<-"C:/fora.csv";reject(function()monitora_v307_cache_validar(x,base,cfg),"absolut")})
other_cfg <- cfg;other_cfg$raio_m <- 240
test("colisao_legada_150_240_comprovada",function()stopifnot(identical(legacy_signature(legacy$manifesto_inputs,cfg),legacy_signature(legacy$manifesto_inputs,other_cfg))))
test("colisao_legada_nao_autoriza_config_diferente",function()reject(function()monitora_v307_cache_validar(legacy,base,other_cfg),"regenere"))
test("novo_config_incompativel",function()reject(function()monitora_v307_cache_validar(modern,base,other_cfg),"configura"))
test("novo_modulo_incompativel",function(){c<-cfg;c$versao_modulo<-"outra";reject(function()monitora_v307_cache_validar(modern,base,c),"configura")})
test("novo_espacial_desligado",function(){c<-cfg;c$validar_espacial<-FALSE;reject(function()monitora_v307_cache_validar(modern,base,c),"configura")})
test("legado_manifesto_ausente",function(){x<-legacy;x$manifesto_inputs<-NULL;reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("legado_manifesto_incompleto",function(){x<-legacy;x$manifesto_inputs$mtime<-NULL;reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("legado_chave_duplicada",function(){x<-legacy;x$manifesto_inputs<-rbind(x$manifesto_inputs,x$manifesto_inputs[1,]);reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("legado_traversal",function(){x<-legacy;x$manifesto_inputs$arquivo[1]<-sub("/input/","/input/../input/",x$manifesto_inputs$arquivo[1],fixed=TRUE);reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("legado_assinatura_alterada",function(){x<-legacy;x$assinatura<-"cache_0";reject(function()monitora_v307_cache_validar(x,base,cfg),"regenere")})
test("input_fisico_diferente",function(){writeLines(c("COLETA,valor","C1,diferente"),file.path(portable,"registros_corrig.csv"));reject(function()monitora_v307_cache_validar(modern,portable,cfg),"inputs");reject(function()monitora_v307_cache_validar(legacy,portable,cfg),"regenere")})
test("input_fisico_ausente",function(){unlink(file.path(portable,"linhagem","ledger.csv"));reject(function()monitora_v307_cache_validar(modern,portable,cfg),"inputs");reject(function()monitora_v307_cache_validar(legacy,portable,cfg),"regenere")})
test("assinar_recusa_produtor_manifesto_obsoleto",function(){x<-make();x$manifesto_inputs$md5[1]<-paste(rep("0",32),collapse="");reject(function()monitora_v307_cache_assinar(x,base,cfg),"conteúdo")})
if(requireNamespace("jsonlite",quietly=TRUE))jsonlite::write_json(list(status="PASS_COMPONENTE_CACHE",ui=FALSE,pipeline=FALSE,tests=results),file.path(out,"TEST_CACHE.json"),pretty=TRUE,auto_unbox=TRUE)
cat("TEST_V307_CACHE_OK",length(results),"contratos; sem UI/pipeline.\n")
