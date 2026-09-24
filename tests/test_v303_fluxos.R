suppressPackageStartupMessages(library(data.table))
source('tests/helpers_test_funcoes.R')
e <- monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$env
out <- 'artifacts/v303/testes'; unlink(out, recursive=TRUE); dir.create(out, recursive=TRUE, showWarnings=FALSE)
e$monitora_fwrite <- function(x,file,...) fwrite(x,file,...)
e$monitora_log_registrar_evento <- function(...) NULL
for (layout in c('antigo','organizado','misto')) {
 root <- file.path(out,layout); base <- file.path(root,'correcoes_campos'); dir.create(base,recursive=TRUE,showWarnings=FALSE)
 pre <- if(layout=='antigo')file.path(base,'relatorios_pre_painel') else file.path(root,'02_painel_correcoes/relatorios_apoio_tematicos/pre_painel')
 pos <- if(layout=='organizado')file.path(root,'02_painel_correcoes/relatorios_apoio_tematicos/pos_painel') else file.path(base,'relatorios_pos_correcoes')
 dir.create(pre,recursive=TRUE,showWarnings=FALSE);dir.create(pos,recursive=TRUE,showWarnings=FALSE)
 fwrite(data.table(uuid_registro='abc',UA='UA001',Longitude=-50),file.path(pre,'registros_teste.csv'))
 fwrite(data.table(uuid_registro='abc',UA='UA002',Longitude=-51),file.path(pos,'registros_teste.csv'))
 x <- e$monitora_relatorios_comparar_pre_pos_correcoes(base)
 stopifnot(nrow(x)==1L,file.exists(file.path(base,'comparacao_relatorios_pre_pos_correcoes.csv')))
 det <- fread(file.path(base,'cmp_campos_alterados_pre_pos_correcoes.csv'))
 stopifnot(setequal(det$campo_alterado,c('UA','Longitude')))
 if(layout=='misto') {
  legado<-file.path(base,'relatorios_pre_painel');dir.create(legado,showWarnings=FALSE)
  file.copy(file.path(pre,'registros_teste.csv'),legado,overwrite=TRUE)
  stopifnot(length(e$monitora_relatorios_resolver_fase(base,'pre_painel'))==1)
  fwrite(data.table(uuid_registro='abc',UA='divergente'),file.path(legado,'registros_teste.csv'))
  erro<-tryCatch(e$monitora_relatorios_resolver_fase(base,'pre_painel'),error=conditionMessage)
  stopifnot(grepl('fontes divergentes',erro))
 }
}
base<-file.path(out,'ausente');dir.create(base,showWarnings=FALSE)
msg<-capture.output(withCallingHandlers(e$monitora_relatorios_comparar_pre_pos_correcoes(base),message=function(m){cat(conditionMessage(m));invokeRestart('muffleMessage')}))
stopifnot(any(grepl('não gerada',msg)))
# Avisos persistidos e deduplicados sem impedir propagação ao chamador.
e$MONITORA_AVISOS_ESTADO <- new.env();st<-e$MONITORA_AVISOS_ESTADO
st$chaves<-character();st$itens<-list();st$etapa<-'teste';st$id<-'test'
e$MONITORA_LOG_DIR<-out;e$MONITORA_EXEC_ID<-'test'
vistos<-0L
withCallingHandlers(withCallingHandlers({warning('repetido');warning('repetido')},warning=e$monitora_aviso_registrar),warning=function(w){vistos<<-vistos+1L;invokeRestart('muffleWarning')})
w<-read.csv(file.path(out,'avisos_execucao_test.csv'));stopifnot(nrow(w)==1L,w$ocorrencias==2L,vistos==2L)
# Nome invertido emite orientação e não move insumos.
dir.create(file.path(out,'input_qfield'),showWarnings=FALSE);writeLines('x',file.path(out,'input_qfield/a.mbtiles'))
msg<-capture.output(withCallingHandlers(e$monitora_qfield_orientar_entrada(out,file.path(out,'qfield_input')),message=function(m){cat(conditionMessage(m));invokeRestart('muffleMessage')}))
stopifnot(any(grepl('input_qfield',msg)),any(grepl('gere novamente',msg)),file.exists(file.path(out,'input_qfield/a.mbtiles')))
# Módulos matemáticos, curadoria, integridade de snapshot e cache permanecem iguais.
a<-parse('artifacts/v303/modulos_base.R');b<-parse('artifacts/v303/modulos_candidata.R')
nomes<-vapply(a,function(x)as.character(x[[2]]),character(1));stopifnot(length(a)==length(b))
for(i in seq_along(a))if(!nomes[i]%in%c('monitora_fogo_api','monitora_fogo_obter_snapshot'))stopifnot(identical(a[[i]],b[[i]]))
cat('PASS: comparação três layouts, alteração espacial, ambiguidade bloqueada, ausência explícita, avisos, QField e AST dos demais módulos preservados.\n')
# Um mesmo produto com nome longo no pré e compacto no pós deve ser comparado como um arquivo.
root<-file.path(out,'nomes_mistos');base<-file.path(root,'correcoes_campos')
pre<-file.path(root,'02_painel_correcoes/relatorios_apoio_tematicos/pre_painel')
pos<-file.path(root,'02_painel_correcoes/ap/pos_painel')
dir.create(pre,recursive=TRUE);dir.create(pos,recursive=TRUE);dir.create(base,recursive=TRUE)
fwrite(data.table(uuid_registro='abc',UA='UA001'),file.path(pre,'registros_formas_vida_exoticas_sem_forma_detalhada.csv'))
fwrite(data.table(uuid_registro='abc',UA='UA002'),file.path(pos,'registros_exoticas_sem_forma.csv'))
x<-e$monitora_relatorios_comparar_pre_pos_correcoes(base);stopifnot(nrow(x)==1L)
det<-fread(file.path(base,'cmp_campos_alterados_pre_pos_correcoes.csv'));stopifnot(identical(det$campo_alterado,'UA'))
cat('PASS: identidade lógica preservada ao comparar nomes físicos longos/curtos.\n')
