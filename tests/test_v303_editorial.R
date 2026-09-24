# Reutiliza os 256 cenários e gates editoriais sem o gate de escopo específico da v3.0.2.
t<-parse('tests/test_v302_numeracao.R')
for(expr in t) {
 if(is.call(expr)&&identical(expr[[1]],quote(`<-`))&&identical(expr[[2]],quote(b)))break
 eval(expr,.GlobalEnv)
}
b<-monitora_test_funcoes('monitora_campsav_alvo_global_v3.0.2.R')
stopifnot(identical(e$monitora_validados_schema_embutido(),b$env$monitora_validados_schema_embutido()))
# Gate estrutural: mudanças adicionais em literais são apenas regras físicas explícitas.
limpar<-function(x){if(is.character(x))return(rep('<literal>',length(x)));if(is.call(x)){for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[i]<-list(limpar(x[[i]]))};x}
common<-intersect(a$funcoes,b$funcoes)
changed<-common[!vapply(common,function(n)identical(limpar(body(e[[n]])),limpar(body(b$env[[n]])))&&identical(formals(e[[n]]),formals(b$env[[n]])),logical(1))]
allowed<-c('monitora_stat_adicionar_caption_painel','monitora_qfield_recortar_mbtiles','monitora_cache_gerar_relatorios_pos_se_preciso','monitora_doc_render_rmd','monitora_perf_registrar_checkpoint','monitora_qfield_gerar','monitora_relatorios_comparar_pre_pos_correcoes','monitora_relatorios_analiticos_chrome_print_isolado',
'monitora_relatorio_validacao_consolidado_gerar','monitora_oraculo_resumo_ocorrencias_localizar','monitora_planilha_importacao_sismonitora_gerar','monitora_output_classificar_arquivo_raiz','monitora_output_destino_correcao','monitora_output_escrever_indice_produtos','monitora_output_organizar_produtos','monitora_relatorios_analiticos_destino_fisico','monitora_relatorios_analiticos_caminho_figura')
print(changed);stopifnot(setequal(changed,allowed))
s<-readLines('R_monitora_campsav_alvo_global.R',warn=FALSE);bytes<-file.info('R_monitora_campsav_alvo_global.R')$size
stopifnot(bytes<5000000,bytes+length(s)<5000000)
cat('PASS: numeração, contrato129 e cálculo preservados; tamanho LF/CRLF abaixo de 5 MB.\n')
