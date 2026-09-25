library(data.table);source('tests/helpers_v304.R');e<-monitora_v304_funcoes('R_monitora_campsav_alvo_global.R');e$monitora_fwrite<-function(x,file,...){data.table::fwrite(x,file,...)}
d<-data.table(COLETA='1',UC='teste',EA='EA',UA='UA',CICLO='c',CAMPANHA='c',`amostragem/registro/forma_vida_nativa`='desconhecida')
p<-tempfile();e$monitora_relatorio_formas_vida_token_gravar(d,p,'desconhecida','formas_vida_desconhecida','Teste')
stopifnot(file.exists(file.path(p,'desconhecida_por_unidade.csv')),!file.exists(file.path(p,'resumo_formas_vida_desconhecida_por_unidade.csv')))
# Executa as expressões de nomes reais extraídas do script, com os contextos longos.
a<-parse('R_monitora_campsav_alvo_global.R',keep.source=FALSE);hits<-list()
visit<-function(x){if(!is.call(x))return();if(identical(x[[1]],as.name('paste0'))&&length(x)>=2L&&is.character(x[[2]])&&x[[2]]%in%c('aud_pipes_','aud_pipe_','p_','ordem_contrato_','resumo_pipes_'))hits[[length(hits)+1L]]<<-x;if(length(x)>1)for(i in 2:length(x))visit(x[[i]])};visit(a[[1]])
z<-new.env(parent=globalenv());z$produto<-'registros_importados_operacional_pre_painel_csv';z$sufixo<-z$produto;z$contexto<-'pos_export_pre_analises_registros_corrig';z$exec_id<-z$exec_id_pers<-z$exec_id_chr<-'20260925_180000';v<-character()
for(x in hits){if(x[[2]]=='resumo_pipes_')z$sufixo<-'pos_export_registros_corrig';if(x[[2]]=='ordem_contrato_')z$sufixo<-'checkpoint2_pos_tokenizacao';if(x[[2]]=='aud_pipe_')z$sufixo<-z$produto;y<-try(eval(x,z),silent=TRUE);if(!inherits(y,'try-error')&&grepl('[.]csv$',y))v<-c(v,y)}
stopifnot(any(startsWith(v,'aud_pipes_importados_')),any(startsWith(v,'aud_pipe_importados_')),any(startsWith(v,'p_pre_analises_')),any(startsWith(v,'ordem_contrato_pos_tokens_')),any(startsWith(v,'resumo_pipes_pos_export_')))
stopifnot(!any(grepl('registros_importados_operacional|pos_export_pre_analises_registros_corrig|checkpoint2_pos_tokenizacao',v)))
cat('PASS: construtores reais de caminhos dinâmicos geram os nomes curtos previstos.\n')
