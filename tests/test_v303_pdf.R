args<-commandArgs(TRUE)
source('tests/helpers_test_funcoes.R');e<-monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$env
out<-if(length(args))args[1] else 'artifacts/v303/pdf';dir.create(out,recursive=TRUE,showWarnings=FALSE)
nav<-e$monitora_relatorios_analiticos_resolver_navegador();stopifnot(nav$ok)
html<-file.path(out,'ensaio.html');writeLines('<html><head><link rel="icon" href="data:,"><meta charset="UTF-8"></head><body><h1>Homologação PDF 3.0.3</h1><p>Documento de teste.</p></body></html>',html)
z<-e$monitora_relatorios_analiticos_chrome_print_isolado(html,file.path(out,'ensaio.pdf'),nav$caminho,timeout=30)
stopifnot(z$ok,z$tamanho_bytes>1000)
# Exercita o caminho de validação/manual completo, incluindo publicação de candidato.
e$monitora_posix_diagnosticar_etapa<-function(etapa,expr)force(expr)
rmd<-file.path(out,'documento.Rmd');writeLines(c('---','title: "Teste documental"','output: html_document','---','','# Teste','Conteúdo de homologação.'),rmd)
z<-e$monitora_doc_render_rmd(rmd,tipo_documento='teste')
stopifnot(setequal(tools::file_ext(z),c('html','pdf')))
# PDF antigo não é aceito se a nova renderização falhar.
e$monitora_relatorios_analiticos_chrome_print_isolado<-function(...)list(ok=FALSE,mensagem='timeout simulado de teste',duracao_seg=1)
z<-e$monitora_doc_render_rmd(rmd,tipo_documento='falha_simulada')
stopifnot(!'pdf'%in%tools::file_ext(z),file.exists(file.path(out,'documento.pdf')),
 any(grepl('timeout simulado',readLines(file.path(out,'PDF_NAO_GERADO_documento.txt')))))
cat('PASS: PDF isolado, renderização documental, causa específica e rejeição de PDF antigo.\n')
