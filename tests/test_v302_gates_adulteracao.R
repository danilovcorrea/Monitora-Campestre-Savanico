source('tests/helpers_v302.R');e<-monitora_v302_carregar()$env
p<-normalizePath('artifacts/v302_numeracao/render_1');aud<-data.table::fread(file.path(p,'auditoria_numeracao_ensaio_1.csv'),colClasses=c(numero='character'),na.strings='')
a<-file.path(p,'ensaio_1.html');doc<-xml2::read_html(a);labels<-xml2::xml_find_all(doc,".//p[@class='monitora-legenda-tabela']");stopifnot(length(labels)==3L)
xml2::xml_set_text(labels[[2]],'Tabela 99 — Legenda adulterada');tmp<-file.path(p,'adulterado.html');xml2::write_html(doc,tmp)
erro<-tryCatch({e$monitora_relatorios_analiticos_auditar_numeracao_formato(tmp,aud);''},error=conditionMessage)
stopifnot(grepl('divergência em tabela',erro));unlink(tmp)
# A rejected HTML must not survive or become a PDF. Inject only the gate failure, keep the real renderer.
e$monitora_relatorios_analiticos_auditar_numeracao_formato<-function(...)stop('falha editorial injetada')
x<-c('---','pagetitle: "Gate"','---','','# Dados',e$monitora_relatorios_analiticos_kable(data.frame(A=1),id='estado-atual'))
r<-e$monitora_relatorios_analiticos_renderizar(x,'gate_falha',p,file.path(p,'teste.css'),c('rmd','md','html','pdf'))
stopifnot(!file.exists(file.path(p,'gate_falha.html')),!file.exists(file.path(p,'gate_falha.pdf')),nrow(attr(r,'erros_renderizacao'))>=1L)
cat('PASS: legenda adulterada rejeitada; HTML reprovado removido; PDF não produzido a partir de HTML reprovado.\n')
