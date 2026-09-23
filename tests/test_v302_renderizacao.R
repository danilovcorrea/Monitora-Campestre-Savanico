source('tests/helpers_v302.R');e<-monitora_v302_carregar()$env
out<-normalizePath('artifacts/v302_numeracao',mustWork=TRUE)
css<-file.path(out,'teste.css');writeLines('table{border-collapse:collapse;width:100%}td,th{border:1px solid #bbb;padding:4px;font-size:10pt}caption{text-align:left;font-weight:bold}img{max-width:100%}h1{font-size:18pt}h2{font-size:14pt}',css)
e$MONITORA_SCRIPT_VERSAO<-'3.0.2-rc01';e$MONITORA_SCRIPT_BUILD_ID<-'v3.0.2-rc01-20260923-r01'
# Preserve cover behavior used in real reports.
logo<-e$monitora_relatorios_analiticos_materializar_logos
for(i in 1:2){
 dir<-file.path(out,paste0('render_',i));dir.create(dir,showWarnings=FALSE)
 png::writePNG(array(0.5,dim=c(200,300,3)),file.path(dir,'grafico.png'))
 file.copy(css,file.path(dir,'teste.css'),overwrite=TRUE)
 png::writePNG(array(1,dim=c(249,176,3)),file.path(dir,'capa.png'))
 capa<-c('<div class="cover">','<div class="capa-docx-fonte">capa.png</div>','<div class="cover-content">','<h1>Relatório analítico detalhado</h1>','<div class="uc">Unidade de Conservação: UC de teste</div>','</div>','</div>')
 tabela<-data.frame(Prioridade='Alta',Natureza='Campo',Recomendação='Conferir dados',Fundamento='Dados disponíveis')
 ampla<-as.data.frame(setNames(rep(list(c('1','2')),9),paste('Campo',1:9)))
 x<-c('---','pagetitle: "Ensaio editorial"','lang: pt-BR','---','',capa,'# Escopo','## Dados disponíveis',
   e$monitora_relatorios_analiticos_kable(tabela,id='recomendacoes'),
   e$monitora_relatorios_analiticos_kable(ampla,id='achados-temporais'),
   if(i==1L)e$monitora_relatorios_analiticos_figura('grafico.png','Gráfico de teste')else 'Sem figura elegível.',
   '# Análises complementares',
   if(i==1L)c('## Fogo',e$monitora_relatorios_analiticos_kable(data.frame(Status='Dados insuficientes'),id='fogo-elegibilidade'))else 'Fogo e clima indisponíveis.',
   '# Limitações','Não há inferência temporal com campanha única.')
 r<-e$monitora_relatorios_analiticos_renderizar(x,paste0('ensaio_',i),dir,file.path(dir,'teste.css'),c('rmd','md','html','docx','pdf'),navegador_pdf=e$monitora_relatorios_analiticos_resolver_navegador())
 saveRDS(r,file.path(dir,'resultado.rds'));print(r[,c('formato','existe','tamanho_bytes')]);print(attr(r,'erros_renderizacao'))
 stopifnot(all(r$existe),all(r$tamanho_bytes>0),nrow(attr(r,'erros_renderizacao'))==0)
}
cat('PASS: fontes/HTML/DOCX/PDF com tabelas nativas, omissões e figuras opcionais.\n')
