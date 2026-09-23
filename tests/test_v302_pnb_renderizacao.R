source('tests/helpers_v302.R');e<-monitora_v302_carregar()$env
raiz<-normalizePath('artifacts/v302_numeracao',mustWork=TRUE)
fonte<-Sys.getenv('MONITORA_QA_PNB_RELATORIOS',unset='')
if(!nzchar(fonte)||!dir.exists(fonte))stop('Defina MONITORA_QA_PNB_RELATORIOS para a pasta de relatórios de origem.')
ids_sint<-c('esforco-formacao','esforco-fluxos','esforco-percentuais','estado-atual','estado-prioritario','calendario','recomendacoes')
ids_det<-c('metodos-perguntas','robustez','esforco-formacao','continuidade','esforco-fluxos','esforco-percentuais','estado-atual','herbaceas-lenhosas','nativas','exoticas','secas-mortas','material','achados-temporais','composicao','calendario','revisitas','fogo-fonte','fogo-cobertura','fogo-elegibilidade','fogo-hipoteses','fogo-registros','fogo-combustivel','clima-anomalias','clima-associacoes','clima-elegibilidade','clima-diagnosticos','multivariada-recorte','multivariada-ajustes','hipoteses-gestao','contexto-impactos','recomendacoes','rastreabilidade')
for(tipo in c('sintetico','detalhado')) {
  dir<-file.path(raiz,paste0('pnb_',tipo));dir.create(dir,showWarnings=FALSE)
  for(sub in c('figuras','recursos')) {
    destino<-file.path(dir,sub)
    if(!dir.exists(destino))stopifnot(file.copy(file.path(fonte,sub),dir,recursive=TRUE))
  }
  css<-list.files(fonte,'[.]css$',full.names=TRUE)[1];stopifnot(file.copy(css,dir,overwrite=TRUE))
  cat('\ncaption { caption-side:top; text-align:left; font-weight:600; break-after:avoid; page-break-after:avoid; }\n',file=file.path(dir,basename(css)),append=TRUE)
  arq<-list.files(fonte,paste0('relatorio_analitico_',tipo,'.*[.]Rmd$'),full.names=TRUE)
  x<-readLines(arq,warn=FALSE,encoding='UTF-8')
  # Reuse the audited analytical content and figures, removing only its old index/anchors.
  a<-which(x=='<div class="monitora-indice">');b<-which(x=='</div>' & seq_along(x)>a)[1]
  x<-x[-seq.int(a,b)]
  x<-sub(' \\{#monitora-secao-[^}]+\\}$','',x)
  x<-sub('^### Resultados com o esforço','#### Resultados com o esforço',x)
  ids<-if(tipo=='sintetico')ids_sint else ids_det
  linhas<-which(grepl('^\\|[ :|-]+\\|[[:space:]]*$',x))-1L
  stopifnot(length(linhas)==length(ids))
  catlg<-e$monitora_relatorios_analiticos_catalogo_tabelas()
  for(j in rev(seq_along(linhas))) {
    id<-ids[j];marc<-c(paste0('<!-- monitora-tabela ',id,' incluida -->'),'',paste0('Table: ',catlg[[id]]),'')
    x<-append(x,marc,after=linhas[j]-1L)
  }
  # Assert that caption insertion changed none of the analytical cells or rows.
  antes<-readLines(arq,warn=FALSE,encoding='UTF-8')
  stopifnot(identical(antes[startsWith(antes,'|')],x[startsWith(x,'|')]))
  writeLines(x,file.path(dir,'entrada_editorial.Rmd'))
  r<-e$monitora_relatorios_analiticos_renderizar(x,paste0('pnb_',tipo,'_v302'),dir,file.path(dir,basename(css)),c('rmd','md','html','docx','pdf'),navegador_pdf=e$monitora_relatorios_analiticos_resolver_navegador(),versao_editorial=if(tipo=='sintetico')'sintético' else 'detalhado')
  saveRDS(r,file.path(dir,'resultado.rds'))
  print(r[,c('formato','existe','tamanho_bytes')]);print(attr(r,'erros_renderizacao'))
  stopifnot(all(r$existe),all(r$tamanho_bytes>0),nrow(attr(r,'erros_renderizacao'))==0)
}
cat('PASS: PNB sintético/detalhado nos cinco formatos, sem recalcular análises ou alterar dados.\n')
