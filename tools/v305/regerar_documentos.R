# Uso: Rscript tools/v305/regerar_documentos.R REPOSITORIO DESTINO ORIGEM
# ORIGEM é preservada; DESTINO deve conter os recursos e resultados já existentes.
args<-commandArgs(TRUE);repo<-normalizePath(args[1]);pasta<-normalizePath(args[2]);fonte<-normalizePath(args[3]);setwd(repo)
suppressPackageStartupMessages(library(data.table));source('tools/v305/carregar.R');e<-monitora_v305_carregar()
for(base in c('analitico_sintetico','analitico_detalhado')) {
 p<-file.path(fonte,paste0(base,'.Rmd'));x<-readLines(p,warn=FALSE,encoding='UTF-8')
 gloss<-which(grepl('^# [0-9]+ Glossário',x));if(length(gloss)) {
   x<-head(x,gloss[1]-1L)
   while(length(x) && (!nzchar(trimws(tail(x,1))) || trimws(tail(x,1))=='<div class="page-break"></div>'))x<-head(x,-1L)
 }
 start<-which(x=='<div class="monitora-indice">');stopifnot(length(start)==1);end<-which(seq_along(x)>start & x=='</div>')[1];x<-x[-seq(start,end)]
 x<-x[!grepl('^\\*\\*Siglas nesta leitura:\\*\\*',x)]
 x<-x[!startsWith(x,'A presença registrada de plantas exóticas, isoladamente, não demonstra invasão biológica.')]
 x<-x[!grepl('reúne a síntese visual dos resultados temporais discutidos nesta seção',x,fixed=TRUE)]
 x<-sub('^(#{1,6}) [0-9]+([.][0-9]+)* (.*) \\{#monitora-secao-[^}]+\\}$','\\1 \\3',x)
 x<-sub('^\\[\\]\\{#monitora-tab-([^}]+)\\}$','<!-- monitora-tabela \\1 incluida -->',x)
 x<-x[!grepl('^\\[\\]\\{#monitora-fig-',x)]
 x<-gsub('(<figcaption[^>]*>)Figura [0-9]+[.] ','\\1',x)
 x<-gsub('\\[Tabela [0-9]+\\]\\(#monitora-tab-([^)]*)\\)','[[tabela:\\1]]',x)
 x<-gsub('\\[Figura [0-9]+\\]\\(#monitora-(fig-[^)]*)\\)','[[figura:\\1]]',x)
 catg<-e$monitora_relatorios_analiticos_catalogo_tabelas()
 for(i in grep('^<!-- monitora-tabela .* incluida -->',x)) {
  id<-sub('^<!-- monitora-tabela (.*) incluida -->$','\\1',x[i]);j<-which(seq_along(x)>i & startsWith(x,'Table:'))[1];stopifnot(length(catg[[id]])==1);x[j]<-paste0('Table: ',catg[[id]])
 }

 # Aplicar os rótulos editoriais também às tabelas já materializadas.
 for(i in which(startsWith(x,'|'))) {
  if(i==length(x)||!grepl('^\\|[-:| ]+\\|$',x[i+1]))next
  z<-strsplit(x[i],'|',fixed=TRUE)[[1]];v<-trimws(z);z<-ifelse(v%in%names(setNames(v,v)),z,z)
  zz<-e$monitora_v305_rotulo_coluna(v);z[v!=zz]<-paste0(' ',zz[v!=zz],' ');x[i]<-paste(z,collapse='|');if(!endsWith(x[i],'|'))x[i]<-paste0(x[i],'|')
 }
 x<-gsub('87\\\\[2614:VPOSDM\\\\]2.0.CO;2','87%5B2614:VPOSDM%5D2.0.CO;2',x,fixed=TRUE)
 for(i in which(grepl('2614:VPOSDM',x,fixed=TRUE)))x[i]<-paste0(sub(' DOI:.*$','',x[i]),' DOI: [10.1890/0012-9658(2006)87[2614:VPOSDM]2.0.CO;2](https://doi.org/10.1890/0012-9658%282006%2987%5B2614:VPOSDM%5D2.0.CO%3B2).')
 x<-gsub('A tabela descreve campanhas','A [[tabela:fogo-cobertura]] descreve campanhas',x,fixed=TRUE)
 # O texto já calculado não exige recalcular modelos para retirar motivos repetidos.
 for(i in which(grepl('**Inferência não estimável nesta execução.** ',x,fixed=TRUE))) {
  prefix<-'**Inferência não estimável nesta execução.** ';partes<-strsplit(x[i],' NE não equivale',fixed=TRUE)[[1]]
  if(length(partes)==2L)x[i]<-paste0(prefix,e$monitora_v305_motivos(sub(prefix,'',partes[1],fixed=TRUE)),' NE não equivale',partes[2])
 }
 cm<-file.path(pasta,'calendario_modelos_por_coleta.csv')
 if(file.exists(cm)) {
  calendario<-e$monitora_v305_calendario(fread(cm))
  x<-gsub('Calendário: [^<]*',calendario,x)
 }
 x<-gsub('Relatório analítico [0-9]+[.][0-9]+[.][0-9]+(-rc[0-9]+)?',paste0('Relatório analítico ',e$MONITORA_SCRIPT_VERSAO),x)
 x<-gsub('Versão do script: [^<]*',paste0('Versão do script: ',e$MONITORA_SCRIPT_VERSAO),x)
 x<-gsub('Build: [^<]*',paste0('Build: ',e$MONITORA_SCRIPT_BUILD_ID),x)
 x<-sub('^Gerado em: .*',paste0('Gerado em: ',format(Sys.time(),'%d/%m/%Y %H:%M %Z'),'</div>'),x)
 # A capa do Word é uma imagem: regenerá-la com os mesmos metadados do texto.
 pegar<-function(padrao){z<-grep(padrao,x,value=TRUE,perl=TRUE);stopifnot(length(z)>=1L);regmatches(z[1],regexec(padrao,z[1],perl=TRUE))[[1]][2]}
 tipo<-if(base=='analitico_sintetico')'sintético'else'detalhado'
 e$monitora_relatorios_analiticos_materializar_capa_docx(
   destino=file.path(pasta,'recursos',paste0('capa_docx_',sub('analitico_','',base),'.png')),
   tipo=tipo,uc=pegar('^.*Unidade de Conservação: ([^<]+).*$'),
   periodo=pegar('^.*Série analisada: ([^<]+).*$'),
   status_validacao=pegar('^.*<span class="status">([^<]+).*$'),
   gerado_em=pegar('^.*Gerado em: ([^<]+).*$'),
   logos_relatorio=c(icmbio=pegar('^.*<img class="logo-icmbio" src="([^"]+)".*$'),monitora_cbc=pegar('^.*<img class="logo-monitora-cbc" src="([^"]+)".*$')),
   dir_relatorio=pasta)
 prod<-e$monitora_relatorios_analiticos_renderizar(x,base,pasta,file.path(pasta,'estilo_relatorio_analitico.css'),c('rmd','md','html','docx','pdf'),navegador_pdf=e$monitora_relatorios_analiticos_resolver_navegador(),versao_editorial=if(base=='analitico_sintetico')'sintético'else'detalhado')
 erros<-attr(prod,'erros_renderizacao');print(erros);stopifnot(nrow(erros)==0)
}
cat('PASS: cinco formatos regenerados pelo renderer de produção, sem recalcular ciência.\n')
