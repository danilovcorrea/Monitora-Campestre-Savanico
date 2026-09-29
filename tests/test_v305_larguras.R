source('tools/v305/carregar.R');e<-monitora_v305_carregar();library(data.table)
base<-'/home/dlinux/Monitora_Dev_20260712/auditoria_relatorios_20260928';result<-list()
for(uc in c('PNI','FNB','PNCV','PNM')) {
 x<-readLines(file.path(base,paste0(uc,'_fonte.Rmd')),warn=FALSE);i<-1;titulo<-''
 while(i<=length(x)) {
  if(startsWith(x[i],'Table:'))titulo<-x[i]
  if(startsWith(x[i],'|') && i<length(x) && grepl('^\\|[-:| ]+\\|$',x[i+1])) {
   split<-function(s)trimws(strsplit(sub('\\|$','',sub('^\\|','',s)),'(?<!\\\\)\\|',perl=TRUE)[[1]])
   cab<-split(x[i]);j<-i+2;rows<-list();while(j<=length(x)&&startsWith(x[j],'|')){rows[[length(rows)+1]]<-split(x[j]);j<-j+1}
   if(length(rows)) {
    mat<-do.call(rbind,rows);z<-tryCatch(e$monitora_v305_larguras(cab,mat),error=function(err)conditionMessage(err))
    result[[length(result)+1]]<-data.table(uc,titulo,colunas=length(cab),ok=is.list(z),paisagem=if(is.list(z))z$paisagem else NA,erro=if(is.list(z))''else z)
   };i<-j
  }else i<-i+1
 }
}
y<-rbindlist(result);fwrite(y,'artifacts/v305/larguras.csv');print(y[ok==FALSE]);print(y[,.(tabelas=.N,paisagem=sum(paisagem)),by=uc]);stopifnot(all(y$ok))
