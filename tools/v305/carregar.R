# Carrega definições e módulos reais sem executar importação ou curadoria.
monitora_v305_carregar <- function(script='R_monitora_campsav_alvo_global.R') {
 source('tests/helpers_v304.R');e<-monitora_v304_funcoes(script);a<-parse(script,keep.source=FALSE)
 load<-function(x){if(!is.call(x))return();if(identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]])&&as.character(x[[2]])=='monitora_relatorios_analiticos_resolver_pandoc'){eval(x,e);return()};if(length(x)>1)for(i in 2:length(x))load(x[[i]])};load(a[[1]])
 s<-readLines(script,warn=FALSE);start<-grep('^eval[(]parse[(]text=rawToChar[(]memDecompress',s);stopifnot(length(start)==1L)
 end<-start+which(grepl('envir=environment',s[(start+1L):length(s)],fixed=TRUE))[1L]
 code<-paste(s[start:end],collapse='\n');eval(parse(text=code),e)
 e$MONITORA_SCRIPT_VERSAO<-'3.0.5-rc01';e$MONITORA_SCRIPT_BUILD_ID<-'v3.0.5-rc01-20260929-r08';e
}
