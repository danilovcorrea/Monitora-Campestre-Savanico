monitora_v304_funcoes <- function(path) {
 e<-new.env(parent=globalenv());a<-parse(path,keep.source=FALSE,encoding='UTF-8')
 visit<-function(x) {
  if(!is.call(x))return()
  op<-as.character(x[[1]])[1]
  if(op %in% c('<-','=') && length(x)>=3L && is.symbol(x[[2]])) {
   rhs<-x[[3]];name<-as.character(x[[2]])
   if(is.call(rhs) && identical(rhs[[1]],as.name("function"))) {eval(x,e);return()}
   if(is.symbol(rhs) && exists(as.character(rhs),e,inherits=FALSE) && is.function(e[[as.character(rhs)]])) {eval(x,e);return()}
  }
  if(length(x)>1)for(i in 2:length(x))visit(x[[i]])
 }
 visit(a[[1]])
 e$MONITORA_COL_ROW_ID<-'MONITORA_ROW_ID';e$MONITORA_COL_UUID_REGISTRO_CANONICO<-'MONITORA_UUID_REGISTRO_CANONICO'
 e$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS<-character();e$MONITORA_PERF_ENABLED<-FALSE
 e$MONITORA_AUDITORIA_SEMANTICA_CORRECOES_COMPLETA<-FALSE
 for(n in c('.MONITORA_CONTRATO_UNICO_CACHE','.MONITORA_CONTRATO_MOVIMENTO_CACHE','.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE','.MONITORA_FECHAMENTO_HIERARQUICO_CACHE','.MONITORA_PUBLICACAO_AE_CACHE_CONTRATO','.MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS','.MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL'))assign(n,new.env(parent=emptyenv()),e)
 e
}
