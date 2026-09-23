source('tests/helpers_test_funcoes.R')
monitora_v302_carregar <- function(fonte='R_monitora_campsav_alvo_global.R') {
  a<-monitora_test_funcoes(fonte);e<-a$env
  visitar<-function(x) {
    if(!is.call(x))return(invisible(NULL))
    if(identical(x[[1]],quote(`<-`)) && is.symbol(x[[2]]) && identical(as.character(x[[2]]),'monitora_relatorios_analiticos_resolver_pandoc')){eval(x,e);return(invisible(NULL))}
    if(length(x)>1)for(i in 2:length(x))if(!identical(x[[i]],quote(expr=)))visitar(x[[i]])
  }
  visitar(a$arvore[[1]])
  a
}
