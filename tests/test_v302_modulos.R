source('tests/helpers_test_funcoes.R')
b<-parse('artifacts/v302_numeracao/modulos_base.R',keep.source=FALSE)
c<-parse('artifacts/v302_numeracao/modulos_candidata.R',keep.source=FALSE)
# Every embedded analytical function remains identical after stripping the new editorial IDs,
# apart from the filter that preserves the pre-existing omission of the methods comparison.
limpar<-function(x) {
 if(!is.call(x))return(x)
 if(identical(x[[1]],quote(monitora_relatorios_analiticos_kable))) {
  nm<-names(x);if(!is.null(nm))x<-x[is.na(nm)|nm!='id']
 }
 if(length(x)>1)for(i in 2:length(x))if(!identical(x[[i]],quote(expr=)))x[i]<-list(limpar(x[[i]]))
 x
}
stopifnot(length(b)==length(c))
nomes<-vapply(b,function(x)as.character(x[[2]]),character(1))
for(i in seq_along(b)) {
 if(nomes[i]=='monitora_v30_editorial')next
 stopifnot(identical(limpar(b[[i]]),limpar(c[[i]])))
}
e<-monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$env
calls<-0L
visitar<-function(x) {
 if(!is.call(x))return(invisible(NULL))
 if(identical(x[[1]],quote(monitora_relatorios_analiticos_kable))) {
  stopifnot('id' %in% names(x),as.character(x[['id']]) %in% names(e$monitora_relatorios_analiticos_catalogo_tabelas()))
  calls<<-calls+1L
 }
 if(length(x)>1)for(i in 2:length(x))if(!identical(x[[i]],quote(expr=)))visitar(x[[i]])
}
invisible(lapply(c,visitar));stopifnot(calls==18L)
invisible(lapply(monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$arvore,visitar));stopifnot(calls==43L)
for(x in c)eval(x,e)
block<-e$monitora_relatorios_analiticos_kable(data.frame(Metodo='A',Limite='B'),id='multivariada-metodos')
stopifnot(!any(grepl('monitora-tabela multivariada-metodos',e$monitora_v30_editorial(c(block,'Texto restante'),'multivariada'))))
cat('PASS: 43 chamadas de tabelas com identidade/título; AST dos cálculos dos módulos idêntico; filtro editorial preservado.\n')
