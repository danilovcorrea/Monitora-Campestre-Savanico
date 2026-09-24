args<-commandArgs(TRUE)
source(file.path(args[1],'tests/helpers_test_funcoes.R'))
e<-monitora_test_funcoes(file.path(args[1],'R_monitora_campsav_alvo_global.R'))$env
h<-list.files(file.path(args[2],'manual_usuario'),pattern='\\.html$',full.names=TRUE)
stopifnot(length(h)==1L)
p<-sub('\\.html$','.pdf',h);tmp<-paste0(p,'.novo.pdf')
r<-e$monitora_relatorios_analiticos_chrome_print_isolado(h,tmp,browser='/usr/bin/google-chrome',timeout=180)
stopifnot(isTRUE(r$ok),file.exists(tmp));stopifnot(file.copy(tmp,p,overwrite=TRUE));unlink(tmp)
cat('Manual PDF atualizado somente a partir do HTML com caminhos revisados; análises não executadas.\n')
