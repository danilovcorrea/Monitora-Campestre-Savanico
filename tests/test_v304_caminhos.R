source('tests/helpers_v304.R');library(data.table)
a<-monitora_v304_funcoes('monitora_campsav_alvo_global_v3.0.3.R');b<-monitora_v304_funcoes('R_monitora_campsav_alvo_global.R')
m<-jsonlite::fromJSON('artifacts/v304/caminhos.json');n<-names(m$nomes)
for(i in seq_along(n)){
 old<-paste0(n[i],'TESTE.csv');new<-paste0(m$nomes[[i]],'TESTE.csv')
 x<-a$monitora_nome_arquivo_logico(old);y<-b$monitora_nome_arquivo_logico(new)
 if(!identical(x,y))stop(old,' -> ',new,'\n',x,' != ',y)
 if(!identical(b$monitora_nome_arquivo_logico(old),x))stop("LEGADO: ",old," -> ",b$monitora_nome_arquivo_logico(old)," esperado ",x)
}
cat('PASS: leitura lógica dos',length(n),'prefixos novos e legados preservada\n')
args<-commandArgs(TRUE)
if(length(args)) {
 dir<-tempfile('ocorrencias_');dir.create(dir);file.copy(list.files(args[1],full.names=TRUE),dir)
 todos<-fread(file.path(dir,'registros_oc_pos_painel.csv'),colClasses='character',na.strings=NULL)
 z<-b$monitora_diag_validar_ocorrencias_materializadas(todos,'pos_painel',dir)
 stopifnot(z$status=='ok_identidades_e_contagens_exatas',z$n_arquivos_tipos==15L)
 cat('PASS: 15 arquivos de ocorrências materializados e identidades exatas preservadas após abreviação.\n')
}
if(file.exists('artifacts/v304/nomes_legados.csv')) {
 pairs<-fread('artifacts/v304/nomes_legados.csv');pairs<-unique(pairs)
 for(i in seq_len(nrow(pairs))){x<-a$monitora_nome_arquivo_logico(pairs$antes[i]);y<-b$monitora_nome_arquivo_logico(pairs$depois[i]);z<-b$monitora_nome_arquivo_logico(pairs$antes[i]);if(!identical(x,y)||!identical(x,z))stop('Compatibilidade: ',pairs$antes[i],' -> ',pairs$depois[i],'\n',x,' != ',y,' legado ',z)}
 cat('PASS: nomes lógicos preservados para',nrow(pairs),'arquivos reais legados.\n')
}
