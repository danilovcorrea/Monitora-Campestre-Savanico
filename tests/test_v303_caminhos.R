source('tests/helpers_test_funcoes.R')
e<-monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$env
b<-monitora_test_funcoes('artifacts/v303/antes_caminhos.R')$env
suppressPackageStartupMessages(library(data.table))
rules<-jsonlite::fromJSON('artifacts/v303/regras_caminhos.json')
old<-names(rules$nomes)
old<-ifelse(grepl('\\.csv$',old),old,paste0(old,'teste.csv'))
map<-data.frame(old=old,new=vapply(seq_along(old),function(i)paste0(rules$nomes[[i]],substring(old[i],nchar(names(rules$nomes)[i])+1L)),character(1L)))
translate<-function(s) {
 for(n in names(rules$diretorios))s<-gsub(paste0('(?<![[:alnum:]_])',n,'(?![[:alnum:]_])'),rules$diretorios[[n]],s,perl=TRUE)
 for(n in names(rules$nomes))s<-gsub(n,rules$nomes[[n]],s,fixed=TRUE)
 s
}
changed<-map[basename(map$old)!=basename(map$new)&!grepl('08_relatorios_analiticos',map$old),]
for(i in seq_len(nrow(changed))) {
 old<-basename(changed$old[i]);new<-basename(changed$new[i])
 if(!identical(e$monitora_output_classificar_arquivo_raiz(new),translate(b$monitora_output_classificar_arquivo_raiz(old)))) stop(old,' -> ',new,': ',e$monitora_output_classificar_arquivo_raiz(new),' != ',translate(b$monitora_output_classificar_arquivo_raiz(old)))
 stopifnot(identical(e$monitora_output_destino_correcao(new),translate(b$monitora_output_destino_correcao(old))))
}
for(uc in c('Estação Ecológica de Taiamã','Parque Nacional Mapinguari')) {
 z<-e$monitora_relatorios_analiticos_destino_fisico('output',uc,'nome_muito_longo','2022-2025')
 stopifnot(z$compactado,z$base_sint=='analitico_sintetico',nchar(z$diretorio_id)==12L)
}
# Catálogos físicos mudam; identidade/severidade/rótulo científicos não.
for(f in c('monitora_validados_schema_embutido','monitora_diag_rel_catalogo_ocorrencias_base'))stopifnot(identical(e[[f]](),b[[f]]()))
x<-e$monitora_caminho_windows_projetado(c('output/teste.csv','output/doc.pdf'),'output')
stopifnot(nchar(x[1])==120+1+nchar('output/teste.csv'))
t<-tempfile();dir.create(file.path(t,'output'),recursive=TRUE)
writeLines('x',file.path(t,'output','teste.csv'))
ind<-e$monitora_output_escrever_indice_produtos(file.path(t,'output'),'teste','caminhos')
stopifnot(all(ind$situacao_caminho_office=='apto_abertura_windows'),all(ind$limite_recomendado_windows==210))
cat('PASS: classificação consistente, nomes compactos, orçamento portátil, autorreferência e contratos preservados.\n')
antigo<-Sys.getenv('MONITORA_DESTINO_COMPARTILHAMENTO')
long<-paste0('C:/',strrep('p',150))
Sys.setenv(MONITORA_DESTINO_COMPARTILHAMENTO=chartr('/',intToUtf8(92L),long))
stopifnot(identical(e$monitora_caminho_windows_projetado('output/a.csv','output'),paste0(long,'/output/a.csv')))
Sys.setenv(MONITORA_DESTINO_COMPARTILHAMENTO=antigo)
fig<-e$monitora_relatorios_analiticos_caminho_figura('output/08_analises/u_teste',paste0(strrep('nome_',30),'.png'))
stopifnot(nchar(basename(fig))<=64)
cat('PASS: contrabarras Windows e nomes de figura portáteis.\n')
