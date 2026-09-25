# Regressão das 256 combinações de elementos presentes/ausentes, sem o gate de escopo da versão antiga.
t<-parse('tests/test_v302_numeracao.R')
for(expr in t){if(is.call(expr)&&identical(expr[[1]],quote(`<-`))&&identical(expr[[2]],quote(b)))break;eval(expr,.GlobalEnv)}
b<-monitora_test_funcoes('monitora_campsav_alvo_global_v3.0.3.R')
for(f in c('monitora_validados_schema_embutido','monitora_diag_rel_catalogo_ocorrencias_base'))stopifnot(identical(e[[f]](),b$env[[f]]()))
s<-readLines('R_monitora_campsav_alvo_global.R',warn=FALSE);bytes<-file.info('R_monitora_campsav_alvo_global.R')$size;stopifnot(bytes<5000000,bytes+length(s)<5000000)
cat('PASS: numeração condicional, contrato 129, catálogo de ocorrências e tamanho LF/CRLF.\n')
