source('tests/helpers_test_funcoes.R')
a<-monitora_test_funcoes('R_monitora_campsav_alvo_global.R');e<-a$env
out<-'artifacts/v302_numeracao/unit';dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(i in 1:4)png::writePNG(array(0.5,dim=c(8,8,3)),file.path(out,paste0(i,'.png')))
ids<-c('esforco-fluxos','estado-atual','exoticas','fogo-fonte')
# Independent omissions of tables and figures; all combinations, with present/omitted sections.
for(mask in 0:255) {
 bits<-as.logical(intToBits(mask)[1:8]);x<-c('# Visão geral','Texto.')
 for(i in 1:4) {
  if(any(bits[c(i,i+4)])) x<-c(x,paste0('## Análise ',i))
  if(bits[i]) x<-c(x,e$monitora_relatorios_analiticos_kable(data.frame(A=1:2,B=c('x','y')),id=ids[i]))
  if(bits[i+4])x<-c(x,e$monitora_relatorios_analiticos_figura(paste0(i,'.png'),paste('Gráfico',i)))
 }
 z<-e$monitora_relatorios_analiticos_numerar(x,out)
 stopifnot(sum(z$auditoria$tipo=='tabela' & z$auditoria$status=='incluido')==sum(bits[1:4]))
 stopifnot(sum(z$auditoria$tipo=='figura' & z$auditoria$status=='incluido')==sum(bits[5:8]))
 e$monitora_relatorios_analiticos_validar_numeracao(e$monitora_relatorios_analiticos_indice(z$conteudo),z$auditoria)
}
cat('PASS: 256 combinações de omissões independentes e reinício por documento.\n')
falha<-function(expr,trecho){v<-tryCatch({force(expr);''},error=conditionMessage);stopifnot(grepl(trecho,v,fixed=TRUE))}
k<-e$monitora_relatorios_analiticos_kable(data.frame(A=1),id='estado-atual')
f<-e$monitora_relatorios_analiticos_figura('1.png','Imagem')
z<-e$monitora_relatorios_analiticos_numerar(c('# A',k,f,'Consulte [[tabela:estado-atual]].'),out)
stopifnot(any(grepl('[Tabela 1](#monitora-tab-estado-atual)',z$conteudo,fixed=TRUE)))
falha(e$monitora_relatorios_analiticos_numerar(c('# A',k,k),out),'duplicada')
falha(e$monitora_relatorios_analiticos_numerar(c('# A',f,f),out),'duplicada')
falha(e$monitora_relatorios_analiticos_numerar(c('# A','### Salto'),out),'Salto de nível')
falha(e$monitora_relatorios_analiticos_numerar(c('# A','[[tabela:estado-atual]]'),out),'não incluído')
falha(e$monitora_relatorios_analiticos_numerar(c('# A',e$monitora_relatorios_analiticos_figura('inexistente.png','Imagem')),out),'ausente')
writeLines('PNG corrompido',file.path(out,'invalido.png'))
falha(e$monitora_relatorios_analiticos_numerar(c('# A',e$monitora_relatorios_analiticos_figura('invalido.png','Imagem')),out),'PNG inválida')
falha(e$monitora_relatorios_analiticos_validar_numeracao(gsub('Figura 1.','Figura 3.',z$conteudo,fixed=TRUE),z$auditoria),'divergentes')
falha(e$monitora_relatorios_analiticos_validar_numeracao(z$conteudo[!grepl('^Table:',z$conteudo)],z$auditoria),'divergentes')
vazio<-e$monitora_relatorios_analiticos_numerar(c('# A',e$monitora_relatorios_analiticos_kable(data.frame(),id='exoticas')),out)
stopifnot(!any(grepl('Table: Tabela',vazio$conteudo)))
falha(e$monitora_relatorios_analiticos_validar_numeracao(gsub('](#monitora-tab-estado-atual)','](#monitora-tab-ausente)',z$conteudo,fixed=TRUE),z$auditoria),'sem destino')
cat('PASS: tabelas vazias, referências, duplicatas, arquivo ausente/corrompido e sequência adulterada.\n')
# Original values preserved in native Word tables, including previously converted recommendations.
tab<-data.frame(Prioridade='Alta',Natureza='Campo',Recomendação='Verificar',Fundamento='Dados')
w<-e$monitora_relatorios_analiticos_conteudo_docx(c('# Relatório',e$monitora_relatorios_analiticos_kable(tab,id='recomendacoes')))
stopifnot(any(grepl('^\\|',w)),!any(grepl('^- \\*\\*Recomendação',w)))
# Contract, sizes and limited changed functions.
b<-monitora_test_funcoes('monitora_campsav_alvo_global_v3.0.1.R')
stopifnot(identical(e$monitora_validados_schema_embutido(),b$env$monitora_validados_schema_embutido()))
common<-intersect(a$funcoes,b$funcoes)
changed<-common[!vapply(common,function(n)identical(body(e[[n]]),body(b$env[[n]]))&&identical(formals(e[[n]]),formals(b$env[[n]])),logical(1))]
allowed<-c('monitora_manual_usuario_gerar','monitora_relatorios_analiticos_kable','monitora_relatorios_analiticos_conteudo_docx','monitora_relatorios_analiticos_indice','monitora_relatorios_analiticos_renderizar','monitora_relatorios_analiticos_gerar')
stopifnot(setequal(changed,allowed))
s<-readLines('R_monitora_campsav_alvo_global.R',warn=FALSE);bytes<-file.info('R_monitora_campsav_alvo_global.R')$size
stopifnot(bytes<5000000,bytes+length(s)<5000000)
cat('PASS: contrato preservado, alterações restritas à edição dos relatórios. LF=',bytes,' CRLF=',bytes+length(s),'\n',sep='')
