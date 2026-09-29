suppressPackageStartupMessages(library(data.table));source('tools/v305/carregar.R');e<-monitora_v305_carregar('monitora_campsav_alvo_global_v3.0.6-rc01.R')
p<-tempfile();dir.create(p)
# Referências devem acompanhar tabelas opcionais e desaparecer com a matriz.
for(antes in c(FALSE,TRUE))for(matriz in c(FALSE,TRUE)) {
 x<-c('# Abertura',if(antes)e$monitora_relatorios_analiticos_kable(data.frame(A=1),id='estado-atual'),'# Evidências, hipóteses e gestão','- Fogo: resumo repetido','- Clima e trajetória: resumo repetido','- Calendário — resumo repetido',if(matriz)e$monitora_relatorios_analiticos_kable(data.frame(A=2),id='hipoteses-gestao')else'Nenhuma hipótese prioritária.','# Recomendações')
 z<-e$monitora_v306_editorial(x,p);stopifnot(!any(grepl('^- (Fogo|Clima|Calendário)',z)))
 n<-e$monitora_relatorios_analiticos_numerar(z,p);s<-paste(n$conteudo,collapse='\n')
 stopifnot(!grepl('\\[\\[',s),grepl('A \\[Tabela ',s)==matriz)
 if(matriz)stopifnot(grepl(paste0('[Tabela ',if(antes)2 else 1,'](#monitora-tab-hipoteses-gestao)'),s,fixed=TRUE))
}
# Ausência de clima/recorte/ordenação: estrutura legível, sem achados inventados.
x<-c('# Análise multivariada integrada da cobertura vegetal','A integração não foi calculada nesta execução.','## Discussão integrada e novas investigações','Uma parcela compartilhada entre época, clima e fogo.','# Evidências, hipóteses e gestão','Sem matriz.','# Recomendações')
y<-e$monitora_v306_editorial(x,p);stopifnot(any(grepl('Não há ajuste integrado elegível',y)),!any(grepl('\\[\\[figura:',y)));invisible(e$monitora_relatorios_analiticos_numerar(y,p))
# As médias editoriais devem preservar formação e exatamente o painel ajustado.
w<-data.table(UC='UC',UA=c('1','2','3'),form_veg=c('campestre','campestre','savanica'),nativa=c(1,3,-2),exotica=0,seca_morta=0,serrapilheira=c(2,4,6),solo_nu=0,delta_epoca=c(0,2,1))
e$monitora_v306_meta_painel(w,data.table(inicio=2020,fim=2022,UAs=3),list(Epoca='delta_epoca'),p)
a<-e$monitora_v306_ler(p,'mv_mudancas_observadas');stopifnot(a[form_veg=='campestre'&indicador=='nativa',delta_medio_pp]==2,a[form_veg=='savanica'&indicador=='nativa',UAs]==1)
# Proporções dos eixos: parcela ajustada é denominador diferente do total.
Y<-matrix(c(1,-1,0,2,0,-2,0,1,-1),3);s<-svd(Y*.5);e$monitora_v306_meta_ordenacao(s,Y,p);a<-e$monitora_v306_ler(p,'mv_ord_eixos');stopifnot(abs(sum(a$parcela_ajustada_pct)-100)<1e-8,abs(sum(a$variacao_total_pct)-25)<1e-8)
# O artefato publicado permanece intacto; candidata em LF e CRLF < 5 MB.
s<-readBin('monitora_campsav_alvo_global_v3.0.6-rc01.R','raw',n=6e6);stopifnot(length(s)<5e6,length(s)+sum(s==as.raw(10))<5e6);stopifnot(digest::digest(file='monitora_campsav_alvo_global_v3.0.5.R',algo='sha256')=='a991c144ac09e7faa94b4c48d95e623a72a3a0d4c524ecbb8599da902d8ff348')
unlink(p,recursive=TRUE);cat('PASS: referências condicionais, ausências, população, denominadores dos eixos e preservação da versão publicada.\n')
