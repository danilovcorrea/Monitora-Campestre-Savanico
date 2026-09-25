# Rscript tests/test_v304_tabela_sintetica.R [script] [pasta_csv_estado]
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
script <- if (length(args)) args[1L] else 'R_monitora_campsav_alvo_global.R'
base <- if (length(args) >= 2L) args[2L] else '../atualizacoes_v304_20260925/FNB/r03/output/08_analises/u_e1e7dd6739'
# Extrair apenas o helper local, sem executar cálculos/pipeline do relatório.
src <- readLines(script, encoding='UTF-8', warn=FALSE)
a <- grep('^  estado_resumir_relatorio <- function', src)
b <- grep('^  estado_prioritario_sintetico <-', src)
stopifnot(length(a)==1L, length(b)==1L, b>a)
e <- new.env(parent=globalenv())
eval(parse(text=src[a:(b-1L)]), e)
specs <- list(
 c('estrutura_viva','Componente estrutural','Estrutura viva','2'),
 c('formas_nativas','Forma de vida nativa','Nativas','3'),
 c('formas_exoticas','Forma de vida exótica','Exóticas','2'),
 c('formas_secas_mortas','Forma de vida seca ou morta','Secas ou mortas','2'),
 c('material_botanico','Material botânico','Material botânico','2'))
parts <- lapply(specs, function(z) {
 x <- fread(file.path(base,paste0('estado_atual_',z[1L],'.csv')))
 snapshot <- copy(x)
 # Reproduz somente o filtro documental e os limites já estabelecidos.
 x <- x[is.na(`Nº de registros`) | `Nº de registros` > 0]
 expected <- x[,head(.SD,as.integer(z[4L])),by=Formação]
 setnames(expected,z[2L],'Indicador'); expected[,Eixo:=z[3L]]
 out <- e$estado_resumir_relatorio(x,z[3L],as.integer(z[4L]))
 setcolorder(expected,names(out))
 stopifnot(identical(out,expected), 'Campanha (ano)' %in% names(out),
   all(out[['Campanha (ano)']]==2026L), !any(grepl('^20[0-9]{2}$',out$Indicador)),
   identical(snapshot,fread(file.path(base,paste0('estado_atual_',z[1L],'.csv')))))
 out
})
actual <- rbindlist(parts,fill=TRUE,use.names=TRUE)
stopifnot(nrow(actual)==22L,ncol(actual)==9L,
 identical(names(actual),c('Eixo','Campanha (ano)','Formação','Indicador','Nº de UAs',
  'Nº de registros','Nº de pontos com presença','Cobertura (%)','Proporção relativa (%)')))
# Comparação independente com o CSV legado afetado: mesmas linhas, métricas,
# anos e categorias, só unificação dos cinco nomes de coluna redundantes.
legacy <- fread(file.path(base,'estado_atual_prioritario_relatorio_sintetico.csv'))
if (!'Campanha (ano)' %in% names(legacy)) {
 for (z in specs) stopifnot(identical(as.character(actual[Eixo==z[3L],Indicador]),
   as.character(legacy[Eixo==z[3L]][[z[2L]]])))
 stopifnot(identical(as.integer(legacy$Indicador),actual[['Campanha (ano)']]))
} else stopifnot(identical(as.character(legacy$Indicador),actual$Indicador))
metrics <- intersect(names(legacy),setdiff(names(actual),c('Indicador','Campanha (ano)')))
stopifnot(identical(legacy[,..metrics],actual[,..metrics]))
# Metadados inesperados não podem voltar a se passar por categoria.
bad <- copy(parts[[1L]]); bad[,Eixo:=NULL]; bad[,metadado_novo:='x']
stopifnot(inherits(try(e$estado_resumir_relatorio(bad,'Estrutura viva',2L),silent=TRUE),'try-error'))
# A quebra depende do ID semântico, nunca do número editorial da tabela.
source('tests/helpers_v304.R')
fun <- monitora_v304_funcoes(script)
html <- tempfile(fileext='.html')
writeLines('<html><head></head><body><p><span id="monitora-tab-estado-prioritario"></span></p><table><caption>Tabela 5 — Estado prioritário</caption><tr><td>92,6</td></tr></table><p><span id="monitora-tab-outra"></span></p><table><caption>Tabela 6 — Outra</caption><tr><td>37</td></tr></table></body></html>',html,useBytes=TRUE)
fun$monitora_relatorios_analiticos_html_legendas_tabelas(html)
doc <- xml2::read_html(html)
blocks <- xml2::xml_find_all(doc,".//div[@class='monitora-bloco-tabela']")
stopifnot(length(blocks)==2L,
 identical(xml2::xml_attr(blocks[[1L]],'style'),'break-inside:auto; page-break-inside:auto'),
 is.na(xml2::xml_attr(blocks[[2L]],'style')),
 grepl('break-after:avoid',xml2::xml_attr(xml2::xml_find_first(blocks[[1L]],'./p'),'style'),fixed=TRUE),
 length(xml2::xml_find_all(doc,'.//table'))==2L,
 identical(xml2::xml_text(xml2::xml_find_all(doc,'.//td')),c('92,6','37')))
unlink(html)
cat('PASS: FNB real — 22 linhas, nove colunas, anos/categorias/métricas preservados; quebra restrita à tabela prioritária.\n')
