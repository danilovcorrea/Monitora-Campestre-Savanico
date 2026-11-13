suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
script <- if (length(args)) args[1L] else "monitora_campsav_alvo_global_v3.0.8.R"
source("tools/v308/carregar.R")
e <- monitora_v308_carregar(script)
dir <- tempfile("v308_integracao_"); dir.create(file.path(dir, "figuras"), recursive = TRUE)
falha <- function(expr, padrao = "Gate de paginas de evidencia") {
  mensagem <- tryCatch({force(expr); ""}, error = conditionMessage)
  stopifnot(grepl(padrao, mensagem, fixed = TRUE))
}
criar <- function(nome, ultima = .7) {
  arq <- file.path(dir, "figuras", nome)
  paths <- c(arq, file.path(dirname(arq), paste0(sub("[.]png$", "", nome), c("_p2.png", "_p3.png"))))
  for (i in seq_along(paths)) png::writePNG(array(c(.1, .4, ultima)[i], dim = c(8, 8, 3)), paths[i])
  m <- e$monitora_v308_evidencia_manifestos(arq)
  cells <- data.table(valor_cientifico = seq_len(9L) / 17,
    arquivo_canonico = nome, input_row = seq_len(9L), cell_id = paste0("cell_", seq_len(9L)),
    pagina = rep(1:3, each = 3), arquivo = rep(basename(paths), each = 3))
  fwrite(cells, m$celulas, bom = TRUE, na = "")
  pages <- data.table(arquivo_canonico = nome, arquivo = basename(paths), pagina = 1:3,
    n_paginas = 3L, n_resultados = 3L, md5 = unname(tools::md5sum(paths)),
    sha256 = vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), character(1L)),
    celulas_arquivo = basename(m$celulas), celulas_md5 = unname(tools::md5sum(m$celulas)),
    celulas_sha256 = digest::digest(file = m$celulas, algo = "sha256"),
    fonte_pt = 7, largura_pol = 6.65, altura_pol = 8, fit = TRUE)
  fwrite(pages, m$paginas, bom = TRUE, na = "")
  list(arq = arq, paths = paths, manifest = m, pages = pages, cells = cells)
}
a <- criar("evidencia_estatistica_a.png")
b <- criar("evidencia_estatistica_b.png", .9)
g <- data.table(id = "inferencias_a", disponivel = TRUE, arquivo_relatorio = paste0("figuras/", basename(a$arq)),
  hash_md5 = unname(tools::md5sum(a$arq)), n_resultados_estatisticos = 9L,
  incorporada_relatorio_detalhado = TRUE, status_documental = "selecionada_relatorio_detalhado", painel_representante = "inferencias_a")
original <- copy(g); audit <- data.frame(id = 1:9, valor = 1:9 / 17)
attr(g, "auditoria_estatistica") <- audit
indice <- e$monitora_v308_completar_indice_graficos(g, dir)
stopifnot(nrow(indice) == 1L, indice$n_resultados_estatisticos == 9L, indice$n_paginas_evidencia == 3L,
  all(vapply(names(original), function(k) identical(indice[[k]], original[[k]]), logical(1L))),
  identical(attr(indice, "auditoria_estatistica"), audit), identical(g$n_resultados_estatisticos, original$n_resultados_estatisticos))
ia <- e$monitora_v308_ler_paginas_evidencia(a$arq, dir)
ib <- e$monitora_v308_ler_paginas_evidencia(b$arq, dir)
stopifnot(ia$paginas$md5[1] == ib$paginas$md5[1],
  e$monitora_v308_chave_conjunto_evidencia(ia) != e$monitora_v308_chave_conjunto_evidencia(ib))
fig <- e$monitora_relatorios_analiticos_figura(g$arquivo_relatorio, "Conjunto completo.")
expanded <- e$monitora_v308_expandir_figuras(c("# Evidências", fig), dir)
stopifnot(sum(startsWith(expanded, "<figure")) == 3L,
  identical(e$monitora_v308_expandir_figuras(expanded, dir), expanded))
numerado <- e$monitora_relatorios_analiticos_numerar(expanded, dir)
stopifnot(sum(numerado$auditoria$tipo == "figura") == 3L,
  !any(grepl("[[figura:", numerado$conteudo, fixed = TRUE)))
docx <- e$monitora_relatorios_analiticos_conteudo_docx(numerado$conteudo)
stopifnot(sum(grepl("^!\\[Figura [123]", docx)) == 3L)
e$monitora_v308_validar_paginas_documentais(g, dir, expanded, character())
falha(e$monitora_v308_validar_paginas_documentais(g, dir, expanded[!grepl('_p3.png', expanded, fixed = TRUE)], character()))
falha(e$monitora_v308_validar_paginas_documentais(g, dir, expanded, fig))
salvos <- lapply(c(a$manifest$paginas, a$manifest$celulas), readBin, what = "raw", n = 1e8)
unlink(c(a$manifest$paginas, a$manifest$celulas))
stopifnot(!e$monitora_v308_ler_paginas_evidencia(a$arq, dir)$manifesto)
falha(e$monitora_v308_completar_indice_graficos(g, dir))
falha(e$monitora_v308_validar_paginas_documentais(g, dir, expanded, character()))
for (i in 1:2) writeBin(salvos[[i]], c(a$manifest$paginas, a$manifest$celulas)[i])
pag <- copy(a$pages); pag$fonte_pt[1] <- 6.9; fwrite(pag, a$manifest$paginas)
falha(e$monitora_v308_ler_paginas_evidencia(a$arq, dir))
fwrite(a$pages, a$manifest$paginas, bom = TRUE, na = "")
cells <- copy(a$cells); cells$input_row[9] <- 8L; fwrite(cells, a$manifest$celulas, bom = TRUE, na = "")
pag <- copy(a$pages); pag$celulas_md5 <- unname(tools::md5sum(a$manifest$celulas)); pag$celulas_sha256 <- digest::digest(file = a$manifest$celulas, algo = "sha256")
fwrite(pag, a$manifest$paginas, bom = TRUE, na = "")
falha(e$monitora_v308_ler_paginas_evidencia(a$arq, dir))
fwrite(a$cells, a$manifest$celulas, bom = TRUE, na = ""); fwrite(a$pages, a$manifest$paginas, bom = TRUE, na = "")
png::writePNG(array(.2, dim = c(8, 8, 3)), a$paths[3])
falha(e$monitora_v308_ler_paginas_evidencia(a$arq, dir))
unlink(dir, recursive = TRUE)
cat("PASS: particao, hashes integrais, indice sem duplicar estatistica, referencias, Word, adulteracoes e manifestos obrigatorios.\n")
