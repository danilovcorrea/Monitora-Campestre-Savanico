args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
base_dir <- if (length(file_arg)) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))) else normalizePath(".", mustWork = TRUE)
script <- file.path(base_dir, "monitora_campsav_alvo_global_v2.9.25.R")
stopifnot(file.exists(script), file.info(script)$size < 5 * 1024^2)

codigo_externo <- parse(script, keep.source = FALSE)
stopifnot(length(codigo_externo) == 1L, is.call(codigo_externo[[1L]]), identical(codigo_externo[[1L]][[1L]], quote(base::evalq)))
codigo <- as.list(codigo_externo[[1L]][[2L]])[-1L]
nome_atribuicao <- function(x) {
  if (!is.call(x) || length(x) < 3L || !identical(x[[1L]], as.name("<-")) || !is.symbol(x[[2L]])) return("")
  as.character(x[[2L]])
}
carregar <- function(nome, ambiente) {
  idx <- which(vapply(codigo, function(x) identical(nome_atribuicao(x), nome), logical(1)))
  stopifnot(length(idx) == 1L)
  eval(codigo[[idx]], envir = ambiente)
}

e <- new.env(parent = globalenv())
carregar("monitora_qfield_xml", e)
carregar("monitora_qfield_escrever_qgs", e)
destino <- tempfile(fileext = ".qgs")
e$monitora_qfield_escrever_qgs(destino, "UC de teste", list(), list(), c(-1000, -1000, 1000, 1000))

doc <- xml2::read_xml(destino, options = "NONET")
camada <- xml2::xml_find_all(doc, ".//projectlayers/maplayer[layername='Google Satellite']")
arvore <- xml2::xml_find_all(doc, ".//layer-tree-layer[@name='Google Satellite']")
stopifnot(length(camada) == 1L, length(arvore) == 1L)
stopifnot(xml2::xml_text(xml2::xml_find_first(camada, "./provider")) == "wms")
fonte <- xml2::xml_text(xml2::xml_find_first(camada, "./datasource"))
stopifnot(grepl("^crs=EPSG:3857&format&type=xyz&url=https://mt1[.]google[.]com/vt/", fonte))
stopifnot(grepl("lyrs%3Ds", fonte, fixed = TRUE), grepl("zmax=20", fonte, fixed = TRUE), grepl("zmin=0", fonte, fixed = TRUE))
stopifnot(!grepl("(?:^|[&?])(key|token|session)=", fonte, perl = TRUE, ignore.case = TRUE))
stopifnot(xml2::xml_attr(arvore, "providerKey") == "wms")
stopifnot(xml2::xml_attr(arvore, "checked") == "Qt::Unchecked")
stopifnot(!grepl("^[.]/mapas/", fonte))
stopifnot(length(xml2::xml_find_all(doc, ".//ProjectDisplaySettings[@CoordinateType='MapGeographic']")) == 1L)
stopifnot(xml2::xml_text(xml2::xml_find_first(camada, "./attribution")) == "Google")

cat("TEST_V2925_QFIELD_GOOGLE_ONLINE_OK\n")
