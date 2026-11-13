# Gate estático de publicação; não executa o script, funções ou dados de UC.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
candidate <- args[[1L]]; public <- args[[2L]]
lines <- readLines(public, encoding = "UTF-8", warn = FALSE)
old <- c('MONITORA_SCRIPT_VERSAO <- "3.0.8"',
  'MONITORA_SCRIPT_BUILD_ID <- "v3.0.8-20261113-r01"')
new <- c('MONITORA_SCRIPT_VERSAO <- "3.0.8-dev_r04"',
  'MONITORA_SCRIPT_BUILD_ID <- "v3.0.8-dev_r04-20261113"')
for (i in seq_along(old)) {
  hit <- which(lines == old[[i]])
  stopifnot(length(hit) == 1L)
  lines[hit] <- new[[i]]
}
a <- parse(candidate, encoding = "UTF-8", keep.source = FALSE)
b <- parse(public, encoding = "UTF-8", keep.source = FALSE)
normalized <- parse(text = lines, encoding = "UTF-8", keep.source = FALSE)
stopifnot(identical(a, normalized), length(b) == 1L,
  identical(b[[1L]][[1L]], quote(base::evalq)))
jsonlite::write_json(list(status = "PASS", type = "STATIC_PUBLICATION_GATE",
  R = R.version.string, ast_identical_after_metadata_normalization = TRUE,
  single_outer_evalq = TRUE, Source_executed = FALSE,
  institutional_data_read = FALSE, new_homologation = FALSE), args[[3L]], auto_unbox = TRUE, pretty = TRUE)
cat("PASS: promoção estática; AST preservada, uma expressão externa, nenhum Source.\n")
