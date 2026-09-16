#!/usr/bin/env Rscript
args <- commandArgs(TRUE)
if (length(args) != 2L) stop("Uso: Rscript test_v2926_promocao_candidata.R <candidata.R> <publica.R>")
candidata <- parse(args[[1L]], keep.source = FALSE, encoding = "UTF-8")
publica <- parse(args[[2L]], keep.source = FALSE, encoding = "UTF-8")
normalizar <- function(arv) {
  stopifnot(length(arv) == 1L, identical(arv[[1L]][[1L]], quote(base::evalq)))
  visitar <- function(z) {
    if (!is.call(z)) return(z)
    if (identical(z[[1L]], quote(`<-`)) && is.symbol(z[[2L]]) &&
        as.character(z[[2L]]) %in% c("MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID")) {
      z[[3L]] <- "<IDENTIFICADOR_PUBLICO>"
      return(z)
    }
    if (length(z) > 1L) for (i in 2:length(z)) z[i] <- list(visitar(z[[i]]))
    z
  }
  visitar(arv[[1L]])
}
if (!identical(normalizar(candidata), normalizar(publica))) {
  stop("A release pública alterou expressões além dos identificadores de versão.")
}
cat("TEST_V2926_PROMOCAO_CANDIDATA_OK; AST_IDENTICA_EXCETO_IDENTIFICADORES\n")
