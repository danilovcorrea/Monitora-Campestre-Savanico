#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
  library(terra)
})
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.5.R",
  mustWork = TRUE
)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_relatorios_analiticos_normalizar_nome_uc",
  "monitora_relatorios_analiticos_status_limite_uc",
  "monitora_relatorios_analiticos_limite_uc_oficial"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nome <- as.character(x[[2L]])
    if (nome %in% alvos) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))
env$MONITORA_SCRIPT_VERSAO <- "2.9.5-dev-online-test"

uas <- data.table(
  UC = "Floresta Nacional de Contendas do Sincorá",
  lon = -41.1,
  lat = -13.8
)
resultado <- env$monitora_relatorios_analiticos_limite_uc_oficial(uas, ativado = TRUE)
status <- as.data.table(resultado$status)
if (!isTRUE(status$localizado[[1L]]) ||
    !identical(status$metodo_obtencao[[1L]], "WFS_oficial_ICMBio_INDE")) print(status)
stopifnot(
  nrow(status) == 1L,
  isTRUE(status$solicitado[[1L]]),
  isTRUE(status$localizado[[1L]]),
  identical(status$metodo_obtencao[[1L]], "WFS_oficial_ICMBio_INDE"),
  isTRUE(status$uso_arquivo_temporario[[1L]]),
  identical(status$artefato_espacial_persistido[[1L]], FALSE),
  !is.null(resultado$limite),
  nrow(resultado$limite) == 1L,
  nzchar(terra::crs(resultado$limite, proj = TRUE)),
  isTRUE(status$estados_localizados[[1L]]),
  isTRUE(status$biomas_localizados[[1L]])
)

cat("TEST_V295_LOCATOR_ONLINE_OK\n")
