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
  "monitora_relatorios_analiticos_http_get_auditado",
  "monitora_relatorios_analiticos_status_limite_uc",
  "monitora_relatorios_analiticos_contexto_localizador_sem_uc",
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

uas <- if (length(args) >= 2L) {
  fonte_uas <- normalizePath(args[[2L]], mustWork = TRUE)
  fonte <- fread(fonte_uas, encoding = "UTF-8", showProgress = FALSE)
  obrigatorias <- c("UC", "long_ini", "long_fin", "lon_meio", "lat_ini", "lat_fin", "lat_meio")
  stopifnot(all(obrigatorias %in% names(fonte)), uniqueN(fonte$UC) == 1L)
  unique(fonte[, ..obrigatorias])
} else {
  data.table(
    UC = "Floresta Nacional de Contendas do Sincorá",
    long_ini = -41.1054,
    long_fin = -41.1052,
    lon_meio = -41.1053,
    lat_ini = -13.9065,
    lat_fin = -13.9061,
    lat_meio = -13.9063
  )
}
resultado <- env$monitora_relatorios_analiticos_limite_uc_oficial(uas, ativado = TRUE)
status <- as.data.table(resultado$status)
if (!isTRUE(status$localizado[[1L]]) ||
    !(status$metodo_obtencao[[1L]] %in% c(
      "WFS_oficial_ICMBio_INDE_por_extensao_da_rede", "ZIP_oficial_ICMBio"
    ))) print(status)
stopifnot(
  nrow(status) == 1L,
  isTRUE(status$solicitado[[1L]]),
  isTRUE(status$localizado[[1L]]),
  status$metodo_obtencao[[1L]] %in% c(
    "WFS_oficial_ICMBio_INDE_por_extensao_da_rede", "ZIP_oficial_ICMBio"
  ),
  isTRUE(status$uso_arquivo_temporario[[1L]]),
  identical(status$artefato_espacial_persistido[[1L]], FALSE),
  !is.null(resultado$limite),
  nrow(resultado$limite) == 1L,
  nzchar(terra::crs(resultado$limite, proj = TRUE)),
  isTRUE(status$estados_localizados[[1L]]),
  isTRUE(status$biomas_localizados[[1L]])
)

cat(
  "TEST_V295_LOCATOR_ONLINE_OK\n",
  "UC=", unique(uas$UC), "\n",
  "METODO=", status$metodo_obtencao[[1L]], "\n",
  "COMPONENTES=", status$componentes_localizador[[1L]], "\n",
  sep = ""
)
