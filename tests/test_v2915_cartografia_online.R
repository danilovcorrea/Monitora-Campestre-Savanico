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
    "monitora_campsav_alvo_global_v2.9.15.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
arvore <- parse(file = script, keep.source = FALSE)
alvos <- c(
  "monitora_doc_sha256",
  "monitora_relatorios_analiticos_http_get_auditado",
  "monitora_cartografia_http_texto",
  "monitora_cartografia_extrair_links",
  "monitora_cartografia_ordenar_links_biomas",
  "monitora_cartografia_anos_malha_ibge",
  "monitora_cartografia_normalizar_siglas_uf",
  "monitora_relatorios_analiticos_obter_estados_ibge",
  "monitora_relatorios_analiticos_obter_biomas_ibge"
)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)),
       "Funções cartográficas não puderam ser extraídas.")
env$MONITORA_SCRIPT_VERSAO <- "2.9.15-teste-online"

tmp <- tempfile("v2915_cartografia_online_")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
materializar <- function(x) {
  dados <- terra::as.data.frame(x, geom = "WKT")
  terra::vect(dados, geom = tail(names(dados), 1L), crs = terra::crs(x))
}

tempo_estados <- system.time({
  estados <- env$monitora_relatorios_analiticos_obter_estados_ibge(tmp, materializar)
})[["elapsed"]]
exigir(isTRUE(estados$localizado) && !isTRUE(estados$fallback),
       paste0("Malha estadual anual mais recente não foi obtida: ", estados$motivo))
exigir(identical(estados$autoridade, "IBGE") && identical(estados$edicao, "2025"),
       paste0("Edição estadual inesperada: ", estados$edicao))
exigir(nrow(estados$dados) == 27L && "SIGLA_UF" %in% names(estados$dados),
       "Malha estadual não contém as 27 UFs com sigla normalizada.")
exigir(grepl("BR_UF_2025\\.zip", estados$fonte),
       "Fonte estadual não aponta para o ZIP oficial de 2025.")
exigir(nchar(estados$sha256) == 64L,
       "Checksum SHA-256 da malha estadual não foi registrado.")

tempo_biomas <- system.time({
  biomas <- env$monitora_relatorios_analiticos_obter_biomas_ibge(tmp, materializar)
})[["elapsed"]]
exigir(isTRUE(biomas$localizado) && !isTRUE(biomas$fallback),
       paste0("Base atual de biomas não foi obtida: ", biomas$motivo))
exigir(identical(biomas$autoridade, "IBGE") && identical(biomas$edicao, "2025"),
       paste0("Edição de biomas inesperada: ", biomas$edicao))
exigir(identical(biomas$escala, "1:250.000"),
       paste0("Escala de biomas inesperada: ", biomas$escala))
exigir(grepl("2025_Biomas", biomas$fonte),
       "Fonte de biomas não aponta para a edição oficial 2025.")
exigir("NOM_BIOMA" %in% names(biomas$dados) &&
         any(grepl("Pampa", as.character(biomas$dados$NOM_BIOMA), ignore.case = TRUE)),
       "A camada atual não contém o bioma Pampa identificável.")
exigir(nchar(biomas$sha256) == 64L,
       "Checksum SHA-256 da camada de biomas não foi registrado.")

cat(sprintf(
  "TEST_V2915_CARTOGRAFIA_ONLINE_OK estados=%.3fs biomas=%.3fs\n",
  tempo_estados, tempo_biomas
))
