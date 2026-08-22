#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(terra)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.16.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.15.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
arvore <- parse(file = script, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)

inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"',
           'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"',
           'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio_congelado(linhas), inicio_congelado(base)),
  "A seção congelada de inicialização do RStudio foi alterada."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.16"',
  'margem_tecnica_rgb <- 0.05',
  'max_paginas = 20L',
  'catalogo_truncado',
  'monitora_relatorios_analiticos_mosaico_previews <- function',
  'preview_multitemporal',
  'cache_persistente_reutilizado',
  'c("cog_visual", "cog_bandas_rgb")',
  'sentinel2-v2916-hires-cog-v6',
  'cache persistente não atende ao contrato de alta resolução',
  'Prévia pode existir como recurso diagnóstico, mas jamais passa pelo gate',
  'última composição Sentinel-2 L2A',
  'falha na aquisição COG Sentinel de alta resolução'
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Revisão ausente: ", trecho))
exigir(
  !grepl('margens_rgb <- c(0.50, 0.65, 0.80)', texto, fixed = TRUE),
  "A reconstrução tripla do mosaico permaneceu ativa."
)
exigir(
  !grepl("falha após quatro extensões progressivas", texto, fixed = TRUE),
  "Diagnóstico incorreto de quatro extensões permaneceu."
)

alvos <- c(
  "monitora_relatorios_analiticos_consultar_sentinel2",
  "monitora_relatorios_analiticos_cache_sentinel_persistente",
  "monitora_relatorios_analiticos_baixar_asset_atomico",
  "monitora_relatorios_analiticos_preview_georreferenciado",
  "monitora_relatorios_analiticos_mosaico_previews"
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
       "Funções Sentinel focais não puderam ser extraídas.")
env$MONITORA_SCRIPT_VERSAO <- "2.9.16-teste"

if (identical(toupper(Sys.getenv("MONITORA_QA_ONLINE", unset = "N")), "S")) {
  consulta <- env$monitora_relatorios_analiticos_consultar_sentinel2(
    bbox = c(xmin = -56.2, ymin = -31.0, xmax = -55.0, ymax = -29.5),
    data_inicio = as.Date("2026-06-01"),
    data_fim = as.Date("2026-08-21"),
    limite_itens = 1L,
    max_paginas = 3L,
    max_itens_total = 20L,
    tempo_maximo_seg = 60
  )
  exigir(nrow(consulta) == 3L, "A paginação online não percorreu três páginas.")
  exigir(identical(attr(consulta, "paginas_catalogo"), 3L),
         "Quantidade de páginas consultadas não foi auditada.")
  exigir(isTRUE(attr(consulta, "catalogo_truncado")),
         "Consulta limitada com próxima página não foi marcada como truncada.")
  exigir(all(c(
    "thumbnail_href", "proj_epsg", "visual_transform", "visual_shape",
    "item_xmin", "item_ymin", "item_xmax", "item_ymax"
  ) %in% names(consulta)), "Metadados geoespaciais da prévia estão incompletos.")

  cache <- tempfile("v2916_preview_")
  dir.create(cache, recursive = TRUE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)
  preview <- env$monitora_relatorios_analiticos_preview_georreferenciado(
    consulta[1L], cache
  )
  exigir(terra::nlyr(preview) == 3L && nzchar(terra::crs(preview)),
         "Prévia Sentinel não foi georreferenciada em três bandas.")
}

cat("TEST_V2916_SENTINEL_RESILIENTE_OK\n")
