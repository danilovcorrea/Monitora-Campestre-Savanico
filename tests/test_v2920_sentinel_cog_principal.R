#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
arquivo_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
raiz <- if (length(arquivo_arg)) {
  normalizePath(file.path(dirname(arquivo_arg[[1L]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
script <- file.path(raiz, "monitora_campsav_alvo_global_v2.9.20.R")
stopifnot(file.exists(script))
linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")

extrair_funcao <- function(nome) {
  inicio <- grep(paste0("^", nome, " <- function\\("), linhas)
  stopifnot(length(inicio) == 1L)
  saldo <- 0L
  abriu <- FALSE
  fim <- NA_integer_
  for (ii in inicio[[1L]]:length(linhas)) {
    texto <- gsub('"(?:\\\\.|[^"\\\\])*"', '""', linhas[[ii]], perl = TRUE)
    saldo <- saldo + lengths(regmatches(texto, gregexpr("\\{", texto))) -
      lengths(regmatches(texto, gregexpr("\\}", texto)))
    if (grepl("\\{", texto)) abriu <- TRUE
    if (abriu && saldo == 0L) {
      fim <- ii
      break
    }
  }
  stopifnot(is.finite(fim))
  linhas[inicio[[1L]]:fim]
}

ler_cog <- extrair_funcao("monitora_relatorios_analiticos_ler_cog_recortado")
texto_ler_cog <- paste(ler_cog, collapse = "\n")
stopifnot(!grepl("GTIFF_DIR", texto_ler_cog, fixed = TRUE))
stopifnot(!grepl("terra::ext(overview) <-", texto_ler_cog, fixed = TRUE))
stopifnot(!grepl("terra::crs(overview) <-", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("OVERVIEW_LEVEL=", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("contrato_overview_valido", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("terra::same.crs", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("terra::crop", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("terra::intersect", texto_ler_cog, fixed = TRUE))
stopifnot(grepl("raster_recortado", texto_ler_cog, fixed = TRUE))

ambiente <- new.env(parent = baseenv())
eval(parse(text = paste(
  extrair_funcao("monitora_relatorios_analiticos_cache_sentinel_compativel"),
  collapse = "\n"
)), envir = ambiente)
compativel <- ambiente$monitora_relatorios_analiticos_cache_sentinel_compativel

stopifnot(compativel(list(
  versao_cache = "sentinel2-hires-v3-cog-principal",
  modo_composicao = "cog_cor_natural_alta_resolucao",
  resolucao_m = 90
)))
stopifnot(compativel(list(
  versao_cache = "sentinel2-hires-v4-overview-gdal-validado",
  modo_composicao = "cog_cor_natural_alta_resolucao",
  resolucao_m = 90
)))
stopifnot(compativel(list(
  versao_cache = "sentinel2-hires-v2",
  modo_composicao = "cog_bandas_rgb_alta_resolucao",
  resolucao_m = 30
)))
stopifnot(compativel(list(
  versao_cache = "sentinel2-hires-v2",
  modo_composicao = "cog_cor_natural_alta_resolucao",
  resolucao_m = 10
)))
stopifnot(!compativel(list(
  versao_cache = "sentinel2-hires-v2",
  modo_composicao = "cog_cor_natural_alta_resolucao",
  resolucao_m = 20.47
)))
stopifnot(!compativel(list(
  versao_cache = "sentinel2-hires-v2",
  modo_composicao = "cog_cor_natural_alta_resolucao",
  resolucao_m = 319.3
)))
stopifnot(!compativel(list(
  versao_cache = "sentinel2-hires-v3-cog-principal",
  modo_composicao = "preview_georreferenciada",
  resolucao_m = 10
)))

texto <- paste(linhas, collapse = "\n")
stopifnot(grepl("sentinel2-v2920-overview-gdal-validado-v9", texto, fixed = TRUE))
stopifnot(grepl("sentinel2-v2920-bandas-diretas-template-v9", texto, fixed = TRUE))
stopifnot(grepl("sentinel2-hires-v3-cog-principal", texto, fixed = TRUE))
stopifnot(grepl("sentinel2-hires-v4-overview-gdal-validado", texto, fixed = TRUE))
stopifnot(grepl('MONITORA_SCRIPT_VERSAO <- "2.9.20"', texto, fixed = TRUE))

message("OK: overview GDAL com contrato espacial validado e caches v3/v4 auditados.")
