#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})

args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.1.R"
destino <- if (length(args) >= 2L) args[[2L]] else
  file.path(tempdir(), "mapa_continuidade_uas_satelite_v291.png")
cache_tif <- if (length(args) >= 3L) args[[3L]] else
  Sys.getenv("MONITORA_QA_SENTINEL2_TIF", unset = "")
resumo_csv <- if (length(args) >= 4L) args[[4L]] else
  Sys.getenv("MONITORA_QA_RESUMO_ESPACIAL", unset = "")

if (!file.exists(script) || !file.exists(cache_tif) || !file.exists(resumo_csv)) {
  stop("Entradas do teste Sentinel-2 ausentes.", call. = FALSE)
}

arvore <- parse(file = script, keep.source = FALSE)
funcoes <- new.env(parent = globalenv())
coletar <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]]) &&
      is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")) {
    nome <- as.character(expr[[2L]])
    if (startsWith(nome, "monitora_relatorios_analiticos_")) eval(expr, envir = funcoes)
    return(invisible(NULL))
  }
  if (length(expr) > 1L) for (ii in seq_along(expr)[-1L]) coletar(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
list2env(as.list.environment(funcoes, all.names = TRUE), envir = .GlobalEnv)
linhas_cabecalho_script <- readLines(script, n = 500L, warn = FALSE, encoding = "UTF-8")
extrair_constante_script <- function(nome, padrao) {
  rx <- paste0("^[[:space:]]*", nome, "[[:space:]]*<-[[:space:]]*\"")
  linha <- grep(rx, linhas_cabecalho_script, value = TRUE)[1L]
  if (is.na(linha) || !nzchar(linha)) return(padrao)
  sub('^[^\"]*\"([^\"]+)\".*$', "\\1", linha, perl = TRUE)
}
MONITORA_SCRIPT_BUILD_ID <- extrair_constante_script(
  "MONITORA_SCRIPT_BUILD_ID",
  "build não identificado"
)
MONITORA_SCRIPT_VERSAO <- extrair_constante_script(
  "MONITORA_SCRIPT_VERSAO",
  "versão não identificada"
)

uas <- fread(resumo_csv, encoding = "UTF-8")
necessarias <- c(
  "long_ini", "lat_ini", "long_fin", "lat_fin", "lon_meio", "lat_meio",
  "classe_continuidade_label", "formacao_mais_recente_label"
)
if (!all(necessarias %in% names(uas))) stop("Resumo espacial incompleto.", call. = FALSE)
uas[, formacao_label := formacao_mais_recente_label]
uas[, `:=`(ANO_INICIAL = 2023L, ANO_FINAL = 2026L)]
dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
limite_uc <- monitora_relatorios_analiticos_limite_uc_oficial(
  uas,
  ativado = TRUE
)
monitora_relatorios_analiticos_renderizar_sentinel2(
  rgb = terra::rast(cache_tif),
  uas = uas,
  destino = destino,
  data_aquisicao = "2026-07-23",
  nuvens_area_pct = 2.2,
  atribuicao = "Contém dados Copernicus Sentinel modificados 2026 • AWS Open Data / Earth Search",
  cenas = "S2B_24LWK_20260723_0_L2A | S2B_24LVK_20260723_0_L2A",
  resolucao_origem_m = 10,
  limite_uc = limite_uc$limite,
  estados = limite_uc$estados,
  biomas = limite_uc$biomas,
  status_limite_uc = limite_uc$status
)
if (!file.exists(destino) || file.info(destino)$size < 100000L) {
  stop("Mapa Sentinel-2 profissional não foi materializado.", call. = FALSE)
}
if (!isTRUE(limite_uc$status$localizado[[1L]])) {
  stop(paste0("Limite oficial da UC não localizado: ", limite_uc$status$motivo[[1L]]), call. = FALSE)
}
if (!isTRUE(limite_uc$status$uso_arquivo_temporario[[1L]]) ||
    isTRUE(limite_uc$status$artefato_espacial_persistido[[1L]])) {
  stop("O limite oficial da UC não respeitou o contrato de uso temporário.", call. = FALSE)
}
if (!isTRUE(limite_uc$status$estados_localizados[[1L]]) ||
    !isTRUE(limite_uc$status$biomas_localizados[[1L]])) {
  stop("Estados ou biomas oficiais não foram materializados para o localizador.", call. = FALSE)
}
cat(
  "QA_V291_SENTINEL_CARTOGRAFIA_OK\n",
  "MAPA=", normalizePath(destino, winslash = "/"), "\n",
  "LIMITE_UC=", limite_uc$status$nome_uc_oficial[[1L]], "\n",
  sep = ""
)
