#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.16.R"
script <- normalizePath(script, mustWork = TRUE)
publico <- normalizePath(
  "monitora_campsav_alvo_global_v2.9.16.R",
  mustWork = TRUE
)

linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
linhas_publicas <- readLines(publico, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

stopifnot(
  identical(linhas[seq_len(316L)], linhas_publicas[seq_len(316L)]),
  grepl('c("cog_visual", "cog_bandas_rgb")', texto, fixed = TRUE),
  grepl("max_celulas_mapa_relatorio <- 2000000L", texto, fixed = TRUE),
  grepl("margem_tecnica_rgb <- 0.05", texto, fixed = TRUE),
  grepl("monitora_relatorios_analiticos_tiles_minimos_bbox", texto, fixed = TRUE),
  grepl("Projetar diretamente para o template", texto, fixed = TRUE),
  grepl("GTIFF_DIR:", texto, fixed = TRUE),
  grepl("sentinel2-v2916-hires-cog-v6", texto, fixed = TRUE),
  grepl('datatype = "INT2U"', texto, fixed = TRUE),
  grepl("NAflag = 65535", texto, fixed = TRUE),
  grepl("sentinel2_hires_ultimo_", texto, fixed = TRUE),
  grepl("qualidade_resolucao_aprovada", texto, fixed = TRUE),
  grepl("tempo_orcamento_mosaico_seg", texto, fixed = TRUE),
  grepl("tempo_mosaico_rgb_seg", texto, fixed = TRUE),
  grepl("permitir_cobertura_parcial = FALSE", texto, fixed = TRUE),
  grepl("prévia pode existir como recurso diagnóstico", tolower(texto), fixed = TRUE)
)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_relatorios_analiticos_aoi_satelite",
  "monitora_relatorios_analiticos_template_satelite",
  "monitora_relatorios_analiticos_status_mapa_satelite",
  "monitora_relatorios_analiticos_tiles_minimos_bbox",
  "monitora_relatorios_analiticos_bbox_satelite",
  "monitora_relatorios_analiticos_metricas_radiometricas",
  "monitora_relatorios_analiticos_ajustar_radiometria",
  "monitora_relatorios_analiticos_avaliar_rgb_mapa"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (as.character(x[[1L]])[1L] %in% c("<-", "=") &&
      length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos) {
    eval(x, envir = env)
  }
  if (length(x) > 1L) {
    for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  }
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

suppressPackageStartupMessages(library(terra))
aoi <- env$monitora_relatorios_analiticos_aoi_satelite(c(
  xmin = -53, ymin = -19, xmax = -52, ymax = -18
))
template <- env$monitora_relatorios_analiticos_template_satelite(
  aoi,
  resolucao_m = 10,
  max_celulas = 2000000
)
stopifnot(
  terra::ncell(template) <= 2030000,
  terra::ncol(template) > 1300L,
  all(terra::res(template) >= 10)
)

status <- env$monitora_relatorios_analiticos_status_mapa_satelite(TRUE)
stopifnot(all(c(
  "resolucao_fonte_m",
  "resolucao_alvo_render_m",
  "limite_resolucao_aceitavel_m",
  "qualidade_resolucao_aprovada",
  "densidade_impressao_ppi",
  "qualidade_densidade_aprovada",
  "qualidade_radiometrica_aprovada",
  "tempo_orcamento_mosaico_seg",
  "tempo_mosaico_rgb_seg"
) %in% names(status)))

uas_compactas <- data.table::data.table(
  long_ini = -52.0005, long_fin = -51.9995, lon_meio = -52,
  lat_ini = -18.0005, lat_fin = -17.9995, lat_meio = -18
)
bbox_compacto <- env$monitora_relatorios_analiticos_bbox_satelite(uas_compactas)
largura_m_aprox <- diff(bbox_compacto[c("xmin", "xmax")]) *
  111320 * cos(mean(bbox_compacto[c("ymin", "ymax")]) * pi / 180)
stopifnot(largura_m_aprox >= 10300)

rgb_teste <- terra::rast(
  nrows = 800, ncols = 1040,
  xmin = 0, xmax = 10400, ymin = 0, ymax = 8000,
  crs = "EPSG:3857", nlyrs = 3
)
set.seed(2916)
cinza_teste <- pmax(
  0,
  pmin(255, stats::rnorm(terra::ncell(rgb_teste), 38, 6))
)
valores_teste <- cbind(cinza_teste, cinza_teste, cinza_teste)
terra::values(rgb_teste) <- valores_teste
aoi_teste <- terra::as.polygons(terra::ext(rgb_teste))
terra::crs(aoi_teste) <- terra::crs(rgb_teste)
avaliacao <- env$monitora_relatorios_analiticos_avaliar_rgb_mapa(
  rgb_teste, aoi_teste
)
stopifnot(
  isTRUE(avaliacao$ajuste_aplicado),
  isTRUE(avaliacao$qualidade_densidade_aprovada),
  avaliacao$densidade_impressao_ppi >= 150,
  avaliacao$pos$brilho > avaliacao$pre$brilho,
  avaliacao$pos$n_alertas <= avaliacao$pre$n_alertas
)

tiles <- data.frame(
  id = c("oeste", "centro", "leste_redundante"),
  item_xmin = c(0, 1, 1.8), item_ymin = c(0, 0, 0),
  item_xmax = c(1.2, 2.2, 3), item_ymax = c(1, 1, 1),
  nuvens_catalogo_pct = c(0, 0, 0)
)
tiles_minimos <- env$monitora_relatorios_analiticos_tiles_minimos_bbox(
  tiles,
  c(xmin = 0, ymin = 0, xmax = 2, ymax = 1),
  tamanho_grade = 80L
)
stopifnot(
  identical(as.character(tiles_minimos$id), c("oeste", "centro")),
  !"leste_redundante" %in% tiles_minimos$id
)

cat("TEST_V2916_SENTINEL_QUALIDADE_PERFORMANCE_OK\n")
