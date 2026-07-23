args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Uso: Rscript tests/benchmark_spatial_v280.R SCRIPT REGISTROS_CORRIG", call. = FALSE)
suppressPackageStartupMessages(library(data.table))
exprs <- parse(normalizePath(args[[1L]], mustWork = TRUE), keep.source = FALSE, encoding = "UTF-8")
for (expr in exprs) {
  if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
  rhs <- expr[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
}
MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 20
MONITORA_RAIO_ALERTA_ESPACIAL_M <- 10
MONITORA_MIN_COLETAS_CONSENSO_ESPACIAL <- 2L
MONITORA_MIN_COLETAS_CONSENSO_LOO_ESPACIAL <- 1L
MONITORA_VALIDACAO_ESPACIAL_LEAVE_ONE_OUT <- "S"
MONITORA_USAR_ACURACIA_GPS_NA_TRIAGEM <- "S"
MONITORA_ACURACIA_GPS_MAX_ALERTA_M <- 10
MONITORA_VALIDAR_COMPRIMENTO_TRANSECTO <- "S"
MONITORA_COMPRIMENTO_TRANSECTO_ESPERADO_M <- 50
MONITORA_TOLERANCIA_COMPRIMENTO_TRANSECTO_M <- 20
dt <- fread(normalizePath(args[[2L]], mustWork = TRUE), encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)
t_prepare <- system.time(coletas <- monitora_esp_preparar_coletas(dt))[["elapsed"]]
t_validate <- system.time(res <- monitora_esp_validar_coletas(coletas))[["elapsed"]]
cat(sprintf("BENCHMARK_SPATIAL script=%s linhas=%d coletas=%d prepare=%.3f validate=%.3f total=%.3f\n", basename(args[[1L]]), nrow(dt), nrow(coletas), t_prepare, t_validate, t_prepare + t_validate))
