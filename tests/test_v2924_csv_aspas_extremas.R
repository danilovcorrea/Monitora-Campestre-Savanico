#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.24-dev_r05.R", mustWork = TRUE)
arquivo_real <- if (length(args) >= 2L) normalizePath(args[[2L]], mustWork = TRUE) else NA_character_
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (as.character(x[[1L]])[1L] %in% c("<-", "=") && length(x) >= 3L &&
      is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    try(eval(x, env), silent = TRUE)
  }
  if (length(x) > 1L) for (ii in 2:length(x)) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

env$MONITORA_EXEC_ID <- "gate_aspas_extremas"
env$MONITORA_OUTPUT_DIR <- tempfile("gate_aspas_output_")
env$MONITORA_LOG_DIR <- tempfile("gate_aspas_log_")
dir.create(env$MONITORA_OUTPUT_DIR, recursive = TRUE)
dir.create(env$MONITORA_LOG_DIR, recursive = TRUE)
env$monitora_fwrite <- function(...) invisible(TRUE)
env$monitora_log_registrar_evento <- function(...) invisible(TRUE)

dir_gate <- tempfile("gate_csv_aspas_")
dir.create(dir_gate)
arquivo_extremo <- file.path(dir_gate, "registros_corrig_extremo.csv")
arquivo_ordinario <- file.path(dir_gate, "registros_corrig_ordinario.csv")
valor_extremo <- paste0("antes ", strrep('"', 2048L), " depois")
valor_ordinario <- 'texto com "aspas" ordinárias'
utils::write.csv(
  data.frame(COLETA = "1", texto = valor_extremo, check.names = FALSE),
  arquivo_extremo, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
utils::write.csv(
  data.frame(COLETA = "1", texto = valor_ordinario, check.names = FALSE),
  arquivo_ordinario, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

assert(isTRUE(env$monitora_csv_tem_sequencia_aspas_extrema(arquivo_extremo)), "Detector não identificou a sequência extrema serializada.")
assert(!isTRUE(env$monitora_csv_tem_sequencia_aspas_extrema(arquivo_ordinario)), "Detector classificou aspas ordinárias como extremas.")

lido_extremo <- env$monitora_registros_corrig_ler_csv_normalizado(arquivo_extremo, modo = "gate_aspas_extremas")
lido_ordinario <- env$monitora_registros_corrig_ler_csv_normalizado(arquivo_ordinario, modo = "gate_aspas_ordinarias")
assert(identical(lido_extremo$texto[[1L]], valor_extremo), "Fallback não preservou literalmente as 2.048 aspas do valor extremo.")
assert(identical(lido_ordinario$texto[[1L]], valor_ordinario), "Leitor selecionado alterou texto com aspas ordinárias.")

if (!is.na(arquivo_real)) {
  alvo <- "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)"
  assert(isTRUE(env$monitora_csv_tem_sequencia_aspas_extrema(arquivo_real)), "Arquivo real RVSVOB não acionou o fallback esperado.")
  t0 <- proc.time()[["elapsed"]]
  real <- env$monitora_registros_corrig_ler_csv_normalizado(arquivo_real, modo = "gate_real_rvsvob")
  duracao <- proc.time()[["elapsed"]] - t0
  assert(nrow(real) == 23533L && ncol(real) == 198L, "Estrutura real da RVSVOB divergiu após o fallback.")
  assert(alvo %in% names(real), "Campo textual real da RVSVOB não foi preservado.")
  valores <- as.character(real[COLETA == "11014"][[alvo]])
  n_aspas <- lengths(regmatches(valores, gregexpr('"', valores, fixed = TRUE)))
  assert(length(valores) == 101L && all(n_aspas == 2048L), "Fallback não preservou as 2.048 aspas literais nas 101 linhas reais da COLETA 11014.")
  cat(sprintf("TEST_V2924_CSV_ASPAS_EXTREMAS_REAL_OK|seg=%.3f\n", duracao))
} else {
  cat("TEST_V2924_CSV_ASPAS_EXTREMAS_SINTETICO_OK\n")
}
