#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.5.R",
  mustWork = TRUE
)
entrada_arg <- if (length(args) >= 2L) args[[2L]] else
  Sys.getenv("MONITORA_TESTE_RETOMADA_REAL", unset = "")
if (!nzchar(entrada_arg) || !file.exists(entrada_arg)) {
  stop(
    "Informe um registros_corrig.csv real no segundo argumento ou em MONITORA_TESTE_RETOMADA_REAL.",
    call. = FALSE
  )
}
entrada <- normalizePath(entrada_arg, mustWork = TRUE)

parsed <- parse(file = script, keep.source = FALSE)
body <- as.call(c(list(as.name("{")), as.list(parsed)))
env <- new.env(parent = globalenv())

carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    carregar <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    carregar <- carregar || grepl("^MONITORA_COL_", nome) ||
      nome == "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS"
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

stopifnot(exists(
  "monitora_coletores_repeat_sanitizar_legado",
  envir = env, inherits = FALSE
))
stopifnot(exists(
  "monitora_correcao_corrigir_ponto_metro",
  envir = env, inherits = FALSE
))
dt <- fread(entrada, encoding = "UTF-8", showProgress = FALSE)
stopifnot(nrow(dt) >= 1000L)

raiz <- tempfile("v295_perf_retomada_")
dir.create(raiz, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(raiz, "output")
env$MONITORA_LOG_DIR <- file.path(raiz, "log")
env$MONITORA_EXEC_ID <- "v295_perf_retomada"
env$MONITORA_SCRIPT_VERSAO <- "2.9.5-dev-test"
env$monitora_log_registrar_evento <- function(...) invisible(TRUE)
env$monitora_fwrite <- function(x, arquivo, ...) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, ...)
}

tempo <- system.time({
  res <- env$monitora_coletores_repeat_sanitizar_legado(
    dt,
    output_dir = env$MONITORA_OUTPUT_DIR,
    log_dir = env$MONITORA_LOG_DIR,
    exec_id = env$MONITORA_EXEC_ID,
    contexto = "teste_retomada_real_sem_legado",
    abortar_nao_reconhecido = TRUE
  )
})[["elapsed"]]

env$MONITORA_CORRIGIR_PONTO_METRO_AUTOMATICO <- TRUE
tempo_ponto_metro <- system.time({
  dt_ponto_metro <- env$monitora_correcao_corrigir_ponto_metro(dt)
})[["elapsed"]]

stopifnot(
  isTRUE(res$ok),
  !isTRUE(res$alterou),
  nrow(res$auditoria) == 0L,
  is.finite(tempo),
  tempo < 5,
  nrow(dt_ponto_metro) == nrow(dt),
  is.finite(tempo_ponto_metro),
  tempo_ponto_metro < 5
)

cat(sprintf(
  paste0(
    "TEST_V295_PERFORMANCE_RETOMADA_REAL_OK linhas=%d colunas=%d ",
    "sanitizacao_sem_legado=%.3fs ponto_metro=%.3fs\n"
  ),
  nrow(dt), ncol(dt), tempo, tempo_ponto_metro
))
