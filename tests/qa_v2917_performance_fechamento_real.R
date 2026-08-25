#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
scripts <- c(
  base = normalizePath(args[[1L]], mustWork = TRUE),
  candidata = normalizePath(args[[2L]], mustWork = TRUE)
)
arquivo <- normalizePath(args[[3L]], mustWork = TRUE)
dados <- fread(arquivo, encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)

carregar <- function(script) {
  arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
  env <- new.env(parent = globalenv())
  walk <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    if (as.character(x[[1L]])[1L] %in% c("<-", "=") && length(x) >= 3L &&
        is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
        identical(as.character(x[[3L]][[1L]]), "function")) eval(x, env)
    for (ii in seq_along(x)[-1L]) try(walk(x[[ii]]), silent = TRUE)
    invisible(NULL)
  }
  invisible(lapply(as.list(arvore), walk))
  env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
  env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
  env$MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
  env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
  env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
  env$.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())
  env$monitora_log_registrar_evento <- function(...) invisible(NULL)
  env$monitora_fwrite <- function(...) invisible(NULL)
  env
}

ambientes <- lapply(scripts, carregar)
resultado <- rbindlist(lapply(names(ambientes), function(nome) {
  env <- ambientes[[nome]]
  schema <- dados[0L]
  ### Aquecimento idêntico ao fluxo real, que já consulta o contrato no módulo
  ### de reconciliação imediatamente anterior ao fechamento.
  try(env$monitora_contrato_unico_indices_cache(validar = TRUE), silent = TRUE)
  try(env$monitora_correcao_contrato_fechamento_hierarquico(schema), silent = TRUE)
  env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
  tempo_schema <- system.time(
    regras <- env$monitora_correcao_contrato_fechamento_hierarquico(schema)
  )[["elapsed"]]
  copia <- copy(dados)
  tempo_linhas <- system.time(
    env$monitora_correcao_recalcular_superiores_vinculados(
      copia, seq_len(nrow(copia)), modo_encostam = "acrescentar"
    )
  )[["elapsed"]]
  data.table(
    versao = nome, n_linhas = nrow(dados), n_colunas = ncol(dados),
    n_regras = nrow(regras), tempo_schema_s = tempo_schema,
    tempo_fechamento_total_s = tempo_linhas
  )
}))
print(resultado)
base <- resultado[versao == "base", tempo_fechamento_total_s]
novo <- resultado[versao == "candidata", tempo_fechamento_total_s]
stopifnot(
  is.finite(base), is.finite(novo),
  novo <= base * 1.25 + 1.0,
  resultado[versao == "candidata", tempo_schema_s] <= 1.5
)
cat("QA_V2917_PERFORMANCE_FECHAMENTO_REAL_OK\n")
