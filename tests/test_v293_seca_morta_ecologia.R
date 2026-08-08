#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R",
  mustWork = TRUE
)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nm <- as.character(x[[2L]])
    if (nm %in% c("monitora_diag_rel_catalogo_ocorrencias_base", "monitora_diag_seca_morta_ocorrencias_revisao")) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))

env$MONITORA_COL_ROW_ID <- ".monitora_row_id"
env$monitora_correcao_colunas_chave <- function(dt) list(tipo_forma_vida = "Encostam")
env$monitora_correcao_colunas_forma_vida_categoria <- function(dt, categoria) {
  if (identical(categoria, "seca_morta")) "forma_seca" else character()
}
env$monitora_relatorio_exoticas_normalizar_token <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]+", "_", x)
}
env$monitora_relatorio_exoticas_tem_token <- function(x, tokens) {
  toks <- lapply(strsplit(tolower(as.character(x)), "[[:space:],;|]+"), unique)
  vapply(toks, function(z) any(z %in% tokens), logical(1L))
}

fixture <- data.table(
  .monitora_row_id = paste0("r", 1:4),
  Encostam = c("nativa seca_morta", "seca_morta", "nativa", "seca_morta"),
  forma_seca = c("graminoide arbusto_abaixo", "", "", "arvore_acima")
)
det <- env$monitora_diag_seca_morta_ocorrencias_revisao(fixture)
stopifnot(
  nrow(det) == 3L,
  setequal(det$linha_indice, c(1L, 4L)),
  setequal(det$forma_de_vida_detectada, c("graminoide", "arbusto_abaixo", "arvore_acima"))
)

catalogo <- env$monitora_diag_rel_catalogo_ocorrencias_base()
stopifnot(
  catalogo[tipo_ocorrencia == "seca_morta_em_revisao", severidade] == "revisao",
  catalogo[tipo_ocorrencia == "seca_morta_sem_forma_vida", severidade] == "impeditiva"
)

codigo <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('vida_cols <- unname\\(MONITORA_COLUNAS_FORMA_VIDA_STAT\\[c\\("nativa", "exotica"\\)\\]\\)', codigo),
  grepl('"bromelioide"', codigo, fixed = TRUE),
  grepl('revisao_nao_impeditiva_sem_inferencia_causal', codigo, fixed = TRUE),
  grepl('achados_hipoteses_e_linhas_de_pesquisa.csv', codigo, fixed = TRUE),
  grepl('Cobertura e proporção relativa', codigo, fixed = TRUE)
)

cat("V293_SECA_MORTA_ECOLOGIA_TESTS_OK\n")
