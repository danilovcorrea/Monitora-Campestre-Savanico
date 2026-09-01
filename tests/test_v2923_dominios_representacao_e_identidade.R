#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.23.R",
  mustWork = TRUE
)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    try(eval(x, env), silent = TRUE)
  }
  if (length(x) > 1L) for (ii in 2:length(x)) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
for (nm in c(
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_CONTRATO_MOVIMENTO_CACHE",
  ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nm, new.env(parent = emptyenv()), envir = env)

col_form_veg <- "amostragem/registro/form_veg"
col_forma_nativa <- "amostragem/registro/forma_vida_nativa"
dt <- data.table(
  COLETA = c("A", "B", "C", "D"),
  MONITORA_ROW_ID = paste0("row_", 1:4),
  form_veg = c("campestre", "Campestre", "Savânica", "CAMPRESTRE"),
  forma_nativa = c("graminoide", "outra", "graminoide", "graminoide")
)
setnames(dt, c("form_veg", "forma_nativa"), c(col_form_veg, col_forma_nativa))

aud <- env$monitora_contrato_validar_dominios_dataset_pre_painel(dt)
assert(
  nrow(aud[caminho_registro == col_form_veg]) == 1L &&
    aud[caminho_registro == col_form_veg, linha_indice][1L] == 4L &&
    aud[caminho_registro == col_form_veg, token_invalido][1L] == "CAMPRESTRE",
  "Labels atuais e exatos de select_one não foram reconhecidos pelo próprio contrato, ou grafia não contratual foi aceita."
)
assert(
  nrow(aud[caminho_registro == col_forma_nativa]) == 1L &&
    aud[caminho_registro == col_forma_nativa, linha_indice][1L] == 2L &&
    aud[caminho_registro == col_forma_nativa, token_invalido][1L] == "outra",
  "Token histórico realmente fora do domínio deixou de permanecer impeditivo."
)

ocorrencias <- aud[, .(
  tipo_ocorrencia = "token_fora_dominio_contrato",
  monitora_row_id = dt$MONITORA_ROW_ID[linha_indice],
  linha_indice, COLETA,
  caminho_registro, list_name, token_invalido
)]
chaves <- env$monitora_diag_chave_ocorrencia(ocorrencias)
assert(
  length(chaves) == nrow(ocorrencias) && !anyDuplicated(chaves),
  "Ocorrências de domínio distintas na mesma linha ainda colidem na chave diagnóstica."
)

ids <- env$monitora_pendencias_ocorrencia_id(ocorrencias)
assert(
  length(ids) == nrow(ocorrencias) && !anyDuplicated(ids),
  "Ocorrências de domínio distintas na mesma linha ainda colidem na identidade persistente."
)

oc_mesma_linha <- data.table(
  tipo_ocorrencia = "token_fora_dominio_contrato",
  monitora_row_id = "row_composta", linha_indice = 10L, COLETA = "E",
  caminho_registro = c(col_form_veg, col_forma_nativa),
  list_name = c("form_veg", "forma_vida_nativa"),
  token_invalido = c("CAMPRESTRE", "outra")
)
assert(
  !anyDuplicated(env$monitora_diag_chave_ocorrencia(oc_mesma_linha)) &&
    !anyDuplicated(env$monitora_pendencias_ocorrencia_id(oc_mesma_linha)),
  "Duas pendências contratuais diferentes na mesma linha não receberam identidades distintas."
)

cat("TEST_V2923_DOMINIOS_REPRESENTACAO_E_IDENTIDADE_OK\n")
