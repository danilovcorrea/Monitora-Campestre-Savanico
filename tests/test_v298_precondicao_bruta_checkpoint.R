#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.8.R",
  mustWork = TRUE
)
texto <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
exigir <- function(trecho) {
  if (!grepl(trecho, texto, fixed = TRUE)) {
    stop("Contrato de precondição bruta ausente: ", trecho, call. = FALSE)
  }
}

for (trecho in c(
  "vals_brutos_norm <- monitora_correcao_na_para_vazio(vals)",
  "valores_brutos_unicos <- unique(vals_brutos_norm)",
  "valor_original_esperado_exato = if (length(valores_brutos_unicos) == 1L)",
  "as.character(resumo_lote$valor_original_esperado_exato[ii])",
  "length(valores_brutos) == 1L && all(iguais_borda) && !all(iguais_exatos)",
  '"nao_migrada_divergencia_material"',
  "data.table::set(z, i = ii, j = \"valor_original_esperado\", value = bruto_i)"
)) exigir(trecho)

cat("TEST_V298_PRECONDICAO_BRUTA_CHECKPOINT_OK\n")
