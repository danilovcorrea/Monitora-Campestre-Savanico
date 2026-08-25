#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(args[[1L]], mustWork = TRUE)
arquivos <- normalizePath(args[-1L], mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (as.character(x[[1L]])[1L] %in% c("<-", "=") && length(x) >= 3L &&
      is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]]), "function")) eval(x, env)
  for (ii in seq_along(x)[-1L]) try(carregar(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar))
env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())
env$monitora_log_registrar_evento <- function(...) invisible(NULL)
env$monitora_fwrite <- function(...) invisible(NULL)

tempos <- vector("list", length(arquivos))
for (ii in seq_along(arquivos)) {
  arq <- arquivos[[ii]]
  schema <- fread(arq, nrows = 0L, encoding = "UTF-8", showProgress = FALSE)
  inicio <- proc.time()[["elapsed"]]
  contexto <- env$monitora_contrato_unico_resolver_contexto_impactos(schema)
  regras <- env$monitora_correcao_contrato_fechamento_hierarquico(schema)
  duracao <- proc.time()[["elapsed"]] - inicio
  assert(
    nrow(contexto) == 4L && all(contexto$status_resolucao == "resolvido_unico"),
    paste0("Schema real não resolveu os quatro campos de contexto: ", arq, " [",
           paste(contexto$papel_contexto, contexto$status_resolucao, sep = "=", collapse = " | "), "]")
  )
  abertas <- regras[
    grepl("outra.*especie|outra_sp", env$monitora_correcao_normalizar_nome_coluna(campo_inferior)) &
      status_regra == "ok"
  ]
  assert(
    !any(env$monitora_correcao_normalizar_nome_coluna(abertas$token) %in% c("outra", "outro", "outras")),
    paste0("Schema real ainda deriva token histórico outra de espécie aberta: ", arq)
  )
  tempos[[ii]] <- data.table(
    arquivo = arq, n_colunas = ncol(schema),
    n_regras = nrow(regras), duracao_resolucao_seg = duracao
  )
}

### Caso dourado histórico APAI 17626/pontos 11 e 23: o conteúdo legítimo do
### campo aberto sustenta arbusto_abaixo, sem reintroduzir o token legado outra.
idx_apai <- which(grepl("/APAI/", arquivos, fixed = TRUE))
if (length(idx_apai)) {
  dt <- fread(arquivos[idx_apai[[1L]]], encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)
  ch <- env$monitora_correcao_colunas_chave(dt)
  linhas <- which(
    as.character(dt[[ch$coleta]]) == "17626" &
      as.character(dt[[ch$ponto_amostral]]) %in% c("11", "23")
  )
  assert(length(linhas) == 2L, "APAI real: caso 17626/11+23 ausente")
  caso <- copy(dt[linhas])
  regras <- env$monitora_correcao_contrato_fechamento_hierarquico(caso)
  rr <- regras[
    status_regra == "ok" & categoria == "exotica" & token == "arbusto_abaixo" &
      grepl("outra.*especie.*arbusto", env$monitora_correcao_normalizar_nome_coluna(campo_inferior))
  ]
  assert(nrow(rr) == 1L, "APAI real: espécie aberta não resolveu para arbusto_abaixo")
  pai <- rr$campo_superior[[1L]]
  for (jj in seq_len(nrow(caso))) {
    toks <- strsplit(trimws(as.character(caso[[pai]][jj])), "[[:space:]]+")[[1L]]
    set(caso, i = jj, j = pai, value = paste(setdiff(toks, c("outra", "outro", "outras")), collapse = " "))
  }
  env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
  env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
  env$monitora_correcao_recalcular_superiores_vinculados(
    caso, seq_len(nrow(caso)), modo_encostam = "acrescentar"
  )
  assert(
    all(env$monitora_correcao_token_presente_vec(caso[[pai]], "arbusto_abaixo")) &&
      !any(env$monitora_correcao_token_presente_vec(caso[[pai]], "outra")),
    "APAI real: fechamento não preservou a correção histórica"
  )
}

resultado <- rbindlist(tempos)
print(resultado[, .(n_colunas, n_regras, duracao_resolucao_seg)])
cat(sprintf(
  "QA_V2917_SCHEMAS_REAIS_OK arquivos=%d max_resolucao=%.3fs\n",
  nrow(resultado), max(resultado$duracao_resolucao_seg)
))
