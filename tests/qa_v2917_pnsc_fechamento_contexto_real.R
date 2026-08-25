#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(args[[1L]], mustWork = TRUE)
arquivo_real <- normalizePath(args[[2L]], mustWork = TRUE)
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
env$monitora_fwrite <- function(...) invisible(NULL)
env$monitora_diag_rel_write_dt <- function(...) invisible(NULL)
env$monitora_log_registrar_evento <- function(...) invisible(NULL)

dt <- fread(arquivo_real, encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)
ch <- env$monitora_correcao_colunas_chave(dt)
assert(!is.na(ch$coleta) && !is.na(ch$ponto_amostral), "PNSC real: chaves COLETA/ponto não resolvidas")

col_pai <- env$monitora_correcao_coluna_forma_vida(dt, "exotica")
col_filho <- "Outra espécie de erva não graminoide exótica: (amostragem/registro)"
idx_42512 <- which(
  as.character(dt[[ch$coleta]]) == "42512" &
    as.character(dt[[ch$ponto_amostral]]) == "11"
)
assert(length(idx_42512) == 1L && col_filho %in% names(dt), "PNSC real: caso 42512/p11 ausente")
caso <- copy(dt[idx_42512])
tokens_pai <- strsplit(trimws(as.character(caso[[col_pai]][1L])), "[[:space:]]+")[[1L]]
tokens_pai <- setdiff(tokens_pai, c("outra", "outro", "outras"))
set(caso, i = 1L, j = col_pai, value = paste(tokens_pai, collapse = " "))
antes <- as.character(caso[[col_pai]][1L])
env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
aud <- env$monitora_correcao_recalcular_superiores_vinculados(
  caso, 1L, modo_encostam = "acrescentar"
)
depois <- as.character(caso[[col_pai]][1L])
assert(identical(antes, depois), paste0("PNSC real: fechamento alterou pai: ", antes, " -> ", depois))
assert(!env$monitora_correcao_token_presente_vec(depois, "outra"), "PNSC real: token outra foi reinjetado")
assert(env$monitora_correcao_token_presente_vec(depois, "erva_nao_graminoide"), "PNSC real: forma correta foi perdida")
assert(
  !nrow(aud) || !("token" %in% names(aud)) || !any(
    !is.na(aud$token) &
      env$monitora_correcao_normalizar_nome_coluna(aud$token) %in% c("outra", "outro", "outras")
  ),
  "PNSC real: auditoria registrou ascensão indevida de outra"
)

coletas_contexto <- c("13022", "13021", "11902", "11464", "11463")
idx_contexto <- vapply(coletas_contexto, function(id) {
  hit <- which(as.character(dt[[ch$coleta]]) == id)
  if (length(hit)) hit[[1L]] else NA_integer_
}, integer(1L))
assert(all(!is.na(idx_contexto)), "PNSC real: uma das cinco COLETAs de contexto não foi localizada")
ocorrencias <- data.table(
  linha_indice = idx_contexto,
  forma_de_vida_detectada = "graminoide"
)
rel <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
  dt, ocorrencias, tempfile("qa_v2917_pnsc_real_"), "pre_painel"
)
alvo <- rel$operacional[COLETA %in% coletas_contexto]
assert(nrow(alvo) == 5L, "PNSC real: relatório não preservou as cinco COLETAs")
assert(
  all(alvo$status_resolucao_contexto == "resolvido_integral"),
  paste0("PNSC real: resolução contextual incompleta: ", paste(unique(alvo$status_resolucao_contexto), collapse = " | "))
)
assert(
  all(alvo$contexto_fogo == "fogo_explicito_em_campo_estruturado"),
  paste0("PNSC real: contexto de fogo não reconhecido: ", paste(alvo$COLETA, alvo$contexto_fogo, sep = "=", collapse = " | "))
)
assert(
  all(nzchar(alvo$tipos_impacto_manejo_uso)),
  "PNSC real: tipos de impacto foram esvaziados no relatório"
)

cat(sprintf(
  "QA_V2917_PNSC_REAL_OK linhas=%d coletas_contexto=%d\n",
  nrow(dt), nrow(alvo)
))
