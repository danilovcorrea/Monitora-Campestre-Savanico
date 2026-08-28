#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 3L)
script <- normalizePath(args[[1L]], mustWork = TRUE)
dados_path <- normalizePath(args[[2L]], mustWork = TRUE)
ledger_path <- normalizePath(args[[3L]], mustWork = TRUE)

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
    carregar <- carregar || grepl("^MONITORA_COL_", nome) || nome %in% c(
      "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS", "MONITORA_SCRIPT_VERSAO",
      "MONITORA_SCRIPT_BUILD_ID"
    )
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

dir_teste <- tempfile("v2919_sanhab_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_LOG_DIR <- file.path(dir_teste, "log")
env$MONITORA_CORRECOES_DIR <- file.path(dir_teste, "output", "correcoes")
dir.create(env$MONITORA_LOG_DIR, recursive = TRUE)
dir.create(env$MONITORA_CORRECOES_DIR, recursive = TRUE)
env$MONITORA_EXEC_ID <- "v2919_sanhab_invalido_atomico"
env$MONITORA_LOG_EXECUCAO <- data.table(
  etapa = character(), severidade = character(), arquivo = character(),
  detalhe = character(), acao = character()
)
env$MONITORA_DEPENDENCIAS_CORRECOES <- data.table()
env$MONITORA_PERF_ENABLED <- FALSE
env$MONITORA_AUDITORIA_SEMANTICA_CORRECOES_COMPLETA <- FALSE
for (nome_cache in c(
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nome_cache, new.env(parent = emptyenv()), envir = env)

dt <- fread(dados_path, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
ledger <- fread(ledger_path, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
op <- tail(ledger[grepl("^SANHAB", id_correcao)], 1L)
stopifnot(nrow(op) == 1L)

habito <- as.character(op$atributo_coluna_registros_corrig[1L])
stopifnot(length(habito) == 1L, habito %in% names(dt))

chaves <- env$monitora_correcao_colunas_chave(dt)
rec <- env$monitora_correcao_reconciliar_sanhab_estado_corrente(
  dt, op, chaves = chaves, dicionario = NULL,
  indice = env$monitora_correcao_criar_indice_linhas(dt, chaves),
  arquivo_correcao = ledger_path
)
stopifnot(
  nrow(rec$corr) == 1L,
  rec$n_removidos == 0L,
  nrow(rec$audit) == 0L,
  as.integer(rec$corr$n_linhas_efetivas[1L]) > 0L
)
ids_efetivos <- env$monitora_correcao_parse_ids_estaveis(
  rec$corr$alvos_efetivos_monitora_row_id[1L]
)
idx <- match(ids_efetivos, as.character(dt[[env$MONITORA_COL_ROW_ID]]))
idx <- idx[!is.na(idx)]
n_efetivos <- as.integer(rec$corr$n_linhas_efetivas[1L])
stopifnot(length(idx) == n_efetivos)
dominio_valido <- c("terrestre", "epifita", "rupicola")
stopifnot(all(!tolower(trimws(as.character(dt[[habito]][idx]))) %chin% dominio_valido))

res <- env$monitora_correcao_aplicar_plano_atomico_sessao(
  dt, rec$corr, chaves = chaves, dicionario = NULL,
  arquivo_correcao = "teste_v2919_sanhab_invalido_atomico",
  aplicar_exclusoes = TRUE, silencioso = TRUE, registrar_perf = FALSE
)
valor_novo <- as.character(op$valor_novo[1L])
stopifnot(!isTRUE(res$falha), all(as.character(res$dt[[habito]][idx]) == valor_novo))

det_pos <- env$monitora_correcao_ocorrencias_habito_detalhadas(res$dt)
stopifnot(!any(as.integer(det_pos$linha_indice) %in% idx))

# Replay idempotente: hábito já válido não pode ser sobrescrito ou voltar a ser alvo.
chaves_pos <- env$monitora_correcao_colunas_chave(res$dt)
rec_pos <- env$monitora_correcao_reconciliar_sanhab_estado_corrente(
  res$dt, op, chaves = chaves_pos, dicionario = NULL,
  indice = env$monitora_correcao_criar_indice_linhas(res$dt, chaves_pos),
  arquivo_correcao = ledger_path
)
stopifnot(
  nrow(rec_pos$corr) == 0L,
  rec_pos$n_removidos == n_efetivos,
  all(as.character(res$dt[[habito]][idx]) == valor_novo)
)

# O predicado contratual também deve reter vazio e outro legado inválido não
# vazio. Os dois estados são testados isoladamente para não pressupor que a
# fixture real tenha ao menos duas linhas efetivas na última operação SANHAB.
dt_vazio <- copy(dt)
set(dt_vazio, i = idx[1L], j = habito, value = "")
det_vazio <- env$monitora_correcao_ocorrencias_habito_detalhadas(dt_vazio)
stopifnot(idx[1L] %in% as.integer(det_vazio$linha_indice))

dt_invalido <- copy(dt)
set(dt_invalido, i = idx[1L], j = habito, value = "invalido_legado")
det_invalido <- env$monitora_correcao_ocorrencias_habito_detalhadas(dt_invalido)
stopifnot(idx[1L] %in% as.integer(det_invalido$linha_indice))

cat("TEST_V2919_SANHAB_INVALIDO_ATOMICO_OK: alvo contratual inválido corrigido; replay idempotente; vazio e inválido não vazio reconhecidos\n")
