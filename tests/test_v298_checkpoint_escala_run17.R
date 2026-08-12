#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.8.R",
  mustWork = TRUE
)
pncv_arquivo <- Sys.getenv("MONITORA_TESTE_PNCV_DADOS", unset = "")
checkpoint_run17 <- Sys.getenv("MONITORA_TESTE_RUN17_CHECKPOINT", unset = "")
stopifnot(file.exists(pncv_arquivo), file.exists(checkpoint_run17))

parsed <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    carregar <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    carregar <- carregar || grepl("^MONITORA_COL_", nome) || nome == "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS"
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
for (expr in parsed) carregar_definicoes(expr)

dir_teste <- tempfile("v298_checkpoint_escala_run17_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_CORRECOES_DIR <- file.path(env$MONITORA_OUTPUT_DIR, "correcoes_campos")
env$MONITORA_EXEC_ID <- "v298_checkpoint_escala_run17"
env$MONITORA_SCRIPT_VERSAO <- "2.9.8-dev"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.8-test"
dir.create(env$MONITORA_CORRECOES_DIR, recursive = TRUE)

dados <- fread(
  pncv_arquivo, encoding = "UTF-8", na.strings = NULL,
  colClasses = "character", showProgress = FALSE
)
legado <- readRDS(checkpoint_run17)
stopifnot(
  identical(legado$checkpoint_schema, "monitora_justificativas_checkpoint_v1"),
  nrow(legado$justificativas_ativas) == 3583L
)

template <- env$monitora_correcao_template()
operacoes <- template[rep(NA_integer_, 30L)]
operacoes[, `:=`(
  id_correcao = sprintf("V298_RUN17_RECUPERADA_%02d", seq_len(.N)),
  tipo_correcao = "edicao_campo", ordem_operacao = as.character(seq_len(.N)),
  coleta = as.character(seq_len(.N)), atributo_coluna_registros_corrig = "atributo_teste",
  acao = "update", valor_original_esperado = "antes", valor_novo = "depois"
)]
auditoria_falha <- data.table(
  id_correcao = operacoes$id_correcao[1L],
  status = "falha_preview_precondicao",
  mensagem = "Homologação de preservação integral na escala da run17"
)

inicio <- proc.time()[["elapsed"]]
arquivo <- env$monitora_pendencias_justificativas_checkpoint_recuperavel(
  eventos_sessao = legado$justificativas_ativas,
  eventos_encerrados_sessao = legado$justificativas_encerradas,
  auditoria_reconciliacao = legado$auditoria_reconciliacao,
  motivo = "homologacao_escala_run17",
  correcoes_sessao = operacoes,
  correcoes_solicitadas = operacoes,
  correcoes_historico_intencoes = operacoes,
  correcoes_espaciais = data.table(),
  auditoria_preview_falha = auditoria_falha,
  mensagem_preview_falha = "Falha exata homologada",
  assinatura_preview_falha = "assinatura_run17",
  fingerprint_base = env$monitora_painel_checkpoint_fingerprint_base(dados)
)
duracao_gravacao <- proc.time()[["elapsed"]] - inicio
stopifnot(file.exists(arquivo), file.info(arquivo)$size > 0L)

env$monitora_output_organizar_produtos(
  env$MONITORA_OUTPUT_DIR, env$MONITORA_EXEC_ID,
  contexto = "pos_relatorios_diagnosticos_pre_painel"
)
stopifnot(!file.exists(arquivo))

inicio_localizacao <- proc.time()[["elapsed"]]
localizado <- env$monitora_painel_checkpoint_localizar(dados)
duracao_localizacao <- proc.time()[["elapsed"]] - inicio_localizacao
stopifnot(isTRUE(localizado$encontrado), identical(localizado$status, "checkpoint_integral_v2"))
obj <- localizado$objeto
stopifnot(
  nrow(obj$correcoes_sessao) == 30L,
  nrow(obj$correcoes_solicitadas) == 30L,
  nrow(obj$correcoes_historico_intencoes) == 30L,
  nrow(obj$justificativas_ativas) == 3583L,
  nrow(obj$auditoria_preview_falha) == 1L,
  identical(obj$filas_sha256, env$monitora_painel_checkpoint_assinatura_filas(
    obj$correcoes_sessao, obj$correcoes_solicitadas, obj$correcoes_historico_intencoes,
    obj$correcoes_espaciais, obj$justificativas_ativas, obj$justificativas_encerradas
  ))
)

dados_divergentes <- data.table::copy(dados)
coluna_mutavel <- setdiff(names(dados_divergentes), env$MONITORA_COL_ROW_ID)[1L]
data.table::set(
  dados_divergentes, i = 1L, j = coluna_mutavel,
  value = paste0(as.character(dados_divergentes[[coluna_mutavel]][1L]), "__BASE_DIVERGENTE_V298")
)
rejeitado <- env$monitora_painel_checkpoint_localizar(dados_divergentes)
stopifnot(
  !isTRUE(rejeitado$encontrado),
  any(grepl("base_divergente", as.character(rejeitado$rejeicoes), fixed = TRUE))
)
env$monitora_pendencias_justificativas_checkpoint_remover(localizado$arquivo)
restantes <- unlist(lapply(
  env$monitora_painel_checkpoint_diretorios(),
  list.files, pattern = "^checkpoint_recuperavel_(painel|justificativas)_", full.names = TRUE
), use.names = FALSE)
stopifnot(!length(restantes))

cat(sprintf(
  "TEST_V298_CHECKPOINT_ESCALA_RUN17_OK: linhas_base=%d; correcoes=%d; justificativas=%d; gravacao=%.3fs; localizacao_validacao=%.3fs\n",
  nrow(dados), nrow(obj$correcoes_sessao), nrow(obj$justificativas_ativas),
  duracao_gravacao, duracao_localizacao
))
