#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.6.R",
  mustWork = TRUE
)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_pendencias_ocorrencia_id",
  "monitora_pendencias_justificativas_template",
  "monitora_pendencias_justificativas_normalizar_sessao",
  "monitora_pendencias_justificativas_validar_sessao",
  "monitora_pendencias_justificativas_criar_lote",
  "monitora_pendencias_justificativas_adicionar_lote_atomico",
  "monitora_pendencias_justificativas_reconstituir_lotes",
  "monitora_pendencias_justificativas_reconciliar_sessao",
  "monitora_pendencias_justificativas_ler_historico",
  "monitora_pendencias_justificativas_preparar",
  "monitora_pendencias_justificativas_publicar_par_atomico",
  "monitora_pendencias_justificativas_persistir_preparado",
  "monitora_painel_transacao_arquivos_iniciar",
  "monitora_painel_transacao_arquivos_rollback",
  "monitora_painel_transacao_arquivos_finalizar",
  "monitora_pendencias_justificativas_checkpoint_recuperavel",
  "monitora_pendencias_justificativas_checkpoint_remover"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nome <- as.character(x[[2L]])
    if (nome %in% alvos) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

env$monitora_correcao_hash_texto <- function(x) {
  vapply(as.character(x), digest::digest, character(1L), algo = "sha256", serialize = FALSE)
}
env$monitora_fwrite <- function(x, arquivo, ...) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, na = "")
}
env$MONITORA_EXEC_ID <- "v296_just_tx"
env$MONITORA_SCRIPT_VERSAO <- "2.9.6"
dir_teste <- tempfile("v296_just_tx_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_INPUT_DIR <- file.path(dir_teste, "input")
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_CORRECOES_DIR <- file.path(env$MONITORA_OUTPUT_DIR, "correcoes_campos")
dir.create(env$MONITORA_INPUT_DIR, recursive = TRUE)

oc <- data.table(
  ocorrencia_id = paste0("occ_", 1:3),
  tipo_ocorrencia = c("seca_morta_em_revisao", "seca_morta_em_revisao", "pendencia_espacial"),
  COLETA = c("C1", "C2", "C3"), UC = "UC Teste", EA = "EA1",
  UA = c("UA1", "UA1", "UA2"), ANO = c("2025", "2026", "2026")
)
lote <- env$monitora_pendencias_justificativas_criar_lote(
  oc, "Responsável teste", "pendencia_legitima",
  "Justificativa auditável com documentação suficiente para o teste.",
  "2026-08-11 12:00:00.000001"
)
adicao <- env$monitora_pendencias_justificativas_adicionar_lote_atomico(
  env$monitora_pendencias_justificativas_template(), lote$dados, oc
)
stopifnot(isTRUE(lote$ok), isTRUE(adicao$ok), nrow(adicao$dados) == 3L)

rec <- env$monitora_pendencias_justificativas_reconciliar_sessao(
  adicao$dados, oc[ocorrencia_id != "occ_3"],
  "2026-08-11 12:05:00.000001"
)
validacao_ativas <- env$monitora_pendencias_justificativas_validar_sessao(
  rec$ativas, oc[ocorrencia_id != "occ_3"]
)
stopifnot(
  rec$n_ativas == 2L,
  rec$n_encerradas == 1L,
  nrow(rec$eventos_encerrados) == 2L,
  isTRUE(validacao_ativas$ok),
  all(rec$ativas$n_ocorrencias_lote == 2L),
  setequal(rec$eventos_encerrados$status_evento, c("vigente", "encerrada_por_resolucao")),
  identical(
    rec$eventos_encerrados[status_evento == "encerrada_por_resolucao", timestamp_evento],
    "2026-08-11 12:05:00.000001"
  ),
  identical(rec$auditoria$timestamp_reconciliacao, "2026-08-11 12:05:00.000001"),
  rec$eventos_encerrados[status_evento == "encerrada_por_resolucao", max(.N), by = ocorrencia_id]$V1 <= 1L
)

preparado <- env$monitora_pendencias_justificativas_preparar(
  ocorrencias_atuais = oc[ocorrencia_id != "occ_3"],
  eventos_sessao = rec$ativas,
  eventos_encerrados_sessao = rec$eventos_encerrados
)
stopifnot(
  nrow(preparado$historico) == 4L,
  nrow(preparado$pendencias_remanescentes) == 2L,
  all(preparado$pendencias_remanescentes$status_justificativa == "vigente"),
  preparado$historico[
    status_evento == "encerrada_por_resolucao",
    max(.N), by = ocorrencia_id
  ]$V1 <= 1L
)
persistido <- env$monitora_pendencias_justificativas_persistir_preparado(preparado)
stopifnot(
  file.exists(persistido$arquivo_historico),
  file.exists(persistido$arquivo_pendencias),
  nrow(fread(persistido$arquivo_historico)) == 4L
)

oc_404 <- data.table(
  ocorrencia_id = sprintf("occ_perf_%04d", 1:404),
  tipo_ocorrencia = "seca_morta_em_revisao",
  COLETA = sprintf("C%04d", 1:404), UC = "UC Teste", EA = "EA1",
  UA = sprintf("UA%04d", 1:404), ANO = "2026"
)
lote_404 <- env$monitora_pendencias_justificativas_criar_lote(
  oc_404, "Responsável teste", "pendencia_legitima",
  "Justificativa em lote auditável para homologação de desempenho.",
  "2026-08-11 12:10:00.000001"
)
tempo_404 <- system.time({
  rec_404 <- env$monitora_pendencias_justificativas_reconciliar_sessao(
    lote_404$dados, oc_404[101:404], "2026-08-11 12:11:00.000001"
  )
})[["elapsed"]]
stopifnot(
  rec_404$n_ativas == 304L,
  rec_404$n_encerradas == 100L,
  tempo_404 < 2
)

arquivo_existente <- file.path(dir_teste, "transacao", "existente.txt")
arquivo_novo <- file.path(dir_teste, "transacao", "novo.txt")
dir.create(dirname(arquivo_existente), recursive = TRUE)
writeLines("conteudo_anterior", arquivo_existente)
tx <- env$monitora_painel_transacao_arquivos_iniciar(c(arquivo_existente, arquivo_novo))
writeLines("conteudo_parcial", arquivo_existente)
writeLines("arquivo_parcial", arquivo_novo)
env$monitora_painel_transacao_arquivos_rollback(tx)
env$monitora_painel_transacao_arquivos_finalizar(tx)
stopifnot(
  identical(readLines(arquivo_existente), "conteudo_anterior"),
  !file.exists(arquivo_novo)
)

checkpoint <- env$monitora_pendencias_justificativas_checkpoint_recuperavel(
  rec$ativas, rec$eventos_encerrados, rec$auditoria,
  motivo = "homologacao_checkpoint_recuperavel"
)
checkpoint_obj <- readRDS(checkpoint)
stopifnot(
  file.exists(checkpoint),
  identical(checkpoint_obj$checkpoint_schema, "monitora_justificativas_checkpoint_v1"),
  nrow(checkpoint_obj$justificativas_ativas) == 2L,
  nrow(checkpoint_obj$justificativas_encerradas) == 2L
)
env$monitora_pendencias_justificativas_checkpoint_remover()
stopifnot(!file.exists(checkpoint))

cat(sprintf(
  "OK v2.9.6 justificativas/transação: encerradas=%d; ativas=%d; lote404=%.4fs\n",
  rec$n_encerradas, rec$n_ativas, tempo_404
))
