#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.6.R",
  mustWork = TRUE
)
run15_arg <- if (length(args) >= 2L) args[[2L]] else Sys.getenv("MONITORA_QA_RUN15", unset = "")
if (!nzchar(run15_arg)) {
  stop("Informe a run de homologação como segundo argumento ou MONITORA_QA_RUN15.", call. = FALSE)
}
run15 <- normalizePath(run15_arg, mustWork = TRUE)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      identical(as.character(x[[2L]]), "monitora_linhagem_inventario_sessoes_dt") &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(exists("monitora_linhagem_inventario_sessoes_dt", envir = env, inherits = FALSE))

entrada <- fread(
  file.path(run15, "input", "linhagem", "correcoes_semanticas_consolidada.csv"),
  colClasses = "character", na.strings = NULL, showProgress = FALSE
)
saida <- fread(
  file.path(run15, "output", "02_painel_correcoes", "linhagem", "correcoes_semanticas_consolidada.csv"),
  colClasses = "character", na.strings = NULL, showProgress = FALSE
)
apps <- fread(
  file.path(run15, "output", "02_painel_correcoes", "linhagem", "aplicacoes_correcoes.csv"),
  colClasses = "character", na.strings = NULL, showProgress = FALSE
)
ids_entrada <- unique(as.character(entrada$event_id))
novos <- saida[!(event_id %in% ids_entrada)]

env$MONITORA_TRILHA_SEMANTICA_HERDADA <- entrada
env$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table()
env$MONITORA_TRILHA_SEMANTICA_SESSAO <- novos
env$MONITORA_REPLAY_APLICACOES <- apps
env$MONITORA_EXEC_ID <- "20260811_145524"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.5-r05-20260811"
env$MONITORA_LINHAGEM_STATUS <- "historico_incremental_importado"
env$MONITORA_LINHAGEM_MANIFESTO <- file.path(
  run15, "output", "02_painel_correcoes", "linhagem", "manifesto_linhagem.json"
)
env$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- data.table(
  exec_id = "20260811_145524", responsavel_sessao = "Bolsista homologação",
  data_hora = "2026-08-11 14:55:24 -0300", instituicao = "CBC/ICMBio",
  modo_execucao = "painel_incremental_completo",
  acao_encerramento = "salvar_checkpoint_e_fechar",
  n_operacoes_sessao = 15L, n_itens_auditaveis_sessao = 15L
)

resultado <- env$monitora_linhagem_inventario_sessoes_dt()
valor <- function(chave) as.character(resultado$resumo[indicador == chave, valor][1L])

stopifnot(
  valor("execucoes_rodadas_registradas") == "15",
  valor("sessoes_com_decisoes_semanticas") == "14",
  valor("execucoes_sem_decisoes_novas") == "1",
  valor("eventos_herdados_ja_materializados") == "525",
  valor("decisoes_novas_sessao_atual") == "15",
  valor("eventos_semanticos_acumulados") == "540",
  nrow(resultado$sessoes) == 15L,
  resultado$sessoes[classificacao_sessao == "execucao_sem_decisoes_novas", .N] == 1L,
  resultado$sessoes[exec_id == "20260811_145524", instituicao] == "CBC/ICMBio",
  resultado$sessoes[exec_id == "20260811_145524", acao_encerramento] == "salvar_checkpoint_e_fechar",
  resultado$sessoes[exec_id == "20260811_145524", n_operacoes_sessao] == 15L,
  resultado$integridade[verificacao == "eventos_herdados_ausentes_no_acumulado", valor] == "0",
  resultado$integridade[verificacao == "eventos_herdados_alterados", valor] == "0",
  resultado$tempo_s < 2
)

cat(sprintf(
  "OK v2.9.6 inventário de sessões PNCV: rodadas=%s; sessões com decisões=%s; sem decisões=%s; herdados=%s; atuais=%s; acumulados=%s; %.4fs\n",
  valor("execucoes_rodadas_registradas"),
  valor("sessoes_com_decisoes_semanticas"),
  valor("execucoes_sem_decisoes_novas"),
  valor("eventos_herdados_ja_materializados"),
  valor("decisoes_novas_sessao_atual"),
  valor("eventos_semanticos_acumulados"),
  resultado$tempo_s
))
