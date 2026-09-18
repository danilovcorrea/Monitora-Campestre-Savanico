#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Uso: Rscript test_v2926_r03_relatorio_checkpoint_fnb.R <candidata.R> <registros_corrig_fnb.csv> <diretorio_saida>", call. = FALSE)
}

candidata <- normalizePath(args[[1L]], mustWork = TRUE)
registros_path <- normalizePath(args[[2L]], mustWork = TRUE)
destino <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)
if (file.exists(destino) && !dir.exists(destino)) stop("Destino isolado não é um diretório.", call. = FALSE)
dir.create(file.path(destino, "input"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "output"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "log"), recursive = TRUE, showWarnings = FALSE)

source(file.path(dirname(candidata), "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")
e <- monitora_test_funcoes(candidata)$env
registros <- fread(registros_path, colClasses = "character", showProgress = FALSE, encoding = "UTF-8")

e$MONITORA_SCRIPT_VERSAO <- "2.9.26"
e$MONITORA_SCRIPT_BUILD_ID <- "v2.9.26-20260917-r03"
e$MONITORA_BASE_DIR <- destino
e$MONITORA_INPUT_DIR <- file.path(destino, "input")
e$MONITORA_OUTPUT_DIR <- file.path(destino, "output")
e$MONITORA_LOG_DIR <- file.path(destino, "log")
e$MONITORA_EXEC_ID <- "gate_fnb_v2926_r03"
e$MONITORA_MODO_EXECUCAO <- "painel_incremental_registros_corrig"
e$MONITORA_RESPONSAVEL_CORRECAO <- "gate de homologação"
e$MONITORA_INSTITUICAO_RESPONSAVEL <- "ICMBio"
e$MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
e$MONITORA_REGISTROS_VALIDADOS_GERADO <- TRUE
e$MONITORA_LOG_EXECUCAO <- data.table()
ledger <- file.path(
  e$MONITORA_OUTPUT_DIR,
  "02_painel_correcoes", "linhagem", "correcoes_semanticas_consolidada.csv"
)
aplicacoes <- file.path(
  e$MONITORA_OUTPUT_DIR,
  "02_painel_correcoes", "linhagem", "aplicacoes_correcoes.csv"
)
if (!file.exists(ledger) || !file.exists(aplicacoes)) {
  stop("A fixture FNB deve preservar a linhagem materializada da run17.", call. = FALSE)
}
e$MONITORA_TRILHA_SEMANTICA_HERDADA <- fread(ledger, showProgress = FALSE, encoding = "UTF-8")
e$MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table()
e$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table()
e$MONITORA_REPLAY_APLICACOES <- fread(aplicacoes, showProgress = FALSE, encoding = "UTF-8")

universo <- e$monitora_doc_universos_ocorrencias(registros)
if (!is.list(universo) || !nrow(universo$pos_painel)) {
  stop("O universo documental da FNB não foi construído.", call. = FALSE)
}

wd_anterior <- getwd()
on.exit(setwd(wd_anterior), add = TRUE)
setwd(destino)
artefatos <- e$monitora_relatorio_validacao_consolidado_gerar(
  registros_corrig = registros,
  output_dir = e$MONITORA_OUTPUT_DIR,
  log_dir = e$MONITORA_LOG_DIR,
  exec_id = e$MONITORA_EXEC_ID,
  responsavel = e$MONITORA_RESPONSAVEL_CORRECAO,
  instituicao = e$MONITORA_INSTITUICAO_RESPONSAVEL,
  formatos = "md",
  previa_documental = TRUE
)

esperados <- file.path(
  e$MONITORA_OUTPUT_DIR,
  "07_relatorio_validacao",
  paste0("relatorio_validacao_consolidado_", e$MONITORA_EXEC_ID, c(".Rmd", ".md"))
)
if (!all(file.exists(esperados)) || any(file.info(esperados)$size < 1L)) {
  stop("O relatório do checkpoint FNB não foi materializado integralmente.", call. = FALSE)
}
if (file.exists(file.path(e$MONITORA_OUTPUT_DIR, "07_relatorio_validacao", "RELATORIO_NAO_GERADO.txt"))) {
  stop("O marcador de falha do relatório foi materializado indevidamente.", call. = FALSE)
}

cat(sprintf(
  "TEST_V2926_R03_RELATORIO_CHECKPOINT_FNB_OK; linhas=%d; coletas=%d; artefatos=%d; rmd=%d; md=%d\n",
  nrow(registros), uniqueN(registros$COLETA), length(artefatos), file.info(esperados[[1L]])$size,
  file.info(esperados[[2L]])$size
))
