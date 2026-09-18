#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == "windows" && requireNamespace("rmarkdown", quietly = TRUE) &&
    !rmarkdown::pandoc_available()) {
  pandoc_rstudio <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (file.exists(file.path(pandoc_rstudio, "pandoc.exe"))) Sys.setenv(RSTUDIO_PANDOC = pandoc_rstudio)
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Uso: Rscript test_v2926_r04_relatorio_validacao_autonomo.R <candidata.R> <registros_corrig.csv> <saida>", call. = FALSE)
}
candidata <- normalizePath(args[[1L]], mustWork = TRUE)
registros_path <- normalizePath(args[[2L]], mustWork = TRUE)
destino <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)
dir.create(file.path(destino, "input"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "output"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "log"), recursive = TRUE, showWarnings = FALSE)

source(file.path(dirname(candidata), "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")
e <- monitora_test_funcoes(candidata)$env
analiticas <- grep("^monitora_relatorios_analiticos_", ls(e, all.names = TRUE), value = TRUE)
if (length(analiticas)) rm(list = analiticas, envir = e)

registros <- fread(registros_path, colClasses = "character", showProgress = FALSE, encoding = "UTF-8")
e$MONITORA_SCRIPT_VERSAO <- "2.9.26"
e$MONITORA_SCRIPT_BUILD_ID <- "v2.9.26-20260918-r04"
e$MONITORA_BASE_DIR <- destino
e$MONITORA_INPUT_DIR <- file.path(destino, "input")
e$MONITORA_OUTPUT_DIR <- file.path(destino, "output")
e$MONITORA_LOG_DIR <- file.path(destino, "log")
e$MONITORA_EXEC_ID <- "gate_relatorio_autonomo_r04"
e$MONITORA_MODO_EXECUCAO <- "painel_incremental_registros_corrig"
e$MONITORA_RESPONSAVEL_CORRECAO <- "gate de homologação"
e$MONITORA_INSTITUICAO_RESPONSAVEL <- "ICMBio"
e$MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
e$MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
e$MONITORA_LOG_EXECUCAO <- data.table()
e$MONITORA_TRILHA_SEMANTICA_HERDADA <- data.table()
e$MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table()
e$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table()
e$MONITORA_REPLAY_APLICACOES <- data.table()

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
  formatos = c("html", "docx"),
  previa_documental = TRUE
)

base <- file.path(e$MONITORA_OUTPUT_DIR, "07_relatorio_validacao", paste0("relatorio_validacao_consolidado_", e$MONITORA_EXEC_ID))
esperados <- paste0(base, c(".Rmd", ".md", ".html", ".docx"))
stopifnot(all(file.exists(esperados)), all(file.info(esperados)$size > 0L))
stopifnot(!file.exists(file.path(dirname(base), "RELATORIO_NAO_GERADO.txt")))
stopifnot(file.exists(file.path(dirname(base), "auditoria_integridade_docx_relatorio_validacao.csv")))
stopifnot(any(normalizePath(artefatos, winslash = "/", mustWork = FALSE) == normalizePath(paste0(base, ".docx"), winslash = "/", mustWork = FALSE)))

texto <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
checkpoint <- grep("monitora_relatorio_validacao_consolidado_tentar\\(", texto)[1L]
obrigatorias <- c(
  "monitora_doc_fmt_num", "monitora_doc_fmt_percentual",
  "monitora_doc_referencia_docx_sha256", "monitora_doc_referencia_docx_base64",
  "monitora_doc_referencia_docx_materializar", "monitora_doc_coluna_contextual_mesclavel",
  "monitora_doc_validacao_preservar_linhas_tabela"
)
linhas_def <- vapply(obrigatorias, function(nome) {
  z <- grep(paste0("^", nome, "[[:space:]]*<-[[:space:]]*function"), texto)
  if (length(z) != 1L) stop("Definição autônoma ausente ou duplicada: ", nome, call. = FALSE)
  z
}, integer(1L))
stopifnot(all(linhas_def < checkpoint))
corpo_doc <- paste(deparse(body(e$monitora_doc_render_editaveis)), collapse = "\n")
stopifnot(!grepl("monitora_relatorios_analiticos_", corpo_doc, fixed = TRUE))

cat(sprintf(
  "TEST_V2926_R04_RELATORIO_VALIDACAO_AUTONOMO_OK; linhas=%d; artefatos=%d; docx=%d; funcoes_analiticas_removidas=%d\n",
  nrow(registros), length(artefatos), file.info(paste0(base, ".docx"))$size, length(analiticas)
))
