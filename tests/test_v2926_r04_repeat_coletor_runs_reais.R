#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Uso: Rscript test_v2926_r04_repeat_coletor_runs_reais.R <candidata.R> <run_FNB> <run_RVOB>", call. = FALSE)
}
candidata <- normalizePath(args[[1L]], mustWork = TRUE)
runs <- setNames(lapply(args[2:3], normalizePath, mustWork = TRUE), c("FNB", "RVOB"))
source(file.path(dirname(candidata), "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")

resultados <- lapply(names(runs), function(nome_run) {
  e <- monitora_test_funcoes(candidata)$env
  e$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
  e$MONITORA_SCRIPT_VERSAO <- "2.9.26"
  e$MONITORA_SCRIPT_BUILD_ID <- "v2.9.26-20260918-r04"
  tmp <- tempfile(paste0("monitora_r04_", tolower(nome_run), "_"))
  e$MONITORA_OUTPUT_DIR <- file.path(tmp, "output")
  e$MONITORA_LOG_DIR <- file.path(tmp, "log")
  e$MONITORA_CORRECOES_DIR <- file.path(tmp, "correcoes")
  e$MONITORA_EXEC_ID <- paste0("gate_real_", tolower(nome_run))
  e$MONITORA_LOG_EXECUCAO <- data.table()
  dir.create(e$MONITORA_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(e$MONITORA_LOG_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(e$MONITORA_CORRECOES_DIR, recursive = TRUE, showWarnings = FALSE)

  base <- runs[[nome_run]]
  registros <- fread(file.path(base, "input", "registros_corrig.csv"), colClasses = "character", na.strings = NULL, showProgress = FALSE)
  arquivos_aud <- list.files(file.path(base, "log"), pattern = "^auditoria_correcoes_campos_[0-9]{8}_[0-9]{6}[.]csv$", full.names = TRUE)
  if (length(arquivos_aud) != 1L) stop(nome_run, ": auditoria de correções da sessão não é unívoca.", call. = FALSE)
  aud <- fread(arquivos_aud, colClasses = "character", na.strings = NULL, showProgress = FALSE)
  aud <- aud[grepl("^COLREP_", as.character(id_correcao)) & as.character(status) %in% c("aplicada", "aplicada_atomica")]
  if (!nrow(aud)) stop(nome_run, ": operações reais COLREP não localizadas.", call. = FALSE)

  for (ii in seq_len(nrow(aud))) {
    atributo <- as.character(aud$atributo[ii])
    if (!(atributo %in% names(registros))) stop(nome_run, ": atributo ausente: ", atributo, call. = FALSE)
    rid <- as.character(aud$monitora_row_id[ii])
    alvo <- if (!is.na(rid) && nzchar(rid) && e$MONITORA_COL_ROW_ID %in% names(registros)) {
      which(as.character(registros[[e$MONITORA_COL_ROW_ID]]) == rid)
    } else suppressWarnings(as.integer(aud$linha_indice[ii]))
    if (length(alvo) != 1L || is.na(alvo) || alvo < 1L || alvo > nrow(registros)) {
      stop(nome_run, ": alvo real COLREP não pôde ser relocalizado de forma unívoca.", call. = FALSE)
    }
    esperado_antes <- e$monitora_correcao_na_para_vazio(aud$valor_antes[ii])
    observado_antes <- e$monitora_correcao_na_para_vazio(registros[[atributo]][alvo])
    if (!identical(observado_antes, esperado_antes)) {
      stop(nome_run, ": fixture de entrada não confere com valor_antes auditado em ", aud$id_correcao[ii], call. = FALSE)
    }
    data.table::set(registros, i = alvo, j = atributo, value = as.character(aud$valor_depois[ii]))
  }

  persist_aplicacao <- e$monitora_correcao_auditar_persistencia_operacoes(
    registros, aud, chaves = e$monitora_correcao_colunas_chave(registros),
    contexto = paste0("pos_aplicacao_real_", tolower(nome_run)), abortar = FALSE
  )
  if (any(grepl("^falha", persist_aplicacao$status_persistencia))) {
    stop(nome_run, ": operações não conferiram imediatamente após a aplicação.", call. = FALSE)
  }
  materializado <- e$monitora_coletores_repeat_materializar_corrig(
    copy(registros), output_dir = e$MONITORA_OUTPUT_DIR, log_dir = e$MONITORA_LOG_DIR,
    exec_id = e$MONITORA_EXEC_ID, contexto = paste0("gate_real_", tolower(nome_run))
  )
  persist_export <- e$monitora_correcao_auditar_persistencia_operacoes(
    materializado, aud, chaves = e$monitora_correcao_colunas_chave(materializado),
    contexto = paste0("pos_export_real_", tolower(nome_run)), abortar = FALSE
  )
  falhas <- sum(grepl("^falha", persist_export$status_persistencia))
  if (falhas) stop(nome_run, ": persistência semântica real manteve ", falhas, " falsa(s) falha(s).", call. = FALSE)
  data.table(
    UC = nome_run, operacoes_atomicas = nrow(aud), coletas = uniqueN(aud$COLETA),
    reclassificadas_por_estado = sum(persist_export$status_persistencia == "ok_repeat_coletor_estado_grupo_persistiu"),
    falhas = falhas
  )
})

resumo <- rbindlist(resultados)
stopifnot(resumo[UC == "FNB", reclassificadas_por_estado] >= 2L)
stopifnot(resumo[UC == "RVOB", reclassificadas_por_estado] >= 4L)
cat("TEST_V2926_R04_REPEAT_COLETOR_RUNS_REAIS_OK\n")
print(resumo)
