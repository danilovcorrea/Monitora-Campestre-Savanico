#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: Rscript test_v2926_r04_repeat_coletor_persistencia_semantica.R <candidata.R>", call. = FALSE)

candidata <- normalizePath(args[[1L]], mustWork = TRUE)
source(file.path(dirname(candidata), "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")
e <- monitora_test_funcoes(candidata)$env
tmp <- tempfile("monitora_r04_repeat_")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
e$MONITORA_OUTPUT_DIR <- file.path(tmp, "output")
e$MONITORA_LOG_DIR <- file.path(tmp, "log")
e$MONITORA_CORRECOES_DIR <- file.path(tmp, "correcoes")
e$MONITORA_EXEC_ID <- "gate_repeat_r04"
e$MONITORA_LOG_EXECUCAO <- data.table()
dir.create(e$MONITORA_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(e$MONITORA_LOG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(e$MONITORA_CORRECOES_DIR, recursive = TRUE, showWarnings = FALSE)

e$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
rid <- e$MONITORA_COL_ROW_ID
apos_aplicacao <- data.table(
  COLETA = rep("C1", 3L), uuid_registro = paste0("u", 1:3),
  `coletor/nome` = c("Ana", "", "Caio"), COLETORES = c("Ana", "", "Caio"),
  `coletor/cpf` = c("111", "", "333")
)
apos_aplicacao[, (rid) := paste0("r", 1:3)]
aud <- data.table(
  id_correcao = rep("COLREP_TESTE", 3L), status = "aplicada",
  atributo = c("coletor/nome", "COLETORES", "coletor/cpf"), linha_indice = "2",
  monitora_row_id = "r2", valor_antes = c("Bruno", "Bruno", "222"), valor_depois = "",
  COLETA = "C1", coleta_uuid = "", uuid_registro = "u2", UC = "UC", EA = "", UA = "",
  CICLO = "", CAMPANHA = "", ANO = "", `Data (data_hora)` = "",
  `Ponto amostral` = "", `Ponto metro` = ""
)
chaves <- e$monitora_correcao_colunas_chave(apos_aplicacao)
primeira <- e$monitora_correcao_auditar_persistencia_operacoes(
  apos_aplicacao, aud, chaves = chaves, contexto = "pos_aplicacao_objeto", abortar = FALSE
)
stopifnot(!any(grepl("^falha", primeira$status_persistencia)))
stopifnot(exists("MONITORA_PERSISTENCIA_REPEAT_COLETOR_ESTADO_ESPERADO", envir = .GlobalEnv, inherits = FALSE))

compactado <- copy(apos_aplicacao)
compactado[, `coletor/nome` := c("Ana", "Caio", "")]
compactado[, COLETORES := c("Ana", "Caio", "")]
compactado[, `coletor/cpf` := c("111", "333", "")]
segunda <- e$monitora_correcao_auditar_persistencia_operacoes(
  compactado, aud, chaves = e$monitora_correcao_colunas_chave(compactado),
  contexto = "pos_export_registros_corrig", abortar = FALSE
)
stopifnot(!any(grepl("^falha", segunda$status_persistencia)))
stopifnot(all(segunda$status_persistencia == "ok_repeat_coletor_estado_grupo_persistiu"))
stopifnot(all(segunda$modo_comparacao == "estado_semantico_repeat_coletor_por_coleta"))

alterado <- copy(compactado)
alterado[2L, `coletor/nome` := "Nome realmente divergente"]
alterado[2L, COLETORES := "Nome realmente divergente"]
negativo <- e$monitora_correcao_auditar_persistencia_operacoes(
  alterado, aud, chaves = e$monitora_correcao_colunas_chave(alterado),
  contexto = "gate_negativo_repeat", abortar = FALSE
)
stopifnot(any(grepl("^falha", negativo$status_persistencia)))

cat("TEST_V2926_R04_REPEAT_COLETOR_PERSISTENCIA_SEMANTICA_OK; positivo=3; negativo_bloqueado=TRUE\n")
