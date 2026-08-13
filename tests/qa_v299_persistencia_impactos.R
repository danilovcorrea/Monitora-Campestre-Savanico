#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: qa_v299_r03_persistencia_impactos.R SCRIPT", call. = FALSE)
script <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)

### Carrega somente definições de funções e constantes indispensáveis;
### o pipeline monolítico não é executado pelo teste unitário.
constantes <- c(
  "MONITORA_COL_ROW_ID", "MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]])) {
    nome <- as.character(x[[2L]])
    if (is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]])[1L], "function")) {
      eval(x, .GlobalEnv)
      return(invisible(NULL))
    }
    if (nome %in% constantes) {
      try(eval(x, .GlobalEnv), silent = TRUE)
      return(invisible(NULL))
    }
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(parse(file = script, keep.source = FALSE)), coletar))

necessarias <- c(
  "monitora_correcao_expandir_dependencias_impactos_legadas",
  "monitora_correcao_marcar_checkpoint_por_auditoria_operacoes"
)
stopifnot(all(vapply(necessarias, exists, logical(1L), envir = .GlobalEnv, inherits = FALSE)))
monitora_log_registrar_evento <- function(...) invisible(NULL)

cols <- list(
  coleta = "COLETA",
  pai = "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)",
  tipos = "Qual(is)? (impact_manejo_uso)",
  outro = "Outros tipos de manejo ou uso: (impact_manejo_uso)",
  descricao = "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)"
)
dt <- data.table(
  COLETA = rep("42871", 3L),
  monitora_row_id = paste0("row_", 1:3)
)
dt[, (cols$pai) := "não"]
dt[, (cols$tipos) := NA_character_]
dt[, (cols$outro) := NA_character_]
dt[, (cols$descricao) := NA_character_]

corr <- data.table(
  id_correcao = "CORR_20260812150122_5208",
  coleta = "42871",
  escopo_aplicacao = "coleta_inteira",
  atributo_coluna_registros_corrig = cols$descricao,
  acao = "update",
  valor_original_esperado = NA_character_,
  valor_novo = "Vestígios de fogo",
  migrated_from_schema = "correcoes_semanticas_v1",
  preconditions_json = "legacy_operational_fields",
  tipo_correcao = "simples_ou_lote",
  operation_kind = "atribuir_valor_canonico",
  atributo_canonico = cols$descricao,
  event_item_id = "ITEM_1",
  hash_operacao_semantica = "hash_legado",
  motivo = NA_character_,
  mensagem_validacao = NA_character_,
  created_build = "v2.9.9-dev-r01-20260812",
  script_versao_replay = "2.9.9-dev-r01",
  n_linhas_esperado = 3L,
  n_linhas_alvo = 3L
)

mig <- monitora_correcao_expandir_dependencias_impactos_legadas(dt, corr)
if (nrow(mig$corr) != 4L) {
  print(mig$corr[, .(
    id_correcao, atributo_coluna_registros_corrig, acao, valor_novo,
    migrated_from_schema
  )])
  print(mig$auditoria)
}
stopifnot(
  nrow(mig$corr) == 4L,
  uniqueN(mig$corr$id_correcao) == 1L,
  nrow(mig$auditoria) == 1L,
  identical(mig$auditoria$status, "migrada_dependencia_impactos_atomica"),
  identical(mig$auditoria$inferencia_causal, FALSE),
  all(c(cols$pai, cols$tipos, cols$outro, cols$descricao) %in% mig$corr$atributo_coluna_registros_corrig),
  identical(mig$corr[atributo_coluna_registros_corrig == cols$pai, valor_novo], "sim"),
  identical(mig$corr[atributo_coluna_registros_corrig == cols$tipos, valor_novo], "outros"),
  identical(mig$corr[atributo_coluna_registros_corrig == cols$outro, valor_novo], "Vestígios de fogo")
)

### Idempotência: estado já coerente não recebe derivações novas.
dt_ok <- copy(dt)
dt_ok[, (cols$pai) := "sim"]
dt_ok[, (cols$tipos) := "outros"]
dt_ok[, (cols$outro) := "Vestígios de fogo"]
mig_ok <- monitora_correcao_expandir_dependencias_impactos_legadas(dt_ok, corr)
stopifnot(nrow(mig_ok$corr) == 1L, nrow(mig_ok$auditoria) == 0L)

### Checkpoint operacional antigo pode não carregar os metadados v2; a
### ausência conjunta desses campos é a sentinela de migração, nunca o texto.
corr_raw <- copy(corr)
corr_raw[, c(
  "migrated_from_schema", "preconditions_json", "operation_kind",
  "created_build"
) := NA_character_]
mig_raw <- monitora_correcao_expandir_dependencias_impactos_legadas(dt, corr_raw)
stopifnot(nrow(mig_raw$corr) == 4L, nrow(mig_raw$auditoria) == 1L)

### Gate: falha de operação não pode ser rotulada como pendência dos dados.
MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES <- FALSE
MONITORA_PERSISTENCIA_OPERACOES_USUARIO_PENDENTES <- FALSE
MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 <- TRUE
MONITORA_REGISTROS_CORRIG_CONTRATO_CONTEXTO <- "teste"
falha <- data.table(
  id_correcao = "CORR_USUARIO_1",
  status_persistencia = "falha_valor_nao_persistiu"
)
monitora_correcao_marcar_checkpoint_por_auditoria_operacoes(falha, "qa")
stopifnot(
  isTRUE(MONITORA_PERSISTENCIA_OPERACOES_USUARIO_PENDENTES),
  isFALSE(MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES),
  isFALSE(MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS),
  isFALSE(MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21)
)

cat("QA_V299_R03_PERSISTENCIA_IMPACTOS_OK\n")
