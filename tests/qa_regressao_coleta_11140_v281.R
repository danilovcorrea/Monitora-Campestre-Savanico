args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Uso: Rscript tests/qa_regressao_coleta_11140_v281.R SCRIPT REGISTROS_CORRIG CORRECOES_CAMPOS",
    call. = FALSE
  )
}

script_path <- normalizePath(args[[1L]], mustWork = TRUE)
registros_path <- normalizePath(args[[2L]], mustWork = TRUE)
correcoes_path <- normalizePath(args[[3L]], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

carregar_funcoes_script <- function(path) {
  carregar_expr <- function(expr) {
    if (!is.call(expr)) return(invisible(NULL))
    if (
      identical(expr[[1L]], as.name("<-")) &&
        length(expr) >= 3L &&
        is.symbol(expr[[2L]]) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      eval(expr, envir = .GlobalEnv)
      return(invisible(NULL))
    }
    for (ii in seq_along(expr)) try(carregar_expr(expr[[ii]]), silent = TRUE)
    invisible(NULL)
  }
  for (expr in parse(path, keep.source = FALSE, encoding = "UTF-8")) carregar_expr(expr)
  invisible(TRUE)
}
carregar_funcoes_script(script_path)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- unique(c(
  ".id", "id", "ID", "uuid", "UUID", "uuid_registro", "UUID_REGISTRO",
  "coleta_uuid", "COLETA_UUID", "COLETA", "PROTOCOLO", "arquivo_origem",
  "linha_indice", "linha_origem", "linha_origem_registros_corrig",
  "ordem_linha_original", "arquivo_fonte", "source_file",
  "MONITORA_HABITOS_CANONICOS_PRESERVADOS",
  "ANO", "DATA_MONITORA_PARSEADA", "num_placa_formatado",
  "DATA DO REGISTRO", "DATA DO RECEBIMENTO", "ULTIMA EDICAO",
  "data_do_registro", "data_do_recebimento", "ultima_edicao"
))
MONITORA_OPCAO_CHECKPOINTS_GRANULARES_CORRECOES <- "N"
MONITORA_OPCAO_PULAR_RECALCULO_DATA_HORA_SEM_ALTERACAO <- "S"
MONITORA_REAPLICAR_CORRECOES_ANTERIORES <- FALSE
MONITORA_REPLAY_DIAGNOSTICO_NAO_ABORTAR <- FALSE
MONITORA_REPLAY_SEMANTICO_EM_EXECUCAO <- FALSE
MONITORA_CORRECOES_REAPLICADAS_PRE_PAINEL <- FALSE
MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES <- FALSE
MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 <- FALSE
MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
MONITORA_FALHAR_PRODUTOS_FINAIS_AUSENTES <- FALSE

.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())

monitora_perf_registrar_checkpoint <- function(...) invisible(NULL)
monitora_log_registrar_evento <- function(...) invisible(NULL)
monitora_correcao_console_msg <- function(...) invisible(NULL)
monitora_cadeia_dados_relatorio_gerar <- function(...) invisible(NULL)
monitora_fwrite <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!length(names(x))) {
    file.create(path)
    return(invisible(path))
  }
  data.table::fwrite(x, path, ...)
  invisible(path)
}

dt_in <- fread(
  registros_path,
  encoding = "UTF-8",
  na.strings = c("", "NA"),
  colClasses = "character",
  showProgress = FALSE
)
ops_todas <- fread(
  correcoes_path,
  encoding = "UTF-8",
  na.strings = c("", "NA"),
  colClasses = "character",
  showProgress = FALSE
)
assert("coleta" %in% names(ops_todas), "ledger sem coluna coleta")
ops <- ops_todas[as.character(coleta) == "11140"]
assert(nrow(ops) == 3L, "ledger real não contém as três operações específicas de 11140")
assert(
  setequal(as.character(ops$ponto_amostral), c("18", "22", "40")),
  "ledger real não aponta exatamente 11140/18, 11140/22 e 11140/40"
)

ch_in <- monitora_correcao_colunas_chave(dt_in)
assert(
  !is.na(ch_in$coleta) && !is.na(ch_in$ponto_amostral),
  "chaves COLETA/ponto não resolvidas na base real"
)
alvos_in <- which(
  as.character(dt_in[[ch_in$coleta]]) == "11140" &
    as.character(dt_in[[ch_in$ponto_amostral]]) %in% c("18", "22", "40")
)
assert(length(alvos_in) == 3L, "base real não contém exatamente os três alvos 11140")

desc_in <- monitora_correcao_classificar_desconhecida(
  dt_in,
  chaves = ch_in,
  dicionario = NULL
)
desc_11140_in <- desc_in[
  linha_indice %in% alvos_in &
    tipo_ocorrencia == "forma_vida_desconhecida"
]
assert(
  nrow(desc_11140_in) == 3L,
  "estado inicial real não contém as três ocorrências desconhecidas esperadas"
)

qa_dir <- tempfile("qa_v281_11140_")
dir.create(qa_dir, recursive = TRUE)
ops_path <- file.path(qa_dir, "correcoes_11140.csv")
fwrite(ops, ops_path, na = "NA")

MONITORA_EXEC_ID <- "QA_V281_COLETA_11140"
MONITORA_OUTPUT_DIR <- file.path(qa_dir, "output")
MONITORA_LOG_DIR <- file.path(qa_dir, "log")
MONITORA_CORRECOES_DIR <- file.path(MONITORA_OUTPUT_DIR, "correcoes")
MONITORA_INPUT_DIR <- dirname(registros_path)
MONITORA_ARQUIVO_CORRECOES_CAMPOS <- ops_path
dir.create(MONITORA_OUTPUT_DIR, recursive = TRUE)
dir.create(MONITORA_LOG_DIR, recursive = TRUE)
dir.create(MONITORA_CORRECOES_DIR, recursive = TRUE)

dt_final <- monitora_correcao_aplicar_arquivo(
  data.table::copy(dt_in),
  arquivo_correcao = ops_path,
  dicionario = NULL
)
assert(nrow(dt_final) == nrow(dt_in), "aplicação alterou a cardinalidade da base")

ch_final <- monitora_correcao_colunas_chave(dt_final)
alvos_final <- which(
  as.character(dt_final[[ch_final$coleta]]) == "11140" &
    as.character(dt_final[[ch_final$ponto_amostral]]) %in% c("18", "22", "40")
)
assert(length(alvos_final) == 3L, "alvos 11140 não foram preservados após aplicação")

desc_final <- monitora_correcao_classificar_desconhecida(
  dt_final,
  chaves = ch_final,
  dicionario = NULL
)
assert(
  !any(desc_final$linha_indice %in% alvos_final),
  "ao menos um dos três alvos 11140 continua classificado como desconhecido"
)

colunas_lista <- list(
  nativa = monitora_correcao_coluna_forma_vida(dt_final, "nativa"),
  exotica = monitora_correcao_coluna_forma_vida(dt_final, "exotica"),
  seca_morta = monitora_correcao_coluna_forma_vida(dt_final, "seca_morta"),
  outra_forma_vida = monitora_correcao_coluna_forma_vida(dt_final, "outra_forma_vida")
)
tokens_alvo <- lapply(alvos_final, function(ii) {
  unique(unlist(lapply(colunas_lista, function(cc) {
    if (is.na(cc) || !(cc %in% names(dt_final))) return(character(0))
    monitora_correcao_tokenizar(dt_final[[cc]][ii])
  })))
})
names(tokens_alvo) <- as.character(dt_final[[ch_final$ponto_amostral]][alvos_final])
esperados <- c(`18` = "arbusto_abaixo", `22` = "musgos", `40` = "samambaia")
for (ponto in names(esperados)) {
  assert(
    esperados[[ponto]] %in% tokens_alvo[[ponto]],
    paste0("ponto ", ponto, " não recebeu o token esperado ", esperados[[ponto]])
  )
}

cat(
  "QA_REGRESSAO_COLETA_11140_V281_OK",
  "linhas=", nrow(dt_final),
  "alvos=11140/18,11140/22,11140/40",
  "tokens=arbusto_abaixo,musgos,samambaia",
  "\n"
)
