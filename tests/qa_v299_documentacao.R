#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: qa_v299_r04_documentacao.R SCRIPT", call. = FALSE)
script <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)

constantes <- c("MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID", "MONITORA_COL_ROW_ID")
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
  "monitora_manual_usuario_gerar", "monitora_doc_modos_passo_a_passo",
  "monitora_relatorio_validacao_consolidado_gerar"
)
stopifnot(all(vapply(necessarias, exists, logical(1L), envir = .GlobalEnv, inherits = FALSE)))

raiz_configurada <- Sys.getenv("QA_V299_ARTEFATOS_DIR", unset = "")
raiz <- if (nzchar(raiz_configurada)) raiz_configurada else tempfile("qa_v299_r04_doc_")
dir.create(raiz, recursive = TRUE)
raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
antigo <- getwd()
manter <- toupper(Sys.getenv("QA_V299_MANTER_ARTEFATOS", unset = "N")) == "S"
on.exit({
  setwd(antigo)
  if (!manter) unlink(raiz, recursive = TRUE, force = TRUE)
}, add = TRUE)
setwd(raiz)
dir.create("input")
dir.create("output/01_produtos_dados", recursive = TRUE)
dir.create("log")
MONITORA_LOG_DIR <- normalizePath("log", winslash = "/", mustWork = TRUE)
MONITORA_OUTPUT_DIR <- normalizePath("output", winslash = "/", mustWork = TRUE)
MONITORA_EXEC_ID <- "QA_R04_DOCUMENTACAO"
MONITORA_MODO_EXECUCAO <- "registros_corrig_completo"
MONITORA_RESPONSAVEL_CORRECAO <- "Usuário de teste"
MONITORA_INSTITUICAO_RESPONSAVEL <- "CBC/ICMBio"
MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
formatos_teste <- trimws(strsplit(Sys.getenv("QA_V299_DOC_FORMATOS", unset = ""), ",", fixed = TRUE)[[1L]])
formatos_teste <- formatos_teste[nzchar(formatos_teste)]

modos <- monitora_doc_modos_passo_a_passo()
stopifnot(nrow(modos) == 13L, uniqueN(modos$modo) == 13L)

manual <- monitora_manual_usuario_gerar(
  docs_dir = "manual_usuario",
  versao = "qa-r04",
  formatos = formatos_teste
)
rmd_manual <- manual[grepl("\\.Rmd$", manual)][1L]
stopifnot(file.exists(rmd_manual), dir.exists("manual_usuario/dados_apoio"))
txt_manual <- paste(readLines(rmd_manual, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
cfg_manual <- fread("manual_usuario/dados_apoio/configuracao_inicial.csv", colClasses = "character")
stopifnot(
  !grepl(normalizePath(raiz, winslash = "/", mustWork = TRUE), txt_manual, fixed = TRUE),
  !grepl("output/03_auditorias/relatorios_validacao", txt_manual, fixed = TRUE),
  grepl("output/07_relatorio_validacao", txt_manual, fixed = TRUE),
  "MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF" %in% cfg_manual$variavel,
  identical(cfg_manual[variavel == "MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF", sub("^padrão ", "", strsplit(valores, ";", fixed = TRUE)[[1L]][1L])], "N"),
  grepl("Correções de registros", txt_manual, fixed = TRUE),
  !grepl("bolsist", txt_manual, ignore.case = TRUE)
)

dt <- data.table(
  COLETA = rep("1", 3L), UC = "UC de teste", EA = "EA1", UA = "UA1",
  ANO = "2026", valor = 1:3
)
fwrite(dt, "output/01_produtos_dados/registros_corrig.csv")
rel <- monitora_relatorio_validacao_consolidado_gerar(
  registros_corrig = dt,
  output_dir = "output",
  log_dir = "log",
  exec_id = MONITORA_EXEC_ID,
  responsavel = MONITORA_RESPONSAVEL_CORRECAO,
  instituicao = MONITORA_INSTITUICAO_RESPONSAVEL,
  formatos = formatos_teste
)
rmd_rel <- rel[grepl("\\.Rmd$", rel)][1L]
stopifnot(
  file.exists(rmd_rel),
  grepl("07_relatorio_validacao", rmd_rel, fixed = TRUE),
  dir.exists("output/07_relatorio_validacao/dados_apoio")
)
txt_rel <- paste(readLines(rmd_rel, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  !grepl(normalizePath(raiz, winslash = "/", mustWork = TRUE), txt_rel, fixed = TRUE),
  grepl("# Resumo executivo", txt_rel, fixed = TRUE),
  grepl("# Histórico, sessões e rodadas", txt_rel, fixed = TRUE),
  grepl("# Conclusão e continuidade", txt_rel, fixed = TRUE),
  !grepl("bolsist", txt_rel, ignore.case = TRUE),
  !grepl("\\b(gate|ledger|sidecar|preflight|roll-forward)\\b", txt_rel, ignore.case = TRUE, perl = TRUE)
)

cat("QA_V299_R04_DOCUMENTACAO_OK artefatos=", normalizePath(raiz, winslash = "/", mustWork = TRUE), "\n", sep = "")
