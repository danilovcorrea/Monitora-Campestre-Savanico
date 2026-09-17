#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(args[[1L]], mustWork = TRUE)
rodada <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
destino <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)
if (dir.exists(destino) || file.exists(destino)) stop("Destino isolado já existe.", call. = FALSE)
dir.create(file.path(destino, "output"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "log"), recursive = TRUE, showWarnings = FALSE)
source(file.path(dirname(candidata), "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")
e <- monitora_test_funcoes(candidata)$env
e$MONITORA_BASE_DIR <- rodada

locale_anterior <- Sys.getlocale("LC_CTYPE")
on.exit(try(Sys.setlocale("LC_CTYPE", locale_anterior), silent = TRUE), add = TRUE)
if (!identical(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")), "C")) stop("Não foi possível ativar LC_CTYPE=C.", call. = FALSE)

fonte <- file.path(rodada, "output", "01_produtos_dados", "registros_corrig.csv")
dt <- e$monitora_io_ler_csv_texto(fonte)
hash_antes <- digest::digest(dt, algo = "sha256", serialize = TRUE)
sanitizada_geral <- e$monitora_sanitizar_ausencias_produto(dt)
if (!identical(dim(sanitizada_geral), dim(dt)) || !identical(names(sanitizada_geral), names(dt))) {
  stop("A sanitização tabular geral alterou dimensão ou schema.", call. = FALSE)
}
resultado <- e$monitora_publicacao_ae_preparar_registros_corrig_para_csv(
  dt,
  contexto = "gate_finalizacao_apai_v2927",
  output_dir = file.path(destino, "output"),
  log_dir = file.path(destino, "log"),
  exec_id = "gate_v2927"
)
hash_depois <- digest::digest(dt, algo = "sha256", serialize = TRUE)
if (!identical(hash_antes, hash_depois)) stop("A preparação para CSV alterou a fonte em memória.", call. = FALSE)
if (!identical(dim(resultado), dim(dt)) || !identical(names(resultado), names(dt))) stop("A preparação alterou dimensão ou schema.", call. = FALSE)
chars <- names(resultado)[vapply(resultado, is.character, logical(1L))]
if (length(chars) && any(!vapply(resultado[, ..chars], function(x) all(validUTF8(x[!is.na(x)])), logical(1L)))) {
  stop("A preparação produziu texto inválido em UTF-8.", call. = FALSE)
}
auditoria <- file.path(destino, "output", "auditoria_registros_corrig_ausencias_fisicas_na.csv")
if (!file.exists(auditoria) || file.info(auditoria)$size <= 0L) stop("Auditoria da sanitização final não foi materializada.", call. = FALSE)
if (!identical(Sys.getlocale("LC_CTYPE"), "C")) stop("A sanitização alterou o locale da sessão.", call. = FALSE)
cat(sprintf(
  "TEST_V2927_FINALIZACAO_APAI_WINDOWS_OK; linhas=%d; colunas=%d; hash_fonte_intacto=TRUE; auditoria=%s\n",
  nrow(resultado), ncol(resultado), normalizePath(auditoria, winslash = "/", mustWork = TRUE)
))
