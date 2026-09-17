#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Uso: Rscript test_v2926_r02_escopo_restrito.R <release_r02.R> <baseline_r01.R>", call. = FALSE)
}
candidata <- normalizePath(args[[1L]], mustWork = TRUE)
baseline <- normalizePath(args[[2L]], mustWork = TRUE)

funcoes <- function(path) {
  arvore <- parse(path, keep.source = FALSE, encoding = "UTF-8")[[1L]]
  saida <- list()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    atribuicao <- as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L
    if (atribuicao && is.symbol(no[[2L]]) && is.call(no[[3L]]) && identical(as.character(no[[3L]][[1L]])[1L], "function")) {
      saida[[as.character(no[[2L]])]] <<- no[[3L]]
      return(invisible(NULL))
    }
    if (length(no) > 1L) for (i in 2:length(no)) visitar(no[[i]])
    invisible(NULL)
  }
  visitar(arvore)
  saida
}

a <- funcoes(baseline)
b <- funcoes(candidata)
comuns <- intersect(names(a), names(b))
alteradas <- sort(comuns[!vapply(comuns, function(nm) identical(a[[nm]], b[[nm]]), logical(1L))])
novas <- sort(setdiff(names(b), names(a)))
removidas <- sort(setdiff(names(a), names(b)))
permitidas_alteradas <- sort(c(
  "monitora_io_converter_utf8_sem_perda",
  "monitora_io_ler_csv_texto",
  "monitora_publicacao_ae_ausencia_fisica_na",
  "monitora_publicacao_ae_preparar_registros_corrig_para_csv",
  "monitora_qfield_escrever_qgs",
  "monitora_qfield_gerar",
  "monitora_qfield_info_raster",
  "monitora_qfield_slug",
  "monitora_qfield_xml",
  "monitora_sanitizar_ausencias_produto",
  "monitora_relatorios_analiticos_dt",
  "monitora_relatorios_analiticos_epoca",
  "monitora_relatorios_analiticos_gerar"
))
permitidas_novas <- sort(c(
  "monitora_locale_utf8_ativar",
  "monitora_locale_utf8_restaurar",
  "monitora_publicacao_ae_eh_traco_ausencia",
  "monitora_publicacao_ae_texto_utf8",
  "monitora_qfield_dt_utf8",
  "monitora_qfield_utf8"
))
if (!identical(alteradas, permitidas_alteradas)) stop("Funções alteradas fora do escopo: ", paste(setdiff(alteradas, permitidas_alteradas), collapse = " | "), call. = FALSE)
if (!identical(novas, permitidas_novas)) stop("Funções novas fora do escopo: ", paste(setdiff(novas, permitidas_novas), collapse = " | "), call. = FALSE)
if (length(removidas)) stop("Funções removidas: ", paste(removidas, collapse = " | "), call. = FALSE)
cat(sprintf(
  "TEST_V2926_R02_ESCOPO_RESTRITO_OK; alteradas=%d; novas=%d; removidas=0\n",
  length(alteradas), length(novas)
))
