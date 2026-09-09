#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.24-dev_r05.R", mustWork = TRUE)
baseline <- normalizePath(if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.23.R", mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (as.character(x[[1L]])[1L] %in% c("<-", "=") && length(x) >= 3L &&
      is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    try(eval(x, env), silent = TRUE)
  }
  if (length(x) > 1L) for (ii in 2:length(x)) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

linhas <- function(p) readLines(p, warn = FALSE, encoding = "UTF-8")
inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  assert(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
assert(identical(inicio_congelado(linhas(script)), inicio_congelado(linhas(baseline))), "Inicialização congelada do RStudio foi alterada.")

dir_aud <- tempfile("qa_uc_grafia_")
dir.create(dir_aud)
env$monitora_fwrite <- function(x, file, ...) data.table::fwrite(x, file, ...)
fonte <- data.table(
  UC = c(
    "Refúgio de Vida Silvestre das Veredas do Oeste Baiano",
    "Refúgio da Vida Silvestre das Veredas do Oeste Baiano"
  ),
  ANO = c(2022L, 2026L),
  COLETA = c("A", "B")
)
fonte_antes <- copy(fonte)
stat <- copy(fonte)
assert(isTRUE(env$monitora_stat_reconciliar_grafias_uc(stat, dir_aud)), "Grafias equivalentes não foram reconciliadas.")
assert(uniqueN(stat$UC) == 1L && unique(stat$UC) == "Refúgio da Vida Silvestre das Veredas do Oeste Baiano", "Grafia mais recente não prevaleceu.")
assert(identical(fonte, fonte_antes), "Fonte foi alterada pela reconciliação estatística.")
aud <- fread(file.path(dir_aud, "03_auditorias", "estatisticas", "auditoria_reconciliacao_grafias_uc_estatisticas.csv"), encoding = "UTF-8")
assert(nrow(aud) == 2L && all(aud$status == "reconciliada_na_camada_estatistica") && all(!aud$alterou_dados_fonte), "Auditoria da reconciliação está incompleta.")

distintas <- data.table(UC = c("Parque Nacional de Brasília", "Reserva Biológica da Contagem"), ANO = 2026L)
assert(!isTRUE(env$monitora_stat_reconciliar_grafias_uc(distintas, dir_aud)) && uniqueN(distintas$UC) == 2L, "UCs distintas foram conciliadas.")

harm <- env$monitora_relatorios_analiticos_harmonizar_uc_registros(
  fonte,
  "Refúgio da Vida Silvestre das Veredas do Oeste Baiano"
)
assert(uniqueN(harm$registros$UC) == 1L && isTRUE(harm$alterado), "Cópia interna do relatório não foi harmonizada.")
assert(identical(fonte, fonte_antes) && all(!harm$auditoria$alterou_registros_corrig_fonte), "Harmonização do relatório alterou a fonte.")
erro <- tryCatch({
  env$monitora_relatorios_analiticos_harmonizar_uc_registros(
    distintas,
    "Parque Nacional de Brasília"
  )
  ""
}, error = conditionMessage)
assert(grepl("diferem materialmente", erro, fixed = TRUE), "Cópia do relatório aceitou UCs distintas.")

cat("TEST_V2924_UC_GRAFIA_WINDOWS_OK\n")
