#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Uso: Rscript test_v2926_r03_relatorio_checkpoint_ordem.R <candidata.R> <baseline_r02.R>", call. = FALSE)
}

candidata <- normalizePath(args[[1L]], mustWork = TRUE)
baseline <- normalizePath(args[[2L]], mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

linhas <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
definicoes <- grep(
  "^monitora_stat_uc_chave_equivalencia[[:space:]]*<-[[:space:]]*function\\(",
  linhas
)
usos <- grep("monitora_stat_uc_chave_equivalencia\\(", linhas)
usos <- setdiff(usos, definicoes)
checkpoint <- grep("monitora_relatorio_validacao_consolidado_tentar\\(", linhas)

assert(length(definicoes) == 1L, "A função de equivalência da UC deve possuir uma única definição.")
assert(length(usos) > 0L, "Nenhum consumidor da função de equivalência da UC foi localizado.")
assert(definicoes[[1L]] < min(usos), "A função de equivalência da UC permanece posterior ao primeiro uso.")
assert(length(checkpoint) >= 1L && definicoes[[1L]] < min(checkpoint),
  "A função de equivalência da UC não está disponível antes do relatório do checkpoint parcial.")

encontrar_funcao <- function(arquivo, nome) {
  arvore <- parse(arquivo, keep.source = FALSE, encoding = "UTF-8")
  achados <- list()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    atribuicao <- as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L
    if (atribuicao && is.symbol(no[[2L]]) && identical(as.character(no[[2L]]), nome) &&
        is.call(no[[3L]]) && identical(as.character(no[[3L]][[1L]])[1L], "function")) {
      achados[[length(achados) + 1L]] <<- no[[3L]]
    }
    if (length(no) > 1L) for (ii in 2:length(no)) visitar(no[[ii]])
    invisible(NULL)
  }
  visitar(arvore[[1L]])
  assert(length(achados) == 1L, paste0("Função não unívoca: ", nome))
  achados[[1L]]
}

funcao_candidata <- encontrar_funcao(candidata, "monitora_stat_uc_chave_equivalencia")
funcao_baseline <- encontrar_funcao(baseline, "monitora_stat_uc_chave_equivalencia")
assert(identical(funcao_candidata, funcao_baseline),
  "A correção alterou a semântica da equivalência de nomes de UC; somente a ordem era autorizada.")

env <- new.env(parent = globalenv())
normalizar <- eval(funcao_candidata, envir = env)
obtido <- normalizar(c(
  "Refúgio de Vida Silvestre Veredas do Oeste Baiano",
  "Refúgio da Vida Silvestre Veredas do Oeste Baiano",
  "PARQUE NACIONAL DE BRASÍLIA"
))
esperado <- c(
  "refúgio vida silvestre veredas oeste baiano",
  "refúgio vida silvestre veredas oeste baiano",
  "parque nacional brasília"
)
assert(identical(obtido, esperado), "A função movida não preservou o comportamento homologado.")

cat(sprintf(
  "TEST_V2926_R03_RELATORIO_CHECKPOINT_ORDEM_OK; definicao=%d; primeiro_uso=%d; primeiro_checkpoint=%d; semantica_identica_r02=TRUE\n",
  definicoes[[1L]], min(usos), min(checkpoint)
))
