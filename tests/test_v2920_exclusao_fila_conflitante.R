#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
arquivo_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
raiz <- if (length(arquivo_arg)) {
  normalizePath(file.path(dirname(arquivo_arg[[1L]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
args_trailing <- commandArgs(trailingOnly = TRUE)
script <- if (length(args_trailing)) {
  normalizePath(args_trailing[[1L]], mustWork = TRUE)
} else file.path(raiz, "monitora_campsav_alvo_global_v2.9.20.R")
stopifnot(file.exists(script))
linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")

inicio <- grep("^    shiny::observeEvent\\(input\\$excluir_correcoes_pendentes, \\{$", linhas)
stopifnot(length(inicio) == 1L)
fim_candidatos <- grep("^    shiny::observeEvent\\(input\\$usar_exotica_selecionada, \\{$", linhas)
fim <- fim_candidatos[fim_candidatos > inicio[[1L]]][[1L]] - 1L
bloco <- paste(linhas[inicio[[1L]]:fim], collapse = "\n")

# A exclusão é a operação de recuperação: ela precisa ser efetivada mesmo
# quando outras intenções da fila ainda apresentam conflitos.
stopifnot(grepl(
  "monitora_painel_reconciliar_fila_semantica(solicitadas, gravar = TRUE, bloquear = FALSE)",
  bloco, fixed = TRUE
))
stopifnot(!grepl("if (rec_pos_exclusao$n_conflitos > 0L) return(NULL)", bloco, fixed = TRUE))
stopifnot(grepl("rv$correcoes_solicitadas <- data.table::copy(solicitadas)", bloco, fixed = TRUE))
stopifnot(grepl("rv$correcoes <- data.table::copy(rec_pos_exclusao$corr)", bloco, fixed = TRUE))
stopifnot(grepl("Permanecem ", bloco, fixed = TRUE))
stopifnot(grepl("antes de atualizar a prévia integral ou salvar", bloco, fixed = TRUE))

suppressPackageStartupMessages(library(data.table))
corpo <- parse(script, keep.source = FALSE)[[1L]][[2L]]
ambiente <- new.env(parent = globalenv())
for (expressao in as.list(corpo)[-1L]) {
  eh_funcao <- is.call(expressao) && identical(expressao[[1L]], as.name("<-")) &&
    is.symbol(expressao[[2L]]) && is.call(expressao[[3L]]) &&
    identical(expressao[[3L]][[1L]], as.name("function"))
  if (isTRUE(eh_funcao)) try(eval(expressao, envir = ambiente), silent = TRUE)
}
ambiente$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
dados <- data.table::data.table(COLETA = "C1", atributo = "original", MONITORA_ROW_ID = "r1")
criar <- function(id, novo) ambiente$monitora_correcao_criar_operacao(
  id, "homologacao", "edicao_campo", 1L, "registro",
  coleta = "C1", linha_indice = 1L, atributo = "atributo", acao = "update",
  valor_original = "original", valor_novo = novo, n_esperado = 1L, n_alvo = 1L
)
fila <- data.table::rbindlist(list(
  criar("EXCLUIR", "a"), criar("CONFLITO_1", "b"), criar("CONFLITO_2", "c")
), fill = TRUE, use.names = TRUE)
solicitadas <- fila[id_correcao != "EXCLUIR"]
resultado <- ambiente$monitora_correcao_reconciliar_plano_semantico(
  dados, solicitadas, contexto = "teste_exclusao_recuperacao",
  gravar = FALSE, abortar = FALSE
)
stopifnot(
  nrow(solicitadas) == 2L,
  !("EXCLUIR" %in% solicitadas$id_correcao),
  resultado$n_conflitos == 1L,
  setequal(resultado$corr$id_correcao, c("CONFLITO_1", "CONFLITO_2"))
)

message("OK: exclusão recupera atomicamente a fila e mantém conflitos remanescentes auditados e bloqueados.")
