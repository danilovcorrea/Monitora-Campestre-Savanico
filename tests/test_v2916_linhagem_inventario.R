#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.16.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
arvore <- parse(file = script, keep.source = FALSE)
alvos <- c(
  "monitora_linhagem_inventario_sessoes_importar",
  "monitora_linhagem_inventario_sessoes_dt"
)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  if (length(x) > 1L) for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)),
       "Funções do inventário não puderam ser extraídas.")

env$monitora_linhagem_hash_arquivo <- function(path) {
  digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}
env$monitora_linhagem_metadados_sessoes_consolidar <- function() data.table()
env$MONITORA_TRILHA_SEMANTICA_HERDADA <- data.table()
env$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table()
env$MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table()
env$MONITORA_REPLAY_APLICACOES <- data.table()
env$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- data.table()
env$MONITORA_EXEC_ID <- "20260821_999999"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.16-20260821"

tmp <- tempfile("v2916_inventario_")
dir.create(file.path(tmp, "linhagem"), recursive = TRUE)
on.exit({
  unlink(tmp, recursive = TRUE, force = TRUE)
  rm(
    list = intersect(
      c(
        "MONITORA_INVENTARIO_SESSOES_LINHAGEM_HERDADO",
        "MONITORA_INVENTARIO_SESSOES_LINHAGEM_STATUS"
      ),
      ls(envir = .GlobalEnv, all.names = TRUE)
    ),
    envir = .GlobalEnv
  )
}, add = TRUE)

inventario <- data.table(
  ordem_sessao = c("1", "2"),
  exec_id = c("20260813_010243", "20260821_184721"),
  build = c("v2.9.10-20260813", "v2.9.16-20260821"),
  data_hora_referencia = c(
    "nao_registrada_no_legado",
    "2026-08-21 18:48:40 -0300"
  )
)
arquivo <- file.path(tmp, "linhagem", "inventario_sessoes_linhagem.csv")
fwrite(inventario, arquivo, bom = TRUE)
manifesto <- list(
  inventario_sessoes_sha256 = env$monitora_linhagem_hash_arquivo(arquivo),
  session_inventory_count = 2L
)
importado <- env$monitora_linhagem_inventario_sessoes_importar(tmp, manifesto)
exigir(identical(importado$exec_id, inventario$exec_id),
       "A importação alterou a ordem do inventário assinado.")
env$MONITORA_INVENTARIO_SESSOES_LINHAGEM_HERDADO <- copy(importado)

resultado <- env$monitora_linhagem_inventario_sessoes_dt()$sessoes
exigir(
  identical(
    resultado$exec_id,
    c(inventario$exec_id, env$MONITORA_EXEC_ID)
  ),
  "A continuidade não preservou a ordem herdada e a sessão atual ao final."
)
exigir(all(resultado$n_eventos_criados[1:2] == 0L),
       "Sessões somente inventariadas criaram eventos artificiais.")

fwrite(inventario[1L], arquivo, bom = TRUE)
erro_hash <- tryCatch(
  {
    env$monitora_linhagem_inventario_sessoes_importar(tmp, manifesto)
    ""
  },
  error = conditionMessage
)
exigir(grepl("hash do inventário", erro_hash, fixed = TRUE),
       "Alteração do inventário assinado não foi bloqueada.")

cat("TEST_V2916_LINHAGEM_INVENTARIO_OK\n")
