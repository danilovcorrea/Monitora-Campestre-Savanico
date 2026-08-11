#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Uso: test_v295_seca_morta_performance.R SCRIPT_BASE SCRIPT_NOVO PNCV_CSV", call. = FALSE)
}
scripts <- c(
  base = normalizePath(args[[1L]], mustWork = TRUE),
  novo = normalizePath(args[[2L]], mustWork = TRUE)
)
entrada <- normalizePath(args[[3L]], mustWork = TRUE)
dados <- fread(
  entrada, encoding = "UTF-8", na.strings = NULL,
  colClasses = "character", showProgress = FALSE
)

carregar_ambiente <- function(script) {
  arvore <- parse(file = script, keep.source = FALSE)
  env <- new.env(parent = globalenv())
  alvos <- c(
    "monitora_diag_seca_morta_ocorrencias_revisao",
    "monitora_diag_seca_morta_gravar_relatorio_operacional",
    "monitora_relatorio_exoticas_normalizar_token",
    "monitora_relatorio_exoticas_tem_token",
    "monitora_correcao_normalizar_nome_coluna"
  )
  coletar <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1L]])[1L]
    if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
        is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
      nome <- as.character(x[[2L]])
      if (nome %in% alvos) eval(x, env)
      return(invisible(NULL))
    }
    for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
    invisible(NULL)
  }
  invisible(lapply(as.list(arvore), coletar))
  stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))
  env$MONITORA_COL_ROW_ID <- ".monitora_row_id"
  primeira <- function(dt, exatos, padrao) {
    hit <- exatos[exatos %in% names(dt)]
    if (length(hit)) return(hit[[1L]])
    norm <- env$monitora_correcao_normalizar_nome_coluna(names(dt))
    idx <- which(grepl(padrao, norm, perl = TRUE))
    if (length(idx)) names(dt)[idx[[1L]]] else NA_character_
  }
  env$monitora_correcao_colunas_chave <- function(dt) list(
    coleta = primeira(dt, c("COLETA", "coleta"), "^coleta$"),
    uc = primeira(dt, c("UC", "uc"), "^uc$|unidade.*conserv"),
    ea = primeira(dt, c("EA", "ea"), "^ea$|estacao_amostral|estrato.*amostral"),
    ua = primeira(dt, c("UA", "ua"), "^ua$|unidade.*amostral"),
    ano = primeira(dt, c("ANO", "ano"), "^ano$"),
    tipo_forma_vida = primeira(
      dt,
      c("amostragem/registro/tipo_forma_vida", "tipo_forma_vida",
        "**Encostam** na vareta: (amostragem/registro)"),
      "tipo.*forma.*vida|encostam.*vareta"
    )
  )
  env$monitora_correcao_colunas_forma_vida_categoria <- function(...) character()
  env$monitora_correcao_xlsform_meta_atual <- function(...) list(opcoes = data.table())
  env$monitora_diag_rel_write_dt <- function(x, arquivo) invisible(arquivo)
  env
}

ambientes <- lapply(scripts, carregar_ambiente)
medicoes <- rbindlist(lapply(names(ambientes), function(versao) {
  env <- ambientes[[versao]]
  ocorrencias <- env$monitora_diag_seca_morta_ocorrencias_revisao(dados)
  tempos <- numeric(5L)
  resultados <- vector("list", length(tempos))
  for (ii in seq_along(tempos)) {
    tempos[[ii]] <- system.time({
      resultados[[ii]] <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
        dados, ocorrencias, tempfile(paste0("v295_perf_seca_", versao, "_")),
        paste0("perf_", versao)
      )
    })[["elapsed"]]
  }
  data.table(
    versao,
    n_ocorrencias = nrow(ocorrencias),
    n_coletas = nrow(resultados[[length(resultados)]]$operacional),
    mediana_s = median(tempos),
    minimo_s = min(tempos),
    maximo_s = max(tempos)
  )
}))

stopifnot(
  uniqueN(medicoes$n_ocorrencias) == 1L,
  uniqueN(medicoes$n_coletas) == 1L,
  medicoes[versao == "novo", mediana_s] <=
    medicoes[versao == "base", mediana_s] * 2 + 0.25
)
print(medicoes)
cat("TEST_V295_SECA_MORTA_PERFORMANCE_OK\n")
