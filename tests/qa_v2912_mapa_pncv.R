#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "Uso: Rscript tests/qa_v2912_mapa_pncv.R <script> <output_fonte> <dir_qa>",
    call. = FALSE
  )
}
script <- normalizePath(args[[1L]], mustWork = TRUE)
fonte <- normalizePath(args[[2L]], mustWork = TRUE)
dir_qa <- normalizePath(args[[3L]], mustWork = FALSE)
expectativa_limite_principal <- if (length(args) < 4L) {
  "S"
} else {
  toupper(trimws(args[[4L]]))
}
dir.create(dir_qa, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(digest)
})

arvore <- parse(file = script, keep.source = FALSE)
funcoes <- new.env(parent = globalenv())
coletar_funcoes <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]])) {
    nome <- as.character(expr[[2L]])
    rhs <- expr[[3L]]
    definicao_funcional <- is.call(rhs) &&
      identical(as.character(rhs[[1L]]), "function")
    if (isTRUE(definicao_funcional) && (
      startsWith(nome, "monitora_relatorios_analiticos_") ||
        nome %in% c(
          "monitora_arquivo_retentativas",
          "monitora_arquivo_hash_transacao",
          "monitora_arquivo_publicar_candidato",
          "monitora_relatorio_rotulo_formacao"
        )
    )) eval(expr, envir = funcoes)
    return(invisible(NULL))
  }
  for (ii in seq_along(expr)[-1L]) coletar_funcoes(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar_funcoes))
list2env(as.list.environment(funcoes, all.names = TRUE), envir = .GlobalEnv)

MONITORA_SCRIPT_VERSAO <- "2.9.12"
MONITORA_SCRIPT_BUILD_ID <-
  "v2.9.12-20260814"

stat <- fread(
  file.path(fonte, "01_produtos_dados", "registros_corrig_stat.csv"),
  encoding = "UTF-8",
  showProgress = FALSE
)
candidatos_continuidade <- list.files(
  file.path(fonte, "08_relatorios_analiticos"),
  pattern = "^continuidade_uas[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
stopifnot(length(candidatos_continuidade) == 1L)
arquivo_continuidade <- candidatos_continuidade[[1L]]
uc_slug <- basename(dirname(arquivo_continuidade))
continuidade <- fread(
  arquivo_continuidade,
  encoding = "UTF-8",
  showProgress = FALSE
)

dir_figuras <- file.path(dir_qa, "figuras")
dir_cache <- file.path(
  dir_qa,
  "90_cache",
  "relatorios_analiticos_sentinel2",
  uc_slug
)
dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_cache, recursive = TRUE, showWarnings = FALSE)

cache_fonte <- file.path(
  fonte,
  "90_cache",
  "relatorios_analiticos_sentinel2",
  uc_slug
)
if (dir.exists(cache_fonte)) {
  arquivos_cache <- list.files(cache_fonte, full.names = TRUE)
  if (length(arquivos_cache)) {
    file.copy(arquivos_cache, dir_cache, overwrite = TRUE, recursive = FALSE)
  }
}

inicio <- proc.time()[["elapsed"]]
resultado <- monitora_relatorios_analiticos_mapas(
  stat = stat,
  continuidade = continuidade,
  dir_figuras = dir_figuras,
  dir_cache = dir_cache,
  mapa_satelite = TRUE,
  fonte_mapa_satelite = "SENTINEL2_PUBLICO"
)
duracao <- proc.time()[["elapsed"]] - inicio

stopifnot(
  isTRUE(resultado$status_satelite$gerado[[1L]]),
  isTRUE(resultado$status_limite_uc$localizado[[1L]]),
  file.exists(resultado$mapa_satelite),
  file.info(resultado$mapa_satelite)$size > 10000L
)
if (identical(expectativa_limite_principal, "S")) {
  stopifnot(
    "limite_uc_mapa_principal" %in% names(resultado$status_satelite),
    isTRUE(resultado$status_satelite$limite_uc_mapa_principal[[1L]]),
    grepl(
    "Contorno amarelo: limite oficial da UC.",
    resultado$status_satelite$legenda_relatorio[[1L]],
    fixed = TRUE
    )
  )
  stopifnot(grepl(
    "no mapa principal e no localizador.",
    resultado$metadados_cartograficos[
      elemento_mgb2 == "linhagem",
      valor
    ][[1L]],
    fixed = TRUE
  ))
}
if (identical(expectativa_limite_principal, "N")) {
  stopifnot(
    "limite_uc_mapa_principal" %in% names(resultado$status_satelite),
    !isTRUE(resultado$status_satelite$limite_uc_mapa_principal[[1L]]),
    !grepl(
      "Contorno amarelo: limite oficial da UC.",
      resultado$status_satelite$legenda_relatorio[[1L]],
      fixed = TRUE
    )
  )
  stopifnot(grepl(
    "apenas no localizador, pois o contorno não intercepta a moldura principal.",
    resultado$metadados_cartograficos[
      elemento_mgb2 == "linhagem",
      valor
    ][[1L]],
    fixed = TRUE
  ))
}

fwrite(
  resultado$status_satelite,
  file.path(dir_qa, "auditoria_mapa_satelite.csv")
)
fwrite(
  resultado$status_limite_uc,
  file.path(dir_qa, "auditoria_limite_uc_oficial.csv")
)
fwrite(
  resultado$metadados_cartograficos,
  file.path(dir_qa, "metadados_cartograficos_mgb2.csv")
)
cat(
  "QA_V2912_MAPA_PNCV_OK\n",
  "MAPA=", normalizePath(resultado$mapa_satelite, winslash = "/"), "\n",
  "DURACAO_S=", sprintf("%.3f", duracao), "\n",
  sep = ""
)
