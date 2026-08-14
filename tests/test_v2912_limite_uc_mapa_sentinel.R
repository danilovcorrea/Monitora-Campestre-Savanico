#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.12.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.11.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
linhas <- readLines(candidato, warn = FALSE, encoding = "UTF-8")
base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

arvore <- parse(file = candidato, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)
inicio <- function(x) {
  a <- match("base::evalq({", x)
  b <- match(
    "### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------",
    x
  )
  exigir(!is.na(a) && !is.na(b), "Bloco inicial não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub(
    'MONITORA_SCRIPT_VERSAO <- ".*"',
    'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"',
    z
  )
  z <- sub(
    'MONITORA_SCRIPT_BUILD_ID <- ".*"',
    'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"',
    z
  )
  z
}
exigir(
  identical(inicio(linhas), inicio(base)),
  "A seção congelada anterior às variáveis manuais divergiu da v2.9.11."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.12"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.12-20260814"',
  'cor_limite_uc <- "#F4C300"',
  "limite_uc_mapa_principal = FALSE",
  "terra::project(limite_uc, crs_rgb)",
  "monitora_relatorios_analiticos_limite_intercepta_moldura",
  "linhas_limite_uc <- terra::as.lines(limite_uc_mapa)",
  '"intersects"',
  "border = cor_limite_uc",
  "labels = \"Limite da UC\"",
  'attr(ok, "limite_uc_mapa_principal") <- isTRUE(',
  "limite_uc_mapa_principal = limite_uc_mapa_principal_renderizado",
  "Contorno amarelo: limite oficial da UC."
)) {
  exigir(
    grepl(trecho, texto, fixed = TRUE),
    paste0("Revisão cartográfica ausente: ", trecho)
  )
}

for (hardcode in c("PNCV", "Chapada dos Veadeiros", "C:/scr_test")) {
  exigir(
    !grepl(hardcode, texto, fixed = TRUE),
    paste0("Hardcode geográfico/local introduzido: ", hardcode)
  )
}

ambiente_teste <- new.env(parent = globalenv())
coletar_funcao <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  if (
    identical(expr[[1L]], as.name("<-")) &&
      is.symbol(expr[[2L]]) &&
      identical(
        as.character(expr[[2L]]),
        "monitora_relatorios_analiticos_limite_intercepta_moldura"
      )
  ) {
    eval(expr, envir = ambiente_teste)
    return(invisible(NULL))
  }
  invisible(lapply(as.list(expr), coletar_funcao))
}
invisible(lapply(as.list(arvore), coletar_funcao))
exigir(
  exists(
    "monitora_relatorios_analiticos_limite_intercepta_moldura",
    envir = ambiente_teste,
    inherits = FALSE
  ),
  "Função de decisão de visibilidade não foi extraída."
)
referencia <- terra::rast(
  nrows = 10L,
  ncols = 10L,
  xmin = 2,
  xmax = 3,
  ymin = 2,
  ymax = 3,
  crs = "EPSG:4326"
)
limite_fora_da_moldura <- terra::as.polygons(
  terra::ext(0, 10, 0, 10),
  crs = "EPSG:4326"
)
limite_interceptando_moldura <- terra::as.polygons(
  terra::ext(0, 2.5, 0, 2.5),
  crs = "EPSG:4326"
)
exigir(
  !ambiente_teste$monitora_relatorios_analiticos_limite_intercepta_moldura(
    limite_fora_da_moldura,
    referencia
  ),
  "Limite exterior à moldura foi classificado indevidamente como visível."
)
exigir(
  ambiente_teste$monitora_relatorios_analiticos_limite_intercepta_moldura(
    limite_interceptando_moldura,
    referencia
  ),
  "Limite que intercepta a moldura não foi classificado como visível."
)

cat("TEST_V2912_LIMITE_UC_MAPA_SENTINEL_OK\n")
