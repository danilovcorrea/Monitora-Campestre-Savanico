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
mapas <- if (length(args) >= 3L) {
  vapply(args[3:length(args)], normalizePath, character(1L), mustWork = TRUE)
} else {
  character()
}

exigir <- function(ok, mensagem) {
  if (!isTRUE(ok)) stop(mensagem, call. = FALSE)
}
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
  sub(
    'MONITORA_SCRIPT_BUILD_ID <- ".*"',
    'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"',
    z
  )
}
exigir(
  identical(inicio(linhas), inicio(base)),
  "A seção congelada anterior às variáveis manuais divergiu da v2.9.11."
)

for (trecho in c(
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.12-20260814"',
  "largura_prancha_px <- 2800L",
  "altura_prancha_px <- 3200L",
  "fracao_faixa_inferior <- 0.21",
  "margem_lateral_mapa_linhas <- 2.0",
  "fig = c(0, 1, fracao_faixa_inferior, 1)",
  "fig_faixa_inferior <- c(0, 1, 0, fracao_faixa_inferior)",
  "margem_faixa_y <- margem_ndc_y / fracao_faixa_inferior",
  "fracao_faixa_inferior * tamanho_dispositivo_pol[[2L]]"
)) {
  exigir(
    grepl(trecho, texto, fixed = TRUE),
    paste0("Contrato de largura cartográfica ausente: ", trecho)
  )
}
for (legado in c(
  "fig = c(0, 1, 0.24, 1)",
  "fig_faixa_inferior <- c(0, 1, 0, 0.24)",
  "mar = c(3.7, 4.6, 4.7, 3.8)"
)) {
  exigir(
    !grepl(legado, texto, fixed = TRUE),
    paste0("Geometria cartográfica legada ainda ativa: ", legado)
  )
}
for (hardcode in c("PNCV", "PNCA", "APAI", "C:/scr_test")) {
  exigir(
    !grepl(hardcode, texto, fixed = TRUE),
    paste0("Hardcode geográfico/local introduzido: ", hardcode)
  )
}

if (length(mapas)) {
  exigir(requireNamespace("png", quietly = TRUE), "Pacote png indisponível.")
  for (arquivo in mapas) {
    imagem <- png::readPNG(arquivo)
    altura <- dim(imagem)[[1L]]
    largura <- dim(imagem)[[2L]]
    exigir(
      identical(c(largura, altura), c(2800L, 3200L)),
      paste0("Dimensão inesperada: ", basename(arquivo))
    )
    linhas_mapa <- seq_len(floor(altura * 0.79))
    nao_branco <- imagem[linhas_mapa, , 1L] < 0.985 |
      imagem[linhas_mapa, , 2L] < 0.985 |
      imagem[linhas_mapa, , 3L] < 0.985
    colunas <- which(colSums(nao_branco) >= 5L)
    largura_externa <- (max(colunas) - min(colunas) + 1L) / largura
    exigir(
      largura_externa >= 0.975,
      paste0(
        "Moldura/coordenadas ocupam menos de 97,5% da largura: ",
        basename(arquivo)
      )
    )
    cat(sprintf(
      "%s: %d x %d px; largura externa=%.2f%%; margens=%d/%d px\n",
      basename(dirname(dirname(arquivo))),
      largura,
      altura,
      100 * largura_externa,
      min(colunas) - 1L,
      largura - max(colunas)
    ))
    rm(imagem, nao_branco)
    invisible(gc())
  }
}

cat("TEST_V2912_LARGURA_CARTOGRAFICA_OK\n")
