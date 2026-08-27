#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.19.R",
  winslash = "/", mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.18.R",
  winslash = "/", mustWork = TRUE
)

exigir <- function(ok, mensagem) if (!isTRUE(ok)) stop(mensagem, call. = FALSE)
limite <- 5 * 1024^2
margem_minima <- 512 * 1024
linhas_c <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
linhas_b <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
bytes_c <- file.info(candidata)$size
bytes_crlf <- bytes_c + length(linhas_c)
exigir(
  bytes_crlf <= limite - margem_minima,
  paste0("Candidata sem margem segura em CRLF: ", bytes_crlf, " bytes.")
)

tempo_parse_c <- system.time(arvore_c <- parse(candidata, keep.source = FALSE))[["elapsed"]]
tempo_parse_b <- system.time(arvore_b <- parse(baseline, keep.source = FALSE))[["elapsed"]]
exigir(
  length(arvore_c) == 1L && identical(arvore_c[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)
exigir(
  tempo_parse_c <= tempo_parse_b * 1.25 + 0.20,
  "Regressão relevante no parse do arquivo único."
)

inicio <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco inicial não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio(linhas_c), inicio(linhas_b)),
  "A seção congelada de inicialização do RStudio divergiu da v2.9.18."
)

crlf <- tempfile(fileext = ".R")
con <- file(crlf, open = "wb")
writeBin(charToRaw(paste0(paste(linhas_c, collapse = "\r\n"), "\r\n")), con)
close(con)
on.exit(unlink(crlf), add = TRUE)
exigir(file.info(crlf)$size == bytes_crlf, "Simulação CRLF não reproduziu o tamanho projetado.")
exigir(file.info(crlf)$size < limite, "Cópia CRLF excede o limite fixo do RStudio.")
exigir(length(parse(crlf, keep.source = FALSE)) == 1L, "Cópia CRLF deixou de ser parseável.")

cat(sprintf(
  paste0(
    "TEST_V2919_ARQUIVO_UNICO_RSTUDIO_OK; LF=%d; CRLF=%d; margem=%d; ",
    "parse_baseline=%.3fs; parse_candidata=%.3fs\n"
  ),
  bytes_c, bytes_crlf, limite - bytes_crlf, tempo_parse_b, tempo_parse_c
))
