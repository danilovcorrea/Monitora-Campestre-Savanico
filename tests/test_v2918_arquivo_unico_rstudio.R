#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.18.R",
  winslash = "/", mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.17.R",
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
exigir(tempo_parse_c <= tempo_parse_b * 1.25 + 0.20, "Regressão relevante no parse do arquivo único.")

### Materializa uma cópia CRLF real, valida o mesmo limite usado pelo editor e
### prova que o parser continua aceitando o artefato convertido no Windows.
crlf <- tempfile(fileext = ".R")
con <- file(crlf, open = "wb")
writeBin(charToRaw(paste0(paste(linhas_c, collapse = "\r\n"), "\r\n")), con)
close(con)
on.exit(unlink(crlf), add = TRUE)
exigir(file.info(crlf)$size == bytes_crlf, "Simulação CRLF não reproduziu o tamanho projetado.")
exigir(file.info(crlf)$size < limite, "Cópia CRLF excede o limite fixo do RStudio.")
arvore_crlf <- parse(crlf, keep.source = FALSE)
exigir(length(arvore_crlf) == 1L, "Cópia CRLF deixou de ser parseável.")

intervalo <- function(linhas, inicio, fim) {
  a <- grep(inicio, linhas)
  b <- grep(fim, linhas)
  exigir(length(a) == 1L && length(b) == 1L && b > a, "Intervalo contratual não localizado.")
  c(a, b)
}

### Fora da representação física do contrato e dos literais de versão, todo o
### código executável deve permanecer byte a byte igual à v2.9.17.
normalizar_executavel <- function(linhas, compacta = FALSE) {
  r <- if (compacta) {
    intervalo(
      linhas,
      "^monitora_correcao_hex_para_raw <- function",
      "^monitora_correcao_xlsforms_embutidos_cache_publicacao_ae <- function"
    )
  } else {
    intervalo(
      linhas,
      "^monitora_correcao_fread_tsv_embutido <- function",
      "^monitora_correcao_xlsforms_embutidos_cache_publicacao_ae <- function"
    )
  }
  linhas <- linhas[-seq.int(r[[1L]], r[[2L]] - 1L)]
  linhas <- linhas[!grepl("^[[:space:]]*#", linhas)]
  linhas <- linhas[nzchar(trimws(linhas))]
  linhas <- gsub("2\\.9\\.18", "<VERSAO>", linhas)
  linhas <- gsub("2\\.9\\.17", "<VERSAO>", linhas)
  linhas <- gsub("20260826-r01", "<BUILD>", linhas)
  linhas <- gsub("20260825-r01", "<BUILD>", linhas)
  linhas
}
exigir(
  identical(
    normalizar_executavel(linhas_c, compacta = TRUE),
    normalizar_executavel(linhas_b, compacta = FALSE)
  ),
  "Código executável fora do contrato compactado divergiu da v2.9.17."
)

carregar_contrato <- function(linhas, compacta = FALSE) {
  r <- if (compacta) {
    intervalo(
      linhas,
      "^monitora_correcao_hex_para_raw <- function",
      "^monitora_correcao_xlsforms_embutidos_cache_publicacao_ae <- function"
    )
  } else {
    intervalo(
      linhas,
      "^monitora_correcao_fread_tsv_embutido <- function",
      "^monitora_correcao_xlsforms_embutidos_cache_publicacao_ae <- function"
    )
  }
  env <- new.env(parent = .GlobalEnv)
  eval(parse(text = linhas[r[[1L]]:(r[[2L]] - 1L)]), envir = env)
  env
}
env_b <- carregar_contrato(linhas_b)
env_c <- carregar_contrato(linhas_c, compacta = TRUE)

tempo_b <- system.time(meta_b <- env_b$monitora_correcao_xlsforms_embutidos())[["elapsed"]]
tempo_c <- system.time(meta_c <- env_c$monitora_correcao_xlsforms_embutidos())[["elapsed"]]
nomes <- c("campos", "opcoes", "dependencias", "arquivos")
exigir(identical(names(meta_b), nomes) && identical(names(meta_c), nomes), "Componentes contratuais divergentes.")
for (nome in nomes) {
  exigir(
    identical(as.data.frame(meta_b[[nome]]), as.data.frame(meta_c[[nome]])),
    paste0("Contrato compactado divergiu em ", nome, ".")
  )
}
exigir(tempo_c <= tempo_b * 2 + 0.10, "Restauração do contrato compactado introduziu regressão relevante.")

cat(sprintf(
  paste0(
    "TEST_V2918_ARQUIVO_UNICO_RSTUDIO_OK; LF=%d; CRLF=%d; margem=%d; ",
    "parse_baseline=%.3fs; parse_candidata=%.3fs; contrato_baseline=%.3fs; contrato_candidata=%.3fs\n"
  ),
  bytes_c, bytes_crlf, limite - bytes_crlf,
  tempo_parse_b, tempo_parse_c, tempo_b, tempo_c
))
