#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.23.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.22.R",
  mustWork = TRUE
)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

linhas_c <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
linhas_b <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
arvore_c <- parse(candidata, keep.source = FALSE, encoding = "UTF-8")
arvore_b <- parse(baseline, keep.source = FALSE, encoding = "UTF-8")
assert(length(arvore_c) == 1L && identical(arvore_c[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq.")

inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  assert(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
assert(identical(inicio_congelado(linhas_c), inicio_congelado(linhas_b)),
  "A inicialização congelada do RStudio divergiu da v2.9.22 pública.")

encontrar_funcao <- function(x, nome) {
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
  visitar(x)
  assert(length(achados) == 1L, paste0("Função não unívoca: ", nome))
  achados[[1L]]
}

payload_c <- encontrar_funcao(arvore_c[[1L]], "monitora_correcao_xlsforms_embutidos")
payload_b <- encontrar_funcao(arvore_b[[1L]], "monitora_correcao_xlsforms_embutidos")
hex_c <- encontrar_funcao(arvore_c[[1L]], "monitora_correcao_hex_para_raw")
hex_b <- encontrar_funcao(arvore_b[[1L]], "monitora_correcao_hex_para_raw")
assert(identical(payload_c, payload_b) && identical(hex_c, hex_b),
  "O código que materializa o contrato XLSForm embutido foi alterado.")

env_c <- new.env(parent = globalenv())
env_b <- new.env(parent = globalenv())
env_c$monitora_correcao_hex_para_raw <- eval(hex_c, env_c)
env_b$monitora_correcao_hex_para_raw <- eval(hex_b, env_b)
env_c$monitora_correcao_xlsforms_embutidos <- eval(payload_c, env_c)
env_b$monitora_correcao_xlsforms_embutidos <- eval(payload_b, env_b)
obj_c <- env_c$monitora_correcao_xlsforms_embutidos()
obj_b <- env_b$monitora_correcao_xlsforms_embutidos()
assert(
  identical(obj_c, obj_b) &&
    identical(serialize(obj_c, NULL, version = 3L), serialize(obj_b, NULL, version = 3L)),
  "O contrato XLSForm materializado divergiu da v2.9.22 pública."
)

texto <- paste(linhas_c, collapse = "\n")
for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.23"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.23-20260901-r01"',
  'contexto_dominio', 'mapa_labels'
)) assert(grepl(trecho, texto, fixed = TRUE), paste0("Requisito da candidata ausente: ", trecho))

limite <- 5 * 1024^2
bytes_lf <- file.info(candidata)$size
bytes_crlf <- bytes_lf + length(linhas_c)
assert(bytes_crlf < limite, "A candidata excederia 5 MiB no RStudio em CRLF.")

cat(sprintf(
  paste0("TEST_V2923_INTEGRIDADE_CONTRATO_RELEASE_OK; contrato_identico=TRUE; ",
    "inicio_RStudio_identico=TRUE; LF=%d; CRLF=%d; margem_RStudio=%d\n"),
  bytes_lf, bytes_crlf, limite - bytes_crlf
))
