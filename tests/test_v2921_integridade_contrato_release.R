#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.21-dev_r01.R",
  winslash = "/", mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.20.R",
  winslash = "/", mustWork = TRUE
)

exigir <- function(ok, mensagem) if (!isTRUE(ok)) stop(mensagem, call. = FALSE)
linhas_c <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
linhas_b <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto_c <- paste(linhas_c, collapse = "\n")
arvore_c <- parse(candidata, keep.source = FALSE, encoding = "UTF-8")
arvore_b <- parse(baseline, keep.source = FALSE, encoding = "UTF-8")

exigir(
  length(arvore_c) == 1L && identical(arvore_c[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)

inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio_congelado(linhas_c), inicio_congelado(linhas_b)),
  "A inicialização congelada do RStudio divergiu da v2.9.20."
)

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
  exigir(length(achados) == 1L, paste0("Função não unívoca: ", nome))
  achados[[1L]]
}

### O contrato XLSForm embutido é congelado: tanto o corpo da função quanto
### o objeto materializado precisam permanecer rigorosamente idênticos.
payload_c <- encontrar_funcao(arvore_c[[1L]], "monitora_correcao_xlsforms_embutidos")
payload_b <- encontrar_funcao(arvore_b[[1L]], "monitora_correcao_xlsforms_embutidos")
exigir(
  identical(payload_c, payload_b),
  "O corpo da função que materializa o contrato XLSForm embutido foi alterado."
)
env_c <- new.env(parent = globalenv())
env_b <- new.env(parent = globalenv())
hex_c <- encontrar_funcao(arvore_c[[1L]], "monitora_correcao_hex_para_raw")
hex_b <- encontrar_funcao(arvore_b[[1L]], "monitora_correcao_hex_para_raw")
exigir(identical(hex_c, hex_b), "O decodificador do contrato embutido foi alterado.")
env_c$monitora_correcao_hex_para_raw <- eval(hex_c, env_c)
env_b$monitora_correcao_hex_para_raw <- eval(hex_b, env_b)
env_c$monitora_correcao_xlsforms_embutidos <- eval(payload_c, env_c)
env_b$monitora_correcao_xlsforms_embutidos <- eval(payload_b, env_b)
obj_c <- env_c$monitora_correcao_xlsforms_embutidos()
obj_b <- env_b$monitora_correcao_xlsforms_embutidos()
exigir(
  identical(obj_c, obj_b) && identical(serialize(obj_c, NULL, version = 3L), serialize(obj_b, NULL, version = 3L)),
  "O conteúdo materializado do contrato XLSForm embutido divergiu da v2.9.20."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.21"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.21-20260828-r01"',
  'monitora_correcao_contrato_choices_atuais <- function',
  'monitora_correcao_contrato_mapa_outras_especies_exoticas <- function',
  'monitora_correcao_fechar_outras_especies_exoticas <- function',
  'monitora_correcao_reconciliar_outros_orfao_exotica <- function',
  '"MIGRACAO_TOKEN_ORFAO_OUTROS_EXOTICA"',
  '"bloqueada_token_fora_dominio_choices_pai"',
  '"atributo_schema129"',
  'monitora_validados_validar_condicionais_xlsform21 <- function',
  'monitora_correcao_tokens_residuo_historico_outras_formas()',
  'registros_validados.csv bloqueado'
)) exigir(grepl(trecho, texto_c, fixed = TRUE), paste0("Requisito v2.9.21 ausente: ", trecho))

exigir(
  !grepl('tokens_exotica_raiz = c("outra", "outros")', texto_c, fixed = TRUE),
  "O relatório ainda confunde o órfão `outros` com outra forma de vida."
)
exigir(
  !grepl('registros_corrig.csv bloqueado', texto_c, fixed = TRUE),
  "Mensagem final ainda atribui o bloqueio ao checkpoint corrigido."
)

limite <- 5 * 1024^2
bytes_lf <- file.info(candidata)$size
bytes_crlf <- bytes_lf + length(linhas_c)
exigir(bytes_crlf < limite, "A candidata excederia o limite fixo de 5 MiB do RStudio em CRLF.")

cat(sprintf(
  paste0(
    "TEST_V2921_INTEGRIDADE_CONTRATO_RELEASE_OK; contrato_identico=TRUE; ",
    "LF=%d; CRLF=%d; margem_RStudio=%d\n"
  ),
  bytes_lf, bytes_crlf, limite - bytes_crlf
))
