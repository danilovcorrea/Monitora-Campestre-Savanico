#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Uso: Rscript test_v2926_r04_escopo_contrato_release.R <candidata_r04.R> <baseline_publica_r03.R>", call. = FALSE)
}
candidata <- normalizePath(args[[1L]], mustWork = TRUE)
baseline <- normalizePath(args[[2L]], mustWork = TRUE)
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
  "A inicialização congelada do RStudio divergiu da versão pública r03.")

funcoes <- function(arvore) {
  saida <- list()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    atribuicao <- as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L
    if (atribuicao && is.symbol(no[[2L]]) && is.call(no[[3L]]) &&
        identical(as.character(no[[3L]][[1L]])[1L], "function")) {
      nome <- as.character(no[[2L]])
      saida[[nome]] <<- c(saida[[nome]], list(no[[3L]]))
      return(invisible(NULL))
    }
    if (length(no) > 1L) for (ii in 2:length(no)) visitar(no[[ii]])
    invisible(NULL)
  }
  visitar(arvore[[1L]])
  saida
}
fc <- funcoes(arvore_c)
fb <- funcoes(arvore_b)
novas_autorizadas <- c(
  "monitora_doc_fmt_num", "monitora_doc_fmt_percentual",
  "monitora_doc_referencia_docx_sha256", "monitora_doc_referencia_docx_base64",
  "monitora_doc_referencia_docx_materializar", "monitora_doc_coluna_contextual_mesclavel",
  "monitora_doc_validacao_preservar_linhas_tabela",
  "monitora_persist_repeat_coletor_estado", "monitora_persist_repeat_coletor_capturar_esperado",
  "monitora_persist_repeat_coletor_reclassificar"
)
alteradas_autorizadas <- c(
  "monitora_doc_title_case_utf8", "monitora_doc_historico_relatorio",
  "monitora_doc_render_editaveis", "monitora_relatorio_validacao_consolidado_gerar",
  "monitora_correcao_auditar_persistencia_operacoes"
)
novas <- setdiff(names(fc), names(fb))
removidas <- setdiff(names(fb), names(fc))
assert(setequal(novas, novas_autorizadas), paste0("Funções novas fora do escopo: ", paste(setdiff(novas, novas_autorizadas), collapse = " | ")))
assert(!length(removidas), paste0("Funções públicas foram removidas: ", paste(removidas, collapse = " | ")))
comuns <- intersect(names(fc), names(fb))
alteradas <- comuns[!vapply(comuns, function(nm) identical(fc[[nm]], fb[[nm]]), logical(1L))]
assert(setequal(alteradas, alteradas_autorizadas), paste0(
  "Escopo funcional divergente; alteradas=", paste(sort(alteradas), collapse = " | ")
))

assert(length(fc$monitora_correcao_hex_para_raw) == 1L && length(fc$monitora_correcao_xlsforms_embutidos) == 1L,
  "Funções do contrato XLSForm deixaram de ser unívocas.")
hex_c <- eval(fc$monitora_correcao_hex_para_raw[[1L]], envir = new.env(parent = globalenv()))
hex_b <- eval(fb$monitora_correcao_hex_para_raw[[1L]], envir = new.env(parent = globalenv()))
env_c <- new.env(parent = globalenv()); env_c$monitora_correcao_hex_para_raw <- hex_c
env_b <- new.env(parent = globalenv()); env_b$monitora_correcao_hex_para_raw <- hex_b
payload_c <- eval(fc$monitora_correcao_xlsforms_embutidos[[1L]], env_c)()
payload_b <- eval(fb$monitora_correcao_xlsforms_embutidos[[1L]], env_b)()
assert(
  identical(payload_c, payload_b) &&
    identical(serialize(payload_c, NULL, version = 3L), serialize(payload_b, NULL, version = 3L)),
  "O contrato XLSForm materializado divergiu da versão pública r03."
)

texto <- paste(linhas_c, collapse = "\n")
assert(grepl('MONITORA_SCRIPT_VERSAO <- "2.9.26"', texto, fixed = TRUE), "Serial público divergente.")
assert(grepl('MONITORA_SCRIPT_BUILD_ID <- "v2.9.26-20260918-r04"', texto, fixed = TRUE), "Build r04 ausente.")
limite <- 5 * 1024^2
bytes_lf <- file.info(candidata)$size
bytes_crlf <- bytes_lf + length(linhas_c)
assert(bytes_crlf < limite, "A candidata excederia 5 MiB no RStudio em CRLF.")

cat(sprintf(
  paste0("TEST_V2926_R04_ESCOPO_CONTRATO_RELEASE_OK; contrato_identico_r03=TRUE; ",
    "inicio_RStudio_identico=TRUE; funcoes_novas=%d; funcoes_alteradas=%d; LF=%d; CRLF=%d; margem_RStudio=%d\n"),
  length(novas), length(alteradas), bytes_lf, bytes_crlf, limite - bytes_crlf
))
