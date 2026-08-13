#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.10.R", mustWork = TRUE)
baseline <- normalizePath(if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.9.R", mustWork = TRUE)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
ler <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
linhas <- ler(candidato); base <- ler(baseline); texto <- paste(linhas, collapse = "\n")

arvore <- parse(file = candidato, keep.source = FALSE)
exigir(length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
       "A release deve conservar uma única expressão externa base::evalq.")

inicio <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco de inicialização não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(identical(inicio(linhas), inicio(base)),
       "A seção de inicialização anterior às variáveis manuais divergiu da v2.9.9.")

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.10"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.10-20260813"',
  'MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF <- "N"',
  'monitora_doc_dir(output_dir, "07_relatorio_validacao")',
  'monitora_painel_recalcular_preview_integral_seguro',
  'falha_precondicao_lote_detectada_antes_operacoes_volumosas',
  'auditoria_preview_motor_unico_falhas_ultima_execucao.csv',
  'MONITORA_PERSISTENCIA_OPERACOES_USUARIO_PENDENTES <- FALSE',
  'monitora_relatorios_analiticos_caminho_figura',
  'file.path("figuras", basename(arq_esforco))'
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Contrato v2.9.10 ausente: ", trecho))

exigir(!grepl("2.9.10-dev", texto, fixed = TRUE), "Marcador dev permaneceu no script público.")
exigir(!grepl("bolsist", texto, ignore.case = TRUE), "Termo específico de vínculo permaneceu no script.")
exigir(!grepl("RBG|run05|OneDrive|C:/scr_test|C:\\\\scr_test", texto, perl = TRUE),
       "O script público contém hardcode de UC ou caminho local.")

for (arquivo in c("README.md", "CHANGELOG.md", "GUIA_USUARIO_v2.9.10.md", "RELEASE_NOTES_v2.9.10.md", "VERSION"))
  exigir(file.exists(arquivo), paste0("Arquivo de release ausente: ", arquivo))

cat("TEST_V2910_RELEASE_CONTRACT_OK\n")
