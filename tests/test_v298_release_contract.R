#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else "monitora_campsav_alvo_global_v2.9.8.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.7.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
ler <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
linhas <- ler(candidato)
texto <- paste(linhas, collapse = "\n")
linhas_base <- ler(baseline)

arvore <- parse(file = candidato, keep.source = FALSE)
exigir(
  length(arvore) == 1L && is.call(arvore[[1L]]) &&
    identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A release deve conservar uma única expressão externa base::evalq."
)
inicio_normalizado <- function(x) {
  ini <- match("base::evalq({", x)
  fim <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(ini) && !is.na(fim), "Bloco de inicialização não localizado.")
  z <- x[ini:(fim - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio_normalizado(linhas), inicio_normalizado(linhas_base)),
  "A inicialização anterior às variáveis manuais divergiu da baseline homologada."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.8"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.8-20260812.1"',
  'MONITORA_MODO_EXECUCAO <- "painel_incremental_completo"',
  'MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"',
  'checkpoint_schema = "monitora_painel_checkpoint_v2"',
  "monitora_painel_checkpoint_fingerprint_base",
  "monitora_painel_checkpoint_assinatura_filas",
  "monitora_painel_checkpoint_reidratar_precondicoes_lote",
  "valor_original_esperado_exato",
  "migrada_espacos_borda",
  "nao_migrada_divergencia_material",
  "retornar_falha_sem_finalizacao",
  "auditoria_preview_motor_unico_falhas_ultima_execucao.csv"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Contrato v2.9.8 ausente: ", trecho))

exigir(!grepl("2.9.8-dev", texto, fixed = TRUE), "Marcador dev permaneceu no script público.")
exigir(!grepl("PNCV|run19|OneDrive|C:/scr_test|C:\\\\scr_test", texto, perl = TRUE),
       "O script público contém hardcode de UC ou caminho local.")

for (arquivo in c(
  "README.md", "CHANGELOG.md", "GUIA_USUARIO_v2.9.8.md",
  "RELEASE_NOTES_v2.9.8.md", "VERSION"
)) exigir(file.exists(arquivo), paste0("Arquivo de release ausente: ", arquivo))

cat("TEST_V298_RELEASE_CONTRACT_OK\n")
