#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(if (length(args) >= 1L) args[[1L]] else "monitora_campsav_alvo_global_v2.9.7.R", mustWork = TRUE)
baseline <- normalizePath(if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.6.R", mustWork = TRUE)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
ler <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
linhas <- ler(candidato); texto <- paste(linhas, collapse = "\n"); linhas_base <- ler(baseline)

arvore <- parse(file = candidato, keep.source = FALSE)
exigir(length(arvore) == 1L && is.call(arvore[[1L]]) && identical(arvore[[1L]][[1L]], quote(base::evalq)),
       "A release deve conservar uma única expressão externa base::evalq.")
inicio_normalizado <- function(x) {
  ini <- match("base::evalq({", x)
  fim <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(ini) && !is.na(fim), "Bloco de inicialização não localizado.")
  z <- x[ini:(fim - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(identical(inicio_normalizado(linhas), inicio_normalizado(linhas_base)),
       "A inicialização anterior às variáveis manuais divergiu da v2.9.6 homologada.")
exigir(grepl('MONITORA_SCRIPT_VERSAO <- "2.9.7"', texto, fixed = TRUE), "Versão pública incorreta.")
exigir(grepl('MONITORA_SCRIPT_BUILD_ID <- "v2.9.7-20260812.1"', texto, fixed = TRUE), "Build público incorreto.")
exigir(!grepl("2.9.7-dev", texto, fixed = TRUE), "Marcador dev permaneceu no script público.")

for (trecho in c(
  "MONITORA_METADADOS_SESSOES_SCHEMA_ATUAL", "monitora_linhagem_metadados_sessoes_normalizar",
  "monitora_linhagem_metadados_sessoes_importar", "monitora_linhagem_metadados_sessoes_consolidar",
  "metadados_sessoes_painel_consolidado.csv", "metadados_sessoes_sha256",
  "session_metadata_count", "execucoes_sem_metadados_persistidos",
  "hash_metadados_sessoes_manifesto_vs_arquivo", "## Sessões, rodadas e integridade da trilha"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Contrato v2.9.7 ausente: ", trecho))

for (arquivo in c(
  "README.md", "CHANGELOG.md", "GUIA_USUARIO_v2.9.7.md", "RELEASE_NOTES_v2.9.7.md",
  "tools/recuperar_metadados_sessoes_linhagem.R", "tests/test_v297_metadados_sessoes_linhagem.R"
)) exigir(file.exists(arquivo), paste0("Arquivo de release ausente: ", arquivo))
readme <- paste(ler("README.md"), collapse = "\n")
for (trecho in c(
  "Versão: `v2.9.7`", "metadados_sessoes_painel_consolidado.csv",
  "Copie a linhagem do **output**", "migração técnica única"
)) exigir(grepl(trecho, readme, fixed = TRUE), paste0("README incompleto: ", trecho))

tool <- paste(ler("tools/recuperar_metadados_sessoes_linhagem.R"), collapse = "\n")
exigir(grepl("revision_id", tool, fixed = TRUE) && grepl("parent_revision_id", tool, fixed = TRUE),
       "A recuperação não reconstrói ancestralidade por revisão.")
exigir(!grepl("FNCS|APAI|OneDrive", tool, perl = TRUE), "A ferramenta pública contém hardcode de UC ou caminho local.")

cat("TEST_V297_RELEASE_CONTRACT_OK\n")
