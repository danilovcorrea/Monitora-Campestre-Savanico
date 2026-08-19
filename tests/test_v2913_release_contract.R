#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.13.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.12.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

arvore <- parse(file = script, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
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
  identical(inicio(linhas), inicio(base)),
  "A seção congelada anterior às variáveis manuais divergiu da v2.9.12."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.13"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.13-20260819"',
  'Haverá identificação de espécie ou outro nível taxonômico? — ',
  'nao_aplicavel_serie_temporal_uma_campanha',
  'auditoria_aplicabilidade_inferencia_temporal.csv',
  'monitora_arquivo_publicar_candidato',
  'auditoria_transacao_justificativas_ultima_execucao.csv',
  'monitora_correcao_contrato_edicao_precalcular',
  'auditoria_aquisicao_camadas_cartograficas.csv',
  'WFS_GetFeature_BBOX_rede',
  'diretorio_publico_IBGE_com_fallbacks',
  'janela_busca_config <- suppressWarnings(as.integer(janela_busca_dias)[1L])',
  'monitora_relatorios_analiticos_janelas_sentinel2',
  'n_janelas_consultadas = n_janelas_executadas',
  '" | janela consultada: ", as.integer(janela_busca_dias), " dias"',
  'limite_nuvens_area_pct = limite_nuvens_config',
  'cor_limite_uc <- "#F4C300"',
  'monitora_relatorios_analiticos_limite_intercepta_moldura',
  'labels = "Limite da UC"',
  'largura_prancha_px <- 2800L',
  'altura_prancha_px <- 3200L',
  'fracao_faixa_inferior <- 0.21',
  'monitora_painel_contrato_limpeza <- list(',
  'monitora_painel_limpar_widgets <- function',
  'monitora_painel_limpar_tabelas <- function',
  'DT::clearSearch(proxy)',
  'rv$justificativas_selec_ids <- character(0)',
  'rv$justificativas_sessao_selec_ids <- character(0)',
  'monitora_painel_limpar_estado(incluir_geral = TRUE)',
  'monitora_painel_limpar_estado(incluir_geral = FALSE)',
  'MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS <- "N"',
  'monitora_incorporacao_novas_coletas_executar <- function',
  'incorporacoes_novas_coletas.csv',
  'monitora_relatorios_analiticos_esforco_incremental <- function',
  'monitora_relatorios_analiticos_figuras_incremento <- function',
  'monitora_relatorios_analiticos_classificar_fogo_outros <- function',
  'fogo_qualquer_contexto = "Fogo — coletas com pelo menos um contexto declarado"',
  'monitora_stat_normalizar_chave_formacao <- function',
  'monitora_paleta_categorias_semantica <- function',
  'monitora_plot_gate_categorias_gerais <- function',
  'monitora_stat_adicionar_simbolo_composicao_borda <- function',
  'monitora_relatorios_analiticos_selecionar_candidato_sentinel <- function',
  'intervalo_mm <- 7.5',
  'monitora_garantir_pacotes_opcionais <- function'
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Revisão v2.9.13 ausente: ", trecho))

exigir(!grepl("2.9.13-dev", texto, fixed = TRUE), "Marcador dev permaneceu no script público.")
exigir(!grepl("dev-r[0-9]+", texto, perl = TRUE), "Build interno permaneceu no script público.")
exigir(!grepl("bolsist", texto, ignore.case = TRUE), "Termo específico de vínculo permaneceu no script.")

for (hardcode in c(
  "Estação Ecologica de Cuniã",
  "C:/scr_test", "C:\\scr_test"
)) exigir(!grepl(hardcode, texto, fixed = TRUE), paste0("Hardcode local/UC introduzido: ", hardcode))

for (arquivo in c(
  "README.md", "CHANGELOG.md", "GUIA_USUARIO_v2.9.13.md",
  "RELEASE_NOTES_v2.9.13.md", "VERSION",
  "monitora_campsav_alvo_global.R", "R_monitora_campsav_alvo_global.R",
  "R/monitora_campsav_alvo_global.R",
  "releases/v2.9.13/SHA256SUMS.txt",
  "release_assets/v2.9.13/SHA256SUMS.txt"
)) exigir(file.exists(arquivo), paste0("Arquivo de release ausente: ", arquivo))

exigir(
  identical(trimws(readLines("VERSION", warn = FALSE)), "2.9.13"),
  "VERSION não contém exatamente 2.9.13."
)
hashes <- unname(tools::md5sum(c(
  script,
  "monitora_campsav_alvo_global.R",
  "R_monitora_campsav_alvo_global.R",
  "R/monitora_campsav_alvo_global.R",
  "releases/v2.9.13/monitora_campsav_alvo_global_v2.9.13.R",
  "release_assets/v2.9.13/monitora_campsav_alvo_global_v2.9.13.R"
)))
exigir(length(unique(hashes)) == 1L, "Cópias públicas do script não são byte a byte idênticas.")

cat("TEST_V2913_RELEASE_CONTRACT_OK\n")
