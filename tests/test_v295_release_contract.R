#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arquivo <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.5.R"
if (!file.exists(arquivo)) stop("Script candidato v2.9.5 não localizado: ", arquivo, call. = FALSE)

arvore <- parse(file = arquivo, keep.source = FALSE)
linhas <- readLines(arquivo, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
linha_bloco_operacional <- match(
  "### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------",
  linhas
)
if (is.na(linha_bloco_operacional)) stop("Bloco operacional principal ausente.", call. = FALSE)
texto_inicio <- paste(linhas[seq_len(linha_bloco_operacional - 1L)], collapse = "\n")
exigir <- function(ok, mensagem) if (!isTRUE(ok)) stop(mensagem, call. = FALSE)

exigir(length(arvore) == 1L,
       "O programa deve conter uma única expressão externa, como na v2.9.1.")
exigir(is.call(arvore[[1L]]) &&
         identical(arvore[[1L]][[1L]], quote(base::evalq)),
       "A expressão externa deixou de ser base::evalq, baseline da v2.9.1.")
exigir(grepl("base::evalq({", texto, fixed = TRUE),
       "O encapsulamento monolítico homologado na v2.9.1 está ausente.")
for (trecho_regressivo in c(
  "MONITORA_CARREGAMENTO_INTERNO_OK",
  "MONITORA_INICIO_BOOTSTRAP",
  "MONITORA_ARQUIVO_BOOTSTRAP",
  "arvore <- parse(file = arquivo_atual, keep.source = TRUE",
  "getActiveDocumentContext()$path",
  "getSourceEditorContext()$path",
  "Fim do corpo executado pelo carregador interno sem eco"
)) exigir(!grepl(trecho_regressivo, texto_inicio, fixed = TRUE),
          paste0("Componente regressivo de inicialização ainda presente: ", trecho_regressivo))
exigir(grepl('MONITORA_INICIO_PRIMEIRA_EXPRESSAO <- Sys.time()', texto, fixed = TRUE),
       "O relógio inicial deixou de seguir a baseline v2.9.1.")
exigir(grepl('MONITORA_SCRIPT_VERSAO <- "2.9.5"', texto, fixed = TRUE),
       "Identificação pública v2.9.5 ausente.")
exigir(grepl('MONITORA_SCRIPT_BUILD_ID <- "v2.9.5-20260811.2"', texto, fixed = TRUE),
       "Build público v2.9.5 inesperado.")
exigir(!grepl("2.9.5-dev", texto, fixed = TRUE),
       "Marca de desenvolvimento permaneceu no script público.")
for (trecho_obsoleto in c(
  "O carregador interno localiza este mesmo .R",
  "limita a chamada corrente a duas expressões externas"
)) exigir(!grepl(trecho_obsoleto, texto, fixed = TRUE),
          paste0("Descrição obsoleta da inicialização ainda presente: ", trecho_obsoleto))
for (trecho_inicio in c(
  "MONITORA_SOURCE_ECHO_DESATIVADO_AUTOMATICAMENTE <- FALSE",
  "MONITORA_RSTUDIO_SOURCE_ECHO <- local({",
  'readRStudioPreference("source_with_echo", default = FALSE)',
  'writeRStudioPreference("source_with_echo", FALSE)'
)) exigir(grepl(trecho_inicio, texto, fixed = TRUE),
          paste0("Proteção de inicialização da baseline v2.9.1 ausente: ", trecho_inicio))
exigir(grepl('MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "N"', texto, fixed = TRUE),
       "Relatórios analíticos deixaram de ser opcionais e sem custo por padrão.")
exigir(grepl('MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"', texto, fixed = TRUE),
       "Sentinel-2 deixou de ser o padrão condicionado aos relatórios.")
exigir(grepl('classe_mudanca == "inconclusivo", "·"', texto, fixed = TRUE),
       "O símbolo inconclusivo dos relatórios não foi padronizado com os PNGs técnicos.")
exigir(!grepl('classe_mudanca == "inconclusivo", "?"', texto, fixed = TRUE),
       "O símbolo obsoleto ? permaneceu no mapeamento analítico de inconclusivo.")
exigir(grepl('simbolos_permitidos <- c("", "↑", "↓", "≈", "·", "—")', texto, fixed = TRUE),
       "O vocabulário auditado dos símbolos analíticos divergiu do hotfix.")
for (legenda_obsoleta in c("? inconclusivo", "? resultado inconclusivo")) {
  exigir(!grepl(legenda_obsoleta, texto, fixed = TRUE),
         paste0("Legenda analítica obsoleta permaneceu: ", legenda_obsoleta))
}

pos_abas <- vapply(c(
  "Correções de registros", "Equipe da COLETA", "Validação espacial",
  "Justificar pendências"
), function(rotulo) regexpr(
  paste0('shiny::tabPanel(\n            "', rotulo, '"'),
  texto,
  fixed = TRUE
)[1L], integer(1L))
exigir(all(pos_abas > 0L) && identical(order(pos_abas), 1:4),
       "A ordem operacional das quatro abas divergiu do contrato v2.9.5.")

for (trecho in c(
  "Filtrar e selecionar pendências",
  "just_selecionar_filtradas",
  "just_sessao_excluir_selecionadas",
  "monitora_pendencias_justificativas_adicionar_lote_atomico",
  "monitora_pendencias_justificativas_publicar_par_atomico",
  "classificacao_triagem",
  "criterios_atendidos",
  "coletas_com_ocorrencia_ano_atual",
  "relatorio_operacional_seca_morta",
  "resumo_seca_morta_por_ua_ano",
  "trajetorias_seca_morta_por_ua",
  "diagnostico_complementar_nao_impeditivo_fora_contrato",
  "geoservicos.inde.gov.br/geoserver/ICMBio/ows",
  "artefato_final_verificado",
  "sem_comparacao_pareada",
  "auditoria_simbolos_medias_anuais_relatorio.csv"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Componente v2.9.5 ausente: ", trecho))

for (checkpoint in c(
  "entrada_fluxo_principal_painel",
  "sincronizacao_alias_coletores_pre_painel",
  "sanitizacao_coletores_pre_painel"
)) exigir(grepl(checkpoint, texto, fixed = TRUE), paste0("Checkpoint v2.9.5 ausente: ", checkpoint))

exigir(!grepl("copy_of_limite_ucs_federais_082026.zip", texto, fixed = TRUE),
       "O localizador contém hardcode do arquivo oficial corrente.")
exigir(grepl("uso_arquivo_temporario = TRUE", texto, fixed = TRUE) &&
         grepl("artefato_espacial_persistido = FALSE", texto, fixed = TRUE),
       "A garantia de uso espacial estritamente temporário divergiu.")

cat("TEST_V295_RELEASE_CONTRACT_OK\n")
