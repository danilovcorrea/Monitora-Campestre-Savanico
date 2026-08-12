#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.6.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.5.R",
  mustWork = TRUE
)
read_texto <- function(arq) readLines(arq, warn = FALSE, encoding = "UTF-8")
linhas <- read_texto(candidato)
linhas_base <- read_texto(baseline)
texto <- paste(linhas, collapse = "\n")
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = candidato, keep.source = FALSE)
exigir(length(arvore) == 1L, "A candidata deve conservar uma única expressão externa.")
exigir(is.call(arvore[[1L]]) && identical(arvore[[1L]][[1L]], quote(base::evalq)),
       "A expressão externa deve permanecer base::evalq.")

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
       "O bloco de inicialização anterior às variáveis manuais divergiu da v2.9.5 homologada.")

padroes <- c(
  'MONITORA_MODO_EXECUCAO <- "painel_incremental_completo"',
  'MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"',
  'MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"',
  'MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- "S"',
  'MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "N"',
  'MONITORA_OPCAO_GERAR_MANUAL_USUARIO <- "S"',
  'MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO <- "S"',
  'MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"',
  'MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"',
  'MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "S"',
  'MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"',
  'MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "S"'
)
for (padrao in padroes) exigir(grepl(padrao, texto, fixed = TRUE),
                               paste0("Default ausente: ", padrao))

for (trecho in c(
  "monitora_pendencias_justificativas_reconciliar_sessao",
  "monitora_pendencias_justificativas_preparar",
  "monitora_painel_transacao_arquivos_rollback",
  "encerrada_por_resolucao",
  "monitora_painel_tentar_salvar_e_fechar",
  "monitora_linhagem_inventario_sessoes_dt",
  "inventario_sessoes_linhagem.csv",
  "execucoes_sem_decisoes_novas",
  "metadados_sessao",
  "monitora_pendencias_biologicas_indice",
  "monitora_coletores_repeat_sanitizar_pares",
  "monitora_planilha_importacao_sismonitora_gerar_seguro",
  "falha_na_geracao_produto_opcional",
  "ids_ocorrencias_atuais",
  "monitora_relatorios_analiticos_chrome_print_isolado",
  "performance_relatorios_analiticos.csv",
  "auditoria_renderizacao_pdf_isolada.csv"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Implementação v2.9.6 ausente: ", trecho))

exigir(grepl('shiny::selectizeInput\\(\\s*"just_rotulos_lote"', texto, perl = TRUE),
       "O filtro de rótulos de justificativas não é um input estável.")
exigir(!grepl('output\\$just_rotulos_lote_ui\\s*<-', texto, perl = TRUE),
       "O filtro de rótulos voltou a ser reconstruído dinamicamente.")
exigir(length(gregexpr("if (!isTRUE(rv$preview_dirty))", texto, fixed = TRUE)[[1L]]) >= 2L,
       "As atualizações sem correções pendentes não possuem fast path suficiente.")
exigir(grepl(
  'produto_linha("registros_validados_importacao_sismonitora*.xlsx"',
  texto, fixed = TRUE
), "A planilha opcional do SISMONITORA deixou de constar na auditoria final.")
exigir(grepl(
  'espera_registros_validados_importacao_sismonitora, FALSE, 1L',
  texto, fixed = TRUE
), "A planilha opcional do SISMONITORA voltou a bloquear os demais produtos.")
exigir(!grepl(
  'intersect(rv$justificativas_selec_ids, ids_ocorrencias)',
  texto, fixed = TRUE
), "A reconciliação voltou a descartar a seleção de pendências ainda não justificadas.")
exigir(grepl(
  'tipo_tempo = "subetapa_informativa_nao_aditiva"',
  texto, fixed = TRUE
), "As subetapas dos relatórios voltaram a ser contabilizadas em duplicidade.")

exigir(length(gregexpr("rv\\$preview_dirty <- TRUE", texto, perl = TRUE)[[1L]]) >= 5L,
       "Operações espaciais não invalidam consistentemente a prévia.")
exigir(length(gregexpr('default = "."', texto, fixed = TRUE)[[1L]]) >= 3L &&
         !grepl('default = "?"', texto, fixed = TRUE),
       "Símbolo estatístico inconclusivo deixou de ser o ponto homologado.")

readme <- paste(read_texto("README.md"), collapse = "\n")
for (trecho in c(
  "Revise as opções antes de cada execução",
  "painel_incremental_completo` exige exatamente um `registros_corrig*.csv`",
  "inventario_sessoes_linhagem.csv"
)) exigir(grepl(trecho, readme, fixed = TRUE), paste0("Alerta obrigatório ausente no README: ", trecho))
exigir(grepl("Desative\\s+produtos que não sejam necessários", readme, perl = TRUE),
       "O README não orienta desativar produtos desnecessários.")

cat("TEST_V296_RELEASE_CONTRACT_OK\n")
