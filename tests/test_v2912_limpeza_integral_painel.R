#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.12.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.11.R",
  mustWork = TRUE
)

exigir <- function(ok, mensagem) {
  if (!isTRUE(ok)) stop(mensagem, call. = FALSE)
}
extrair_bloco <- function(linhas, inicio, fim) {
  a <- grep(inicio, linhas, fixed = TRUE)[1L]
  exigir(!is.na(a), paste0("Início de bloco ausente: ", inicio))
  deslocamento <- grep(fim, linhas[(a + 1L):length(linhas)], fixed = TRUE)[1L]
  exigir(!is.na(deslocamento), paste0("Fim de bloco ausente: ", fim))
  b <- a + deslocamento
  linhas[a:(b - 1L)]
}
extrair_ids_widgets <- function(texto) {
  padrao <- paste0(
    "shiny::(?:selectizeInput|selectInput|textInput|textAreaInput|",
    "checkboxGroupInput|checkboxInput|radioButtons|numericInput)",
    "\\s*\\(\\s*\"([^\"]+)\""
  )
  ocorrencias <- gregexpr(padrao, texto, perl = TRUE)[[1L]]
  if (identical(ocorrencias, -1L)) return(character())
  trechos <- regmatches(texto, list(ocorrencias))[[1L]]
  unique(sub(padrao, "\\1", trechos, perl = TRUE))
}
extrair_strings <- function(texto) {
  ocorrencias <- gregexpr('"[^"\\n]+"', texto, perl = TRUE)[[1L]]
  if (identical(ocorrencias, -1L)) return(character())
  gsub('^"|"$', "", regmatches(texto, list(ocorrencias))[[1L]])
}
inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match(
    "### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------",
    x
  )
  exigir(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub(
    'MONITORA_SCRIPT_VERSAO <- ".*"',
    'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"',
    z
  )
  sub(
    'MONITORA_SCRIPT_BUILD_ID <- ".*"',
    'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"',
    z
  )
}

linhas <- readLines(candidato, warn = FALSE, encoding = "UTF-8")
base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
arvore <- parse(file = candidato, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)
exigir(
  identical(inicio_congelado(linhas), inicio_congelado(base)),
  "A seção congelada anterior às variáveis manuais divergiu da v2.9.11."
)
exigir(
  grepl(
    'MONITORA_SCRIPT_BUILD_ID <- "v2.9.12-20260814"',
    texto,
    fixed = TRUE
  ),
  "Build público esperado da v2.9.12 ausente."
)

painel <- extrair_bloco(
  linhas,
  "monitora_correcao_painel <- function",
  "monitora_cache_painel_dirs_busca <- function"
)
painel_texto <- paste(painel, collapse = "\n")
contrato <- extrair_bloco(
  linhas,
  "monitora_painel_contrato_limpeza <- list(",
  "monitora_painel_limpar_widgets <- function"
)
contrato_texto <- paste(contrato, collapse = "\n")
ids_widgets <- extrair_ids_widgets(painel_texto)
ids_contrato <- unique(c(extrair_strings(contrato_texto), "valor_novo"))
nao_classificados <- setdiff(ids_widgets, ids_contrato)
exigir(
  !length(nao_classificados),
  paste0(
    "Controle(s) editável(is) do painel sem classificação no contrato: ",
    paste(sort(nao_classificados), collapse = ", ")
  )
)

for (id in c(
  "responsavel",
  "rv$correcoes",
  "rv$correcoes_solicitadas",
  "rv$correcoes_espaciais",
  "rv$justificativas_sessao",
  "rv$correcoes_historico_intencoes",
  "rv$auditoria_conciliacao_semantica",
  "rv$auditoria_espacial_sessao",
  "rv$auditoria_reconciliacao_justificativas"
)) {
  exigir(
    grepl(paste0('"', id, '"'), contrato_texto, fixed = TRUE),
    paste0("Estado auditável não está explicitamente preservado: ", id)
  )
}

for (id in c(
  "coletor_nome", "coletor_cpf", "coletor_motivo", "coletor_confirmar",
  "just_rotulos_lote", "just_tipo", "just_texto", "just_confirmar_lote",
  "just_sessao_filtro_rotulos", "just_sessao_filtro_coletas",
  "just_sessao_filtro_classes", "coletores_tabela",
  "just_tabela_ocorrencias", "just_tabela_sessao",
  "auditoria_perfil_painel_contrato_unico"
)) {
  exigir(
    grepl(paste0('"', id, '"'), contrato_texto, fixed = TRUE),
    paste0("Cobertura obrigatória ausente no contrato de limpeza: ", id)
  )
}

limpeza <- extrair_bloco(
  linhas,
  "monitora_painel_limpar_estado <- function",
  "shiny::observeEvent(input$limpar_filtros"
)
limpeza_texto <- paste(limpeza, collapse = "\n")
for (atribuicao_proibida in c(
  "rv$correcoes <-",
  "rv$correcoes_solicitadas <-",
  "rv$correcoes_espaciais <-",
  "rv$justificativas_sessao <-",
  "rv$correcoes_historico_intencoes <-",
  "rv$auditoria_conciliacao_semantica <-",
  "rv$auditoria_espacial_sessao <-",
  "rv$auditoria_reconciliacao_justificativas <-"
)) {
  exigir(
    !grepl(atribuicao_proibida, limpeza_texto, fixed = TRUE),
    paste0("A limpeza passou a modificar fila/histórico preservado: ", atribuicao_proibida)
  )
}

for (trecho in c(
  "rv$justificativas_selec_ids <- character(0)",
  "rv$justificativas_sessao_selec_ids <- character(0)",
  "rv$justificativas_exclusao_ids_pendente <- character(0)",
  "DT::selectRows(proxy, NULL)",
  "DT::clearSearch(proxy)",
  "monitora_just_dt_sincronizar_selecao()",
  "monitora_painel_limpar_estado(incluir_geral = TRUE)",
  "monitora_painel_limpar_estado(incluir_geral = FALSE)"
)) {
  exigir(
    grepl(trecho, painel_texto, fixed = TRUE),
    paste0("Operação obrigatória da limpeza integral ausente: ", trecho)
  )
}

for (operacao_cara in c(
  "monitora_painel_recalcular_preview_integral_seguro",
  "monitora_painel_reconciliar_justificativas_sessao",
  "data.table::fread",
  "read.csv",
  "saveRDS",
  "write.csv",
  "openxlsx::write.xlsx"
)) {
  exigir(
    !grepl(operacao_cara, limpeza_texto, fixed = TRUE),
    paste0("A limpeza visual passou a executar operação cara: ", operacao_cara)
  )
}

cat(sprintf(
  "Controles editáveis inventariados=%d; classificados=%d; não classificados=0\n",
  length(ids_widgets),
  length(intersect(ids_widgets, ids_contrato))
))
cat("TEST_V2912_LIMPEZA_INTEGRAL_PAINEL_OK\n")
