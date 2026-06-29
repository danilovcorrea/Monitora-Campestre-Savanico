script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Script principal não encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

normalizar <- function(x) gsub("[[:space:]]+", " ", x)

falhar <- function(...) stop(paste0(...), call. = FALSE)

exigir_regex <- function(x, padrao, mensagem) {
  if (!grepl(padrao, x, perl = TRUE)) falhar(mensagem)
}

exigir_fixo <- function(x, padrao, mensagem) {
  if (!grepl(padrao, x, fixed = TRUE)) falhar(mensagem)
}

trecho_entre <- function(linhas, inicio_padrao, fim_padrao = NULL, apos_inicio = TRUE) {
  ini <- grep(inicio_padrao, linhas, perl = TRUE)
  if (!length(ini)) return(character())
  ini <- ini[1]
  if (is.null(fim_padrao)) return(linhas[ini:length(linhas)])
  fim_rel <- grep(fim_padrao, linhas[seq.int(ini + as.integer(apos_inicio), length(linhas))], perl = TRUE)
  if (!length(fim_rel)) return(linhas[ini:length(linhas)])
  fim <- ini + as.integer(apos_inicio) + fim_rel[1] - 2L
  linhas[ini:max(ini, fim)]
}

painel_linhas <- trecho_entre(
  linhas,
  "^monitora_correcao_painel\\s*<-\\s*function\\s*\\(",
  "^monitora_cache_painel_hash_inputs\\s*<-\\s*function\\s*\\("
)

if (!length(painel_linhas)) {
  falhar("Não foi possível localizar monitora_correcao_painel().")
}

painel <- paste(painel_linhas, collapse = "\n")
painel_norm <- normalizar(painel)

idx_coleta_ui <- grep("^\\s*[\"']coleta[\"']\\s*,\\s*$", painel_linhas, perl = TRUE)
coleta_ui_vazia <- any(vapply(idx_coleta_ui, function(i) {
  janela <- paste(painel_linhas[max(1L, i - 3L):min(length(painel_linhas), i + 18L)], collapse = "\n")
  grepl("selectizeInput", janela, fixed = TRUE) &&
    grepl("selected\\s*=\\s*character\\s*\\(\\s*0\\s*\\)", janela, perl = TRUE)
}, logical(1)))
if (!coleta_ui_vazia) {
  falhar("COLETA deve abrir sem seleção inicial explícita.")
}

idx_flush <- grep("session\\$onFlushed\\s*\\(", painel_linhas, perl = TRUE)
coleta_reset_flush <- any(vapply(idx_flush, function(i) {
  janela <- paste(painel_linhas[i:min(length(painel_linhas), i + 18L)], collapse = "\n")
  grepl("updateSelectizeInput", janela, fixed = TRUE) &&
    grepl("[\"']coleta[\"']", janela, perl = TRUE) &&
    grepl("selected\\s*=\\s*character\\s*\\(\\s*0\\s*\\)", janela, perl = TRUE)
}, logical(1)))
if (!coleta_reset_flush) {
  falhar("COLETA deve ser mantida sem pré-seleção após flush/reset inicial.")
}

dados_linhas <- trecho_entre(
  painel_linhas,
  "dados_filtrados\\s*<-\\s*shiny::reactive\\s*\\(",
  "monitora_painel_filtrar_coletas_por_criterios\\s*<-\\s*function\\s*\\("
)
dados_norm <- normalizar(paste(dados_linhas, collapse = "\n"))

exigir_regex(
  dados_norm,
  "dados_filtrados\\s*<-\\s*shiny::reactive\\s*\\(",
  "Bloco dados_filtrados() não localizado."
)
exigir_regex(
  dados_norm,
  "coleta_val\\s*<-\\s*monitora_painel_valor\\s*\\(\\s*input\\$coleta\\s*\\)",
  "dados_filtrados() deve depender explicitamente de input$coleta."
)
exigir_regex(
  dados_norm,
  "!\\s*nzchar\\s*\\(\\s*coleta_val\\s*\\)[^;]*return\\s*\\(\\s*x\\s*\\[\\s*0\\s*\\]\\s*\\)",
  "dados_filtrados() deve retornar zero linhas quando COLETA está vazia."
)
exigir_regex(
  dados_norm,
  "as\\.character\\s*\\(\\s*get\\s*\\(\\s*chaves\\$coleta\\s*\\)\\s*\\)\\s*==\\s*coleta_val",
  "dados_filtrados() deve filtrar linhas pela COLETA selecionada."
)
if (!grepl("return\\s*\\(\\s*x\\s*\\[\\s*0\\s*\\]\\s*\\)", dados_norm, perl = TRUE)) {
  falhar("dados_filtrados() aparenta não proteger abertura sem COLETA.")
}

exigir_fixo(
  painel,
  "shiny::observeEvent(input$add_corr",
  "Observer da operação simples/principal não localizado."
)
exigir_fixo(
  painel,
  "monitora_painel_usar_lote_coletas()",
  "Bloco de operação em lote por coletas não localizado."
)
exigir_fixo(
  painel,
  "shiny::observeEvent(input$add_mv_lote",
  "Observer da operação em lote de múltiplos valores não localizado."
)

exigir_fixo(
  painel,
  "output$status_correcoes",
  "Bloco de atualização de contador/status de correções não localizado."
)
exigir_fixo(
  painel,
  "monitora_painel_n_operacoes_semanticas(rv$correcoes)",
  "Contador semântico de operações não localizado."
)
exigir_fixo(
  painel,
  "output$preview_pendencias_sessao",
  "Bloco de preview/resumo de pendências pós-operação não localizado."
)
exigir_fixo(
  painel,
  "monitora_painel_preview_dados_pos_operacoes",
  "Preview de dados pós-operação não localizado."
)
exigir_fixo(
  painel,
  "monitora_painel_resumo_impeditivas_preview",
  "Resumo de impeditivas pós-operação não localizado."
)
exigir_fixo(
  painel,
  "delta_linhas",
  "Resumo pós-operação deve manter atualização de delta de linhas."
)
exigir_fixo(
  painel,
  "delta_coletas",
  "Resumo pós-operação deve manter atualização de delta de coletas."
)

cat("PAINEL_REGRESSAO_ESTATICA_OK\n")
