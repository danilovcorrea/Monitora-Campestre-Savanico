script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")

falhar <- function(...) stop(paste0(...), call. = FALSE)

trecho_entre <- function(linhas, inicio_padrao, fim_padrao = NULL, apos_inicio = TRUE) {
  ini <- grep(inicio_padrao, linhas, perl = TRUE)
  if (!length(ini)) return(character())
  ini <- ini[1L]
  if (is.null(fim_padrao)) return(linhas[ini:length(linhas)])
  fim_rel <- grep(fim_padrao, linhas[seq.int(ini + as.integer(apos_inicio), length(linhas))], perl = TRUE)
  if (!length(fim_rel)) return(linhas[ini:length(linhas)])
  fim <- ini + as.integer(apos_inicio) + fim_rel[1L] - 2L
  linhas[ini:max(ini, fim)]
}

render_ui_fn <- paste(trecho_entre(
  linhas,
  "output\\$ui_valor_novo_controle\\s*<-\\s*shiny::renderUI",
  "output\\$ui_contrato_entrada_info\\s*<-\\s*shiny::renderUI"
), collapse = "\n")

if (!nzchar(render_ui_fn)) falhar("output$ui_valor_novo_controle nao encontrado.")

painel_texto <- paste(linhas, collapse = "\n")

# -----------------------------------------------------------------------
# 1. dateInput removido: nao pre-preenche mais com Sys.Date()
# -----------------------------------------------------------------------
if (grepl("dateInput", render_ui_fn, fixed = TRUE)) {
  falhar(
    "dateInput ainda presente em output$ui_valor_novo_controle. ",
    "Correcao Bloco 10b: substituir por textInput(value='', placeholder='YYYY-MM-DD')."
  )
}

# -----------------------------------------------------------------------
# 2. textInput com value="" e placeholder="YYYY-MM-DD" para tipo date
# -----------------------------------------------------------------------
if (!grepl('"YYYY-MM-DD"', render_ui_fn, fixed = TRUE)) {
  falhar(
    "textInput para tipo date deve usar placeholder = \"YYYY-MM-DD\". ",
    "Bolsista precisa digitar a data manualmente; campo deve comecar vazio."
  )
}
if (!grepl('value\\s*=\\s*""', render_ui_fn, perl = TRUE)) {
  falhar("textInput para data deve ter value = \"\" (string vazia, sem pre-preenchimento).")
}

# -----------------------------------------------------------------------
# 3. Widgets de hora e data/hora continuam usando textInput (sem regressao)
# -----------------------------------------------------------------------
if (!grepl("HH:MM:SS", render_ui_fn, fixed = TRUE)) {
  falhar("Widget de hora (tipo='time') deve continuar usando textInput com placeholder HH:MM:SS.")
}
if (!grepl("YYYY-MM-DDTHH:MM:SS", render_ui_fn, fixed = TRUE)) {
  falhar("Widget de data/hora (tipo='datetime') deve continuar usando textInput com placeholder YYYY-MM-DDTHH:MM:SS.")
}

# -----------------------------------------------------------------------
# 4. Validacao de valor vazio ainda bloqueia (bloqueada_valor_vazio)
#    textInput vazio sera rejeitado antes de qualquer check de formato
# -----------------------------------------------------------------------
if (!grepl("bloqueada_valor_vazio", painel_texto, fixed = TRUE)) {
  falhar("Validacao bloqueada_valor_vazio deve existir: campo de data vazio deve ser rejeitado.")
}

# -----------------------------------------------------------------------
# 5. Validacao de formato de data ainda existe (bloqueada_formato_data)
#    Data invalida digitada pelo bolsista e rejeitada com mensagem clara
# -----------------------------------------------------------------------
if (!grepl("bloqueada_formato_data", painel_texto, fixed = TRUE)) {
  falhar("Validacao bloqueada_formato_data deve existir: data em formato errado deve ser rejeitada.")
}

cat("PAINEL_DATA_HORA_SEM_DATA_ATUAL_ESTATICA_OK\n")
