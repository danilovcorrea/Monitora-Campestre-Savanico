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

auditoria_101 <- paste(trecho_entre(
  linhas,
  "^monitora_registros_corrig_auditar_atributos_101\\s*<-\\s*function\\s*\\(",
  "^monitora_registros_corrig_contrato_dropdown_painel_101\\s*<-\\s*function\\s*\\("
), collapse = "\n")

if (!nzchar(auditoria_101)) falhar("monitora_registros_corrig_auditar_atributos_101 nao encontrada.")

linhas_fn <- strsplit(auditoria_101, "\n", fixed = TRUE)[[1L]]

# Extrair choices_seguras de dentro da funcao de auditoria
trecho_choices <- paste(trecho_entre(
  linhas_fn,
  "choices_seguras\\s*<-\\s*function\\s*\\(",
  "tokens_select_multiple\\s*<-\\s*function\\s*\\("
), collapse = "\n")

if (!nzchar(trecho_choices)) falhar("choices_seguras nao encontrada dentro de auditoria 101.")

# 1. "Savânica" aceito para form_veg: a excecao esta em choices_seguras para form_veg
if (!grepl("Sav", trecho_choices, fixed = TRUE)) {
  falhar(
    "choices_seguras deve aceitar 'Savânica' para form_veg. ",
    "registros_corrig materializa form_veg como rotulo legivel, nao como token XLSForm."
  )
}
if (!grepl("form_veg", trecho_choices, fixed = TRUE)) {
  falhar("choices_seguras deve conter excecao especifica para form_veg.")
}

# 2. A excecao e condicional a list_name == "form_veg" (nao bypass amplo)
if (!grepl("identical\\s*\\(\\s*list_name\\s*,\\s*[\"']form_veg[\"']\\s*\\)", trecho_choices, perl = TRUE)) {
  falhar(
    "A excecao de form_veg deve usar identical(list_name, 'form_veg') ",
    "para nao criar bypass amplo para todos os select_one."
  )
}

# 3. A excecao so se aplica quando choices ja foram carregadas (length(choices) > 0)
if (!grepl("length\\s*\\(\\s*choices\\s*\\)", trecho_choices, perl = TRUE)) {
  falhar(
    "A excecao de form_veg deve verificar length(choices) para nao contornar ",
    "a ausencia de XLSForm carregado."
  )
}

# 4. atributo_101_valor_fora_dominio ainda existe na funcao (nao foi removido)
if (!grepl("atributo_101_valor_fora_dominio", auditoria_101, fixed = TRUE)) {
  falhar("atributo_101_valor_fora_dominio deve existir: validacao de dominio nao pode ser removida.")
}

# 5. choices_seguras ainda chama monitora_validados_xlsform21_choices (fonte primaria preservada)
if (!grepl("monitora_validados_xlsform21_choices", trecho_choices, fixed = TRUE)) {
  falhar("choices_seguras deve continuar usando monitora_validados_xlsform21_choices como fonte primaria.")
}

# 6. Campestre tambem e aceito (outro label valido de form_veg)
if (!grepl("Campestre", trecho_choices, fixed = TRUE)) {
  falhar("choices_seguras deve aceitar 'Campestre' alem de 'Savânica' para form_veg.")
}

cat("AUDITORIA_101_FORM_VEG_LABEL_ESTATICA_OK\n")
