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

# 1. Dominio select_one/select_multiple excluido de atributos cadastrais (is.na(cad_tipo))
#    A condicao de entrada do bloco de dominio deve verificar is.na(cad_tipo), garantindo
#    que UC (e ea/ua/ciclo/campanha) nao gere atributo_101_valor_fora_dominio via token XLSForm.
if (!grepl("select_one.*is\\.na\\(cad_tipo\\)|is\\.na\\(cad_tipo\\).*select_one", auditoria_101, perl = TRUE)) {
  falhar(
    "A condicao do bloco de dominio select_one/select_multiple deve incluir is.na(cad_tipo) ",
    "para excluir atributos cadastrais (uc/ea/ua/ciclo/campanha) da comparacao com tokens XLSForm."
  )
}

# 2. atributo_101_valor_fora_dominio ainda existe na funcao (nao foi removido genericamente)
if (!grepl("atributo_101_valor_fora_dominio", auditoria_101, fixed = TRUE)) {
  falhar("atributo_101_valor_fora_dominio deve existir na funcao de auditoria 101 (validacao nao removida).")
}

# 3. cadastro_uc_ausente_ou_invalida preservado: check de vazio para atributos cadastrais ainda existe
if (!grepl("cadastro_uc_ausente_ou_invalida", auditoria_101, fixed = TRUE)) {
  falhar("cadastro_uc_ausente_ou_invalida deve existir: UC sem valor ainda e pendencia impeditiva.")
}
if (!grepl("cad_tipo", auditoria_101, fixed = TRUE)) {
  falhar("cad_tipo deve ser usado na funcao para tratar atributos cadastrais.")
}

# 4. O bloco de cad_tipo ainda verifica vazio (not any present) — auditoria de UC vazia preservada
trecho_cad <- paste(trecho_entre(
  strsplit(auditoria_101, "\n", fixed = TRUE)[[1L]],
  "cad_tipo\\s*<-\\s*switch",
  "tipo_base\\s*<-\\s*as\\.character"
), collapse = "\n")
if (!nzchar(trecho_cad)) falhar("Bloco cad_tipo nao localizado dentro da auditoria 101.")
if (!grepl("!any\\(!vazio\\(vals\\)\\)|any\\(!vazio", trecho_cad, perl = TRUE)) {
  falhar("Verificacao de vazio para atributos cadastrais deve existir (UC vazia = impeditiva).")
}

# 5. choices_seguras e a busca de dominio ainda existem (logica de dominio nao deletada)
if (!grepl("choices_seguras", auditoria_101, fixed = TRUE)) {
  falhar("choices_seguras deve existir: validacao de dominio para atributos nao-cadastrais deve permanecer.")
}

cat("AUDITORIA_101_UC_LABEL_ESTATICA_OK\n")
