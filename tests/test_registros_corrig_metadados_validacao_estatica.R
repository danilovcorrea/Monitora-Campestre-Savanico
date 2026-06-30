script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")

falhar <- function(...) stop(paste0(...), call. = FALSE)

trecho_entre <- function(linhas, inicio_padrao, fim_padrao = NULL) {
  ini <- grep(inicio_padrao, linhas, perl = TRUE)
  if (!length(ini)) return(character())
  ini <- ini[1L]
  if (is.null(fim_padrao)) return(linhas[ini:length(linhas)])
  fim_rel <- grep(fim_padrao, linhas[seq.int(ini + 1L, length(linhas))], perl = TRUE)
  if (!length(fim_rel)) return(linhas[ini:length(linhas)])
  fim <- ini + fim_rel[1L] - 1L
  linhas[ini:max(ini, fim)]
}

sync <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_v262_sincronizar_template_2025_registros_corrig\\s*<-\\s*function",
  "^monitora_publicacao_v262_responsavel_validacao\\s*<-\\s*function"
), collapse = "\n")

resp <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_v262_responsavel_validacao\\s*<-\\s*function",
  "^monitora_publicacao_v262_preencher_metadados_validacao_registros_corrig\\s*<-\\s*function"
), collapse = "\n")

helper <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_v262_preencher_metadados_validacao_registros_corrig\\s*<-\\s*function",
  "^monitora_publicacao_v262_exportar_registros_validados_selecao\\s*<-\\s*function"
), collapse = "\n")

aa <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_aa_preparar_validar_registros_corrig\\s*<-\\s*function",
  "^monitora_registros_validados_materializado_sem_bloqueio\\s*<-\\s*function"
), collapse = "\n")

selecao <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_v262_exportar_registros_validados_selecao\\s*<-\\s*function",
  "^### contrato XLSForm21 nos atributos"
), collapse = "\n")

if (!nzchar(sync)) falhar("Sincronizacao template 2025 nao encontrada.")
if (!nzchar(resp)) falhar("Helper de fonte de validador nao encontrado.")
if (!nzchar(helper)) falhar("Helper de metadados de validacao nao encontrado.")
if (!nzchar(aa)) falhar("Fluxo AA de checkpoint nao encontrado.")
if (!nzchar(selecao)) falhar("Exportacao por selecao/reordenacao nao encontrada.")

if (!grepl("data\\.table::set\\s*\\(\\s*dt\\s*,\\s*j\\s*=\\s*\"validado\"\\s*,\\s*value\\s*=\\s*rep\\s*\\(\\s*\"sim\"", helper, perl = TRUE)) {
  falhar("validado = 'sim' deve ser preenchido em registros_corrig.")
}
if (!grepl("format\\s*\\(\\s*Sys\\.Date\\s*\\(\\s*\\)\\s*,\\s*\"%Y-%m-%d\"\\s*\\)", helper, perl = TRUE)) {
  falhar("data_validacao deve usar formato YYYY-MM-DD.")
}
if (!grepl("data\\.table::set\\s*\\(\\s*dt\\s*,\\s*j\\s*=\\s*\"data_validacao\"", helper, perl = TRUE)) {
  falhar("data_validacao deve ser preenchida em registros_corrig.")
}
if (!grepl("data\\.table::set\\s*\\(\\s*dt\\s*,\\s*j\\s*=\\s*\"obs_validacao\"", helper, perl = TRUE)) {
  falhar("obs_validacao deve ser preenchida em registros_corrig.")
}
if (!grepl("obs\\s*<-\\s*\"checkpoint_ok\"", helper, perl = TRUE)) {
  falhar("obs_validacao deve ser exatamente checkpoint_ok.")
}
if (!grepl("checkpoint_ok", helper, fixed = TRUE)) {
  falhar("obs_validacao deve registrar checkpoint_ok.")
}
if (grepl("sha256_corrig", helper, fixed = TRUE)) {
  falhar("obs_validacao nao deve conter sha256_corrig.")
}
if (!grepl("hash_interno_registros_corrig", helper, fixed = TRUE)) {
  falhar("Hash, quando existir, deve ficar apenas na auditoria/log interno.")
}
if (grepl("data_hora/data|data_hora\\\\/data|data_hora", helper, perl = TRUE)) {
  falhar("Helper de metadados nao deve alterar data_hora/data.")
}

if (!grepl("MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA", resp, fixed = TRUE)) {
  falhar("Fonte do validador deve priorizar responsavel salvo nas correcoes/painel.")
}
if (!grepl("MONITORA_RESPONSAVEL_VALIDACAO", resp, fixed = TRUE)) {
  falhar("Fonte do validador deve aceitar MONITORA_RESPONSAVEL_VALIDACAO explicito.")
}
if (grepl("Sys\\.info\\s*\\(", resp, perl = TRUE) ||
    grepl("Sys\\.getenv\\s*\\(\\s*['\"]USER(NAME)?['\"]", resp, perl = TRUE)) {
  falhar("Validador nao pode usar usuario do SO como fallback.")
}

if (!grepl("depois\\[!nzchar\\(depois\\)\\]\\s*<-\\s*\"nao\"", sync, perl = TRUE)) {
  falhar("Antes do checkpoint aprovado, validado vazio deve continuar como 'nao'.")
}

idx_chamada <- regexpr("monitora_publicacao_v262_preencher_metadados_validacao_registros_corrig\\s*\\(", aa, perl = TRUE)[1L]
idx_bloq <- regexpr("if\\s*\\(\\s*isTRUE\\s*\\(\\s*n_bloq_val\\s*>\\s*0L\\s*\\)\\s*\\)", aa, perl = TRUE)[1L]
idx_selo <- regexpr("monitora_publicacao_z_gravar_selo_registros_corrig", aa, fixed = TRUE)[1L]
if (idx_chamada < 0L) falhar("Fluxo aprovado deve chamar preenchimento de metadados em registros_corrig.")
if (idx_bloq < 0L || idx_chamada < idx_bloq) {
  falhar("Metadados de validacao so podem ser preenchidos apos bloqueios contratuais zerados.")
}
if (idx_selo < 0L || idx_chamada > idx_selo) {
  falhar("Metadados de validacao devem ser preenchidos antes do selo aprovado de registros_corrig.")
}

if (!grepl("out <- data\\.table::copy\\(dt\\[, \\.\\.cols\\]\\)", selecao, perl = TRUE)) {
  falhar("registros_validados deve herdar/reordenar colunas de registros_corrig.")
}
if (grepl("validado\\s*[:=]|j\\s*=\\s*\"validado\"|data_validacao\\s*[:=]|j\\s*=\\s*\"data_validacao\"", selecao, perl = TRUE)) {
  falhar("registros_validados nao deve recalcular metadados administrativos.")
}

cat("REGISTROS_CORRIG_METADADOS_VALIDACAO_OK\n")
