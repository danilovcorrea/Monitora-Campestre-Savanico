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

helper <- paste(trecho_entre(
  linhas,
  "^monitora_registros_validados_contrato_corrig_liberado\\s*<-\\s*function",
  "^monitora_registros_validados_exportar\\s*<-\\s*function"
), collapse = "\n")

exportar <- paste(trecho_entre(
  linhas,
  "^monitora_registros_validados_exportar\\s*<-\\s*function",
  "^monitora_registros_validados_materializado_sem_bloqueio\\s*<-\\s*function"
), collapse = "\n")

if (!nzchar(helper))  falhar("Helper monitora_registros_validados_contrato_corrig_liberado nao encontrado.")
if (!nzchar(exportar)) falhar("monitora_registros_validados_exportar nao encontrado.")

# 1. Helper existe e retorna FALSE quando ha pendencias
if (!grepl("monitora_registros_validados_contrato_corrig_liberado\\s*<-\\s*function", helper, perl = TRUE)) {
  falhar("Helper nao esta definido como funcao.")
}
if (!grepl("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS", helper, fixed = TRUE)) {
  falhar("Helper deve verificar MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS.")
}
if (!grepl("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", helper, fixed = TRUE)) {
  falhar("Helper deve verificar MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21.")
}
if (!grepl("return\\s*\\(\\s*FALSE\\s*\\)", helper, perl = TRUE)) {
  falhar("Helper deve retornar FALSE quando bloqueado.")
}
if (!grepl("return\\s*\\(\\s*TRUE\\s*\\)", helper, perl = TRUE)) {
  falhar("Helper deve retornar TRUE quando liberado.")
}

# 2. exportar verifica MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS (via helper ou direto)
if (!grepl("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS", exportar, fixed = TRUE) &&
    !grepl("monitora_registros_validados_contrato_corrig_liberado", exportar, fixed = TRUE)) {
  falhar("exportar deve verificar PENDENCIAS_IMPEDITIVAS ou chamar o helper de liberacao.")
}

# 3. exportar verifica MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 ou o helper
if (!grepl("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", exportar, fixed = TRUE) &&
    !grepl("monitora_registros_validados_contrato_corrig_liberado", exportar, fixed = TRUE)) {
  falhar("exportar deve verificar CONTRATO_VALIDADO_XLSFORM21 ou chamar o helper de liberacao.")
}

# 4. exportar usa o helper
if (!grepl("monitora_registros_validados_contrato_corrig_liberado\\s*\\(", exportar, perl = TRUE)) {
  falhar("exportar deve chamar monitora_registros_validados_contrato_corrig_liberado().")
}

# 5. quando bloqueado, seta MONITORA_REGISTROS_VALIDADOS_GERADO = FALSE
if (!grepl('assign\\s*\\(\\s*"MONITORA_REGISTROS_VALIDADOS_GERADO"\\s*,\\s*FALSE', exportar, perl = TRUE)) {
  falhar("exportar deve setar MONITORA_REGISTROS_VALIDADOS_GERADO = FALSE quando bloqueado.")
}

# 6. a guarda de bloqueio precede qualquer fwrite na funcao
linhas_exportar <- strsplit(exportar, "\n", fixed = TRUE)[[1L]]
linha_guarda <- grep("monitora_registros_validados_contrato_corrig_liberado\\s*\\(", linhas_exportar, perl = TRUE)
linha_fwrite  <- grep("data\\.table::fwrite|data.table::fwrite", linhas_exportar, perl = TRUE)
if (!length(linha_guarda)) falhar("Guarda de liberacao nao localizada dentro de exportar.")
if (length(linha_fwrite) && min(linha_fwrite) < min(linha_guarda)) {
  falhar("fwrite aparece antes da guarda de liberacao em exportar.")
}

# 7. fluxo principal: registros_corrig ainda pode ser checkpoint (AA retorna invisivel com pendencias)
aa <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_aa_preparar_validar_registros_corrig\\s*<-\\s*function",
  "^monitora_registros_validados_materializado_sem_bloqueio\\s*<-\\s*function"
), collapse = "\n")
if (!grepl("return\\s*\\(\\s*invisible\\s*\\(", aa, perl = TRUE)) {
  falhar("AA deve retornar invisivel (checkpoint) mesmo com pendencias impeditivas.")
}
if (!grepl("checkpoint_com_pendencias_impeditivas", aa, fixed = TRUE)) {
  falhar("AA deve registrar contexto de checkpoint com pendencias impeditivas.")
}

# 8. Sem alteracao em arquivos proibidos
arquivos_proibidos <- c("VERSION", "README.md", "CHANGELOG.md", "CHANGELOG", "README")
mtime_script <- file.info(script_path)$mtime
for (arq in arquivos_proibidos) {
  if (file.exists(arq)) {
    mt <- file.info(arq)$mtime
    if (!is.na(mt) && !is.na(mtime_script) && mt > mtime_script) {
      falhar("Arquivo proibido modificado neste bloco: ", arq)
    }
  }
}

cat("REGISTROS_VALIDADOS_TRAVA_CONTRATO_CORRIG_OK\n")
