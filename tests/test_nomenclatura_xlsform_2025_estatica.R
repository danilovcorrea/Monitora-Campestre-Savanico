script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)

if (grepl("XLSForm\\s*2021", texto, perl = TRUE, ignore.case = TRUE)) {
  falhar("Texto publico ambiguo/proibido encontrado: XLSForm 2021.")
}

if (grepl("auditoria contratual\\s+XLSForm21", texto, perl = TRUE, ignore.case = TRUE)) {
  falhar("Texto publico ambiguo encontrado: auditoria contratual XLSForm21.")
}

if (!grepl("Contrato XLSForm/SISMONITORA 2025", texto, fixed = TRUE) &&
    !grepl("Contrato XLSForm 2025 (21FEV25)", texto, fixed = TRUE)) {
  falhar("Nomenclatura publica do contrato 2025 nao encontrada.")
}

linhas_xlsform21 <- grep("xlsform21|XLSForm21|XLSForm 21", linhas, perl = TRUE, ignore.case = FALSE, value = TRUE)
linhas_xlsform21 <- linhas_xlsform21[!grepl("^\\s*###", linhas_xlsform21)]
linhas_xlsform21 <- linhas_xlsform21[!grepl("monitora_[A-Za-z0-9_]*xlsform21|_[a-z0-9_]*xlsform21|xlsform21_|token_xlsform21|ajustes_xlsform21|meta_xlsform21|problemas_.*xlsform21|caminhos\\$.*xlsform21", linhas_xlsform21, perl = TRUE)]
linhas_xlsform21 <- linhas_xlsform21[!grepl("auditoria_registros_corrig_contrato_xlsform21|SISMONITORA_21FEV25|21FEV25", linhas_xlsform21, fixed = FALSE)]
if (length(linhas_xlsform21)) {
  falhar("Uso publico/remanescente de xlsform21 fora de nome interno legado: ", paste(utils::head(trimws(linhas_xlsform21), 5L), collapse = " | "))
}

cat("NOMENCLATURA_XLSFORM_2025_OK\n")
