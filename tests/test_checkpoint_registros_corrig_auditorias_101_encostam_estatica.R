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

# Extrair trechos relevantes
helper <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_ab_auditar_pendencias_impeditivas_complementares_101_encostam\\s*<-\\s*function",
  "^monitora_publicacao_ab_auditar_pendencias_impeditivas\\s*<-\\s*function"
), collapse = "\n")

ab <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_ab_auditar_pendencias_impeditivas\\s*<-\\s*function",
  "^monitora_publicacao_ab_logar_resumo_pendencias\\s*<-\\s*function"
), collapse = "\n")

aa <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_aa_preparar_validar_registros_corrig\\s*<-\\s*function",
  "^monitora_registros_validados_materializado_sem_bloqueio\\s*<-\\s*function"
), collapse = "\n")

if (!nzchar(helper)) falhar("Helper complementar nao encontrado.")
if (!nzchar(ab))     falhar("monitora_publicacao_ab_auditar_pendencias_impeditivas nao encontrado.")
if (!nzchar(aa))     falhar("monitora_publicacao_aa_preparar_validar_registros_corrig nao encontrado.")

# 1. Helper existe com nome correto
if (!grepl("monitora_publicacao_ab_auditar_pendencias_impeditivas_complementares_101_encostam\\s*<-\\s*function", helper, perl = TRUE)) {
  falhar("Helper complementar nao esta definido como funcao.")
}

# 2. AB chama o helper
if (!grepl("monitora_publicacao_ab_auditar_pendencias_impeditivas_complementares_101_encostam\\s*\\(", ab, perl = TRUE)) {
  falhar("AB nao chama o helper complementar 101/Encostam.")
}

# 3. Helper chama auditoria 101 com exists() e data.table::copy
if (!grepl("monitora_registros_corrig_auditar_atributos_101", helper, fixed = TRUE)) {
  falhar("Helper nao chama monitora_registros_corrig_auditar_atributos_101.")
}
if (!grepl("exists\\s*\\(\\s*[\"']monitora_registros_corrig_auditar_atributos_101[\"']", helper, perl = TRUE)) {
  falhar("Helper deve guardar existencia de monitora_registros_corrig_auditar_atributos_101 com exists().")
}
if (!grepl("data.table::copy", helper, fixed = TRUE)) {
  falhar("Helper deve usar data.table::copy para nao mutar dt.")
}

# 4. Helper chama auditoria Encostam com exists() e data.table::copy
if (!grepl("monitora_registros_corrig_auditar_encostam_derivado", helper, fixed = TRUE)) {
  falhar("Helper nao chama monitora_registros_corrig_auditar_encostam_derivado.")
}
if (!grepl("exists\\s*\\(\\s*[\"']monitora_registros_corrig_auditar_encostam_derivado[\"']", helper, perl = TRUE)) {
  falhar("Helper deve guardar existencia de monitora_registros_corrig_auditar_encostam_derivado com exists().")
}

# 4b. Helper preserva corrigivel_no_painel e acao_sugerida no data.table normalizado
if (!grepl("corrigivel_no_painel", helper, fixed = TRUE)) {
  falhar("Helper deve incluir corrigivel_no_painel no data.table normalizado.")
}
if (!grepl("acao_sugerida", helper, fixed = TRUE)) {
  falhar("Helper deve incluir acao_sugerida no data.table normalizado.")
}

# 5. Severidade impeditiva e filtrada no helper
if (!grepl("severidade\\s*==\\s*[\"']impeditiva[\"']", helper, perl = TRUE)) {
  falhar("Helper deve filtrar apenas severidade impeditiva.")
}

# 6. MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS e setado em AB
if (!grepl("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS", ab, fixed = TRUE)) {
  falhar("AB deve setar MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS.")
}
if (!grepl('assign\\s*\\(\\s*"MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS"', ab, perl = TRUE)) {
  falhar("AB deve usar assign() para MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS.")
}

# 7. MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 = FALSE quando ha pendencias em AA
if (!grepl('assign\\s*\\(\\s*"MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21"\\s*,\\s*FALSE', aa, perl = TRUE)) {
  falhar("AA deve setar MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 = FALSE quando ha pendencias.")
}

# 8. registros_corrig.csv continua permitido como checkpoint (AA retorna invisivel com pendencias)
if (!grepl("return\\s*\\(\\s*invisible\\s*\\(", aa, perl = TRUE)) {
  falhar("AA deve retornar invisivel (materializar checkpoint) mesmo com pendencias impeditivas.")
}
if (!grepl("checkpoint_com_pendencias_impeditivas", aa, fixed = TRUE)) {
  falhar("AA deve registrar contexto de checkpoint com pendencias impeditivas.")
}

# 9. registros_validados bloqueado quando ha pendencias impeditivas
if (!grepl("MONITORA_REGISTROS_VALIDADOS_GERADO", aa, fixed = TRUE)) {
  falhar("AA deve controlar MONITORA_REGISTROS_VALIDADOS_GERADO.")
}
if (!grepl('assign\\s*\\(\\s*"MONITORA_REGISTROS_VALIDADOS_GERADO"\\s*,\\s*FALSE', aa, perl = TRUE)) {
  falhar("AA deve setar MONITORA_REGISTROS_VALIDADOS_GERADO = FALSE quando ha pendencias.")
}

# 10. Sem alteracao em VERSION, README, CHANGELOG
arquivos_proibidos <- c("VERSION", "README.md", "CHANGELOG.md", "CHANGELOG", "README")
for (arq in arquivos_proibidos) {
  if (file.exists(arq)) {
    info <- file.info(arq)
    # Verificacao leve: arquivo existe mas nao foi modificado apos o script principal
    mtime_script <- file.info(script_path)$mtime
    mtime_arq    <- info$mtime
    if (!is.na(mtime_arq) && !is.na(mtime_script) && mtime_arq > mtime_script) {
      falhar("Arquivo proibido modificado neste bloco: ", arq)
    }
  }
}

cat("CHECKPOINT_REGISTROS_CORRIG_AUDITORIAS_101_ENCOSTAM_OK\n")
