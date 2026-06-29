script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

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

normalizar <- function(x) gsub("[[:space:]]+", " ", x)

painel <- paste(trecho_entre(
  linhas,
  "^monitora_correcao_painel\\s*<-\\s*function\\s*\\(",
  "^monitora_cache_painel_hash_inputs\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!nzchar(painel)) falhar("Nao foi possivel localizar monitora_correcao_painel().")

preparar_corrig <- paste(trecho_entre(
  linhas,
  "^monitora_publicacao_aa_preparar_validar_registros_corrig\\s*<-\\s*function\\s*\\(",
  "^monitora_registros_validados_materializado_sem_bloqueio\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!nzchar(preparar_corrig)) falhar("Nao foi possivel localizar monitora_publicacao_aa_preparar_validar_registros_corrig().")

registros_validados <- paste(trecho_entre(
  linhas,
  "^monitora_registros_validados_exportar\\s*<-\\s*function\\s*\\(",
  "^}\\s*$"
), collapse = "\n")
if (!nzchar(registros_validados)) falhar("Nao foi possivel localizar monitora_registros_validados_exportar().")

# 1. Helpers existem no painel
helpers <- c(
  "monitora_painel_tipos_encostam_derivado",
  "monitora_painel_auditoria_encostam_derivado_dt",
  "monitora_painel_coletas_encostam_derivado"
)
for (h in helpers) {
  if (!grepl(paste0(h, "\\s*<-\\s*function\\s*\\("), painel, perl = TRUE)) {
    falhar("Helper nao encontrado no painel: ", h)
  }
}

# 2. Tipos esperados aparecem no catalogo
tipos_enc <- c(
  "encostam_desatualizado",
  "encostam_token_sem_inferior",
  "encostam_inferior_sem_token",
  "encostam_solo_nu_conflitante",
  "encostam_token_desconhecido",
  "encostam_desconhecida_superior_only",
  "encostam_outra_forma_vida",
  "encostam_coluna_nao_resolvida"
)
faltantes <- tipos_enc[!vapply(tipos_enc, grepl, logical(1), x = painel, fixed = TRUE)]
if (length(faltantes)) {
  falhar("Tipos encostam ausentes do painel: ", paste(faltantes, collapse = ", "))
}

# encostam_vazio_apos_recalculo nao deve estar no catalogo encostam do painel
if (grepl("encostam_vazio_apos_recalculo", painel, fixed = TRUE)) {
  falhar("encostam_vazio_apos_recalculo nao deve constar no catalogo encostam do painel.")
}

# 3. Severidade impeditiva e coluna corrigivel_no_painel no catalogo
trecho_catalogo <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "monitora_painel_tipos_encostam_derivado\\s*<-\\s*function\\s*\\(",
  "monitora_painel_auditoria_encostam_derivado_dt\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!grepl("severidade\\s*=\\s*[\"']impeditiva[\"']", trecho_catalogo, perl = TRUE)) {
  falhar("Tipos encostam devem ter severidade impeditiva no catalogo do painel.")
}
if (!grepl("corrigivel_no_painel", trecho_catalogo, fixed = TRUE)) {
  falhar("Catalogo encostam deve conter coluna corrigivel_no_painel.")
}
# encostam_coluna_nao_resolvida deve ser FALSE (ultimo elemento, valor FALSE apos TRUE repetidos)
if (!grepl("corrigivel_no_painel\\s*=\\s*c\\s*\\(.*FALSE\\s*\\)", trecho_catalogo, perl = TRUE)) {
  falhar("corrigivel_no_painel deve conter FALSE para encostam_coluna_nao_resolvida.")
}
# todos os corrigiveis devem ser TRUE (deve haver pelo menos um TRUE no vetor)
if (!grepl("corrigivel_no_painel\\s*=\\s*c\\s*\\(\\s*TRUE", trecho_catalogo, perl = TRUE)) {
  falhar("corrigivel_no_painel deve ter TRUE para os tipos corrigiveis.")
}

# 4. Auditoria encostam chamada com dados filtrados/preview (nao com dt global)
if (grepl("monitora_painel_auditoria_encostam_derivado_dt\\s*\\(\\s*dt\\s*\\)", painel, perl = TRUE) ||
    grepl("monitora_painel_coletas_encostam_derivado\\s*\\(\\s*dt\\s*\\)", painel, perl = TRUE) ||
    grepl("monitora_registros_corrig_auditar_encostam_derivado\\s*\\(\\s*dt\\s*\\)", painel, perl = TRUE)) {
  falhar("Auditoria encostam nao deve chamar dt global diretamente no painel.")
}

# 5. Integrado em coletas_triagem_dados (x dinamico)
if (!grepl("monitora_painel_coletas_encostam_derivado\\s*\\(\\s*x\\s*\\)", painel, perl = TRUE)) {
  falhar("monitora_painel_coletas_encostam_derivado deve ser chamado com x em coletas_triagem_dados.")
}

# 6. Integrado em resumo_impeditivas_dados
trecho_resumo_dados <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "monitora_painel_resumo_impeditivas_dados\\s*<-\\s*function\\s*\\(",
  "monitora_painel_debug_preview_triout\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!grepl("monitora_painel_auditoria_encostam_derivado_dt", trecho_resumo_dados, fixed = TRUE)) {
  falhar("monitora_painel_resumo_impeditivas_dados deve chamar monitora_painel_auditoria_encostam_derivado_dt.")
}
if (!grepl("monitora_painel_tipos_encostam_derivado", trecho_resumo_dados, fixed = TRUE)) {
  falhar("monitora_painel_resumo_impeditivas_dados deve referenciar monitora_painel_tipos_encostam_derivado.")
}
# Os tipos aparecem no catalogo (painel inteiro), nao como literais em resumo_impeditivas_dados
faltantes_painel <- tipos_enc[!vapply(tipos_enc, grepl, logical(1), x = painel, fixed = TRUE)]
if (length(faltantes_painel)) {
  falhar("Tipos encostam ausentes do catalogo no painel: ", paste(faltantes_painel, collapse = ", "))
}

# 7. Integrado em resumo_impeditivas_pre
trecho_resumo_pre <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "monitora_painel_resumo_impeditivas_pre\\s*<-\\s*function\\s*\\(",
  "monitora_painel_tem_impeditivas_pre\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!grepl("monitora_painel_tipos_encostam_derivado", trecho_resumo_pre, fixed = TRUE)) {
  falhar("monitora_painel_resumo_impeditivas_pre deve referenciar monitora_painel_tipos_encostam_derivado.")
}

# 8. Auditoria encostam ausente de materialização, selo/checkpoint, registros_validados, validação espacial
contextos_proibidos <- list(
  preparar_validar_registros_corrig = preparar_corrig,
  registros_validados_exportar = registros_validados
)
for (ctx_nome in names(contextos_proibidos)) {
  ctx <- contextos_proibidos[[ctx_nome]]
  if (grepl("monitora_registros_corrig_auditar_encostam_derivado\\s*\\(", ctx, perl = TRUE) ||
      grepl("monitora_painel_auditoria_encostam_derivado_dt\\s*\\(", ctx, perl = TRUE)) {
    falhar("Auditoria encostam nao deve aparecer em: ", ctx_nome)
  }
}

# Verificar ausencia em bloco de materializacao (checkpoint/selo)
trecho_mat <- paste(trecho_entre(
  linhas,
  "monitora_perf_registrar_checkpoint",
  NULL
), collapse = "\n")
# Auditoria encostam nao deve ser chamada diretamente no escopo de checkpoint/selo fora do painel
# (verificacao leve: funcao de auditoria nao deve aparecer apos o bloco de encerramento do painel)
idx_fim_painel <- grep("^monitora_cache_painel_hash_inputs\\s*<-\\s*function\\s*\\(", linhas, perl = TRUE)
if (length(idx_fim_painel)) {
  pos_fim <- idx_fim_painel[1L]
  linhas_pos_painel <- linhas[seq.int(pos_fim, length(linhas))]
  if (grepl("monitora_registros_corrig_auditar_encostam_derivado\\s*\\(", paste(linhas_pos_painel, collapse = "\n"), perl = TRUE)) {
    falhar("Auditoria encostam nao deve ser chamada fora do contexto do painel (pos-painel).")
  }
}

cat("PAINEL_ENCOSTAM_DERIVADO_ESTATICA_OK\n")
