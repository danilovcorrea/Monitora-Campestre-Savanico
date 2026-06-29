script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

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

if (!grepl("monitora_registros_corrig_auditar_atributos_101\\s*\\(", painel, perl = TRUE)) {
  falhar("Auditoria 101 deve ser chamada dentro do contexto do painel.")
}
if (!grepl("monitora_painel_resumo_impeditivas_dados\\s*<-\\s*function\\s*\\(", painel, perl = TRUE) ||
    !grepl("monitora_painel_auditoria_101_dt", painel, fixed = TRUE) ||
    !grepl("monitora_painel_resumo_impeditivas_preview", painel, fixed = TRUE)) {
  falhar("Auditoria 101 deve estar integrada ao resumo/preview diagnostico do painel.")
}

tipos_101 <- c(
  "atributo_101_nao_resolvido",
  "atributo_101_alias_conflitante",
  "cadastro_uc_ausente_ou_invalida",
  "cadastro_ea_ausente_ou_invalida",
  "cadastro_ua_ausente_ou_invalida",
  "cadastro_ciclo_ausente_ou_invalido",
  "cadastro_campanha_ausente_ou_invalida",
  "atributo_101_formato_invalido",
  "atributo_101_valor_fora_dominio"
)
faltantes <- tipos_101[!vapply(tipos_101, grepl, logical(1), x = painel, fixed = TRUE)]
if (length(faltantes)) {
  falhar("Tipos de pendencia 101 ausentes do catalogo/resumo do painel: ", paste(faltantes, collapse = ", "))
}
if (grepl("monitora_painel_coletas_auditoria_101\\s*\\(\\s*dt\\s*\\)", painel, perl = TRUE)) {
  falhar("Auditoria 101 nao deve rodar com dt global na abertura/montagem inicial do painel.")
}
montagem_inicial <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "coletas_triagem_por_tipo\\s*<-\\s*monitora_painel_coletas_diagnosticas_preferir_relatorios\\s*\\(",
  "monitora_correcao_console_msg\\s*\\("
), collapse = "\n")
if (grepl("monitora_painel_coletas_auditoria_101\\s*\\(", montagem_inicial, perl = TRUE) ||
    grepl("monitora_registros_corrig_auditar_atributos_101\\s*\\(", montagem_inicial, perl = TRUE)) {
  falhar("Montagem inicial de coletas_triagem_por_tipo nao deve chamar auditoria 101.")
}
if (!grepl("severidade\\s*=\\s*[\"']impeditiva[\"']", painel, perl = TRUE)) {
  falhar("Pendencias 101 devem ser catalogadas como severidade impeditiva no painel.")
}
if (!grepl("corrigivel_no_painel", painel, fixed = TRUE) ||
    !grepl("n_corrigiveis_painel", painel, fixed = TRUE) ||
    !grepl("n_nao_corrigiveis_painel", painel, fixed = TRUE)) {
  falhar("Painel deve distinguir pendencias 101 corrigiveis e nao corrigiveis no painel.")
}

if (grepl("monitora_registros_corrig_auditar_atributos_101\\s*\\(", preparar_corrig, perl = TRUE)) {
  falhar("Auditoria 101 nao deve ser chamada em monitora_publicacao_aa_preparar_validar_registros_corrig() neste bloco.")
}
if (grepl("monitora_registros_corrig_auditar_atributos_101\\s*\\(", registros_validados, perl = TRUE)) {
  falhar("Auditoria 101 nao deve ser chamada em registros_validados neste bloco.")
}

painel_norm <- normalizar(painel)
if (!grepl("selected\\s*=\\s*character\\s*\\(\\s*0\\s*\\)", painel_norm, perl = TRUE) ||
    !grepl("dados_filtrados\\s*<-\\s*shiny::reactive", painel_norm, perl = TRUE) ||
    !grepl("return\\s*\\(\\s*x\\s*\\[\\s*0\\s*\\]\\s*\\)", painel_norm, perl = TRUE)) {
  falhar("Guarda estatica basica do painel deixou de encontrar selecao vazia/dados_filtrados com x[0].")
}

cat("PAINEL_AUDITORIA_101_ESTATICA_OK\n")
