script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)

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

painel <- paste(trecho_entre(
  linhas,
  "^monitora_correcao_painel\\s*<-\\s*function\\s*\\(",
  "^monitora_cache_painel_hash_inputs\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!nzchar(painel)) falhar("Nao foi possivel localizar monitora_correcao_painel().")

helper <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "monitora_painel_pendencias_impeditivas_detalhadas_dados\\s*<-\\s*function",
  "monitora_painel_tabela_impeditivas_ui\\s*<-\\s*function"
), collapse = "\n")
if (!nzchar(helper)) falhar("Helper de pendencias impeditivas detalhadas nao encontrado.")

if (!grepl("Pendências que impedem registros_validados", painel, fixed = TRUE)) {
  falhar("Secao/tabela 'Pendencias que impedem registros_validados' ausente.")
}
if (!grepl("pendencias_impeditivas_detalhadas", painel, fixed = TRUE) ||
    !grepl("output\\$pendencias_impeditivas_detalhadas\\s*<-\\s*DT::renderDT", painel, perl = TRUE)) {
  falhar("Tabela DT de pendencias impeditivas detalhadas ausente.")
}
if (!grepl("monitora_publicacao_ab_auditar_pendencias_impeditivas\\s*\\(", helper, perl = TRUE)) {
  falhar("Detalhe deve usar a auditoria impeditiva AB.")
}
if (!grepl("gravar\\s*=\\s*FALSE", helper, perl = TRUE) ||
    !grepl("marcar_base\\s*=\\s*FALSE", helper, perl = TRUE) ||
    !grepl("data.table::copy\\s*\\(\\s*x\\s*\\)", helper, perl = TRUE)) {
  falhar("Auditoria detalhada no painel deve ser sem gravacao, sem marcar base e com copia de x.")
}

cols <- c(
  "tipo_pendencia", "atributo", "n_linhas", "n_coletas", "coletas_exemplo",
  "ponto_amostral_exemplo", "ponto_metro_exemplo", "acao_sugerida"
)
faltantes <- cols[!vapply(cols, grepl, logical(1), x = helper, fixed = TRUE)]
if (length(faltantes)) {
  falhar("Colunas minimas ausentes no detalhe: ", paste(faltantes, collapse = ", "))
}
if (!grepl("severidade", helper, fixed = TRUE) ||
    !grepl("rotulo_pendencia", helper, fixed = TRUE) ||
    !grepl("corrigivel_no_painel", helper, fixed = TRUE) ||
    !grepl("escopo", helper, fixed = TRUE)) {
  falhar("Detalhe deve incluir severidade, rotulo, corrigivel_no_painel e escopo.")
}
if (!grepl("atributo_101_nao_resolvido", helper, fixed = TRUE) ||
    !grepl("atributo_global", helper, fixed = TRUE) ||
    !grepl("não aplicável", helper, fixed = TRUE) ||
    !grepl("n_coletas = if \\(identical\\(escopo\\[1L\\], \"atributo_global\"\\)\\) NA_integer_", helper, perl = TRUE)) {
  falhar("atributo_101_nao_resolvido deve ser tratado como atributo_global com coletas nao aplicaveis.")
}
if (!grepl("Escalar para desenvolvedor: corrigir contrato/schema/materialização.", helper, fixed = TRUE) ||
    !grepl("encostam_coluna_nao_resolvida", helper, fixed = TRUE)) {
  falhar("Pendencias estruturais devem ser nao corrigiveis e escaladas para desenvolvedor.")
}
if (!grepl("contrato_dropdown_painel_101", helper, fixed = TRUE) ||
    !grepl("atributo_materializavel", helper, fixed = TRUE)) {
  falhar("Corrigibilidade deve considerar contrato final materializavel exposto ao bolsista.")
}
if (!grepl("MONITORA_CORRECAO_COLUNAS_PROTEGIDAS", helper, fixed = TRUE) ||
    !grepl("atributo_tecnico", helper, fixed = TRUE) ||
    !grepl("coluna técnica de registros_corrig não é alvo editável do bolsista", helper, fixed = TRUE)) {
  falhar("Colunas tecnicas de registros_corrig nao podem aparecer como editaveis/corrigiveis para bolsista.")
}
if (!grepl("encostam_token_desconhecido", helper, fixed = TRUE) ||
    !grepl("revisar sanitização pré-painel de Encostam/token desconhecido", helper, fixed = TRUE)) {
  falhar("encostam_token_desconhecido deve ser tratado como erro pre-painel nao corrigivel pelo bolsista.")
}
if (!grepl("monitora_correcao_sanitizar_encostam_desconhecida_para_nativa\\s*<-\\s*function", texto, perl = TRUE) ||
    !grepl("MONITORA_SANITIZACAO_ENCOSTAM_DESCONHECIDA_PRE_PAINEL\\s*<-\\s*monitora_correcao_sanitizar_encostam_desconhecida_para_nativa", texto, perl = TRUE)) {
  falhar("Rotina pre-painel de Encostam desconhecida -> nativa deve permanecer presente.")
}
if (!grepl('preencher_por_linha("COLETA", "coleta")', helper, fixed = TRUE) ||
    !grepl('preencher_por_linha("ponto_amostral", "ponto_amostral")', helper, fixed = TRUE) ||
    !grepl('preencher_por_linha("ponto_metro", "ponto_metro")', helper, fixed = TRUE)) {
  falhar("Detalhe deve tentar resolver COLETA, ponto_amostral e ponto_metro por linha real.")
}

if (!grepl("monitora_painel_tabela_impeditivas_ui\\s*<-\\s*function", painel, perl = TRUE) ||
    !grepl("monitora_painel_resumo_impeditivas_dados\\s*<-\\s*function", painel, perl = TRUE) ||
    !grepl("preview_pendencias_sessao", painel, fixed = TRUE)) {
  falhar("Resumo antigo de impeditivas parece ter sido removido.")
}

cat("PAINEL_PENDENCIAS_IMPEDITIVAS_DETALHADAS_OK\n")
