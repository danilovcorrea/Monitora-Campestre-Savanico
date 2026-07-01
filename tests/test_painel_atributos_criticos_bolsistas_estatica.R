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

# --- Extrair trechos relevantes ---

editaveis_linhas <- trecho_entre(
  linhas,
  "^monitora_painel_atributos_editaveis_sismonitora_2025\\s*<-\\s*function",
  "^monitora_painel_atributos_bloqueados_sismonitora_2025\\s*<-\\s*function"
)
editaveis <- paste(editaveis_linhas, collapse = "\n")

dropdown_fn <- paste(trecho_entre(
  linhas,
  "^monitora_registros_corrig_contrato_dropdown_painel_101\\s*<-\\s*function",
  "^monitora_registros_corrig_contrato_edicao_painel_101\\s*<-\\s*function"
), collapse = "\n")

choices_mestre_fn <- paste(trecho_entre(
  linhas,
  "monitora_painel_choices_atributos_contrato_mestre\\s*<-\\s*function",
  "^\\s+contrato_dropdown_painel_101\\s*<-\\s*monitora_registros_corrig_contrato_dropdown"
), collapse = "\n")

widget_fn <- paste(trecho_entre(
  linhas,
  "monitora_painel_widget_valor_atributo\\s*<-\\s*function",
  "output\\$ui_contrato_entrada_info\\s*<-\\s*shiny::renderUI"
), collapse = "\n")

if (!nzchar(editaveis))       falhar("monitora_painel_atributos_editaveis_sismonitora_2025 nao encontrada.")
if (!nzchar(dropdown_fn))     falhar("monitora_registros_corrig_contrato_dropdown_painel_101 nao encontrada.")
if (!nzchar(choices_mestre_fn)) falhar("monitora_painel_choices_atributos_contrato_mestre nao encontrada.")
if (!nzchar(widget_fn))       falhar("monitora_painel_widget_valor_atributo nao encontrada.")

# -----------------------------------------------------------------------
# 1. Atributos de formas de vida NATIVAS na lista de 101 editaveis
# -----------------------------------------------------------------------
nativas_esperadas <- c(
  "forma_vida_nativa",
  "forma_vida_nativa_bromelioide_sp",
  "forma_vida_nativa_cactacea_sp",
  "forma_vida_nativa_orquidea_sp",
  "forma_vida_nativa_graminoide",
  "forma_vida_nativa_erva_nao_graminoide",
  "forma_vida_nativa_arbusto_abaixo",
  "forma_vida_nativa_arbusto_acima",
  "forma_vida_nativa_arvore_abaixo",
  "forma_vida_nativa_arvore_acima"
)
ausentes <- nativas_esperadas[!vapply(nativas_esperadas, grepl, logical(1), x = editaveis, fixed = TRUE)]
if (length(ausentes)) {
  falhar(
    "Atributos de nativas ausentes de monitora_painel_atributos_editaveis_sismonitora_2025: ",
    paste(ausentes, collapse = ", "),
    ". Relato de bolsistas: nativas nao apareciam no dropdown."
  )
}

# -----------------------------------------------------------------------
# 2. Material botanico (forma_serrapilheira) na lista de 101 editaveis
# -----------------------------------------------------------------------
if (!grepl("forma_serrapilheira", editaveis, fixed = TRUE)) {
  falhar(
    "forma_serrapilheira (material botanico) deve estar em monitora_painel_atributos_editaveis_sismonitora_2025. ",
    "Relato de bolsistas: material botanico nao aparecia no dropdown."
  )
}

# -----------------------------------------------------------------------
# 3. Data (data_hora/data) e Horario (data_hora/hora) na lista de 101 editaveis
# -----------------------------------------------------------------------
if (!grepl("data_hora/data", editaveis, fixed = TRUE)) {
  falhar("data_hora/data ausente de monitora_painel_atributos_editaveis_sismonitora_2025.")
}
if (!grepl("data_hora/hora", editaveis, fixed = TRUE)) {
  falhar("data_hora/hora ausente de monitora_painel_atributos_editaveis_sismonitora_2025.")
}

# -----------------------------------------------------------------------
# 4. forma_vida_outros (outras plantas terrestres) na lista de 101 editaveis
# -----------------------------------------------------------------------
if (!grepl("forma_vida_outros", editaveis, fixed = TRUE)) {
  falhar(
    "forma_vida_outros (outras plantas terrestres) deve estar na lista de atributos editaveis 101. ",
    "Bolsistas precisam poder adicionar/remover tokens neste campo via dropdown."
  )
}

# -----------------------------------------------------------------------
# 5. Dropdown construido a partir do contrato mestre (nao do template antigo)
# -----------------------------------------------------------------------
if (!grepl("monitora_registros_corrig_contrato_mestre_2025", dropdown_fn, fixed = TRUE)) {
  falhar("monitora_registros_corrig_contrato_dropdown_painel_101 deve usar contrato_mestre_2025 como fonte.")
}
if (!grepl("nrow\\s*\\(\\s*out\\s*\\)\\s*!=\\s*101L", dropdown_fn, perl = TRUE) &&
    !grepl("101L", dropdown_fn, fixed = TRUE)) {
  falhar("monitora_registros_corrig_contrato_dropdown_painel_101 deve garantir exatamente 101 atributos (guard nrow==101L).")
}

# -----------------------------------------------------------------------
# 6. Aliases de busca para material botanico e outras plantas terrestres
#    no builder de choices do painel (melhora discoverability no selectize)
# -----------------------------------------------------------------------
if (!grepl("material botanico", choices_mestre_fn, fixed = TRUE)) {
  falhar("choices_atributos_contrato_mestre deve incluir alias 'material botanico' para facilitar busca de forma_serrapilheira.")
}
if (!grepl("outras plantas terrestres", choices_mestre_fn, fixed = TRUE)) {
  falhar("choices_atributos_contrato_mestre deve incluir alias 'outras plantas terrestres' para facilitar busca de forma_vida_outros.")
}
if (!grepl("musgos", choices_mestre_fn, fixed = TRUE)) {
  falhar("choices_atributos_contrato_mestre deve incluir alias 'musgos' (relevante para outras plantas terrestres).")
}

# -----------------------------------------------------------------------
# 7. Widget de data usa textInput sem pre-preenchimento (CORRIGIDO em Bloco 10b)
#    dateInput com value=NULL pre-preencheria com Sys.Date(); substituido por
#    textInput("valor_novo", value = "", placeholder = "YYYY-MM-DD").
# -----------------------------------------------------------------------
if (grepl("dateInput", widget_fn, fixed = TRUE)) {
  falhar(
    "dateInput ainda presente em ui_valor_novo_controle. ",
    "Deve ter sido substituido por textInput(value='', placeholder='YYYY-MM-DD') no Bloco 10b."
  )
}
if (!grepl('"YYYY-MM-DD"', widget_fn, fixed = TRUE)) {
  falhar("textInput para data deve usar placeholder = \"YYYY-MM-DD\" (aspas literais).")
}

# -----------------------------------------------------------------------
# 8. Movimento assistido individual e em lote: destino agora inclui
#    outra_forma_vida (outras plantas terrestres, liquens e/ou fungos).
#    ISSUE HISTORICO (RESOLVIDO): outra_forma_vida nao era destino no
#    movimento assistido; bolsistas precisavam usar o dropdown de correcao
#    simples. Corrigido: MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO
#    estende origem/destino do movimento assistido (simples e lote) para
#    incluir outra_forma_vida. Ver tests/test_movimento_formas_vida_outras_plantas_estatica.R
#    para a cobertura completa da correcao.
# -----------------------------------------------------------------------
painel_texto <- paste(linhas, collapse = "\n")
if (!grepl("MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO\\s*<-\\s*c\\s*\\(\\s*MONITORA_TRIAGEM_CATEGORIAS_FORMA\\s*,\\s*\"outra_forma_vida\"\\s*\\)", painel_texto, perl = TRUE)) {
  falhar("MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO (origem/destino do movimento assistido) nao encontrada ou nao inclui outra_forma_vida.")
}
if (!grepl('"mv_forma_origem"', painel_texto, fixed = TRUE) || !grepl('"mv_forma_destino"', painel_texto, fixed = TRUE)) {
  falhar("Movimento assistido individual deve ter campos separados mv_forma_origem/mv_forma_destino.")
}

cat("PAINEL_ATRIBUTOS_CRITICOS_BOLSISTAS_OK\n")
