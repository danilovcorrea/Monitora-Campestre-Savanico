script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)

exigir_fixo <- function(x, padrao, mensagem) {
  if (!grepl(padrao, x, fixed = TRUE)) falhar(mensagem)
}

exigir_regex <- function(x, padrao, mensagem) {
  if (!grepl(padrao, x, perl = TRUE)) falhar(mensagem)
}

proibir_regex <- function(x, padrao, mensagem) {
  if (grepl(padrao, x, perl = TRUE)) falhar(mensagem)
}

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

# -----------------------------------------------------------------------
# 1. Existe categoria operacional outra_forma_vida (catalogo A/B).
# -----------------------------------------------------------------------
exigir_fixo(
  texto,
  'MONITORA_CORRECAO_CATEGORIAS_FORMA_VIDA_OPERACIONAL <- c("nativa", "exotica", "seca_morta", "outra_forma_vida")',
  "Catalogo operacional MONITORA_CORRECAO_CATEGORIAS_FORMA_VIDA_OPERACIONAL nao encontrado ou nao inclui outra_forma_vida."
)
exigir_regex(
  painel,
  "MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO\\s*<-\\s*c\\s*\\(\\s*MONITORA_TRIAGEM_CATEGORIAS_FORMA\\s*,\\s*\"outra_forma_vida\"\\s*\\)",
  "Categorias de movimento do painel (MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO) nao incluem outra_forma_vida."
)

# -----------------------------------------------------------------------
# 2. Existe resolucao de coluna para forma_vida_outros, sem alterar as
#    funcoes antigas restritas a nativa/exotica/seca_morta (item B).
# -----------------------------------------------------------------------
exigir_fixo(
  texto,
  "monitora_correcao_coluna_lista_forma_vida_operacional <- function(dt, categoria)",
  "monitora_correcao_coluna_lista_forma_vida_operacional() nao encontrada."
)
exigir_fixo(
  texto,
  "monitora_correcao_colunas_limpeza_outras_formas(dt, NULL)",
  "Resolucao de outra_forma_vida deve reaproveitar monitora_correcao_colunas_limpeza_outras_formas (campo_atual_forma_vida_outros)."
)
exigir_fixo(
  texto,
  'if (!identical(cat_val, "outra_forma_vida")) return(monitora_correcao_coluna_forma_vida(dt, cat_val))',
  "Resolucao operacional deve delegar nativa/exotica/seca_morta para monitora_correcao_coluna_forma_vida() sem reescreve-la (item B)."
)
funcoes_antigas <- trecho_entre(linhas, "^monitora_correcao_coluna_forma_vida\\s*<-\\s*function", "^monitora_correcao_colunas_forma_vida_principais\\s*<-\\s*function")
funcoes_antigas_txt <- paste(funcoes_antigas, collapse = "\n")
if (grepl("outra_forma_vida", funcoes_antigas_txt, fixed = TRUE)) {
  falhar("monitora_correcao_coluna_forma_vida()/monitora_correcao_colunas_forma_vida_categoria() nao devem ser alteradas para conhecer outra_forma_vida (usar a funcao operacional nova).")
}

# -----------------------------------------------------------------------
# 3. Choices de forma_vida_outros incluem musgos, hepaticas, antoceros,
#    liquens, fungos (contrato XLSForm 2025).
# -----------------------------------------------------------------------
exigir_fixo(
  texto,
  'c("musgos", "hepaticas", "antoceros", "liquens", "fungos")',
  "Lista canonica de tokens validos de forma_vida_outros (musgos/hepaticas/antoceros/liquens/fungos) nao encontrada."
)
exigir_fixo(
  painel,
  "monitora_painel_montar_choices_formas_outros_mv <- function()",
  "Builder de choices de outra_forma_vida para o movimento assistido nao encontrado."
)
exigir_fixo(
  painel,
  "monitora_registros_validados_forma_outros_tokens_atuais()",
  "Choices de outra_forma_vida devem reaproveitar monitora_registros_validados_forma_outros_tokens_atuais() (fonte unica dos tokens validos)."
)

# -----------------------------------------------------------------------
# 4. Movimento assistido simples: sem selected padrao em
#    mv_origem/mv_destino/mv_forma_origem/mv_forma_destino; mv_forma (unico,
#    antigo) foi removido; mv_forma_origem/mv_forma_destino existem (item 6/C).
# -----------------------------------------------------------------------
exigir_fixo(painel, '"mv_forma_origem"', "Campo mv_forma_origem (forma de vida de origem) nao encontrado no movimento assistido simples.")
exigir_fixo(painel, '"mv_forma_destino"', "Campo mv_forma_destino (forma de vida de destino) nao encontrado no movimento assistido simples.")
proibir_regex(painel, 'selectInput\\("mv_forma"\\s*,', "Campo unico mv_forma (substituido por mv_forma_origem/mv_forma_destino) ainda presente.")

ui_mv_origem <- trecho_entre(linhas, '"mv_origem", "Origem"', NULL)[1]
if (is.na(ui_mv_origem) || grepl('selected\\s*=\\s*"(nativa|exotica|seca_morta)"', ui_mv_origem, perl = TRUE)) {
  falhar("mv_origem nao deve ter categoria pre-selecionada por padrao (item 9/10).")
}
ui_mv_destino <- trecho_entre(linhas, '"mv_destino", "Destino"', NULL)[1]
if (is.na(ui_mv_destino) || grepl('selected\\s*=\\s*"(nativa|exotica|seca_morta)"', ui_mv_destino, perl = TRUE)) {
  falhar("mv_destino nao deve ter categoria pre-selecionada por padrao (item 9/10).")
}
exigir_regex(painel, 'selectizeInput\\("mv_forma_origem".*?selected\\s*=\\s*character\\(0\\)', "mv_forma_origem deve iniciar com selected = character(0) (sem pre-preenchimento).")
exigir_regex(painel, 'selectizeInput\\("mv_forma_destino".*?selected\\s*=\\s*character\\(0\\)', "mv_forma_destino deve iniciar com selected = character(0) (sem pre-preenchimento).")
exigir_fixo(painel, 'selectInput("mv_habito", "Hábito, quando obrigatório", choices = MONITORA_TRIAGEM_HABITO_NAO_APLICA_CHOICES, selected = "")', "mv_habito deve iniciar vazio (sem pre-preenchimento).")

# -----------------------------------------------------------------------
# 5. Movimento em lote: sem selected padrao em
#    mv_lote_origem/mv_lote_destino/mv_lote_formas/mv_lote_forma_destino;
#    opcao "todas" pode existir, mas nao pre-selecionada (item D/9/10).
# -----------------------------------------------------------------------
ui_mv_lote_origem <- trecho_entre(linhas, '"mv_lote_origem", "Origem do lote"', NULL)[1]
if (is.na(ui_mv_lote_origem) || grepl('selected\\s*=\\s*"(nativa|exotica|seca_morta)"', ui_mv_lote_origem, perl = TRUE)) {
  falhar("mv_lote_origem nao deve ter categoria pre-selecionada por padrao.")
}
ui_mv_lote_destino <- trecho_entre(linhas, '"mv_lote_destino", "Destino do lote"', NULL)[1]
if (is.na(ui_mv_lote_destino) || grepl('selected\\s*=\\s*"(nativa|exotica|seca_morta)"', ui_mv_lote_destino, perl = TRUE)) {
  falhar("mv_lote_destino nao deve ter categoria pre-selecionada por padrao.")
}
mv_lote_formas_bloco <- paste(trecho_entre(linhas, '"mv_lote_formas"', "^\\s*\\),?\\s*$"), collapse = "\n")
if (grepl('selected\\s*=\\s*"__todas__"', mv_lote_formas_bloco, perl = TRUE)) {
  falhar("mv_lote_formas nao deve vir com '__todas__' pre-selecionado por padrao (item D).")
}
exigir_fixo(painel, 'choices = character(0),\n          selected = character(0),\n          multiple = TRUE,\n          options = list(placeholder = "Selecione a origem para listar formas/opção \'todas\' presentes no escopo filtrado"', "mv_lote_formas deve iniciar com choices/selected vazios (populado sob demanda).")
mv_lote_forma_destino_bloco <- paste(trecho_entre(linhas, '"mv_lote_forma_destino"', "^\\s*\\),?\\s*$"), collapse = "\n")
if (grepl('selected\\s*=\\s*"[a-z_]+"', mv_lote_forma_destino_bloco, perl = TRUE) && !grepl('selected\\s*=\\s*character\\(0\\)', mv_lote_forma_destino_bloco, perl = TRUE)) {
  falhar("mv_lote_forma_destino nao deve ter forma pre-selecionada por padrao.")
}
exigir_fixo(painel, 'stats::setNames("__todas__", "Todas as formas encontradas na origem")', "Opcao 'Todas as formas encontradas na origem' deve continuar existindo (apenas nao selecionada por padrao).")

# -----------------------------------------------------------------------
# 6. Reatividade aos filtros do painel (item F): dados_movimento_escopo()
#    e reage a escopo_coletas/coletas_lote/UC-EA-ANO-CICLO-CAMPANHA-UA/COLETA.
# -----------------------------------------------------------------------
exigir_fixo(painel, "dados_movimento_escopo <- shiny::reactive({", "Reativo dados_movimento_escopo() nao encontrado (item F).")
exigir_fixo(painel, "monitora_painel_usar_lote_coletas()", "dados_movimento_escopo() deve considerar o escopo (coleta individual x coletas do lote).")
exigir_fixo(painel, "categorias_origem_disponiveis <- shiny::reactive({", "Reativo categorias_origem_disponiveis() nao encontrado (item F).")
exigir_fixo(painel, "formas_origem_disponiveis_mv <- shiny::reactive({", "Reativo de formas de origem disponiveis no escopo nao encontrado (item F).")
exigir_fixo(painel, "formas_destino_contrato_mv <- shiny::reactive({", "Reativo de formas de destino validas pelo contrato nao encontrado (item F).")
for (gatilho in c("input$mv_origem", "input$mv_destino", "input$mv_lote_origem", "input$mv_lote_destino")) {
  exigir_fixo(painel, gatilho, paste0("Reativo do movimento assistido nao le ", gatilho, "."))
}
exigir_fixo(painel, "ignoreInit = TRUE", "Observers de atualizacao de choices do movimento assistido devem usar ignoreInit = TRUE (item F, nao recriar gargalo no bootstrap).")

# -----------------------------------------------------------------------
# 7. Aplicadores (MOVFV/MVLOTE) e reforco semantico aceitam outra_forma_vida.
# -----------------------------------------------------------------------
aplicador_simples <- paste(trecho_entre(linhas, "^monitora_correcao_aplicar_movimento_forma_vida_atomico\\s*<-\\s*function", "^monitora_correcao_aplicar_movimentos_forma_vida_atomicos\\s*<-\\s*function"), collapse = "\n")
if (!nzchar(aplicador_simples)) falhar("monitora_correcao_aplicar_movimento_forma_vida_atomico() nao encontrado.")
exigir_fixo(aplicador_simples, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, origem)", "Aplicador simples (MOVFV) deve resolver a coluna de origem pelo resolver operacional (aceita outra_forma_vida).")
exigir_fixo(aplicador_simples, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, destino)", "Aplicador simples (MOVFV) deve resolver a coluna de destino pelo resolver operacional (aceita outra_forma_vida).")
exigir_fixo(aplicador_simples, "MONITORA_CORRECAO_CATEGORIAS_FORMA_VIDA_OPERACIONAL", "Aplicador simples (MOVFV) deve validar categoria de origem/destino contra o catalogo operacional (nativa/exotica/seca_morta/outra_forma_vida).")

reforco <- paste(trecho_entre(linhas, "^monitora_correcao_reforcar_movimentos_forma_vida\\s*<-\\s*function", "^monitora_correcao_(contar_residuos_outras_formas|limpeza_atomica_outras_formas_de_vida)"), collapse = "\n")
if (!nzchar(reforco)) falhar("monitora_correcao_reforcar_movimentos_forma_vida() nao encontrado.")
exigir_fixo(reforco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, linha_mov$categoria_origem[1])", "Backstop semantico do movimento deve resolver a coluna de origem pelo resolver operacional (aceita outra_forma_vida).")
exigir_fixo(reforco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, linha_mov$categoria_destino[1])", "Backstop semantico do movimento deve resolver a coluna de destino pelo resolver operacional (aceita outra_forma_vida).")

aplicador_lote <- paste(trecho_entre(linhas, "^monitora_correcao_aplicar_movimento_forma_vida_lote_atomico\\s*<-\\s*function.*gravar_relatorio_ambiguidades", "^monitora_correcao_aplicar_movimento_forma_vida_lote_atomico__v262"), collapse = "\n")
if (!nzchar(aplicador_lote)) falhar("monitora_correcao_aplicar_movimento_forma_vida_lote_atomico() (corpo alcancavel, pre-alias __v262) nao encontrado.")
exigir_fixo(aplicador_lote, "MONITORA_CORRECAO_CATEGORIAS_FORMA_VIDA_OPERACIONAL", "Aplicador em lote (MVLOTE) deve validar origem/destino contra o catalogo operacional (nativa/exotica/seca_morta/outra_forma_vida).")
exigir_fixo(aplicador_lote, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, origem)", "Aplicador em lote (MVLOTE) deve resolver a coluna de origem pelo resolver operacional.")
exigir_fixo(aplicador_lote, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, destino)", "Aplicador em lote (MVLOTE) deve resolver a coluna de destino pelo resolver operacional.")

# Observers do painel: origem/destino do movimento simples e em lote usam o
# resolver operacional e o catalogo MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO.
add_mv_bloco <- paste(trecho_entre(linhas, "shiny::observeEvent\\(input\\$add_mv,\\s*\\{", "shiny::observeEvent\\(input\\$excluir_correcoes_pendentes,\\s*\\{|session\\$onSessionEnded"), collapse = "\n")
exigir_fixo(add_mv_bloco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, mv_origem_val)", "Observer input$add_mv deve resolver col_origem pelo resolver operacional.")
exigir_fixo(add_mv_bloco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, mv_destino_val)", "Observer input$add_mv deve resolver col_destino pelo resolver operacional.")
exigir_fixo(add_mv_bloco, "MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO", "Observer input$add_mv deve validar mv_origem/mv_destino contra MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO.")

add_mv_lote_bloco <- paste(trecho_entre(linhas, "shiny::observeEvent\\(input\\$add_mv_lote,\\s*\\{", "shiny::observeEvent\\(input\\$add_mv,\\s*\\{"), collapse = "\n")
exigir_fixo(add_mv_lote_bloco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, origem_val)", "Observer input$add_mv_lote deve resolver col_origem pelo resolver operacional.")
exigir_fixo(add_mv_lote_bloco, "monitora_correcao_coluna_lista_forma_vida_operacional(dt, destino_val)", "Observer input$add_mv_lote deve resolver col_destino pelo resolver operacional.")
exigir_fixo(add_mv_lote_bloco, "MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO", "Observer input$add_mv_lote deve validar origem_val/destino_val contra MONITORA_TRIAGEM_CATEGORIAS_FORMA_MOVIMENTO.")

# -----------------------------------------------------------------------
# 8. Recalculo de tipo_forma_vida considera forma_vida_outros (item G/13).
# -----------------------------------------------------------------------
recalculo <- paste(trecho_entre(linhas, "^monitora_correcao_calcular_tipo_forma_vida_esperado\\s*<-\\s*function", "^monitora_registros_corrig_auditar_encostam_derivado\\s*<-\\s*function"), collapse = "\n")
if (!nzchar(recalculo)) falhar("monitora_correcao_calcular_tipo_forma_vida_esperado() nao encontrado.")
exigir_fixo(recalculo, 'outra_forma_vida = tryCatch(monitora_correcao_coluna_lista_forma_vida_operacional(dt, "outra_forma_vida")', "parent_cols do recalculo de tipo_forma_vida deve incluir outra_forma_vida (forma_vida_outros).")
exigir_fixo(recalculo, 'categorias_controladas <- c("nativa", "exotica", "seca_morta", "outra_forma_vida")', "categorias_controladas do recalculo deve incluir outra_forma_vida, senao o token superior fica preso ao valor anterior em vez de refletir o estado atual de forma_vida_outros.")

# -----------------------------------------------------------------------
# 9. Habito nao e exigido para musgos/hepaticas/antoceros/liquens/fungos.
# -----------------------------------------------------------------------
exigir_fixo(painel, 'MONITORA_TRIAGEM_FORMAS_CONDICIONAIS <- c("bromelioide", "cactacea", "orquidea", "samambaia")', "Lista de formas que exigem habito mudou; nenhum token de forma_vida_outros pode aparecer aqui.")
for (tok_outros in c("musgos", "hepaticas", "antoceros", "liquens", "fungos")) {
  if (grepl(paste0('MONITORA_TRIAGEM_FORMAS_CONDICIONAIS.*"', tok_outros, '"'), painel, perl = TRUE)) {
    falhar(paste0("Token '", tok_outros, "' (forma_vida_outros) nao pode exigir habito."))
  }
}
exigir_fixo(
  texto,
  "monitora_painel_forma_exige_habito",
  "monitora_painel_forma_exige_habito() nao encontrada (decide, por forma, se habito e exigido; nenhum token de forma_vida_outros deve satisfaze-la)."
)

# -----------------------------------------------------------------------
# 10. Bloqueio/auditoria para dependentes nao migraveis quando destino for
#     outra_forma_vida (especie/nome popular e habito da origem sem
#     equivalente seguro no destino) - item 14/15, sem perda silenciosa.
# -----------------------------------------------------------------------
exigir_fixo(
  aplicador_simples,
  "sem equivalente seguro em 'outras plantas terrestres, líquens e/ou fungos'; valor anterior preservado apenas na auditoria",
  "Aplicador simples deve auditar explicitamente dependentes (especie/nome popular ou habito) da origem removidos sem equivalente seguro no destino outra_forma_vida."
)
exigir_fixo(
  aplicador_lote,
  "sem equivalente seguro em 'outras plantas terrestres, líquens e/ou fungos'; valor anterior preservado apenas na auditoria",
  "Aplicador em lote deve auditar explicitamente dependentes (especie/nome popular ou habito) da origem removidos sem equivalente seguro no destino outra_forma_vida."
)
exigir_fixo(texto, 'roles <- c("atributo_dependente_habito", "especie_nome_popular")', "Resolucao de dependentes da origem (usada para limpeza item 14/15) deve continuar cobrindo habito e especie/nome popular.")

cat("MOVIMENTO_FORMAS_VIDA_OUTRAS_PLANTAS_ESTATICA_OK\n")
