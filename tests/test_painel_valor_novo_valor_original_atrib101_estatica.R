script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Script principal nao encontrado: ", script_path, call. = FALSE)

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto  <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

# --- 1. valor_novo nunca recebe string "NA" via updateNumericInput ---

exigir(
  !grepl('updateNumericInput.*valor_novo.*value\\s*=\\s*NA[^_]', texto, perl = TRUE),
  "updateNumericInput para valor_novo com value=NA encontrado: remove essa chamada (envia string 'NA' ao cliente)."
)

# --- 2. Todos os update*Input de valor_novo usam value='' ou selected=character(0) ---

exigir(
  grepl('updateTextAreaInput.*valor_novo.*value\\s*=\\s*""', texto, perl = TRUE) ||
    grepl('updateTextInput.*valor_novo.*value\\s*=\\s*""', texto, perl = TRUE),
  "Reset de valor_novo deve usar updateTextInput/TextAreaInput com value=''."
)
exigir(
  grepl('updateSelectizeInput.*valor_novo.*selected\\s*=\\s*character\\(0\\)', texto, perl = TRUE),
  "Reset de valor_novo deve usar updateSelectizeInput com selected=character(0)."
)

# --- 3. ui_valor_novo_controle inicializa com value='' (nao value=NA, nao value='NA') ---

idx_ui_valor_novo <- grep("ui_valor_novo_controle", linhas, fixed = TRUE)
exigir(
  length(idx_ui_valor_novo) > 0L,
  "output$ui_valor_novo_controle nao encontrado no script."
)
bloco_inicio <- min(idx_ui_valor_novo)
bloco_fim    <- min(grep("^\\s*\\}\\s*$", linhas[seq(bloco_inicio + 1L, length(linhas))]) + bloco_inicio, na.rm = TRUE)
bloco_texto  <- paste(linhas[seq(bloco_inicio, min(bloco_fim, bloco_inicio + 120L))], collapse = "\n")
exigir(
  !grepl('value\\s*=\\s*NA[^_]', bloco_texto, perl = TRUE),
  "ui_valor_novo_controle nao deve construir nenhum widget com value=NA."
)
exigir(
  !grepl('value\\s*=\\s*"NA"', bloco_texto, perl = TRUE),
  "ui_valor_novo_controle nao deve construir nenhum widget com value='NA' (string)."
)

# --- 4. monitora_painel_valor_original_sugerido filtra NA-like strings ---

idx_sugerido <- grep("monitora_painel_valor_original_sugerido", linhas, fixed = TRUE)
exigir(
  length(idx_sugerido) > 0L,
  "monitora_painel_valor_original_sugerido nao encontrado."
)
def_linha <- min(idx_sugerido[grepl("<-\\s*shiny::reactive", linhas[idx_sugerido], perl = TRUE)])
bloco_sugerido <- paste(linhas[seq(def_linha, min(def_linha + 30L, length(linhas)))], collapse = "\n")

exigir(
  grepl('"NA"', bloco_sugerido, fixed = TRUE) &&
    grepl('"<NA>"', bloco_sugerido, fixed = TRUE),
  "monitora_painel_valor_original_sugerido deve filtrar strings 'NA' e '<NA>' alem de is.na()."
)
exigir(
  grepl('""', bloco_sugerido, fixed = TRUE),
  "monitora_painel_valor_original_sugerido deve filtrar string vazia '' da lista de valores."
)

# --- 5. monitora_painel_valor_original_sugerido exibe resumo para valores multiplos ---

exigir(
  grepl("Múltiplos valores no escopo", texto, fixed = TRUE),
  "monitora_painel_valor_original_sugerido deve mostrar resumo quando ha multiplos valores no escopo."
)

# --- 6. Label de display 'Atributo 101 nao resolvido' removido das strings usuario-visiveis ---

exigir(
  !grepl('"Atributo 101 não resolvido"', texto, fixed = TRUE),
  "A string de display 'Atributo 101 nao resolvido' ainda existe; substitua por label mais descritivo."
)

# --- 7. Novo label descritivo para atributo_101_nao_resolvido ---

exigir(
  grepl("Atributo contratual não resolvido no registros_corrig", texto, fixed = TRUE),
  "Label 'Atributo contratual nao resolvido no registros_corrig' ausente."
)

# --- 8. acao_sugerida para atributo_101 inclui texto acionavel ---

exigir(
  grepl("Pendência técnica de contrato/schema", texto, fixed = TRUE),
  "acao_sugerida para atributo_101_nao_resolvido deve incluir 'Pendencia tecnica de contrato/schema'."
)
exigir(
  grepl("Não corrigível pela bolsista no painel", texto, fixed = TRUE) ||
    grepl("Nao corrigivel pela bolsista no painel", texto, fixed = TRUE),
  "acao_sugerida para atributo_101_nao_resolvido deve explicitar que nao e corrigivel pela bolsista no painel."
)
exigir(
  grepl("suporte técnico", texto, fixed = TRUE) ||
    grepl("suporte tecnico", texto, fixed = TRUE),
  "acao_sugerida para atributo_101_nao_resolvido deve referenciar o suporte tecnico."
)
exigir(
  grepl("auditoria_painel_controle_atributos.csv", texto, fixed = TRUE),
  "acao_sugerida deve citar auditoria_painel_controle_atributos.csv."
)
exigir(
  grepl("auditoria_mapa_colunas_canonicas_ultima_execucao.csv", texto, fixed = TRUE),
  "acao_sugerida deve citar auditoria_mapa_colunas_canonicas_ultima_execucao.csv."
)
exigir(
  grepl("linha(s)", texto, fixed = TRUE),
  "acao_sugerida deve incluir 'linha(s)' para indicar quantas linhas sao afetadas."
)
exigir(
  !grepl('"Escalar para desenvolvedor: corrigir contrato/schema/materialização."', texto, fixed = TRUE),
  "Mensagem generica 'Escalar para desenvolvedor' nao deve ser a acao_sugerida para atributo_101_nao_resolvido."
)

# --- 9. atributo_101_nao_resolvido disparado por coluna ausente, nao por contagem de linhas ---

exigir(
  grepl("atributo_101_nao_resolvido", texto, fixed = TRUE),
  "Tipo atributo_101_nao_resolvido deve existir no script."
)
exigir(
  grepl("cols_presentes", texto, fixed = TRUE) || grepl("nenhuma coluna encontrada", texto, fixed = TRUE),
  "Trigger de atributo_101_nao_resolvido deve verificar presenca de coluna, nao contagem de linhas."
)
idx_add_101 <- grep('atributo_101_nao_resolvido.*impeditiva|add.*atributo_101_nao_resolvido', linhas, perl = TRUE)
if (length(idx_add_101) > 0L) {
  janela_inicio <- max(1L, min(idx_add_101) - 5L)
  janela_fim    <- min(length(linhas), min(idx_add_101) + 5L)
  janela        <- paste(linhas[seq(janela_inicio, janela_fim)], collapse = "\n")
  exigir(
    !grepl('n_preenchidos\\s*==\\s*101|n_linhas\\s*==\\s*101', janela, perl = TRUE),
    "O trigger de atributo_101_nao_resolvido nao deve usar n_preenchidos==101 nem n_linhas==101 como condicao."
  )
}

# --- 10. Resolver coluna usa xlsform_caminho_registro para Data (data_hora/data) ---

exigir(
  grepl("xlsform_caminho_registro", texto, fixed = TRUE),
  "dicionario_atributos e resolver_coluna devem usar xlsform_caminho_registro para resolver 'data_hora/data' -> 'Data (data_hora)'."
)
exigir(
  grepl('xlsform_caminho_registro == nome', texto, fixed = TRUE),
  "resolver_coluna deve fazer lookup por xlsform_caminho_registro == nome."
)

# --- 11. DATA DO REGISTRO e DATA_MONITORA_PARSEADA permanecem protegidas ---

exigir(
  grepl("DATA DO REGISTRO", texto, fixed = TRUE) &&
    grepl("DATA_MONITORA_PARSEADA", texto, fixed = TRUE) &&
    grepl("MONITORA_CORRECAO_COLUNAS_PROTEGIDAS", texto, fixed = TRUE),
  "DATA DO REGISTRO e DATA_MONITORA_PARSEADA devem permanecer em MONITORA_CORRECAO_COLUNAS_PROTEGIDAS."
)

# --- 12. Aba espacial preservada (regressao) ---

exigir(
  grepl('shiny::tabPanel', texto, fixed = TRUE) &&
    grepl("MONITORA_VALIDAR_ESPACIAL_COLETAS", texto, fixed = TRUE),
  "Aba espacial com shiny::tabPanel e MONITORA_VALIDAR_ESPACIAL_COLETAS deve existir (regressao)."
)

cat("PAINEL_VALOR_NOVO_VALOR_ORIGINAL_ATRIB101_ESTATICA_OK\n")
