args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library(data.table))
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R", mustWork = TRUE)
parsed <- parse(file = script)
body <- parsed[[1L]][[2L]]
env <- new.env(parent = globalenv())

carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    carregar <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    carregar <- carregar || nome %in% c("MONITORA_COL_ROW_ID", "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS")
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_SCRIPT_VERSAO <- "2.9.3-dev-test"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.3-dev-test"
env$MONITORA_OUTPUT_DIR <- tempdir()
env$MONITORA_LOG_DIR <- tempdir()
env$MONITORA_CORRECOES_DIR <- tempdir()
env$MONITORA_EXEC_ID <- "v293_test"
env$MONITORA_DEPENDENCIAS_CORRECOES <- data.table()
for (nome_cache in c(
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nome_cache, new.env(parent = emptyenv()), envir = env)

contrato <- env$monitora_contrato_unico_embutido()
coletores_contrato <- contrato$atributos[name_curto %in% c("nome", "cpf")]
stopifnot(
  nrow(coletores_contrato) == 2L,
  all(coletores_contrato$repeat_pai == "coletor"),
  all(coletores_contrato$cardinalidade_operacional == "membro_repeat_coleta"),
  identical(coletores_contrato[name_curto == "nome", required], "yes"),
  is.na(coletores_contrato[name_curto == "cpf", required]) || !nzchar(coletores_contrato[name_curto == "cpf", required])
)
indices <- env$monitora_contrato_unico_indices(contrato, validar = TRUE)
stopifnot(
  "por_repeat_pai" %in% names(indices$indices),
  nrow(indices$indices$por_repeat_pai[repeat_pai == "coletor" & name_curto %in% c("nome", "cpf")]) == 2L
)

choices_impactos <- contrato$choices[
  grepl("21FEV25", arquivo_xlsform, ignore.case = TRUE) &
    list_name %in% c("impacto_manejo_uso", "tipos_impacto_manejo_uso"),
  .(list_name, name, label)
]
stopifnot(
  identical(choices_impactos[list_name == "impacto_manejo_uso", name], c("sim", "não")),
  identical(choices_impactos[list_name == "impacto_manejo_uso", label], c("Sim", "Não")),
  identical(choices_impactos[list_name == "tipos_impacto_manejo_uso", name], c(
    "incendio", "aceiro", "queima_prescrita", "queima_controlada", "extrativismo",
    "herbivoria", "pisoteio", "restauracao", "uso_publico", "outros"
  ))
)

dados <- data.table(
  COLETA = rep("C1", 101L), UC = "UC teste", EA = "EA1", UA = "UA1", ANO = "2026",
  MONITORA_ROW_ID = paste0("r", seq_len(101L)),
  `ponto_amostral (amostragem/registro)` = as.character(0:100),
  `coletor/nome` = c("Pessoa A", "Pessoa B", "Pessoa C", rep("", 98L)),
  `coletor/cpf` = c("", "22222222222", "", rep("", 98L))
)
dict <- env$monitora_correcao_dicionario_atributos(dados, NULL)
stopifnot(all(dict[atributo_coluna_registros_corrig %in% c("coletor/nome", "coletor/cpf"), painel_editavel] == FALSE))

criar_op_repeat <- function(id, ordem, linha, atributo, acao, antes = NA_character_, depois = NA_character_) {
  op <- env$monitora_correcao_criar_operacao(
    id, "Teste", "edicao_repeat_coletor", ordem, "uuid_registro", coleta = "C1",
    linha_indice = linha, atributo = atributo, acao = acao, valor_original = antes,
    valor_novo = depois, n_esperado = 1L, n_alvo = 1L, motivo = "Teste repeat"
  )
  op[, monitora_row_id := paste0("r", linha)]
  op
}
ops_edicao <- rbindlist(list(
  criar_op_repeat("rep_editar", 1L, 2L, "coletor/nome", "update", "Pessoa B", "Pessoa B revisada"),
  criar_op_repeat("rep_editar", 2L, 2L, "coletor/cpf", "clear", "22222222222")
), fill = TRUE)
gate_repeat_ok <- env$monitora_correcao_validar_contrato_edicao(
  dados, "coletor/nome", "update", "Pessoa B revisada",
  corr_linha = ops_edicao[1L], dicionario = dict
)
op_repeat_insegura <- copy(ops_edicao[1L])
op_repeat_insegura[, `:=`(escopo_aplicacao = "coleta", n_linhas_esperado = 101L, n_linhas_alvo = 101L)]
gate_repeat_insegura <- env$monitora_correcao_validar_contrato_edicao(
  dados, "coletor/nome", "update", "Pessoa B revisada",
  corr_linha = op_repeat_insegura, dicionario = dict
)
stopifnot(
  isTRUE(gate_repeat_ok$ok),
  !isTRUE(gate_repeat_insegura$ok),
  identical(gate_repeat_insegura$status, "bloqueada_repeat_coletor_inseguro")
)
resultado_edicao <- env$monitora_correcao_aplicar_plano_atomico_sessao(dados, ops_edicao, dicionario = dict)
stopifnot(
  !isTRUE(resultado_edicao$falha),
  identical(resultado_edicao$dt[["coletor/nome"]][2L], "Pessoa B revisada"),
  is.na(resultado_edicao$dt[["coletor/cpf"]][2L]),
  identical(resultado_edicao$dt[["coletor/nome"]][-2L], dados[["coletor/nome"]][-2L])
)
repeat_edicao <- env$monitora_coletores_repeat_extrair_grupo(resultado_edicao$dt, seq_len(101L))
stopifnot(
  identical(repeat_edicao$nomes, c("Pessoa A", "Pessoa B revisada", "Pessoa C")),
  identical(repeat_edicao$cpfs, c("", "", "")),
  isTRUE(env$monitora_publicacao_ab_cpf_avaliar("")$valido)
)

ops_exclusao <- rbindlist(list(
  criar_op_repeat("rep_excluir", 1L, 2L, "coletor/nome", "clear", "Pessoa B revisada"),
  criar_op_repeat("rep_excluir", 2L, 2L, "coletor/cpf", "clear")
), fill = TRUE)
resultado_exclusao <- env$monitora_correcao_aplicar_plano_atomico_sessao(resultado_edicao$dt, ops_exclusao, dicionario = dict)
stopifnot(!isTRUE(resultado_exclusao$falha))
repeat_exclusao <- env$monitora_coletores_repeat_extrair_grupo(resultado_exclusao$dt, seq_len(101L))
stopifnot(identical(repeat_exclusao$nomes, c("Pessoa A", "Pessoa C")), identical(repeat_exclusao$cpfs, c("", "")))

tokens <- env$monitora_correcao_aplicar_operacao(rep("incendio aceiro", 3L), "append_token", "restauracao uso_publico")
stopifnot(all(vapply(tokens, function(x) all(c("incendio", "aceiro", "restauracao", "uso_publico") %in% env$monitora_correcao_tokenizar(x)), logical(1L))))
tokens <- env$monitora_correcao_aplicar_operacao(tokens, "remove_token", "incendio restauracao")
stopifnot(all(vapply(tokens, function(x) identical(sort(env$monitora_correcao_tokenizar(x)), sort(c("aceiro", "uso_publico"))), logical(1L))))

rbg_arquivo <- Sys.getenv("MONITORA_TESTE_RBG_DADOS", unset = "")
if (file.exists(rbg_arquivo)) {
  rbg <- fread(rbg_arquivo, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
  rbg_dict <- env$monitora_correcao_dicionario_atributos(rbg, NULL)
  stopifnot(all(rbg_dict[atributo_coluna_registros_corrig %in% c("coletor/nome", "coletor/cpf"), painel_editavel] == FALSE))
  impacto <- rbg_dict[grepl("^Ocorreram impactos", atributo_coluna_registros_corrig), atributo_coluna_registros_corrig][1L]
  tipos <- rbg_dict[grepl("^Qual\\(is\\)\\?", atributo_coluna_registros_corrig), atributo_coluna_registros_corrig][1L]
  stopifnot(
    rbg_dict[atributo_coluna_registros_corrig == impacto, tipo_base_edicao] == "select_one",
    rbg_dict[atributo_coluna_registros_corrig == tipos, tipo_base_edicao] == "select_multiple",
    rbg_dict[atributo_coluna_registros_corrig == tipos, acoes_permitidas] == "append_token;remove_token;replace_token"
  )
  coleta_col <- env$monitora_correcao_colunas_chave(rbg)$coleta
  resumo_repeat <- rbg[, {
    membros <- env$monitora_coletores_repeat_membros(.SD, seq_len(.N))
    .(n_membros = nrow(membros), n_cpfs = sum(nzchar(membros$cpf)))
  }, by = coleta_col]
  stopifnot(nrow(resumo_repeat) > 0L, all(resumo_repeat$n_membros >= 1L), all(resumo_repeat$n_cpfs <= resumo_repeat$n_membros))
}

cat("V293_REPEAT_IMPACTOS_TESTS_OK\n")
