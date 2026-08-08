args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({
  library(data.table)
  library(shiny)
  library(DT)
})
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R", mustWork = TRUE)
rbg_arquivo <- Sys.getenv("MONITORA_TESTE_RBG_DADOS", unset = "")
stopifnot(file.exists(rbg_arquivo))

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
    carregar <- carregar || grepl("^MONITORA_COL_", nome) || nome == "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS"
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

dir_teste <- tempfile("v293_panel_server_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_LOG_DIR <- file.path(dir_teste, "log")
env$MONITORA_CORRECOES_DIR <- file.path(dir_teste, "output", "correcoes")
env$MONITORA_EXEC_ID <- "v293_panel_server"
env$MONITORA_LOG_EXECUCAO <- data.table(
  etapa = character(), severidade = character(), arquivo = character(),
  detalhe = character(), acao = character()
)
env$MONITORA_VALIDAR_ESPACIAL_COLETAS <- FALSE
env$MONITORA_ARQUIVO_CORRECOES_ESPACIAIS <- file.path(dir_teste, "correcoes_espaciais.csv")
env$MONITORA_ARQUIVO_CORRECOES_CAMPOS <- file.path(dir_teste, "correcoes_campos.csv")
env$MONITORA_DEPENDENCIAS_CORRECOES <- data.table()
env$MONITORA_SCRIPT_VERSAO <- "2.9.3-dev-test"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.3-dev-test"
env$MONITORA_PERF_ENABLED <- FALSE
for (nome_cache in c(
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nome_cache, new.env(parent = emptyenv()), envir = env)

rbg <- fread(rbg_arquivo, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
impacto_col <- names(rbg)[grepl("^Ocorreram impactos", names(rbg))][1L]
tipos_col <- names(rbg)[grepl("^Qual\\(is\\)\\?", names(rbg))][1L]
stopifnot(!is.na(impacto_col), !is.na(tipos_col))
coletas_sim <- unique(as.character(rbg[["COLETA"]][env$monitora_correcao_normalizar_nome_coluna(rbg[[impacto_col]]) == "sim"]))
coletas_sim <- coletas_sim[!is.na(coletas_sim) & nzchar(coletas_sim)]
stopifnot(length(coletas_sim) > 0L)
coleta_teste <- coletas_sim[1L]
coletas_nao <- rbg[, .(
  n_linhas = .N,
  pai_nao = all(env$monitora_correcao_normalizar_nome_coluna(get(impacto_col)) == "nao"),
  tipos_vazios = all(env$monitora_correcao_vazio_vec(get(tipos_col)))
), by = COLETA][n_linhas == 101L & pai_nao & tipos_vazios, as.character(COLETA)]
stopifnot(length(coletas_nao) > 0L)
coleta_nao_teste <- coletas_nao[1L]
dados <- rbg[COLETA %in% c(coleta_teste, coleta_nao_teste)]
stopifnot(nrow(dados[COLETA == coleta_teste]) == 101L, nrow(dados[COLETA == coleta_nao_teste]) == 101L)
if (!(env$MONITORA_COL_ROW_ID %in% names(dados))) dados[, (env$MONITORA_COL_ROW_ID) := paste0("v293_r", seq_len(.N))]

capturado <- NULL
ns_shiny <- asNamespace("shiny")
run_app_original <- get("runApp", envir = ns_shiny)
unlockBinding("runApp", ns_shiny)
assign("runApp", function(app, ...) {
  capturado <<- app
  data.table()
}, envir = ns_shiny)
lockBinding("runApp", ns_shiny)
on.exit({
  unlockBinding("runApp", ns_shiny)
  assign("runApp", run_app_original, envir = ns_shiny)
  lockBinding("runApp", ns_shiny)
}, add = TRUE)

meta <- env$monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()
invisible(env$monitora_correcao_painel(dados, meta, env$MONITORA_ARQUIVO_CORRECOES_CAMPOS))
stopifnot(inherits(capturado, "shiny.appobj"), is.function(capturado$serverFuncSource()))
rv_painel <- get("rv", envir = environment(capturado$serverFuncSource()), inherits = FALSE)
estado_botoes_painel <- get("painel_estado", envir = environment(capturado$serverFuncSource()), inherits = FALSE)
ops_repeat_capturados <- NULL
ops_exclusao_coletor_capturados <- NULL
ops_impacto_capturados <- NULL
ops_impacto_sim_capturados <- NULL
justificativas_lote_capturadas <- NULL

shiny::testServer(capturado$serverFuncSource(), {
  session$setInputs(
    responsavel = "Homologação automatizada",
    coleta = coleta_teste,
    escopo_coletas = "coleta_individual"
  )
  session$flushReact()

  estado_inicial <- coletores_estado_efetivo()
  stopifnot(nrow(estado_inicial$membros) >= 1L)
  n_membros_inicial <- nrow(estado_inicial$membros)

  session$setInputs(
    coletor_nome = "Integrante de teste sem CPF",
    coletor_cpf = "",
    coletor_motivo = "Homologação da inclusão com CPF opcional.",
    coletor_confirmar = TRUE,
    coletor_adicionar = 1L
  )
  session$flushReact()
  ops_repeat <- data.table::as.data.table(rv_painel$correcoes)[tipo_correcao == "edicao_repeat_coletor"]
  ops_repeat_capturados <<- data.table::copy(ops_repeat)
  stopifnot(
    nrow(ops_repeat) == length(estado_inicial$cols$nome_aliases),
    setequal(as.character(ops_repeat$atributo_coluna_registros_corrig), estado_inicial$cols$nome_aliases),
    all(as.character(ops_repeat$valor_novo) == "Integrante de teste sem CPF"),
    !any(ops_repeat$atributo_coluna_registros_corrig %in% estado_inicial$cols$cpf_aliases),
    nrow(coletores_estado_efetivo()$membros) == n_membros_inicial + 1L
  )

  resultado_repeat <- env$monitora_correcao_aplicar_plano_atomico_sessao(
    dados, rv_painel$correcoes,
    chaves = env$monitora_correcao_colunas_chave(dados),
    dicionario = env$monitora_correcao_dicionario_atributos(dados, meta)
  )
  stopifnot(!isTRUE(resultado_repeat$falha))
  repeat_final <- env$monitora_coletores_repeat_extrair_grupo(resultado_repeat$dt, which(resultado_repeat$dt$COLETA == coleta_teste))
  stopifnot("Integrante de teste sem CPF" %in% repeat_final$nomes)
  pos_novo <- match("Integrante de teste sem CPF", repeat_final$nomes)
  stopifnot(identical(repeat_final$cpfs[pos_novo], ""))

  rv_painel$correcoes <- env$monitora_correcao_template()
  rv_painel$correcoes_solicitadas <- env$monitora_correcao_template()
  rv_painel$correcoes_historico_intencoes <- env$monitora_correcao_template()
  rv_painel$preview_dirty <- FALSE
  membro_excluir <- estado_inicial$membros[1L]
  session$setInputs(
    coletores_tabela_rows_selected = 1L,
    coletor_motivo = "Homologação da exclusão individual de integrante.",
    coletor_confirmar = TRUE,
    coletor_excluir = 1L
  )
  session$flushReact()
  ops_exclusao <- data.table::as.data.table(rv_painel$correcoes)[tipo_correcao == "edicao_repeat_coletor"]
  ops_exclusao_coletor_capturados <<- data.table::copy(ops_exclusao)
  stopifnot(
    nrow(ops_exclusao) %in% 1:2,
    data.table::uniqueN(ops_exclusao$id_correcao) == 1L,
    all(ops_exclusao$linha_indice == membro_excluir$linha_indice),
    any(ops_exclusao$atributo_coluna_registros_corrig == estado_inicial$cols$nome & ops_exclusao$acao == "clear")
  )
  resultado_exclusao <- env$monitora_correcao_aplicar_plano_atomico_sessao(
    dados, ops_exclusao,
    chaves = env$monitora_correcao_colunas_chave(dados),
    dicionario = env$monitora_correcao_dicionario_atributos(dados, meta)
  )
  stopifnot(
    !isTRUE(resultado_exclusao$falha),
    env$monitora_correcao_vazio(resultado_exclusao$dt[[estado_inicial$cols$nome]][membro_excluir$linha_indice]),
    is.na(estado_inicial$cols$cpf) || env$monitora_correcao_vazio(resultado_exclusao$dt[[estado_inicial$cols$cpf]][membro_excluir$linha_indice])
  )

  rv_painel$correcoes <- env$monitora_correcao_template()
  rv_painel$correcoes_solicitadas <- env$monitora_correcao_template()
  rv_painel$correcoes_historico_intencoes <- env$monitora_correcao_template()
  rv_painel$preview_dirty <- FALSE
  session$setInputs(
    atributo = tipos_col,
    acao = "append_token",
    escopo = "coleta_inteira",
    n_esperado = 101L
  )
  session$flushReact()
  ctx_tipos <- monitora_painel_impacto_contexto_efetivo()
  stopifnot(isTRUE(ctx_tipos$permitido_tipos))
  html_valor <- paste(as.character(output$ui_valor_novo_controle), collapse = " ")
  stopifnot(
    grepl("Incêndio — incendio", html_valor, fixed = TRUE),
    grepl("Pisoteio da vegetação", html_valor, fixed = TRUE),
    grepl("pisoteio", html_valor, fixed = TRUE)
  )

  rv_painel$correcoes <- env$monitora_correcao_template()
  rv_painel$correcoes_solicitadas <- env$monitora_correcao_template()
  rv_painel$correcoes_historico_intencoes <- env$monitora_correcao_template()
  rv_painel$preview_dirty <- FALSE
  session$setInputs(
    atributo = impacto_col,
    acao = "update",
    valor_novo = "não",
    escopo = "coleta_inteira",
    n_esperado = 101L,
    motivo = "Homologação da cascata condicional de impactos.",
    confirmar_abrangencia = TRUE,
    add_corr = 1L
  )
  session$flushReact()
  ops_impacto <- data.table::as.data.table(rv_painel$correcoes)
  ops_impacto_capturados <<- data.table::copy(ops_impacto)
  stopifnot(
    nrow(ops_impacto) >= 2L,
    any(ops_impacto$atributo_coluna_registros_corrig == impacto_col & ops_impacto$acao == "update"),
    any(ops_impacto$atributo_coluna_registros_corrig == tipos_col & ops_impacto$acao == "remove_token"),
    data.table::uniqueN(ops_impacto$id_correcao) == 1L
  )
  resultado_impacto <- env$monitora_correcao_aplicar_plano_atomico_sessao(
    dados, ops_impacto,
    chaves = env$monitora_correcao_colunas_chave(dados),
    dicionario = env$monitora_correcao_dicionario_atributos(dados, meta)
  )
  stopifnot(
    !isTRUE(resultado_impacto$falha),
    all(env$monitora_correcao_normalizar_nome_coluna(resultado_impacto$dt[COLETA == coleta_teste][[impacto_col]]) == "nao"),
    all(env$monitora_correcao_vazio_vec(resultado_impacto$dt[COLETA == coleta_teste][[tipos_col]]))
  )

  rv_painel$correcoes <- env$monitora_correcao_template()
  rv_painel$correcoes_solicitadas <- env$monitora_correcao_template()
  rv_painel$correcoes_historico_intencoes <- env$monitora_correcao_template()
  rv_painel$preview_dirty <- FALSE
  session$setInputs(
    coleta = coleta_nao_teste,
    atributo = impacto_col,
    acao = "update",
    valor_novo = "sim",
    escopo = "coleta_inteira",
    n_esperado = 101L,
    motivo = "Homologação da ativação condicional de impactos.",
    confirmar_abrangencia = TRUE,
    add_corr = 2L
  )
  session$flushReact()
  session$setInputs(
    coleta = coleta_nao_teste,
    atributo = tipos_col,
    acao = "append_token",
    valor_novo = "incendio",
    escopo = "coleta_inteira",
    n_esperado = 101L,
    motivo = "Homologação da inclusão de token após selecionar Sim.",
    confirmar_abrangencia = TRUE
  )
  session$flushReact()
  stopifnot(isTRUE(monitora_painel_impacto_contexto_efetivo()$permitido_tipos))
  estado_botoes_painel$botoes_ultima_conclusao[["add_corr"]] <- Sys.time() - 5
  session$setInputs(add_corr = 3L)
  session$flushReact()
  ops_impacto_sim <- data.table::as.data.table(rv_painel$correcoes)
  ops_impacto_sim_capturados <<- data.table::copy(ops_impacto_sim)
  stopifnot(
    any(ops_impacto_sim$atributo_coluna_registros_corrig == impacto_col & ops_impacto_sim$acao == "update" & ops_impacto_sim$valor_novo == "sim"),
    any(ops_impacto_sim$atributo_coluna_registros_corrig == tipos_col & ops_impacto_sim$acao == "append_token" & ops_impacto_sim$valor_novo == "incendio")
  )
  resultado_impacto_sim <- env$monitora_correcao_aplicar_plano_atomico_sessao(
    dados, ops_impacto_sim,
    chaves = env$monitora_correcao_colunas_chave(dados),
    dicionario = env$monitora_correcao_dicionario_atributos(dados, meta)
  )
  linhas_nao <- which(resultado_impacto_sim$dt$COLETA == coleta_nao_teste)
  stopifnot(
    !isTRUE(resultado_impacto_sim$falha),
    all(env$monitora_correcao_normalizar_nome_coluna(resultado_impacto_sim$dt[[impacto_col]][linhas_nao]) == "sim"),
    all(vapply(resultado_impacto_sim$dt[[tipos_col]][linhas_nao], function(v) "incendio" %in% env$monitora_correcao_tokenizar(v), logical(1L)))
  )

  rv_painel$ocorrencias_idx <- data.table(
    ocorrencia_id = c("occ_lote_1", "occ_lote_2"),
    tipo_ocorrencia = c("mudanca_formacao_vegetacional", "mudanca_formacao_vegetacional"),
    COLETA = c(coleta_teste, coleta_nao_teste), UC = "UC teste", EA = "EA1", UA = "UA1",
    ANO = c("2025", "2026"), linha_indice = c(1L, 102L),
    ocorrencia_token = "", ocorrencia_atributo = "formacao_vegetacional",
    ocorrencia_status = "revisao", ocorrencia_detalhe = "Mudança entre anos"
  )
  rv_painel$justificativas_sessao <- env$monitora_pendencias_justificativas_template()
  rv_painel$preview_revision <- rv_painel$preview_revision + 1L
  session$setInputs(
    just_tabela_ocorrencias_rows_selected = c(1L, 2L),
    just_tipo = "pendencia_legitima",
    just_texto = "Mudança ecológica documentada e confirmada em campo.",
    just_confirmar_lote = TRUE,
    just_adicionar = 1L
  )
  session$flushReact()
  lote <- data.table::as.data.table(rv_painel$justificativas_sessao)
  justificativas_lote_capturadas <<- data.table::copy(lote)
  stopifnot(
    nrow(lote) == 2L,
    data.table::uniqueN(lote$evento_lote_id) == 1L,
    data.table::uniqueN(lote$evento_justificativa_id) == 2L,
    setequal(as.character(lote$ocorrencia_id), c("occ_lote_1", "occ_lote_2")),
    identical(as.integer(lote$ordem_no_lote), 1:2),
    all(as.integer(lote$n_ocorrencias_lote) == 2L)
  )
})

n_aliases_nome <- length(intersect(c("coletor/nome", "COLETORES", "Coletores"), names(dados)))
stopifnot(nrow(ops_repeat_capturados) == n_aliases_nome, nrow(ops_exclusao_coletor_capturados) %in% 1:3, nrow(ops_impacto_capturados) >= 2L, nrow(ops_impacto_sim_capturados) >= 2L, nrow(justificativas_lote_capturadas) == 2L)
arquivo_repeat <- file.path(dir_teste, "correcoes_repeat.csv")
arquivo_exclusao <- file.path(dir_teste, "correcoes_exclusao_coletor.csv")
arquivo_impacto <- file.path(dir_teste, "correcoes_impacto.csv")
arquivo_impacto_sim <- file.path(dir_teste, "correcoes_impacto_sim.csv")
fwrite(ops_repeat_capturados, arquivo_repeat, na = "")
fwrite(ops_exclusao_coletor_capturados, arquivo_exclusao, na = "")
fwrite(ops_impacto_capturados, arquivo_impacto, na = "")
fwrite(ops_impacto_sim_capturados, arquivo_impacto_sim, na = "")
dicionario_real <- env$monitora_correcao_dicionario_atributos(dados, meta)

persistido_repeat <- env$monitora_correcao_aplicar_arquivo(data.table::copy(dados), arquivo_repeat, dicionario = dicionario_real)
repeat_persistido <- env$monitora_coletores_repeat_extrair_grupo(persistido_repeat, which(persistido_repeat$COLETA == coleta_teste))
pos_repeat_persistido <- match("Integrante de teste sem CPF", repeat_persistido$nomes)
stopifnot(!is.na(pos_repeat_persistido), identical(repeat_persistido$cpfs[pos_repeat_persistido], ""))

persistido_exclusao <- env$monitora_correcao_aplicar_arquivo(data.table::copy(dados), arquivo_exclusao, dicionario = dicionario_real)
linha_excluida <- as.integer(ops_exclusao_coletor_capturados$linha_indice[1L])
cols_repeat <- env$monitora_coletores_repeat_colunas_visiveis(persistido_exclusao)
stopifnot(
  env$monitora_correcao_vazio(persistido_exclusao[[cols_repeat$nome]][linha_excluida]),
  is.na(cols_repeat$cpf) || env$monitora_correcao_vazio(persistido_exclusao[[cols_repeat$cpf]][linha_excluida])
)

persistido_impacto <- env$monitora_correcao_aplicar_arquivo(data.table::copy(dados), arquivo_impacto, dicionario = dicionario_real)
linhas_sim_persistidas <- which(persistido_impacto$COLETA == coleta_teste)
stopifnot(
  all(env$monitora_correcao_normalizar_nome_coluna(persistido_impacto[[impacto_col]][linhas_sim_persistidas]) == "nao"),
  all(env$monitora_correcao_vazio_vec(persistido_impacto[[tipos_col]][linhas_sim_persistidas]))
)

persistido_impacto_sim <- env$monitora_correcao_aplicar_arquivo(data.table::copy(dados), arquivo_impacto_sim, dicionario = dicionario_real)
linhas_nao_persistidas <- which(persistido_impacto_sim$COLETA == coleta_nao_teste)
stopifnot(
  all(env$monitora_correcao_normalizar_nome_coluna(persistido_impacto_sim[[impacto_col]][linhas_nao_persistidas]) == "sim"),
  all(vapply(persistido_impacto_sim[[tipos_col]][linhas_nao_persistidas], function(v) "incendio" %in% env$monitora_correcao_tokenizar(v), logical(1L)))
)

cat("V293_PANEL_SERVER_TESTS_OK\n")
