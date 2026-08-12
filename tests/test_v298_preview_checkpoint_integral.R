#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(shiny)
  library(DT)
})
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.8.R",
  mustWork = TRUE
)
pncv_arquivo <- Sys.getenv("MONITORA_TESTE_PNCV_DADOS", unset = "")
stopifnot(file.exists(pncv_arquivo))

parsed <- parse(file = script, keep.source = FALSE)
body <- as.call(c(list(as.name("{")), as.list(parsed)))
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

dir_teste <- tempfile("v298_preview_checkpoint_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_LOG_DIR <- file.path(dir_teste, "log")
env$MONITORA_CORRECOES_DIR <- file.path(dir_teste, "output", "correcoes_campos")
env$MONITORA_EXEC_ID <- "v298_preview_checkpoint"
env$MONITORA_LOG_EXECUCAO <- data.table(
  etapa = character(), severidade = character(), arquivo = character(),
  detalhe = character(), acao = character()
)
env$MONITORA_VALIDAR_ESPACIAL_COLETAS <- FALSE
env$MONITORA_ARQUIVO_CORRECOES_ESPACIAIS <- file.path(dir_teste, "correcoes_espaciais.csv")
env$MONITORA_ARQUIVO_CORRECOES_CAMPOS <- file.path(dir_teste, "correcoes_campos.csv")
env$MONITORA_DEPENDENCIAS_CORRECOES <- data.table()
env$MONITORA_SCRIPT_VERSAO <- "2.9.8-dev"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.8-test"
env$MONITORA_PERF_ENABLED <- FALSE
for (nome_cache in c(
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nome_cache, new.env(parent = emptyenv()), envir = env)

checkpoint_legado_incompativel <- Sys.getenv("MONITORA_TESTE_RUN17_CHECKPOINT", unset = "")
if (file.exists(checkpoint_legado_incompativel)) {
  dir.create(file.path(env$MONITORA_CORRECOES_DIR, "cache_sessao"), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(
    checkpoint_legado_incompativel,
    file.path(env$MONITORA_CORRECOES_DIR, "cache_sessao", basename(checkpoint_legado_incompativel)),
    overwrite = TRUE
  ))
}

teste_base_completa_legado <- identical(Sys.getenv("MONITORA_TESTE_BASE_COMPLETA_LEGADO", unset = "N"), "S")
amostra <- fread(
  pncv_arquivo, nrows = if (isTRUE(teste_base_completa_legado)) -1L else 10000L,
  encoding = "UTF-8", na.strings = NULL,
  colClasses = "character", showProgress = FALSE
)
stopifnot("COLETA" %in% names(amostra), nrow(amostra) > 0L)
coleta_teste <- amostra[, .N, by = COLETA][!is.na(COLETA) & nzchar(COLETA)][order(-N), COLETA][1L]
dados <- if (isTRUE(teste_base_completa_legado)) amostra else amostra[as.character(COLETA) == as.character(coleta_teste)]
stopifnot(nrow(dados) > 0L)
if (!(env$MONITORA_COL_ROW_ID %in% names(dados))) {
  dados[, (env$MONITORA_COL_ROW_ID) := paste0("v298_pncv_", seq_len(.N))]
}

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
if (isTRUE(teste_base_completa_legado)) {
  cat(sprintf(
    "TEST_V298_CHECKPOINT_LEGADO_RUN17_REJEITADO_COM_SEGURANCA: status=%s; justificativas_restauradas=%d\n",
    shiny::isolate(as.character(rv_painel$checkpoint_recuperado_status)),
    nrow(shiny::isolate(rv_painel$justificativas_sessao))
  ))
  stopifnot(
    !nzchar(shiny::isolate(as.character(rv_painel$checkpoint_recuperado_status))),
    nrow(shiny::isolate(rv_painel$justificativas_sessao)) == 0L
  )
  quit(save = "no", status = 0L)
}
if (file.exists(checkpoint_legado_incompativel)) {
  stopifnot(
    !nzchar(shiny::isolate(as.character(rv_painel$checkpoint_recuperado_status))),
    nrow(shiny::isolate(rv_painel$justificativas_sessao)) == 0L
  )
}

primeiro_tempo <- NA_real_
segundo_tempo <- NA_real_
mensagem_primeira <- ""
mensagem_segunda <- ""
checkpoint_path <- ""
terceiro_tempo <- NA_real_
shiny::testServer(capturado$serverFuncSource(), {
  atributo_teste <- names(dados)[grepl("^Ocorreram impactos", names(dados))][1L]
  stopifnot(!is.na(atributo_teste), nzchar(atributo_teste))
  op <- env$monitora_correcao_criar_operacao(
    id = "V298_FALHA_PRECONDICAO", responsavel = "Homologação automatizada",
    tipo = "edicao_campo", ordem = 1L, escopo = "coleta_inteira",
    coleta = coleta_teste, atributo = atributo_teste,
    acao = "update", valor_original = "VALOR_ORIGINAL_INCOMPATIVEL_V298", valor_novo = "sim",
    n_esperado = nrow(dados), n_alvo = nrow(dados), motivo = "Homologação do diagnóstico persistente"
  )
  op <- env$monitora_correcao_anexar_contexto_operacao(
    op, dados, seq_len(nrow(dados)), env$monitora_correcao_colunas_chave(dados)
  )
  rv_painel$correcoes <- data.table::copy(op)
  rv_painel$correcoes_solicitadas <- data.table::copy(op)
  rv_painel$correcoes_historico_intencoes <- data.table::copy(op)
  rv_painel$preview_dirty <- TRUE

  t1 <- proc.time()[["elapsed"]]
  mensagem_primeira <<- tryCatch({
    monitora_painel_recalcular_preview_integral(origem = "teste_primeira_falha")
    "SEM_ERRO"
  }, error = function(e) conditionMessage(e))
  primeiro_tempo <<- proc.time()[["elapsed"]] - t1
  stopifnot(
    grepl("V298_FALHA_PRECONDICAO", mensagem_primeira, fixed = TRUE),
    grepl(as.character(coleta_teste), mensagem_primeira, fixed = TRUE),
    grepl("falha_preview_precondicao", mensagem_primeira, fixed = TRUE),
    nrow(rv_painel$preview_falha_auditoria) > 0L,
    nzchar(rv_painel$preview_falha_assinatura)
  )
  arq_aud <- file.path(env$MONITORA_CORRECOES_DIR, "auditoria_preview_motor_unico_falhas_ultima_execucao.csv")
  stopifnot(file.exists(arq_aud), file.info(arq_aud)$size > 0L)

  t2 <- proc.time()[["elapsed"]]
  mensagem_segunda <<- tryCatch({
    monitora_painel_recalcular_preview_integral(origem = "teste_falha_repetida")
    "SEM_ERRO"
  }, error = function(e) conditionMessage(e))
  segundo_tempo <<- proc.time()[["elapsed"]] - t2
  stopifnot(
    grepl("fila é idêntica", mensagem_segunda, fixed = TRUE),
    segundo_tempo < 1,
    segundo_tempo < primeiro_tempo
  )

  monitora_painel_tentar_salvar_e_fechar(confirmado_pendencias = FALSE)
  t3 <- proc.time()[["elapsed"]]
  monitora_painel_tentar_salvar_e_fechar(confirmado_pendencias = FALSE)
  terceiro_tempo <<- proc.time()[["elapsed"]] - t3
  stopifnot(terceiro_tempo < 1)
  checkpoints <- list.files(
    file.path(env$MONITORA_CORRECOES_DIR, "cache_sessao"),
    pattern = "^checkpoint_recuperavel_painel_.*[.]rds$", full.names = TRUE
  )
  stopifnot(length(checkpoints) == 1L)
  checkpoint_path <<- checkpoints[[1L]]
  obj <- readRDS(checkpoint_path)
  stopifnot(
    identical(obj$checkpoint_schema, "monitora_painel_checkpoint_v2"),
    nrow(obj$correcoes_sessao) == 1L,
    nrow(obj$correcoes_solicitadas) == 1L,
    nrow(obj$correcoes_historico_intencoes) == 1L,
    nrow(obj$auditoria_preview_falha) > 0L,
    identical(as.character(obj$correcoes_sessao$id_correcao), "V298_FALHA_PRECONDICAO")
  )
})

stopifnot(file.exists(checkpoint_path))
localizacao_checkpoint <- env$monitora_painel_checkpoint_localizar(dados)
if (!isTRUE(localizacao_checkpoint$encontrado)) {
  stop(
    "Checkpoint integral não pôde ser relocalizado antes da reabertura: status=",
    as.character(localizacao_checkpoint$status), "; rejeicoes=",
    paste(as.character(localizacao_checkpoint$rejeicoes), collapse = " | "),
    call. = FALSE
  )
}
capturado <- NULL
invisible(env$monitora_correcao_painel(dados, meta, env$MONITORA_ARQUIVO_CORRECOES_CAMPOS))
stopifnot(inherits(capturado, "shiny.appobj"))
rv_restaurado <- get("rv", envir = environment(capturado$serverFuncSource()), inherits = FALSE)
shiny::testServer(capturado$serverFuncSource(), {
  stopifnot(
    identical(rv_restaurado$checkpoint_recuperado_status, "checkpoint_integral_v2"),
    nrow(rv_restaurado$correcoes) == 1L,
    identical(as.character(rv_restaurado$correcoes$id_correcao), "V298_FALHA_PRECONDICAO"),
    nrow(rv_restaurado$preview_falha_auditoria) > 0L,
    isTRUE(rv_restaurado$preview_dirty)
  )
})

cat(sprintf(
  "TEST_V298_PREVIEW_CHECKPOINT_INTEGRAL_OK: PNCV_COLETA=%s; primeira_falha=%.4fs; repeticao_cacheada=%.4fs; segundo_salvamento_cacheado=%.4fs\n",
  coleta_teste, primeiro_tempo, segundo_tempo, terceiro_tempo
))
