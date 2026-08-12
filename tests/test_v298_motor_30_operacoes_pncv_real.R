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
for (expr in parsed) carregar_definicoes(expr)

dir_teste <- tempfile("v298_motor_30_ops_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_LOG_DIR <- file.path(dir_teste, "log")
env$MONITORA_CORRECOES_DIR <- file.path(dir_teste, "output", "correcoes_campos")
env$MONITORA_EXEC_ID <- "v298_motor_30_ops"
env$MONITORA_DEPENDENCIAS_CORRECOES <- data.table()
env$MONITORA_SCRIPT_VERSAO <- "2.9.8-dev"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.8-test"
for (pasta in c(env$MONITORA_OUTPUT_DIR, env$MONITORA_LOG_DIR, env$MONITORA_CORRECOES_DIR)) {
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
}
for (nome_cache in c(
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nome_cache, new.env(parent = emptyenv()), envir = env)

dados <- fread(
  pncv_arquivo, encoding = "UTF-8", na.strings = NULL,
  colClasses = "character", showProgress = FALSE
)
dados <- env$monitora_identidade_assegurar(dados, contexto = "teste_v298_motor_30_ops")
meta <- env$monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()
dados <- env$monitora_correcao_preparar_schema_edicao_painel(
  dados, meta, contexto = "teste_v298_motor_30_ops"
)
chaves <- env$monitora_correcao_colunas_chave(dados)
dicionario <- env$monitora_correcao_dicionario_atributos(dados, meta)

atributo <- names(dados)[grepl("^Descreva observações gerais do transecto", names(dados))][1L]
stopifnot(!is.na(atributo), nzchar(atributo), atributo %in% names(dados))
normalizar <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}
universo <- dados[, .(
  n_linhas = .N,
  n_valores = uniqueN(normalizar(get(atributo))),
  valor_original = normalizar(get(atributo))[1L]
), by = COLETA][!is.na(COLETA) & nzchar(COLETA) & n_valores == 1L]
stopifnot(nrow(universo) >= 30L)
universo <- universo[seq_len(30L)]

operacoes <- rbindlist(lapply(seq_len(nrow(universo)), function(ii) {
  coleta <- as.character(universo$COLETA[ii])
  linhas <- which(as.character(dados[[chaves$coleta]]) == coleta)
  novo <- paste0("Homologação v2.9.8 da COLETA ", coleta)
  op <- env$monitora_correcao_criar_operacao(
    id = sprintf("V298_PNCV_OP_%02d", ii), responsavel = "Homologação automatizada",
    tipo = "edicao_campo", ordem = ii, escopo = "coleta_inteira",
    coleta = coleta, atributo = atributo, acao = "update",
    valor_original = as.character(universo$valor_original[ii]), valor_novo = novo,
    n_esperado = length(linhas), n_alvo = length(linhas),
    motivo = "Homologação do motor único com 30 operações reais"
  )
  env$monitora_correcao_anexar_contexto_operacao(op, dados, linhas, chaves)
}), fill = TRUE, use.names = TRUE)
stopifnot(nrow(operacoes) == 30L)

inicio <- proc.time()[["elapsed"]]
resultado <- env$monitora_correcao_aplicar_plano_atomico_sessao(
  dados, operacoes, chaves = chaves, dicionario = dicionario,
  arquivo_correcao = "homologacao_v298_30_operacoes_pncv",
  aplicar_exclusoes = TRUE, silencioso = TRUE, registrar_perf = FALSE
)
duracao <- proc.time()[["elapsed"]] - inicio
stopifnot(is.list(resultado), !isTRUE(resultado$falha), nrow(resultado$dt) == nrow(dados))

for (ii in seq_len(nrow(universo))) {
  coleta <- as.character(universo$COLETA[ii])
  esperado <- paste0("Homologação v2.9.8 da COLETA ", coleta)
  observado <- unique(normalizar(resultado$dt[as.character(get(chaves$coleta)) == coleta, get(atributo)]))
  stopifnot(identical(observado, esperado))
}

operacoes_falha <- data.table::copy(operacoes)
operacoes_falha[1L, `:=`(
  id_correcao = "V298_PNCV_FALHA_EXATA",
  valor_original_esperado = "VALOR_ORIGINAL_INCOMPATIVEL_V298"
)]
inicio_falha <- proc.time()[["elapsed"]]
resultado_falha <- env$monitora_correcao_aplicar_plano_atomico_sessao(
  dados, operacoes_falha, chaves = chaves, dicionario = dicionario,
  arquivo_correcao = "homologacao_v298_30_operacoes_pncv_com_falha",
  aplicar_exclusoes = TRUE, silencioso = TRUE, registrar_perf = FALSE
)
duracao_falha <- proc.time()[["elapsed"]] - inicio_falha
stopifnot(
  isTRUE(resultado_falha$falha), nrow(resultado_falha$audit) > 0L,
  any(as.character(resultado_falha$audit$id_correcao) == "V298_PNCV_FALHA_EXATA"),
  any(as.character(resultado_falha$audit$status) == "falha_preview_precondicao")
)

cat(sprintf(
  "TEST_V298_MOTOR_30_OPERACOES_PNCV_REAL_OK: linhas=%d; coletas=%d; operacoes=%d; duracao_sucesso=%.3fs; duracao_falha_exata=%.3fs\n",
  nrow(dados), nrow(universo), nrow(operacoes), duracao, duracao_falha
))
