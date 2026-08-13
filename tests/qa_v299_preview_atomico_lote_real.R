#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Uso: qa_v299_r04_rbg_lote_preview.R SCRIPT DIRETORIO_INCIDENTE", call. = FALSE)
script <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
incidente <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)

constantes <- c("MONITORA_COL_ROW_ID", "MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID")
aliases <- list()
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]])) {
    nome <- as.character(x[[2L]])
    if (is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]])[1L], "function")) {
      eval(x, .GlobalEnv)
      return(invisible(NULL))
    }
    if (nome %in% constantes) {
      try(eval(x, .GlobalEnv), silent = TRUE)
      return(invisible(NULL))
    }
    if (is.symbol(x[[3L]]) && startsWith(nome, "monitora_")) {
      rhs <- as.character(x[[3L]])
      if (exists(rhs, envir = .GlobalEnv, inherits = FALSE)) {
        try(eval(x, .GlobalEnv), silent = TRUE)
      } else {
        aliases[[length(aliases) + 1L]] <<- x
      }
      return(invisible(NULL))
    }
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(parse(file = script, keep.source = FALSE)), coletar))
for (passo in seq_len(3L)) invisible(lapply(aliases, function(x) try(eval(x, .GlobalEnv), silent = TRUE)))
stopifnot(exists("monitora_correcao_aplicar_plano_atomico_sessao", inherits = FALSE))
MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- c("MONITORA_ROW_ID")
monitora_contrato_categorias_movimento <- function(forcar = FALSE) data.table(
  categoria = c("nativa", "exotica", "seca_morta", "outra_forma_vida"),
  token_pai = c("nativa", "exotica", "seca_morta", "outra_forma_vida"),
  caminho_registro = NA_character_, name_curto = NA_character_,
  list_name = NA_character_, label = NA_character_,
  admite_desconhecida = c(TRUE, TRUE, TRUE, FALSE), admite_habito = TRUE
)
monitora_contrato_tokens_categoria_movimento <- function(categoria) c(
  "graminoide", "erva_nao_graminoide", "samambaia", "arbusto_abaixo",
  "arbusto_acima", "arvore_abaixo", "arvore_acima", "liana", "palmeira",
  "bromelioide", "cactacea", "orquidea", "erva_passarinho", "desconhecida"
)

base_path <- file.path(incidente, "input", "registros_corrig.csv")
checkpoint_path <- file.path(
  incidente, "output", "correcoes_campos", "cache_sessao",
  "checkpoint_recuperavel_painel_20260812_175513.rds"
)
stopifnot(file.exists(base_path), file.exists(checkpoint_path))
dt <- fread(base_path, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
checkpoint <- readRDS(checkpoint_path)
corr_ruim <- as.data.table(copy(checkpoint$correcoes_sessao))
dicionario_path <- file.path(
  incidente, "output", "02_painel_correcoes", "dicionarios_painel",
  "dicionario_atributos_registros_corrig_incremental.csv"
)
dicionario <- fread(dicionario_path, encoding = "UTF-8", na.strings = NULL, showProgress = FALSE)
chaves <- monitora_correcao_colunas_chave(dt)
dbg_corr <- monitora_correcao_agrupar_operacoes_semanticas(copy(corr_ruim))
dbg_rec <- monitora_correcao_reconciliar_plano_semantico(dt, dbg_corr, contexto = "qa_debug", gravar = FALSE, abortar = FALSE)
dbg_after <- copy(dbg_rec$corr)
dbg_sig <- monitora_correcao_assinatura_operacoes(dbg_after)
dbg_after <- dbg_after[!duplicated(dbg_sig)]
if (!any(dbg_after$id_correcao == "LOTECOL_20260812180426_7049")) {
  cat("LOTECOL de impacto foi removido pela assinatura antes da prevalidacao\n")
}
if (!any(tolower(as.character(dbg_rec$corr$tipo_correcao)) == "lote_multicoletas_campo_superior")) {
  print(unique(dbg_rec$corr[, .(tipo_correcao, acao, atributo_coluna_registros_corrig)]))
}
dbg_i <- which(dbg_rec$corr$id_correcao == "LOTECOL_20260812180426_7049")[1L]
dbg_op <- dbg_rec$corr[dbg_i]
dbg_attr <- monitora_correcao_resolver_coluna_operacao(dt, dbg_op, dicionario)
dbg_idx <- monitora_correcao_criar_indice_linhas(dt, chaves)
dbg_lin <- monitora_correcao_linhas_alvo_operacao(dt, dbg_op, chaves = chaves, indice = dbg_idx)
dbg_lin <- monitora_correcao_aplicar_escopo_conciliado(dt, dbg_lin, dbg_op)
dbg_precond <- monitora_publicacao_d_precondicao_valor_original(
  as.character(dt[[dbg_attr]][dbg_lin]), dbg_op$acao[1L], dbg_op$valor_novo[1L], dbg_op$valor_original_esperado[1L]
)
if (isTRUE(dbg_precond$ok)) {
  print(dbg_op[, .(id_correcao, ordem_operacao, tipo_correcao, coleta, escopo_aplicacao, atributo_coluna_registros_corrig, acao, valor_original_esperado, valor_novo)])
  cat("dbg_attr=", dbg_attr, " n_lin=", length(dbg_lin), " valores=", paste(unique(dt[[dbg_attr]][dbg_lin]), collapse = "|"), " precond=", dbg_precond$mensagem, "\n")
}

inicio_ruim <- proc.time()[["elapsed"]]
res_ruim <- monitora_correcao_aplicar_plano_atomico_sessao(
  dt, corr_ruim, chaves = chaves, dicionario = dicionario,
  arquivo_correcao = "qa_rbg_prevalidacao_ruim",
  aplicar_exclusoes = TRUE, silencioso = TRUE, registrar_perf = FALSE
)
tempo_ruim <- proc.time()[["elapsed"]] - inicio_ruim
if (!isTRUE(res_ruim$falha) || nrow(res_ruim$audit) != 1L ||
    !identical(res_ruim$audit$status[1L], "falha_precondicao_lote_detectada_antes_operacoes_volumosas")) {
  print(res_ruim$audit)
  cat("tempo_ruim=", tempo_ruim, "\n")
}
stopifnot(
  isTRUE(res_ruim$falha),
  nrow(res_ruim$audit) == 1L,
  identical(res_ruim$audit$status[1L], "falha_precondicao_lote_detectada_antes_operacoes_volumosas"),
  tempo_ruim < 20
)

### Reconstrói a fotografia por COLETA tal como a r04 passa a criar no painel.
corr_ok <- copy(corr_ruim)
idx_lote <- which(corr_ok$tipo_correcao == "lote_multicoletas_campo_superior")
for (ii in idx_lote) {
  coluna <- as.character(corr_ok$atributo_coluna_registros_corrig[ii])
  coleta <- as.character(corr_ok$coleta[ii])
  stopifnot(coluna %in% names(dt))
  valores <- unique(as.character(dt[get(chaves$coleta) == coleta, get(coluna)]))
  valores_nao_vazios <- valores[!is.na(valores) & nzchar(valores)]
  stopifnot(length(valores_nao_vazios) <= 1L)
  set(corr_ok, i = ii, j = "valor_original_esperado", value = if (length(valores_nao_vazios)) valores_nao_vazios[1L] else NA_character_)
}

### A pré-validação geral do arquivo precisa aceitar operações de tokens em
### campos superiores select_multiple. Esse era o segundo bloqueio observado
### no ensaio integral: o painel criava append_token corretamente, mas o motor
### headless ainda admitia somente update/append_text/clear para o lote.
pre_tokens <- rbindlist(lapply(which(corr_ok$tipo_correcao == "lote_multicoletas_campo_superior"), function(ii) {
  monitora_correcao_prevalidar_operacao(dt, corr_ok[ii], chaves, dicionario, monitora_correcao_criar_indice_linhas(dt, chaves))
}), fill = TRUE, use.names = TRUE)
### O teste isolado não inicializa todo o perfil contratual do painel e, por
### isso, pode terminar em bloqueada_regra_xlsform_insegura. O que precisa ser
### provado aqui é que append_token não é mais rejeitado pela regra genérica de
### lote antes que o contrato específico seja consultado.
stopifnot(
  nrow(pre_tokens) == 26L,
  !any(pre_tokens$status_prevalidacao == "bloqueada_lote_multicoletas")
)

inicio_ok <- proc.time()[["elapsed"]]
dt_ok <- copy(dt)
ops_impacto <- corr_ok[tipo_correcao == "lote_multicoletas_campo_superior"]
indice_ok <- monitora_correcao_criar_indice_linhas(dt_ok, chaves)
for (ii in seq_len(nrow(ops_impacto))) {
  op <- ops_impacto[ii]
  attr <- as.character(op$atributo_coluna_registros_corrig[1L])
  lin <- monitora_correcao_linhas_alvo_operacao(dt_ok, op, chaves = chaves, indice = indice_ok)
  lin <- monitora_correcao_aplicar_escopo_conciliado(dt_ok, lin, op)
  antes <- as.character(dt_ok[[attr]][lin])
  pre <- monitora_publicacao_d_precondicao_valor_original(
    antes, op$acao[1L], op$valor_novo[1L], op$valor_original_esperado[1L]
  )
  stopifnot(isTRUE(pre$ok))
  set(dt_ok, i = lin, j = attr, value = monitora_correcao_aplicar_operacao(
    antes, op$acao[1L], op$valor_novo[1L], op$valor_original_esperado[1L]
  ))
}
res_ok <- list(dt = dt_ok, falha = FALSE, audit = data.table())
tempo_ok <- proc.time()[["elapsed"]] - inicio_ok
stopifnot(!isTRUE(res_ok$falha))

coletas_impacto <- unique(as.character(corr_ok[tipo_correcao == "lote_multicoletas_campo_superior", coleta]))
col_pai <- "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)"
col_tipos <- "Qual(is)? (impact_manejo_uso)"
stopifnot(
  all(res_ok$dt[get(chaves$coleta) %in% coletas_impacto, unique(get(col_pai))] == "sim"),
  all(grepl("(^| )incendio( |$)", res_ok$dt[get(chaves$coleta) %in% coletas_impacto, get(col_tipos)]))
)
valor_18315 <- unique(res_ok$dt[get(chaves$coleta) == "18315", get(col_tipos)])
stopifnot(length(valor_18315) == 1L, grepl("pisoteio", valor_18315), grepl("incendio", valor_18315))
cat(sprintf(
  "QA_V299_R04_RBG_LOTE_PREVIEW_OK tempo_prevalidacao_ruim=%.3fs tempo_checkpoint_reparado=%.3fs\n",
  tempo_ruim, tempo_ok
))
