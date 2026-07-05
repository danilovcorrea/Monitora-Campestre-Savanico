#!/usr/bin/env Rscript
# Executa UM caso da matriz 2x2 (chamada real e isolada de
# monitora_correcao_painel() via shiny::runApp()) usando dataset e stubs
# 100% sinteticos. Mesma tecnica validada em
# diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/harness_runapp_sintetico.R
# (parse() + harvest recursivo da AST, sem source() do script inteiro).
#
# Uso: Rscript rodar_caso_app.R <caso A|B|C|D> <tmp_root> <script_path> <porta>
#
# Este script SO deve ser executado localmente pelo usuario (Fedora), nunca
# pelo motor de IA. Ele fica bloqueado em shiny::runApp() ate o usuario
# clicar em algo (via rodar_caso_chromote.R, em outro processo R) ou ate o
# timeout do processo pai.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) stop("uso: rodar_caso_app.R <caso> <tmp_root> <script_path> <porta>")
caso <- args[[1]]
tmp_root <- args[[2]]
script_path <- args[[3]]
porta <- as.integer(args[[4]])

cat("=== rodar_caso_app.R - caso ", caso, " ===\n", sep = "")
cat("tmp_root:", tmp_root, "\n")
cat("script_path:", script_path, "\n")
cat("porta:", porta, "\n")
cat("R.version.string:", R.version.string, "\n")

dir_correcoes <- file.path(tmp_root, "correcoes_campos")
dir_log <- file.path(tmp_root, "log")
dir_output <- file.path(tmp_root, "output")
dir.create(dir_correcoes, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(MONITORA_PAINEL_LAUNCH_BROWSER = "false")
Sys.setenv(MONITORA_DEBUG_PREVIEW_PAINEL = "N")
options(
  shiny.port = porta,
  shiny.host = "127.0.0.1",
  shiny.launch.browser = FALSE,
  shiny.autoreload = FALSE
)

suppressMessages({
  library(shiny)
  library(DT)
  library(data.table)
})

## Passo 1-3: parse + harvest recursivo (identico ao harness validado em
## 20260705_093608) - nenhum source() do arquivo inteiro, nenhuma condicao
## de if avaliada durante a colheita de funcoes.
exprs <- parse(file = script_path, keep.source = FALSE)

is_function_assign <- function(e) {
  if (!is.call(e)) return(FALSE)
  if (!is.symbol(e[[1]])) return(FALSE)
  op <- as.character(e[[1]])
  if (!(op %in% c("<-", "=", "<<-"))) return(FALSE)
  if (length(e) < 3) return(FALSE)
  target <- e[[2]]
  if (!is.symbol(target)) return(FALSE)
  rhs <- e[[3]]
  is.call(rhs) && is.symbol(rhs[[1]]) && identical(as.character(rhs[[1]]), "function")
}

harvest <- function(e, out) {
  if (is_function_assign(e)) {
    out[[length(out) + 1]] <- list(nm = as.character(e[[2]]), rhs = e[[3]])
    return(out)
  }
  if (is.call(e) && is.symbol(e[[1]])) {
    op <- as.character(e[[1]])
    if (identical(op, "{")) {
      for (i in seq_len(length(e) - 1)) out <- harvest(e[[i + 1]], out)
    } else if (identical(op, "if")) {
      if (length(e) >= 3) out <- harvest(e[[3]], out)
      if (length(e) >= 4) out <- harvest(e[[4]], out)
    }
  }
  out
}

harvested <- list()
for (e in exprs) harvested <- harvest(e, harvested)
cat("n_expressoes_top_level:", length(exprs), "\n")
cat("n_atribuicoes_function_capturadas:", length(harvested), "\n")

fn_env <- new.env(parent = globalenv())
n_ok <- 0L
for (item in harvested) {
  fn_obj <- tryCatch(eval(item$rhs, envir = fn_env), error = function(err) NULL)
  if (is.function(fn_obj)) {
    assign(item$nm, fn_obj, envir = fn_env)
    n_ok <- n_ok + 1L
  }
}
cat("n_funcoes_definidas_com_sucesso:", n_ok, "\n")
stopifnot(exists("monitora_correcao_painel", envir = fn_env, inherits = FALSE))
stopifnot(exists("monitora_painel_encerrar_sem_materializar", envir = fn_env, inherits = FALSE) || TRUE)

## Passo 4: constantes MONITORA_*/.MONITORA_* do fecho - todas isoladas sob
## tmp_root (nunca output/, log/ ou input/ reais do projeto). Inclui, em
## relacao ao harness anterior, MONITORA_ARQUIVO_CORRECOES_ESPACIAIS
## explicito, para fechar o gap de default relativo a input/correcoes_espaciais.csv
## identificado nesta rodada (script.R linha ~18692).
MONITORA_EXEC_ID <<- paste0("SINTETICO_MATRIZ_", caso, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
MONITORA_CORRECOES_DIR <<- dir_correcoes
MONITORA_LOG_DIR <<- dir_log
MONITORA_OUTPUT_DIR <<- dir_output
MONITORA_ARQUIVO_CORRECOES_CAMPOS <<- file.path(dir_correcoes, "correcoes_campos_sintetico.csv")
MONITORA_ARQUIVO_CORRECOES_ESPACIAIS <<- file.path(dir_correcoes, "correcoes_espaciais_sintetico.csv")
MONITORA_INPUT_DIR <<- file.path(tmp_root, "input_isolado_nao_usado")

MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <<- unique(c(
  ".id", "id", "ID", "uuid", "UUID", "uuid_registro", "UUID_REGISTRO",
  "coleta_uuid", "COLETA_UUID", "COLETA", "PROTOCOLO", "arquivo_origem",
  "linha_indice", "linha_origem", "linha_origem_registros_corrig",
  "ordem_linha_original", "arquivo_fonte", "source_file",
  "ANO", "DATA_MONITORA_PARSEADA", "num_placa_formatado",
  "DATA DO REGISTRO", "DATA DO RECEBIMENTO", "ULTIMA EDICAO",
  "data_do_registro", "data_do_recebimento", "ultima_edicao"
))

MONITORA_LOG_EXECUCAO <<- data.table::data.table(
  etapa = character(), severidade = character(), arquivo = character(),
  detalhe = character(), acao = character()
)

MONITORA_RELATORIOS_DIAGNOSTICOS_PRE_PAINEL <<- NULL

.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <<- new.env(parent = emptyenv())
.MONITORA_RELATORIO_COLUNAS_CACHE <<- new.env(parent = emptyenv())
.MONITORA_RELATORIO_EXOTICAS_DETECTAR_COLUNAS_FORMA <<- fn_env$monitora_relatorio_exoticas_detectar_colunas_forma
.MONITORA_RELATORIO_EXOTICAS_DETECTAR_COLUNAS_ESPECIES <<- fn_env$monitora_relatorio_exoticas_detectar_colunas_especies

MONITORA_PERF_ENABLED <<- FALSE
MONITORA_PERF_START_TIME <<- Sys.time()
MONITORA_PERF_LAST_TIME <<- MONITORA_PERF_START_TIME
MONITORA_PERF_PAUSED_SEC <<- 0
MONITORA_PERF_PAUSE_START <<- fn_env$monitora_posixct_na()
MONITORA_PERF_EXECUCAO <<- data.table::data.table(
  ordem = integer(), etapa = character(), inicio = character(), fim = character(),
  duracao_seg = numeric(), duracao_acumulada_seg = numeric(), n_linhas = integer(),
  n_colunas = integer(), objeto_mb = numeric(), rss_mb = numeric(),
  mem_available_mb = numeric(), detalhe = character(), tempo_usuario_pausado_seg = numeric()
)

MONITORA_PROGRESSO_HABILITADO <<- FALSE
MONITORA_PROGRESSO_BACKEND <<- "txt"
MONITORA_PROGRESSO_BACKEND_ATIVO <<- NA_character_
MONITORA_PROGRESSO_CLI_DISPONIVEL <<- FALSE
MONITORA_PROGRESSO_CLI_FALHOU <<- FALSE
MONITORA_PROGRESSO_CLI_AVISO_AUSENTE_EMITIDO <<- FALSE
MONITORA_PROGRESSO_CLI_FALLBACK_TXT <<- "N"
MONITORA_PROGRESSO_CLI_FORMAT <<- NA_character_
MONITORA_PROGRESSO_CLI_FORMAT_DONE <<- NA_character_
MONITORA_PROGRESSO_CLI_LARGURA <<- 180L
MONITORA_PROGRESSO_CLI_LAYOUT <<- "barra_completa"
MONITORA_PROGRESSO_CLI_MANTER_HISTORICO <<- "N"
MONITORA_PROGRESSO_CLI_STATUS_MAX_CHARS <<- 180L
MONITORA_PROGRESSO_FLUSH_MIN_INTERVALO_SEG <<- 1.5
MONITORA_PROGRESSO_LIMITE_PRE_FINAL <<- 9950L
MONITORA_PROGRESSO_TOTAL <<- 10000L
MONITORA_PROGRESSO_VALOR <<- 0L
MONITORA_PROGRESSO_PB <<- NULL
MONITORA_PROGRESSO_ALVO_CHECKPOINT <<- 0L
MONITORA_PROGRESSO_RENDER_MIN_DELTA <<- 20L
MONITORA_PROGRESSO_RENDER_MIN_INTERVALO_SEG <<- 0.35
MONITORA_PROGRESSO_ULTIMA_ETAPA <<- NA_character_
MONITORA_PROGRESSO_ULTIMO_FLUSH_TS <<- Sys.time() - 3600
MONITORA_PROGRESSO_ULTIMO_RENDER_TS <<- Sys.time() - 3600
MONITORA_PROGRESSO_ULTIMO_STATUS <<- NA_character_
MONITORA_PROGRESSO_ULTIMO_STATUS_RENDERIZADO <<- NA_character_
MONITORA_PROGRESSO_ULTIMO_VALOR_RENDERIZADO <<- -1L

cat("n_constantes_MONITORA_definidas:", length(Filter(function(n) grepl("^\\.?MONITORA_", n), ls(envir = globalenv()))), "\n")

## Passo 5: dt e meta_xls 100% sinteticos - identicos aos usados na rodada
## anterior (diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608),
## que ja comprovou reproduzir a pendencia impeditiva "nativa sem forma de
## vida" (linhas com forma_vida_nativa_arvore_abaixo == "").
n_linhas_sinteticas <- 6L
dt_sintetico <- data.table::data.table(
  COLETA = sprintf("SINTETICO-%03d", seq_len(n_linhas_sinteticas)),
  uuid_registro = sprintf("uuid-sintetico-%03d", seq_len(n_linhas_sinteticas)),
  coleta_uuid = sprintf("uuid-coleta-sintetico-%03d", seq_len(n_linhas_sinteticas)),
  `amostragem/registro/ponto_amostral` = rep(c("1", "2"), length.out = n_linhas_sinteticas),
  `amostragem/registro/ponto_metro` = rep(c("0", "10", "20"), length.out = n_linhas_sinteticas),
  `amostragem/registro/tipo_forma_vida` = rep("nativa", n_linhas_sinteticas),
  `amostragem/registro/forma_vida_nativa_arvore_abaixo` = rep(c("1", ""), length.out = n_linhas_sinteticas),
  UC = rep("UC_SINTETICA", n_linhas_sinteticas),
  EA = rep("EA_SINTETICA", n_linhas_sinteticas),
  ANO = rep("2026", n_linhas_sinteticas),
  CICLO = rep("1", n_linhas_sinteticas),
  UA = rep("UA_SINTETICA", n_linhas_sinteticas),
  CAMPANHA = rep("CAMPANHA_SINTETICA", n_linhas_sinteticas),
  PROTOCOLO = sprintf("PROTOCOLO-SINT-%03d", seq_len(n_linhas_sinteticas)),
  data_do_registro = rep("2026-07-01 10:00:00", n_linhas_sinteticas),
  validado = rep("nao", n_linhas_sinteticas)
)
cat("dt_sintetico dim:", paste(dim(dt_sintetico), collapse = " x "), "\n")

meta_xls_sintetico <- list(
  arquivos = data.table::data.table(
    arquivo_xlsform = "sismonitora_sintetico.xlsx", form_id = "sismonitora_sintetico", version = "1"
  ),
  campos = data.table::data.table(
    arquivo_xlsform = "sismonitora_sintetico.xlsx",
    name = c("ponto_amostral", "ponto_metro", "tipo_forma_vida", "forma_vida_nativa_arvore_abaixo"),
    type = c("text", "text", "select_one tipo_forma_vida", "text"),
    label = c("Ponto amostral", "Ponto metro", "Tipo de forma de vida", "Forma de vida nativa: arvore abaixo"),
    caminho_registro = c(
      "amostragem/registro/ponto_amostral", "amostragem/registro/ponto_metro",
      "amostragem/registro/tipo_forma_vida", "amostragem/registro/forma_vida_nativa_arvore_abaixo"
    ),
    list_name = c(NA_character_, NA_character_, "tipo_forma_vida", NA_character_),
    tipo_base = c("texto", "texto", "texto", "texto"),
    required = c(TRUE, TRUE, TRUE, FALSE),
    relevant = c(NA_character_, NA_character_, NA_character_, NA_character_)
  ),
  opcoes = data.table::data.table(
    arquivo_xlsform = "sismonitora_sintetico.xlsx",
    list_name = "tipo_forma_vida",
    name = c("nativa", "exotica"),
    label = c("Nativa", "Exotica")
  ),
  dependencias = data.table::data.table()
)

arquivo_saida_final <- MONITORA_ARQUIVO_CORRECOES_CAMPOS
arquivo_saida_espacial_final <- MONITORA_ARQUIVO_CORRECOES_ESPACIAIS

## Passo 6-7: chamada real de monitora_correcao_painel() ate shiny::runApp().
cat("=== Chamando monitora_correcao_painel() (chamada real, caso ", caso, ") ===\n", sep = "")
resultado <- tryCatch({
  fn_env$monitora_correcao_painel(
    dt = dt_sintetico,
    meta_xls = meta_xls_sintetico,
    arquivo_saida = arquivo_saida_final
  )
}, error = function(e) {
  cat("ERRO_CAPTURADO: ", conditionMessage(e), "\n", sep = "")
  structure(list(erro = conditionMessage(e)), class = "erro_harness")
})

cat("=== Fim da chamada (retornou/foi interrompido) ===\n")
classe_resultado <- paste(class(resultado), collapse = ",")
cat("classe do resultado:", classe_resultado, "\n")

mensagem_erro <- if (inherits(resultado, "erro_harness")) resultado$erro else NA_character_
retorno_normal <- !inherits(resultado, "erro_harness")
n_linhas_saida <- if (retorno_normal) tryCatch(nrow(resultado), error = function(e) NA_integer_) else NA_integer_

json_out <- list(
  caso = caso,
  porta = porta,
  classe_resultado = classe_resultado,
  retorno_normal = retorno_normal,
  mensagem_erro = mensagem_erro,
  n_linhas_resultado = n_linhas_saida,
  arquivo_saida_existe = file.exists(arquivo_saida_final),
  arquivo_saida_espacial_existe = file.exists(arquivo_saida_espacial_final)
)
arq_json <- file.path(tmp_root, paste0("RESULTADO_CASO_", caso, ".json"))
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(json_out, arq_json, auto_unbox = TRUE, null = "null", na = "null")
} else {
  # fallback minimo sem depender de jsonlite
  esc <- function(x) if (is.na(x)) "null" else paste0('"', gsub('"', '\\"', as.character(x), fixed = TRUE), '"')
  linhas_json <- c(
    "{",
    paste0('  "caso": ', esc(caso), ","),
    paste0('  "porta": ', porta, ","),
    paste0('  "classe_resultado": ', esc(classe_resultado), ","),
    paste0('  "retorno_normal": ', tolower(as.character(retorno_normal)), ","),
    paste0('  "mensagem_erro": ', esc(mensagem_erro), ","),
    paste0('  "n_linhas_resultado": ', if (is.na(n_linhas_saida)) "null" else n_linhas_saida, ","),
    paste0('  "arquivo_saida_existe": ', tolower(as.character(file.exists(arquivo_saida_final))), ","),
    paste0('  "arquivo_saida_espacial_existe": ', tolower(as.character(file.exists(arquivo_saida_espacial_final)))),
    "}"
  )
  writeLines(linhas_json, arq_json)
}
cat("RESULTADO_JSON_ESCRITO:", arq_json, "\n")
