#!/usr/bin/env Rscript
# Rodada dedicada de teste: shiny::runApp() real de monitora_correcao_painel()
# usando SOMENTE dataset sintetico e stubs sinteticos das 45 constantes MONITORA_*
# identificadas em diagnostics/validacao_fecho_painel_codetools_20260705_015121/.
#
# Metodo seguro (mesma familia do fecho_painel_codetools.R, estendido):
#   1. parse() do script inteiro (NAO source()) - nada e avaliado nesta etapa.
#   2. Filtro AST recursivo: extrai SOMENTE atribuicoes `nome <- function(...) {...}`,
#      inclusive quando aninhadas dentro de blocos if(){}/{} de nivel superior
#      (padrao usado no script original para "dobrar" secoes no editor, ex. a
#      funcao monitora_validados_schema_embutido esta dentro de um `if (TRUE) { }`
#      de nivel 0). NENHUMA condicao de `if` e avaliada; so navegamos a AST.
#      Isso resolve o gap dos "25 nomes monitora_* nao capturados" documentado em
#      diagnostics/validacao_fecho_painel_codetools_20260705_015121/VALIDACAO_FECHO_PAINEL_CODETOOLS.md.
#   3. Cada funcao e avaliada isoladamente num ambiente `fn_env` (parent=globalenv(),
#      apos os pacotes shiny/DT/data.table serem carregados) - define a funcao,
#      nao executa seu corpo.
#   4. As 45 constantes MONITORA_*/.MONITORA_* sao definidas ANTES da chamada,
#      com valores sinteticos ou identicos aos defaults literais do proprio
#      script (quando o default e so um valor de configuracao, nao dado real).
#   5. dt e meta_xls sao 100% sinteticos/fabricados nesta rodada; nenhum arquivo
#      de dados reais (registros_importados*.csv) e lido.
#   6. MONITORA_CORRECOES_DIR / MONITORA_LOG_DIR / MONITORA_OUTPUT_DIR apontam
#      para um diretorio isolado sob diagnostics/, nunca para output/ real do
#      projeto.
#   7. shiny::runApp() e chamado de verdade (esta e a funcao real do script,
#      nao um mock) num host/porta fixos e locais; o processo roda sob
#      `timeout` do shell (ver script wrapper) para encerrar de forma
#      controlada, pois nao ha navegador real clicando em "Salvar"/"Fechar".

args <- commandArgs(trailingOnly = TRUE)
tmp_root <- if (length(args) >= 1) args[[1]] else stop("uso: harness_runapp_sintetico.R <tmp_root> <script_path>")
script_path <- if (length(args) >= 2) args[[2]] else stop("uso: harness_runapp_sintetico.R <tmp_root> <script_path>")

cat("=== Harness runApp() sintetico H2R-C ===\n")
cat("tmp_root:", tmp_root, "\n")
cat("script_path:", script_path, "\n")
cat("R.version.string:", R.version.string, "\n")

dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
dir_correcoes <- file.path(tmp_root, "correcoes_campos")
dir_log <- file.path(tmp_root, "log")
dir_output <- file.path(tmp_root, "output")
dir.create(dir_correcoes, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(MONITORA_PAINEL_LAUNCH_BROWSER = "false")
Sys.setenv(MONITORA_DEBUG_PREVIEW_PAINEL = "N")
options(
  shiny.port = 38765L,
  shiny.host = "127.0.0.1",
  shiny.launch.browser = FALSE,
  shiny.autoreload = FALSE
)

suppressMessages({
  library(shiny)
  library(DT)
  library(data.table)
})

## ---------------------------------------------------------------------
## Passo 1-3: parse + harvest recursivo (seguro por construcao) + eval das
## funcoes num fn_env (parent = globalenv() para resolver funcoes de base/
## pacotes anexados sem precisar prefixo ::, sem executar o pipeline).
## ---------------------------------------------------------------------
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
      # if(cond) bloco [else bloco] - a condicao NUNCA e avaliada aqui, so
      # navegamos os ramos na AST em busca de definicoes de funcao.
      if (length(e) >= 3) out <- harvest(e[[3]], out)
      if (length(e) >= 4) out <- harvest(e[[4]], out)
    }
  }
  out
}

harvested <- list()
for (e in exprs) harvested <- harvest(e, harvested)
cat("n_expressoes_top_level:", length(exprs), "\n")
cat("n_atribuicoes_function_capturadas (recursivo, incl. if-wrapped):", length(harvested), "\n")

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
stopifnot(exists("monitora_validados_schema_embutido", envir = fn_env, inherits = FALSE))

## ---------------------------------------------------------------------
## Passo 4: as 45 constantes MONITORA_*/.MONITORA_* do fecho, definidas no
## globalenv() (fn_env cascateia ate globalenv(), entao <<- dentro das
## funcoes atualiza aqui). Valores: sinteticos (diretorios/IDs isolados) ou
## identicos ao default literal do script original quando e so config,
## nao dado real (ex.: lista de colunas protegidas, estrutura de tabelas
## vazias de log/performance).
## ---------------------------------------------------------------------
MONITORA_EXEC_ID <<- paste0("SINTETICO_TESTE_", format(Sys.time(), "%Y%m%d_%H%M%S"))
MONITORA_CORRECOES_DIR <<- dir_correcoes
MONITORA_LOG_DIR <<- dir_log
MONITORA_OUTPUT_DIR <<- dir_output
MONITORA_ARQUIVO_CORRECOES_CAMPOS <<- file.path(dir_correcoes, "correcoes_campos_sintetico.csv")

# Lista literal identica ao default do script (linhas 3176-3183) - sao nomes
# de coluna de config, nao dado real; replicar aqui e seguro.
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

# Performance/progresso: desabilitados (evita depender de todo o estado
# interno da barra de progresso; funcoes monitora_perf_*/monitora_progresso_*
# devem checar esses flags e retornar cedo).
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

## ---------------------------------------------------------------------
## Passo 5: dt e meta_xls 100% sinteticos.
## Nomes de atributo usados abaixo vem do schema PUBLICO embutido no script
## (monitora_validados_schema_embutido(), estrutura de formulario, nao dado
## real) - so a FORMA (nomes de coluna) e reaproveitada; TODOS os VALORES
## abaixo sao fabricados para este teste.
## ---------------------------------------------------------------------
schema129 <- fn_env$monitora_validados_schema_embutido()
cat("schema129 nrow:", nrow(schema129), "\n")

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
data.table::setnames(
  dt_sintetico,
  old = c(
    "amostragem/registro/ponto_amostral", "amostragem/registro/ponto_metro",
    "amostragem/registro/tipo_forma_vida", "amostragem/registro/forma_vida_nativa_arvore_abaixo"
  ),
  new = c(
    "amostragem/registro/ponto_amostral", "amostragem/registro/ponto_metro",
    "amostragem/registro/tipo_forma_vida", "amostragem/registro/forma_vida_nativa_arvore_abaixo"
  ),
  skip_absent = TRUE
)
cat("dt_sintetico dim:", paste(dim(dt_sintetico), collapse = " x "), "\n")
cat("dt_sintetico names:", paste(names(dt_sintetico), collapse = ", "), "\n")

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

## ---------------------------------------------------------------------
## Passo 6-7: chamada real de monitora_correcao_painel(). Isso vai executar
## de fato o corpo da funcao (preparar schema, dicionario, contrato) e, se
## nao houver stop() antes, chegar ate shiny::runApp() de verdade.
## ---------------------------------------------------------------------
cat("=== Chamando monitora_correcao_painel() (chamada real) ===\n")
resultado <- tryCatch({
  fn_env$monitora_correcao_painel(
    dt = dt_sintetico,
    meta_xls = meta_xls_sintetico,
    arquivo_saida = MONITORA_ARQUIVO_CORRECOES_CAMPOS
  )
}, error = function(e) {
  cat("ERRO_CAPTURADO: ", conditionMessage(e), "\n", sep = "")
  structure(list(erro = conditionMessage(e)), class = "erro_harness")
})

cat("=== Fim da chamada (retornou/foi interrompido) ===\n")
cat("classe do resultado:", paste(class(resultado), collapse = ","), "\n")
if (inherits(resultado, "erro_harness")) {
  cat("RESULTADO: erro antes/durante runApp(): ", resultado$erro, "\n", sep = "")
} else {
  cat("RESULTADO: retorno normal de monitora_correcao_painel (runApp() encerrou sem excecao).\n")
}
