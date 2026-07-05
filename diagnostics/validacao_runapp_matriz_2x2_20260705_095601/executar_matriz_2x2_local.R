#!/usr/bin/env Rscript
# Orquestrador da matriz 2x2 de validacao do fluxo de fechamento do painel
# Shiny de monitora_campsav_alvo_global_v2.6.0.R, apos a correcao da funcao
# ausente monitora_painel_encerrar_sem_materializar (achado registrado em
# diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/VALIDACAO_RUNAPP_SINTETICO.md).
#
# Matriz:
#   A. fechar sem salvar, SEM operacoes no painel
#   B. fechar sem salvar, COM operacoes no painel
#   C. salvar checkpoint com pendencias, SEM operacoes no painel
#   D. salvar checkpoint com pendencias, COM operacoes no painel
#
# Metodo: para cada caso, sobe um processo R em segundo plano rodando
# shiny::runApp() de verdade (rodar_caso_app.R, dataset 100% sintetico,
# MONITORA_CORRECOES_DIR/MONITORA_LOG_DIR/MONITORA_OUTPUT_DIR isolados sob
# tmp_isolado_<caso>/), espera a porta abrir, interage via Chrome headless
# real (rodar_caso_chromote.R) clicando nos botoes reais da UI, espera o
# processo do app encerrar (ou aplica timeout de seguranca), e verifica os
# resultados esperados a partir do log e do RESULTADO_CASO_<X>.json.
#
# ATENCAO: este script SO deve ser executado localmente pelo usuario
# (Fedora), nunca pelo motor de IA. Ele executa R, shiny::runApp() e Chrome
# headless de verdade.
#
# Uso: Rscript executar_matriz_2x2_local.R [caminho_script_producao]

## ---------------------------------------------------------------------
## Localizacao dos caminhos
## ---------------------------------------------------------------------
get_script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  pref <- "--file="
  hit <- grep(pref, a, fixed = TRUE, value = TRUE)
  if (length(hit) == 1) return(dirname(normalizePath(sub(pref, "", hit[[1]]), mustWork = FALSE)))
  normalizePath(getwd())
}

this_dir <- get_script_dir()
args_usuario <- commandArgs(trailingOnly = TRUE)

# raiz do repo = dois niveis acima de diagnostics/validacao_runapp_matriz_2x2_*/
repo_root <- normalizePath(file.path(this_dir, "..", ".."), mustWork = FALSE)
script_path_default <- file.path(repo_root, "monitora_campsav_alvo_global_v2.6.0.R")
script_path <- if (length(args_usuario) >= 1) args_usuario[[1]] else script_path_default

if (!file.exists(script_path)) {
  stop("Script de producao nao encontrado: ", script_path, ". Informe o caminho como argumento: Rscript executar_matriz_2x2_local.R <caminho>")
}

cat("=== executar_matriz_2x2_local.R ===\n")
cat("diretorio do harness:", this_dir, "\n")
cat("script de producao:", script_path, "\n")
cat("R.version.string:", R.version.string, "\n\n")

casos <- c("A", "B", "C", "D")
portas <- c(A = 38810L, B = 38811L, C = 38812L, D = 38813L)
TIMEOUT_PORTA_SEG <- 20
TIMEOUT_APP_POS_CLIQUES_SEG <- 25

## ---------------------------------------------------------------------
## Utilitarios de processo (base R + utilitarios de shell padrao do
## Linux: kill -0 para checar processo vivo, sem executar R/Shiny agora -
## sao usados so quando ESTE script for executado localmente pelo usuario).
## ---------------------------------------------------------------------
processo_vivo <- function(pid) {
  if (is.na(pid)) return(FALSE)
  identical(suppressWarnings(system(sprintf("kill -0 %d 2>/dev/null", pid))), 0L)
}

matar_processo <- function(pid, sinal = "TERM") {
  if (!is.na(pid) && processo_vivo(pid)) {
    system(sprintf("kill -%s %d 2>/dev/null", sinal, pid))
  }
}

porta_pronta <- function(porta, timeout_seg) {
  t0 <- Sys.time()
  repeat {
    con <- tryCatch(
      suppressWarnings(socketConnection(host = "127.0.0.1", port = porta, open = "r+", timeout = 1)),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      close(con)
      return(TRUE)
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_seg) return(FALSE)
    Sys.sleep(0.5)
  }
}

## ---------------------------------------------------------------------
## Execucao de um caso
## ---------------------------------------------------------------------
executar_caso <- function(caso) {
  cat("\n--- Iniciando caso ", caso, " ---\n", sep = "")
  tmp_root <- file.path(this_dir, paste0("tmp_isolado_", caso))
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
  porta <- portas[[caso]]
  log_app <- file.path(this_dir, paste0("log_caso_", caso, "_app.log"))
  log_chromote <- file.path(this_dir, paste0("log_caso_", caso, "_chromote.log"))
  app_script <- file.path(this_dir, "rodar_caso_app.R")
  chromote_script <- file.path(this_dir, "rodar_caso_chromote.R")

  cmd_bg <- sprintf(
    "Rscript %s %s %s %s %s > %s 2>&1 & echo $!",
    shQuote(app_script), shQuote(caso), shQuote(tmp_root), shQuote(script_path), porta, shQuote(log_app)
  )
  pid_txt <- tryCatch(system(cmd_bg, intern = TRUE), error = function(e) character(0))
  pid <- suppressWarnings(as.integer(trimws(pid_txt[[length(pid_txt)]])))
  cat("PID do app (caso ", caso, "): ", pid, "\n", sep = "")

  ok_porta <- porta_pronta(porta, TIMEOUT_PORTA_SEG)
  if (!ok_porta) {
    cat("FALHA: porta ", porta, " nao abriu em ", TIMEOUT_PORTA_SEG, "s (caso ", caso, ").\n", sep = "")
    matar_processo(pid, "TERM")
    return(list(caso = caso, passou = FALSE, motivo = "porta_nao_abriu", pid = pid))
  }
  cat("Porta ", porta, " pronta (caso ", caso, ").\n", sep = "")

  cmd_chromote <- sprintf("Rscript %s %s %s > %s 2>&1", shQuote(chromote_script), shQuote(caso), porta, shQuote(log_chromote))
  system(cmd_chromote, wait = TRUE)

  t0 <- Sys.time()
  while (processo_vivo(pid) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < TIMEOUT_APP_POS_CLIQUES_SEG) {
    Sys.sleep(0.5)
  }
  encerrou_sozinho <- !processo_vivo(pid)
  if (!encerrou_sozinho) {
    cat("AVISO: processo do app (caso ", caso, ", pid ", pid, ") nao encerrou sozinho; enviando SIGTERM.\n", sep = "")
    matar_processo(pid, "TERM")
    Sys.sleep(1)
    if (processo_vivo(pid)) matar_processo(pid, "KILL")
  }

  log_txt <- if (file.exists(log_app)) paste(readLines(log_app, warn = FALSE), collapse = "\n") else ""
  chromote_txt <- if (file.exists(log_chromote)) paste(readLines(log_chromote, warn = FALSE), collapse = "\n") else ""
  arq_json <- file.path(tmp_root, paste0("RESULTADO_CASO_", caso, ".json"))
  json_res <- if (file.exists(arq_json)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      tryCatch(jsonlite::fromJSON(arq_json), error = function(e) NULL)
    } else NULL
  } else NULL

  # Exige explicitamente pelo menos 1 operacao nova (nao basta a mensagem
  # aparecer com "0 operação(ões) nova(s)", o que indicaria que a correção
  # foi rejeitada silenciosamente por alguma validação de contrato).
  precondicao_operacao_ok <- if (caso %in% c("B", "D")) {
    grepl("adicionada à sessão do painel: [1-9][0-9]* operação", log_txt, perl = TRUE)
  } else TRUE

  contem_erro_funcao_ausente <- grepl('could not find function "monitora_painel_encerrar_sem_materializar"', log_txt, fixed = TRUE)

  criterio <- switch(
    caso,
    A = !contem_erro_funcao_ausente &&
      grepl("Execução encerrada pelo usuário sem materializar registros_corrig.csv", log_txt, fixed = TRUE) &&
      isTRUE(!is.null(json_res) && !isTRUE(json_res$arquivo_saida_existe)),
    B = precondicao_operacao_ok &&
      !contem_erro_funcao_ausente &&
      grepl("Execução encerrada pelo usuário sem materializar registros_corrig.csv", log_txt, fixed = TRUE) &&
      isTRUE(!is.null(json_res) && !isTRUE(json_res$arquivo_saida_existe)),
    C = isTRUE(!is.null(json_res) && isTRUE(json_res$retorno_normal)) &&
      isTRUE(!is.null(json_res) && !isTRUE(json_res$arquivo_saida_existe)) &&
      grepl("Painel fechado sem correções novas|Nenhuma correção de campos pendente na sessão", log_txt),
    D = precondicao_operacao_ok &&
      isTRUE(!is.null(json_res) && isTRUE(json_res$retorno_normal)) &&
      isTRUE(!is.null(json_res) && isTRUE(json_res$arquivo_saida_existe)) &&
      isTRUE(!is.null(json_res) && !is.null(json_res$n_linhas_resultado) && json_res$n_linhas_resultado > 0),
    FALSE
  )

  cat("Caso ", caso, ": precondicao_operacao_ok=", precondicao_operacao_ok,
      " contem_erro_funcao_ausente=", contem_erro_funcao_ausente,
      " criterio_passou=", isTRUE(criterio), "\n", sep = "")

  list(
    caso = caso,
    passou = isTRUE(criterio),
    precondicao_operacao_ok = precondicao_operacao_ok,
    contem_erro_funcao_ausente = contem_erro_funcao_ausente,
    encerrou_sozinho = encerrou_sozinho,
    json = json_res,
    log_app = log_app,
    log_chromote = log_chromote,
    pid = pid
  )
}

resultados <- lapply(casos, executar_caso)
names(resultados) <- casos

## ---------------------------------------------------------------------
## RESULTADO_MATRIZ_2X2.json
## ---------------------------------------------------------------------
arq_resultado_geral <- file.path(this_dir, "RESULTADO_MATRIZ_2X2.json")
resumo_lista <- lapply(resultados, function(r) {
  list(
    caso = r$caso,
    passou = r$passou,
    precondicao_operacao_ok = r$precondicao_operacao_ok,
    contem_erro_funcao_ausente = r$contem_erro_funcao_ausente,
    encerrou_sozinho = r$encerrou_sozinho,
    json_caso = r$json
  )
})
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(resumo_lista, arq_resultado_geral, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
} else {
  writeLines(capture.output(str(resumo_lista)), arq_resultado_geral)
}
cat("\nResultado geral escrito em:", arq_resultado_geral, "\n")

## ---------------------------------------------------------------------
## VALIDACAO_RUNAPP_MATRIZ_2X2.md
## ---------------------------------------------------------------------
todos_passaram <- all(vapply(resultados, function(r) isTRUE(r$passou), logical(1)))

linhas_md <- c(
  "# Validacao da matriz 2x2 - fechamento do painel Shiny (H2R-C)",
  "",
  paste0("Data/hora de execucao: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Script de producao testado: `", script_path, "`"),
  "",
  "## Contexto",
  "",
  "Esta rodada valida a correcao aplicada em `monitora_painel_encerrar_sem_materializar`",
  "(funcao que estava ausente, causando erro real de producao no fluxo",
  "\"Fechar sem salvar\" -> \"Cancelar execucao sem materializar\", conforme",
  "`diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/VALIDACAO_RUNAPP_SINTETICO.md`).",
  "",
  "## Matriz executada",
  "",
  "| Caso | Acao | Operacoes no painel | Passou |",
  "|------|------|----------------------|--------|",
  paste0("| A | Fechar sem salvar | Sem operacoes | ", ifelse(resultados$A$passou, "SIM", "NAO"), " |"),
  paste0("| B | Fechar sem salvar | Com operacoes | ", ifelse(resultados$B$passou, "SIM", "NAO"), " |"),
  paste0("| C | Salvar checkpoint com pendencias | Sem operacoes | ", ifelse(resultados$C$passou, "SIM", "NAO"), " |"),
  paste0("| D | Salvar checkpoint com pendencias | Com operacoes | ", ifelse(resultados$D$passou, "SIM", "NAO"), " |"),
  "",
  "## Detalhes por caso",
  ""
)
for (caso in casos) {
  r <- resultados[[caso]]
  linhas_md <- c(
    linhas_md,
    paste0("### Caso ", caso),
    paste0("- passou: ", r$passou),
    paste0("- precondicao_operacao_ok: ", r$precondicao_operacao_ok),
    paste0("- contem_erro_funcao_ausente: ", r$contem_erro_funcao_ausente),
    paste0("- encerrou_sozinho: ", r$encerrou_sozinho),
    paste0("- log app: `", basename(r$log_app), "`"),
    paste0("- log chromote: `", basename(r$log_chromote), "`"),
    ""
  )
}
linhas_md <- c(
  linhas_md,
  "## Isolamento",
  "",
  "- Cada caso roda em `tmp_isolado_<CASO>/` proprio, com `MONITORA_CORRECOES_DIR`,",
  "  `MONITORA_LOG_DIR`, `MONITORA_OUTPUT_DIR` e `MONITORA_ARQUIVO_CORRECOES_ESPACIAIS`",
  "  isolados sob este diretorio de diagnostico.",
  "- Nenhum dado real foi lido; dataset 100% sintetico (6 linhas fabricadas).",
  "- Nao houve escrita em `input/`, `output/` ou `log/` reais do projeto.",
  "",
  paste0("## Resultado final: ", ifelse(todos_passaram, "MATRIZ_RUNAPP_2X2_OK", "FALHA"))
)
arq_md <- file.path(this_dir, "VALIDACAO_RUNAPP_MATRIZ_2X2.md")
writeLines(linhas_md, arq_md)
cat("Relatorio markdown escrito em:", arq_md, "\n")

if (todos_passaram) {
  cat("\nMATRIZ_RUNAPP_2X2_OK\n")
} else {
  cat("\nMATRIZ_RUNAPP_2X2_FALHOU\n")
  quit(status = 1)
}
