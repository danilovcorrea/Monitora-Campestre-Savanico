#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(openxlsx)
})
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R", mustWork = TRUE)
fixture <- Sys.getenv("MONITORA_TESTE_FNCS_2026_REGISTROS_VALIDADOS", unset = "")
stopifnot(nzchar(fixture), file.exists(fixture))

parsed <- parse(file = script)
body <- parsed[[1L]][[2L]]
env <- new.env(parent = globalenv())
carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    if (is.call(rhs) && identical(as.character(rhs[[1L]]), "function")) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

raiz <- tempfile("v293_xlsx_coletores_")
dir.create(file.path(raiz, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(raiz, "log"), recursive = TRUE)
on.exit(unlink(raiz, recursive = TRUE, force = TRUE), add = TRUE)
env$MONITORA_OUTPUT_DIR <- raiz
env$MONITORA_LOG_DIR <- file.path(raiz, "log")
env$MONITORA_EXEC_ID <- "v293_xlsx_coletores"
env$MONITORA_SCRIPT_VERSAO <- "2.9.3-dev-test"
env$MONITORA_REGISTROS_VALIDADOS_GERADO <- TRUE
env$MONITORA_LOG_EXECUCAO <- data.table(
  etapa = character(), severidade = character(), arquivo = character(),
  detalhe = character(), acao = character()
)

fonte <- fread(fixture, colClasses = "character", na.strings = NULL, check.names = FALSE, encoding = "UTF-8", showProgress = FALSE)
fonte <- fonte[
  uc == "Floresta Nacional de Contendas do Sincorá" &
    ciclo == "Ciclo-2026-VgCS" & campanha == "Campanha-2026-VgCS"
]
stopifnot(nrow(fonte) == 5858L, uniqueN(fonte$coleta) == 58L)
schema_validado_original <- names(fonte)
coleta_teste <- as.character(fonte$coleta[1L])
idx <- which(fonte$coleta == coleta_teste)
stopifnot(length(idx) == 101L, all(c("coletor/nome", "coletor/cpf") %in% names(fonte)))
fonte[idx, `coletor/nome` := "{'Pessoa Alfa': '52998224725', 'Pessoa Beta': '017********', 'Pessoa Gama': ''}"]
fonte[idx, `coletor/cpf` := ""]

san <- env$monitora_coletores_repeat_sanitizar_legado(
  fonte, output_dir = raiz, log_dir = file.path(raiz, "log"), exec_id = env$MONITORA_EXEC_ID,
  contexto = "integracao_xlsx", abortar_nao_reconhecido = TRUE
)
fonte_saneada <- san$dt
stopifnot(
  isTRUE(san$ok), nrow(san$auditoria) == 1L,
  identical(fonte_saneada[idx, `coletor/nome`][1:3], c("Pessoa Alfa", "Pessoa Beta", "Pessoa Gama")),
  identical(fonte_saneada[idx, `coletor/cpf`][1:3], c("52998224725", "", ""))
)
### Em produção, a projeção registros_corrig -> registros_validados conserva
### somente o schema contratual. Removemos aqui apenas aliases auxiliares que a
### sanitização acrescentou ao fixture já projetado para reproduzir esse gate.
extras_auxiliares <- setdiff(names(fonte_saneada), schema_validado_original)
if (length(extras_auxiliares)) fonte_saneada[, (extras_auxiliares) := NULL]
hash_antes <- digest(fonte_saneada, algo = "sha256")
fonte_csv <- file.path(raiz, "01_produtos_dados", "registros_validados.csv")
fwrite(fonte_saneada, fonte_csv, sep = ",", quote = "auto", qmethod = "double", na = "", bom = TRUE, encoding = "UTF-8", eol = "\n")
resultado <- env$monitora_planilha_importacao_sismonitora_gerar(
  registros_validados = fonte_saneada, output_dir = raiz, log_dir = file.path(raiz, "log"),
  exec_id = env$MONITORA_EXEC_ID, schema = data.table(atributo = names(fonte_saneada)),
  fonte_csv = fonte_csv, remover_uuid = TRUE
)
stopifnot(length(resultado$produto) == 1L, file.exists(resultado$produto), file.info(resultado$produto)$size > 0L)
stopifnot(identical(hash_antes, digest(fonte_saneada, algo = "sha256")))

planilha <- read.xlsx(resultado$produto, sheet = "Preenchimento", colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
headers <- env$monitora_importacao_sismonitora_headers(data.table(atributo = names(fonte_saneada)))
pos <- match(c("coletor/nome", "coletor/cpf"), headers)
stopifnot(!anyNA(pos))
dados <- as.data.table(planilha[3:nrow(planilha), pos, drop = FALSE])
setnames(dados, c("nome", "cpf"))
ordem_bloco <- which(as.character(resultado$auditoria$coleta) == coleta_teste)
stopifnot(length(ordem_bloco) == 1L)
inicio <- (ordem_bloco - 1L) * 101L + 1L
bloco <- dados[inicio:(inicio + 100L)]
limpar <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
stopifnot(
  identical(limpar(bloco$nome)[1:3], c("Pessoa Alfa", "Pessoa Beta", "Pessoa Gama")),
  identical(limpar(bloco$cpf)[1:3], c("52998224725", "", "")),
  all(limpar(bloco$nome)[4:101] == ""), all(limpar(bloco$cpf)[2:101] == ""),
  !any(grepl("^\\s*\\{", limpar(bloco$nome), perl = TRUE)),
  resultado$manifesto$n_coletas == 58L, resultado$manifesto$n_linhas_dados == 5858L
)

cat("V293_XLSX_COLETORES_SANITIZADOS_OK linhas=5858 coletas=58 xlsx=1\n")
