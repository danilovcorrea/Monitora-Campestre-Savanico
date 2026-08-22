#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(openxlsx)
})

exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global.R",
  winslash = "/", mustWork = TRUE
)
fixture <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else Sys.getenv(
    "MONITORA_TESTE_REGISTROS_VALIDADOS", unset = ""
  ),
  winslash = "/", mustWork = TRUE
)
linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
inicio <- grep(
  "^monitora_importacao_sismonitora_modelo_sha256 <-", linhas
) - 1L
fim <- grep("^monitora_registros_validados_exportar <-", linhas) - 1L
exigir(
  length(inicio) == 1L && length(fim) == 1L && fim > inicio,
  "Bloco do gerador SISMONITORA não localizado."
)
env <- new.env(parent = globalenv())
env$MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- TRUE
eval(parse(text = linhas[inicio:fim]), envir = env)
env$MONITORA_SCRIPT_VERSAO <- "2.9.16"
env$MONITORA_REGISTROS_VALIDADOS_GERADO <- TRUE

fonte <- fread(
  fixture, colClasses = "character", na.strings = NULL,
  check.names = FALSE, encoding = "UTF-8", showProgress = FALSE
)
chaves <- c("uc", "ciclo", "campanha")
exigir(all(chaves %in% names(fonte)), "Fixture sem chaves contextuais.")
ctx <- fonte[, .N, by = chaves][order(N)][1L]
ctx_uc <- ctx$uc[[1L]]
ctx_ciclo <- ctx$ciclo[[1L]]
ctx_campanha <- ctx$campanha[[1L]]
amostra <- fonte[
  uc == ctx_uc & ciclo == ctx_ciclo & campanha == ctx_campanha
]
exigir(nrow(amostra) > 0L, "Contexto real vazio.")

saida <- tempfile("qa_v2916_office_")
nome_logico <- "registros_validados_importacao_sismonitora.xlsx"
repeat {
  candidato <- file.path(saida, "01_produtos_dados", nome_logico)
  compacto <- file.path(
    saida, "01_produtos_dados", "sis_ctx-001-0000000000.xlsx"
  )
  ultracompacto <- file.path(
    saida, "01_produtos_dados", "s_0000000000.xlsx"
  )
  if (nchar(candidato) > 210L && nchar(compacto) > 210L &&
      nchar(ultracompacto) <= 210L) break
  saida <- paste0(saida, "x")
}
dir.create(file.path(saida, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida, "logs"), recursive = TRUE)
on.exit(unlink(sub("/segmento_caminho.*$", "", saida), recursive = TRUE, force = TRUE), add = TRUE)
fonte_csv <- file.path(saida, "01_produtos_dados", "registros_validados.csv")
fwrite(amostra, fonte_csv, bom = TRUE, na = "", encoding = "UTF-8")

tempo <- system.time(resultado <- env$monitora_planilha_importacao_sismonitora_gerar(
  registros_validados = amostra,
  output_dir = saida,
  log_dir = file.path(saida, "logs"),
  exec_id = "qa_v2916_caminho_longo",
  schema = data.table(atributo = names(amostra)),
  fonte_csv = fonte_csv,
  remover_uuid = FALSE
))
produto <- resultado$produto[[1L]]
exigir(file.exists(produto) && file.info(produto)$size > 0L, "XLSX real ausente ou vazio.")
exigir(
  grepl("^s_[a-f0-9]{10}\\.xlsx$", basename(produto)),
  "Nome físico compacto não foi aplicado ao XLSX real."
)
exigir(nchar(produto) <= 210L, "XLSX compacto ainda excede 210 caracteres.")
exigir(
  identical(
    resultado$manifesto$produto_logico[[1L]],
    nome_logico
  ),
  "Manifesto perdeu o nome lógico do produto."
)
exigir(
  identical(
    openxlsx::getSheetNames(produto),
    c("Preenchimento", "Opções válidas", "Campos Comuns")
  ),
  "Estrutura de três abas do XLSX foi alterada."
)
exigir(unname(tempo[["elapsed"]]) < 30, "Compactação introduziu custo relevante.")
cat(sprintf(
  "TEST_V2916_CAMINHOS_OFFICE_REAL_OK elapsed=%.3fs path=%d\n",
  unname(tempo[["elapsed"]]), nchar(produto)
))
