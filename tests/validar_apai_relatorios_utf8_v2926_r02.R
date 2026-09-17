#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
raiz <- normalizePath(args[[1L]], mustWork = TRUE)
dirs <- list.dirs(file.path(raiz, "08_relatorios_analiticos"), recursive = FALSE, full.names = TRUE)
if (length(dirs) != 1L) stop("Diretório analítico da UC não é unívoco.", call. = FALSE)
dir_rel <- dirs[[1L]]
esperados <- c(
  unlist(lapply(c("sintetico", "detalhado"), function(tipo) {
    unlist(lapply(c("Rmd", "md", "html"), function(ext) {
      list.files(dir_rel, pattern = paste0("relatorio_analitico_", tipo, ".*[.]", ext, "$"), full.names = TRUE)
    }), use.names = FALSE)
  }), use.names = FALSE),
  file.path(dir_rel, c("manifesto_relatorios_analiticos.csv", "indice_relatorios_analiticos.csv"))
)
if (length(esperados) != 8L || any(!file.exists(esperados)) || any(file.info(esperados)$size <= 0L)) {
  stop("Os seis documentos e dois índices esperados não foram materializados.", call. = FALSE)
}
textos <- list.files(dir_rel, pattern = "[.](csv|txt|md|Rmd|html|json)$", recursive = TRUE, full.names = TRUE)
invalidos <- textos[!vapply(textos, function(arq) {
  bruto <- readBin(arq, "raw", n = file.info(arq)$size)
  validUTF8(rawToChar(bruto))
}, logical(1L))]
if (length(invalidos)) stop("Produtos textuais inválidos em UTF-8: ", paste(basename(invalidos), collapse = " | "), call. = FALSE)
detalhado <- list.files(dir_rel, pattern = "relatorio_analitico_detalhado.*[.]md$", full.names = TRUE)
conteudo <- paste(readLines(detalhado, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (length(detalhado) != 1L || !grepl("Área de Proteção Ambiental Ibirapuitã", conteudo, fixed = TRUE)) {
  stop("O nome acentuado da UC não foi preservado no relatório detalhado.", call. = FALSE)
}
cat(sprintf(
  "VALIDAR_APAI_RELATORIOS_UTF8_V2926_R02_OK; arquivos_textuais=%d; documentos=%d; diretorio=%s\n",
  length(textos), length(esperados) - 2L, normalizePath(dir_rel, winslash = "/", mustWork = TRUE)
))
