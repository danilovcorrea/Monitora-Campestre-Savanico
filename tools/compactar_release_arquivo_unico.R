#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Uso: Rscript tools/compactar_release_arquivo_unico.R <fonte.R> <destino.R>", call. = FALSE)
}

fonte <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
destino <- args[[2L]]
linhas <- readLines(fonte, warn = FALSE, encoding = "UTF-8")

unico <- function(idx, descricao) {
  if (length(idx) != 1L || is.na(idx)) {
    stop("Marcador não unívoco: ", descricao, call. = FALSE)
  }
  idx
}

inicio_contrato <- unico(
  grep("^monitora_correcao_fread_tsv_embutido <- function", linhas),
  "início do contrato XLSForm embutido"
)
fim_contrato <- unico(
  grep("^monitora_correcao_xlsforms_embutidos_cache_publicacao_ae <- function", linhas),
  "fim do contrato XLSForm embutido"
)
if (fim_contrato <= inicio_contrato) stop("Intervalo inválido do contrato embutido.", call. = FALSE)

ambiente_contrato <- new.env(parent = .GlobalEnv)
eval(parse(text = linhas[inicio_contrato:(fim_contrato - 1L)]), envir = ambiente_contrato)
metadados <- ambiente_contrato$monitora_correcao_xlsforms_embutidos()
metadados_portaveis <- lapply(metadados, function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  out[] <- lapply(out, as.character)
  out
})

serializado <- serialize(metadados_portaveis, NULL, xdr = TRUE, version = 2L)
compactado <- memCompress(serializado, type = "gzip")
hex <- paste(sprintf("%02x", as.integer(compactado)), collapse = "")
hex_partes <- substring(
  hex,
  seq.int(1L, nchar(hex), by = 120L),
  pmin(seq.int(120L, nchar(hex) + 119L, by = 120L), nchar(hex))
)

contrato_compacto <- c(
  "### Contrato XLSForm único embutido e compactado --------------------------",
  "### O payload abaixo preserva byte a byte as tabelas lógicas dos XLSForms",
  "### históricos. A compactação reduz somente o tamanho físico do arquivo",
  "### único; não altera linhas, colunas, valores, tipos lógicos ou precedência.",
  "monitora_correcao_hex_para_raw <- function(hex) {",
  "  n <- nchar(hex, type = \"bytes\")",
  "  if (!length(n) || is.na(n) || n < 2L || (n %% 2L) != 0L || grepl(\"[^0-9a-f]\", hex)) {",
  "    stop(\"Payload hexadecimal do contrato único inválido.\", call. = FALSE)",
  "  }",
  "  ini <- seq.int(1L, n, by = 2L)",
  "  as.raw(strtoi(substring(hex, ini, ini + 1L), base = 16L))",
  "}",
  "",
  "monitora_correcao_xlsforms_embutidos <- function() {",
  "  payload_hex <- paste0(",
  paste0("    \"", hex_partes, "\"", ifelse(seq_along(hex_partes) < length(hex_partes), ",", "")),
  "  )",
  "  objeto <- tryCatch(",
  "    unserialize(memDecompress(monitora_correcao_hex_para_raw(payload_hex), type = \"gzip\")),",
  "    error = function(e) stop(\"Falha ao restaurar o contrato único embutido: \", conditionMessage(e), call. = FALSE)",
  "  )",
  "  esperado <- c(campos = 416L, opcoes = 1349L, dependencias = 465L, arquivos = 4L)",
  "  if (!identical(names(objeto), names(esperado))) {",
  "    stop(\"Contrato único embutido restaurado com componentes inesperados.\", call. = FALSE)",
  "  }",
  "  objeto <- lapply(objeto, data.table::as.data.table)",
  "  observado <- vapply(objeto, nrow, integer(1L))",
  "  if (!identical(unname(observado), unname(esperado))) {",
  "    stop(\"Contrato único embutido restaurado com cardinalidade inesperada.\", call. = FALSE)",
  "  }",
  "  objeto",
  "}",
  ""
)

linhas <- c(
  linhas[seq_len(inicio_contrato - 1L)],
  contrato_compacto,
  linhas[fim_contrato:length(linhas)]
)

substituir_unico <- function(padrao, novo, descricao) {
  idx <- unico(grep(padrao, linhas), descricao)
  linhas[[idx]] <<- novo
}

substituir_unico("^### Versão pública do script:", "### Versão pública do script: 2.9.18", "versão editorial")
substituir_unico("^### Baseline pública de origem:", "### Baseline pública de origem: v2.9.17", "baseline editorial")
substituir_unico(
  "^MONITORA_SCRIPT_VERSAO <-",
  "MONITORA_SCRIPT_VERSAO <- \"2.9.18\"",
  "versão executável"
)
substituir_unico(
  "^MONITORA_SCRIPT_BUILD_ID <-",
  "MONITORA_SCRIPT_BUILD_ID <- \"v2.9.18-20260826-r01\"",
  "build executável"
)

linhas <- sub(
  'ifnotfound = "v2\\.9\\.17-20260825-r01"',
  'ifnotfound = "v2.9.18-20260826-r01"',
  linhas
)
linhas <- sub(
  'ifnotfound = "2\\.9\\.17"',
  'ifnotfound = "2.9.18"',
  linhas
)

idx_hist <- unico(grep("^### A v2\\.9\\.17 conecta", linhas), "histórico v2.9.17")
nota_nova <- c(
  "### A v2.9.18 reduz o tamanho físico do arquivo único para permanecer",
  "### abaixo do limite do editor do RStudio inclusive após conversão para",
  "### finais de linha CRLF no Windows. O contrato XLSForm embutido passa a",
  "### usar serialização portável compactada e validada por componentes e",
  "### cardinalidade. Comentários internos redundantes foram compactados,",
  "### mantendo cabeçalho, instruções operacionais e divisores de seção.",
  "### Nenhuma regra de dados, painel, produto, linhagem ou relatório mudou.",
  "### O bloco funcional de inicialização rápida permanece preservado.",
  "###"
)
linhas <- append(linhas, nota_nova, after = idx_hist - 1L)

# Comentários explicativos internos repetem o histórico completo mantido no
# cabeçalho. Mantemos integralmente o início operacional e todos os divisores
# de seção; a remoção abaixo não altera tokens executáveis do programa.
preservar_inicio_ate <- 900L + length(nota_nova)
eh_comentario_triplo <- grepl("^[[:space:]]*###", linhas)
eh_divisor_secao <- grepl("[-=]{3,}[[:space:]]*$", linhas)
remover <- seq_along(linhas) > preservar_inicio_ate & eh_comentario_triplo & !eh_divisor_secao
linhas <- linhas[!remover]

dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
writeLines(linhas, destino, useBytes = TRUE)

bytes_lf <- file.info(destino)$size
n_linhas <- length(linhas)
bytes_crlf <- bytes_lf + n_linhas
limite <- 5 * 1024^2
if (bytes_crlf >= limite) {
  stop(
    "Candidata ainda excederia 5 MiB em CRLF: ", bytes_crlf,
    " bytes; limite=", limite, call. = FALSE
  )
}

cat(
  "Candidata materializada:\n",
  "  fonte: ", fonte, "\n",
  "  destino: ", normalizePath(destino, winslash = "/", mustWork = TRUE), "\n",
  "  bytes LF: ", bytes_lf, "\n",
  "  linhas: ", n_linhas, "\n",
  "  bytes CRLF simulados: ", bytes_crlf, "\n",
  "  margem sob 5 MiB: ", limite - bytes_crlf, "\n",
  sep = ""
)
