#!/usr/bin/env Rscript
# Comparação integral das funções carregadas e dos contratos com a release pública.
args <- commandArgs(TRUE)
candidate <- if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v3.0.8.R"
baseline <- "monitora_campsav_alvo_global_v3.0.7.R"
source("tools/v308/carregar.R")
a <- monitora_v308_carregar(baseline)
b <- monitora_v308_carregar(candidate)
checks <- 0L
check <- function(label, ok) {
  if (!isTRUE(ok)) stop("FALHOU: ", label, call. = FALSE)
  checks <<- checks + 1L
  cat("OK:", label, "\n")
}
check("versão explícita e baseline pública preservada", grepl("^3[.]0[.]8(-dev_r[0-9]+)?$", b$MONITORA_SCRIPT_VERSAO) &&
  identical(basename(candidate), paste0("monitora_campsav_alvo_global_v", b$MONITORA_SCRIPT_VERSAO, ".R")) &&
  identical(a$MONITORA_SCRIPT_VERSAO, "3.0.7") && trimws(readLines("VERSION")) %in% c("3.0.7", "3.0.8"))
names_a <- ls(a, all.names = TRUE)
functions <- names_a[vapply(names_a, function(nm) is.function(a[[nm]]), logical(1L))]
check("nenhuma função produtiva removida", all(functions %in% ls(b, all.names = TRUE)))
changed <- functions[!vapply(functions, function(nm)
  identical(formals(a[[nm]]), formals(b[[nm]])) && identical(body(a[[nm]]), body(b[[nm]])), logical(1L))]
allowed <- c("monitora_v305_figura_evidencia", "monitora_relatorios_analiticos_renderizar", "monitora_relatorios_analiticos_gerar", "monitora_output_reescrever_referencias")
print(changed)
check("alterações limitadas à paginação e integração documental", setequal(changed, allowed))
check("contrato XLSForm/129 campos idêntico", identical(a$monitora_validados_schema_embutido(), b$monitora_validados_schema_embutido()))
freeze <- function(file) {
  s <- readLines(file, encoding = "UTF-8", warn = FALSE)
  start <- match("base::evalq({", s)
  end <- grep("^### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI(?:[[:space:]-]*)$", s, perl = TRUE)
  stopifnot(length(end) == 1L, !is.na(start), end > start)
  x <- s[start:(end - 1L)]
  x <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', x)
  sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', x)
}
check("inicialização congelada literal preservada, salvo identificadores da versão", identical(freeze(baseline), freeze(candidate)))
ar <- parse(candidate, encoding = "UTF-8", keep.source = FALSE)
check("uma expressão externa base::evalq", length(ar) == 1L && identical(ar[[1L]][[1L]], quote(base::evalq)))
is_def <- function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
  identical(x[[2L]], as.name("monitora_relatorios_analiticos_gerar"))
paths <- function(tree, predicate) {
  found <- character()
  visit <- function(x, path) {
    if (!is.call(x)) return(invisible(NULL))
    if (predicate(x)) found <<- c(found, path)
    if (length(x) > 1L) for (i in 2:length(x)) visit(x[[i]], paste0(path, "/", i))
    invisible(NULL)
  }
  visit(tree[[1L]], "1")
  found
}
old_path <- paths(parse(baseline, encoding = "UTF-8", keep.source = FALSE), is_def)
new_path <- paths(ar, function(x) identical(x,
  quote(base::eval(MONITORA_V308_DEFINICAO_RELATORIO, envir = environment()))))
check("definição compactada avaliada uma vez, na posição original", length(old_path) == 1L && identical(old_path, new_path))
q <- b$MONITORA_V308_DEFINICAO_RELATORIO
probe <- new.env(parent = baseenv())
eval(q, probe)
check("quote contém exclusivamente a definição e preserva AST/ambiente", length(q) == 2L &&
  identical(formals(probe$monitora_relatorios_analiticos_gerar), formals(b$monitora_relatorios_analiticos_gerar)) &&
  identical(body(probe$monitora_relatorios_analiticos_gerar), body(b$monitora_relatorios_analiticos_gerar)) &&
  identical(environment(probe$monitora_relatorios_analiticos_gerar), probe) &&
  identical(environment(b$monitora_relatorios_analiticos_gerar), b))
raw <- readBin(candidate, "raw", n = file.info(candidate)$size)
lf <- length(raw); crlf <- lf + sum(raw == as.raw(10L))
check("teto decimal de 5.000.000 bytes em LF e CRLF", max(lf, crlf) <= 5000000L)
if (identical(b$MONITORA_SCRIPT_VERSAO, "3.0.8")) {
  expected_sha <- jsonlite::read_json("docs/VALIDACAO_PUBLICACAO_v308.json", simplifyVector = TRUE)$public_sha256
} else {
  revision <- sub("^.*-dev_", "", b$MONITORA_SCRIPT_VERSAO)
  expected_sha <- jsonlite::read_json(file.path("tools/v308", paste0("BUILD_", revision, ".json")), simplifyVector = TRUE)$sha256
}
check("SHA256 igual ao recibo da versão", identical(expected_sha,
  digest::digest(file = candidate, algo = "sha256", serialize = FALSE)))
cat(sprintf("PASS v308 contrato: %d gates; %d funções comparadas; LF=%d, CRLF=%d.\n", checks, length(functions), lf, crlf))
