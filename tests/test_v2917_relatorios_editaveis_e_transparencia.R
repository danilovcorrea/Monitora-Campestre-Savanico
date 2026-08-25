#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(rmarkdown)
  library(knitr)
  library(jsonlite)
  library(digest)
  library(xml2)
  library(zip)
})

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.16.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

linhas <- readLines(candidato, warn = FALSE, encoding = "UTF-8")
base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
arvore <- parse(file = candidato, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)

inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio_congelado(linhas), inicio_congelado(base)),
  "A seção congelada de inicialização do RStudio foi alterada."
)

for (trecho in c(
  'MONITORA_FORMATOS_RELATORIO_VALIDACAO <- c("html", "docx", "pdf")',
  'c("rmd", "md", "html", "docx", "pdf")',
  "monitora_doc_render_editaveis <- function",
  "monitora_doc_validacao_conteudo_editavel <- function",
  "monitora_doc_validacao_auditar_docx <- function",
  "painel fixo iniciado em",
  "A direção foi confrontada e corroborada em"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Revisão ausente: ", trecho))

alvos <- c(
  "monitora_doc_rmd_setup_editavel",
  "monitora_doc_validacao_conteudo_editavel",
  "monitora_doc_validacao_publicar",
  "monitora_doc_validacao_auditar_md",
  "monitora_doc_validacao_auditar_docx",
  "monitora_doc_validacao_adequar_layout_docx",
  "monitora_doc_render_editaveis",
  "monitora_relatorios_analiticos_referencia_docx_sha256",
  "monitora_relatorios_analiticos_referencia_docx_base64",
  "monitora_relatorios_analiticos_referencia_docx_materializar",
  "monitora_relatorios_analiticos_coluna_contextual_mesclavel",
  "monitora_relatorios_analiticos_docx_preservar_linhas_tabela",
  "monitora_relatorios_analiticos_fmt_p",
  "monitora_relatorios_analiticos_fmt_p_frase"
)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)),
       "Nem todas as funções focais foram extraídas.")
list2env(as.list.environment(env, all.names = TRUE), envir = .GlobalEnv)

exigir(
  identical(monitora_relatorios_analiticos_fmt_p_frase(0.0004), "valor de p ajustado < 0,001"),
  "A desigualdade do valor de p ainda recebe sinal de igualdade indevido."
)
exigir(
  identical(monitora_relatorios_analiticos_fmt_p_frase(0.024), "valor de p ajustado = 0,024"),
  "A redação do valor de p numérico divergiu."
)

preservar_qa <- identical(Sys.getenv("MONITORA_QA_PRESERVAR", unset = "N"), "S")
dir_qa <- if (isTRUE(preservar_qa)) {
  file.path("artifacts", "qa_v2917_validacao_editavel_debug")
} else {
  tempfile("qa_v2917_validacao_editavel_")
}
if (dir.exists(dir_qa) && isTRUE(preservar_qa)) unlink(dir_qa, recursive = TRUE, force = TRUE)
dir.create(file.path(dir_qa, "dados_apoio"), recursive = TRUE, showWarnings = FALSE)
if (!isTRUE(preservar_qa)) {
  on.exit(unlink(dir_qa, recursive = TRUE, force = TRUE), add = TRUE)
} else {
  cat("QA_DIR=", dir_qa, "\n", sep = "")
}
fwrite(data.table(
  Informação = c("Versão", "Situação"),
  Valor = c("2.9.17-dev", "validado")
), file.path(dir_qa, "dados_apoio", "identificacao.csv"), bom = TRUE)

conteudo <- c(
  "---",
  "title: 'Relatório antigo'",
  "output:",
  "  html_document: default",
  "---",
  "",
  "```{r setup-documental, echo=FALSE}",
  "objeto_antigo <- TRUE",
  "```",
  "<style>",
  "body{color:#000}",
  "</style>",
  "<div class='monitora-capa'><div class='kicker'>Programa Monitora · CBC/ICMBio</div><h1>Relatório de validação</h1><p>Registro auditável.</p></div>",
  "",
  "# Resumo executivo",
  "",
  "**Situação dos dados:** validado.",
  "",
  "<div class='monitora-resumo'>",
  "<div class='monitora-card'><strong>Situação</strong><br>validado</div>",
  "<div class='monitora-card'><strong>Entrada</strong><br>2 arquivos</div>",
  "</div>",
  "",
  "<div class='monitora-alerta'><strong>Como interpretar:</strong> conteúdo auditável.</div>",
  "",
  "## Identificação",
  "",
  "```{r rel-meta, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}",
  ".monitora_doc_print_table('dados_apoio/identificacao.csv', n=30, cols=c('Informação','Valor'), largura=72)",
  "```",
  "",
  "Consulte [os dados de apoio](dados_apoio/identificacao.csv)."
)

inicio <- proc.time()[["elapsed"]]
gerados <- monitora_doc_render_editaveis(
  conteudo = conteudo,
  base_dir = dir_qa,
  base_nome = "validacao_teste",
  formatos = c("md", "docx"),
  versao_script = "2.9.17-dev",
  build_script = "qa-v2917",
  exec_id = "20260824_qa",
  responsavel = "Equipe de teste",
  instituicao = "ICMBio",
  log_dir = dir_qa
)
duracao <- proc.time()[["elapsed"]] - inicio

md <- file.path(dir_qa, "validacao_teste.md")
docx <- file.path(dir_qa, "validacao_teste.docx")
exigir(all(file.exists(c(md, docx))), "Markdown ou DOCX não foi materializado.")
exigir(all(c(md, docx) %in% gerados), "A função não declarou ambos os formatos gerados.")
exigir(file.info(docx)$size > 10000L, "DOCX de teste é pequeno demais para ser válido.")

md_txt <- readLines(md, warn = FALSE, encoding = "UTF-8")
exigir(!any(grepl("^```\\{r", md_txt)), "Markdown final ainda contém chunk de R.")
exigir(!any(grepl("<style|monitora-card|monitora-capa", md_txt)),
       "Markdown final ainda contém CSS/layout HTML.")
exigir(any(grepl("^\\|[[:space:]]*Informação[[:space:]]*\\|[[:space:]]*Valor", md_txt, perl = TRUE)),
       "Tabela não foi materializada no Markdown.")

aud_md <- fread(file.path(dir_qa, "auditoria_integridade_markdown_relatorio_validacao.csv"))
aud_docx <- fread(file.path(dir_qa, "auditoria_integridade_docx_relatorio_validacao.csv"))
exigir(all(aud_md$conforme), "Auditoria do Markdown registrou não conformidade.")
exigir(all(aud_docx$conforme), "Auditoria do DOCX registrou não conformidade.")

cat(sprintf("TEST_V2917_RELATORIOS_EDITAVEIS_E_TRANSPARENCIA_OK duracao=%.3fs\n", duracao))
