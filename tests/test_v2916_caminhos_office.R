#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
})

exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
script <- normalizePath(
  if (length(commandArgs(trailingOnly = TRUE))) {
    commandArgs(trailingOnly = TRUE)[[1L]]
  } else {
    "monitora_campsav_alvo_global.R"
  },
  winslash = "/",
  mustWork = TRUE
)
arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_relatorios_analiticos_destino_fisico"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  if (length(x) > 1L) for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(
  exists(alvos[[1L]], envir = env, inherits = FALSE),
  "Seletor de caminho físico dos relatórios não foi carregado."
)

uc <- "Reserva Biológica do Guaporé"
slug <- "reserva_biologica_do_guapore"
curto <- env$monitora_relatorios_analiticos_destino_fisico(
  output_dir = tempfile("saida_curta_"),
  uc = uc,
  uc_slug = slug,
  periodo = "2019-2025"
)
exigir(!curto$compactado, "Caminho curto foi compactado sem necessidade.")
exigir(
  identical(curto$base_det, paste0(
    "relatorio_analitico_detalhado_", slug, "_2019-2025"
  )),
  "Nome editorial deixou de ser preservado quando o caminho é seguro."
)

raiz_longa <- file.path(
  tempfile("raiz_"),
  paste(rep("segmento_caminho_windows", 4L), collapse = "_")
)
longo_1 <- env$monitora_relatorios_analiticos_destino_fisico(
  raiz_longa, uc, slug, "2019-2025"
)
longo_2 <- env$monitora_relatorios_analiticos_destino_fisico(
  raiz_longa, uc, slug, "2019-2025"
)
exigir(longo_1$compactado, "Caminho longo não acionou compactação.")
exigir(
  identical(longo_1, longo_2),
  "Compactação de caminho não é determinística."
)
exigir(
  grepl("^uc-[a-f0-9]{10}$", longo_1$diretorio_id) &&
    identical(longo_1$base_sint, "analitico_sintetico") &&
    identical(longo_1$base_det, "analitico_detalhado"),
  "Identificadores físicos compactos não seguem o contrato."
)
exigir(
  max(nchar(file.path(
    longo_1$diretorio,
    paste0(c(longo_1$base_sint, longo_1$base_det), ".docx")
  ))) <= 240L,
  "Mesmo compactado, o relatório excederia o orçamento de 240 caracteres."
)

texto <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (trecho in c(
  "caminhos_logicos, type = \"chars\") > 210L",
  '"sis_", contexto_id, ".xlsx"',
  '"s_", contexto_hash, ".xlsx"',
  "produto_logico = contextos$nome_arquivo_logico[ii]",
  'base_relatorio_fisica <- "validacao_consolidado"',
  'metadados_caminho_relatorio.csv',
  "limite_recomendado_windows",
  "situacao_caminho_office"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Proteção ausente: ", trecho))

cat("TEST_V2916_CAMINHOS_OFFICE_OK\n")
