#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arquivo <- if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.4.R"

if (!file.exists(arquivo)) {
  stop("Script público v2.9.4 não localizado: ", arquivo, call. = FALSE)
}

invisible(parse(file = arquivo, keep.source = FALSE))
linhas <- readLines(arquivo, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

exigir <- function(condicao, mensagem) {
  if (!isTRUE(condicao)) stop(mensagem, call. = FALSE)
}

exigir(
  grepl('MONITORA_SCRIPT_VERSAO <- "2.9.4"', texto, fixed = TRUE),
  "Identificação pública v2.9.4 ausente."
)
exigir(
  grepl('MONITORA_SCRIPT_BUILD_ID <- "v2.9.4-20260808.1"', texto, fixed = TRUE),
  "Build público v2.9.4 inesperado."
)
exigir(
  !grepl("base::evalq({", texto, fixed = TRUE),
  "O encapsulamento global por evalq reapareceu."
)
exigir(
  grepl('MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "N"', texto, fixed = TRUE),
  "O módulo de relatórios deve permanecer opcional e desligado por padrão."
)
exigir(
  grepl('MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"', texto, fixed = TRUE),
  "O Sentinel-2 deve ser o padrão quando os relatórios forem ativados."
)
exigir(
  grepl(
    "MONITORA_RELATORIOS_ANALITICOS_MAPA_SATELITE <- isTRUE(\n  MONITORA_GERAR_RELATORIOS_ANALITICOS\n) && identical(MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE, \"S\")",
    texto,
    fixed = TRUE
  ),
  "O gate de custo zero do Sentinel quando relatórios estão desligados divergiu."
)
exigir(
  grepl("monitora_relatorios_analiticos_resolver_pandoc <- local({", texto, fixed = TRUE),
  "Resolvedor multiplataforma do Pandoc ausente."
)
exigir(
  grepl("A seleção é determinística e balanceia mudanças direcionais", texto, fixed = TRUE),
  "Conteúdo executivo compacto do relatório sintético ausente."
)
exigir(
  grepl("A tabela apresenta uma seleção editorial determinística", texto, fixed = TRUE),
  "Conteúdo compacto do relatório detalhado ausente."
)

cat("OK: contrato de release v2.9.4 validado em", normalizePath(arquivo), "\n")
