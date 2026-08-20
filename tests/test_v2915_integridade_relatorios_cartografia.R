#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.15.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "monitora_campsav_alvo_global_v2.9.14.R",
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
  "A seção congelada de inicialização do RStudio divergiu da v2.9.14."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.15"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.15-20260820"',
  "monitora_cartografia_http_texto <- function",
  "monitora_relatorios_analiticos_obter_estados_ibge <- function",
  "monitora_relatorios_analiticos_obter_biomas_ibge <- function",
  'autoridade_estados = "IBGE"',
  'autoridade_biomas = "IBGE"',
  'autoridade_limite_uc = "ICMBio"',
  "sha256_arquivo_temporario"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Revisão ausente: ", trecho))

### O catálogo de biomas não pode voltar a ser decodificado à força como
### UTF-8. Essa foi a causa comprovada da seleção silenciosa da base 1:5 M.
bloco_biomas <- linhas[
  grep("^monitora_relatorios_analiticos_obter_biomas_ibge <- function", linhas)[1L]:
    (grep("^monitora_relatorios_analiticos_status_limite_uc <- function", linhas)[1L] - 1L)
]
exigir(
  !grepl('httr::content\\(req_indice\\$resposta, as = "text", encoding = "UTF-8"',
         paste(bloco_biomas, collapse = "\n")),
  "O índice de biomas voltou a forçar UTF-8."
)

alvos <- c(
  "monitora_plot_preparar_rotulos_proporcao_obrigatorios",
  "monitora_plot_camadas_rotulos_proporcao_obrigatorios",
  "monitora_plot_calcular_limites_x_proporcao_rotulos",
  "monitora_plot_coord_cartesian",
  "monitora_plot_coord_x_proporcao_obrigatorios",
  "monitora_cartografia_extrair_links",
  "monitora_cartografia_ordenar_links_biomas",
  "monitora_cartografia_anos_malha_ibge",
  "monitora_relatorios_analiticos_status_limite_uc"
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

env$MONITORA_FONTE_ROTULO_PROP <- 2.8
env$MONITORA_LINEHEIGHT_ROTULO <- 0.92

### Regressão APAI: o subconjunto de uma formação ausente tem zero linhas,
### mas deve conservar integralmente o schema consumido pelas camadas ggplot.
vazio <- data.table(
  ANO = integer(), form_veg = character(), prop = numeric(), n = integer(),
  categoria = character(), categoria_label = character()
)
preparado <- env$monitora_plot_preparar_rotulos_proporcao_obrigatorios(vazio)
schema_camadas <- c(
  "prop_num_rotulo_obrig", "rotulo_prop_interno", "rotulo_prop_externo",
  "ANO_factor_rotulo", "ANO_label_rotulo", "x_meio_plot",
  "x_conector_rotulo", "x_alvo_rotulo", "y_base_rotulo",
  "y_alvo_rotulo", "hjust_rotulo", "usar_cotovelo_rotulo"
)
exigir(nrow(preparado) == 0L && all(schema_camadas %in% names(preparado)),
       "Subconjunto vazio não preservou o schema integral das camadas.")
camadas <- tryCatch(
  env$monitora_plot_camadas_rotulos_proporcao_obrigatorios(preparado),
  error = function(e) e
)
exigir(!inherits(camadas, "error") && is.list(camadas),
       paste0("Camadas do subconjunto vazio ainda falham: ",
              if (inherits(camadas, "error")) conditionMessage(camadas) else "resultado inválido"))

links <- c(
  "Biomas_5000mil.zip",
  "Biomas_250mil.zip",
  "2025_Biomas-e-Sistema-Costeiro-Marinho-do-Brasil-1-250000_shp.zip",
  "2019_Biomas-e-Sistema-Costeiro-Marinho-do-Brasil-1-250000_shp.zip"
)
ordenados <- env$monitora_cartografia_ordenar_links_biomas(links)
exigir(startsWith(basename(ordenados[[1L]]), "2025_"),
       "A edição 2025 de biomas não recebeu prioridade.")
exigir(grepl("5000mil", tail(ordenados, 1L), ignore.case = TRUE),
       "A base 1:5 M não ficou restrita ao último fallback.")

html_anos <- paste0(
  '<a href="municipio_2023/">2023</a>',
  '<a href="municipio_2025/">2025</a>',
  '<a href="municipio_2024/">2024</a>'
)
exigir(identical(env$monitora_cartografia_anos_malha_ibge(html_anos), 2025:2023),
       "Ordenação das edições anuais da malha estadual está incorreta.")

status <- env$monitora_relatorios_analiticos_status_limite_uc(TRUE, "teste")
campos_auditoria <- c(
  "autoridade_limite_uc", "produto_limite_uc", "data_atualizacao_limite_uc",
  "sha256_limite_uc", "fallback_limite_uc", "autoridade_estados",
  "versao_estados", "escala_estados", "sha256_estados", "fallback_estados",
  "autoridade_biomas", "versao_biomas", "escala_biomas", "sha256_biomas",
  "fallback_biomas"
)
exigir(all(campos_auditoria %in% names(status)),
       "Auditoria cartográfica não contém todos os metadados de proveniência.")

cat("TEST_V2915_INTEGRIDADE_RELATORIOS_CARTOGRAFIA_OK\n")
