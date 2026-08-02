#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(ggplot2)
  library(knitr)
  library(rmarkdown)
})

args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.1.R"
fonte <- if (length(args) >= 2L) args[[2L]] else
  Sys.getenv("MONITORA_QA_OUTPUT_FONTE", unset = "")

assert <- function(condicao, mensagem) {
  if (!isTRUE(condicao)) stop(mensagem, call. = FALSE)
}
assert(file.exists(script), "Script candidato ausente.")
assert(dir.exists(fonte), "Output-fonte ausente.")

arvore <- parse(file = script, keep.source = FALSE)
funcoes <- new.env(parent = globalenv())
coletar <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]]) &&
      is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")) {
    nome <- as.character(expr[[2L]])
    if (startsWith(nome, "monitora_relatorios_analiticos_") ||
        nome %in% c(
          "monitora_relatorio_rotulo_formacao",
          "monitora_relatorio_rotulo_grupo",
          "monitora_relatorio_rotulo_metrica",
          "monitora_relatorio_classe_portugues"
        )) eval(expr, envir = funcoes)
    return(invisible(NULL))
  }
  if (length(expr) > 1L) for (ii in seq_along(expr)[-1L]) coletar(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
list2env(as.list.environment(funcoes, all.names = TRUE), envir = .GlobalEnv)

necessarias <- c(
  "monitora_relatorios_analiticos_conteudo_docx",
  "monitora_relatorios_analiticos_referencia_docx_materializar",
  "monitora_relatorios_analiticos_mapas",
  "monitora_relatorios_analiticos_esforco"
)
assert(all(vapply(necessarias, exists, logical(1L))), "Funções focais ausentes.")

qa_persistente <- if (length(args) >= 3L) trimws(args[[3L]]) else ""
qa <- if (nzchar(qa_persistente)) {
  qa_persistente
} else {
  tempfile("qa_v291_docx_cartografia_")
}
dir.create(qa, recursive = TRUE)
if (!nzchar(qa_persistente)) {
  on.exit(unlink(qa, recursive = TRUE, force = TRUE), add = TRUE)
}

referencia <- file.path(qa, "referencia.docx")
monitora_relatorios_analiticos_referencia_docx_materializar(referencia)
assert(
  identical(
    digest(file = referencia, algo = "sha256"),
    monitora_relatorios_analiticos_referencia_docx_sha256()
  ),
  "Referência DOCX embutida divergiu."
)

dir_rel_fonte <- file.path(
  fonte,
  "08_relatorios_analiticos",
  "floresta_nacional_de_contendas_do_sincora"
)
rmd_fonte <- list.files(
  dir_rel_fonte,
  pattern = "relatorio_analitico_sintetico_.*\\.Rmd$",
  full.names = TRUE
)[1L]
assert(file.exists(rmd_fonte), "Rmd sintético de referência ausente.")
dir_docx <- file.path(qa, "docx")
dir.create(dir_docx)
file.copy(file.path(dir_rel_fonte, "figuras"), dir_docx, recursive = TRUE)
conteudo <- readLines(rmd_fonte, warn = FALSE, encoding = "UTF-8")
conteudo <- monitora_relatorios_analiticos_conteudo_docx(conteudo)
rmd_docx <- file.path(dir_docx, "relatorio.Rmd")
writeLines(conteudo, rmd_docx, useBytes = TRUE)
docx <- file.path(dir_docx, "relatorio.docx")
rmarkdown::render(
  rmd_docx,
  output_format = rmarkdown::word_document(
    toc = TRUE,
    toc_depth = 3L,
    reference_docx = referencia,
    pandoc_args = "--wrap=none"
  ),
  output_file = basename(docx),
  output_dir = dir_docx,
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
assert(file.exists(docx) && file.info(docx)$size > 100000L, "DOCX focal ausente ou pequeno.")
midias <- utils::unzip(docx, list = TRUE)$Name
assert(sum(grepl("^word/media/", midias)) >= 4L, "Figuras não foram incorporadas ao DOCX.")
texto_docx <- file.path(dir_docx, "relatorio.txt")
rmarkdown::pandoc_convert(docx, to = "plain", output = texto_docx)
texto_docx <- paste(readLines(texto_docx, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert(
  grepl("Resumo executivo", texto_docx, ignore.case = TRUE),
  "Conteúdo textual ausente no DOCX focal."
)

registros <- fread(
  file.path(fonte, "01_produtos_dados", "registros_corrig.csv"),
  encoding = "UTF-8",
  showProgress = FALSE
)
stat <- fread(
  file.path(fonte, "01_produtos_dados", "registros_corrig_stat.csv"),
  encoding = "UTF-8",
  showProgress = FALSE
)
esforco <- monitora_relatorios_analiticos_esforco(registros, stat)
dir_mapas <- file.path(qa, "mapas")
dir.create(dir_mapas)
mapas <- monitora_relatorios_analiticos_mapas(
  stat,
  esforco$continuidade,
  dir_mapas,
  file.path(qa, "cache"),
  mapa_satelite = FALSE
)
assert(
  file.exists(mapas$mapa_resumo) && file.info(mapas$mapa_resumo)$size > 100000L &&
    file.exists(mapas$mapa_paineis) && file.info(mapas$mapa_paineis)$size > 100000L,
  "Mapas vetoriais profissionais não foram materializados."
)

cat(
  "QA_V291_DOCX_CARTOGRAFIA_FOCAL_OK\n",
  "DOCX=", normalizePath(docx, winslash = "/"), "\n",
  "MAPA_RESUMO=", normalizePath(mapas$mapa_resumo, winslash = "/"), "\n",
  "MAPA_PAINEIS=", normalizePath(mapas$mapa_paineis, winslash = "/"), "\n",
  sep = ""
)
