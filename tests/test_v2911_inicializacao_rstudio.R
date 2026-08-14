#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args) >= 1L) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.11.R",
  mustWork = TRUE
)
referencia_v291 <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else
    "releases/v2.9.1/monitora_campsav_alvo_global_v2.9.1.R",
  mustWork = TRUE
)
simular_source_com_eco <- function(arquivo) {
  tempo_parse <- system.time({
    original <- parse(file = arquivo, keep.source = TRUE)
  })[["elapsed"]]
  inofensivo <- as.expression(rep(
    list(quote(invisible(NULL))),
    length(original)
  ))
  attributes(inofensivo) <- attributes(original)
  saida <- tempfile(fileext = ".txt")
  sink(saida)
  on.exit({
    while (sink.number() > 0L) sink()
    unlink(saida)
  }, add = TRUE)
  tempo_eco <- system.time(source(
    exprs = inofensivo,
    echo = TRUE,
    max.deparse.length = 150L
  ))[["elapsed"]]
  sink()
  bytes_eco <- unname(file.info(saida)$size)
  unlink(saida)
  list(
    expressoes_externas = length(original),
    parse_s = unname(tempo_parse),
    eco_s = unname(tempo_eco),
    eco_bytes = bytes_eco
  )
}

resultado <- list(
  v291 = simular_source_com_eco(referencia_v291),
  v2911 = simular_source_com_eco(candidato)
)

stopifnot(
  resultado$v291$expressoes_externas == 1L,
  resultado$v2911$expressoes_externas == 1L,
  resultado$v291$eco_bytes <= 512,
  resultado$v2911$eco_bytes <= 512,
  identical(resultado$v291$eco_bytes, resultado$v2911$eco_bytes)
)

for (nome in names(resultado)) {
  x <- resultado[[nome]]
  cat(sprintf(
    "%s: expressoes=%d; parse=%.3fs; source_echo_simulado=%.3fs; eco=%d bytes\n",
    nome,
    x$expressoes_externas,
    x$parse_s,
    x$eco_s,
    x$eco_bytes
  ))
}
cat("TEST_V2911_INICIALIZACAO_RSTUDIO_OK\n")
