args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.11.R"
script <- normalizePath(script, mustWork = TRUE)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_relatorios_analiticos_status_mapa_satelite",
  "monitora_relatorios_analiticos_janelas_sentinel2"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (identical(x[[1L]], as.name("<-")) && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos) {
    eval(x, envir = env)
  }
  invisible(lapply(as.list(x), coletar))
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

hoje <- as.Date("2026-08-13")
inicio_missao <- as.Date("2015-06-23")
janelas <- env$monitora_relatorios_analiticos_janelas_sentinel2(
  janela_inicial_dias = 60L,
  hoje = hoje,
  inicio_missao = inicio_missao
)
stopifnot(
  identical(head(janelas$janela_acumulada_dias, 3L), c(60L, 120L, 180L)),
  identical(janelas$data_inicio[[1L]], hoje - 59L),
  identical(tail(janelas$data_inicio, 1L), inicio_missao),
  identical(janelas$data_fim[[1L]], hoje),
  all(janelas$data_inicio <= janelas$data_fim),
  all(janelas$data_fim[-1L] + 1L == janelas$data_inicio[-nrow(janelas)])
)

janelas_180 <- env$monitora_relatorios_analiticos_janelas_sentinel2(
  janela_inicial_dias = 180L,
  hoje = hoje,
  inicio_missao = inicio_missao
)
stopifnot(
  janelas_180$janela_acumulada_dias[[1L]] == 180L,
  identical(tail(janelas_180$data_inicio, 1L), inicio_missao)
)

status <- env$monitora_relatorios_analiticos_status_mapa_satelite(
  solicitado = TRUE,
  provedor = "Sentinel-2",
  motivo = "teste"
)
stopifnot(all(c(
  "inicio_catalogo_consultado", "fim_catalogo_consultado",
  "n_janelas_consultadas", "janelas_consultadas_dias"
) %in% names(status)))

qa <- paste(readLines("tests/qa_relatorios_analiticos_v291.R", warn = FALSE), collapse = "\n")
stopifnot(
  grepl('MONITORA_QA_SENTINEL2", unset = "S"', qa, fixed = TRUE),
  grepl("status_satelite$solicitado", qa, fixed = TRUE),
  grepl("status_limite$localizador_completo", qa, fixed = TRUE)
)

cat("TEST_V2911_SENTINEL_PROGRESSIVO_OK\n")
