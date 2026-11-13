#!/usr/bin/env Rscript
# Replay documental em cópia privada: estatísticas prontas; nenhuma nova curadoria.
# Fogo, clima e imagem orbital fora do escopo deste teste de paginação.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
candidate <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
sources <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
target <- args[[3L]]
stopifnot(!dir.exists(target))
dir.create(target, recursive = TRUE)
source("tools/v308/carregar.R")
e <- monitora_v308_carregar(candidate)
# Constantes literais reais (fontes/limiares/cores), sem executar inicialização.
safe <- function(x) {
  if (is.null(x) || is.atomic(x)) return(TRUE)
  if (is.symbol(x)) return(exists(as.character(x), e, inherits = TRUE))
  pure <- c("c", "list", "data.table", "integer", "numeric", "character", "logical", "+", "-", "*", "/", "Sys.getenv", "Sys.time", "tolower", "toupper", "trimws", "nzchar",
    "as.character", "as.numeric", "as.integer", "is.finite", "isTRUE", "identical", "%in%", "&&", "||", "!",
    "paste", "paste0", "unlist", "strsplit", "unique", "sort", "getOption", "new.env", "emptyenv", "globalenv")
  if (!is.call(x) || !is.symbol(x[[1L]]) || !as.character(x[[1L]]) %in% pure) return(FALSE)
  all(vapply(as.list(x)[-1L], safe, logical(1L)))
}
constants <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (identical(x[[1L]], as.name("function")) || identical(x[[1L]], as.name("quote"))) return(invisible(NULL))
  if (identical(x[[1L]], as.name("<-")) && is.symbol(x[[2L]])) {
    if (startsWith(as.character(x[[2L]]), "MONITORA_") && safe(x[[3L]])) eval(x, e)
    return(invisible(NULL))
  }
  if (length(x) > 1L) for (i in 2:length(x)) constants(x[[i]])
  invisible(NULL)
}
for (node in parse(candidate, encoding = "UTF-8", keep.source = FALSE)) constants(node)
e$MONITORA_BASE_DIR <- normalizePath(target, winslash = "/", mustWork = TRUE)
e$MONITORA_OUTPUT_DIR <- file.path(e$MONITORA_BASE_DIR, "output")
e$MONITORA_LOG_DIR <- file.path(e$MONITORA_BASE_DIR, "log")
dir.create(e$MONITORA_OUTPUT_DIR); dir.create(e$MONITORA_LOG_DIR)
e$MONITORA_EXEC_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
e$MONITORA_LOG_EXECUCAO <- data.table(data_hora = character(), etapa = character(), severidade = character(), arquivo = character(), detalhe = character(), acao = character())
e$MONITORA_OPCAO_ANALISE_FOGO <- "N"
e$MONITORA_OPCAO_ANALISE_CLIMA <- "N"
e$MONITORA_VALIDACAO_ESPACIAL_POS_PAINEL_RESULTADO <- NULL
e$MONITORA_PROGRESSO_HABILITADO <- FALSE
files <- sort(list.files(sources, full.names = TRUE, recursive = TRUE))
before <- tools::md5sum(files)
read_stat <- function(name) fread(file.path(sources, "output/05_estatisticas", name), showProgress = FALSE)
call_args <- list(
  registros = fread(file.path(sources, "input/registros_corrig.csv"), colClasses = "character", showProgress = FALSE),
  stat = fread(file.path(sources, "output/01_produtos_dados/registros_corrig_stat.csv"), showProgress = FALSE),
  mudanca_periodo = read_stat("estatistica_pareada_periodo_editorial.csv"),
  mudanca_linha_base = read_stat("estatisticas_mudanca_linha_base.csv"),
  composicao_periodo = read_stat("estatisticas_composicao_geral_ano_a_ano.csv"),
  composicao_linha_base = read_stat("estatisticas_composicao_linha_base.csv"),
  config_stat = read_stat("estatisticas_mudanca_config.csv"),
  cob_categ = read_stat("cob_veg_categ.csv"), cob_nat = read_stat("cob_veg_form_vida_nat.csv"),
  cob_exot = read_stat("cob_veg_form_vida_exot.csv"),
  prop_herb_lenh = read_stat("prop_rel_herb_lenh.csv"), cob_herb_lenh = read_stat("cob_veg_herb_lenh.csv"),
  prop_categ = read_stat("prop_rel_categ.csv"), prop_nat = read_stat("prop_rel_form_vida_nat.csv"),
  prop_exot = read_stat("prop_rel_form_vida_exot.csv"), prop_seca = read_stat("prop_rel_form_vida_seca_morta.csv"),
  cob_seca = read_stat("cob_veg_form_vida_seca_morta.csv"),
  prop_material = read_stat("prop_rel_material_botanico.csv"), cob_material = read_stat("cob_veg_material_botanico.csv"),
  output_dir = e$MONITORA_OUTPUT_DIR, formatos = c("rmd", "md", "html", "docx", "pdf"),
  mapa_satelite = FALSE, status_validacao = "CONSULTAR AUDITORIAS"
)
start <- proc.time()[["elapsed"]]
failure <- NULL
warnings_observed <- list()
tryCatch({
  result <- withCallingHandlers(do.call(e$monitora_relatorios_analiticos_gerar, call_args), warning = function(w) {
    warnings_observed[[length(warnings_observed) + 1L]] <<- list(message = conditionMessage(w),
      call = paste(deparse(conditionCall(w)), collapse = " "))
    invokeRestart("muffleWarning")
  })
  saveRDS(result, file.path(target, "resultado.rds"))
}, error = function(err) {
  failure <<- conditionMessage(err)
  cat("ERRO_REPLAY_PNB:", failure, "\n")
})
preserved <- identical(before, tools::md5sum(files))
jsonlite::write_json(list(candidate = basename(candidate), sha256 = digest::digest(file = candidate, algo = "sha256", serialize = FALSE),
  R = R.version.string, ggplot2 = as.character(packageVersion("ggplot2")),
  scope = "Replay documental PNB com tabelas prontas; fogo/clima/orbital desligados; sem rodada de curadoria ou recálculo do pipeline estatístico",
  full_pipeline = FALSE, manual_rstudio = FALSE, source_preserved = preserved,
  elapsed_seconds = proc.time()[["elapsed"]] - start, error = failure,
  warnings = warnings_observed,
  ok = is.null(failure) && preserved), file.path(target, "RESULTADO_REPLAY.json"), auto_unbox = TRUE, pretty = TRUE)
stopifnot(preserved)
if (!is.null(failure)) stop(failure, call. = FALSE)
cat("PASS REPLAY DOCUMENTAL PNB\n")
