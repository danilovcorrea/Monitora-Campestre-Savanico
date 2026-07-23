args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Uso: Rscript tests/qa_kml_v280.R SCRIPT REGISTROS_CORRIG_STAT OUTPUT_DIR", call. = FALSE)
}

script_path <- normalizePath(args[[1L]], mustWork = TRUE)
csv_path <- normalizePath(args[[2L]], mustWork = TRUE)
output_dir <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)

suppressPackageStartupMessages(library(data.table))

MONITORA_EXPORTAR_KML <- TRUE
MONITORA_MAX_UAS_KML_AUTO <- 10000L
MONITORA_OUTPUT_DIR <- output_dir
registros_corrig_stat <- data.table::fread(csv_path, encoding = "UTF-8")

monitora_dt_referenciar <- function(x) data.table::copy(data.table::as.data.table(x))
monitora_perf_registrar_checkpoint <- function(...) invisible(NULL)
monitora_log_registrar_evento <- function(etapa, nivel, arquivo, mensagem, detalhe = "") {
  message("[", nivel, "] ", etapa, ": ", mensagem)
  invisible(NULL)
}
monitora_fwrite <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x, path, ...)
  invisible(path)
}
monitora_rm_seguro <- function(...) {
  objetos <- unique(as.character(unlist(list(...), use.names = FALSE)))
  objetos <- intersect(objetos, ls(envir = .GlobalEnv, all.names = TRUE))
  if (length(objetos)) rm(list = objetos, envir = .GlobalEnv)
  invisible(NULL)
}
monitora_recurso_gc <- function(...) invisible(gc(verbose = FALSE))
if (identical(Sys.getenv("QA_KML_FORCAR_FALLBACK_ZIP", unset = "0"), "1")) {
  requireNamespace <- function(package, quietly = FALSE) {
    if (identical(package, "zip")) return(FALSE)
    base::requireNamespace(package, quietly = quietly)
  }
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
inicio <- grep("^### Exportação dos arquivos KML(?:/KMZ)?\\.$", linhas)
fim <- grep("^### Relatório de performance da execução$", linhas)
if (length(inicio) != 1L || length(fim) != 1L || inicio >= fim) {
  stop("Não foi possível isolar inequivocamente o módulo KML/KMZ da candidata.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
tempo <- system.time(eval(parse(text = linhas[inicio:(fim - 1L)]), envir = .GlobalEnv))

modulo_kmz <- identical(linhas[[inicio]], "### Exportação dos arquivos KML/KMZ.")
esperados <- if (modulo_kmz) {
  file.path(
    output_dir,
    "04_validacao_espacial",
    c(
      "UAs_verg_ini_verg_fin.kml",
      "UAs_verg_ini_verg_fin.kmz",
      "UAs_registros_corrig_stat.kml",
      "UAs_registros_corrig_stat.kmz",
      "UAs_areas_operacionais_protecao_100m.kml",
      "UAs_areas_operacionais_protecao_100m.kmz",
      "auditoria_produtos_kml_kmz.csv",
      "auditoria_areas_operacionais_protecao_100m.csv",
      "files/icon1.png"
    )
  )
} else {
  file.path(output_dir, c("UAs_verg_ini_verg_fin.kml", "UAs_registros_corrig_stat.kml"))
}
faltantes <- esperados[!file.exists(esperados) | file.info(esperados)$size <= 0]
if (length(faltantes)) {
  stop("Produtos KML/KMZ ausentes ou vazios: ", paste(basename(faltantes), collapse = ", "), call. = FALSE)
}

cat(sprintf("QA_KML_EXECUCAO_OK elapsed=%.3f user=%.3f system=%.3f n=%d\n", tempo[["elapsed"]], tempo[["user.self"]], tempo[["sys.self"]], nrow(registros_corrig_stat)))
