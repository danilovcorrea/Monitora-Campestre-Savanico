suppressPackageStartupMessages(library(data.table))
main <- function() {
  args <- commandArgs(TRUE)
  if (length(args) < 3L) stop("Uso: candidata.R pasta_familia_real destino_novo")
  source("tools/v308/carregar.R")
  e <- monitora_v308_carregar(args[1L])
  origem <- normalizePath(args[2L], winslash = "/", mustWork = TRUE)
  raiz <- args[3L]
  if (dir.exists(raiz) && length(list.files(raiz, all.files = TRUE, no.. = TRUE))) stop("Destino de teste deve ser novo.")
  dir.create(raiz, recursive = TRUE, showWarnings = FALSE)
  raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  e$MONITORA_BASE_DIR <- raiz; e$MONITORA_INPUT_DIR <- file.path(raiz, "input")
  e$MONITORA_OUTPUT_DIR <- file.path(raiz, "output"); e$MONITORA_LOG_DIR <- file.path(raiz, "log")
  e$MONITORA_CORRECOES_DIR <- file.path(e$MONITORA_OUTPUT_DIR, "correcoes_campos")
  e$MONITORA_EXEC_ID <- paste0("v308_org_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  e$MONITORA_V307_TRANSACAO_FINAL_ATIVA <- NULL
  e$MONITORA_OUTPUT_MAPA_MOVIMENTACOES <- data.table(); e$MONITORA_LOG_EXECUCAO <- data.table()
  gravar <- function(p, txt) { dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines(txt, p, useBytes = TRUE) }
  hash <- function(p) digest::digest(file = p, algo = "sha256")
  snapshot <- function(p) {
    x <- sort(list.files(p, full.names = TRUE, recursive = TRUE)); x <- x[!dir.exists(x)]
    setNames(vapply(x, hash, character(1L)), substring(x, nchar(p) + 2L))
  }
  anterior <- Sys.getenv("MONITORA_DESTINO_COMPARTILHAMENTO", unset = NA_character_)
  on.exit(if (is.na(anterior)) Sys.unsetenv("MONITORA_DESTINO_COMPARTILHAMENTO") else Sys.setenv(MONITORA_DESTINO_COMPARTILHAMENTO = anterior), add = TRUE)
  Sys.setenv(MONITORA_DESTINO_COMPARTILHAMENTO = paste0("C:/", strrep("x", 124L)))
  gravar(file.path(e$MONITORA_INPUT_DIR, "fonte.txt"), "fonte privada intocada")
  manifest <- list.files(origem, pattern = "_paginas[.]csv$", full.names = TRUE)
  stopifnot(length(manifest) == 1L)
  p <- fread(manifest)
  info <- e$monitora_v308_ler_paginas_evidencia(file.path(origem, p$arquivo_canonico[1L]))
  stopifnot(info$manifesto)
  relatorio <- file.path(e$MONITORA_OUTPUT_DIR, "08_analises", "u_0123456789")
  figuras <- file.path(relatorio, "figuras"); dir.create(figuras, recursive = TRUE)
  sidecars <- e$monitora_v308_evidencia_manifestos(info$canonico)
  fs <- c(info$arquivos, sidecars$paginas, sidecars$celulas)
  stopifnot(all(file.copy(fs, figuras, overwrite = FALSE)))
  canonico <- file.path(figuras, basename(info$canonico))
  relativo <- paste0("figuras/", basename(canonico))
  conteudo <- e$monitora_v308_expandir_figuras(c("# Prova", e$monitora_relatorios_analiticos_figura(relativo, "Figura de origem preservada.")), relatorio)
  for (ext in c("Rmd", "md", "html")) gravar(file.path(relatorio, paste0("analitico_detalhado.", ext)), conteudo)
  g <- data.table(id = "inferencias_teste", disponivel = TRUE, arquivo_relatorio = relativo,
    hash_md5 = unname(tools::md5sum(canonico)), n_resultados_estatisticos = nrow(info$celulas),
    incorporada_relatorio_detalhado = TRUE, status_documental = "selecionada_relatorio_detalhado")
  g <- e$monitora_v308_completar_indice_graficos(g, relatorio)
  fwrite(g, file.path(relatorio, "indice_selecao_graficos.csv"), bom = TRUE)
  legado <- file.path(e$MONITORA_OUTPUT_DIR, basename(canonico))
  png::writePNG(array(.42, dim = c(8, 8, 3)), legado)
  sha_legado <- hash(legado)
  gravar(file.path(e$MONITORA_OUTPUT_DIR, "07_validacao", "referencia_legacy.txt"), basename(legado))
  sem_marcador <- file.path(e$MONITORA_OUTPUT_DIR, "08_analises", "u_0123456789_sem_marcador", "analitico_detalhado.Rmd")
  gravar(sem_marcador, basename(legado))
  antes <- snapshot(relatorio); input_antes <- snapshot(e$MONITORA_INPUT_DIR)
  e$monitora_output_organizar_produtos(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "fechamento_final_prova_paginas_v308")
  depois <- snapshot(relatorio)
  recuperado <- e$monitora_v308_ler_paginas_evidencia(canonico, relatorio)
  links <- readLines(file.path(e$MONITORA_OUTPUT_DIR, "07_validacao", "referencia_legacy.txt"), warn = FALSE)
  checks <- list(report_all_bytes_unchanged = identical(antes, depois),
    input_unchanged = identical(input_antes, snapshot(e$MONITORA_INPUT_DIR)),
    legacy_moved_without_loss = !file.exists(legado) && length(links) == 1L && file.exists(file.path(e$MONITORA_OUTPUT_DIR, links)) && hash(file.path(e$MONITORA_OUTPUT_DIR, links)) == sha_legado,
    new_family_complete = nrow(recuperado$celulas) == nrow(info$celulas) && nrow(recuperado$paginas) == nrow(info$paginas),
    unmarked_report_still_rewritten = identical(readLines(sem_marcador, warn = FALSE), links),
    external_reference_rewritten = length(links) == 1L && links != basename(legado),
    declared_root_127 = nchar(Sys.getenv("MONITORA_DESTINO_COMPARTILHAMENTO")) == 127L,
    canonical_paths_under_248 = all(nchar(e$monitora_caminho_windows_projetado(file.path(figuras, basename(fs)), e$MONITORA_OUTPUT_DIR)) <= 248L))
  e$monitora_v308_validar_paginas_documentais(g, relatorio, readLines(file.path(relatorio, "analitico_detalhado.Rmd"), warn = FALSE), character())
  jsonlite::write_json(list(pass = all(unlist(checks)), checks = checks, candidate_sha256 = hash(args[1L]),
    source_family = origem, report = relatorio, projected_paths = e$monitora_caminho_windows_projetado(file.path(figuras, basename(fs)), e$MONITORA_OUTPUT_DIR),
    report_snapshot_before = as.list(antes), report_snapshot_after = as.list(depois), pipeline_scientific_executed = FALSE),
    file.path(raiz, "PROVA_ORGANIZADOR_PAGINAS.json"), pretty = TRUE, auto_unbox = TRUE)
  stopifnot(all(unlist(checks)))
  cat("PASS_ORGANIZADOR_REAL_PAGINAS", raiz, "\n")
}
main()
