#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Informe diretórios de execução.", call. = FALSE)

res <- lapply(args, function(run_dir) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  out <- file.path(run_dir, "output")
  aud_path <- file.path(out, "00_manifesto_execucao", "caminhos_office_v2916.csv")
  idx_path <- file.path(out, "indice_produtos.csv")
  stopifnot(file.exists(aud_path), file.exists(idx_path))
  aud <- fread(aud_path, encoding = "UTF-8")
  aud_mov <- aud[tipo != "sem_alteracao_necessaria"]
  if (nrow(aud_mov)) stopifnot(
    all(aud_mov$conteudo_preservado),
    all(file.exists(file.path(out, aud_mov$caminho_atual)))
  )
  idx <- fread(idx_path, encoding = "UTF-8")
  atuais <- gsub("\\\\", "/", list.files(out, recursive = TRUE, full.names = FALSE))
  stopifnot(setequal(atuais, idx$caminho_relativo))
  ext <- tolower(tools::file_ext(idx$caminho_relativo))
  criticos <- idx[
    situacao_caminho_office == "revisar_caminho_windows" &
      ext %in% c("doc", "docx", "pdf", "html", "htm", "xls", "xlsx")
  ]
  stopifnot(nrow(criticos) == 0L)
  data.table(
    run = basename(run_dir),
    movimentos = nrow(aud_mov),
    hashes_preservados = if (nrow(aud_mov)) all(aud_mov$conteudo_preservado) else TRUE,
    documentos_manuais_criticos = nrow(criticos),
    maior_caminho_antes = if (nrow(aud_mov)) max(aud_mov$comprimento_anterior, na.rm = TRUE) else NA_integer_,
    maior_caminho_depois = if (nrow(aud_mov)) max(aud_mov$comprimento_atual, na.rm = TRUE) else NA_integer_,
    arquivos_inventariados = length(atuais)
  )
})
res <- rbindlist(res, fill = TRUE)
print(res)
cat(sprintf(
  "QA_V2916_CAMINHOS_PASTAS_REAIS_OK runs=%d movimentos=%d\n",
  nrow(res), sum(res$movimentos)
))
