#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.6.R",
  mustWork = TRUE
)

alvos <- c(
  "monitora_publicacao_ab_cpf_digitos_verificadores_validos",
  "monitora_publicacao_ab_cpf_avaliar",
  "monitora_coletores_repeat_limpar",
  "monitora_coletores_repeat_normalizar_nome",
  "monitora_coletores_repeat_sanitizar_pares",
  "monitora_pendencias_biologicas_indice",
  "monitora_pendencias_ocorrencia_id",
  "monitora_planilha_importacao_sismonitora_gerar_seguro"
)
env <- new.env(parent = globalenv())
arvore <- parse(file = script, keep.source = FALSE)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nome <- as.character(x[[2L]])
    if (nome %in% alvos) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

env$monitora_correcao_hash_texto <- function(x) {
  vapply(as.character(x), digest::digest, character(1L), algo = "sha256", serialize = FALSE)
}

### Caso real que encerrou a run01: nomes permanecem; o CPF associado a mais
### de um nome é removido de todos os pares conflitantes, sem inferência.
fonte <- Sys.getenv("MONITORA_QA_REGISTROS_CORRIG", unset = "")
if (file.exists(fonte)) {
  hdr <- names(fread(fonte, nrows = 0L, encoding = "UTF-8"))
  cols <- intersect(c("COLETA", "coleta", "coletor/nome", "coletor/cpf"), hdr)
  x <- fread(
    fonte, select = cols, colClasses = "character", na.strings = NULL,
    encoding = "UTF-8", showProgress = FALSE
  )
  col_coleta <- intersect(c("COLETA", "coleta"), names(x))[1L]
  caso <- x[get(col_coleta) == "28498"]
  caso <- caso[nzchar(as.character(`coletor/nome`)) | nzchar(as.character(`coletor/cpf`))]
  nomes_antes <- as.character(caso[["coletor/nome"]])
  cpfs_antes <- as.character(caso[["coletor/cpf"]])
  san <- env$monitora_coletores_repeat_sanitizar_pares(nomes_antes, cpfs_antes)
  mapa <- data.table(nome = nomes_antes, cpf = cpfs_antes)
  ambiguos <- mapa[nzchar(nome) & nzchar(cpf), .(n_nomes = uniqueN(nome)), by = cpf][n_nomes > 1L, cpf]
  stopifnot(
    length(ambiguos) >= 1L,
    identical(san$nomes, env$monitora_coletores_repeat_normalizar_nome(nomes_antes)),
    all(!nzchar(san$cpfs[cpfs_antes %chin% ambiguos])),
    san$n_cpfs_descartados >= 2L,
    identical(san$status, "sanitizado_com_descarte_cpf_sem_inferencia")
  )
}

### O fechamento de muitas COLETAS não revalida o mesmo CPF repetidamente.
avaliar_original <- env$monitora_publicacao_ab_cpf_avaliar
n_avaliacoes <- 0L
env$monitora_publicacao_ab_cpf_avaliar <- function(x) {
  n_avaliacoes <<- n_avaliacoes + 1L
  list(valido = rep(TRUE, length(x)), valor = as.character(x))
}
cache_cpf <- new.env(hash = TRUE, parent = emptyenv())
for (ii in seq_len(50L)) {
  san_cache <- env$monitora_coletores_repeat_sanitizar_pares(
    c("Pessoa A", "Pessoa B"), c("11111111111", "22222222222"),
    avaliacao_cache = cache_cpf
  )
  stopifnot(identical(san_cache$cpfs, c("11111111111", "22222222222")))
}
stopifnot(n_avaliacoes == 1L, length(ls(cache_cpf, all.names = TRUE)) == 2L)
env$monitora_publicacao_ab_cpf_avaliar <- avaliar_original

### Textos de exibição podem mudar; a identidade canônica não.
bio <- data.table(
  tipo_ocorrencia = "seca_morta_em_revisao", rotulo_ocorrencia = "Revisão",
  COLETA = "C1", linha_indice = 7L, monitora_row_id = "row_7",
  forma_de_vida_detectada = "arbusto", categoria_superior = "seca_morta",
  atributo_problema = "forma_vida_seca_morta", status_contrato = "em_revisao",
  UC = "UC", EA = "EA", UA = "UA", ANO = "2026"
)
idx1 <- env$monitora_pendencias_biologicas_indice(bio)
bio[, rotulo_ocorrencia := "Rótulo editorial revisado"]
idx2 <- env$monitora_pendencias_biologicas_indice(bio)
stopifnot(identical(
  env$monitora_pendencias_ocorrencia_id(idx1),
  env$monitora_pendencias_ocorrencia_id(idx2)
))

### Falha deliberada do XLSX não interrompe o chamador nem produtos seguintes.
dir_teste <- tempfile("v296_xlsx_resiliente_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- dir_teste
env$MONITORA_EXEC_ID <- "v296_xlsx_forcado"
env$monitora_fwrite <- function(x, arquivo, ...) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, na = "")
}
env$monitora_planilha_importacao_sismonitora_gerar <- function(...) {
  stop("falha_xlsx_forcada_homologacao", call. = FALSE)
}
suprimiu_stop <- TRUE
resultado <- withCallingHandlers(
  tryCatch(
    env$monitora_planilha_importacao_sismonitora_gerar_seguro(),
    error = function(e) { suprimiu_stop <<- FALSE; NULL }
  ),
  warning = function(w) invokeRestart("muffleWarning")
)
produto_seguinte <- file.path(dir_teste, "produto_seguinte.ok")
writeLines("ok", produto_seguinte)
stopifnot(
  suprimiu_stop,
  is.list(resultado),
  !isTRUE(env$MONITORA_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA_GERADO),
  grepl("falha_xlsx_forcada_homologacao", env$MONITORA_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA_ERRO, fixed = TRUE),
  file.exists(file.path(dir_teste, "03_auditorias", "contrato_xlsform", "falha_planilha_importacao_sismonitora.csv")),
  file.exists(produto_seguinte)
)

cat("TEST_V296_RESILIENCIA_PRODUTOS_CPF_OK\n")
