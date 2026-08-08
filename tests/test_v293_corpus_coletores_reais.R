args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library(data.table))
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R", mustWork = TRUE)
parsed <- parse(file = script)
body <- parsed[[1L]][[2L]]
env <- new.env(parent = globalenv())

carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    if (is.call(rhs) && identical(as.character(rhs[[1L]]), "function")) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

raizes <- c(
  Sys.getenv("MONITORA_TESTE_COLETORES_PLANILHA", unset = ""),
  Sys.getenv("MONITORA_TESTE_COLETORES_DOWNLOAD", unset = "")
)
raizes <- raizes[dir.exists(raizes)]
if (!length(raizes)) {
  cat("V293_CORPUS_COLETORES_REAIS_SKIPPED_DIRETORIOS_AUSENTES\n")
  quit(save = "no", status = 0L)
}

arquivos <- unique(unlist(lapply(raizes, list.files, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE), use.names = FALSE))
stopifnot(length(arquivos) > 0L)
partes <- list()
kk <- 0L
falhas_leitura <- 0L
for (ii in seq_along(arquivos)) {
  hdr <- tryCatch(names(fread(arquivos[ii], nrows = 0L, encoding = "UTF-8", showProgress = FALSE, check.names = FALSE)), error = function(e) character())
  col_nome <- intersect(c("coletor/nome", "COLETORES", "Coletores"), hdr)[1L]
  if (!length(col_nome) || is.na(col_nome)) next
  col_coleta <- intersect(c("COLETA", "coleta", "coleta_uuid", "UUID", "uuid"), hdr)[1L]
  col_cpf <- intersect(c("coletor/cpf", "CPF (coletor)", "cpf (coletor)"), hdr)[1L]
  selecao <- unique(na.omit(c(col_coleta, col_nome, col_cpf)))
  x <- tryCatch(fread(arquivos[ii], select = selecao, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(x)) {
    falhas_leitura <- falhas_leitura + 1L
    next
  }
  kk <- kk + 1L
  partes[[kk]] <- data.table(
    COLETA = paste0("arquivo_", ii, "::", if (length(col_coleta) && !is.na(col_coleta)) as.character(x[[col_coleta]]) else seq_len(nrow(x))),
    COLETORES = as.character(x[[col_nome]]),
    `coletor/cpf` = if (length(col_cpf) && !is.na(col_cpf)) as.character(x[[col_cpf]]) else ""
  )
}
stopifnot(falhas_leitura == 0L, kk > 0L)
amostra <- rbindlist(partes, fill = TRUE, use.names = TRUE)
amostra[, COLETORES := env$monitora_coletores_repeat_limpar(COLETORES)]
legado <- amostra[grepl("^\\s*\\{|^\\s*equipe\\s*:", COLETORES, ignore.case = TRUE, perl = TRUE)]
stopifnot(nrow(legado) > 0L)
payloads <- unique(legado[, .(COLETORES, `coletor/cpf`)])
avaliados <- lapply(seq_len(nrow(payloads)), function(ii) env$monitora_coletores_repeat_parse_legado(payloads$COLETORES[ii], payloads[["coletor/cpf"]][ii]))
ok_parse <- vapply(avaliados, function(z) isTRUE(z$reconhecido), logical(1L))
if (any(!ok_parse)) {
  contar <- function(x, padrao) lengths(regmatches(x, gregexpr(padrao, x, perl = TRUE)))
  z <- payloads[!ok_parse]
  diagnostico <- data.table(
    prefixo_mapa = startsWith(trimws(z$COLETORES), "{"),
    prefixo_equipe = grepl("^\\s*equipe\\s*:", z$COLETORES, ignore.case = TRUE, perl = TRUE),
    n_caracteres = nchar(z$COLETORES), n_chaves_abre = contar(z$COLETORES, "\\{"),
    n_chaves_fecha = contar(z$COLETORES, "\\}"), n_dois_pontos = contar(z$COLETORES, ":"),
    n_aspas_simples = contar(z$COLETORES, "'"), n_aspas_duplas = contar(z$COLETORES, '"')
  )
  print(diagnostico[, .N, by = .(prefixo_mapa, prefixo_equipe, n_caracteres, n_chaves_abre, n_chaves_fecha, n_dois_pontos, n_aspas_simples, n_aspas_duplas)][order(-N)])
}
stopifnot(all(ok_parse))
stopifnot(any(vapply(avaliados, function(z) identical(z$motivo_cpf, "cpf_unico_para_varios_nomes_associacao_ambigua"), logical(1L))))
stopifnot(!any(vapply(avaliados, function(z) isTRUE(z$n_cpfs_descartados > 0L) && any(nzchar(z$cpfs)), logical(1L))))

dir_teste <- tempfile("v293_corpus_coletores_")
dir.create(dir_teste, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(dir_teste, "output")
env$MONITORA_LOG_DIR <- file.path(dir_teste, "log")
env$MONITORA_EXEC_ID <- "v293_corpus_coletores"
env$MONITORA_SCRIPT_VERSAO <- "2.9.3-dev-test"
grupos_legados <- unique(legado$COLETA)
entrada_sanitizacao <- copy(amostra[COLETA %in% grupos_legados])
san <- env$monitora_coletores_repeat_sanitizar_legado(
  entrada_sanitizacao, output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, contexto = "corpus_real", abortar_nao_reconhecido = TRUE
)
stopifnot(isTRUE(san$ok), isTRUE(san$alterou), nrow(san$auditoria) == uniqueN(grupos_legados))
stopifnot(
  !any(grepl("^\\s*\\{|^\\s*equipe\\s*:", san$dt[["coletor/nome"]], ignore.case = TRUE, perl = TRUE)),
  !any(grepl("^\\s*\\{|^\\s*equipe\\s*:", san$dt[["COLETORES"]], ignore.case = TRUE, perl = TRUE)),
  all(env$monitora_publicacao_ab_cpf_avaliar(san$dt[["coletor/cpf"]])$valido),
  all(san$auditoria$cpf_inferido == FALSE),
  file.exists(file.path(env$MONITORA_OUTPUT_DIR, "03_auditorias", "cadastro", "auditoria_sanitizacao_coletores.csv"))
)
san2 <- env$monitora_coletores_repeat_sanitizar_legado(
  copy(san$dt), output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, contexto = "corpus_real_idempotencia", abortar_nao_reconhecido = TRUE
)
stopifnot(!isTRUE(san2$alterou), nrow(san2$auditoria) == 0L, identical(san$dt, san2$dt))

cat(sprintf(
  paste0(
    "V293_CORPUS_COLETORES_REAIS_OK arquivos_csv=%d arquivos_com_coletor=%d ",
    "linhas_legadas=%d payloads_unicos=%d coletas_sanitizadas=%d nomes_recuperados=%d ",
    "cpfs_observados=%d cpfs_preservados=%d cpfs_descartados=%d\n"
  ),
  length(arquivos), kk, nrow(legado), nrow(payloads), nrow(san$auditoria),
  sum(san$auditoria$n_nomes_recuperados), sum(san$auditoria$n_cpfs_observados),
  sum(san$auditoria$n_cpfs_preservados), sum(san$auditoria$n_cpfs_descartados)
))
