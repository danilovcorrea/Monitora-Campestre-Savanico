#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE)) {
  stop("A recuperação exige os pacotes jsonlite e digest.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Uso: Rscript tools/recuperar_metadados_sessoes_linhagem.R <diretorio_UC> <run_tip_canonica>", call. = FALSE)
}
dir_uc <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
run_tip <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
if (!startsWith(run_tip, paste0(dir_uc, "/"))) stop("A run tip deve pertencer ao diretório da UC.", call. = FALSE)

hash_arquivo <- function(path) digest::digest(file = path, algo = "sha256")
manifestos <- list.files(
  dir_uc, pattern = "^manifesto_linhagem[.]json$", recursive = TRUE,
  full.names = TRUE, include.dirs = FALSE
)
manifestos <- manifestos[grepl("/output/02_painel_correcoes/linhagem/manifesto_linhagem[.]json$", manifestos)]
if (!length(manifestos)) stop("Nenhum manifesto de saída foi localizado para a UC.", call. = FALSE)

nodes <- rbindlist(lapply(manifestos, function(path) {
  man <- jsonlite::read_json(path, simplifyVector = TRUE)
  data.table(
    run = sub("/output/02_painel_correcoes/linhagem/manifesto_linhagem[.]json$", "", path),
    manifesto = path,
    revision_id = as.character(man$revision_id)[1L],
    parent_revision_id = if (is.null(man$parent_revision_id) || !length(man$parent_revision_id) || is.na(man$parent_revision_id[[1L]])) "" else as.character(man$parent_revision_id)[1L],
    exec_id = as.character(man$exec_id)[1L], build = as.character(man$build)[1L],
    ledger_sha256 = as.character(man$ledger_sha256)[1L]
  )
}), fill = TRUE)
if (nodes[, anyDuplicated(revision_id)]) stop("Revisões duplicadas impedem reconstruir a cadeia de forma inequívoca.", call. = FALSE)
tip <- nodes[run == run_tip]
if (nrow(tip) != 1L) stop("A run tip não possui um único manifesto de saída.", call. = FALSE)

cadeia <- list(); atual <- tip
repeat {
  cadeia[[length(cadeia) + 1L]] <- atual
  pai <- as.character(atual$parent_revision_id)[1L]
  if (!nzchar(pai)) break
  anterior <- nodes[revision_id == pai]
  if (nrow(anterior) != 1L) stop("Cadeia interrompida ou ambígua na revisão-pai: ", pai, call. = FALSE)
  atual <- anterior
}
cadeia <- rbindlist(rev(cadeia), fill = TRUE)

ler_sidecar <- function(run, exec_id, build, revision_id, parent_revision_id) {
  candidatos <- list.files(run, pattern = paste0("^metadados_sessao_painel_", exec_id, "[.]csv$"), recursive = TRUE, full.names = TRUE)
  candidatos <- candidatos[grepl("/output/", candidatos)]
  if (length(candidatos) != 1L) {
    stop("Esperado um sidecar para exec_id ", exec_id, "; encontrados: ", length(candidatos), call. = FALSE)
  }
  x <- fread(candidatos, colClasses = "character", na.strings = NULL, showProgress = FALSE)
  obrig <- c("exec_id", "data_hora", "modo_execucao", "responsavel_sessao", "instituicao", "acao_encerramento", "n_operacoes_sessao", "n_itens_auditaveis_sessao")
  if (nrow(x) != 1L || length(setdiff(obrig, names(x))) || !identical(as.character(x$exec_id), exec_id)) {
    stop("Sidecar inválido para exec_id ", exec_id, call. = FALSE)
  }
  for (cc in obrig) if (!nzchar(trimws(as.character(x[[cc]])[1L]))) stop("Campo vazio no sidecar ", exec_id, ": ", cc, call. = FALSE)
  data.table(
    metadados_schema_version = "monitora_metadados_sessoes_v1",
    exec_id = exec_id, data_hora = as.character(x$data_hora), build_sessao = build,
    modo_execucao = as.character(x$modo_execucao), responsavel_sessao = as.character(x$responsavel_sessao),
    instituicao = as.character(x$instituicao), acao_encerramento = as.character(x$acao_encerramento),
    n_operacoes_sessao = as.character(x$n_operacoes_sessao),
    n_itens_auditaveis_sessao = as.character(x$n_itens_auditaveis_sessao),
    origem_metadados = "recuperacao_historica_validada", sidecar_sha256 = hash_arquivo(candidatos)
  )
}

metadados <- rbindlist(Map(
  ler_sidecar, cadeia$run, cadeia$exec_id, cadeia$build,
  cadeia$revision_id, cadeia$parent_revision_id
), fill = TRUE)
if (metadados[, anyDuplicated(exec_id)]) stop("A cadeia recuperada contém exec_id duplicado.", call. = FALSE)
setorder(metadados, data_hora, exec_id)

dir_lin <- file.path(run_tip, "output", "02_painel_correcoes", "linhagem")
arq_ledger <- file.path(dir_lin, "correcoes_semanticas_consolidada.csv")
if (!file.exists(arq_ledger) || !identical(hash_arquivo(arq_ledger), as.character(tip$ledger_sha256))) {
  stop("O ledger físico da run tip diverge do manifesto; recuperação bloqueada.", call. = FALSE)
}
arq_csv <- file.path(dir_lin, "metadados_sessoes_painel_recuperacao.csv")
arq_json <- file.path(dir_lin, "manifesto_recuperacao_metadados_sessoes.json")
arq_aud <- file.path(dir_lin, "auditoria_recuperacao_metadados_sessoes.csv")
if (any(file.exists(c(arq_csv, arq_json, arq_aud)))) {
  stop("Artefatos de recuperação já existem na run tip; nenhum arquivo foi sobrescrito.", call. = FALSE)
}
fwrite(metadados, arq_csv, na = "")
hash_meta <- hash_arquivo(arq_csv)
excluidas <- nodes[!revision_id %in% cadeia$revision_id]
manifesto_rec <- list(
  recovery_schema = "monitora_recuperacao_metadados_sessoes_v1",
  base_revision_id = as.character(tip$revision_id),
  base_ledger_sha256 = as.character(tip$ledger_sha256),
  metadados_recuperacao_sha256 = hash_meta,
  n_sessoes = as.integer(nrow(metadados)),
  exec_ids = as.character(metadados$exec_id),
  runs_canonicas = sub(paste0("^", dir_uc, "/"), "", cadeia$run),
  runs_excluidas = sub(paste0("^", dir_uc, "/"), "", excluidas$run),
  criterio = "ancestralidade_exata_entre_revision_id_e_parent_revision_id_ate_a_run_tip"
)
jsonlite::write_json(manifesto_rec, arq_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
auditoria <- rbindlist(list(
  cadeia[, .(run = sub(paste0("^", dir_uc, "/"), "", run), exec_id, revision_id, parent_revision_id, status = "incluida_cadeia_canonica")],
  excluidas[, .(run = sub(paste0("^", dir_uc, "/"), "", run), exec_id, revision_id, parent_revision_id, status = "excluida_nao_ancestral_da_run_tip")]
), fill = TRUE)
fwrite(auditoria, arq_aud, na = "")
cat(sprintf("RECUPERACAO_OK: %d sessão(ões); tip=%s; hash=%s\n", nrow(metadados), basename(run_tip), hash_meta))
