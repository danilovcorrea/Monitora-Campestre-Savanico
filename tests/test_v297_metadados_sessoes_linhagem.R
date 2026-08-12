#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.7.R", mustWork = TRUE)
tips <- if (length(args) >= 3L) args[2:3] else c(
  "/mnt/c/Users/danil/OneDrive - ICMBio/ICMBio/Monitora_CBC/SISMONITORA/dados_organizados/dados_pre_validados/FNCS/2.9.5_FNCS_rod05",
  "/mnt/c/Users/danil/OneDrive - ICMBio/ICMBio/Monitora_CBC/SISMONITORA/dados_organizados/dados_pre_validados/APAI/2.9.4_APAI_rod04"
)
esperados <- c(FNCS = 5L, APAI = 4L)

env <- new.env(parent = globalenv())
alvos <- c(
  "MONITORA_METADADOS_SESSOES_SCHEMA_ATUAL",
  "monitora_linhagem_hash_arquivo", "monitora_linhagem_metadados_sessoes_template",
  "monitora_linhagem_metadados_sessoes_normalizar", "monitora_linhagem_metadados_sessoes_importar",
  "monitora_linhagem_metadados_sessoes_consolidar", "monitora_linhagem_metadados_sessoes_escrever",
  "monitora_linhagem_inventario_sessoes_dt", "monitora_linhagem_reassinar_pos_organizacao"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) && as.character(x[[2L]]) %in% alvos) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(parse(file = script, keep.source = FALSE)), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))
env$monitora_fwrite <- function(x, arquivo, ...) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, na = "")
}
env$monitora_correcao_hash_texto <- function(x) digest::digest(paste(as.character(x), collapse = "|"), algo = "sha256")

for (tip in tips) {
  tip <- normalizePath(tip, mustWork = TRUE)
  uc <- if (grepl("FNCS", tip, fixed = TRUE)) "FNCS" else "APAI"
  lin <- file.path(tip, "output", "02_painel_correcoes", "linhagem")
  stopifnot(all(file.exists(file.path(lin, c(
    "manifesto_linhagem.json", "correcoes_semanticas_consolidada.csv",
    "metadados_sessoes_painel_recuperacao.csv", "manifesto_recuperacao_metadados_sessoes.json"
  )))))
  td <- tempfile(paste0("v297_", tolower(uc), "_")); dir.create(file.path(td, "input", "linhagem"), recursive = TRUE)
  arquivos <- c(
    "manifesto_linhagem.json", "correcoes_semanticas_consolidada.csv",
    "metadados_sessoes_painel_recuperacao.csv", "manifesto_recuperacao_metadados_sessoes.json"
  )
  stopifnot(all(file.copy(file.path(lin, arquivos), file.path(td, "input", "linhagem", arquivos))))
  man <- jsonlite::read_json(file.path(td, "input", "linhagem", "manifesto_linhagem.json"), simplifyVector = TRUE)
  recuperados <- env$monitora_linhagem_metadados_sessoes_importar(
    file.path(td, "input"), man,
    file.path(td, "input", "linhagem", "correcoes_semanticas_consolidada.csv")
  )
  stopifnot(nrow(recuperados) == esperados[[uc]], uniqueN(recuperados$exec_id) == nrow(recuperados))
  stopifnot(all(recuperados$origem_metadados == "recuperacao_historica_validada"))

  # A migração é fail-closed: qualquer alteração posterior no arquivo aditivo
  # invalida a assinatura do manifesto de recuperação.
  dir_tamper <- file.path(td, "tamper_recuperacao", "input", "linhagem")
  dir.create(dir_tamper, recursive = TRUE)
  stopifnot(all(file.copy(file.path(td, "input", "linhagem", arquivos), file.path(dir_tamper, arquivos))))
  rec_tamper <- fread(file.path(dir_tamper, "metadados_sessoes_painel_recuperacao.csv"), colClasses = "character", na.strings = NULL)
  rec_tamper[1L, instituicao := "alterada_apos_assinatura"]
  fwrite(rec_tamper, file.path(dir_tamper, "metadados_sessoes_painel_recuperacao.csv"), na = "")
  bloqueou_tamper <- tryCatch({
    env$monitora_linhagem_metadados_sessoes_importar(
      file.path(td, "tamper_recuperacao", "input"), man,
      file.path(dir_tamper, "correcoes_semanticas_consolidada.csv")
    )
    FALSE
  }, error = function(e) grepl("hash", tolower(conditionMessage(e)), fixed = TRUE))
  stopifnot(bloqueou_tamper)

  conflito <- copy(recuperados[1L]); conflito[, instituicao := "instituicao_conflitante"]
  bloqueou <- tryCatch({
    env$monitora_linhagem_metadados_sessoes_normalizar(rbindlist(list(recuperados, conflito)), abortar = TRUE)
    FALSE
  }, error = function(e) grepl("conflitantes", conditionMessage(e), fixed = TRUE))
  stopifnot(bloqueou)

  exec_novo <- paste0("20990101_", if (uc == "FNCS") "010101" else "020202")
  env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.7-dev-teste"
  env$MONITORA_ARQUIVO_METADADOS_SESSAO_PAINEL <- ""
  env$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- data.table(
    exec_id = exec_novo, data_hora = "2099-01-01 01:01:01 -0300",
    modo_execucao = "painel_incremental_completo", responsavel_sessao = "Homologação",
    instituicao = "ICMBio", acao_encerramento = "salvar_checkpoint_e_fechar",
    n_operacoes_sessao = 0L, n_itens_auditaveis_sessao = 0L
  )
  saida1 <- env$monitora_linhagem_metadados_sessoes_escrever(file.path(td, "saida1", "linhagem"))
  stopifnot(nrow(saida1$dados) == esperados[[uc]] + 1L, exec_novo %in% saida1$dados$exec_id)

  # O manifesto pós-organização assina o ledger de metadados e sua cardinalidade.
  out_assinado <- file.path(td, "saida_assinada")
  dir.create(file.path(out_assinado, "01_produtos_dados"), recursive = TRUE)
  dir.create(file.path(out_assinado, "02_painel_correcoes", "linhagem"), recursive = TRUE)
  writeLines("dados_teste", file.path(out_assinado, "01_produtos_dados", "registros_corrig.csv"))
  fwrite(data.table(event_id = "evt_teste"), file.path(out_assinado, "02_painel_correcoes", "linhagem", "correcoes_semanticas_consolidada.csv"))
  fwrite(data.table(tipo_sanitizacao = character()), file.path(out_assinado, "02_painel_correcoes", "linhagem", "auditoria_sanitizacoes_automaticas.csv"))
  file.copy(saida1$arquivo, file.path(out_assinado, "02_painel_correcoes", "linhagem", "metadados_sessoes_painel_consolidado.csv"))
  jsonlite::write_json(list(
    revision_id = "rev_anterior", parent_revision_id = as.character(man$revision_id),
    exec_id = exec_novo, build = "v2.9.7-dev-teste", modo = "painel_incremental_completo"
  ), file.path(out_assinado, "02_painel_correcoes", "linhagem", "manifesto_linhagem.json"), auto_unbox = TRUE)
  reassinado <- env$monitora_linhagem_reassinar_pos_organizacao(out_assinado, "teste_v297")
  stopifnot(
    identical(as.integer(reassinado$session_metadata_count), as.integer(nrow(saida1$dados))),
    identical(
      as.character(reassinado$metadados_sessoes_sha256),
      env$monitora_linhagem_hash_arquivo(file.path(out_assinado, "02_painel_correcoes", "linhagem", "metadados_sessoes_painel_consolidado.csv"))
    )
  )

  # Segunda geração: usa somente o consolidado assinado; os artefatos de
  # recuperação não são necessários nem propagados.
  dir2 <- file.path(td, "geracao2", "input", "linhagem"); dir.create(dir2, recursive = TRUE)
  file.copy(saida1$arquivo, file.path(dir2, "metadados_sessoes_painel_consolidado.csv"))
  file.copy(file.path(td, "input", "linhagem", "correcoes_semanticas_consolidada.csv"), file.path(dir2, "correcoes_semanticas_consolidada.csv"))
  man2 <- man
  man2$metadados_sessoes_sha256 <- env$monitora_linhagem_hash_arquivo(file.path(dir2, "metadados_sessoes_painel_consolidado.csv"))
  man2$session_metadata_count <- nrow(saida1$dados)
  jsonlite::write_json(man2, file.path(dir2, "manifesto_linhagem.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  env$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- NULL
  herd2 <- env$monitora_linhagem_metadados_sessoes_importar(
    file.path(td, "geracao2", "input"), man2,
    file.path(dir2, "correcoes_semanticas_consolidada.csv")
  )
  stopifnot(nrow(herd2) == nrow(saida1$dados), setequal(herd2$exec_id, saida1$dados$exec_id))
  stopifnot(identical(
    get0("MONITORA_METADADOS_SESSOES_LINHAGEM_STATUS", envir = .GlobalEnv, inherits = FALSE),
    "ledger_metadados_sessoes_assinado_importado"
  ))

  # O consolidado permanente também bloqueia adulteração na geração seguinte.
  dir2_tamper <- file.path(td, "geracao2_tamper", "input", "linhagem")
  dir.create(dir2_tamper, recursive = TRUE)
  stopifnot(all(file.copy(list.files(dir2, full.names = TRUE), dir2_tamper)))
  meta_tamper <- fread(file.path(dir2_tamper, "metadados_sessoes_painel_consolidado.csv"), colClasses = "character", na.strings = NULL)
  meta_tamper[1L, responsavel_sessao := "alterado_apos_assinatura"]
  fwrite(meta_tamper, file.path(dir2_tamper, "metadados_sessoes_painel_consolidado.csv"), na = "")
  bloqueou_consolidado <- tryCatch({
    env$monitora_linhagem_metadados_sessoes_importar(
      file.path(td, "geracao2_tamper", "input"), man2,
      file.path(dir2_tamper, "correcoes_semanticas_consolidada.csv")
    )
    FALSE
  }, error = function(e) grepl("hash", tolower(conditionMessage(e)), fixed = TRUE))
  stopifnot(bloqueou_consolidado)

  ledger <- fread(file.path(lin, "correcoes_semanticas_consolidada.csv"), colClasses = "character", na.strings = NULL)
  apps <- fread(file.path(lin, "aplicacoes_correcoes.csv"), colClasses = "character", na.strings = NULL)
  env$MONITORA_TRILHA_SEMANTICA_HERDADA <- ledger
  env$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table()
  env$MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table()
  env$MONITORA_REPLAY_APLICACOES <- apps
  env$MONITORA_EXEC_ID <- exec_novo
  env$MONITORA_LINHAGEM_STATUS <- "proveniencia_historica_importada_hash_verificado"
  env$MONITORA_LINHAGEM_MANIFESTO <- file.path(dir2, "manifesto_linhagem.json")
  inv <- env$monitora_linhagem_inventario_sessoes_dt()
  linhas_meta <- inv$sessoes[exec_id %in% herd2$exec_id]
  stopifnot(
    nrow(linhas_meta) == nrow(herd2),
    all(linhas_meta$metadados_sessao == "registrados_na_linhagem"),
    !any(grepl("nao_registrad", linhas_meta$instituicao)),
    !any(grepl("nao_registrad", linhas_meta$modo_execucao)),
    !any(grepl("nao_registrad", linhas_meta$acao_encerramento)),
    all(!is.na(linhas_meta$n_operacoes_sessao)),
    all(!is.na(linhas_meta$n_itens_auditaveis_sessao))
  )
  int_meta <- inv$integridade[verificacao %in% c(
    "metadados_sessoes_manifesto", "execucoes_sem_metadados_persistidos",
    "hash_metadados_sessoes_manifesto_vs_arquivo"
  )]
  stopifnot(nrow(int_meta) == 3L, all(int_meta$status == "ok"))
  cat(sprintf("%s_OK: recuperadas=%d; segunda_geracao=%d; inventario_completo=%d; tempo=%.4fs\n",
              uc, esperados[[uc]], nrow(herd2), nrow(linhas_meta), inv$tempo_s))
}
cat("TEST_V297_METADADOS_SESSOES_LINHAGEM_OK\n")
