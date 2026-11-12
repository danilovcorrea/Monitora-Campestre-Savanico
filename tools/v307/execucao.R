# Estado de uma rodada e proteção do conjunto publicado. Não há execução no
# carregamento deste módulo: o primeiro ponto do Source chama execucao_iniciar.
monitora_v307_execucao_defaults <- function() {
  vazias <- c(
    "MONITORA_TRILHA_SEMANTICA_SESSAO", "MONITORA_TRILHA_SEMANTICA_REPLAY",
    "MONITORA_TRILHA_SEMANTICA_HERDADA", "MONITORA_REPLAY_APLICACOES",
    "MONITORA_RELATORIOS_DIAGNOSTICOS_POS_PAINEL",
    "MONITORA_VALIDACAO_ESPACIAL_POS_PAINEL_RESULTADO",
    "MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA",
    "MONITORA_METADADOS_SESSOES_LINHAGEM_HERDADOS",
    "MONITORA_METADADOS_SESSOES_RECUPERACAO_AUDITORIA",
    "MONITORA_INVENTARIO_SESSOES_LINHAGEM_HERDADO",
    "MONITORA_LINHAGEM_INCORPORACOES_HERDADAS",
    "MONITORA_LINHAGEM_INCORPORACOES_ATUAIS",
    "MONITORA_AUDITORIA_SANITIZACOES_HERDADA",
    "MONITORA_AUDITORIA_SANITIZACOES_CONSOLIDADA",
    "MONITORA_AUDITORIA_CPF_SESSAO", "MONITORA_AUDITORIA_UUID_REGISTRO_SESSAO",
    "MONITORA_AUDITORIA_MIGRACAO_OUTROS_ORFAO_SESSAO",
    "MONITORA_AUDITORIA_SANITIZACAO_COLETORES_SESSAO",
    "MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO",
    "MONITORA_V307_JUSTIFICATIVAS_IMPORTADAS", "MONITORA_V307_JUSTIFICATIVAS_CONTEXTO",
    "MONITORA_V307_JUSTIFICATIVAS_BASE", "MONITORA_V307_JUSTIFICATIVAS_REPLAY",
    "MONITORA_V307_CACHE_VALIDADO",
    "MONITORA_PLOT_COLS_CACHE",
    "MONITORA_PLOT_COLS_NATIVA", "MONITORA_PLOT_COLS_EXOTICA",
    "MONITORA_PLOT_COLS_SECA_MORTA", "MONITORA_PLOT_COLS_MATERIAL_BOTANICO",
    "MONITORA_IO_CACHE_INVENTARIO_INPUT", "MONITORA_REPLAY_ORACULO_RESULTADO_FINAL",
    "MONITORA_REGISTROS_VALIDADOS_CACHE_PRE_CONTRATO",
    "MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA",
    "MONITORA_AUDITORIA_PERSISTENCIA_POS_EXPORT_ULTIMA",
    "MONITORA_ARQUIVOS_AUDITORIA_PERSISTENCIA_POS_EXPORT_ULTIMA",
    "MONITORA_AUDITORIA_CONCILIACAO_SEMANTICA_ULTIMA",
    "MONITORA_CONFLITOS_SEMANTICOS_ULTIMOS", "MONITORA_PLANO_CORRECOES_EFETIVO_ULTIMO",
    "MONITORA_COLETAS_EXCLUIDAS_CORRECOES_ULTIMA",
    "MONITORA_REGISTROS_VALIDADOS_ULTIMA_AUDITORIA",
    "MONITORA_REGISTROS_VALIDADOS_BLOQUEIOS_ULTIMA",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_AUDITORIA",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_RESUMO"
  )
  out <- setNames(rep(list(NULL), length(vazias)), vazias)
  for (nm in c(
    "MONITORA_PUBLICACAO_AE_CACHE_COLNAMES_NORM",
    "MONITORA_PUBLICACAO_AE_CACHE_RESOLVER_COLUNA",
    "MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS_EMBUTIDOS",
    "MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_RESOLUCAO_COLUNAS",
    "MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_INDICES",
    "MONITORA_PUBLICACAO_AJ_CACHE_PLANO_MAPEAMENTO",
    ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL", ".MONITORA_CONTRATO_MOVIMENTO_CACHE",
    ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE", ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE"
  )) out[nm] <- list(new.env(parent = emptyenv()))
  for (nm in c(
    "MONITORA_RESPONSAVEL_SESSAO_PAINEL", "MONITORA_ARQUIVO_METADADOS_SESSAO_PAINEL",
    "MONITORA_LINHAGEM_ARQUIVO_HERDADO", "MONITORA_LINHAGEM_LEDGER_HERDADO_RAW",
    "MONITORA_LINHAGEM_MANIFESTO"
  )) out[nm] <- list("")
  for (nm in c(
    "MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES",
    "MONITORA_PERSISTENCIA_OPERACOES_USUARIO_PENDENTES",
    "MONITORA_REPLAY_SEMANTICO_EM_EXECUCAO", "MONITORA_REPLAY_ORACULO_VALIDADO_FINAL",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",
    "MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21",
    "MONITORA_REGISTROS_VALIDADOS_GERADO",
    "MONITORA_REGISTROS_VALIDADOS_PRE_SANITIZACAO_APLICADA",
    "MONITORA_RECALC_DATA_HORA_EXECUTADO_ULTIMO",
    "MONITORA_PUBLICACAO_AC_DIRTY_DEPENDENCIAS",
    "MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE"
  )) out[nm] <- list(FALSE)
  out$MONITORA_METADADOS_SESSOES_LINHAGEM_STATUS <- "nao_avaliados"
  out$MONITORA_INVENTARIO_SESSOES_LINHAGEM_STATUS <- "nao_avaliado"
  out$MONITORA_LINHAGEM_STATUS <- "execucao_sem_heranca_incremental"
  out$MONITORA_LINHAGEM_PARENT_REVISION <- NA_character_
  out$MONITORA_LINHAGEM_INPUT_SHA256 <- NA_character_
  out
}

# Nomes reservados de produtos do pipeline. Aqui é necessário remover o binding,
# pois os consumidores antigos usam exists(), inclusive nos ramos após falha.
# Não há varredura por prefixo nem remoção de variáveis R externas a esta lista.
monitora_v307_execucao_produtos_reservados <- function() {
  c("registros_importados_bruto", "registros_importados", "registros_corrig",
    "registros_validados", "registros_corrig_stat", "triagem_painel_unificada",
    "reg_categ_plantas_longer", "reg_formas_vida_nat_longer",
    "reg_formas_vida_exot_longer", "reg_formas_vida_seca_morta_longer", "reg_herb_lenh",
    "reg_corrig_stat_summarise_p1", "reg_corrig_stat_summarise_p1_presence",
    "reg_corrig_stat_summarise_p2", "reg_corrig_stat_summarise_p2_presence",
    "reg_corrig_stat_summarise_p3", "reg_corrig_stat_summarise_p3_presence",
    "reg_corrig_stat_summarise_p4", "reg_corrig_stat_summarise_p4_presence",
    "reg_corrig_stat_summarise_p5", "reg_corrig_stat_summarise_p5_presence",
    "reg_corrig_stat_summarise_material_botanico", "reg_corrig_stat_summarise_material_botanico_presence")
}

monitora_v307_execucao_ids_reservados <- function(envir = .GlobalEnv) {
  ids <- as.character(get0("MONITORA_EXEC_ID", envir = envir, inherits = FALSE))
  reg <- getOption("monitora.v307.execucao.ids", character())
  ids <- c(ids, if (is.character(reg)) reg else character())
  for (nm in c(
    "MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA", "MONITORA_METADADOS_SESSOES_LINHAGEM_HERDADOS",
    "MONITORA_INVENTARIO_SESSOES_LINHAGEM_HERDADO", "MONITORA_REPLAY_APLICACOES",
    "MONITORA_TRILHA_SEMANTICA_HERDADA", "MONITORA_TRILHA_SEMANTICA_SESSAO",
    "MONITORA_TRILHA_SEMANTICA_REPLAY"
  )) {
    x <- get0(nm, envir = envir, inherits = FALSE)
    if (is.data.frame(x)) for (cc in intersect(c("exec_id", "origin_exec_id", "exec_id_registro"), names(x))) {
      ids <- c(ids, as.character(x[[cc]]))
    }
  }
  input <- get0("MONITORA_INPUT_DIR", envir = envir, inherits = TRUE)
  output <- get0("MONITORA_OUTPUT_DIR", envir = envir, inherits = TRUE)
  dirs <- unique(c(if (length(input)) file.path(input, "linhagem"),
                   if (length(output)) file.path(output, "02_painel_correcoes", "linhagem")))
  for (d in dirs) {
    for (nm in c("metadados_sessoes_painel_consolidado.csv", "inventario_sessoes_linhagem.csv")) {
      arq <- file.path(d, nm)
      if (!file.exists(arq) || !requireNamespace("data.table", quietly = TRUE)) next
      x <- tryCatch(suppressWarnings(data.table::fread(
        arq, select = "exec_id", colClasses = "character", showProgress = FALSE
      )), error = function(e) NULL)
      if (is.data.frame(x) && "exec_id" %in% names(x)) ids <- c(ids, x$exec_id)
    }
    arq <- file.path(d, "manifesto_linhagem.json")
    if (file.exists(arq) && requireNamespace("jsonlite", quietly = TRUE)) {
      x <- tryCatch(jsonlite::read_json(arq, simplifyVector = TRUE), error = function(e) NULL)
      if (is.list(x)) ids <- c(ids, as.character(x$exec_id))
    }
  }
  if (length(output) && requireNamespace("data.table", quietly = TRUE)) {
    arq <- file.path(output, "manifesto_execucao_ultima_execucao.csv")
    if (file.exists(arq)) {
      x <- tryCatch(suppressWarnings(data.table::fread(arq, select = "exec_id", colClasses = "character", showProgress = FALSE)), error = function(e) NULL)
      if (is.data.frame(x) && "exec_id" %in% names(x)) ids <- c(ids, x$exec_id)
    }
  }
  # Os sidecars da sessão reservam IDs mesmo se a rodada anterior não finalizou.
  for (d in unique(c(
    if (length(output)) file.path(output, "02_painel_correcoes", "operacoes"),
    get0("MONITORA_CORRECOES_DIR", envir = envir, inherits = TRUE)
  ))) {
    nms <- if (dir.exists(d)) list.files(d, pattern = "^metadados_sessao_painel_[0-9]{8}_[0-9]{6}(_[0-9]{3,})?[.]csv$") else character()
    ids <- c(ids, sub("[.]csv$", "", sub("^metadados_sessao_painel_", "", nms)))
  }
  unique(ids[!is.na(ids) & nzchar(trimws(ids))])
}

monitora_v307_execucao_iniciar <- function(envir = .GlobalEnv, agora = Sys.time(), ids_reservados = character()) {
  if (!is.environment(envir)) stop("Ambiente da execução inválido.", call. = FALSE)
  ativa <- get0("MONITORA_V307_TRANSACAO_FINAL_ATIVA", envir = envir, inherits = FALSE)
  if (is.environment(ativa) && ativa$estado %in% c("aberta", "rollback_incompleto")) {
    stop("Nova execução bloqueada: há uma publicação transacional ainda aberta.", call. = FALSE)
  }
  reservados <- unique(c(monitora_v307_execucao_ids_reservados(envir), as.character(ids_reservados)))
  base <- format(agora, "%Y%m%d_%H%M%S")
  if (length(base) != 1L || is.na(base) || !grepl("^[0-9]{8}_[0-9]{6}$", base)) {
    stop("Instante inválido para identificar a execução.", call. = FALSE)
  }
  id <- base
  n <- 0L
  # Se só o último sufixo ficou no disco, os anteriores já foram usados.
  sufixados <- reservados[!is.na(reservados) & grepl(paste0("^", base, "_[0-9]{3,}$"), reservados)]
  if (length(sufixados)) {
    usados <- as.double(sub(paste0("^", base, "_"), "", sufixados))
    if (any(!is.finite(usados)) || max(usados) >= .Machine$integer.max) stop("Limite de identificadores da execução excedido.", call. = FALSE)
    n <- as.integer(max(usados)) + 1L
    id <- paste0(base, "_", sprintf("%03d", n))
  }
  while (id %in% reservados) {
    n <- n + 1L
    if (is.na(n)) stop("Limite de identificadores da execução excedido.", call. = FALSE)
    id <- paste0(base, "_", sprintf("%03d", n))
  }
  defaults <- monitora_v307_execucao_defaults()
  reservados_produtos <- monitora_v307_execucao_produtos_reservados()
  remover <- reservados_produtos[vapply(reservados_produtos, exists, logical(1L), envir = envir, inherits = FALSE)]
  if (length(remover)) rm(list = remover, envir = envir)
  for (nm in names(defaults)) assign(nm, defaults[[nm]], envir = envir)
  assign("MONITORA_EXEC_ID", id, envir = envir)
  assign("MONITORA_V307_TRANSACAO_FINAL_ATIVA", NULL, envir = envir)
  options(monitora.v307.execucao.ids = unique(c(reservados, id)), monitora.v307.justificativas.avisos = character())
  invisible(id)
}

# Canonicaliza também caminhos ainda inexistentes, resolvendo o ancestral físico.
monitora_v307_caminho_canonico <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("Caminho transacional vazio.", call. = FALSE)
  path <- path.expand(as.character(path))
  partes <- character()
  atual <- path
  while (!file.exists(atual) && !dir.exists(atual)) {
    superior <- dirname(atual)
    if (identical(atual, superior)) stop("Caminho transacional sem ancestral acessível: ", path, call. = FALSE)
    partes <- c(basename(atual), partes)
    atual <- superior
  }
  atual <- normalizePath(atual, winslash = "/", mustWork = TRUE)
  for (p in partes) atual <- file.path(atual, p)
  # Resolve . e .. inclusive depois do ancestral existente.
  pedacos <- strsplit(gsub("\\\\", "/", atual), "/", fixed = TRUE)[[1L]]
  final <- character()
  for (p in pedacos) {
    if (!nzchar(p) || p == ".") next
    if (p == "..") final <- head(final, -1L) else final <- c(final, p)
  }
  prefixo <- if (startsWith(atual, "//")) "//" else if (startsWith(atual, "/")) "/" else ""
  paste0(prefixo, paste(final, collapse = "/"))
}

monitora_v307_caminho_contido <- function(path, raiz) {
  if (.Platform$OS.type == "windows") { path <- tolower(path); raiz <- tolower(raiz) }
  identical(path, raiz) || startsWith(path, paste0(sub("/+$", "", raiz), "/"))
}

monitora_v307_transacao_destinos <- function(output_dir, caminho_saida = character(),
                                            envir = .GlobalEnv, arquivos = character(),
                                            raizes = character(), protegidos = character()) {
  output_dir <- monitora_v307_caminho_canonico(output_dir)
  caminho_saida <- as.character(caminho_saida)
  caminho_saida <- caminho_saida[!is.na(caminho_saida) & nzchar(caminho_saida)]
  caminho_saida <- vapply(caminho_saida, function(p) {
    if (dirname(p) %in% c(".", "")) file.path(output_dir, basename(p)) else p
  }, character(1L))
  cfg <- function(nm) {
    z <- get0(nm, envir = envir, inherits = TRUE)
    if (is.character(z)) z[!is.na(z) & nzchar(z)] else character()
  }
  base <- cfg("MONITORA_BASE_DIR")
  if (!length(base)) base <- dirname(output_dir)
  # O fluxo final muda cwd para output. A origem protegida acompanha a base
  # estável do projeto, nunca esse diretório de trabalho transitório.
  protegidos <- unique(c(protegidos, cfg("MONITORA_INPUT_DIR"), cfg("MONITORA_DADOSORIGINAIS_DIR"),
    file.path(base[1L], "dadosoriginais")))
  # Não se incorpora input/linhagem ao conjunto de saída nem por um alias físico.
  list(
    arquivos = unique(c(arquivos, caminho_saida)),
    raizes = unique(c(file.path(output_dir, c("01_produtos_dados", "02_painel_correcoes", "03_auditorias/persistencia")),
                      cfg("MONITORA_CORRECOES_DIR"), raizes)),
    superficiais = output_dir,
    protegidos = protegidos,
    output_dir = output_dir
  )
}

monitora_v307_validacao_exigir <- function(resultado, mensagem) {
  explicito <- is.list(resultado) && !is.data.frame(resultado) && !is.null(resultado$ok)
  if (identical(resultado, FALSE) || (explicito && !isTRUE(resultado$ok))) {
    detalhe <- if (explicito && length(resultado$erro)) as.character(resultado$erro)[1L] else ""
    stop(mensagem, if (nzchar(detalhe) && !is.na(detalhe)) paste0(" ", detalhe) else "", call. = FALSE)
  }
  invisible(TRUE)
}

monitora_v307_transacao_preflight <- function(envir = .GlobalEnv, verificacoes = list()) {
  consolidar <- get0("monitora_linhagem_metadados_sessoes_consolidar", envir = envir, inherits = TRUE)
  if (!is.function(consolidar)) stop("Preflight bloqueado: consolidador dos metadados de sessões indisponível.", call. = FALSE)
  metadados <- consolidar()
  validar <- get0("monitora_replay_validar_v2", envir = envir, inherits = TRUE)
  trilhas <- lapply(c("MONITORA_TRILHA_SEMANTICA_HERDADA", "MONITORA_TRILHA_SEMANTICA_REPLAY", "MONITORA_TRILHA_SEMANTICA_SESSAO"),
    function(nm) get0(nm, envir = envir, inherits = TRUE))
  if (any(vapply(trilhas, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1L)))) {
    if (!requireNamespace("data.table", quietly = TRUE) || !is.function(validar)) {
      stop("Preflight bloqueado: validador do ledger indisponível.", call. = FALSE)
    }
    ledger <- data.table::rbindlist(trilhas, use.names = TRUE, fill = TRUE)
    if ("event_item_id" %in% names(ledger)) ledger <- ledger[is.na(event_item_id) | !duplicated(event_item_id)]
    validar(ledger, abortar = TRUE)
  }
  for (f in verificacoes) {
    if (!is.function(f)) stop("Verificação adicional de preflight inválida.", call. = FALSE)
    monitora_v307_validacao_exigir(f(), "Preflight adicional bloqueou a publicação.")
  }
  invisible(metadados)
}

# Lista somente os destinos declarados. Links novos são removíveis no rollback,
# mas links preexistentes são recusados: não se presume propriedade do alvo.
monitora_v307_transacao_listar <- function(destinos, aceitar_links = FALSE) {
  arquivos <- links <- diretorios <- character()
  visitar <- function(path, recursivo) {
    link <- suppressWarnings(Sys.readlink(path))
    if (length(link) && !is.na(link) && nzchar(link)) {
      if (!aceitar_links) stop("Destino transacional é um link simbólico: ", path, call. = FALSE)
      links <<- c(links, path)
      return(invisible(NULL))
    }
    if (dir.exists(path)) {
      diretorios <<- c(diretorios, path)
      for (p in list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE)) {
        if (dir.exists(p) && !recursivo) next
        visitar(p, recursivo)
      }
    } else if (file.exists(path)) arquivos <<- c(arquivos, path)
    invisible(NULL)
  }
  for (p in destinos$raizes) visitar(p, TRUE)
  for (p in destinos$superficiais) visitar(p, FALSE)
  for (p in destinos$arquivos) {
    if (dir.exists(p)) stop("Esperava-se arquivo, mas o destino é diretório: ", p, call. = FALSE)
    visitar(p, FALSE)
  }
  list(arquivos = sort(unique(arquivos)), links = unique(links), diretorios = unique(diretorios))
}

monitora_v307_transacao_hash <- function(paths) {
  if (!length(paths)) return(character())
  hashes <- unname(as.character(tools::md5sum(paths)))
  if (anyNA(hashes)) stop("Não foi possível conferir os bytes de um destino transacional.", call. = FALSE)
  hashes
}

monitora_v307_transacao_diagnosticar <- function(tx, mensagem = "") {
  # Diagnóstico operacional, jamais um certificado de integridade/publicação.
  texto <- c(paste0("estado_transacao=", tx$estado), paste0("exec_id=", tx$exec_id),
    paste0("mensagem=", gsub("[\r\n]+", " ", mensagem)),
    paste0("backup=", tx$backup_dir), "O estado acima não certifica a publicação dos produtos.")
  try(writeLines(texto, file.path(tx$backup_dir, "diagnostico.txt"), useBytes = TRUE), silent = TRUE)
  invisible(file.path(tx$backup_dir, "diagnostico.txt"))
}

monitora_v307_transacao_iniciar <- function(destinos, preflight, envir = .GlobalEnv,
                                           diagnostico_dir = file.path(tempdir(), "monitora_v307_transacoes")) {
  if (!is.list(destinos) || !is.function(preflight)) stop("Contrato da transação inválido.", call. = FALSE)
  ativa <- get0("MONITORA_V307_TRANSACAO_FINAL_ATIVA", envir = envir, inherits = FALSE)
  if (is.environment(ativa) && ativa$estado %in% c("aberta", "rollback_incompleto")) stop("Publicação transacional já está aberta ou tem rollback incompleto.", call. = FALSE)
  for (cc in c("arquivos", "raizes", "superficiais", "protegidos")) {
    destinos[[cc]] <- unique(vapply(destinos[[cc]], monitora_v307_caminho_canonico, character(1L)))
  }
  alvos <- unique(c(destinos$arquivos, destinos$raizes, destinos$superficiais))
  for (alvo in alvos) for (p in destinos$protegidos) {
    if (monitora_v307_caminho_contido(alvo, p) || monitora_v307_caminho_contido(p, alvo)) {
      stop("Transação bloqueada: destino sobrepõe dados de entrada/originais: ", alvo, call. = FALSE)
    }
  }
  diagnostico_dir <- monitora_v307_caminho_canonico(diagnostico_dir)
  for (p in unique(c(alvos, destinos$protegidos))) {
    if (monitora_v307_caminho_contido(diagnostico_dir, p) || monitora_v307_caminho_contido(p, diagnostico_dir)) {
      stop("Diretório de diagnóstico deve ficar fora dos destinos e das entradas.", call. = FALSE)
    }
  }
  # A validação sem escrita vem antes até da criação do backup.
  monitora_v307_validacao_exigir(preflight(), "Preflight bloqueou a publicação.")
  antes <- monitora_v307_transacao_listar(destinos)
  tx <- new.env(parent = emptyenv())
  tx$estado <- "preparando"
  tx$envir <- envir
  tx$destinos <- destinos
  tx$exec_id <- as.character(get0("MONITORA_EXEC_ID", envir = envir, inherits = FALSE))[1L]
  tx$antes <- antes
  tx$hashes <- monitora_v307_transacao_hash(antes$arquivos)
  tx$memoria_nomes <- c(
    "MONITORA_LINHAGEM_MANIFESTO", "MONITORA_METADADOS_SESSOES_LINHAGEM_HERDADOS",
    "MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO", "MONITORA_V307_JUSTIFICATIVAS_BASE",
    "MONITORA_V307_JUSTIFICATIVAS_REPLAY", "MONITORA_V307_CACHE_VALIDADO",
    "MONITORA_AUDITORIA_SANITIZACOES_CONSOLIDADA",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_AUDITORIA",
    "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_RESUMO",
    "MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", "MONITORA_REGISTROS_VALIDADOS_GERADO"
  )
  tx$memoria_existe <- vapply(tx$memoria_nomes, exists, logical(1L), envir = envir, inherits = FALSE)
  tx$memoria <- lapply(tx$memoria_nomes, function(nm) serialize(get0(nm, envir = envir, inherits = FALSE), NULL))
  tx$ancestrais_ausentes <- character()
  for (alvo in c(destinos$raizes, destinos$superficiais, dirname(destinos$arquivos))) {
    atual <- alvo
    while (!dir.exists(atual) && !file.exists(atual)) {
      tx$ancestrais_ausentes <- unique(c(tx$ancestrais_ausentes, atual))
      atual <- dirname(atual)
    }
  }
  dir.create(diagnostico_dir, recursive = TRUE, showWarnings = FALSE)
  tx$backup_dir <- tempfile("transacao_", tmpdir = diagnostico_dir)
  if (!dir.create(tx$backup_dir, recursive = FALSE, showWarnings = FALSE)) stop("Não foi possível criar o backup transacional.", call. = FALSE)
  tx$backups <- file.path(tx$backup_dir, sprintf("arquivo_%06d", seq_along(antes$arquivos)))
  tryCatch({
    for (i in seq_along(antes$arquivos)) {
      if (!isTRUE(file.copy(antes$arquivos[i], tx$backups[i], copy.date = TRUE, copy.mode = TRUE)) ||
          !identical(monitora_v307_transacao_hash(tx$backups[i]), tx$hashes[i])) {
        stop("Não foi possível proteger o destino anterior: ", antes$arquivos[i], call. = FALSE)
      }
    }
    utils::write.table(data.frame(caminho = antes$arquivos, hash_anterior_md5 = tx$hashes, backup = tx$backups),
      file.path(tx$backup_dir, "destinos_anteriores.tsv"), sep = "\t", quote = TRUE, row.names = FALSE)
    saveRDS(list(exec_id = tx$exec_id, destinos = destinos, antes = antes, hashes_md5 = tx$hashes),
      file.path(tx$backup_dir, "escopo_recuperacao.rds"))
  }, error = function(e) {
    tx$estado <- "backup_incompleto_sem_publicacao"
    monitora_v307_transacao_diagnosticar(tx, conditionMessage(e))
    stop(e)
  })
  tx$estado <- "aberta"
  assign("MONITORA_V307_TRANSACAO_FINAL_ATIVA", tx, envir = envir)
  tx
}

monitora_v307_transacao_reverter <- function(tx, motivo = "") {
  if (!is.environment(tx) || !tx$estado %in% c("aberta", "rollback_incompleto")) {
    stop("Transação não está aberta para reversão.", call. = FALSE)
  }
  problemas <- character()
  tentar <- function(f) tryCatch(f(), error = function(e) { problemas <<- c(problemas, conditionMessage(e)); NULL })
  atual <- tentar(function() monitora_v307_transacao_listar(tx$destinos, aceitar_links = TRUE))
  if (!is.null(atual)) {
    for (p in unique(c(setdiff(atual$arquivos, tx$antes$arquivos), atual$links))) tentar(function() {
      if (unlink(p, force = TRUE) != 0L) stop("Falha ao remover destino novo: ", p, call. = FALSE)
    })
    # Diretórios criados durante a ação só são removidos quando vazios.
    novos_dirs <- setdiff(atual$diretorios, tx$antes$diretorios)
    for (p in novos_dirs[order(nchar(novos_dirs), decreasing = TRUE)]) {
      if (dir.exists(p) && !length(list.files(p, all.files = TRUE, no.. = TRUE))) tentar(function() {
        if (unlink(p, recursive = TRUE, force = TRUE) != 0L) stop("Falha ao remover diretório novo: ", p, call. = FALSE)
      })
    }
  }
  for (i in seq_along(tx$antes$arquivos)) tentar(function() {
    p <- tx$antes$arquivos[i]
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(tx$backups[i], p, overwrite = TRUE, copy.date = TRUE, copy.mode = TRUE)) ||
        !identical(monitora_v307_transacao_hash(p), tx$hashes[i])) {
      stop("Falha ao restaurar destino anterior: ", p, call. = FALSE)
    }
  })
  for (p in tx$antes$diretorios) tentar(function() {
    if (!dir.exists(p) && !dir.create(p, recursive = TRUE, showWarnings = FALSE)) stop("Falha ao restaurar diretório anterior: ", p, call. = FALSE)
  })
  for (p in tx$ancestrais_ausentes[order(nchar(tx$ancestrais_ausentes), decreasing = TRUE)]) {
    if (dir.exists(p) && !length(list.files(p, all.files = TRUE, no.. = TRUE))) tentar(function() {
      if (unlink(p, recursive = TRUE, force = TRUE) != 0L) stop("Falha ao remover ancestral novo: ", p, call. = FALSE)
    })
  }
  for (i in seq_along(tx$memoria_nomes)) {
    nm <- tx$memoria_nomes[i]
    if (tx$memoria_existe[i]) {
      valor <- unserialize(tx$memoria[[i]])
      if (requireNamespace("data.table", quietly = TRUE)) valor <- data.table::copy(valor)
      assign(nm, valor, envir = tx$envir)
    } else if (exists(nm, envir = tx$envir, inherits = FALSE)) rm(list = nm, envir = tx$envir)
  }
  tentar(function() {
    depois <- monitora_v307_transacao_listar(tx$destinos)
    if (!identical(depois$arquivos, tx$antes$arquivos) ||
        !identical(monitora_v307_transacao_hash(depois$arquivos), tx$hashes)) {
      stop("Conferência física do rollback divergiu do conjunto anterior.", call. = FALSE)
    }
  })
  tx$estado <- if (length(problemas)) "rollback_incompleto" else "revertida"
  monitora_v307_transacao_diagnosticar(tx, paste(c(motivo, problemas), collapse = " | "))
  if (!length(problemas)) assign("MONITORA_V307_TRANSACAO_FINAL_ATIVA", NULL, envir = tx$envir)
  if (length(problemas)) stop("Rollback incompleto; backups preservados em ", tx$backup_dir, ": ", paste(problemas, collapse = " | "), call. = FALSE)
  invisible(TRUE)
}

monitora_v307_transacao_confirmar <- function(tx, validar = function() TRUE) {
  if (!is.environment(tx) || !identical(tx$estado, "aberta") || !is.function(validar)) stop("Transação não está aberta para confirmação.", call. = FALSE)
  monitora_v307_validacao_exigir(validar(), "Validação final bloqueou a confirmação da publicação.")
  # Nenhuma auditoria de sucesso é emitida por este mecanismo de arquivos.
  tx$estado <- "confirmada"
  assign("MONITORA_V307_TRANSACAO_FINAL_ATIVA", NULL, envir = tx$envir)
  unlink(tx$backup_dir, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

monitora_v307_transacao_executar <- function(acao, destinos, preflight,
                                            validar = function() TRUE, envir = .GlobalEnv,
                                            diagnostico_dir = file.path(tempdir(), "monitora_v307_transacoes"),
                                            injetar_falha = function(etapa, tx) invisible(NULL)) {
  if (!is.function(acao) || !is.function(injetar_falha)) stop("Ação transacional inválida.", call. = FALSE)
  tx <- monitora_v307_transacao_iniciar(destinos, preflight, envir, diagnostico_dir)
  concluida <- FALSE
  erro <- NULL
  on.exit({
    if (!concluida && identical(tx$estado, "aberta")) {
      reversao <- tryCatch({ monitora_v307_transacao_reverter(tx, if (is.null(erro)) "execucao_interrompida" else conditionMessage(erro)); NULL }, error = identity)
      if (inherits(reversao, "error")) stop(if (is.null(erro)) conditionMessage(reversao) else paste(conditionMessage(erro), conditionMessage(reversao), sep = " | "), call. = FALSE)
    }
  }, add = TRUE)
  tryCatch({
    injetar_falha("antes_acao", tx)
    resultado <- acao(tx)
    injetar_falha("apos_acao", tx)
    injetar_falha("antes_confirmar", tx)
    monitora_v307_transacao_confirmar(tx, validar)
    concluida <- TRUE
    resultado
  }, error = function(e) { erro <<- e; stop(e) })
}
