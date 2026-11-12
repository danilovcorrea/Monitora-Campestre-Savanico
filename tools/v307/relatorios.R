# Isola apenas as fontes diretas de uma fase que será efetivamente regenerada.
# O resolvedor legado continua estrito; histórico fica fora do output ativo.
monitora_v307_relatorios_raizes <- function(base_dir, fase) {
  fase <- as.character(fase)[1L]
  if (is.na(fase) || !fase %in% c("pre_painel", "pre_painel_incremental", "pos_painel", "pos_correcoes"))
    stop("Fase de relatório não reconhecida para isolamento.", call. = FALSE)
  fase <- if (fase == "pos_correcoes") "pos_painel" else fase
  fases <- if (fase == "pre_painel_incremental") c("pre_painel", fase) else fase
  raiz <- if (basename(base_dir) %in% c("correcoes_campos", "02_painel_correcoes")) dirname(base_dir) else base_dir
  unique(unlist(lapply(fases, function(f) c(
    file.path(base_dir, if (startsWith(f, "pre_painel")) paste0("relatorios_", f) else "relatorios_pos_correcoes"),
    file.path(raiz, "02_painel_correcoes", "apoio", f),
    file.path(raiz, "02_painel_correcoes", "relatorios_apoio_tematicos", f),
    file.path(raiz, "02_painel_correcoes", "ap", f)
  )), use.names = FALSE))
}

monitora_v307_relatorios_preflight <- function(envir = environment(monitora_v307_relatorios_preflight)) {
  falhas <- getOption("monitora.v307.relatorios.falhas")
  exec_id <- get0("MONITORA_EXEC_ID", envir = envir, inherits = TRUE)
  atuais <- Filter(function(x) identical(x$exec_id, exec_id), falhas)
  if (length(atuais)) return(list(ok = FALSE, erro = paste0(
    "Geração de relatórios não concluída na execução atual: ",
    paste(vapply(atuais, function(x) paste0(x$fase, " (", x$erro, ")"), character(1L)), collapse = "; "))))
  TRUE
}

monitora_v307_relatorios_executar <- function(base_dir, fase, acao,
    envir = environment(monitora_v307_relatorios_executar),
    log_dir = get0("MONITORA_LOG_DIR", envir = envir, inherits = TRUE),
    injetar_falha = function(etapa, i, tx) invisible(NULL)) {
  if (!is.function(acao) || !is.function(injetar_falha)) stop("Ação de relatório inválida.", call. = FALSE)
  cfg <- function(nm) {
    x <- get0(nm, envir = envir, inherits = TRUE)
    if (is.character(x)) x[!is.na(x) & nzchar(x)] else character()
  }
  exec_id <- cfg("MONITORA_EXEC_ID")
  if (length(exec_id) != 1L || !grepl("^[0-9]{8}_[0-9]{6}(_[0-9]{3,})?$", exec_id))
    stop("Execução atual necessária para isolar relatórios.", call. = FALSE)
  chave <- if (fase == "pos_correcoes") "pos_painel" else fase
  marcar <- function(erro) {
    falhas <- Filter(function(x) identical(x$exec_id, exec_id), getOption("monitora.v307.relatorios.falhas"))
    falhas[chave] <- if (is.null(erro)) list(NULL) else list(list(exec_id = exec_id, fase = chave, erro = erro))
    falhas <- Filter(Negate(is.null), falhas)
    options(monitora.v307.relatorios.falhas = falhas)
    invisible(NULL)
  }
  marcar("geração iniciada e ainda não concluída")
  raizes <- monitora_v307_relatorios_raizes(base_dir, fase)
  output <- if (basename(base_dir) %in% c("correcoes_campos", "02_painel_correcoes")) dirname(base_dir) else base_dir
  if (!is.character(log_dir) || length(log_dir) != 1L || is.na(log_dir) || !nzchar(log_dir))
    stop("LOG_DIR necessário para preservar os relatórios anteriores.", call. = FALSE)
  historico_base <- monitora_v307_caminho_canonico(file.path(log_dir, "historico_relatorios"))
  if (monitora_v307_caminho_contido(historico_base, monitora_v307_caminho_canonico(output)))
    stop("Histórico de relatórios deve ficar fora do output ativo.", call. = FALSE)
  base <- cfg("MONITORA_BASE_DIR")
  if (!length(base)) base <- dirname(output)
  protegidos <- c(cfg("MONITORA_INPUT_DIR"), cfg("MONITORA_DADOSORIGINAIS_DIR"), file.path(base[1L], "dadosoriginais"))
  hash <- function(p) vapply(p, function(f) digest::digest(file = f, algo = "sha256"), character(1L), USE.NAMES = FALSE)
  historico <- NULL
  tx_atual <- NULL
  estado <- function(texto) if (!is.null(historico)) writeLines(c(
    paste0("exec_id=", exec_id), paste0("fase=", fase), texto,
    "Preservação operacional de arquivos anteriores à geração; não certifica produtos científicos."
  ), file.path(historico, "estado.txt"), useBytes = TRUE)
  resultado <- tryCatch(monitora_v307_transacao_executar(
    destinos = list(arquivos = file.path(base_dir, paste0("indice_apoio_", fase, ".csv")),
      raizes = character(), superficiais = raizes, protegidos = protegidos),
    preflight = function() TRUE, envir = envir,
    diagnostico_dir = file.path(log_dir, "transacoes_relatorios"),
    acao = function(tx) {
      tx_atual <<- tx
      pasta <- file.path(historico_base, paste0("antes_de_", exec_id))
      dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
      historico <<- tempfile(paste0(fase, "_"), tmpdir = pasta)
      if (!dir.create(historico)) stop("Falha ao criar histórico de relatórios.", call. = FALSE)
      origens <- tx$antes$arquivos[dirname(tx$antes$arquivos) %in% tx$destinos$superficiais]
      # O resolvedor só consulta arquivos diretos não ocultos; subpastas ficam.
      origens <- origens[!startsWith(basename(origens), ".")]
      copias <- file.path(historico, sprintf("arquivo_%06d", seq_along(origens)))
      mapa <- data.frame(exec_id = rep(exec_id, length(origens)), fase = rep(fase, length(origens)),
        origem = origens, origem_relativa = substring(origens, nchar(monitora_v307_caminho_canonico(output)) + 2L),
        arquivo = basename(origens), copia = copias,
        bytes = unname(file.info(origens)$size), sha256 = hash(origens), stringsAsFactors = FALSE)
      tx$historico_relatorios <- historico
      tx$mapa_relatorios <- mapa
      estado("estado_isolamento=preparando")
      for (i in seq_along(origens)) {
        injetar_falha("antes_copia", i, tx)
        if (!isTRUE(file.copy(origens[i], copias[i], copy.date = TRUE, copy.mode = TRUE)))
          stop("Falha ao preservar relatório anterior: ", origens[i], call. = FALSE)
        injetar_falha("apos_copia", i, tx)
        if (!identical(hash(copias[i]), mapa$sha256[i])) stop("Cópia de relatório divergiu do SHA256.", call. = FALSE)
      }
      utils::write.csv(mapa, file.path(historico, "mapa.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      injetar_falha("apos_mapa", 0L, tx)
      for (i in seq_along(origens)) {
        if (!identical(hash(origens[i]), mapa$sha256[i])) stop("Relatório mudou durante o isolamento.", call. = FALSE)
        injetar_falha("antes_retirar", i, tx)
        if (unlink(origens[i], force = TRUE) != 0L || file.exists(origens[i]))
          stop("Falha ao retirar relatório já preservado: ", origens[i], call. = FALSE)
        injetar_falha("apos_retirar", i, tx)
      }
      estado("estado_isolamento=preservado_fora_do_output")
      injetar_falha("antes_gerar", 0L, tx)
      resultado <- acao()
      injetar_falha("apos_gerar", 0L, tx)
      estado("estado_isolamento=geracao_concluida")
      resultado
    }
  ), error = function(e) {
    marcar(conditionMessage(e))
    try(estado(c(paste0("estado_isolamento=", if (is.null(tx_atual)) "nao_iniciado" else tx_atual$estado),
      paste0("erro=", conditionMessage(e)))), silent = TRUE)
    stop(e)
  })
  marcar(NULL)
  resultado
}
