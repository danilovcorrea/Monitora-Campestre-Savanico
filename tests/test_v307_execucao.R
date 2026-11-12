# Fixtures exclusivamente sintéticas; nenhuma entrada operacional é executada.
suppressPackageStartupMessages(library(data.table))
source("tools/v307/execucao.R")

testar_v307_execucao <- function() {
  anterior <- options("monitora.v307.execucao.ids")
  on.exit(options(anterior), add = TRUE)
  options(monitora.v307.execucao.ids = character())
  checks <- 0L
  check <- function(nome, ok) {
    if (!isTRUE(ok)) stop("FALHOU: ", nome, call. = FALSE)
    checks <<- checks + 1L
    cat("OK:", nome, "\n")
  }
  erro <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
  gravar <- function(path, texto) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(texto, path, useBytes = TRUE) }
  foto <- function(path) {
    if (!dir.exists(path)) return(list(arquivos = character(), hashes = character(), dirs = character()))
    arqs <- sort(list.files(path, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE))
    arqs <- arqs[!dir.exists(arqs)]
    list(arquivos = substring(arqs, nchar(path) + 2L), hashes = unname(tools::md5sum(arqs)),
      dirs = sort(substring(list.dirs(path, recursive = TRUE, full.names = TRUE), nchar(path) + 2L)))
  }
  diag <- Sys.getenv("MONITORA_V307_TEST_DIAG_DIR", unset = file.path(tempdir(), "v307_execucao_diagnosticos"))
  dir.create(diag, recursive = TRUE, showWarnings = FALSE)
  fixo <- as.POSIXct("2026-11-11 12:34:56", tz = "UTC")
  base <- format(fixo, "%Y%m%d_%H%M%S")
  env <- new.env(parent = globalenv())
  env$MONITORA_EXEC_ID <- base
  env$MONITORA_RESPONSAVEL_CORRECAO <- "configuracao_preservada"
  env$MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
  env$dados_usuario <- data.table(valor = 123)
  env$MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table(event_id = "antigo")
  env$MONITORA_TRILHA_SEMANTICA_REPLAY <- data.table(event_id = "replay_antigo")
  env$MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO <- list(historico = "antigo")
  env$MONITORA_IO_CACHE_INVENTARIO_INPUT <- list(hash = "antigo")
  env$MONITORA_V307_JUSTIFICATIVAS_BASE <- list(hash = "base_anterior")
  env$MONITORA_V307_JUSTIFICATIVAS_REPLAY <- list(hash = "replay_anterior")
  env$MONITORA_V307_CACHE_VALIDADO <- list(exec_id = "execucao_anterior", memoria_sha256 = "prova_anterior")
  env$registros_corrig_stat <- data.table(resultado = "anterior")
  env$reg_corrig_stat_summarise_p1_presence <- data.table(resultado = "anterior")
  env$MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_INDICES <- new.env()
  env$MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_INDICES$obsoleto <- 1L
  primeiro <- monitora_v307_execucao_iniciar(env, fixo)
  check("ID restaurado não é reutilizado no mesmo segundo", identical(primeiro, paste0(base, "_001")))
  check("Sessão e replay anteriores ficam vazios", !nrow(as.data.table(env$MONITORA_TRILHA_SEMANTICA_SESSAO)) && !nrow(as.data.table(env$MONITORA_TRILHA_SEMANTICA_REPLAY)))
  check("Caches de justificativas e de inventário são limpos", is.null(env$MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO) && is.null(env$MONITORA_IO_CACHE_INVENTARIO_INPUT))
  check("Base/replay de justificativas são limpos e produtos residuais deixam de existir", is.null(env$MONITORA_V307_JUSTIFICATIVAS_BASE) && is.null(env$MONITORA_V307_JUSTIFICATIVAS_REPLAY) && !exists("registros_corrig_stat", env, inherits = FALSE) && !exists("reg_corrig_stat_summarise_p1_presence", env, inherits = FALSE))
  check("Prova de cache validado não migra para um novo Source", is.null(env$MONITORA_V307_CACHE_VALIDADO))
  check("Cache contratual usa ambiente vazio novo", is.environment(env$MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_INDICES) && !length(ls(env$MONITORA_PUBLICACAO_AE_CACHE_CONTRATO_UNICO_INDICES)))
  check("Configuração e objetos do usuário permanecem", identical(env$MONITORA_RESPONSAVEL_CORRECAO, "configuracao_preservada") && identical(env$MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES, "S") && identical(env$dados_usuario, data.table(valor = 123)))
  # Simula recarga do módulo pelo segundo Source, sem depender de relógio/sleep.
  source("tools/v307/execucao.R", local = env)
  segundo <- env$monitora_v307_execucao_iniciar(env, fixo)
  check("Segundo Source gera ID novo sem mudar o instante", identical(segundo, paste0(base, "_002")))
  # A guarda secundária original mantém o ID gerado pela chamada incondicional.
  eval(quote(if (!exists("MONITORA_EXEC_ID", inherits = TRUE) || !nzchar(as.character(MONITORA_EXEC_ID)[1L])) MONITORA_EXEC_ID <- "indevido"), env)
  check("Guarda secundária preserva o mesmo ID", identical(env$MONITORA_EXEC_ID, segundo))
  check("ID novo não exige sufixo quando não há colisão", identical(monitora_v307_execucao_iniciar(new.env(parent = globalenv()), fixo + 1), format(fixo + 1, "%Y%m%d_%H%M%S")))
  check("Instante inválido não altera estado", nzchar(erro(monitora_v307_execucao_iniciar(env, as.POSIXct(NA)))) && identical(env$MONITORA_EXEC_ID, segundo))

  reservar <- tempfile("v307_ids_")
  dir.create(reservar)
  env2 <- new.env(parent = globalenv())
  env2$MONITORA_INPUT_DIR <- file.path(reservar, "input")
  env2$MONITORA_OUTPUT_DIR <- file.path(reservar, "output")
  gravar(file.path(env2$MONITORA_INPUT_DIR, "linhagem", "metadados_sessoes_painel_consolidado.csv"), c("exec_id", base))
  gravar(file.path(env2$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem", "manifesto_linhagem.json"), paste0('{"exec_id":"', base, '_001"}'))
  gravar(file.path(env2$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "operacoes", paste0("metadados_sessao_painel_", base, "_002.csv")), "fixture")
  options(monitora.v307.execucao.ids = character())
  antes_ids <- foto(reservar)
  check("Entrada, manifesto de saída e sidecar reservam IDs", identical(monitora_v307_execucao_iniciar(env2, fixo), paste0(base, "_003")))
  check("Reserva de IDs não escreve nas entradas/saídas", identical(foto(reservar), antes_ids))
  env3 <- new.env(parent = globalenv())
  env3$MONITORA_OUTPUT_DIR <- file.path(reservar, "output_apenas_manifesto")
  gravar(file.path(env3$MONITORA_OUTPUT_DIR, "manifesto_execucao_ultima_execucao.csv"), c("exec_id", paste0(base, "_005")))
  options(monitora.v307.execucao.ids = character())
  check("Processo novo não recicla timestamp ou sufixos anteriores ao último manifesto", identical(monitora_v307_execucao_iniciar(env3, fixo), paste0(base, "_006")))

  # Processo independente restaurando .RData não herda o registro de options.
  imagem <- file.path(reservar, "ambiente.RData")
  MONITORA_EXEC_ID <- base
  MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table(event_id = "residuo_rdata")
  save(MONITORA_EXEC_ID, MONITORA_TRILHA_SEMANTICA_SESSAO, file = imagem)
  script <- file.path(reservar, "novo_processo.R")
  writeLines(c(
    sprintf("source(%s)", encodeString(normalizePath("tools/v307/execucao.R"), quote = '"')),
    sprintf("load(%s)", encodeString(imagem, quote = '"')),
    'id <- monitora_v307_execucao_iniciar(agora = as.POSIXct("2026-11-11 12:34:56", tz="UTC"))',
    sprintf("stopifnot(identical(id, %s), is.null(MONITORA_TRILHA_SEMANTICA_SESSAO))", encodeString(paste0(base, "_001"), quote = '"')),
    'cat("PROCESSO_RDATA_OK\\n")'
  ), script)
  child <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)
  check("Processo novo com RData restaurado renova ID e sessão", is.null(attr(child, "status")) && any(grepl("PROCESSO_RDATA_OK", child, fixed = TRUE)))

  fixture <- function(existente) {
    p <- tempfile("v307_tx_")
    dir.create(p)
    out <- file.path(p, "output")
    e <- new.env(parent = globalenv())
    e$MONITORA_EXEC_ID <- paste0(base, "_fixture")
    e$MONITORA_INPUT_DIR <- file.path(p, "input")
    e$MONITORA_OUTPUT_DIR <- out
    e$MONITORA_LINHAGEM_MANIFESTO <- "manifesto_anterior"
    e$MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO <- data.table(valor = "memoria_anterior")
    e$MONITORA_V307_CACHE_VALIDADO <- list(exec_id = "publicacao_anterior", memoria_sha256 = "cache_anterior")
    arquivos <- file.path(out, c(
      "01_produtos_dados/registros_corrig.csv", "registros_corrig.csv",
      "02_painel_correcoes/linhagem/correcoes_semanticas_consolidada.csv",
      "02_painel_correcoes/linhagem/aplicacoes_correcoes.csv",
      "02_painel_correcoes/linhagem/metadados_sessoes_painel_consolidado.csv",
      "02_painel_correcoes/linhagem/inventario_sessoes_linhagem.csv",
      "02_painel_correcoes/linhagem/justificativas_pendencias_consolidada.csv",
      "02_painel_correcoes/linhagem/manifesto_linhagem.json",
      "02_painel_correcoes/operacoes/justificativas_pendencias_consolidada.csv",
      "02_painel_correcoes/operacoes/pendencias_justificadas.csv",
      "02_painel_correcoes/operacoes/justificativas_ocorrencias_aliases.csv",
      "03_auditorias/persistencia/auditoria_recuperacao_metadados_sessoes.csv",
      "indice_produtos.csv"
    ))
    externo <- file.path(p, "destino_explicito", "registros_corrig.csv")
    arquivos <- c(arquivos, externo)
    gravar(file.path(e$MONITORA_INPUT_DIR, "linhagem", "original.csv"), "linhagem_entrada_imutavel")
    gravar(file.path(p, "dadosoriginais", "original.csv"), "dados_originais_imutaveis")
    if (existente) {
      for (i in seq_along(arquivos)) gravar(arquivos[i], paste0("anterior_", i))
      gravar(file.path(out, "02_painel_correcoes", "antigo", "removido_pela_acao.txt"), "restaurar_tambem")
    }
    spec <- monitora_v307_transacao_destinos(out, externo, envir = e, protegidos = file.path(p, "dadosoriginais"))
    list(p = p, out = out, envir = e, arquivos = arquivos, destinos = spec)
  }
  for (existente in c(FALSE, TRUE)) {
    for (falhar_apos in 0:14) {
      f <- fixture(existente)
      antes <- foto(f$p)
      msg <- erro(monitora_v307_transacao_executar(
        acao = function(tx) {
          if (!falhar_apos) stop("falha_injetada_antes_writes", call. = FALSE)
          f$envir$MONITORA_LINHAGEM_MANIFESTO <- "manifesto_candidato"
          f$envir$MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO[, valor := "candidato"]
          f$envir$MONITORA_V307_CACHE_VALIDADO <- list(exec_id = "publicacao_candidata", memoria_sha256 = "cache_candidato")
          unlink(file.path(f$out, "02_painel_correcoes", "antigo"), recursive = TRUE)
          for (i in seq_along(f$arquivos)) {
            gravar(f$arquivos[i], paste0("novo_", i))
            gravar(file.path(f$out, "02_painel_correcoes", "nova_pasta", ".novo.txt"), "novo")
            if (i == falhar_apos) stop(paste0("falha_injetada_apos_", i), call. = FALSE)
          }
        }, destinos = f$destinos, preflight = function() TRUE,
        envir = f$envir, diagnostico_dir = diag
      ))
      if (!grepl("falha_injetada", msg, fixed = TRUE)) stop("Erro transacional inesperado: ", msg, call. = FALSE)
      check(paste("Rollback byte a byte e diretórios", if (existente) "existentes" else "novos", "após", falhar_apos, "escritas"),
        grepl("falha_injetada", msg, fixed = TRUE) && identical(foto(f$p), antes) &&
        identical(f$envir$MONITORA_LINHAGEM_MANIFESTO, "manifesto_anterior") &&
        identical(f$envir$MONITORA_V307_CACHE_VALIDADO, list(exec_id = "publicacao_anterior", memoria_sha256 = "cache_anterior")) &&
        identical(as.list(f$envir$MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO), list(valor = "memoria_anterior")))
    }
  }
  for (etapa in c("antes_acao", "apos_acao", "antes_confirmar")) {
    f <- fixture(TRUE)
    antes <- foto(f$p)
    msg <- erro(monitora_v307_transacao_executar(
      function(tx) { for (p in f$arquivos) gravar(p, "candidato") },
      f$destinos, function() TRUE, envir = f$envir, diagnostico_dir = diag,
      injetar_falha = function(atual, tx) if (atual == etapa) stop("hook_injetado", call. = FALSE)
    ))
    check(paste("Falha no limite transacional", etapa), grepl("hook_injetado", msg, fixed = TRUE) && identical(foto(f$p), antes))
  }
  f <- fixture(TRUE)
  antes <- foto(f$p)
  msg <- erro(monitora_v307_transacao_executar(function(tx) gravar(f$arquivos[1L], "novo"), f$destinos,
    function() TRUE, validar = function() FALSE, envir = f$envir, diagnostico_dir = diag))
  check("Validação final rejeitada restaura todo o conjunto", grepl("Validação final bloqueou", msg, fixed = TRUE) && identical(foto(f$p), antes))

  # Reutiliza o normalizador/consolidador produtivos, sem executar o monolito.
  prod <- parse("monitora_campsav_alvo_global_v3.0.6.R", keep.source = FALSE, encoding = "UTF-8")
  wanted <- c("monitora_linhagem_metadados_sessoes_template", "monitora_linhagem_metadados_sessoes_normalizar", "monitora_linhagem_metadados_sessoes_consolidar")
  carregar <- function(x, e) {
    if (!is.call(x)) return(invisible(NULL))
    if (identical(x[[1L]], as.name("<-")) && is.symbol(x[[2L]]) && as.character(x[[2L]]) %in% wanted) eval(x, e)
    else if (length(x) > 1L) for (i in 2:length(x)) carregar(x[[i]], e)
  }
  f <- fixture(TRUE)
  for (x in prod) carregar(x, f$envir)
  f$envir$MONITORA_METADADOS_SESSOES_SCHEMA_ATUAL <- "monitora_metadados_sessoes_v1"
  m <- data.table(metadados_schema_version = "monitora_metadados_sessoes_v1", exec_id = base,
    data_hora = "2026-11-11 10:00:00", build_sessao = "fixture", modo_execucao = "fixture",
    responsavel_sessao = "sintetico", instituicao = "fixture", acao_encerramento = "concluir",
    n_operacoes_sessao = "0", n_itens_auditaveis_sessao = "0", origem_metadados = "fixture", sidecar_sha256 = "")
  f$envir$MONITORA_METADADOS_SESSOES_LINHAGEM_HERDADOS <- copy(m)
  m[, data_hora := "2026-11-11 11:00:00"]
  f$envir$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- m
  antes <- foto(f$p)
  n_diag <- length(list.files(diag, all.files = TRUE, no.. = TRUE))
  foi_escrever <- FALSE
  msg <- erro(monitora_v307_transacao_executar(function(tx) { foi_escrever <<- TRUE; gravar(f$arquivos[1L], "indevido") },
    f$destinos, preflight = function() monitora_v307_transacao_preflight(f$envir), envir = f$envir, diagnostico_dir = diag))
  check("Conflito real de metadados bloqueia no preflight antes de writes/backup", grepl("exec_id com versões conflitantes", msg, fixed = TRUE) && !foi_escrever && identical(foto(f$p), antes) && length(list.files(diag, all.files = TRUE, no.. = TRUE)) == n_diag)
  f$envir$MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- copy(f$envir$MONITORA_METADADOS_SESSOES_LINHAGEM_HERDADOS)
  check("Mesmo conteúdo de metadados é aceito pelo preflight", !nzchar(erro(monitora_v307_transacao_preflight(f$envir))))
  antes <- foto(f$p)
  foi_escrever <- FALSE
  msg <- erro(monitora_v307_transacao_executar(function(tx) { foi_escrever <<- TRUE }, f$destinos,
    preflight = function() monitora_v307_transacao_preflight(f$envir, list(function() list(ok = FALSE, erro = "justificativa_fixture_invalida"))),
    envir = f$envir, diagnostico_dir = diag))
  check("Contrato ok=FALSE das justificativas bloqueia antes de writes", grepl("justificativa_fixture_invalida", msg, fixed = TRUE) && !foi_escrever && identical(foto(f$p), antes))

  f <- fixture(TRUE)
  antes <- foto(f$p)
  perigoso <- f$destinos
  perigoso$arquivos <- file.path(f$envir$MONITORA_INPUT_DIR, "linhagem", "original.csv")
  check("Arquivo em input/linhagem nunca entra no rollback", grepl("sobrepõe dados de entrada", erro(monitora_v307_transacao_iniciar(perigoso, function() TRUE, f$envir, diag)), fixed = TRUE) && identical(foto(f$p), antes))
  if (.Platform$OS.type != "windows") {
    link <- file.path(f$out, "02_painel_correcoes", "link_entrada")
    stopifnot(file.symlink(f$envir$MONITORA_INPUT_DIR, link))
    check("Link preexistente para entrada é recusado", grepl("link simbólico", erro(monitora_v307_transacao_iniciar(f$destinos, function() TRUE, f$envir, diag)), fixed = TRUE))
    unlink(link)
    antes <- foto(f$p)
    msg <- erro(monitora_v307_transacao_executar(function(tx) {
      stopifnot(file.symlink(f$envir$MONITORA_INPUT_DIR, link)); stop("falha_link_novo", call. = FALSE)
    }, f$destinos, function() TRUE, envir = f$envir, diagnostico_dir = diag))
    check("Rollback remove só link novo sem seguir a entrada", grepl("falha_link_novo", msg, fixed = TRUE) && identical(foto(f$p), antes))
  }

  f <- fixture(TRUE)
  gravados <- monitora_v307_transacao_executar(function(tx) {
    for (p in f$arquivos) gravar(p, "publicado")
    length(f$arquivos)
  }, f$destinos, function() TRUE, envir = f$envir, diagnostico_dir = diag)
  check("Sucesso publica todos os destinos e fecha a transação", identical(gravados, length(f$arquivos)) && all(vapply(f$arquivos, function(p) identical(readLines(p), "publicado"), logical(1L))) && is.null(f$envir$MONITORA_V307_TRANSACAO_FINAL_ATIVA))
  tx <- monitora_v307_transacao_iniciar(f$destinos, function() TRUE, f$envir, diag)
  check("Source durante transação aberta é bloqueado", grepl("publicação transacional ainda aberta", erro(monitora_v307_execucao_iniciar(f$envir, fixo)), fixed = TRUE))
  check("Transação aninhada independente é bloqueada", grepl("já está aberta", erro(monitora_v307_transacao_iniciar(f$destinos, function() TRUE, f$envir, diag)), fixed = TRUE))
  monitora_v307_transacao_reverter(tx, "fixture_sem_writes")

  f <- fixture(TRUE)
  msg <- erro(monitora_v307_transacao_executar(function(tx) {
    unlink(tx$backups[1L]); gravar(tx$antes$arquivos[1L], "candidato"); stop("erro_original", call. = FALSE)
  }, f$destinos, function() TRUE, envir = f$envir, diagnostico_dir = diag))
  tx <- f$envir$MONITORA_V307_TRANSACAO_FINAL_ATIVA
  check("Rollback incompleto preserva erro original e backups para recuperação", grepl("erro_original", msg, fixed = TRUE) && grepl("Rollback incompleto", msg, fixed = TRUE) && identical(tx$estado, "rollback_incompleto") && dir.exists(tx$backup_dir))
  check("Rollback incompleto impede novo Source e nova publicação", nzchar(erro(monitora_v307_execucao_iniciar(f$envir, fixo))) && nzchar(erro(monitora_v307_transacao_iniciar(f$destinos, function() TRUE, f$envir, diag))))
  diagnostico <- readLines(file.path(tx$backup_dir, "diagnostico.txt"))
  check("Diagnóstico de falha não certifica conjunto final", any(diagnostico == "estado_transacao=rollback_incompleto") && any(grepl("não certifica", diagnostico, fixed = TRUE)))

  # O catch produtivo pós-painel zera a marca de integridade, mas não substitui
  # a lista se o RHS da atribuição falhar. O Source seguinte deve apagar os dois
  # resultados antigos antes que os consumidores get0() possam reutilizá-los.
  blocos_pos <- list(); assigns_pos <- list(); persistir_pos <- NULL
  visitar_pos <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1L]])[1L]
    if (op == "tryCatch" && length(x) > 1L && grepl(
      "MONITORA_RELATORIOS_DIAGNOSTICOS_POS_PAINEL <-", paste(deparse(x[[2L]]), collapse = " "), fixed = TRUE
    )) blocos_pos[[length(blocos_pos) + 1L]] <<- x
    if (op == "<-" && length(x) == 3L && is.symbol(x[[2L]]) && as.character(x[[2L]]) %in% c("regs_pendencias_pos", "validacao_espacial_pos")) {
      assigns_pos[[as.character(x[[2L]])]] <<- x
    }
    if (op == "if" && length(x) >= 3L && identical(x[[2L]], quote(data.table::is.data.table(regs_pendencias_pos)))) persistir_pos <<- x
    if (length(x) > 1L) for (i in 2:length(x)) visitar_pos(x[[i]])
    invisible(NULL)
  }
  for (x in prod) visitar_pos(x)
  stopifnot(length(blocos_pos) == 1L, length(assigns_pos) == 2L, !is.null(persistir_pos))
  f <- fixture(TRUE)
  d <- f$envir
  d$MONITORA_RELATORIOS_DIAGNOSTICOS_POS_PAINEL <- list(registros = data.table(tipo_ocorrencia = "fixture_antiga", COLETA = "COLETA_SOURCE_ANTERIOR", linha_indice = 1L))
  d$MONITORA_VALIDACAO_ESPACIAL_POS_PAINEL_RESULTADO <- list(validacao = data.table(COLETA = "ESPACIAL_SOURCE_ANTERIOR"))
  monitora_v307_execucao_iniciar(d, fixo)
  check("Novo Source limpa somente os dois resultados pós-painel comprovadamente residuais", is.null(d$MONITORA_RELATORIOS_DIAGNOSTICOS_POS_PAINEL) && is.null(d$MONITORA_VALIDACAO_ESPACIAL_POS_PAINEL_RESULTADO))
  d$MONITORA_BASE_DIR_OCORRENCIAS_POS_PAINEL <- file.path(f$out, "ocorrencias_pos")
  d$registros_corrig <- data.table(COLETA = "COLETA_SOURCE_ATUAL")
  d$monitora_diag_rel_gerar_ocorrencias <- function(...) stop("falha_diagnostico_injetada", call. = FALSE)
  d$monitora_correcao_console_msg <- function(...) invisible(NULL)
  assign("MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK", TRUE, envir = .GlobalEnv)
  eval(blocos_pos[[1L]], envir = d)
  eval(assigns_pos$regs_pendencias_pos, envir = d)
  eval(assigns_pos$validacao_espacial_pos, envir = d)
  check("Falha no tryCatch produtivo deixa diagnóstico atual incompleto e sem resultado de outra execução", identical(get("MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK", .GlobalEnv), FALSE) && is.null(d$regs_pendencias_pos) && !nrow(d$validacao_espacial_pos))
  d$snapshot_escrito <- FALSE
  d$monitora_pendencias_justificativas_persistir <- function(...) { d$snapshot_escrito <- TRUE }
  eval(persistir_pos, envir = d)
  check("Guardião produtivo não materializa snapshot de ocorrência residual após falha", !d$snapshot_escrito)

  # O Source real muda cwd para output antes da exportação final. Proteger
  # cwd/dadosoriginais inventava um ancestral protegido dentro da própria saída.
  testar_cwd_output <- function() {
    wd_anterior <- getwd()
    on.exit(setwd(wd_anterior), add = TRUE)
    f <- fixture(TRUE)
    f$envir$MONITORA_BASE_DIR <- f$p
    antes <- foto(f$p)
    setwd(f$out)
    spec <- monitora_v307_transacao_destinos(f$out, "registros_corrig.csv", envir = f$envir)
    raiz_originais <- monitora_v307_caminho_canonico(file.path(f$p, "dadosoriginais"))
    pprotegidos <- vapply(spec$protegidos, monitora_v307_caminho_canonico, character(1L))
    check("cwd em output mantém dadosoriginais ancorado na BASE_DIR real", raiz_originais %in% pprotegidos && !file.path(f$out, "dadosoriginais") %in% pprotegidos)
    msg <- erro(monitora_v307_transacao_executar(function(tx) {
      gravar(file.path(f$out, "registros_corrig.csv"), "candidato_relativo")
      stop("falha_cwd_output", call. = FALSE)
    }, spec, function() TRUE, envir = f$envir, diagnostico_dir = diag))
    check("Exportação relativa com cwd output entra na transação e reverte normalmente", grepl("falha_cwd_output", msg, fixed = TRUE) && identical(foto(f$p), antes))
    spec_orig <- monitora_v307_transacao_destinos(f$out, file.path(f$p, "dadosoriginais", "original.csv"), envir = f$envir)
    check("cwd output não afrouxa a proteção dos originais reais", grepl("sobrepõe dados de entrada", erro(monitora_v307_transacao_iniciar(spec_orig, function() TRUE, f$envir, diag)), fixed = TRUE) && identical(foto(f$p), antes))
    sem_base <- new.env(parent = baseenv())
    sem_base$MONITORA_INPUT_DIR <- f$envir$MONITORA_INPUT_DIR
    sem_base$MONITORA_OUTPUT_DIR <- f$out
    spec_fallback <- monitora_v307_transacao_destinos(f$out, "registros_corrig.csv", envir = sem_base)
    check("Sem BASE_DIR explícita, fallback usa pai do output e independe do cwd", raiz_originais %in% vapply(spec_fallback$protegidos, monitora_v307_caminho_canonico, character(1L)))
  }
  testar_cwd_output()
  cat("TOTAL:", checks, "checagens aprovadas\nDIAGNOSTICOS:", normalizePath(diag), "\n")
}

testar_v307_execucao()
