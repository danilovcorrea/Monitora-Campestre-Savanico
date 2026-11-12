#!/usr/bin/env Rscript
# Integração de publicação, com AST e funções reais da candidata congelada.
# Os validadores biológicos opcionais não fazem parte destas fixtures mínimas;
# exportador, escritores, linhagem, justificativas, preflight e gate são reais.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[1L] else "monitora_campsav_alvo_global_v3.0.7.R"
source("tools/v307/carregar.R")
prototipo <- monitora_v307_carregar(script)
# Reaproveita somente o inventário de constantes da infraestrutura de testes.
# As definições e os loaders reais serão avaliados outra vez em Global, para
# reproduzir o ambiente de Source e não carregar closures presas ao protótipo.
invisible(list2env(as.list(prototipo, all.names = TRUE), envir = .GlobalEnv))
programa <- parse(script, keep.source = FALSE, encoding = "UTF-8")
carregar_definicoes_global <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]])) {
    rhs <- x[[3L]]
    if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) { eval(x, .GlobalEnv); return(invisible(NULL)) }
    if (is.symbol(rhs) && exists(as.character(rhs), .GlobalEnv, inherits = FALSE) && is.function(get(as.character(rhs), .GlobalEnv))) {
      eval(x, .GlobalEnv); return(invisible(NULL))
    }
  }
  if (length(x) > 1L) for (i in 2:length(x)) carregar_definicoes_global(x[[i]])
  invisible(NULL)
}
for (x in programa) carregar_definicoes_global(x)
linhas <- readLines(script, encoding = "UTF-8", warn = FALSE)
for (padrao in c("^eval[(]parse[(]text=rawToChar[(]memDecompress", "^base::eval\\(base::parse\\(text=rawToChar")) {
  inicio <- grep(padrao, linhas)
  stopifnot(length(inicio) == 1L)
  fim <- inicio + which(grepl("envir=environment", linhas[(inicio + 1L):length(linhas)], fixed = TRUE))[1L]
  eval(parse(text = paste(linhas[inicio:fim], collapse = "\n"), encoding = "UTF-8"), envir = .GlobalEnv)
}
if (exists("MONITORA_V307_DEFINICAO_COMPARACAO", envir = .GlobalEnv, inherits = FALSE))
  eval(MONITORA_V307_DEFINICAO_COMPARACAO, envir = .GlobalEnv)
e <- .GlobalEnv
originais <- mget(ls(prototipo, all.names = TRUE), envir = .GlobalEnv, inherits = FALSE)
baseline <- parse("monitora_campsav_alvo_global_v3.0.6.R", keep.source = FALSE, encoding = "UTF-8")
checks <- 0L
checar <- function(nome, ok) {
  if (!isTRUE(ok)) stop("FALHOU: ", nome, call. = FALSE)
  checks <<- checks + 1L
  cat("OK:", nome, "\n")
}
mensagem_erro <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
gravar <- function(path, x) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(x, path, useBytes = TRUE) }
foto <- function(path) {
  if (!dir.exists(path)) return(list(arquivos = character(), hashes = character(), dirs = character()))
  fs <- sort(list.files(path, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  fs <- fs[!dir.exists(fs)]
  list(arquivos = substring(fs, nchar(path) + 2L), hashes = unname(tools::md5sum(fs)),
    dirs = sort(substring(list.dirs(path, full.names = TRUE, recursive = TRUE), nchar(path) + 2L)))
}
visitar <- function(x, f) {
  if (!is.call(x)) return(invisible(NULL))
  if (isTRUE(f(x))) return(invisible(NULL))
  if (length(x) > 1L) for (i in 2:length(x)) visitar(x[[i]], f)
  invisible(NULL)
}
extrair_funcao <- function(arvore, nome) {
  resultado <- NULL
  for (x in arvore) visitar(x, function(z) {
    if (identical(z[[1L]], as.name("<-")) && length(z) == 3L && is.symbol(z[[2L]]) &&
        identical(as.character(z[[2L]]), nome) && is.call(z[[3L]]) && identical(z[[3L]][[1L]], as.name("function"))) {
      resultado <<- z[[3L]]; TRUE
    } else FALSE
  })
  stopifnot(!is.null(resultado))
  eval(resultado, new.env(parent = globalenv()))
}
wrappers <- list()
for (x in programa) visitar(x, function(z) {
  if (identical(z[[1L]], as.name("monitora_v307_transacao_executar"))) {
    txt <- paste(deparse(z), collapse = "\n")
    nome <- c("checkpoint", "pre_analises", "exportacao_final")[vapply(
      c("checkpoint_transacional", "pre_analises_transacional", "exportacao_final_transacional"),
      function(s) grepl(s, txt, fixed = TRUE), logical(1L))]
    if (length(nome) == 1L) wrappers[[nome]] <<- z
    TRUE
  } else FALSE
})
checar("Três pares produtivos têm wrapper transacional extraído da AST", setequal(names(wrappers), c("checkpoint", "pre_analises", "exportacao_final")))
checar("Núcleo real e registradores compartilham o ambiente Global usado pelo Source", all(vapply(c(
  "monitora_v307_execucao_iniciar", "monitora_v307_transacao_executar",
  "monitora_v307_justificativas_registrar_base", "monitora_v307_justificativas_persistir_preparado",
  "monitora_linhagem_finalizar", "monitora_linhagem_reassinar_pos_organizacao"
), function(nm) identical(environment(get(nm, .GlobalEnv)), .GlobalEnv), logical(1L))))

# O contrato global de hash permanece exatamente o da release, incluindo as
# marcas de encoding que o hash legado deliberadamente distingue.
hash_antigo <- extrair_funcao(baseline, "monitora_correcao_hash_texto")
checar("Corpo e assinatura do hash global são idênticos à baseline", identical(body(e$monitora_correcao_hash_texto), body(hash_antigo)) && identical(formals(e$monitora_correcao_hash_texto), formals(hash_antigo)))
for (marca in c("UTF-8", "unknown")) {
  valor <- enc2utf8("Exótica — justificativa, árvore e gramínea")
  Encoding(valor) <- marca
  checar(paste("Hash legado preservado com encoding", marca), identical(e$monitora_correcao_hash_texto(valor), hash_antigo(valor)))
}

# Coleta somente os dois pontos de inicialização de MONITORA_EXEC_ID, sem
# executar importações, instalar dependências nem abrir painel.
bloco_global <- if (length(programa) == 1L && is.call(programa[[1L]]) &&
  identical(programa[[1L]][[1L]], quote(base::evalq))) programa[[1L]][[2L]] else NULL
stopifnot(is.call(bloco_global), identical(bloco_global[[1L]], as.name("{")))
handlers <- Filter(function(z) is.call(z) && identical(z[[1L]], as.name("withCallingHandlers")), as.list(bloco_global)[-1L])
stopifnot(length(handlers) == 1L)
bloco_global <- handlers[[1L]][[2L]]
inicios <- Filter(function(z) is.call(z) && identical(z[[1L]], as.name("monitora_v307_execucao_iniciar")), as.list(bloco_global)[-1L])
guardas <- Filter(function(z) {
  if (!is.call(z) || !identical(z[[1L]], as.name("if")) || length(z) < 3L) return(FALSE)
  corpo <- z[[3L]]
  is.call(corpo) && identical(corpo[[1L]], as.name("{")) && length(corpo) == 2L &&
    is.call(corpo[[2L]]) && identical(corpo[[2L]][[1L]], as.name("<-")) && identical(corpo[[2L]][[2L]], as.name("MONITORA_EXEC_ID"))
}, as.list(bloco_global)[-1L])
checar("Candidata contém uma inicialização incondicional e uma guarda secundária", length(inicios) == 1L && length(guardas) == 1L)
contador <- 0L
real_iniciar <- e$monitora_v307_execucao_iniciar
assign("monitora_v307_execucao_iniciar", function(...) {
  contador <<- contador + 1L
  real_iniciar(agora = as.POSIXct("2026-11-11 12:00:00", tz = "UTC"))
}, .GlobalEnv)
MONITORA_EXEC_ID <- "20000101_000000"
MONITORA_TRILHA_SEMANTICA_SESSAO <- data.table(event_id = "estado_antigo")
for (z in c(inicios, guardas)) eval(z, .GlobalEnv)
id_1 <- MONITORA_EXEC_ID
checar("Primeiro Source inicializa uma única vez e limpa sessão residual", contador == 1L && id_1 != "20000101_000000" && is.null(MONITORA_TRILHA_SEMANTICA_SESSAO))
for (z in c(inicios, guardas)) eval(z, .GlobalEnv)
checar("Segundo Source inicializa uma única vez e troca o ID", contador == 2L && MONITORA_EXEC_ID != id_1)
assign("monitora_v307_execucao_iniciar", real_iniciar, .GlobalEnv)

diag <- Sys.getenv("MONITORA_V307_TEST_DIAG_DIR", unset = file.path(tempdir(), "v307_publicacao_integrada"))
dir.create(diag, recursive = TRUE, showWarnings = FALSE)
raiz <- tempfile("v307_publicacao_"); dir.create(raiz)
caso <- 0L
opcionais_fora_escopo <- c(
  "monitora_publicacao_ab_auditar_pendencias_impeditivas",
  "monitora_registros_validados_materializado_sem_bloqueio",
  "monitora_publicacao_z_selo_registros_corrig_valido",
  "monitora_publicacao_ab_logar_resumo_pendencias",
  "monitora_publicacao_ae_preparar_registros_corrig_para_csv",
  "monitora_produtos_resolver_pipes_por_ponto", "monitora_log_registrar_evento"
)
for (nm in opcionais_fora_escopo) {
  assign(nm, NULL, envir = e)
  assign(nm, NULL, envir = .GlobalEnv)
}
substituir <- function(nome, valor) {
  assign(nome, valor, envir = e)
  assign(nome, valor, envir = .GlobalEnv)
}
restaurar <- function(nomes) for (nm in nomes) substituir(nm, originais[[nm]])
nomes_injetados <- c("monitora_fwrite", "monitora_linhagem_finalizar", "monitora_linhagem_reassinar_pos_organizacao",
  "monitora_publicacao_aa_exportar_registros_corrig_aprovado", "monitora_v307_publicacao_validar",
  "monitora_correcao_auditar_releitura_pos_export")

metadados <- function(exec = MONITORA_EXEC_ID, hora = "2026-11-11 12:00:00 -0300") data.table(
  metadados_schema_version = "monitora_metadados_sessoes_v1", exec_id = exec,
  data_hora = hora, build_sessao = MONITORA_SCRIPT_BUILD_ID, modo_execucao = "fixture_publicacao",
  responsavel_sessao = "Fixture sintética", instituicao = "Fixture", acao_encerramento = "salvar_checkpoint_e_fechar",
  n_operacoes_sessao = "0", n_itens_auditaveis_sessao = "0", origem_metadados = "fixture", sidecar_sha256 = ""
)
nova_fixture <- function() {
  caso <<- caso + 1L
  restaurar(nomes_injetados)
  p <- file.path(raiz, paste0("caso_", caso)); dir.create(p)
  MONITORA_BASE_DIR <<- p
  MONITORA_INPUT_DIR <<- file.path(p, "input"); dir.create(MONITORA_INPUT_DIR)
  MONITORA_OUTPUT_DIR <<- file.path(p, "output"); dir.create(MONITORA_OUTPUT_DIR)
  MONITORA_CORRECOES_DIR <<- file.path(MONITORA_OUTPUT_DIR, "02_painel_correcoes", "operacoes")
  MONITORA_LOG_DIR <<- file.path(diag, paste0("caso_", caso)); dir.create(MONITORA_LOG_DIR, recursive = TRUE)
  e$monitora_v307_execucao_iniciar(ids_reservados = paste0("fixture_", caso))
  MONITORA_MODO_EXECUCAO <<- "fixture_publicacao"
  MONITORA_METADADOS_SESSOES_SCHEMA_ATUAL <<- "monitora_metadados_sessoes_v1"
  MONITORA_REPLAY_SCHEMA_ATUAL <<- "correcoes_semanticas_v2"
  MONITORA_GERAR_REGISTROS_VALIDADOS <<- FALSE
  MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <<- metadados()
  dados <- data.table(COLETA = c("C1", "C2"), MONITORA_ROW_ID = c("R1", "R2"),
    UC = "UC Fixture", EA = "EA1", UA = c("UA1", "UA2"), ANO = "2026", valor = c("anterior_1", "anterior_2"))
  fwrite(dados, file.path(MONITORA_INPUT_DIR, "registros_corrig.csv"))
  MONITORA_LINHAGEM_INPUT_SHA256 <<- e$monitora_v307_justificativas_hash(file.path(MONITORA_INPUT_DIR, "registros_corrig.csv"), arquivo = TRUE)
  e$monitora_v307_justificativas_registrar_base(dados, origem = "fixture_integrada")
  oc <- data.table(tipo_ocorrencia = "seca_morta_em_revisao", rotulo_ocorrencia = "Condição de revisão",
    COLETA = c("C1", "C2"), monitora_row_id = c("R1", "R2"), linha_indice = 1:2,
    UC = "UC Fixture", EA = "EA1", UA = c("UA1", "UA2"), ANO = "2026",
    CICLO = "1", CAMPANHA = "1", ocorrencia_token = "graminoide", ocorrencia_atributo = "fixture", ocorrencia_categoria = "seca_morta")
  oc[, ocorrencia_id := e$monitora_pendencias_ocorrencia_id(.SD)]
  lote <- suppressWarnings(e$monitora_pendencias_justificativas_criar_lote(oc[1L], "Fixture sintética", "pendencia_legitima",
    "Justificativa sintética inicial suficiente para esta validação.", "2026-11-11 12:01:00.000001"))
  if (!isTRUE(lote$ok)) stop(lote$erro)
  prep <- suppressWarnings(e$monitora_pendencias_justificativas_preparar(oc, lote$dados))
  suppressWarnings(e$monitora_pendencias_justificativas_persistir_preparado(prep))
  list(p = p, dados = dados, ocorrencias = oc)
}
contexto <- function(f, dados = f$dados) {
  env <- new.env(parent = e)
  env$obj <- dados
  env$produto <- "registros_corrig.csv"
  env$caminho_produto <- file.path(MONITORA_OUTPUT_DIR, "registros_corrig.csv")
  env$registros_corrig <- dados
  env$caminho_registros_corrig_preanalises <- file.path(MONITORA_OUTPUT_DIR, "registros_corrig.csv")
  env$caminho_registros_corrig_export <- "registros_corrig.csv"
  env
}
publicar <- function(nome, f, dados = f$dados) {
  wd_anterior <- getwd()
  on.exit(setwd(wd_anterior), add = TRUE)
  # Replica o setwd produtivo anterior à exportação final, ausente nos demais pares.
  if (identical(nome, "exportacao_final")) setwd(MONITORA_OUTPUT_DIR)
  invisible(suppressWarnings(eval(wrappers[[nome]], envir = contexto(f, dados))))
}
manifesto <- function() e$monitora_v307_justificativas_json_ler(file.path(MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem", "manifesto_linhagem.json"))

# Caminhos produtivos reais: publicação, reassinatura física e gate final.
for (nome in names(wrappers)) {
  f <- nova_fixture()
  entrada_antes <- foto(MONITORA_INPUT_DIR)
  dados_antes <- as.list(copy(f$dados))
  ordem <- character()
  for (item in list(c("monitora_publicacao_aa_exportar_registros_corrig_aprovado", "exportar"),
      c("monitora_linhagem_finalizar", "finalizar"),
      c("monitora_linhagem_reassinar_pos_organizacao", "reassinar"), c("monitora_v307_publicacao_validar", "validar"))) local({
    fn <- originais[[item[1L]]]; rotulo <- item[2L]
    substituir(item[1L], function(...) { ordem <<- c(ordem, rotulo); fn(...) })
  })
  publicar(nome, f)
  man <- manifesto()
  checar(paste(nome, "publica com funções reais e valida o conjunto completo"),
    isTRUE(e$monitora_v307_publicacao_validar(MONITORA_OUTPUT_DIR)) && identical(as.character(man$exec_id), MONITORA_EXEC_ID) && identical(as.integer(man$justificativas_event_count), 1L))
  checar(paste(nome, "executa finalizar e reassinar antes do gate"), identical(ordem[1:4], c("exportar", "finalizar", "reassinar", "validar")))
  checar(paste(nome, "exportador não altera a tabela entregue por referência"), identical(as.list(f$dados), dados_antes))
  checar(paste(nome, "preserva input e seu conteúdo"), identical(foto(MONITORA_INPUT_DIR), entrada_antes))
}

# Conflito no normalizador real é detectado pelo preflight de cada wrapper,
# antes de chamar o exportador, finalizador ou escritor do conjunto final.
for (nome in names(wrappers)) {
  f <- nova_fixture(); publicar(nome, f)
  antes <- foto(MONITORA_OUTPUT_DIR)
  MONITORA_METADADOS_SESSAO_PAINEL_ULTIMA <- metadados(hora = "2026-11-11 13:00:00 -0300")
  escritos <- 0L
  real_writer <- originais$monitora_fwrite
  substituir("monitora_fwrite", function(...) { escritos <<- escritos + 1L; real_writer(...) })
  msg <- mensagem_erro(publicar(nome, f, copy(f$dados)[, valor := "candidato"]))
  checar(paste(nome, "conflito de metadados bloqueia antes de qualquer escrita final"),
    grepl("exec_id com versões conflitantes", msg, fixed = TRUE) && escritos == 0L && identical(foto(MONITORA_OUTPUT_DIR), antes))
}

# Falha dentro do finalizador real, após materialização parcial de linhagem.
for (nome in names(wrappers)) for (etapa in c("aplicacoes", "apos_finalizar", "apos_reassinar")) {
  f <- nova_fixture(); publicar(nome, f)
  antes <- foto(MONITORA_OUTPUT_DIR)
  entrada_antes <- foto(MONITORA_INPUT_DIR)
  ultimo_antes <- copy(MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO)
  novo <- copy(f$dados)[, valor := c("candidato_1", "candidato_2")]
  if (etapa == "aplicacoes") {
    real_writer <- originais$monitora_fwrite
    substituir("monitora_fwrite", function(x, file = "", ...) {
      out <- real_writer(x, file, ...)
      if (basename(file) == "aplicacoes_correcoes.csv") stop("falha_injetada_aplicacoes", call. = FALSE)
      out
    })
  } else if (etapa == "apos_finalizar") {
    # Nova justificativa é publicada com as APIs reais DENTRO do guardião;
    # assim há bytes distintos para provar também a reversão das operações.
    lote2 <- suppressWarnings(e$monitora_pendencias_justificativas_criar_lote(f$ocorrencias[2L], "Fixture sintética", "pendencia_legitima",
      "Segunda justificativa sintética independente para o ensaio.", "2026-11-11 12:02:00.000001"))
    if (!isTRUE(lote2$ok)) stop(lote2$erro)
    prep2 <- suppressWarnings(e$monitora_pendencias_justificativas_preparar(f$ocorrencias, lote2$dados))
    fn <- originais$monitora_linhagem_finalizar
    substituir("monitora_linhagem_finalizar", function(...) {
      e$monitora_pendencias_justificativas_persistir_preparado(prep2)
      fn(...)
      stop("falha_injetada_apos_finalizar", call. = FALSE)
    })
  } else {
    fn <- originais$monitora_linhagem_reassinar_pos_organizacao
    substituir("monitora_linhagem_reassinar_pos_organizacao", function(...) { fn(...); stop("falha_injetada_apos_reassinar", call. = FALSE) })
  }
  msg <- mensagem_erro(publicar(nome, f, novo))
  checar(paste(nome, "rollback real preserva dados/linhagem/justificativas", etapa),
    grepl(paste0("falha_injetada_", etapa), msg, fixed = TRUE) && identical(foto(MONITORA_OUTPUT_DIR), antes))
  checar(paste(nome, "rollback mantém entrada e estado publicado anterior", etapa),
    identical(foto(MONITORA_INPUT_DIR), entrada_antes) && identical(MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO, ultimo_antes) && is.null(MONITORA_V307_TRANSACAO_FINAL_ATIVA))
}

# A revisão inclui o conteúdo durável das justificativas, sem introduzir ciclo
# de hash com o manifesto que registra a própria revisão.
f <- nova_fixture(); publicar("pre_analises", f)
man1 <- manifesto()
dados_hash <- man1$output_data_sha256
ledger_hash <- man1$ledger_sha256
lote2 <- suppressWarnings(e$monitora_pendencias_justificativas_criar_lote(f$ocorrencias[2L], "Fixture sintética", "pendencia_legitima",
  "Segunda decisão somente documental, sem alterar os dados biológicos.", "2026-11-11 12:02:00.000001"))
stopifnot(lote2$ok)
prep2 <- suppressWarnings(e$monitora_pendencias_justificativas_preparar(f$ocorrencias, lote2$dados))
invisible(suppressWarnings(e$monitora_pendencias_justificativas_persistir_preparado(prep2)))
publicar("pre_analises", f)
man2 <- manifesto()
checar("Justificativa nova muda revision_id mantendo dados e ledger idênticos", !identical(man1$revision_id, man2$revision_id) && identical(dados_hash, man2$output_data_sha256) && identical(ledger_hash, man2$ledger_sha256) && identical(as.integer(man2$justificativas_event_count), 2L))
rev <- man2$revision_id
e$monitora_linhagem_reassinar_pos_organizacao(MONITORA_OUTPUT_DIR, contexto = "teste_idempotencia")
checar("Reassinatura sem mudança física mantém a revisão e valida assinaturas", identical(manifesto()$revision_id, rev) && isTRUE(e$monitora_v307_publicacao_validar(MONITORA_OUTPUT_DIR)))

# Fechamento editorial real após publicar: um nome de evidência citado dentro
# de uma justificativa é texto de decisão imutável, mesmo que o arquivo citado
# seja movido. O filtro também protege fontes brutas e provas de recuperação.
protegidos <- c(
  "output/02_painel_correcoes/linhagem/correcoes_semanticas_consolidada.csv",
  "output/02_painel_correcoes/linhagem/manifesto_linhagem.json",
  "output/correcoes_campos/linhagem/inventario_sessoes_linhagem.csv",
  "output/02_painel_correcoes/operacoes/justificativas_pendencias_consolidada.csv",
  "output/02_painel_correcoes/operacoes/pendencias_justificadas.csv",
  "output/correcoes_campos/pendencias_remanescentes_com_justificativas.csv",
  "output/02_painel_correcoes/operacoes/manifesto_recuperacao_justificativas.json",
  "output/02_painel_correcoes/operacoes/justificativas_fontes/hash.csv",
  "output/justificativas/prova_original.csv", "C:\\output\\02_painel_correcoes\\linhagem\\ledger.csv",
  "output/02_painel_correcoes/operacoes/correcoes_semanticas_sessao_staging.csv",
  "output/02_painel_correcoes/operacoes/metadados_sessao_painel_20990101_010101.csv"
)
mutaveis <- c("output/03_auditorias/importacao/auditoria_referencias.csv", "output/README_OUTPUT.txt", "output/07_validacao/relatorio.md")
checar("Filtro editorial protege linhagem/decisões/fontes/provas em paths Unix e Windows",
  identical(e$monitora_v307_editorial_candidatos_mutaveis(c(protegidos, mutaveis)), mutaveis))
f <- nova_fixture()
lote2 <- suppressWarnings(e$monitora_pendencias_justificativas_criar_lote(f$ocorrencias[2L], "Fixture sintética", "pendencia_legitima",
  "Conferir auditoria_fixture.csv para documentação desta ocorrência.", "2026-11-11 12:02:00.000001"))
stopifnot(lote2$ok)
prep2 <- suppressWarnings(e$monitora_pendencias_justificativas_preparar(f$ocorrencias, lote2$dados))
invisible(suppressWarnings(e$monitora_pendencias_justificativas_persistir_preparado(prep2)))
publicar("pre_analises", f)
antes_man <- manifesto()
antes_hist <- e$monitora_v307_justificativas_ler_historico(avisar = FALSE)
antes_exec <- MONITORA_EXEC_ID
antes_inventario <- fread(file.path(MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem", "inventario_sessoes_linhagem.csv"), colClasses = "character")
arquivos_antes <- list.files(MONITORA_OUTPUT_DIR, recursive = TRUE, full.names = TRUE)
arquivos_antes <- arquivos_antes[e$monitora_v307_editorial_imutavel(arquivos_antes)]
hashes_antes <- vapply(arquivos_antes, function(p) digest::digest(file = p, algo = "sha256"), character(1L))
gravar(file.path(MONITORA_OUTPUT_DIR, "auditoria_fixture.csv"), c("evidencia", "fixture"))
e$monitora_output_organizar_produtos(MONITORA_OUTPUT_DIR, MONITORA_EXEC_ID, contexto = "fechamento_final_fixture")
depois_man <- manifesto()
depois_hist <- e$monitora_v307_justificativas_ler_historico(avisar = FALSE)
depois_inventario <- fread(file.path(MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem", "inventario_sessoes_linhagem.csv"), colClasses = "character")
checar("Organizador real preserva bytes de todas as decisões e fontes já assinadas",
  all(file.exists(arquivos_antes)) && identical(vapply(arquivos_antes, function(p) digest::digest(file = p, algo = "sha256"), character(1L)), hashes_antes))
checar("Organizador real mantém texto/eventos/revisão e gate físico válido",
  identical(as.data.frame(antes_hist), as.data.frame(depois_hist)) &&
  any(grepl("Conferir auditoria_fixture.csv", depois_hist$justificativa, fixed = TRUE)) &&
  identical(antes_man$revision_id, depois_man$revision_id) && isTRUE(e$monitora_v307_publicacao_validar(MONITORA_OUTPUT_DIR)))
checar("Fechamento não cria outro exec_id nem duplica inventário de sessões",
  identical(MONITORA_EXEC_ID, antes_exec) && identical(as.data.frame(antes_inventario), as.data.frame(depois_inventario)) && !anyDuplicated(depois_inventario$exec_id))

restaurar(nomes_injetados)
cat("TOTAL:", checks, "checagens aprovadas\nCANDIDATA:", normalizePath(script), "\nSHA256:", digest::digest(file = script, algo = "sha256"), "\nDIAGNOSTICOS:", normalizePath(diag), "\n")
