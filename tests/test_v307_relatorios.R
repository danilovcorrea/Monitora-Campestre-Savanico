#!/usr/bin/env Rscript
# Funções reais e falhas de IO com fixtures próprias; nunca altera dados de uso.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
script <- if (length(args)) args[1L] else "monitora_campsav_alvo_global_v3.0.7.R"
modulo <- "--modulo" %in% args
source("tools/v307/carregar.R")
e <- monitora_v307_carregar(script)
if (modulo) {
  source("tools/v307/relatorios.R", local = e)
  # Prova anterior à integração: mesma transformação estrutural proposta ao
  # builder. Sem --modulo é obrigatório usar somente os hooks da candidata.
  w <- e$monitora_relatorios_suporte_painel_gravar
  corpo <- as.list(body(w))
  i <- which(vapply(corpo, function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    identical(x[[2L]], as.name("fase_dir")), logical(1L)))
  stopifnot(length(i) == 1L)
  cauda <- as.call(c(list(as.name("{")), corpo[(i + 1L):length(corpo)]))
  propagar <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1L]], as.name("tryCatch")) && "error" %in% names(x)) {
      f <- x[["error"]]
      if (is.call(f) && identical(f[[1L]], as.name("function"))) {
        f[[3L]] <- as.call(c(as.list(f[[3L]]), list(quote(stop(e)))))
        x[["error"]] <- f
      }
    }
    if (length(x) > 1L) for (j in 2:length(x)) x[[j]] <- propagar(x[[j]])
    x
  }
  cauda <- propagar(cauda)
  acao <- as.call(list(as.name("function"), as.pairlist(list()), cauda))
  chamada <- as.call(list(as.name("monitora_v307_relatorios_executar"), as.name("base_dir"), as.name("fase"), acao))
  body(w) <- as.call(c(corpo[seq_len(i)], list(chamada)))
  e$monitora_relatorios_suporte_painel_gravar <- w
}
stopifnot(is.function(e$monitora_v307_relatorios_executar))
stopifnot(grepl("monitora_v307_relatorios_executar", paste(deparse(body(e$monitora_relatorios_suporte_painel_gravar)), collapse = ""), fixed = TRUE))
checks <- 0L
checar <- function(nome, ok) {
  if (!isTRUE(ok)) stop("FALHOU: ", nome, call. = FALSE)
  checks <<- checks + 1L; cat("OK:", nome, "\n")
}
erro <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
gravar <- function(path, texto) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(texto, path, useBytes = TRUE) }
hash <- function(p) unname(vapply(p, function(x) digest::digest(file = x, algo = "sha256"), character(1L)))
foto <- function(p) {
  f <- sort(list.files(p, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  f <- f[!dir.exists(f)]
  setNames(hash(f), substring(f, nchar(p) + 2L))
}
orig_options <- getOption("monitora.v307.relatorios.falhas")
diag_root <- Sys.getenv("MONITORA_V307_TEST_DIAG_DIR", unset = tempdir())
dir.create(diag_root, recursive = TRUE, showWarnings = FALSE)
contador <- 0L
nova <- function() {
  contador <<- contador + 1L
  p <- tempfile("v307_relatorios_", tmpdir = diag_root); dir.create(p)
  e$MONITORA_BASE_DIR <- p
  e$MONITORA_OUTPUT_DIR <- file.path(p, "output")
  e$MONITORA_INPUT_DIR <- file.path(p, "input")
  e$MONITORA_LOG_DIR <- file.path(p, "log")
  e$MONITORA_CORRECOES_DIR <- file.path(p, "output", "correcoes_campos")
  e$MONITORA_EXEC_ID <- paste0("20261111_120000_", sprintf("%03d", contador))
  e$MONITORA_V307_TRANSACAO_FINAL_ATIVA <- NULL
  e$MONITORA_LOG_EXECUCAO <- data.table(data_hora = character(), etapa = character(), severidade = character(), arquivo = character(), detalhe = character(), acao = character())
  options(monitora.v307.relatorios.falhas = NULL)
  gravar(file.path(p, "input", "original.csv"), "entrada preservada")
  gravar(file.path(p, "dadosoriginais", "original.csv"), "dados originais preservados")
  gravar(file.path(p, "output", "02_painel_correcoes", "operacoes", "justificativas.csv"), "decisao imutavel")
  gravar(file.path(p, "output", "02_painel_correcoes", "linhagem", "manifesto_linhagem.json"), "linhagem imutavel")
  p
}
semear <- function(fase = "pre_painel") {
  rs <- e$monitora_v307_relatorios_raizes(e$MONITORA_CORRECOES_DIR, fase)
  for (i in seq_along(rs)) gravar(file.path(rs[i], "relatorio_ocorrencia_formas_vida_desconhecida.txt"), c("RELATORIO", paste0("Execução: anterior_", i)))
  invisible(rs)
}
executar <- function(fase, acao, ...) e$monitora_v307_relatorios_executar(e$MONITORA_CORRECOES_DIR, fase, acao, ...)

p <- nova(); rs <- semear()
entrada <- foto(file.path(p, "input")); originais <- foto(file.path(p, "dadosoriginais"))
ops <- foto(file.path(p, "output", "02_painel_correcoes", "operacoes"))
lin <- foto(file.path(p, "output", "02_painel_correcoes", "linhagem"))
gravar(file.path(rs[1L], "ocorrencias", "diagnostico.csv"), "diagnostico preservado")
diag <- hash(file.path(rs[1L], "ocorrencias", "diagnostico.csv"))
anteriores <- unlist(lapply(rs, function(r) list.files(r, full.names = TRUE, recursive = FALSE)))
anteriores <- anteriores[!dir.exists(anteriores)]
sha_anteriores <- hash(anteriores)
checar("Reproduz conflito real entre legado e organizado", grepl("fontes divergentes", erro(e$monitora_relatorios_resolver_fase(e$MONITORA_CORRECOES_DIR, "pre_painel")), fixed = TRUE))
resultado <- executar("pre_painel", function() {
  checar("Arquivos de todas as raízes anteriores saíram antes do escritor", !any(file.exists(anteriores)))
  gravar(file.path(rs[1L], "relatorio_ocorrencia_formas_vida_desconhecida.txt"), c("RELATORIO", paste0("Execução: ", e$MONITORA_EXEC_ID)))
  73L
})
checar("Devolve o resultado do escritor", identical(resultado, 73L))
resolvidos <- e$monitora_relatorios_resolver_fase(e$MONITORA_CORRECOES_DIR, "pre_painel")
checar("Resolvedor intacto vê apenas a geração corrente", length(resolvidos) == 1L && grepl(e$MONITORA_EXEC_ID, paste(readLines(resolvidos), collapse = ""), fixed = TRUE))
mapas <- list.files(e$MONITORA_LOG_DIR, "^mapa.csv$", recursive = TRUE, full.names = TRUE)
mapa <- read.csv(mapas, stringsAsFactors = FALSE, check.names = FALSE)
checar("Backup permanente preserva todos os bytes e seu mapa", identical(hash(mapa$copia), mapa$sha256) && setequal(mapa$sha256, sha_anteriores) && setequal(mapa$origem, anteriores))
checar("Histórico está fora do output ativo", !any(startsWith(mapa$copia, e$MONITORA_OUTPUT_DIR)))
checar("Subpastas diagnósticas preservadas", identical(hash(file.path(rs[1L], "ocorrencias", "diagnostico.csv")), diag))
checar("Operações, linhagem e entradas preservadas", identical(foto(file.path(p, "input")), entrada) && identical(foto(file.path(p, "dadosoriginais")), originais) && identical(foto(file.path(p, "output", "02_painel_correcoes", "operacoes")), ops) && identical(foto(file.path(p, "output", "02_painel_correcoes", "linhagem")), lin))
checar("Sucesso libera preflight", isTRUE(e$monitora_v307_relatorios_preflight()))
executar("pre_painel", function() gravar(file.path(rs[1L], "novo.csv"), "geracao repetida"))
checar("Repetir fase preserva geração anterior em histórico distinto", length(list.files(e$MONITORA_LOG_DIR, "^mapa.csv$", recursive = TRUE)) == 2L)

for (etapa in c("antes_copia", "apos_copia", "apos_mapa", "apos_retirar", "apos_gerar")) {
  p <- nova(); rs <- semear()
  indice <- file.path(e$MONITORA_CORRECOES_DIR, "indice_apoio_pre_painel.csv")
  gravar(indice, "indice anterior"); antes <- foto(e$MONITORA_OUTPUT_DIR)
  msg <- erro(executar("pre_painel", function() { gravar(file.path(rs[1L], "novo.csv"), "novo"); gravar(indice, "indice novo") },
    injetar_falha = function(atual, i, tx) if (atual == etapa && (i == 0L || i == 2L)) stop("falha injetada")))
  checar(paste("Rollback de falha em", etapa), grepl("falha injetada", msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))
  checar(paste("Falha bloqueia preflight em", etapa), identical(e$monitora_v307_relatorios_preflight()$ok, FALSE))
}
p <- nova(); rs <- semear(); antes <- foto(e$MONITORA_OUTPUT_DIR)
msg <- erro(executar("pre_painel", function() TRUE, injetar_falha = function(etapa, i, tx) {
  if (etapa == "apos_copia" && i == 2L) gravar(tx$mapa_relatorios$copia[i], "copia adulterada")
}))
checar("Cópia com SHA divergente é recusada sem perda", grepl("SHA256", msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))
checar("Backup/diagnóstico de falha é conservado", length(list.files(e$MONITORA_LOG_DIR, "^diagnostico.txt$", recursive = TRUE)) == 1L)
executar("pre_painel", function() gravar(file.path(rs[1L], "novo.csv"), "retry valido"))
checar("Sucesso posterior da mesma fase libera o gate", isTRUE(e$monitora_v307_relatorios_preflight()))

p <- nova(); rs <- semear(); pre <- lapply(rs, foto)
pos_rs <- e$monitora_v307_relatorios_raizes(e$MONITORA_CORRECOES_DIR, "pos_correcoes")
semear("pos_correcoes")
executar("pos_correcoes", function() gravar(file.path(pos_rs[1L], "pos.csv"), "pos novo"))
checar("Regenerar pós no modo cache preserva pré físico", identical(lapply(rs, foto), pre))
checar("Fases pós-correções e pós-painel usam as mesmas fontes", identical(pos_rs, e$monitora_v307_relatorios_raizes(e$MONITORA_CORRECOES_DIR, "pos_painel")))
e$MONITORA_GERAR_RELATORIOS_SUPORTE_PAINEL <- TRUE
e$MONITORA_GERAR_RELATORIOS_POS_CORRECOES <- TRUE
mapas_antes <- list.files(e$MONITORA_LOG_DIR, "^mapa.csv$", recursive = TRUE)
res <- e$monitora_cache_gerar_relatorios_pos_se_preciso(data.table(COLETA = "C1"), file.path(p, "sem_correcoes.csv"), modo = "cache")
checar("Cache sem correções não isola fase que não regenera", identical(lapply(rs, foto), pre) && identical(list.files(e$MONITORA_LOG_DIR, "^mapa.csv$", recursive = TRUE), mapas_antes) && identical(res$status, "nao_gerado_sem_correcoes_novas"))

p <- nova(); semear("pre_painel"); rs <- semear("pre_painel_incremental")
executar("pre_painel_incremental", function() gravar(file.path(e$MONITORA_CORRECOES_DIR, "relatorios_pre_painel_incremental", "atual.csv"), "pre incremental novo"))
checar("Pré incremental retira alternativa antiga que precedia o fallback", !length(e$monitora_relatorios_resolver_fase(e$MONITORA_CORRECOES_DIR, "pre_painel")) && length(e$monitora_relatorios_resolver_fase(e$MONITORA_CORRECOES_DIR, "pre_painel_incremental")) == 1L)

p <- nova(); rs <- semear(); antes <- foto(e$MONITORA_OUTPUT_DIR)
msg <- erro(executar("pre_painel", function() stop("escritor falhou")))
checar("Erro real da geração restaura conjunto anterior", grepl("escritor falhou", msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))
id_anterior <- e$MONITORA_EXEC_ID
e$MONITORA_EXEC_ID <- "20261111_130000"
checar("Novo Source não herda bloqueio da execução anterior", isTRUE(e$monitora_v307_relatorios_preflight()))
e$MONITORA_EXEC_ID <- id_anterior
checar("Retornar à execução falha conserva bloqueio", identical(e$monitora_v307_relatorios_preflight()$ok, FALSE))

p <- nova(); rs <- semear(); antes <- foto(e$MONITORA_OUTPUT_DIR)
msg <- erro(executar("pre_painel", function() TRUE, injetar_falha = function(etapa, i, tx) {
  if (etapa == "apos_retirar" && i == 2L) {
    e$file.copy <- function(...) FALSE
    stop("falha antes de restauracao impossivel")
  }
}))
rm("file.copy", envir = e)
tx <- e$MONITORA_V307_TRANSACAO_FINAL_ATIVA
checar("Falha no rollback preserva backup e bloqueia publicação", grepl("Rollback incompleto", msg) && identical(tx$estado, "rollback_incompleto") && all(file.exists(tx$backups)) && identical(e$monitora_v307_relatorios_preflight()$ok, FALSE))
e$monitora_v307_transacao_reverter(tx, "recurso de copia restaurado no teste")
checar("Backup permite completar recuperação sem perda", identical(foto(e$MONITORA_OUTPUT_DIR), antes))

# Escritor biológico real, organizado como o Source1; nova geração e comparação
# reais no Source2. Não se usa prioridade de localização nem limpeza de ExecID.
p <- nova()
dados <- data.table(COLETA = "C1", UC = "UC_TESTE", EA = "1", UA = "1", CICLO = "1", CAMPANHA = "1", ANO = "2026", Encostam = "nativa", forma_vida_nativa = "desconhecida")
indice <- e$monitora_relatorios_suporte_painel_gravar(dados, "pre_painel")
checar("Escritor real gera todos os relatórios de suporte", nrow(indice) >= 17L)
e$monitora_output_migrar_diretorios_legados(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "teste_source1")
antigo <- file.path(e$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "apoio", "pre_painel", "relatorio_ocorrencia_formas_vida_desconhecida.txt")
sha_antigo <- hash(antigo)
e$MONITORA_EXEC_ID <- "20261111_140000"
indice2 <- e$monitora_relatorios_suporte_painel_gravar(dados, "pre_painel")
checar("Source2 real regenera sem conflito com organizado anterior", nrow(indice2) >= 17L && length(e$monitora_relatorios_resolver_fase(e$MONITORA_CORRECOES_DIR, "pre_painel")) >= 17L)
mapas <- list.files(e$MONITORA_LOG_DIR, "^mapa.csv$", recursive = TRUE, full.names = TRUE)
preservado <- any(vapply(mapas, function(m) sha_antigo %in% read.csv(m, stringsAsFactors = FALSE)$sha256, logical(1L)))
checar("Relatório Source1 organizado preserva seus bytes no histórico", preservado)
e$monitora_relatorios_suporte_painel_gravar(dados, "pos_correcoes")
cmp <- e$monitora_relatorios_comparar_pre_pos_correcoes(e$MONITORA_CORRECOES_DIR)
checar("Comparação real pré/pós permanece efetivamente produzida", nrow(cmp) >= 17L)
antes <- foto(e$MONITORA_OUTPUT_DIR)
real_token <- e$monitora_relatorio_formas_vida_token_gravar
e$monitora_relatorio_formas_vida_token_gravar <- function(...) stop("falha injetada no subrelatorio real")
msg <- erro(e$monitora_relatorios_suporte_painel_gravar(dados, "pre_painel"))
e$monitora_relatorio_formas_vida_token_gravar <- real_token
checar("Escritor real propaga falha antes engolida e restaura bytes", grepl("falha injetada no subrelatorio real", msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))
if (!modulo) checar("Preflight real impede certificar relatório restaurado como corrente", grepl("relat.rio", erro(e$monitora_v307_publicacao_preflight())))
checar("Marcador impede preflight após falha do escritor real", identical(e$monitora_v307_relatorios_preflight()$ok, FALSE))
e$monitora_relatorios_suporte_painel_gravar(dados, "pre_painel")
checar("Nova geração real bem-sucedida limpa somente sua falha", isTRUE(e$monitora_v307_relatorios_preflight()))
options(monitora.v307.relatorios.falhas = orig_options)
cat("PASS:", checks, "checks;", if (modulo) "módulo/hook estrutural de pré-integração" else "hooks e módulo embutidos reais", "\n")
