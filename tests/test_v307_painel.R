#!/usr/bin/env Rscript
# Exercita as reativas extraídas da candidata; não substitui ensaio no navegador.
suppressPackageStartupMessages({library(data.table); library(shiny)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
source("tools/v307/carregar.R")
e <- monitora_v307_carregar(args[[1L]])
tmp <- tempfile("painel_v307_"); dir.create(file.path(tmp,"input"),recursive=TRUE)
e$MONITORA_INPUT_DIR <- file.path(tmp,"input")
e$MONITORA_OUTPUT_DIR <- file.path(tmp,"output")
e$MONITORA_CORRECOES_DIR <- file.path(tmp,"output","correcoes_campos")
e$MONITORA_LOG_DIR <- file.path(tmp,"log")
e$MONITORA_EXEC_ID <- "20990101_010101"
e$MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK <- TRUE
e$MONITORA_VALIDAR_ESPACIAL_COLETAS <- FALSE
e$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
e$monitora_correcao_console_msg <- function(...) invisible(NULL)
registros <- data.table(COLETA=c("C1","C2"),MONITORA_ROW_ID=c("linha1","linha2"))
fwrite(registros,file.path(tmp,"input","registros_corrig.csv"))
e$monitora_v307_justificativas_registrar_base(registros,origem="teste")
oc <- data.table(tipo_ocorrencia="seca_morta_em_revisao",rotulo_ocorrencia="Vegetação seca ou morta",
  COLETA=c("C1","C2"),UC="UC teste",EA="EA1",UA=c("UA1","UA2"),ANO="2026",
  linha_indice=1:2,monitora_row_id=c("linha1","linha2"),ocorrencia_token="graminoide",
  ocorrencia_atributo="forma_vida_seca_morta",ocorrencia_categoria="seca_morta")
oc[,ocorrencia_id:=e$monitora_pendencias_ocorrencia_id(.SD)]
lote <- e$monitora_pendencias_justificativas_criar_lote(oc[1],"Teste automatizado","documentacao_insuficiente",
  "A ocorrência permanece para verificação futura documentada.","2099-01-01 01:02:00.000001")
stopifnot(lote$ok)
preparado <- e$monitora_pendencias_justificativas_preparar(oc,lote$dados)
e$monitora_pendencias_justificativas_persistir_preparado(preparado)

alvos <- c("monitora_painel_justificativas_universo", "just_ocorrencias_base","just_estado","just_ocorrencias_por_justificar",
  "just_ocorrencias_exibidas","just_ids_selecionados", "monitora_painel_reconciliar_justificativas_sessao",
  "normalizar_fila_checkpoint", "justificativas_preparadas")
exprs <- list()
visit <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if(op %in% c("<-","=") && length(x)>=3 && is.symbol(x[[2L]]) && as.character(x[[2L]]) %in% alvos) {
    exprs[[as.character(x[[2L]])]] <<- x; return(invisible(NULL))
  }
  for(i in seq_along(x)[-1L]) visit(x[[i]])
}
invisible(lapply(as.list(parse(args[[1L]],keep.source=FALSE)),visit))
stopifnot(setequal(names(exprs),alvos))
e$fixture_oc <- oc
e$fixture_exprs <- exprs[setdiff(alvos, "justificativas_preparadas")]
e$fixture_preparar_real <- exprs$justificativas_preparadas
server <- function(input,output,session) {
  fixture <- data.table::copy(fixture_oc)
  rv <- shiny::reactiveValues(ocorrencias_idx=data.table::copy(fixture),preview_revision=0L,
    preview_dirty=FALSE,preview_validacao_espacial=data.table(),
    justificativas_sessao=monitora_pendencias_justificativas_template(),
    justificativas_encerradas_sessao=monitora_pendencias_justificativas_template(),
    auditoria_reconciliacao_justificativas=data.table(), tempo_reconciliacao_justificativas_s=0,
    justificativas_sessao_selec_ids=character(), justificativas_selec_ids=fixture_oc$ocorrencia_id)
  for(expr in fixture_exprs) eval(expr,envir=environment())
}
environment(server) <- e
shiny::testServer(server, {
  session$flushReact()
  stopifnot(nrow(just_ocorrencias_base())==2L,
    nrow(just_estado()$pendencias_remanescentes[status_justificativa=="vigente"])==1L,
    nrow(just_ocorrencias_por_justificar())==1L,
    identical(just_ocorrencias_por_justificar()$COLETA,"C2"),
    !fixture$ocorrencia_id[1L] %in% just_ids_selecionados())
  session$setInputs(just_rotulos_lote="seca_morta_em_revisao")
  stopifnot(nrow(just_ocorrencias_exibidas())==1L)
  session$setInputs(just_rotulos_lote="tipo_inexistente")
  stopifnot(nrow(just_ocorrencias_exibidas())==0L)
  session$setInputs(just_rotulos_lote=character())
  novo <- e$monitora_pendencias_justificativas_criar_lote(fixture[2L],"Teste automatizado",
    "documentacao_insuficiente","Decisão independente referente à nova ocorrência.","2099-01-01 01:03:00.000001")
  rv$justificativas_sessao <- novo$dados
  session$flushReact()
  stopifnot(nrow(just_ocorrencias_por_justificar())==0L,
    nrow(just_estado()$pendencias_remanescentes[status_justificativa=="vigente"])==2L)
  # A prévia alterada ainda não comprova resolução; a mesma função local é
  # usada pela reativa, pelo preview e pelo preflight real de salvamento.
  rv$ocorrencias_idx <- fixture[1L]
  rv$preview_dirty <- TRUE
  rec_parcial <- monitora_painel_reconciliar_justificativas_sessao(notificar=FALSE)
  stopifnot(rec_parcial$n_encerradas==0L, nrow(rv$justificativas_sessao)==1L)
  rv$preview_dirty <- FALSE
  # Uma prévia integral do cache declara sua prova no próprio índice, mesmo
  # sem materialização dos relatórios globais.
  e$MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK <- FALSE
  rv$ocorrencias_idx <- e$monitora_v307_justificativas_marcar_diagnostico(rv$ocorrencias_idx, completo=TRUE, espacial=FALSE)
  rec_final <- monitora_painel_reconciliar_justificativas_sessao(notificar=FALSE)
  stopifnot(rec_final$n_encerradas==1L, nrow(rv$justificativas_sessao)==0L,
    nrow(rv$justificativas_encerradas_sessao)==2L,
    !is.null(attr(rv$justificativas_encerradas_sessao,"monitora_v307_aliases")))
  preparar_real <- function() {
    salvar_env <- new.env(parent=e)
    salvar_env$rv <- rv
    salvar_env$monitora_painel_justificativas_universo <- monitora_painel_justificativas_universo
    eval(e$fixture_preparar_real, envir=salvar_env)
    salvar_env$justificativas_preparadas
  }
  justificativas_preparadas <- preparar_real()
  stopifnot(nrow(justificativas_preparadas$historico)==3L,
    all(rv$justificativas_encerradas_sessao$evento_justificativa_id %in% justificativas_preparadas$historico$evento_justificativa_id))
  # Checkpoint integral e normalizador REAIS da candidata quando todas as
  # justificativas novas já encerraram. A fila vazia conserva a prova.
  cp <- e$monitora_pendencias_justificativas_checkpoint_recuperavel(
    rv$justificativas_sessao, rv$justificativas_encerradas_sessao,
    auditoria_reconciliacao=rv$auditoria_reconciliacao_justificativas,
    fingerprint_base=e$monitora_painel_checkpoint_fingerprint_base(registros))
  cp_obj <- readRDS(cp)
  rv$justificativas_sessao <- normalizar_fila_checkpoint(cp_obj$justificativas_ativas,
    e$monitora_pendencias_justificativas_template())
  rv$justificativas_encerradas_sessao <- normalizar_fila_checkpoint(cp_obj$justificativas_encerradas,
    e$monitora_pendencias_justificativas_template())
  stopifnot(nrow(rv$justificativas_sessao)==0L,
    !is.null(attr(rv$justificativas_sessao,"monitora_v307_aliases")))
  justificativas_preparadas <- preparar_real()
  stopifnot(nrow(justificativas_preparadas$historico)==3L,
    sum(justificativas_preparadas$historico$status_evento=="encerrada_por_resolucao")==1L)
  e$monitora_pendencias_justificativas_persistir_preparado(justificativas_preparadas)
  e$MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK <- TRUE
  rv$justificativas_sessao <- e$monitora_pendencias_justificativas_template()
  rv$ocorrencias_idx <- data.table()
  rv$preview_revision <- 1L
  session$flushReact()
  stopifnot(nrow(just_ocorrencias_base())==0L,
    isTRUE(attr(just_ocorrencias_base(),"monitora_diagnostico_completo")),
    nrow(just_ocorrencias_por_justificar())==0L)
})
cat("PASS: reativas extraídas; histórico/fila; preflight real de salvamento; reconciliação parcial/completa; checkpoint integral com fila vazia e origens preservadas. Não substitui ensaio no navegador.\n")
