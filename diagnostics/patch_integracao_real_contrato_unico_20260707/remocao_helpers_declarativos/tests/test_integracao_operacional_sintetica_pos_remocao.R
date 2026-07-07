script <- "monitora_campsav_alvo_global_v2.6.0.R"
ln <- readLines(script, warn = FALSE)
ini <- grep("MONITORA_CU_20260707_INTEGRACAO_OPERACIONAL_REAL_BEGIN", ln, fixed = TRUE); fim <- grep("MONITORA_CU_20260707_INTEGRACAO_OPERACIONAL_REAL_END", ln, fixed = TRUE)
stopifnot(length(ini)==1, length(fim)==1, fim>ini)
blk <- paste(ln[ini:fim], collapse="\n")
e <- new.env(parent=globalenv())
e$monitora_cu_20260707 <- function() list(perfil_export_registros_validados=TRUE)
e$monitora_validados_schema_embutido <- function() c("COLETA","UC","forma_vida")
e$monitora_registros_validados_exportar <- function(dados, perfil_export=NULL, ...) { stopifnot(!is.null(perfil_export)); attr(dados,"perfil_export_usado") <- TRUE; dados }
e$monitora_produtos_resolver_pipes_por_ponto <- function(dados, ...) { attr(dados,"pipes_operacional") <- TRUE; dados }
e$monitora_mvlote_movimento_assistido <- function(origem="", destino="", forma="", ...) list(origem=origem,destino=destino,forma=forma)
e$monitora_ocorrencias_idx_atualizar <- function(x, ...) x
eval(parse(text=blk), envir=e)
r <- e$monitora_registros_validados_exportar(data.frame(COLETA=1,UC="u",forma_vida="bromelioide")); stopifnot(isTRUE(attr(r,"perfil_export_usado")))
p <- e$monitora_produtos_resolver_pipes_por_ponto(data.frame(forma_vida_grupo="bromelioide|epifita",stringsAsFactors=FALSE)); stopifnot(isTRUE(attr(p,"pipes_operacional")))
val <- e$monitora_cu_mvlote_validar_operacao_20260707("A","B","Outras plantas terrestres, líquens e/ou fungos"); stopifnot(isTRUE(val$ok), isTRUE(val$categoria_valida), !isTRUE(val$legado))
leg <- e$monitora_cu_mvlote_validar_operacao_20260707("A","B","outra forma de vida"); stopifnot(isTRUE(leg$legado))
inc <- e$monitora_cu_mvlote_validar_operacao_20260707("","B","Outras plantas terrestres, líquens e/ou fungos"); stopifnot(!isTRUE(inc$ok), isTRUE(inc$incompleta))
oc <- e$monitora_cu_ocorrencias_normalizar_indice_operacional_20260707(data.frame(tipo_ocorrencia="A",COLETA=1)); stopifnot(all(e$monitora_cu_ocorr_campos_20260707() %in% names(oc)))
res <- e$monitora_cu_ocorrencias_resumo_operacional_20260707(oc); stopifnot(all(c("pendentes","resolvidas_na_sessao","sem_ferramenta","bloqueantes") %in% names(res)))
cat("TEST_INTEGRACAO_OPERACIONAL_OK\n")
