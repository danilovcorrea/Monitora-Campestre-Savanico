#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.21-dev_r01.R",
  mustWork = TRUE
)
run4 <- if (length(args) >= 2L) normalizePath(args[[2L]], mustWork = TRUE) else NA_character_
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    try(eval(x, env), silent = TRUE)
  }
  for (ii in seq_along(x)[-1L]) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_QA_CACHES <- new.env(parent = emptyenv())
env$monitora_publicacao_ae_cache_env <- function(namespace = "geral") {
  chave <- gsub("[^[:alnum:]_]+", "_", as.character(namespace)[1L])
  if (!exists(chave, envir = env$.MONITORA_QA_CACHES, inherits = FALSE)) {
    assign(chave, new.env(parent = emptyenv()), envir = env$.MONITORA_QA_CACHES)
  }
  get(chave, envir = env$.MONITORA_QA_CACHES, inherits = FALSE)
}
env$monitora_fwrite <- function(...) invisible(NULL)
env$monitora_diag_rel_write_dt <- function(...) invisible(NULL)
env$monitora_log_registrar_evento <- function(...) invisible(NULL)

col_tipo <- "amostragem/registro/tipo_forma_vida"
col_forma <- "amostragem/registro/forma_vida_exotica"
col_especie <- "amostragem/registro/especie"
col_lista_gram <- "amostragem/registro/especies_exotica_graminoide"
col_texto_gram <- "amostragem/registro/exotica_graminoide_outra_sp"
col_lista_orfa <- "amostragem/registro/especies_exotica_outros"
col_texto_orfao <- "amostragem/registro/exotica_outros_outra_sp"

### RED causal: texto no ramo órfão não pode materializar `outros` no pai.
dt_red <- data.table(
  tipo = "exotica", forma = "graminoide", especie = "",
  lista_gram = "", texto_gram = "", lista_orfa = "", texto_orfao = "Urochloa sp."
)
setnames(dt_red, names(dt_red), c(
  col_tipo, col_forma, col_especie, col_lista_gram,
  col_texto_gram, col_lista_orfa, col_texto_orfao
))
invisible(env$monitora_correcao_recalcular_superiores_vinculados(
  dt_red, 1L, modo_encostam = "acrescentar"
))
assert(
  identical(dt_red[[col_forma]][1L], "graminoide"),
  paste0("RED: fechamento criou token fora do domínio: ", dt_red[[col_forma]][1L])
)

assert(
  exists("monitora_correcao_reconciliar_outros_orfao_exotica", envir = env, inherits = FALSE),
  "Migração contratual do token órfão `outros` não foi implementada."
)

### Preenchimento explícito do campo correto implica somente os ancestrais
### semânticos do contrato vigente; módulo histórico não participa.
dt_folha <- data.table(
  tipo = "exotica", forma = "graminoide", especie = "",
  lista_gram = "", texto_gram = "Urochloa sp.", modulo = "basico"
)
setnames(dt_folha, names(dt_folha), c(
  col_tipo, col_forma, col_especie, col_lista_gram, col_texto_gram,
  "amostragem/registro/modulo"
))
invisible(env$monitora_correcao_recalcular_superiores_vinculados(
  dt_folha, 1L, modo_encostam = "acrescentar"
))
assert(dt_folha[[col_especie]][1L] == "sim", "Folha preenchida não materializou especie=sim.")
assert(
  env$monitora_correcao_token_presente_vec(
    dt_folha[[col_lista_gram]][1L], "exotica_graminoide_outra_sp"
  ),
  "Folha preenchida não materializou o token na lista de espécies graminoides."
)
assert(dt_folha[[col_forma]][1L] == "graminoide", "Folha correta alterou indevidamente a forma de vida.")
assert(dt_folha[["amostragem/registro/modulo"]][1L] == "basico", "Fechamento reescreveu o módulo histórico.")

### Conflito em select_one deve falhar fechado e sem mutação parcial.
dt_conflito <- copy(dt_folha)
set(dt_conflito, i = 1L, j = col_especie, value = "nao")
set(dt_conflito, i = 1L, j = col_lista_gram, value = "")
antes_conflito <- copy(dt_conflito)
invisible(env$monitora_correcao_recalcular_superiores_vinculados(
  dt_conflito, 1L, modo_encostam = "acrescentar"
))
assert(identical(dt_conflito, antes_conflito), "Conflito especie=nao sofreu mutação parcial.")

mapa_contrato <- env$monitora_correcao_contrato_mapa_outras_especies_exoticas()
assert(
  nrow(mapa_contrato$mapeamento) == 14L &&
    !any(mapa_contrato$mapeamento$token_forma == "outros") &&
    nrow(mapa_contrato$orfao) == 1L && mapa_contrato$orfao$token_orfao == "outros",
  "Contrato derivado não separou 14 ramos válidos do ramo órfão `outros`."
)

### Dois ramos válidos tornam o destino ambíguo e bloqueiam a migração inteira.
dt_ambiguo <- data.table(
  tipo = "exotica", forma = "graminoide bambu outros", especie = "",
  lista_gram = "", texto_gram = "", lista_orfa = "", texto_orfao = "Urochloa sp."
)
setnames(dt_ambiguo, names(dt_ambiguo), c(
  col_tipo, col_forma, col_especie, col_lista_gram,
  col_texto_gram, col_lista_orfa, col_texto_orfao
))
antes_ambiguo <- copy(dt_ambiguo)
rec_ambiguo <- env$monitora_correcao_reconciliar_outros_orfao_exotica(dt_ambiguo)
assert(
  identical(rec_ambiguo$dt, antes_ambiguo) && nrow(rec_ambiguo$bloqueios) == 1L &&
    grepl("forma_exotica_valida_nao_unica", rec_ambiguo$bloqueios$motivo),
  "Migração ambígua não falhou fechada."
)

### Tokens homônimos em outros contextos e as variantes contratuais distintas
### não podem acionar a migração exótica.
dt_contextos <- data.table(
  tipo = "outra_forma_vida", forma_outros = "musgos",
  forma_exotica = "outra", impactos = "outros"
)
setnames(dt_contextos, names(dt_contextos), c(
  col_tipo, "amostragem/registro/forma_vida_outros", col_forma,
  "impact_manejo_uso/tipos_impacto_manejo_uso"
))
antes_contextos <- copy(dt_contextos)
rec_contextos <- env$monitora_correcao_reconciliar_outros_orfao_exotica(dt_contextos)
assert(identical(rec_contextos$dt, antes_contextos), "Token de outro contexto foi confundido com o órfão exótico.")

### O validador final deve exigir toda a cadeia da folha preenchida.
out_cond <- data.table(
  tipo = "exotica", forma = "graminoide", especie = "",
  lista = "", folha = "Urochloa sp."
)
setnames(out_cond, names(out_cond), c(
  col_tipo, col_forma, col_especie, col_lista_gram, col_texto_gram
))
problemas_cond <- env$monitora_validados_validar_condicionais_xlsform21(out_cond)
assert(
  all(c(
    "folha_especie_exotica_sem_especie_sim",
    "folha_especie_exotica_sem_token_outra_especie"
  ) %in% problemas_cond$regra),
  "Validação final não detectou ancestrais ausentes da folha preenchida."
)
invisible(env$monitora_correcao_recalcular_superiores_vinculados(
  out_cond, 1L, modo_encostam = "acrescentar"
))
problemas_cond_pos <- env$monitora_validados_validar_condicionais_xlsform21(out_cond)
assert(
  !any(grepl("folha_.*especie_exotica", problemas_cond_pos$regra)),
  "Fechamento semântico não satisfez a validação condicional final."
)
out_dominio <- copy(out_cond)
set(out_dominio, i = 1L, j = col_forma, value = "graminoide outros")
problemas_dom <- env$monitora_validados_validar_dominios_xlsform21(out_dominio, out_dominio)
assert(
  nrow(problemas_dom[atributo == col_forma & grepl("outros", tokens_invalidos)]) == 1L,
  "Validação de domínio deixou de rejeitar `outros` em forma_vida_exotica."
)

texto_script <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert(
  grepl("tokens = monitora_correcao_tokens_residuo_historico_outras_formas()", texto_script, fixed = TRUE) &&
    !grepl('"registros_corrig.csv bloqueado: "', texto_script, fixed = TRUE),
  "Relatório de outras formas ou mensagem de bloqueio ainda conserva a confusão auditada."
)

### Caso real da run 4: cardinalidade e todas as células fora da transação
### devem permanecer byte a byte equivalentes após a recuperação.
if (!is.na(run4)) {
  d <- fread(run4, encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)
  ch <- env$monitora_correcao_colunas_chave(d)
  idx <- which(as.character(d[[ch$coleta]]) == "18785" &
    as.character(d[[ch$ponto_amostral]]) %in% c("16", "18", "30", "31"))
  assert(length(idx) == 4L, "Run 4: os quatro registros causais não foram localizados.")
  antes <- copy(d)
  rec <- env$monitora_correcao_reconciliar_outros_orfao_exotica(d, linhas = idx)
  d <- rec$dt
  mapa <- env$monitora_correcao_contrato_mapa_outras_especies_exoticas()
  resolucao <- env$monitora_correcao_resolver_colunas_mapa_outras_especies(d, mapa)
  col_fisica <- function(path_alvo) {
    hit <- resolucao[path == as.character(path_alvo)[1L] & status_resolucao == "resolvido_unico", coluna]
    assert(length(hit) == 1L, paste0("Run 4: coluna não resolvida: ", path_alvo))
    as.character(hit)
  }
  regra_gram <- mapa$mapeamento[token_forma == "graminoide"]
  col_especie_real <- col_fisica(mapa$especie$especie_path)
  col_forma_real <- col_fisica(mapa$raiz$raiz_path)
  col_lista_gram_real <- col_fisica(regra_gram$lista_path)
  col_texto_gram_real <- col_fisica(regra_gram$folha_path)
  col_texto_orfao_real <- col_fisica(mapa$orfao$folha_orfa_path)
  assert(nrow(d) == nrow(antes) && ncol(d) == ncol(antes), "Run 4: cardinalidade foi alterada.")
  assert(!nrow(rec$bloqueios), "Run 4: migração inequívoca foi bloqueada.")
  assert(nrow(rec$audit) == 20L, "Run 4: a transação não registrou exatamente cinco células por registro.")
  assert(all(d[[col_especie_real]][idx] == "sim"), "Run 4: especie=sim não foi materializado.")
  assert(all(d[[col_forma_real]][idx] == "graminoide"), "Run 4: `outros` não foi removido do pai.")
  assert(all(d[[col_lista_gram_real]][idx] == "exotica_graminoide_outra_sp"), "Run 4: lista graminoide incorreta.")
  assert(all(d[[col_texto_gram_real]][idx] == "Urochloa sp."), "Run 4: texto não foi preservado literalmente no destino.")
  assert(all(env$monitora_correcao_vazio_vec(d[[col_texto_orfao_real]][idx])), "Run 4: origem órfã não foi limpa.")
  cols_tocadas <- c(col_especie_real, col_forma_real, col_lista_gram_real, col_texto_gram_real, col_texto_orfao_real)
  outras <- setdiff(names(d), cols_tocadas)
  assert(identical(d[, ..outras], antes[, ..outras]), "Run 4: célula fora da transação foi alterada.")
  estado <- copy(d)
  rec2 <- env$monitora_correcao_reconciliar_outros_orfao_exotica(d, linhas = idx)
  assert(identical(rec2$dt, estado) && !nrow(rec2$audit), "Run 4: migração não é idempotente.")
}

cat("TEST_V2921_OUTROS_ORFAO_EXOTICA_OK\n")
