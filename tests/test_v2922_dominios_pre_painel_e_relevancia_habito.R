#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.22-dev_r01.R",
  mustWork = TRUE
)
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
for (nm in c(
  ".MONITORA_CONTRATO_UNICO_CACHE", ".MONITORA_CONTRATO_MOVIMENTO_CACHE",
  ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE", ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AE_CACHE_CONTRATO", ".MONITORA_PUBLICACAO_AE_CACHE_XLSFORMS",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) assign(nm, new.env(parent = emptyenv()), envir = env)
env$monitora_fwrite <- function(...) invisible(NULL)
env$monitora_diag_rel_write_dt <- function(...) invisible(NULL)
env$monitora_log_registrar_evento <- function(...) invisible(NULL)

assert(
  exists("monitora_contrato_validar_dominios_dataset_pre_painel", envir = env, inherits = FALSE),
  "RED: validador contratual de domínios pré-painel ainda não existe."
)
assert(
  exists("monitora_correcao_validar_relevancia_habitos_operacoes", envir = env, inherits = FALSE),
  "RED: gate de relevância categoria×forma×hábito ainda não existe."
)

mapa <- env$monitora_correcao_contrato_mapa_outras_especies_exoticas()
regra_gram <- mapa$mapeamento[token_forma == "graminoide"]
assert(nrow(regra_gram) == 1L, "Contrato não resolveu o ramo graminoide de forma unívoca.")

col_tipo <- mapa$tipo$tipo_path
col_raiz <- mapa$raiz$raiz_path
col_lista_gram <- regra_gram$lista_path
col_texto_outro_impacto <- "amostragem/registro/tipos_impacto_manejo_uso_outro"
dt_dom <- data.table(
  COLETA = c("A", "B", "C", "D"),
  MONITORA_ROW_ID = c("r1", "r2", "r3", "r4"),
  tipo = c("exotica", "exotica", "outra_forma_vida", "NA"),
  raiz = c("graminoide", "graminoide", "", "NA"),
  especies = c("braquiaria", "melinis_minutiflora", "", "NA"),
  texto_outro = c("", "", "outros", "")
)
setnames(dt_dom, c("tipo", "raiz", "especies", "texto_outro"), c(
  col_tipo, col_raiz, col_lista_gram, col_texto_outro_impacto
))
aud_dom <- env$monitora_contrato_validar_dominios_dataset_pre_painel(dt_dom)
assert(
  nrow(aud_dom[linha_indice == 1L & caminho_registro == col_lista_gram & token_invalido == "braquiaria"]) == 1L,
  "Token órfão braquiaria não foi detectado no campo/lista contratual exato."
)
assert(
  !nrow(aud_dom[linha_indice == 2L]),
  "Valor contratual válido da espécie exótica sofreu falso positivo."
)
assert(
  !nrow(aud_dom[linha_indice == 3L]),
  "Texto livre 'outros' foi confundido com token select de outro contexto."
)
assert(
  !nrow(aud_dom[linha_indice == 4L]),
  "Placeholder serializado 'NA' foi confundido com token select inválido."
)
assert(
  all(c("coluna_fisica", "caminho_registro", "list_name", "tipo_base", "token_invalido", "choices_validas") %in% names(aud_dom)),
  "Auditoria pré-painel não contém contexto contratual suficiente para correção."
)
env$MONITORA_CORRECOES_DIR <- tempfile("v2922_ponte_contrato_")
dir.create(env$MONITORA_CORRECOES_DIR, recursive = TRUE)
ponte <- env$monitora_ponte_pre_painel_regras_contrato_unico(
  base_dir = env$MONITORA_CORRECOES_DIR
)
assert(
  !nrow(ponte$divergencias) &&
    nrow(ponte$auditoria[
      identificador == "token_fora_dominio_contrato" &
        tipo_divergencia == "validacao_dominio_derivada_diretamente_choices_contrato_unico"
    ]) == 1L,
  "Validação de domínio derivada do contrato foi tratada como regra local concorrente."
)

col_nativa <- "amostragem/registro/forma_vida_nativa"
col_exotica <- "amostragem/registro/forma_vida_exotica"
col_hab_nativa <- "amostragem/registro/forma_vida_nativa_cactacea"
col_hab_exotica <- "amostragem/registro/forma_vida_exotica_cactacea"
dt_hab <- data.table(
  COLETA = "6065",
  MONITORA_ROW_ID = "row_habito_sintetica",
  tipo = "exotica", nativa = "", exotica = "cactacea",
  hab_nativa = "", hab_exotica = ""
)
setnames(dt_hab, c("tipo", "nativa", "exotica", "hab_nativa", "hab_exotica"), c(
  "amostragem/registro/tipo_forma_vida", col_nativa, col_exotica,
  col_hab_nativa, col_hab_exotica
))

op_base <- function(atributo) {
  op <- env$monitora_correcao_criar_operacao(
    id = "CORR_TESTE_PNB", responsavel = "teste", tipo = "simples_ou_lote",
    ordem = 1L, escopo = "coleta_inteira", coleta = "6065",
    atributo = atributo, acao = "update", valor_original = "",
    valor_novo = "terrestre", n_esperado = 1L, n_alvo = 1L,
    motivo = "Teste de relevância condicional do hábito."
  )
  env$monitora_correcao_anexar_contexto_operacao(
    op, dt_hab, 1L, env$monitora_correcao_colunas_chave(dt_hab)
  )
}

op_errada <- op_base(col_hab_nativa)
val_errada <- env$monitora_correcao_validar_relevancia_habitos_operacoes(dt_hab, op_errada)
assert(!isTRUE(val_errada$ok) && nrow(val_errada$problemas) == 1L,
  "Destino nativo incompatível com a linha exótica não foi bloqueado.")
assert(
  val_errada$problemas$categoria_solicitada[1L] == "nativa" &&
    val_errada$problemas$categoria_relevante_sugerida[1L] == "exotica" &&
    val_errada$problemas$atributo_canonico_sugerido[1L] == col_hab_exotica,
  "Bloqueio não informou o destino contratual exótico correto."
)

op_correta <- op_base(col_hab_exotica)
val_correta <- env$monitora_correcao_validar_relevancia_habitos_operacoes(dt_hab, op_correta)
assert(isTRUE(val_correta$ok) && !nrow(val_correta$problemas),
  "Destino exótico relevante foi bloqueado indevidamente.")

texto <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chamadas_gate <- gregexpr(
  "monitora_correcao_validar_relevancia_habitos_operacoes\\(", texto, perl = TRUE
)[[1L]]
assert(
  sum(chamadas_gate > 0L) >= 3L &&
    grepl('etapa = "relevancia_condicional_antes_fila"', texto, fixed = TRUE) &&
    grepl('etapa = "relevancia_condicional_antes_salvamento"', texto, fixed = TRUE),
  "Gate de relevância não está ligado aos três pontos: fila, salvamento e motor atômico."
)

op_replace_distinto <- data.table(
  acao = "replace_token",
  valor_original_esperado = "graminoideerva_nao_graminoide",
  valor_novo = "graminoide",
  forma_valida_escolhida = "graminoide",
  categoria_destino = "nativa"
)
op_append_distinto <- data.table(
  acao = "append_token",
  valor_novo = "erva_nao_graminoide",
  forma_valida_escolhida = "erva_nao_graminoide",
  categoria_destino = "nativa"
)
assert(
  identical(
    env$monitora_painel_relacao_efeitos(op_replace_distinto, op_append_distinto),
    "compativel_replace_append_tokens_distintos"
  ) && identical(
    env$monitora_painel_relacao_efeitos(op_append_distinto, op_replace_distinto),
    "compativel_replace_append_tokens_distintos"
  ),
  "Painel ainda bloqueia Substituir token + Adicionar token distintos na mesma célula."
)
op_append_sobreposto <- data.table(
  acao = "append_token",
  valor_novo = "graminoide",
  forma_valida_escolhida = "graminoide",
  categoria_destino = "nativa"
)
assert(
  startsWith(
    env$monitora_painel_relacao_efeitos(op_replace_distinto, op_append_sobreposto),
    "conflito_"
  ),
  "Painel flexibilizou indevidamente append sobreposto ao destino de replace_token."
)

cat("TEST_V2922_DOMINIOS_PRE_PAINEL_E_RELEVANCIA_HABITO_OK\n")
