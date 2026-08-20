#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(xml2)
  library(rmarkdown)
})

args <- commandArgs(trailingOnly = TRUE)
candidato <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.15.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
texto <- paste(readLines(candidato, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
arvore <- parse(file = candidato, keep.source = FALSE)

alvos <- c(
  "monitora_correcao_normalizar_nome_coluna",
  "monitora_correcao_parse_lista_serializada",
  "monitora_correcao_operacao_toca_desconhecida",
  "monitora_correcao_classes_abrangencia",
  "monitora_correcao_texto_contem_fragmento",
  "monitora_correcao_aplicar_operacao",
  "monitora_correcao_acao_normalizar",
  "monitora_correcao_template",
  "monitora_replay_operation_kinds_suportados",
  "monitora_replay_operation_kind",
  "monitora_replay_migrar_para_v2",
  "monitora_relatorios_analiticos_conteudo_docx",
  "monitora_relatorios_analiticos_docx_auditar_figuras"
)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(
  all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)),
  "Nem todas as funções focais das regressões foram extraídas."
)

### PNCG: acrescentar texto é idempotente e faz parte do contrato de replay.
base_texto <- "Observação original"
fragmento <- "Complemento registrado na sessão"
uma_vez <- env$monitora_correcao_aplicar_operacao(
  base_texto, "append_text", fragmento
)
duas_vezes <- env$monitora_correcao_aplicar_operacao(
  uma_vez, "append_text", fragmento
)
exigir(identical(uma_vez, duas_vezes), "append_text duplicou o fragmento no replay.")
exigir(
  lengths(regmatches(duas_vezes, gregexpr(fragmento, duas_vezes, fixed = TRUE))) == 1L,
  "O fragmento acrescentado não aparece exatamente uma vez."
)
exigir(
  "acrescentar_texto" %in% env$monitora_replay_operation_kinds_suportados(),
  "O handler acrescentar_texto não integra o contrato de replay."
)

pn_stub <- data.table(
  acao = rep("append_text", 3L),
  tipo_correcao = "lote_multicoletas_campo_superior",
  atributo_coluna_registros_corrig = "Observações gerais"
)
exigir(
  all(env$monitora_replay_operation_kind(pn_stub) == "acrescentar_texto"),
  "append_text ainda é classificado como operação não suportada."
)
env$MONITORA_REPLAY_SCHEMAS_SUPORTADOS <- c(
  "correcoes_semanticas_v1", "replay_semantico_v1", "correcoes_semanticas_v2"
)
env$MONITORA_REPLAY_SCHEMA_ATUAL <- "correcoes_semanticas_v2"
ledger_v2 <- data.table(
  event_schema_version = "correcoes_semanticas_v2",
  acao = "append_text",
  operation_kind = "nao_suportada",
  migrated_from_schema = ""
)
ledger_original <- copy(ledger_v2)
ledger_reparado <- env$monitora_replay_migrar_para_v2(ledger_v2, "ledger_teste.csv")
exigir(
  identical(ledger_reparado$operation_kind, "acrescentar_texto") &&
    grepl("append_text_v2_handler_ausente", ledger_reparado$migrated_from_schema, fixed = TRUE),
  "O fast path v2 não reparou o handler legado de append_text."
)
exigir(
  identical(ledger_v2, ledger_original),
  "A migração corretiva modificou o ledger de entrada em vez da cópia em memória."
)

### Exercita as 11 intenções append_text da rodada PNCG, quando a evidência
### real está disponível na estação de homologação.
arquivo_pncg <- if (length(args) >= 2L) args[[2L]] else
  Sys.getenv("MONITORA_TESTE_PNCG_LEDGER", unset = "")
if (nzchar(arquivo_pncg) && file.exists(arquivo_pncg)) {
  pncg <- fread(arquivo_pncg, na.strings = NULL, showProgress = FALSE)
  pncg_append <- pncg[tolower(trimws(acao)) == "append_text"]
  exigir(nrow(pncg_append) == 11L, "A evidência real PNCG não contém as 11 intenções esperadas.")
  exigir(
    all(env$monitora_replay_operation_kind(pncg_append) == "acrescentar_texto"),
    "Alguma intenção append_text real do PNCG continua sem handler."
  )
}

### PNCF: caminhos alternativos que tocam a ocorrência desconhecida precisam
### ser reconhecidos antes da prévia, mesmo usando atributos físicos distintos.
op_movimento <- data.table(
  id_correcao = "MOVFV_teste",
  tipo_correcao = "movimento_forma_vida_atomico",
  acao = "mover_forma_vida",
  atributo_coluna_registros_corrig = "__mover_forma_vida__",
  token_removido = "desconhecida|desconhecido",
  categoria_origem = "exotica",
  categoria_destino = "nativa",
  valor_novo = "desconhecida",
  monitora_row_id = "UUID::alvo"
)
op_direta <- data.table(
  id_correcao = "CORR_teste",
  tipo_correcao = "simples_ou_lote",
  acao = "replace_token",
  atributo_coluna_registros_corrig = "formas_vida_exoticas",
  valor_original_esperado = "desconhecida",
  valor_novo = "lianas",
  monitora_row_id = "UUID::alvo"
)
exigir(
  env$monitora_correcao_operacao_toca_desconhecida(op_movimento) &&
    env$monitora_correcao_operacao_toca_desconhecida(op_direta),
  "Os dois caminhos concorrentes da ocorrência desconhecida não foram reconhecidos."
)
op_tridesc_lote <- data.table(
  id_correcao = "TRIDESC_teste",
  tipo_correcao = "triagem_substituir_desconhecida",
  acao = "substituir_desconhecida",
  escopo_aplicacao = "linhas_pendentes_ocorrencia",
  n_linhas_alvo = 20L
)
exigir(
  identical(env$monitora_correcao_classes_abrangencia(op_tridesc_lote), "correcao_especifica"),
  "TRIDESC voltou a ser classificada como lote genérico."
)
for (trecho in c(
  "(attr_p == attr_n[ii] | exc_n | (desc_n[ii] & desc_p))",
  "if (isTRUE(desc_n[ii]) && isTRUE(desc_p[jj])) return(TRUE)",
  "operação(ões) conflitante(s) na mesma ocorrência ou alvo semântico"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Trava PNCF ausente: ", trecho))

### PNCV: figura HTML com classe deve ser convertida e efetivamente existir no
### OOXML. O gate também precisa recusar um DOCX deliberadamente incompleto.
dir_teste <- tempfile(pattern = "monitora_docx_gate_")
dir.create(dir_teste, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(dir_teste, recursive = TRUE, force = TRUE), add = TRUE)
for (nome in c("mapa.png", "grafico.png")) {
  grDevices::png(file.path(dir_teste, nome), width = 320L, height = 220L)
  graphics::plot(1:3, main = nome)
  grDevices::dev.off()
}
conteudo_integral <- c(
  "---", 'pagetitle: "Teste de integridade"', "---", "",
  '<figure class="mapa-satelite"><img src="mapa.png" style="width:100%;"><figcaption>Figura 1. Mapa Sentinel.</figcaption></figure>',
  '<figure><img src="grafico.png" style="width:100%;"><figcaption>Figura 2. Gráfico analítico.</figcaption></figure>'
)
rmd_integral <- file.path(dir_teste, "integral.Rmd")
writeLines(env$monitora_relatorios_analiticos_conteudo_docx(conteudo_integral), rmd_integral)
convertido <- paste(readLines(rmd_integral, warn = FALSE), collapse = "\n")
exigir(
  grepl("Mapa Sentinel", convertido, fixed = TRUE) && grepl("mapa.png", convertido, fixed = TRUE),
  "A figura Sentinel com classe HTML não foi convertida para o DOCX."
)
rmarkdown::render(
  rmd_integral,
  rmarkdown::word_document(toc = FALSE),
  output_file = "integral.docx",
  output_dir = dir_teste,
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
docx_integral <- file.path(dir_teste, "integral.docx")
auditoria_docx <- env$monitora_relatorios_analiticos_docx_auditar_figuras(
  docx_integral,
  conteudo_integral,
  file.path(dir_teste, "auditoria_docx.csv")
)
exigir(nrow(auditoria_docx) == 2L && all(auditoria_docx$integridade),
       "O DOCX integral não passou no gate de figuras.")

conteudo_incompleto <- conteudo_integral[-which(grepl("mapa-satelite", conteudo_integral, fixed = TRUE))]
rmd_incompleto <- file.path(dir_teste, "incompleto.Rmd")
writeLines(env$monitora_relatorios_analiticos_conteudo_docx(conteudo_incompleto), rmd_incompleto)
rmarkdown::render(
  rmd_incompleto,
  rmarkdown::word_document(toc = FALSE),
  output_file = "incompleto.docx",
  output_dir = dir_teste,
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
falha_esperada <- tryCatch({
  env$monitora_relatorios_analiticos_docx_auditar_figuras(
    file.path(dir_teste, "incompleto.docx"),
    conteudo_integral
  )
  NULL
}, error = identity)
exigir(inherits(falha_esperada, "error"),
       "O gate aceitou um DOCX do qual o mapa Sentinel foi removido.")

### Correspondência documental dos painéis de evidência.
for (trecho in c(
  "bloco_evidencia_inferencial_detalhado <- function",
  "bloco_evidencia_inferencial_detalhado(\"categorias_gerais\")",
  "bloco_evidencia_inferencial_detalhado(\"herbaceas_lenhosas\")",
  "bloco_evidencia_inferencial_detalhado(\"formas_vida_nativas\")",
  "bloco_evidencia_inferencial_detalhado(\"formas_vida_exoticas\")",
  "bloco_evidencia_inferencial_detalhado(\"formas_vida_secas_mortas\")",
  "bloco_evidencia_inferencial_detalhado(\"material_botanico\")",
  "auditoria_incorporacao_paineis_evidencia_relatorio.csv",
  "incorporada_e_verificada_no_relatorio_detalhado"
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Integração documental ausente: ", trecho))

cat("TEST_V2915_REVISOES_POS_AUDITORIA_OK\n")
