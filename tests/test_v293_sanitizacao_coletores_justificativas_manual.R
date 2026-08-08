args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library(data.table))
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.3.R", mustWork = TRUE)
parsed <- parse(file = script)
body <- parsed[[1L]][[2L]]
env <- new.env(parent = globalenv())

carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    carregar <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    carregar <- carregar || grepl("^MONITORA_COL_", nome) || nome == "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS"
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

raiz <- tempfile("v293_coletores_just_")
dir.create(raiz, recursive = TRUE)
env$MONITORA_OUTPUT_DIR <- file.path(raiz, "output")
env$MONITORA_LOG_DIR <- file.path(raiz, "log")
env$MONITORA_CORRECOES_DIR <- file.path(raiz, "output", "correcoes_campos")
env$MONITORA_INPUT_DIR <- file.path(raiz, "input")
env$MONITORA_EXEC_ID <- "v293_coletores_just"
env$MONITORA_SCRIPT_VERSAO <- "2.9.3-dev-test"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.3-dev-test"
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"

bloco <- function(coleta, nomes, cpf = "") {
  data.table(
    COLETA = rep(coleta, 101L),
    UC = "UC teste", EA = "EA1", UA = paste0("UA", coleta), ANO = "2026",
    MONITORA_ROW_ID = paste0(coleta, "_", seq_len(101L)),
    COLETORES = rep(nomes, 101L),
    `coletor/cpf` = rep(cpf, 101L)
  )
}

dados <- rbindlist(list(
  bloco("C1", "{'Pessoa Um,Pessoa Dois,Pessoa Três': '017********'}"),
  bloco("C2", "{'Pessoa Única': '52998224725'}"),
  bloco("C3", "Equipe: Pessoa A / Pessoa B e Pessoa C"),
  bloco("C4", "{'Pessoa Alfa': '52998224725', 'Pessoa Beta': '017********', 'Pessoa Gama': ''}")
), fill = TRUE)

res <- env$monitora_coletores_repeat_sanitizar_legado(
  copy(dados), output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, contexto = "teste_sintetico", abortar_nao_reconhecido = TRUE
)
stopifnot(isTRUE(res$ok), isTRUE(res$alterou), nrow(res$auditoria) == 4L)
stopifnot(
  identical(res$dt[COLETA == "C1", `coletor/nome`][1:3], c("Pessoa Um", "Pessoa Dois", "Pessoa Três")),
  all(res$dt[COLETA == "C1", `coletor/cpf`] == ""),
  res$auditoria[COLETA == "C1", n_cpfs_descartados] == 1L,
  res$auditoria[COLETA == "C1", motivo_cpf] == "cpf_unico_para_varios_nomes_associacao_ambigua"
)
stopifnot(
  identical(res$dt[COLETA == "C2", `coletor/nome`][1L], "Pessoa Única"),
  identical(res$dt[COLETA == "C2", `coletor/cpf`][1L], "52998224725"),
  res$auditoria[COLETA == "C2", n_cpfs_preservados] == 1L
)
stopifnot(
  identical(res$dt[COLETA == "C3", `coletor/nome`][1:3], c("Pessoa A", "Pessoa B", "Pessoa C")),
  all(res$dt[COLETA == "C3", `coletor/cpf`] == "")
)
stopifnot(
  identical(res$dt[COLETA == "C4", `coletor/nome`][1:3], c("Pessoa Alfa", "Pessoa Beta", "Pessoa Gama")),
  identical(res$dt[COLETA == "C4", `coletor/cpf`][1:3], c("52998224725", "", "")),
  res$auditoria[COLETA == "C4", formato_origem] == "mapa_textual_aspas_simples_multiplos_pares",
  res$auditoria[COLETA == "C4", n_cpfs_preservados] == 1L,
  res$auditoria[COLETA == "C4", n_cpfs_descartados] == 1L
)
stopifnot(
  file.exists(file.path(env$MONITORA_OUTPUT_DIR, "03_auditorias", "cadastro", "auditoria_sanitizacao_coletores.csv")),
  !any(grepl("Pessoa|52998224725|017", paste(capture.output(print(res$auditoria)), collapse = " ")))
)
corrig_materializado <- env$monitora_coletores_repeat_materializar_corrig(
  copy(res$dt), output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, contexto = "teste_registros_corrig"
)
c4 <- corrig_materializado[COLETA == "C4"]
proj_nome <- env$monitora_coletores_repeat_projetar_validado(c4, "coletor/nome")$valor
proj_cpf <- env$monitora_coletores_repeat_projetar_validado(c4, "coletor/cpf")$valor
stopifnot(
  identical(proj_nome[1:3], c("Pessoa Alfa", "Pessoa Beta", "Pessoa Gama")),
  identical(proj_cpf[1:3], c("52998224725", "", "")),
  all(proj_nome[4:101] == ""), all(proj_cpf[2:101] == "")
)

segunda <- env$monitora_coletores_repeat_sanitizar_legado(
  copy(res$dt), output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, contexto = "teste_idempotencia", abortar_nao_reconhecido = TRUE
)
stopifnot(!isTRUE(segunda$alterou), nrow(segunda$auditoria) == 0L, identical(res$dt, segunda$dt))

inseguro <- bloco("CX", "{'Pessoa A': '111'}, {'Pessoa B': '222'}")
erro <- tryCatch({
  env$monitora_coletores_repeat_sanitizar_legado(
    inseguro, output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
    exec_id = env$MONITORA_EXEC_ID, contexto = "teste_bloqueio", abortar_nao_reconhecido = TRUE
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("Sanitização de coletores bloqueada", erro, fixed = TRUE))

ocorrencias <- data.table(
  ocorrencia_id = c("occ_1", "occ_2"), tipo_ocorrencia = c("mudanca_formacao_vegetacional", "mudanca_formacao_vegetacional"),
  COLETA = c("C1", "C2"), UC = "UC teste", EA = "EA1", UA = "UA1", ANO = c("2025", "2026")
)
eventos <- data.table(
  evento_justificativa_id = c("jst_1", "jst_2"), evento_lote_id = "jlt_1",
  ordem_no_lote = 1:2, n_ocorrencias_lote = 2L, ocorrencia_id = c("occ_1", "occ_2"),
  status_evento = "vigente", timestamp_evento = "2026-08-07 12:00:00.000000",
  exec_id = env$MONITORA_EXEC_ID, script_versao = env$MONITORA_SCRIPT_VERSAO,
  responsavel = "Pessoa responsável", tipo_justificativa = "pendencia_legitima",
  justificativa = "Mudança ecológica documentada e confirmada em campo.",
  tipo_ocorrencia = "mudanca_formacao_vegetacional", COLETA = c("C1", "C2"),
  UC = "UC teste", EA = "EA1", UA = "UA1", ANO = c("2025", "2026"), evento_origem_id = NA_character_
)
just <- env$monitora_pendencias_justificativas_persistir(ocorrencias, eventos)
stopifnot(
  nrow(just$pendencias_remanescentes) == 2L,
  all(just$pendencias_remanescentes$status_justificativa == "vigente"),
  identical(unique(just$pendencias_remanescentes$evento_lote_id), "jlt_1"),
  identical(as.integer(just$pendencias_remanescentes$ordem_no_lote), 1:2),
  identical(unique(just$historico$evento_lote_id), "jlt_1"),
  identical(as.integer(just$historico$ordem_no_lote), 1:2),
  file.exists(just$arquivo_historico), file.exists(just$arquivo_pendencias)
)
doc_just <- env$monitora_doc_pendencias_justificadas_dt(env$MONITORA_OUTPUT_DIR)
doc_col <- env$monitora_doc_sanitizacao_coletores_dt(env$MONITORA_OUTPUT_DIR)
stopifnot(nrow(doc_just$detalhe) == 2L, sum(doc_just$resumo$n_ocorrencias) == 2L, nrow(doc_col$resumo) >= 3L)

docs <- file.path(raiz, "docs")
manual <- env$monitora_manual_usuario_gerar(docs_dir = docs, versao = "2.9.3-dev-test", formatos = c("html", "pdf"))
rmd_manual <- manual[grepl("\\.Rmd$", manual)][1L]
html_manual <- manual[grepl("\\.html$", manual)][1L]
pdf_manual <- manual[grepl("\\.pdf$", manual)][1L]
stopifnot(
  length(rmd_manual) == 1L, file.exists(rmd_manual),
  length(html_manual) == 1L, file.exists(html_manual), file.info(html_manual)$size > 0L,
  length(pdf_manual) == 1L, file.exists(pdf_manual), file.info(pdf_manual)$size > 0L
)
texto_manual <- paste(readLines(rmd_manual, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
texto_manual_html <- paste(readLines(html_manual, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (termo in c(
  "# Início rápido", "## Equipe da COLETA", "CPF é opcional", "## Impactos de manejo e uso",
  "## Justificar pendências", "justificativa em lote", "## Validação espacial",
  "mudança de formação vegetacional", "# Sanitizações automáticas", "# Solução de problemas",
  "# Proteção de dados", "output/03_auditorias/relatorios_validacao/"
)) stopifnot(grepl(termo, texto_manual, fixed = TRUE))
stopifnot(!grepl("</ a>|<br >|< code>|</ code>|&lt;br|&lt;/?a", texto_manual_html, perl = TRUE))

relatorio <- env$monitora_relatorio_validacao_consolidado_gerar(
  registros_corrig = res$dt,
  output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = env$MONITORA_EXEC_ID, responsavel = "Pessoa responsável",
  instituicao = "CBC/ICMBio", formatos = c("html", "pdf")
)
rmd_relatorio <- relatorio[grepl("\\.Rmd$", relatorio)][1L]
html_relatorio <- relatorio[grepl("\\.html$", relatorio)][1L]
pdf_relatorio <- relatorio[grepl("\\.pdf$", relatorio)][1L]
stopifnot(
  length(rmd_relatorio) == 1L, file.exists(rmd_relatorio),
  length(html_relatorio) == 1L, file.exists(html_relatorio), file.info(html_relatorio)$size > 0L,
  length(pdf_relatorio) == 1L, file.exists(pdf_relatorio), file.info(pdf_relatorio)$size > 0L
)
texto_relatorio <- paste(readLines(rmd_relatorio, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("# Pendências remanescentes e justificativas", texto_relatorio, fixed = TRUE),
  grepl("# Sanitização automática do repeat de coletores", texto_relatorio, fixed = TRUE),
  length(list.files(dirname(rmd_relatorio), pattern = "relatorio_validacao_consolidado_.*\\.json$")) == 1L,
  length(list.files(file.path(dirname(rmd_relatorio), "dados"), pattern = "^pendencias_remanescentes_com_justificativas_.*\\.csv$")) == 1L,
  length(list.files(file.path(dirname(rmd_relatorio), "dados"), pattern = "^auditoria_sanitizacao_coletores_.*\\.csv$")) == 1L
)

sem_legado <- data.table(
  COLETA = rep(paste0("P", seq_len(1000L)), each = 101L),
  `coletor/nome` = rep(c("Pessoa canônica", rep("", 100L)), 1000L),
  COLETORES = rep(c("Pessoa canônica", rep("", 100L)), 1000L),
  `coletor/cpf` = ""
)
tempo_sem_legado <- system.time(
  caminho_quente <- env$monitora_coletores_repeat_sanitizar_legado(
    sem_legado, output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
    exec_id = env$MONITORA_EXEC_ID, contexto = "benchmark_sem_legado", abortar_nao_reconhecido = TRUE
  )
)[["elapsed"]]
stopifnot(!isTRUE(caminho_quente$alterou), nrow(caminho_quente$auditoria) == 0L, tempo_sem_legado < 5)

cat(sprintf("V293_SANITIZACAO_COLETORES_JUSTIFICATIVAS_MANUAL_TESTS_OK tempo_sem_legado_101000_linhas=%.3fs\n", tempo_sem_legado))
