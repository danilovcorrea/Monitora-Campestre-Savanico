args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library(data.table))
script <- if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.2.R"
script <- normalizePath(script, mustWork = TRUE)
parsed <- parse(file = script)
body <- as.call(c(list(as.name("{")), as.list(parsed)))
env <- new.env(parent = globalenv())

carregar_definicoes <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  cabeca <- if (is.symbol(node[[1L]])) as.character(node[[1L]]) else ""
  if (cabeca %in% c("<-", "=") && is.symbol(node[[2L]])) {
    nome <- as.character(node[[2L]])
    rhs <- node[[3L]]
    carregar <- is.call(rhs) && identical(as.character(rhs[[1L]]), "function")
    carregar <- carregar || (is.symbol(rhs) && grepl("^monitora_", as.character(rhs)))
    carregar <- carregar || nome %in% c("MONITORA_COL_ROW_ID", "MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID", "MONITORA_CORRECAO_COLUNAS_PROTEGIDAS")
    if (isTRUE(carregar)) try(eval(node, envir = env), silent = TRUE)
    return(invisible(NULL))
  }
  if (identical(cabeca, "function")) return(invisible(NULL))
  for (ii in seq_along(node)[-1L]) carregar_definicoes(node[[ii]])
  invisible(NULL)
}
carregar_definicoes(body)

env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_SCRIPT_VERSAO <- "2.9.2-test"
env$MONITORA_SCRIPT_BUILD_ID <- "test"
for (nm_cache in c(
  ".MONITORA_MAPA_COLUNAS_CANONICAS_CACHE",
  ".MONITORA_CONTRATO_MOVIMENTO_CACHE",
  ".MONITORA_FECHAMENTO_HIERARQUICO_CACHE",
  ".MONITORA_PUBLICACAO_AD_CACHE_CONTRATUAL"
)) {
  assign(nm_cache, new.env(parent = emptyenv()), envir = env)
}
env$monitora_fwrite <- function(x, arquivo, na = "") {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(data.table::as.data.table(x), arquivo, na = na, bom = TRUE)
}

stopifnot(identical(
  unname(env$monitora_correcao_aplicar_operacao(c("Texto atual", "Texto atual"), "append_text", "Complemento")),
  c("Texto atual; Complemento", "Texto atual; Complemento")
))

form_col <- "Qual a formação vegetacional onde está situado o transecto?"
formveg <- data.table::data.table(
  UC = "UC teste", EA = "EA1", UA = c(rep("UA1", 6L), rep("UA2", 2L)),
  ANO = c(2024, 2024, 2025, 2025, 2025, 2025, 2026, 2026),
  COLETA = c("C1", "C1", "C2", "C2", "C3", "C3", "C4", "C4"),
  MONITORA_ROW_ID = paste0("r", 1:8)
)
formveg[, (form_col) := c("Campestre", "Campestre", "Savânica", "Savânica", "Campestre", "Campestre", "Campestre", "Savânica")]
diag_form <- env$monitora_diag_formacao_vegetacional_temporal(formveg)
stopifnot(nrow(diag_form) == 4L)
stopifnot(all(c("mudanca_formacao_entre_anos", "formacao_divergente_mesma_ua_ano", "formacao_nao_uniforme_na_coleta") %in% unique(unlist(strsplit(diag_form$classes_diagnosticas, ";", fixed = TRUE)))))

esp <- data.table::data.table(
  UC = c("UC", "UC"), EA = c("EA", "EA"), UA = c("1", "1"),
  ANO = c("2024", "2025"), COLETA = c("F", "A"), coordenadas_validas = TRUE,
  coord_inicio_txt = c("-10 -45", "-10.1 -45.1"), coord_fim_txt = c("-10.01 -45.01", "-10.11 -45.11"),
  n_linhas = c(101L, 83L)
)
ops <- env$monitora_espacial_painel_operacoes_lote_ano(
  esp, ano_fonte = "2024", ano_destino = "2025",
  tipo_operacao = "copiar_inicio", n_linhas_esperado = 101L,
  confirmar_abrangencia = TRUE, justificativa = "Teste de abrangência observada"
)
stopifnot(!nzchar(ifelse(is.na(ops$erro), "", ops$erro)))
stopifnot(nrow(ops$ops) == 1L, identical(as.character(ops$ops$n_linhas_esperado), "83"))

dir_just <- tempfile("just_v292_")
dir.create(file.path(dir_just, "input", "linhagem"), recursive = TRUE)
dir.create(file.path(dir_just, "out"), recursive = TRUE)
env$MONITORA_INPUT_DIR <- file.path(dir_just, "input")
env$MONITORA_CORRECOES_DIR <- file.path(dir_just, "out")
env$MONITORA_EXEC_ID <- "exec_test_1"
oc <- data.table::data.table(
  tipo_ocorrencia = "teste", COLETA = "C1", UC = "UC", EA = "EA", UA = "1", ANO = "2025",
  linha_indice = 1L, monitora_row_id = "r1", ocorrencia_categoria = "", ocorrencia_token = "", ocorrencia_atributo = "campo"
)
oc[, ocorrencia_id := env$monitora_pendencias_ocorrencia_id(.SD)]
ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")
sess <- env$monitora_pendencias_justificativas_template()[0]
sess <- data.table::data.table(
  evento_justificativa_id = "jst_1", ocorrencia_id = oc$ocorrencia_id,
  status_evento = "vigente", timestamp_evento = ts, exec_id = "exec_test_1",
  script_versao = "2.9.2-test", responsavel = "Teste", tipo_justificativa = "outro",
  justificativa = "Justificativa suficientemente detalhada para teste.", tipo_ocorrencia = "teste",
  COLETA = "C1", UC = "UC", EA = "EA", UA = "1", ANO = "2025", evento_origem_id = NA_character_
)
p1 <- env$monitora_pendencias_justificativas_persistir(oc, sess)
stopifnot(nrow(p1$historico) == 1L, p1$pendencias_remanescentes$status_justificativa == "vigente")
invisible(file.copy(p1$arquivo_historico, file.path(env$MONITORA_INPUT_DIR, "linhagem", basename(p1$arquivo_historico)), overwrite = TRUE))
env$MONITORA_EXEC_ID <- "exec_test_2"
p2 <- env$monitora_pendencias_justificativas_persistir(oc[0], NULL)
stopifnot(any(p2$historico$status_evento == "vigente"), any(p2$historico$status_evento == "encerrada_por_resolucao"))

docx <- tempfile(fileext = ".docx")
stopifnot(is.function(env$monitora_relatorios_analiticos_referencia_docx_materializar))
stopifnot(is.function(env$monitora_relatorios_analiticos_referencia_docx_base64))
stopifnot(is.function(env$monitora_relatorios_analiticos_referencia_docx_sha256))
env$monitora_relatorios_analiticos_referencia_docx_materializar(docx)
dir_docx <- tempfile("docx_check_")
dir.create(dir_docx)
utils::unzip(docx, exdir = dir_docx)
xml <- xml2::read_xml(file.path(dir_docx, "word", "document.xml"))
ns <- xml2::xml_ns(xml)
sz <- xml2::xml_find_first(xml, ".//w:sectPr/w:pgSz", ns)
mar <- xml2::xml_find_first(xml, ".//w:sectPr/w:pgMar", ns)
stopifnot(xml2::xml_attr(sz, "w") == "11906", xml2::xml_attr(sz, "h") == "16838")
stopifnot(xml2::xml_attr(mar, "left") == "964", xml2::xml_attr(mar, "right") == "964")

conteudo_docx_teste <- env$monitora_relatorios_analiticos_conteudo_docx(c(
  "---", "title: Teste", "---",
  '<div class="callout">Valor de p ajustado = < 0,001. A evidência foi preservada.<br><br>Parágrafo seguinte.</div>'
))
stopifnot(
  any(grepl(
    "Valor de p ajustado = < 0,001. A evidência foi preservada.",
    conteudo_docx_teste,
    fixed = TRUE
  )),
  any(grepl("Parágrafo seguinte.", conteudo_docx_teste, fixed = TRUE))
)

rbg_dados <- Sys.getenv("MONITORA_TESTE_RBG_DADOS", unset = "")
if (file.exists(rbg_dados)) {
  rbg <- data.table::fread(rbg_dados, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
  rbg_dict <- env$monitora_correcao_dicionario_atributos(rbg, NULL)
  rbg_cols <- c(
    impacto = "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)",
    tipos = "Qual(is)? (impact_manejo_uso)",
    outros = "Outros tipos de manejo ou uso: (impact_manejo_uso)",
    descricao = "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)",
    observacao = "Descreva observações gerais do transecto, caso necessário:"
  )
  rd <- rbg_dict[match(unname(rbg_cols), atributo_coluna_registros_corrig)]
  stopifnot(
    identical(rd$tipo_base_edicao, c("select_one", "select_multiple", "text", "text", "text")),
    identical(rd$acoes_permitidas, c(
      "update;clear", "append_token;remove_token;replace_token",
      "update;append_text;clear", "update;append_text;clear", "update;append_text;clear"
    )),
    all(rd$nivel_schema129_contrato_unico == "coleta"),
    all(rd$escopo_operacional_contrato_unico == "coleta_inteira")
  )
  rbg_op <- data.table::data.table(
    coleta = "18316", escopo_aplicacao = "coleta_inteira",
    n_linhas_esperado = 101L, linhas_alvo_serializadas = NA_character_,
    linha_indice = NA_character_, uuid_registro = NA_character_
  )
  rbg_alvo <- env$monitora_correcao_linhas_alvo_operacao(
    rbg, rbg_op, env$monitora_correcao_colunas_chave(rbg), indice = NULL
  )
  stopifnot(length(rbg_alvo) == 101L, all(as.character(rbg$COLETA[rbg_alvo]) == "18316"))
  texto_antes <- as.character(rbg[[rbg_cols[["observacao"]]]][rbg_alvo])
  texto_depois <- env$monitora_correcao_aplicar_operacao(texto_antes, "append_text", "Revisão complementar")
  stopifnot(all(texto_depois == "Parece campestre. Revisão complementar"))
  tokens <- env$monitora_correcao_aplicar_operacao(rep("pastejo fogo", 101L), "append_token", "trilha")
  stopifnot(all(vapply(tokens, function(z) all(c("pastejo", "fogo", "trilha") %in% env$monitora_correcao_tokenizar(z)), logical(1L))))
  tokens <- env$monitora_correcao_aplicar_operacao(tokens, "remove_token", "fogo")
  stopifnot(!any(vapply(tokens, function(z) "fogo" %in% env$monitora_correcao_tokenizar(z), logical(1L))))
}

pncv_dir <- Sys.getenv("MONITORA_TESTE_PNCV_DIR", unset = "")
pncv_dados <- file.path(pncv_dir, "input", "registros_corrig.csv")
pncv_ops <- file.path(pncv_dir, "output", "02_painel_correcoes", "operacoes_sessao", "correcoes_semanticas_solicitadas_painel_ultima_execucao.csv")
if (file.exists(pncv_dados) && file.exists(pncv_ops)) {
  dados <- data.table::fread(pncv_dados, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)
  movimentos <- data.table::fread(pncv_ops, encoding = "UTF-8", na.strings = NULL, colClasses = "character", showProgress = FALSE)[coleta == "42644"]
  stopifnot(nrow(movimentos) == 7L)
  for (ii in seq_len(nrow(movimentos))) {
    mov <- env$monitora_correcao_aplicar_movimento_forma_vida_atomico(dados, movimentos[ii])
    if (isTRUE(mov$falha)) stop(paste("Movimento PNCV falhou:", paste(mov$audit$mensagem, collapse = " | ")))
    dados <- mov$dt
  }
  pontos <- c("39", "40", "43", "45", "47", "49", "50")
  ponto_col <- "ponto_amostral (amostragem/registro)"
  alvo <- dados[COLETA == "42644" & get(ponto_col) %in% pontos]
  nat_lista <- names(alvo)[grepl("Formas de vida de plantas.*nativas", names(alvo))][1L]
  exo_lista <- names(alvo)[grepl("Formas de vida de plantas.*exóticas", names(alvo))][1L]
  hab_nat <- "amostragem/registro/forma_vida_nativa_samambaia"
  sp_nat <- "Espécie ou nome popular (Samambaia) (amostragem/registro)"
  sp_exo <- c(
    "**Espécies** de <span style=\"\"\"\"color:red\"\"\"\"> samambaias exóticas:</span> (amostragem/registro)",
    "Outra espécie de samambaia exótica: (amostragem/registro)"
  )
  contem <- function(x, token) vapply(as.character(x), function(z) token %in% env$monitora_correcao_tokenizar(z), logical(1L))
  vazio <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  stopifnot(nrow(alvo) == 7L, all(contem(alvo[[nat_lista]], "samambaia")), !any(contem(alvo[[exo_lista]], "samambaia")))
  stopifnot(all(tolower(alvo[[hab_nat]]) == "terrestre"), all(!vazio(alvo[[sp_nat]])))
  stopifnot(all(vapply(sp_exo, function(cc) all(vazio(alvo[[cc]])), logical(1L))))
}

cat("SELECTIVE_TESTS_OK\n")
