script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Pacote data.table e obrigatorio para este teste.", call. = FALSE)
}

exprs <- parse(script_path)

nomes_necessarios <- c(
  "monitora_correcao_normalizar_nome_coluna",
  "monitora_correcao_coluna_protegida",
  "monitora_correcao_tipo_xlsform",
  "monitora_painel_template_sismonitora_2025",
  "monitora_painel_atributos_editaveis_sismonitora_2025",
  "monitora_painel_atributos_bloqueados_sismonitora_2025",
  "monitora_painel_admin_controlados_sismonitora_2025",
  "monitora_correcao_aliases_admin_sismonitora",
  "monitora_registros_corrig_contrato_mestre_2025",
  "monitora_registros_corrig_contrato_dropdown_painel_101",
  "monitora_registros_corrig_contrato_edicao_painel_101"
)

lhs_nome <- function(expr) {
  if (!is.call(expr) || !as.character(expr[[1L]]) %in% c("<-", "=")) return(NA_character_)
  alvo <- expr[[2L]]
  if (is.symbol(alvo)) return(as.character(alvo))
  NA_character_
}

env <- new.env(parent = globalenv())
env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- unique(c(
  ".id", "id", "ID", "uuid", "UUID", "uuid_registro", "UUID_REGISTRO",
  "coleta_uuid", "COLETA_UUID", "COLETA", "PROTOCOLO", "arquivo_origem",
  "linha_indice", "linha_origem", "linha_origem_registros_corrig",
  "ordem_linha_original", "arquivo_fonte", "source_file",
  "ANO", "DATA_MONITORA_PARSEADA",
  "DATA DO REGISTRO", "DATA DO RECEBIMENTO", "ULTIMA EDICAO",
  "data_do_registro", "data_do_recebimento", "ultima_edicao"
))
for (expr in exprs) {
  nm <- lhs_nome(expr)
  if (!is.na(nm) && nm %in% nomes_necessarios) {
    eval(expr, envir = env)
  }
}

faltantes_fun <- nomes_necessarios[!vapply(nomes_necessarios, exists, logical(1), envir = env, inherits = FALSE)]
if (length(faltantes_fun)) {
  stop("Objetos necessarios nao carregados: ", paste(faltantes_fun, collapse = ", "), call. = FALSE)
}

contrato <- data.table::as.data.table(env$monitora_registros_corrig_contrato_mestre_2025())
dropdown <- data.table::as.data.table(env$monitora_registros_corrig_contrato_dropdown_painel_101(contrato))

if (!inherits(contrato, c("data.frame", "data.table")) || !inherits(dropdown, c("data.frame", "data.table"))) {
  stop("Contrato/dropdown devem retornar data.frame ou data.table.", call. = FALSE)
}

if (dropdown[, .N] != 101L) {
  stop("Dropdown deve conter exatamente 101 atributos; encontrado ", dropdown[, .N], ".", call. = FALSE)
}

ordem_esperada <- contrato[painel_101 == TRUE & editavel_painel == TRUE & protegido != TRUE][order(ordem_template, atributo_canonico), atributo_canonico]
if (!identical(as.character(dropdown$atributo_canonico), as.character(ordem_esperada))) {
  stop("Dropdown nao segue ordem_template do contrato mestre.", call. = FALSE)
}

if (dropdown[protegido == TRUE, .N] > 0L) {
  stop("Dropdown contem atributo protegido.", call. = FALSE)
}

enc <- dropdown[atributo_canonico == "amostragem/registro/tipo_forma_vida"]
if (nrow(enc) != 1L) {
  stop("Encostam/tipo_forma_vida deve estar presente uma vez no dropdown.", call. = FALSE)
}
if (!identical(as.character(enc$acao_painel[1L]), "recalcular_tipo_forma_vida")) {
  stop("Encostam/tipo_forma_vida deve usar acao recalcular_tipo_forma_vida.", call. = FALSE)
}

calc_update <- dropdown[
  tipo_base %in% c("calculate", "hidden", "metadata") &
    grepl("(^|;)update(;|$)|(^|;)clear(;|$)", acao_painel, perl = TRUE)
]
if (nrow(calc_update)) {
  stop(
    "calculate/hidden/metadata com update/clear no dropdown: ",
    paste(calc_update$atributo_canonico, collapse = ", "),
    call. = FALSE
  )
}

original_edicao <- data.table::data.table(
  atributo_coluna_registros_corrig = as.character(dropdown$coluna_materializada),
  tipo_base_edicao = as.character(dropdown$tipo_base),
  xlsform_tipo = as.character(dropdown$tipo_base),
  xlsform_list_name = "",
  papel_coluna = "metadado_original",
  categoria_coluna = "",
  required_bool = FALSE,
  atende_template_sismonitora = TRUE,
  ordem_template_sismonitora = 999999L,
  atributos_template_sismonitora = "",
  posicoes_template_sismonitora = "",
  estrategias_template_sismonitora = "",
  painel_editavel = TRUE,
  painel_motivo_bloqueio = "",
  acoes_permitidas = ""
)
original_edicao <- data.table::rbindlist(
  list(
    data.table::data.table(
      atributo_coluna_registros_corrig = "atributo_fora_dropdown",
      tipo_base_edicao = "text",
      xlsform_tipo = "text",
      xlsform_list_name = "",
      papel_coluna = "fora_101",
      categoria_coluna = "",
      required_bool = FALSE,
      atende_template_sismonitora = FALSE,
      ordem_template_sismonitora = 999999L,
      atributos_template_sismonitora = "",
      posicoes_template_sismonitora = "",
      estrategias_template_sismonitora = "",
      painel_editavel = FALSE,
      painel_motivo_bloqueio = "fora do dropdown",
      acoes_permitidas = ""
    ),
    original_edicao
  ),
  fill = TRUE,
  use.names = TRUE
)

contrato_edicao_101 <- data.table::as.data.table(
  env$monitora_registros_corrig_contrato_edicao_painel_101(original_edicao, dropdown)
)
edicao_dropdown <- contrato_edicao_101[
  atributo_coluna_registros_corrig %in% as.character(dropdown$coluna_materializada)
]

if (nrow(edicao_dropdown) != 101L) {
  stop("Contrato de edicao mesclado deve manter 101 atributos do dropdown.", call. = FALSE)
}

selects <- edicao_dropdown[tipo_base_edicao %in% c("select_one", "select_multiple")]
selects_com_lista <- selects[!is.na(xlsform_list_name) & nzchar(trimws(xlsform_list_name))]
selects_tipo_incompleto <- selects_com_lista[
  xlsform_tipo %in% c("select_one", "select_multiple") |
    !grepl("^(select_one|select_multiple)\\s+\\S+", xlsform_tipo, perl = TRUE)
]
if (nrow(selects_tipo_incompleto)) {
  stop(
    "Selects com list_name devem ter xlsform_tipo completo: ",
    paste(selects_tipo_incompleto$atributo_coluna_registros_corrig, collapse = ", "),
    call. = FALSE
  )
}

perdeu_lista <- merge(
  dropdown[tipo_base %in% c("select_one", "select_multiple") & !is.na(list_name) & nzchar(trimws(list_name)),
           .(coluna_materializada, list_name_contrato = list_name)],
  edicao_dropdown[, .(coluna_materializada = atributo_coluna_registros_corrig, xlsform_list_name)],
  by = "coluna_materializada",
  all.x = TRUE,
  sort = FALSE
)[is.na(xlsform_list_name) | !nzchar(trimws(xlsform_list_name))]
if (nrow(perdeu_lista)) {
  stop(
    "Contrato de edicao perdeu xlsform_list_name em atributo(s) 101: ",
    paste(perdeu_lista$coluna_materializada, collapse = ", "),
    call. = FALSE
  )
}

sem_acao <- edicao_dropdown[is.na(acoes_permitidas) | !nzchar(trimws(acoes_permitidas))]
if (nrow(sem_acao)) {
  stop(
    "Contrato de edicao perdeu acoes_permitidas em atributo(s) 101: ",
    paste(sem_acao$atributo_coluna_registros_corrig, collapse = ", "),
    call. = FALSE
  )
}

enc_edicao <- edicao_dropdown[atributo_coluna_registros_corrig == "amostragem/registro/tipo_forma_vida"]
if (!identical(as.character(enc_edicao$acoes_permitidas[1L]), "recalcular_tipo_forma_vida")) {
  stop("Contrato de edicao deve preservar recalcular_tipo_forma_vida para Encostam/tipo_forma_vida.", call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
inicio <- grep("monitora_painel_choices_atributos_contrato_mestre <- function", linhas, fixed = TRUE)
fim <- grep("contrato_dropdown_painel_101 <- monitora_registros_corrig_contrato_dropdown_painel_101\\(\\)", linhas, perl = TRUE)
if (!length(inicio) || !length(fim) || fim[1L] <= inicio[1L]) {
  stop("Trecho de montagem contratual das choices do dropdown nao encontrado.", call. = FALSE)
}
trecho <- linhas[inicio[1L]:fim[1L]]
if (any(grepl(":=", trecho, fixed = TRUE)) || any(grepl("\\bset\\s*\\(", trecho, perl = TRUE))) {
  stop("Trecho de montagem das choices do dropdown cria/altera colunas com := ou set().", call. = FALSE)
}

cat("DROPDOWN_PAINEL_101_OK\n")
