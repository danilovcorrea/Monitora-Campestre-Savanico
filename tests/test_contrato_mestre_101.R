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
  "monitora_registros_corrig_contrato_mestre_2025"
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

contrato <- env$monitora_registros_corrig_contrato_mestre_2025()

if (!inherits(contrato, c("data.frame", "data.table"))) {
  stop("Contrato mestre deve retornar data.frame ou data.table.", call. = FALSE)
}
contrato <- data.table::as.data.table(contrato)

cols_essenciais <- c(
  "atributo_canonico",
  "coluna_materializada",
  "aliases_aceitos",
  "origem_contrato",
  "ordem_template",
  "painel_101",
  "editavel_painel",
  "protegido",
  "tipo_base",
  "dominio_tipo",
  "list_name",
  "obrigatorio_registros_corrig",
  "obrigatorio_registros_validados",
  "acao_painel",
  "validacao_materializacao"
)
faltantes_cols <- setdiff(cols_essenciais, names(contrato))
if (length(faltantes_cols)) {
  stop("Colunas essenciais ausentes no contrato mestre: ", paste(faltantes_cols, collapse = ", "), call. = FALSE)
}

n_painel <- contrato[painel_101 == TRUE & editavel_painel == TRUE, .N]
if (!identical(n_painel, 101L)) {
  stop("Esperado exatamente 101 atributos editaveis do painel; encontrado ", n_painel, ".", call. = FALSE)
}

protegidos_editaveis <- contrato[protegido == TRUE & editavel_painel == TRUE]
if (nrow(protegidos_editaveis)) {
  stop(
    "Atributos protegidos aparecem como editaveis: ",
    paste(protegidos_editaveis$atributo_canonico, collapse = ", "),
    call. = FALSE
  )
}

norm <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  gsub("[^a-z0-9]+", "_", x, perl = TRUE)
}

cad_principais <- list(
  uc = "UC",
  ea = "EA",
  ua = "UA",
  ciclo = "Ciclo",
  campanha = "Campanha"
)
contrato[, atributo_norm_teste := norm(atributo_canonico)]
for (canon in names(cad_principais)) {
  hit <- contrato[atributo_norm_teste == canon]
  if (!nrow(hit)) {
    stop("Atributo cadastral principal ausente do contrato: ", canon, call. = FALSE)
  }
  alias_alvo <- cad_principais[[canon]]
  aliases <- paste(hit$aliases_aceitos, collapse = "|")
  if (!grepl(alias_alvo, aliases, fixed = TRUE)) {
    stop("Alias cadastral esperado nao encontrado para ", canon, ": ", alias_alvo, call. = FALSE)
  }
}
contrato[, atributo_norm_teste := NULL]

cat("CONTRATO_MESTRE_OK\n")
cat("CONTRATO_PAINEL_101_OK\n")
