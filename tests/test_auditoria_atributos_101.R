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
  "monitora_registros_corrig_auditar_atributos_101"
)

lhs_nome <- function(expr) {
  if (!is.call(expr) || !as.character(expr[[1L]]) %in% c("<-", "=")) return(NA_character_)
  alvo <- expr[[2L]]
  if (is.symbol(alvo)) return(as.character(alvo))
  NA_character_
}

env <- new.env(parent = globalenv())
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

contrato_base <- data.table::data.table(
  atributo_canonico = c("uc", "ea", "ua", "ciclo", "campanha", "data_validacao", "q_integer", "q_decimal", "q_time", "q_datetime"),
  coluna_materializada = c("uc", "ea", "ua", "ciclo", "campanha", "data_validacao", "q_integer", "q_decimal", "q_time", "q_datetime"),
  aliases_aceitos = c("uc|UC", "ea|EA", "ua|UA", "ciclo|Ciclo|CICLO", "campanha|Campanha|CAMPANHA", "data_validacao", "q_integer", "q_decimal", "q_time", "q_datetime"),
  painel_101 = TRUE,
  editavel_painel = TRUE,
  tipo_base = c("select_one", "select_one", "select_one", "select_one", "select_one", "date", "integer", "decimal", "time", "datetime"),
  list_name = c("uc", "ea", "ua", "ciclo", "campanha", "", "", "", "", "")
)

dt_base <- data.table::data.table(
  COLETA = c("COL1", "COL2"),
  UC = c("UC-A", "UC-A"),
  EA = c("EA-001_Cps", "EA-001_Cps"),
  UA = c("UA-001_VgCS", "UA-001_VgCS"),
  Ciclo = c("Ciclo-2025_VgCS", "Ciclo-2025_VgCS"),
  Campanha = c("Campanha-2025_VgCS", "Campanha-2025_VgCS"),
  data_validacao = c("2026-06-29", "2026-06-30"),
  q_integer = c("1", "2"),
  q_decimal = c("1.25", "2.50"),
  q_time = c("12:30", "23:59:58"),
  q_datetime = c("2026-06-29 12:30:00", "2026-06-30T23:59:58Z")
)

auditar <- function(dt, contrato = contrato_base) {
  data.table::as.data.table(env$monitora_registros_corrig_auditar_atributos_101(dt, contrato))
}

exigir_tipo <- function(aud, tipo) {
  if (!(tipo %in% aud$tipo_pendencia)) {
    stop("Pendencia esperada ausente: ", tipo, ". Encontrado: ", paste(unique(aud$tipo_pendencia), collapse = ", "), call. = FALSE)
  }
}

dt_original <- data.table::copy(dt_base)
aud_ok <- auditar(dt_base)
if (nrow(aud_ok) != 0L) {
  stop("Base sintetica coerente deveria retornar zero pendencias; retornou: ", paste(unique(aud_ok$tipo_pendencia), collapse = ", "), call. = FALSE)
}
if (!identical(dt_base, dt_original)) {
  stop("Auditoria alterou o dt original no caso sem pendencias.", call. = FALSE)
}

dt_sem_attr <- data.table::copy(dt_base)
dt_sem_attr[, q_decimal := NULL]
aud_sem_attr <- auditar(dt_sem_attr)
exigir_tipo(aud_sem_attr, "atributo_101_nao_resolvido")

dt_alias <- data.table::copy(dt_base)
dt_alias[, ea := c("EA-DIVERGENTE", "EA-001_Cps")]
dt_alias_original <- data.table::copy(dt_alias)
aud_alias <- auditar(dt_alias)
exigir_tipo(aud_alias, "atributo_101_alias_conflitante")
if (!identical(dt_alias, dt_alias_original)) {
  stop("Auditoria alterou o dt original no caso de alias conflitante.", call. = FALSE)
}

dt_uc <- data.table::copy(dt_base)
dt_uc[, UC := ""]
aud_uc <- auditar(dt_uc)
exigir_tipo(aud_uc, "cadastro_uc_ausente_ou_invalida")

dt_ea <- data.table::copy(dt_base)
dt_ea[, EA := ""]
aud_ea <- auditar(dt_ea)
exigir_tipo(aud_ea, "cadastro_ea_ausente_ou_invalida")

dt_ua <- data.table::copy(dt_base)
dt_ua[, UA := ""]
aud_ua <- auditar(dt_ua)
exigir_tipo(aud_ua, "cadastro_ua_ausente_ou_invalida")

dt_ciclo <- data.table::copy(dt_base)
dt_ciclo[, Ciclo := ""]
aud_ciclo <- auditar(dt_ciclo)
exigir_tipo(aud_ciclo, "cadastro_ciclo_ausente_ou_invalido")

dt_campanha <- data.table::copy(dt_base)
dt_campanha[, Campanha := ""]
aud_campanha <- auditar(dt_campanha)
exigir_tipo(aud_campanha, "cadastro_campanha_ausente_ou_invalida")

dt_fmt <- data.table::copy(dt_base)
dt_fmt[1L, `:=`(
  data_validacao = "2026-02-31",
  q_integer = "1.5",
  q_decimal = "1,5",
  q_time = "25:61",
  q_datetime = "2026-02-31 24:00:00"
)]
aud_fmt <- auditar(dt_fmt)
exigir_tipo(aud_fmt, "atributo_101_formato_invalido")
if (aud_fmt[tipo_pendencia == "atributo_101_formato_invalido", .N] < 5L) {
  stop("Esperadas pendencias de formato para integer/decimal/date/time/datetime.", call. = FALSE)
}

dt_painel_false <- data.table::copy(dt_base)
dt_painel_false[, q_decimal := NULL]
contrato_painel_false <- data.table::copy(contrato_base)
contrato_painel_false[atributo_canonico == "q_decimal", painel_101 := FALSE]
aud_painel_false <- auditar(dt_painel_false, contrato_painel_false)
if ("atributo_101_nao_resolvido" %in% aud_painel_false$tipo_pendencia) {
  stop("Auditoria nao deve avaliar atributos com painel_101 != TRUE.", call. = FALSE)
}

cat("AUDITORIA_ATRIBUTOS_101_OK\n")
