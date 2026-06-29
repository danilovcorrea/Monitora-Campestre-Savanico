script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Pacote data.table e obrigatorio para este teste.", call. = FALSE)
}

exprs <- parse(script_path)

lhs_nome <- function(expr) {
  if (!is.call(expr) || !as.character(expr[[1L]]) %in% c("<-", "=")) return(NA_character_)
  alvo <- expr[[2L]]
  if (is.symbol(alvo)) return(as.character(alvo))
  NA_character_
}

rhs_eh_funcao <- function(expr) {
  is.call(expr) &&
    as.character(expr[[1L]]) %in% c("<-", "=") &&
    is.call(expr[[3L]]) &&
    identical(as.character(expr[[3L]][[1L]]), "function")
}

env <- new.env(parent = globalenv())
for (expr in exprs) {
  if (is.call(expr) && identical(as.character(expr[[1L]]), "if") &&
      grepl("MONITORA_CORRECAO_COLUNAS_PROTEGIDAS", paste(deparse(expr), collapse = "\n"), fixed = TRUE)) {
    eval(expr, envir = env)
  } else if (rhs_eh_funcao(expr)) {
    eval(expr, envir = env)
  }
}

if (!exists("monitora_registros_corrig_auditar_encostam_derivado", envir = env, inherits = FALSE)) {
  stop("Funcao monitora_registros_corrig_auditar_encostam_derivado nao carregada.", call. = FALSE)
}

auditar <- function(dt) {
  data.table::as.data.table(env$monitora_registros_corrig_auditar_encostam_derivado(dt))
}

exigir_tipo <- function(aud, tipo) {
  if (!(tipo %in% aud$tipo_pendencia)) {
    stop("Pendencia esperada ausente: ", tipo, ". Encontrado: ", paste(unique(aud$tipo_pendencia), collapse = ", "), call. = FALSE)
  }
}

base <- data.table::data.table(
  COLETA = paste0("COL", 1:3),
  UC = "UC-A",
  EA = "EA-001_Cps",
  UA = paste0("UA-00", 1:3),
  CICLO = "Ciclo-2025_VgCS",
  CAMPANHA = "Campanha-2025_VgCS",
  `amostragem/registro/tipo_forma_vida` = c("nativa", "exotica", "solo_nu"),
  `amostragem/registro/forma_vida_nativa` = c("graminoide", "", ""),
  `amostragem/registro/forma_vida_exotica` = c("", "graminoide", ""),
  `amostragem/registro/forma_vida_seca_morta` = c("", "", "")
)

base_original <- data.table::copy(base)
aud_ok <- auditar(base)
if (nrow(aud_ok) != 0L) {
  stop("Base sintetica coerente deveria retornar zero pendencias; retornou: ", paste(unique(aud_ok$tipo_pendencia), collapse = ", "), call. = FALSE)
}
if (!identical(base, base_original)) {
  stop("Auditoria alterou o dt original no caso sem pendencias.", call. = FALSE)
}

dt_token_sem_inferior <- data.table::copy(base[1])
dt_token_sem_inferior[, `amostragem/registro/forma_vida_nativa` := ""]
aud_token_sem_inferior <- auditar(dt_token_sem_inferior)
exigir_tipo(aud_token_sem_inferior, "encostam_desatualizado")
exigir_tipo(aud_token_sem_inferior, "encostam_token_sem_inferior")

dt_inferior_sem_token <- data.table::copy(base[1])
dt_inferior_sem_token[, `amostragem/registro/tipo_forma_vida` := ""]
aud_inferior_sem_token <- auditar(dt_inferior_sem_token)
exigir_tipo(aud_inferior_sem_token, "encostam_inferior_sem_token")

dt_solo <- data.table::copy(base[1])
dt_solo[, `amostragem/registro/tipo_forma_vida` := "solo_nu nativa"]
aud_solo <- auditar(dt_solo)
exigir_tipo(aud_solo, "encostam_solo_nu_conflitante")

dt_banana <- data.table::copy(base[3])
dt_banana[, `amostragem/registro/tipo_forma_vida` := "banana"]
aud_banana <- auditar(dt_banana)
exigir_tipo(aud_banana, "encostam_token_desconhecido")

dt_desc <- data.table::copy(base[3])
dt_desc[, `amostragem/registro/tipo_forma_vida` := "desconhecida"]
aud_desc <- auditar(dt_desc)
exigir_tipo(aud_desc, "encostam_desconhecida_superior_only")

dt_outra <- data.table::copy(base[3])
dt_outra[, `amostragem/registro/tipo_forma_vida` := "outra_forma_vida"]
aud_outra <- auditar(dt_outra)
exigir_tipo(aud_outra, "encostam_outra_forma_vida")

dt_outros <- data.table::copy(base[3])
dt_outros[, `amostragem/registro/tipo_forma_vida` := "outros"]
aud_outros <- auditar(dt_outros)
exigir_tipo(aud_outros, "encostam_outra_forma_vida")

dt_sem_col <- data.table::copy(base)
dt_sem_col[, `amostragem/registro/tipo_forma_vida` := NULL]
aud_sem_col <- auditar(dt_sem_col)
exigir_tipo(aud_sem_col, "encostam_coluna_nao_resolvida")
if (!is.na(aud_sem_col$linha[1L]) || isTRUE(aud_sem_col$corrigivel_no_painel[1L])) {
  stop("encostam_coluna_nao_resolvida deve ser linha NA e nao corrigivel no painel.", call. = FALSE)
}

dt_mut <- data.table::copy(base[1])
dt_mut_original <- data.table::copy(dt_mut)
invisible(auditar(dt_mut))
if (!identical(dt_mut, dt_mut_original)) {
  stop("Auditoria alterou o dt original.", call. = FALSE)
}

cat("AUDITORIA_ENCOSTAM_DERIVADO_OK\n")
