script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
if (!requireNamespace("data.table", quietly = TRUE)) stop("Pacote data.table obrigatorio.", call. = FALSE)

# --------------------------------------------------------------------------
# 1. Carregar funcoes em ambiente isolado (apenas definicoes de funcao)
#    As funcoes obrigatorias estao dentro de um bloco if gigante (expr 920),
#    entao o carregador percorre recursivamente if/{} sem executar o resto.
# --------------------------------------------------------------------------
exprs <- parse(script_path)

env <- new.env(parent = globalenv())

carregar_fn_defs <- function(expr_list, env) {
  for (expr in expr_list) {
    if (!is.call(expr)) next
    op <- tryCatch(as.character(expr[[1L]])[1L], error = function(e) "")
    if (op %in% c("<-", "=") && length(expr) >= 3L) {
      rhs <- expr[[3L]]
      if (is.call(rhs) &&
          tryCatch(identical(as.character(rhs[[1L]]), "function"), error = function(e) FALSE)) {
        tryCatch(eval(expr, envir = env), error = function(e) NULL)
      }
    } else if (op == "{") {
      sub <- as.list(expr)[-1L]
      carregar_fn_defs(sub, env)
    } else if (op == "if") {
      if (length(expr) >= 3L) carregar_fn_defs(list(expr[[3L]]), env)
      if (length(expr) >= 4L) carregar_fn_defs(list(expr[[4L]]), env)
    }
  }
}

carregar_fn_defs(as.list(exprs), env)

fns_obrigatorias <- c(
  "monitora_publicacao_ab_auditar_pendencias_impeditivas_complementares_101_encostam",
  "monitora_publicacao_ab_auditar_pendencias_impeditivas",
  "monitora_registros_validados_contrato_corrig_liberado",
  "monitora_registros_validados_exportar"
)
for (fn in fns_obrigatorias) {
  if (!exists(fn, envir = env, inherits = FALSE)) {
    stop("Funcao nao carregada no env: ", fn, call. = FALSE)
  }
}

# --------------------------------------------------------------------------
# 2. Salvar/restaurar flags globais (limpeza garantida)
# --------------------------------------------------------------------------
globals_mod <- c(
  "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",
  "MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21",
  "MONITORA_REGISTROS_VALIDADOS_GERADO",
  "MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS",
  "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_RESUMO",
  "MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS_AUDITORIA"
)
saved_globals <- lapply(globals_mod, function(g) get0(g, envir = .GlobalEnv, inherits = FALSE))
names(saved_globals) <- globals_mod
on.exit({
  for (g in globals_mod) {
    val <- saved_globals[[g]]
    if (is.null(val)) {
      if (exists(g, envir = .GlobalEnv, inherits = FALSE)) rm(list = g, envir = .GlobalEnv)
    } else {
      assign(g, val, envir = .GlobalEnv)
    }
  }
}, add = TRUE)

# --------------------------------------------------------------------------
# 3. Dados minimos — sem coluna Encostam/tipo_forma_vida
#    Isso garante encostam_coluna_nao_resolvida (Encostam, severidade impeditiva)
#    e ponto_sem_interceptacao (nativo de AB) com linhas reais.
#    marcar_base = FALSE evita problema de NA em linha_indice do Encostam.
# --------------------------------------------------------------------------
tmp <- tempfile(pattern = "trava_rv_test_")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

dt_test <- data.table::data.table(
  COLETA       = c("COL001", "COL002"),
  UC           = c("UC-A",   "UC-A"),
  EA           = c("EA-001", "EA-001"),
  UA           = c("UA-001", "UA-001"),
  CICLO        = c("Ciclo-1", "Ciclo-1"),
  CAMPANHA     = c("Camp-1",  "Camp-1"),
  ponto_amostral = c("1", "2")
  # sem tipo_forma_vida → ponto_sem_interceptacao + encostam_coluna_nao_resolvida
)

# --------------------------------------------------------------------------
# Teste 1: AB detecta pendencias 101/Encostam e seta flag global
# --------------------------------------------------------------------------
res_ab <- env$monitora_publicacao_ab_auditar_pendencias_impeditivas(
  data.table::copy(dt_test),
  contexto    = "teste_funcional",
  output_dir  = tmp,
  log_dir     = tmp,
  gravar      = TRUE,
  marcar_base = FALSE
)

if (!is.list(res_ab))                  stop("AB deve retornar lista.", call. = FALSE)
if (!("auditoria" %in% names(res_ab))) stop("AB deve incluir 'auditoria' na lista.", call. = FALSE)

aud <- data.table::as.data.table(res_ab$auditoria)
if (!nrow(aud)) stop("AB deve detectar ao menos uma pendencia impeditiva.", call. = FALSE)

tipos_enc <- c(
  "encostam_desatualizado", "encostam_token_sem_inferior", "encostam_inferior_sem_token",
  "encostam_solo_nu_conflitante", "encostam_token_desconhecido",
  "encostam_desconhecida_superior_only", "encostam_outra_forma_vida",
  "encostam_coluna_nao_resolvida"
)
tipos_101 <- c(
  "atributo_101_nao_resolvido", "atributo_101_alias_conflitante",
  "cadastro_uc_ausente_ou_invalida", "cadastro_ea_ausente_ou_invalida",
  "cadastro_ua_ausente_ou_invalida", "cadastro_ciclo_ausente_ou_invalido",
  "cadastro_campanha_ausente_ou_invalida", "atributo_101_formato_invalido",
  "atributo_101_valor_fora_dominio"
)

if (!any(aud$tipo_pendencia %in% c(tipos_enc, tipos_101))) {
  stop("AB deve detectar ao menos uma pendencia de auditoria 101 ou Encostam. Encontrado: ",
       paste(unique(aud$tipo_pendencia), collapse = ", "), call. = FALSE)
}

if (!isTRUE(get0("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS", inherits = TRUE))) {
  stop("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS deve ser TRUE apos AB com pendencias.",
       call. = FALSE)
}

# --------------------------------------------------------------------------
# Teste 2: registros_validados bloqueado com PENDENCIAS=TRUE e CONTRATO=FALSE
# --------------------------------------------------------------------------
assign("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",    TRUE,  envir = .GlobalEnv)
assign("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", FALSE, envir = .GlobalEnv)
assign("MONITORA_REGISTROS_VALIDADOS_GERADO",                 FALSE, envir = .GlobalEnv)
assign("MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS",            "S",   envir = .GlobalEnv)

res_exp <- env$monitora_registros_validados_exportar(
  data.table::copy(dt_test),
  output_dir             = tmp,
  log_dir                = tmp,
  exec_id                = "teste_funcional",
  abortar                = FALSE,
  contrato_corrig_validado = TRUE
)

if (is.null(res_exp)) {
  stop("registros_validados_exportar retornou NULL inesperadamente (opcao ativa ou VALIDADOS_GERADO errado?).",
       call. = FALSE)
}
if (!isTRUE(res_exp$bloqueado)) {
  stop("registros_validados_exportar deve retornar bloqueado=TRUE quando ha pendencias impeditivas.",
       call. = FALSE)
}
if (isTRUE(get0("MONITORA_REGISTROS_VALIDADOS_GERADO", inherits = TRUE))) {
  stop("MONITORA_REGISTROS_VALIDADOS_GERADO deve ser FALSE quando exportar esta bloqueado.",
       call. = FALSE)
}
arqs_rv <- c(
  file.path(tmp, "registros_validados.csv"),
  file.path(tmp, "01_produtos_dados", "registros_validados.csv")
)
for (arq in arqs_rv) {
  if (file.exists(arq)) {
    stop("registros_validados.csv nao deve ser criado quando bloqueado: ", arq, call. = FALSE)
  }
}

# --------------------------------------------------------------------------
# Teste 3: helper monitora_registros_validados_contrato_corrig_liberado
# --------------------------------------------------------------------------
liberado <- env$monitora_registros_validados_contrato_corrig_liberado

# 3a: contrato_corrig_validado = FALSE → sempre FALSE
assign("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",    FALSE, envir = .GlobalEnv)
assign("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", TRUE, envir = .GlobalEnv)
if (!isFALSE(liberado(contrato_corrig_validado = FALSE))) {
  stop("Helper deve retornar FALSE quando contrato_corrig_validado = FALSE.", call. = FALSE)
}

# 3b: pendencias = TRUE → FALSE mesmo com contrato e argumento OK
assign("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",    TRUE, envir = .GlobalEnv)
assign("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", TRUE, envir = .GlobalEnv)
if (!isFALSE(liberado(contrato_corrig_validado = TRUE))) {
  stop("Helper deve retornar FALSE quando ha pendencias impeditivas.", call. = FALSE)
}

# 3c: tudo limpo → TRUE
assign("MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS",    FALSE, envir = .GlobalEnv)
assign("MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21", TRUE, envir = .GlobalEnv)
if (!isTRUE(liberado(contrato_corrig_validado = TRUE))) {
  stop("Helper deve retornar TRUE quando contrato validado e sem pendencias.", call. = FALSE)
}

cat("FUNCIONAL_TRAVA_REGISTROS_VALIDADOS_LEVE_OK\n")
