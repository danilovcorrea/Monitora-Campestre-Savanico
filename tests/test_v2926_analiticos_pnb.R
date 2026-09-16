#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.26.R", mustWork = TRUE)
baseline <- normalizePath(if (length(args) >= 2L) args[[2L]] else "releases/v2.9.25/monitora_campsav_alvo_global_v2.9.25.R", mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
assert(length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq.")

encontrar_funcao <- function(x, nome) {
  achados <- list()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    if (as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L &&
        is.symbol(no[[2L]]) && identical(as.character(no[[2L]]), nome) &&
        is.call(no[[3L]]) && identical(as.character(no[[3L]][[1L]])[1L], "function")) {
      achados[[length(achados) + 1L]] <<- no[[3L]]
    }
    if (length(no) > 1L) for (ii in 2:length(no)) visitar(no[[ii]])
    invisible(NULL)
  }
  visitar(x)
  assert(length(achados) == 1L, paste0("Função não unívoca: ", nome))
  achados[[1L]]
}

env <- new.env(parent = globalenv())
for (nome in c("monitora_sum_tokens_by_group", "monitora_stat_preparar_long_ua",
               "monitora_painel_ano_inicial_filtrar_series")) {
  env[[nome]] <- eval(encontrar_funcao(arvore[[1L]], nome), envir = env)
}
env$monitora_dt_referenciar <- data.table::as.data.table
env$monitora_log_registrar_evento <- function(...) invisible(NULL)
env$monitora_recurso_gc <- function(...) invisible(NULL)
env$monitora_progresso_loop_configurar <- function(...) invisible(NULL)
env$monitora_progresso_loop_avancar <- function(...) invisible(NULL)
env$monitora_plot_rotulo_categoria <- identity
env$monitora_grupo_stat_cols <- c("UC", "UA", "ANO", "form_veg")

# Gate 1: ausência real da família vira zero apenas nas contagens criadas.
bruta <- data.table(
  UC = "UC", UA = c("U1", "U2", "U3"), ANO = 2026L, form_veg = "campestre",
  exotica = c("graminoide", "", "arbusto"), campo_original = c(NA, "x", NA)
)
wide <- env$monitora_sum_tokens_by_group(bruta, "exotica", "exot")
setorder(wide, UA)
assert(nrow(wide) == 3L, "O universo amostrado não foi preservado na tokenização.")
assert(identical(wide$exot_graminoide, c(1L, 0L, 0L)), "Zero estrutural de graminoide incorreto.")
assert(identical(wide$exot_arbusto, c(0L, 0L, 1L)), "Zero estrutural de arbusto incorreto.")
assert(all(is.na(bruta[c(1L, 3L), campo_original])), "Campo original foi imputado indevidamente.")

# Gate 2: cobertura mantém zeros; composição sem denominador permanece indefinida.
long_cob <- env$monitora_stat_preparar_long_ua(wide, c("exot_graminoide", "exot_arbusto"),
  "formas_vida_exoticas", "cobertura", "101")
assert(nrow(long_cob[UA == "U2"]) == 2L && all(long_cob[UA == "U2", valor] == 0),
  "Ausência amostrada não foi mantida como cobertura zero.")
long_prop <- env$monitora_stat_preparar_long_ua(wide, c("exot_graminoide", "exot_arbusto"),
  "formas_vida_exoticas", "proporcao_relativa", "relativo")
assert(nrow(long_prop[UA == "U2"]) == 0L,
  "Composição sem qualquer registro da família recebeu denominador artificial.")
assert(long_prop[UA == "U1" & categoria == "exot_arbusto", valor] == 0,
  "Categoria ausente dentro de família presente não permaneceu zero.")

# Gate 3: coorte de sensibilidade depende da UA amostrada, não da ocorrência do indicador.
series <- data.table(
  UC = "UC", UA = c("U1", "U1", "U2", "U2"), ANO = c(2020L, 2021L, 2021L, 2022L),
  form_veg = "campestre", grupo_grafico = "formas_vida_exoticas",
  tipo_metrica = "proporcao_relativa", categoria = "graminoide", valor = c(1, 1, .2, .3)
)
universo <- data.table(
  UC = "UC", UA = c("U1", "U2", "U1", "U2", "U2"),
  ANO = c(2020L, 2020L, 2021L, 2021L, 2022L), form_veg = "campestre"
)
painel <- env$monitora_painel_ano_inicial_filtrar_series(series, universo)
assert("U2" %in% painel[ano_inicial_painel == 2020L, UA],
  "A coorte continuou condicionada à ocorrência do indicador no ano inicial.")

linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
assert(!grepl("mudanca_periodo <- filtrar_ano_inicial", texto, fixed = TRUE),
  "O relatório principal ainda foi substituído pelo painel fixo do primeiro ano.")
assert(grepl("series_anuais_ua <- series_anuais_ua_total", texto, fixed = TRUE),
  "A série descritiva principal não usa todas as UAs observadas em cada ano.")
assert(!grepl("Referência ≥", texto, fixed = TRUE), "A figura ainda usa rótulo de referência inexato.")
assert(grepl("epoca_calendario_resumo_relatorio", texto, fixed = TRUE),
  "A seção de época não materializa a tabela compacta do calendário.")
assert(grepl("filtrar_estado_relatorio", texto, fixed = TRUE),
  "As tabelas do relatório não omitem formas com zero registros.")
assert(grepl("sensivel_a_composicao_painel", texto, fixed = TRUE) &&
       grepl("ganho_elegibilidade_amostral", texto, fixed = TRUE),
  "Elegibilidade amostral e contradição direcional continuam confundidas.")

# Gate 4: início congelado e módulos fora do escopo permanecem idênticos à pública.
linhas_base <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  assert(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
assert(identical(inicio_congelado(linhas), inicio_congelado(linhas_base)),
  "A inicialização congelada do RStudio foi alterada.")
for (nome in "monitora_correcao_xlsforms_embutidos") {
  assert(identical(encontrar_funcao(arvore[[1L]], nome),
                   encontrar_funcao(parse(baseline, keep.source = FALSE)[[1L]], nome)),
    paste0("Módulo fora do escopo alterado: ", nome))
}

bytes_crlf <- file.info(script)$size + length(linhas)
assert(bytes_crlf < 5 * 1024^2, "A candidata excederia 5 MiB com CRLF no Windows/RStudio.")
cat("TEST_V2926_ANALITICOS_PNB_OK\n")
