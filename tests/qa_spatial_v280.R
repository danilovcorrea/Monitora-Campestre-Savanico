args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Uso: Rscript tests/qa_spatial_v280.R SCRIPT", call. = FALSE)
}

script_path <- normalizePath(args[[1L]], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

carregar_funcoes <- function(path) {
  exprs <- parse(path, keep.source = FALSE, encoding = "UTF-8")
  for (expr in exprs) {
    if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
    rhs <- expr[[3L]]
    if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

carregar_funcoes(script_path)

MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 20
MONITORA_RAIO_ALERTA_ESPACIAL_M <- 10
MONITORA_MIN_COLETAS_CONSENSO_ESPACIAL <- 2L
MONITORA_MIN_COLETAS_CONSENSO_LOO_ESPACIAL <- 1L
MONITORA_VALIDACAO_ESPACIAL_LEAVE_ONE_OUT <- "S"
MONITORA_USAR_ACURACIA_GPS_NA_TRIAGEM <- "S"
MONITORA_ACURACIA_GPS_MAX_ALERTA_M <- 10
MONITORA_VALIDAR_COMPRIMENTO_TRANSECTO <- "S"
MONITORA_COMPRIMENTO_TRANSECTO_ESPERADO_M <- 50
MONITORA_TOLERANCIA_COMPRIMENTO_TRANSECTO_M <- 20

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

dist_vec <- monitora_esp_dist_m(-14, -47, c(-14, -14.0001, -14.0002), c(-47, -47, -47))
assert(length(dist_vec) == 3L && all(is.finite(dist_vec)), "distância escalar-vetor ainda produz NA")
assert(abs(dist_vec[[2L]] - monitora_esp_dist_m(-14.0001, -47, -14, -47)) < 1e-9, "distância deixou de ser simétrica")

criar_coleta <- function(ua, ano, coleta, ini_lat, ini_lon, fim_lat, fim_lon,
                         inicio_acuracia = 3, fim_acuracia = 3,
                         n_ini = 1L, n_fim = 1L) {
  data.table(
    id_coleta_espacial = paste("UC", "EA", ua, ano, coleta, sep = "||"),
    UC = "UC", EA = "EA", UA = ua, ANO = as.character(ano), COLETA = as.character(coleta),
    n_linhas = 101L, linha_min = 1L, linha_max = 101L,
    coord_inicio_txt = paste(ini_lat, ini_lon, 100, inicio_acuracia),
    coord_fim_txt = paste(fim_lat, fim_lon, 100, fim_acuracia),
    n_coord_inicio_distintas = as.integer(n_ini), n_coord_fim_distintas = as.integer(n_fim),
    inicio_lat = ini_lat, inicio_lon = ini_lon, inicio_alt = 100, inicio_acuracia = inicio_acuracia, inicio_valida = TRUE,
    fim_lat = fim_lat, fim_lon = fim_lon, fim_alt = 100, fim_acuracia = fim_acuracia, fim_valida = TRUE,
    coordenadas_validas = TRUE,
    comprimento_transecto_m = monitora_esp_dist_m(ini_lat, ini_lon, fim_lat, fim_lon),
    azimute_transecto_graus = monitora_esp_bearing_graus(ini_lat, ini_lon, fim_lat, fim_lon)
  )
}

# Quatro campanhas coerentes e uma coordenada inicial evidentemente deslocada.
base <- rbindlist(list(
  criar_coleta("UA-026", 2022, 1, -14.50, -47.50, -14.00045, -47.00000),
  criar_coleta("UA-026", 2023, 2, -14.00000, -47.00000, -14.00045, -47.00000),
  criar_coleta("UA-026", 2024, 3, -14.00003, -47.00001, -14.00046, -47.00001),
  criar_coleta("UA-026", 2025, 4, -14.00005, -47.00002, -14.00044, -47.00002),
  criar_coleta("UA-026", 2026, 5, -14.00002, -47.00003, -14.00045, -47.00003)
), fill = TRUE)
res <- monitora_esp_validar_coletas(base)
v <- res$validacao[order(ANO)]
assert(v[ANO == "2022", pendencia_espacial][[1L]], "outlier espacial não foi mantido como pendência")
assert(v[ANO != "2022", sum(pendencia_espacial)] == 0L, "maioria temporal coerente foi marcada como pendente")
assert(isTRUE(res$consensos$consenso_robusto[[1L]]), "maioria temporal não gerou consenso robusto")

# Duas séries 2 x 2 não podem escolher referência pela ordem física das linhas.
empate <- rbindlist(list(
  criar_coleta("UA-TIE", 2022, 10, -14.0000, -47.0000, -14.00045, -47.0000),
  criar_coleta("UA-TIE", 2023, 11, -14.00002, -47.0000, -14.00047, -47.0000),
  criar_coleta("UA-TIE", 2024, 12, -14.0100, -47.0100, -14.01045, -47.0100),
  criar_coleta("UA-TIE", 2025, 13, -14.01002, -47.0100, -14.01047, -47.0100)
), fill = TRUE)
re <- monitora_esp_validar_coletas(empate)
assert(!isTRUE(re$consensos$consenso_valido[[1L]]) && isTRUE(re$consensos$consenso_ambiguo[[1L]]), "empate 2 x 2 escolheu referência arbitrária")

# Duas campanhas coerentes são úteis, mas devem ser identificadas como evidência limitada.
duas <- rbindlist(list(
  criar_coleta("UA-2", 2024, 20, -15, -48, -15.00045, -48),
  criar_coleta("UA-2", 2025, 21, -15.00001, -48, -15.00046, -48)
), fill = TRUE)
rd <- monitora_esp_validar_coletas(duas)$validacao
assert(all(rd$status_espacial == "coerente_com_referencia_temporal_limitada"), "duas campanhas foram apresentadas como validação plena")
assert(all(!rd$pendencia_espacial & rd$alerta_referencia_limitada), "referência limitada não foi tratada como alerta não impeditivo")

# Coordenadas divergentes dentro da mesma coleta não podem usar silenciosamente o primeiro valor.
intra <- criar_coleta("UA-I", 2025, 30, -16, -49, -16.00045, -49, n_ini = 2L)
ri <- monitora_esp_validar_coletas(intra)$validacao
assert(ri$status_espacial[[1L]] == "coordenadas_conflitantes_na_coleta" && ri$pendencia_espacial[[1L]], "conflito interno de coordenadas não gerou pendência")

# Uma série inteira coincidente com outra UA deve ser relatada mesmo sem pendência interna.
colisao <- rbindlist(list(
  criar_coleta("UA-A", 2023, 40, -17, -50, -17.00045, -50),
  criar_coleta("UA-A", 2024, 41, -17.00001, -50, -17.00046, -50),
  criar_coleta("UA-A", 2025, 42, -17.00002, -50, -17.00047, -50),
  criar_coleta("UA-B", 2023, 43, -17.00001, -50.00001, -17.00046, -50.00001),
  criar_coleta("UA-B", 2024, 44, -17.00002, -50.00001, -17.00047, -50.00001),
  criar_coleta("UA-B", 2025, 45, -17.00003, -50.00001, -17.00048, -50.00001)
), fill = TRUE)
rc <- monitora_esp_validar_coletas(colisao)
assert(nrow(rc$consensos_uas_coincidentes) >= 1L, "consensos coincidentes de UAs distintas não foram relatados")
assert(all(rc$validacao$alerta_ua_consenso_coincidente), "coletas internamente coerentes ocultaram possível troca sistemática de UA")

plano <- monitora_esp_plano_correcoes(rc)
assert(is.data.table(plano) && nrow(plano) >= 1L, "plano operacional espacial não foi materializado")

# A chave de UA deve distinguir homônimos de UCs/EAs diferentes.
chaves_ua <- monitora_esp_choices_ua_contextuais(data.table(
  UC = c("UC-1", "UC-2"), EA = c("EA-1", "EA-1"), UA = c("UA-01", "UA-01")
))
assert(length(chaves_ua) == 2L && uniqueN(unname(chaves_ua)) == 2L, "seletor contextual colapsou UAs homônimas")
origens_ctx <- data.table(
  UC = c("UC-1", "UC-1", "UC-2"), EA = c("EA-1", "EA-1", "EA-1"), UA = c("UA-01", "UA-01", "UA-01"),
  ANO = c("2024", "2025", "2025"), COLETA = c("100", "101", "900")
)
origem_2025 <- monitora_esp_filtrar_chave_ua(origens_ctx, monitora_esp_chave_ua("UC-1", "EA-1", "UA-01"))[ANO == "2025"]
assert(nrow(origem_2025) == 1L && origem_2025$COLETA[[1L]] == "101", "UA+ANO da fonte não resolveu automaticamente a COLETA inequívoca")

# Apenas inversão inequívoca contra consenso robusto entra na sanitização segura.
inv <- rbindlist(list(
  criar_coleta("UA-INV", 2023, 50, -18, -51, -18.00045, -51),
  criar_coleta("UA-INV", 2024, 51, -18.00001, -51, -18.00046, -51),
  criar_coleta("UA-INV", 2025, 52, -18.00002, -51, -18.00047, -51),
  criar_coleta("UA-INV", 2026, 53, -18.00045, -51, -18, -51)
), fill = TRUE)
r_inv <- monitora_esp_validar_coletas(inv)
p_inv <- monitora_esp_plano_correcoes(r_inv)
assert(nrow(p_inv[elegivel_sanitizacao_automatica == TRUE]) == 1L && p_inv[elegivel_sanitizacao_automatica == TRUE, COLETA][[1L]] == "53", "sanitização segura não isolou apenas a inversão determinística")

# Executor atômico preserva escopo e materializa a trilha antes/depois.
ctx_inv <- r_inv$validacao[COLETA == "53"][1L]
raw <- data.table(
  COLETA = rep("53", 2), UC = rep("UC", 2), EA = rep("EA", 2), UA = rep("UA-INV", 2), ANO = rep("2026", 2),
  ponto_inicio_transecto = rep(ctx_inv$coord_inicio_txt, 2),
  ponto_fim_transecto = rep(ctx_inv$coord_fim_txt, 2)
)
op <- monitora_espacial_painel_operacao_dt(
  "inverter_inicio_fim", "53", n_linhas_esperado = 2L,
  confirmar_abrangencia = TRUE, justificativa = "QA",
  contexto = ctx_inv, tipo_origem = "sanitizacao_espacial_deterministica", coleta_fonte = "53"
)
op[, `:=`(UC_fonte = "UC", EA_fonte = "EA", UA_fonte = "UA-INV", ANO_fonte = "2026")]
op <- monitora_esp_correcoes_schema(op)
aud <- monitora_esp_aplicar_uma_operacao(raw, op[1L], monitora_esp_colunas_chave(raw))
assert(aud$status[[1L]] == "aplicada" && nzchar(aud$coord_inicio_antes[[1L]]) && nzchar(aud$coord_inicio_depois[[1L]]), "auditoria espacial atômica não registrou antes/depois")
assert(all(raw$ponto_inicio_transecto == ctx_inv$coord_fim_txt) && all(raw$ponto_fim_transecto == ctx_inv$coord_inicio_txt), "inversão atômica não persistiu no escopo exato")

# Uma edição de ambos os vértices deve validar o par inteiro antes da primeira escrita.
raw_falha <- copy(raw)
antes_falha <- copy(raw_falha)
op_falha <- copy(op[1L])
op_falha[, `:=`(
  tipo_operacao = "editar_ambos", campo_alvo = "ambos",
  coord_inicio_original_esperada = NA_character_, coord_fim_original_esperada = NA_character_,
  coord_inicio_nova = "-18.1 -51.1 100 3", coord_fim_nova = "coordenada_invalida"
)]
aud_falha <- monitora_esp_aplicar_uma_operacao(raw_falha, op_falha, monitora_esp_colunas_chave(raw_falha))
assert(aud_falha$status[[1L]] == "falha" && identical(raw_falha, antes_falha), "falha no segundo vértice deixou alteração espacial parcial")

# Rejeições de preparação e novos relatórios são sempre materializados.
raw_rej <- rbind(raw[1L], data.table(COLETA = "", UC = "UC", EA = "EA", UA = "", ANO = "2026", ponto_inicio_transecto = "-18 -51 100 3", ponto_fim_transecto = "-18.00045 -51 100 3"), fill = TRUE)
rej <- monitora_esp_preparar_rejeicoes(raw_rej)
assert(nrow(rej) == 1L && rej$motivo_rejeicao[[1L]] == "coleta_e_ua_ausentes", "linha rejeitada na preparação espacial não foi relatada")

qa_dir <- tempfile("qa_spatial_products_")
dir.create(file.path(qa_dir, "out"), recursive = TRUE)
dir.create(file.path(qa_dir, "log"), recursive = TRUE)
rc$rejeicoes_preparacao <- data.table(linha_registros_corrig = integer(), motivo_rejeicao = character())
monitora_esp_gravar_produtos(rc, momento = "diagnostico", output_dir = file.path(qa_dir, "out"), log_dir = file.path(qa_dir, "log"), exec_id = "QA")
prod_dir <- file.path(qa_dir, "out", "validacao_espacial", "diagnostico")
esperados <- c("validacao_espacial_ocorrencias_diagnosticas.csv", "plano_correcoes_espaciais.csv", "validacao_espacial_consensos_uas_coincidentes.csv", "validacao_espacial_rejeicoes_preparacao.csv")
assert(all(file.exists(file.path(prod_dir, esperados))), "novos produtos espaciais não foram materializados")

cat("QA_SPATIAL_V280_OK\n")
