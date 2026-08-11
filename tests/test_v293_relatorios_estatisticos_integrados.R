#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.3.R"
if (!file.exists(script)) stop("Script candidato ausente: ", script, call. = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

arvore <- parse(file = script, keep.source = FALSE)
funcoes <- new.env(parent = globalenv())
coletar <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]]) &&
      is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")) {
    nome <- as.character(expr[[2L]])
    if (startsWith(nome, "monitora_relatorios_analiticos_") ||
        nome %in% c(
          "monitora_relatorio_classe_portugues",
          "monitora_relatorio_rotulo_metrica",
          "monitora_relatorio_rotulo_grupo",
          "monitora_relatorio_rotulo_formacao"
        )) eval(expr, envir = funcoes)
    return(invisible(NULL))
  }
  for (ii in seq_along(expr)[-1L]) coletar(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
list2env(as.list.environment(funcoes, all.names = TRUE), envir = .GlobalEnv)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

stat <- data.table(
  UC = "UC de teste",
  UA = rep(c("1", "2"), each = 2L),
  ANO = rep(c(2025L, 2026L), 2L),
  form_veg = "savanica",
  sum_herbacea = c(9, 8, 1, 2),
  sum_lenhosa = c(1, 2, 1, 2),
  sum_presence_herb = c(90, 80, 10, 20),
  sum_presence_lenh = c(10, 20, 10, 20),
  sum_nativa = c(10, 10, 2, 4),
  sum_exotica = c(0, 0, 0, 0),
  sum_seca_morta = c(0, 0, 0, 0),
  material_botanico = c(0, 0, 0, 0),
  solo_nu = c(0, 0, 0, 0),
  sum_presence_nativa = c(100, 100, 20, 40),
  sum_presence_exotica = 0,
  sum_presence_seca_morta = 0
)
series <- monitora_relatorios_analiticos_series_anuais_ua(stat)
herb_2025 <- series[
  grupo_grafico == "herbaceas_lenhosas" &
    tipo_metrica == "proporcao_relativa" &
    categoria == "sum_herbacea" & ANO == 2025L
]
assert(nrow(herb_2025) == 1L, "Série anual herbácea não foi criada.")
assert(abs(herb_2025$media_percent - 70) < 1e-9,
       "A proporção anual não foi calculada como média entre UAs.")
assert(abs(herb_2025$media_percent - (10 / 12 * 100)) > 10,
       "A série anual regrediu para a proporção agregada de contatos.")
assert(herb_2025$n_UA == 2L && herb_2025$unidade_analitica == "UA",
       "Esforço ou unidade analítica anual incorreto.")

periodo <- rbindlist(list(
  data.table(
    grupo_grafico = "herbaceas_lenhosas", tipo_metrica = "proporcao_relativa",
    form_veg = "savanica", categoria = c("sum_herbacea", "sum_lenhosa"),
    categoria_label = c("Herbácea", "Lenhosa"), ano_1 = 2025L, ano_2 = 2026L,
    n_UA_pareadas = 8L, diferenca_pp = c(4, -4),
    ci95_lower_pp = c(2, -6), ci95_upper_pp = c(6, -2),
    p_ajustado_fdr = 0.02,
    classe_mudanca = c("aumento", "reducao")
  ),
  data.table(
    grupo_grafico = "herbaceas_lenhosas", tipo_metrica = "cobertura",
    form_veg = "savanica", categoria = c("sum_presence_herb", "sum_presence_lenh"),
    categoria_label = c("Herbácea", "Lenhosa"), ano_1 = 2025L, ano_2 = 2026L,
    n_UA_pareadas = 8L, diferenca_pp = c(1, 0),
    ci95_lower_pp = c(-1, -1), ci95_upper_pp = c(3, 1),
    p_ajustado_fdr = c(0.4, 1),
    classe_mudanca = c("estabilidade_equivalente", "inconclusivo")
  )
))
linha_base <- copy(periodo)[, `:=`(
  anos_linha_base = "2024;2025",
  p_ajustado_fdr_linha_base = p_ajustado_fdr
)]
composicao_periodo <- unique(periodo[, .(
  grupo_grafico, tipo_metrica, form_veg, ano_1, ano_2,
  n_UA_pareadas,
  distancia_centroide_hellinger = 0.08,
  ci95_lower_dist_hellinger = 0.04,
  ci95_upper_dist_hellinger = 0.12,
  p_ajustado_fdr_composicao = 0.02,
  classe_mudanca_composicao = "mudanca_composicao"
)])
composicao_linha_base <- unique(composicao_periodo[, .(
  grupo_grafico, tipo_metrica, form_veg, ano_2,
  anos_linha_base = "2024;2025", n_UA_pareadas,
  distancia_centroide_hellinger = 0.07,
  ci95_lower_dist_hellinger = 0.03,
  ci95_upper_dist_hellinger = 0.11,
  p_ajustado_fdr_composicao_linha_base = 0.03,
  classe_mudanca_composicao_linha_base = "mudanca_composicao"
)])
config <- data.table(
  parametro = c("alpha", "efeito_minimo_pp", "margem_equivalencia_pp"),
  valor = c(0.05, 2, 5)
)

dir_fig <- tempfile("monitora_stats_fig_")
dir.create(dir_fig, recursive = TRUE)
indice <- monitora_relatorios_analiticos_graficos_editoriais(
  data.table(), data.table(), data.table(), data.table(), dir_fig,
  series_anuais_ua = series,
  mudanca_periodo = periodo,
  mudanca_linha_base = linha_base,
  composicao_periodo = composicao_periodo,
  composicao_linha_base = composicao_linha_base,
  config_stat = config
)
paineis <- indice[grepl("^inferencias_", id) & disponivel == TRUE]
assert(nrow(paineis) == 2L, "Painéis inferenciais sintéticos esperados não foram gerados.")
assert(all(file.exists(file.path(dirname(dir_fig), paineis$arquivo_relatorio))) ||
         all(file.exists(file.path(dir_fig, basename(paineis$arquivo_relatorio)))),
       "PNG inferencial sintético ausente.")
auditoria <- as.data.table(attr(indice, "auditoria_estatistica"))
assert(nrow(auditoria) == nrow(periodo) + nrow(composicao_periodo),
       "Auditoria não cobre exatamente todas as células inferenciais.")
assert(setequal(unique(auditoria$classe_periodo),
                c("aumento", "reducao", "estabilidade_equivalente", "inconclusivo", "mudanca_composicao")),
       "Classes de mudança, estabilidade e composição não foram preservadas.")
assert(all(auditoria$associacao_nao_causal), "Cautela causal ausente na auditoria.")
simbolos <- as.data.table(attr(indice, "auditoria_simbolos_series"))
assert(nrow(simbolos) >= 8L && !anyDuplicated(simbolos[, .(plot_id, form_veg, categoria, ANO)]),
       "Auditoria dos símbolos não cobre univocamente as médias anuais do teste.")
assert(all(simbolos[ANO == 2025L, simbolo_estatistico] == ""),
       "O primeiro ano de cada indicador deveria permanecer sem símbolo.")
simbolos_prop_2026 <- simbolos[
  plot_id == "herbaceas_lenhosas_proporcao" & tipo_metrica == "proporcao_relativa" & ANO == 2026L
][order(categoria), simbolo_estatistico]
assert(identical(simbolos_prop_2026, c("↑", "↓")), paste0(
  "Aumento e redução não foram associados às médias corretas: ",
  paste(simbolos_prop_2026, collapse = "|")
))
simbolos_cobertura_2026 <- simbolos[
  grupo_grafico == "herbaceas_lenhosas" & tipo_metrica == "cobertura" & ANO == 2026L,
  simbolo_estatistico
]
assert(setequal(simbolos_cobertura_2026, c("≈", "·")),
       "Estabilidade e resultado inconclusivo não foram representados por ≈ e ·.")
assert(!any(simbolos$simbolo_estatistico == "?"),
       "O símbolo obsoleto ? permaneceu associado às médias dos relatórios.")
assert(all(simbolos[
  grupo_grafico == "categorias_gerais" & tipo_metrica == "cobertura" & ANO == 2026L,
  simbolo_estatistico
] == "—"),
       "Médias sem comparação pareada deveriam ser explicitamente classificadas por —.")

cat("TEST_V293_RELATORIOS_ESTATISTICOS_INTEGRADOS_OK\n")
