#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
raiz <- normalizePath(if (length(args)) args[[1L]] else ".", mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
localizar_um <- function(padrao) {
  x <- list.files(raiz, pattern = padrao, recursive = TRUE, full.names = TRUE)
  assert(length(x) == 1L, paste0("Produto não unívoco: ", padrao, " (", length(x), ")"))
  x[[1L]]
}

stat_path <- localizar_um("registros_corrig_stat[.]csv$")
mud_path <- localizar_um("estatisticas_mudanca_ano_a_ano[.]csv$")
idx_path <- localizar_um("indice_evidencias_relatorio[.]csv$")
cal_path <- localizar_um("epoca_calendario_resumo_relatorio[.]csv$")
map_path <- localizar_um("auditoria_mapa_satelite[.]csv$")
md_path <- localizar_um("relatorio_analitico_detalhado_.*[.]md$")

stat <- fread(stat_path, encoding = "UTF-8")
mud <- fread(mud_path, encoding = "UTF-8")
idx <- fread(idx_path, encoding = "UTF-8")
cal <- fread(cal_path, encoding = "UTF-8")

# Gate 1: para cobertura, ausência de uma família é zero e todas as UAs comuns
# do par de anos entram em cada categoria existente dessa família.
universo <- unique(stat[, .(UC = as.character(UC), UA = as.character(UA),
  ANO = as.integer(ANO), form_veg = as.character(form_veg))])
alvos_cob <- mud[grupo_grafico %in% c("formas_vida_exoticas", "formas_vida_secas_mortas") &
  tipo_metrica == "cobertura"]
pares_alvo <- unique(alvos_cob[, .(form_veg, ano_1, ano_2)])
pares_esperados <- pares_alvo[, {
  a <- merge(universo[form_veg == .BY$form_veg & ANO == .BY$ano_1, .(UC, UA)],
    universo[form_veg == .BY$form_veg & ANO == .BY$ano_2, .(UC, UA)], by = c("UC", "UA"))
  .(n_esperado = nrow(a))
}, by = .(form_veg, ano_1, ano_2)]
alvos_cob <- merge(alvos_cob, pares_esperados, by = c("form_veg", "ano_1", "ano_2"), all.x = TRUE)
assert(nrow(alvos_cob) > 0L && all(alvos_cob$n_UA_pareadas == alvos_cob$n_esperado),
  "Cobertura de exóticas/secas-mortas ainda perdeu UAs sem ocorrência da família.")

# Gate 2: composição só inclui UAs cujo total da família é positivo em ambos os anos.
verificar_relativo <- function(grupo, prefixo) {
  cols <- grep(paste0("^", prefixo), names(stat), value = TRUE)
  assert(length(cols) > 0L, paste0("Família sem colunas: ", grupo))
  z <- copy(stat)
  z[, total_familia := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
  z <- unique(z[total_familia > 0, .(UC = as.character(UC), UA = as.character(UA),
    ANO = as.integer(ANO), form_veg = as.character(form_veg))])
  obs <- mud[grupo_grafico == grupo & tipo_metrica == "proporcao_relativa"]
  pares_alvo <- unique(obs[, .(form_veg, ano_1, ano_2)])
  pares <- pares_alvo[, {
    a <- merge(z[form_veg == .BY$form_veg & ANO == .BY$ano_1, .(UC, UA)],
      z[form_veg == .BY$form_veg & ANO == .BY$ano_2, .(UC, UA)], by = c("UC", "UA"))
    .(n_esperado = nrow(a))
  }, by = .(form_veg, ano_1, ano_2)]
  obs <- merge(obs, pares, by = c("form_veg", "ano_1", "ano_2"), all.x = TRUE)
  assert(nrow(obs) > 0L && all(!is.na(obs$n_esperado)) && all(obs$n_UA_pareadas == obs$n_esperado),
    paste0("Denominador relativo ou pareamento incorreto em ", grupo, "."))
}
verificar_relativo("formas_vida_exoticas", "exot_")
verificar_relativo("formas_vida_secas_mortas", "seca_morta_")

# Gate 3: a evidência principal não é mais substituída pelo painel fixo de 2020.
assert(!any(grepl("painel fixo iniciado", idx$populacao_analitica, fixed = TRUE), na.rm = TRUE),
  "A evidência principal ainda usa painel fixo do primeiro ano.")
assert(any(idx$n_UA_pareadas >= 20L, na.rm = TRUE),
  "A ampliação efetiva do painel não chegou à evidência temporal principal.")
mat <- mud[grupo_grafico == "material_botanico" & tipo_metrica == "cobertura" & ano_1 == 2025L & ano_2 == 2026L]
assert(nrow(mat) > 0L && setequal(unique(mat$n_UA_pareadas), c(20L, 28L)),
  "A população própria de material botânico 2025–2026 sofreu regressão.")

# Gate 4: calendário compacto é fiel aos meses observados e aos recortes exatos.
assert(identical(cal$Ano, 2020:2026), "Tabela de calendário não contém exatamente 2020–2026.")
esperado <- c("março (8)", "agosto (3); novembro (8)", "outubro (42); novembro (5)",
  "fevereiro (24); março (24)", "fevereiro (48)", "fevereiro (19); março (29)", "janeiro (48)")
assert(identical(cal[["Períodos observados (UAs)"]], esperado), "Meses/contagens da tabela de calendário divergiram.")
assert(any(grepl("referência 2023–2025", cal[["Papel nos cenários de referência"]], fixed = TRUE)) &&
       any(grepl("referência 2024–2025", cal[["Papel nos cenários de referência"]], fixed = TRUE)),
  "Os recortes de referência não foram descritos por seus anos exatos.")

# Gate 5: CSV completo preserva zeros; tabela impressa não os exibe.
estado_paths <- list.files(dirname(idx_path), pattern = "^estado_atual_formas_.*[.]csv$", full.names = TRUE)
zeros <- rbindlist(lapply(estado_paths, function(p) fread(p, encoding = "UTF-8")), fill = TRUE)
zeros <- zeros[suppressWarnings(as.numeric(`Nº de registros`)) == 0]
assert(nrow(zeros) > 0L, "CSV de auditoria deixou de preservar categorias com zero.")
linhas_md <- readLines(md_path, warn = FALSE, encoding = "UTF-8")
col_ind <- intersect(c("Forma de vida nativa", "Forma de vida exótica", "Forma de vida seca ou morta"), names(zeros))
rotulos_zero <- unique(unlist(zeros[, ..col_ind], use.names = FALSE))
rotulos_zero <- rotulos_zero[!is.na(rotulos_zero) & nzchar(rotulos_zero)]
linhas_tabela <- linhas_md[grepl("^\\|", linhas_md)]
linha_zero_impressa <- vapply(linhas_tabela, function(linha) {
  cel <- trimws(strsplit(linha, "|", fixed = TRUE)[[1L]])
  cel <- cel[nzchar(cel)]
  pos <- which(cel %in% rotulos_zero)
  length(pos) && any(vapply(pos, function(j) length(cel) >= j + 2L && identical(cel[[j + 2L]], "0"), logical(1L)))
}, logical(1L))
assert(!any(linha_zero_impressa),
  "O relatório detalhado ainda imprime forma de vida com zero registros.")

# Gate 6: mapa Sentinel já homologado permanece presente e aprovado.
mapa <- fread(map_path, encoding = "UTF-8")
assert(nrow(mapa) == 1L && isTRUE(mapa$solicitado[[1L]]) && isTRUE(mapa$gerado[[1L]]) &&
       isTRUE(mapa$qualidade_resolucao_aprovada[[1L]]) && isTRUE(mapa$qualidade_radiometrica_aprovada[[1L]]),
  "Mapa Sentinel não foi gerado/aprovado; homologação deve parar.")
assert(file.exists(file.path(dirname(idx_path), "figuras", "mapa_continuidade_uas_satelite.png")),
  "PNG do mapa Sentinel ausente.")

cat("TEST_V2926_PNB_RESULTADO_ANALITICOS_OK\n")
