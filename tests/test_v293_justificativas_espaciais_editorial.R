#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.3.R",
  mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
nomes <- c(
  "monitora_correcao_hash_texto",
  "monitora_esp_ocorrencias_diagnosticas",
  "monitora_pendencias_ocorrencia_id",
  "monitora_pendencias_espaciais_indice",
  "monitora_relatorios_analiticos_coluna_contextual_mesclavel",
  "monitora_relatorios_analiticos_html_mesclar_contexto"
)
coletar <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]]) &&
      is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")) {
    if (as.character(expr[[2L]]) %in% nomes) eval(expr, envir = env)
    return(invisible(NULL))
  }
  for (ii in seq_along(expr)[-1L]) coletar(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
list2env(as.list.environment(env, all.names = TRUE), envir = .GlobalEnv)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

validacao <- data.table(
  id_coleta_espacial = c("id_A", "id_B"),
  UC = "UC teste", EA = "EA1", UA = c("UA1", "UA2"),
  ANO = c("2025", "2026"), COLETA = c("C1", "C2"),
  status_espacial = c("pendencia_inicio_divergente", "validada_espacialmente"),
  pendencia_espacial = c(TRUE, FALSE),
  alerta_raio_rigoroso = FALSE,
  alerta_acuracia_gps = c(FALSE, TRUE),
  alerta_comprimento_transecto = FALSE,
  alerta_referencia_limitada = FALSE,
  alerta_proximidade_outra_ua = FALSE,
  alerta_serie_temporal_secundaria = FALSE,
  alerta_ua_consenso_coincidente = FALSE,
  sugestao_operacional = c("revisar coordenada inicial", "revisar acurácia informada")
)

idx <- monitora_pendencias_espaciais_indice(validacao)
assert(nrow(idx) == 2L, "Pendência e alerta espaciais não foram ambos indexados.")
assert(setequal(idx$fonte_indice, "validacao_espacial"), "Fonte espacial não foi preservada.")
assert(any(grepl("^pendencia_espacial__", idx$tipo_ocorrencia)), "Pendência espacial sem tipo próprio.")
assert(any(grepl("^alerta_espacial__", idx$tipo_ocorrencia)), "Alerta espacial sem tipo próprio.")
assert(all(grepl("^ESP::", idx$monitora_row_id)), "Alvo físico espacial não é estável por COLETA.")

idx[, ocorrencia_id := monitora_pendencias_ocorrencia_id(.SD)]
idx_reordenado <- monitora_pendencias_espaciais_indice(validacao[2:1])
idx_reordenado[, ocorrencia_id := monitora_pendencias_ocorrencia_id(.SD)]
assert(setequal(idx$ocorrencia_id, idx_reordenado$ocorrencia_id),
       "O identificador espacial mudou apenas por reordenação das linhas.")
assert(!anyDuplicated(idx$ocorrencia_id), "Pendência e alerta espaciais colapsaram no mesmo identificador.")

resolvida <- copy(validacao)
resolvida[, `:=`(
  status_espacial = "validada_espacialmente",
  pendencia_espacial = FALSE,
  alerta_acuracia_gps = FALSE
)]
assert(nrow(monitora_pendencias_espaciais_indice(resolvida)) == 0L,
       "Ocorrência espacial resolvida permaneceu no índice.")

html <- tempfile(fileext = ".html")
writeLines(c(
  "<!doctype html><html><body><table>",
  "<thead><tr><th>Ano</th><th>Formação</th><th>Métrica</th></tr></thead>",
  "<tbody>",
  "<tr><td>2025</td><td>Savânica</td><td>Cobertura</td></tr>",
  "<tr><td>2025</td><td>Savânica</td><td>Cobertura</td></tr>",
  "</tbody></table></body></html>"
), html, useBytes = TRUE)
monitora_relatorios_analiticos_html_mesclar_contexto(html)
doc <- xml2::read_html(html)
assert(identical(xml2::xml_attr(xml2::xml_find_first(doc, ".//tbody/tr[1]/td[1]"), "rowspan"), "2"),
       "Ano repetido não foi mesclado na apresentação HTML.")
assert(length(xml2::xml_find_all(doc, ".//tbody/tr/td[normalize-space(.)='Savânica']")) == 2L,
       "Formação vegetacional foi mesclada, contrariando a exceção editorial.")
assert(!monitora_relatorios_analiticos_coluna_contextual_mesclavel("Formação"),
       "Formação vegetacional foi classificada como coluna mesclável.")

texto_script <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (termo in c(
  "preview_validacao_espacial",
  "monitora_espacial_preview_pos_operacoes",
  "validacao_espacial = validacao_esp_atual",
  "auditoria_simbolos_medias_anuais_relatorio.csv",
  "plot.title.position = \"plot\"",
  "text-align:justify"
)) assert(grepl(termo, texto_script, fixed = TRUE), paste("Implementação estrutural ausente:", termo))

cat("TEST_V293_JUSTIFICATIVAS_ESPACIAIS_EDITORIAL_OK\n")
