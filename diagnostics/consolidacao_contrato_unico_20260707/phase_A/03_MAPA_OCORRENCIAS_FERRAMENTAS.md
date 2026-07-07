# 03_MAPA_OCORRENCIAS_FERRAMENTAS

## ocorrencias_idx
- L19035: `  monitora_painel_ocorrencias_idx_leve <- function(triagem, coletas_por_tipo) {`
- L19112: `  rv <- shiny::reactiveValues(correcoes = monitora_correcao_template(), preview = data.table::data.table(), painel_finalizado = FALSE, ponto_alvo = "", moviment`
- L19113: `  ocorrencias_idx_base_painel <- monitora_painel_ocorrencias_idx_leve(triagem_painel_unificada, coletas_triagem_por_tipo)`
- L19114: `  rv$ocorrencias_idx <- data.table::copy(ocorrencias_idx_base_painel)`
- L19115: `  ocorrencias_resumo_base_painel <- monitora_painel_ocorrencias_resumo_leve(ocorrencias_idx_base_painel)`
- L20009: `      try(monitora_painel_atualizar_ocorrencias_idx_fila(), silent = TRUE)`
- L20051: `    monitora_painel_atualizar_ocorrencias_idx_fila <- function() {`
- L20052: `      idx <- data.table::copy(ocorrencias_idx_base_painel)`
- L20066: `      rv$ocorrencias_idx <- idx[]`
- L22140: `      try(monitora_painel_atualizar_ocorrencias_idx_fila(), silent = TRUE)`

## ocorrencias_resumo
- L14987: `    monitora_oraculo_fwrite(resumo, file.path(out_dir, paste0("oraculo_ocorrencias_resumo_", fase_atual, "_vs_", fase_oraculo, ".csv")))`
- L15019: `  arq_res <- file.path(out_dir, paste0("oraculo_ocorrencias_resumo_", fase_atual, "_vs_", fase_oraculo, ".csv"))`
- L19100: `  monitora_painel_ocorrencias_resumo_leve <- function(idx) {`
- L19112: `  rv <- shiny::reactiveValues(correcoes = monitora_correcao_template(), preview = data.table::data.table(), painel_finalizado = FALSE, ponto_alvo = "", moviment`
- L19115: `  ocorrencias_resumo_base_painel <- monitora_painel_ocorrencias_resumo_leve(ocorrencias_idx_base_painel)`
- L19116: `  rv$ocorrencias_resumo <- data.table::copy(ocorrencias_resumo_base_painel)`
- L20067: `      rv$ocorrencias_resumo <- monitora_painel_ocorrencias_resumo_leve(idx)`
- L21229: `      abertas <- tryCatch(data.table::as.data.table(rv$ocorrencias_resumo), error = function(e) data.table::data.table())`

## ferramenta_resolutiva
