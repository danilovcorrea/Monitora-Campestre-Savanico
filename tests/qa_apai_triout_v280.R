args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Uso: Rscript tests/qa_apai_triout_v280.R SCRIPT ZIP_APAI", call. = FALSE)
script_path <- normalizePath(args[[1L]], mustWork = TRUE)
zip_path <- normalizePath(args[[2L]], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

exprs <- parse(script_path, keep.source = FALSE, encoding = "UTF-8")
for (expr in exprs) {
  if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
  rhs <- expr[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
}
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)

lista <- utils::unzip(zip_path, list = TRUE)
alvo <- lista$Name[grepl("/input/registros_corrig\\.csv$", lista$Name)]
assert(length(alvo) == 1L, "fixture APAI não contém um único input/registros_corrig.csv")
exdir <- tempfile("qa_apai_")
dir.create(exdir, recursive = TRUE)
utils::unzip(zip_path, files = alvo, exdir = exdir, junkpaths = TRUE)
dt <- data.table::fread(file.path(exdir, "registros_corrig.csv"), encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)

ch <- monitora_correcao_colunas_chave(dt)
assert(!is.na(ch$coleta) && !is.na(ch$ponto_amostral), "chaves COLETA/ponto não resolvidas no APAI")
linhas <- which(as.character(dt[[ch$coleta]]) == "17626" & as.character(dt[[ch$ponto_amostral]]) %in% c("11", "23"))
assert(length(linhas) == 2L, "as duas ocorrências APAI 17626/11 e 17626/23 não foram localizadas")

cols_info <- monitora_correcao_colunas_limpeza_outras_formas(dt, NULL)
linhas_residuo <- monitora_correcao_linhas_residuo_outras_formas(dt, seq_len(nrow(dt)), cols_info)
assert(length(linhas_residuo) == 3L, "o universo real APAI não reproduziu as três linhas residuais registradas no console")
col_exotica <- cols_info[classe == "lista_principal_forma_vida" & categoria == "exotica", coluna][1L]
assert(!is.na(col_exotica) && all(monitora_correcao_token_presente_vec(dt[[col_exotica]][linhas], "arbusto_abaixo")), "forma válida arbusto_abaixo ausente antes da TRIOUT")
cols_especie <- names(dt)[grepl("outra.*especie.*arbusto.*exotic.*inferior", monitora_correcao_normalizar_nome_coluna(names(dt)), perl = TRUE)]
assert(length(cols_especie) == 1L && all(as.character(dt[[cols_especie]][linhas]) == "MM"), "descritor legítimo MM não localizado antes da TRIOUT")

res <- monitora_correcao_aplicar_limpeza_outras_formas_atomica(dt, linhas_residuo, id_correcao = "TRIOUT_APAI_QA")
assert(!isTRUE(res$falha), "backend TRIOUT falhou nas ocorrências reais APAI")
assert(!length(monitora_correcao_linhas_residuo_outras_formas(dt, seq_len(nrow(dt)), monitora_correcao_colunas_limpeza_outras_formas(dt, NULL))), "TRIOUT deixou resíduo no universo real APAI")
assert(all(as.character(dt[[col_exotica]][linhas]) == "arbusto_abaixo"), "TRIOUT real não removeu somente o token histórico outra")
assert(all(as.character(dt[[cols_especie]][linhas]) == "MM"), "TRIOUT real apagou o descritor legítimo MM")
assert(!any(monitora_correcao_token_presente_vec(dt[[col_exotica]][linhas], "outra")), "token histórico outra persistiu após TRIOUT real")

cat("QA_APAI_TRIOUT_V280_OK residuos=3 preservacao_especifica=coleta_17626_pontos_11_23\n")
