#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Uso: test_v295_seca_morta_real.R SCRIPT PNM_CSV PNCV_CSV", call. = FALSE)
}
script <- normalizePath(args[[1L]], mustWork = TRUE)
arquivos <- c(PNM = normalizePath(args[[2L]], mustWork = TRUE),
              PNCV = normalizePath(args[[3L]], mustWork = TRUE))
raiz_saida <- trimws(Sys.getenv("MONITORA_TESTE_SECA_MORTA_SAIDA", unset = ""))
if (nzchar(raiz_saida)) dir.create(raiz_saida, recursive = TRUE, showWarnings = FALSE)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_diag_seca_morta_ocorrencias_revisao",
  "monitora_diag_seca_morta_gravar_relatorio_operacional",
  "monitora_relatorio_exoticas_normalizar_token",
  "monitora_relatorio_exoticas_tem_token",
  "monitora_correcao_normalizar_nome_coluna"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nome <- as.character(x[[2L]])
    if (nome %in% alvos) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

env$MONITORA_COL_ROW_ID <- ".monitora_row_id"
primeira <- function(dt, exatos, padrao) {
  hit <- exatos[exatos %in% names(dt)]
  if (length(hit)) return(hit[[1L]])
  norm <- env$monitora_correcao_normalizar_nome_coluna(names(dt))
  idx <- which(grepl(padrao, norm, perl = TRUE))
  if (length(idx)) names(dt)[idx[[1L]]] else NA_character_
}
env$monitora_correcao_colunas_chave <- function(dt) list(
  coleta = primeira(dt, c("COLETA", "coleta"), "^coleta$"),
  uc = primeira(dt, c("UC", "uc"), "^uc$|unidade.*conserv"),
  ea = primeira(dt, c("EA", "ea"), "^ea$|estacao_amostral|estrato.*amostral"),
  ua = primeira(dt, c("UA", "ua"), "^ua$|unidade.*amostral"),
  ano = primeira(dt, c("ANO", "ano"), "^ano$"),
  tipo_forma_vida = primeira(
    dt,
    c("amostragem/registro/tipo_forma_vida", "tipo_forma_vida",
      "**Encostam** na vareta: (amostragem/registro)"),
    "tipo.*forma.*vida|encostam.*vareta"
  )
)
env$monitora_correcao_colunas_forma_vida_categoria <- function(...) character()
env$monitora_correcao_xlsform_meta_atual <- function(...) list(opcoes = data.table())
env$monitora_diag_rel_write_dt <- function(x, arquivo) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, bom = TRUE)
  invisible(arquivo)
}

resumo <- rbindlist(lapply(names(arquivos), function(unidade) {
  inicio <- proc.time()[["elapsed"]]
  dados <- fread(
    arquivos[[unidade]], encoding = "UTF-8", na.strings = NULL,
    colClasses = "character", showProgress = FALSE
  )
  leitura_s <- proc.time()[["elapsed"]] - inicio
  inicio_diag <- proc.time()[["elapsed"]]
  ocorrencias <- env$monitora_diag_seca_morta_ocorrencias_revisao(dados)
  diagnostico_s <- proc.time()[["elapsed"]] - inicio_diag
  pasta <- if (nzchar(raiz_saida)) {
    file.path(raiz_saida, tolower(unidade))
  } else {
    tempfile(paste0("v295_seca_morta_", tolower(unidade), "_"))
  }
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  inicio_rel <- proc.time()[["elapsed"]]
  rel <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
    dados, ocorrencias, pasta, "teste_real"
  )
  relatorio_s <- proc.time()[["elapsed"]] - inicio_rel
  coluna_coleta <- env$monitora_correcao_colunas_chave(dados)$coleta
  stopifnot(
    nrow(rel$operacional) == uniqueN(dados[[coluna_coleta]][ocorrencias$linha_indice]),
    sum(rel$operacional$n_eventos_linha_forma) == nrow(ocorrencias),
    all(rel$operacional$percentual_pontos_seca_morta >= 0 &
          rel$operacional$percentual_pontos_seca_morta <= 100),
    all(c(
      "classificacao_triagem", "criterio_principal", "criterios_atendidos",
      "percentual_ua_ano_anterior", "variacao_pp_ua_desde_medicao_anterior"
    ) %in% names(rel$operacional)),
    all(nzchar(as.character(rel$operacional$COLETA))),
    all(nzchar(as.character(rel$por_ano$coletas_amostradas))),
    all(
      rel$por_ano$n_coletas_com_seca_morta == 0L |
        nzchar(as.character(rel$por_ano$coletas_com_ocorrencia))
    ),
    all(nzchar(as.character(rel$por_ua_ano$coletas_amostradas))),
    all(
      rel$por_ua_ano$n_coletas_com_seca_morta == 0L |
        nzchar(as.character(rel$por_ua_ano$coletas_com_ocorrencia))
    ),
    all(
      rel$por_forma_vida$n_eventos_linha_forma == 0L |
        nzchar(as.character(rel$por_forma_vida$coletas_com_ocorrencia))
    ),
    all(nzchar(as.character(rel$trajetorias$coletas_amostradas_ano_atual))),
    all(
      rel$trajetorias$n_pontos_seca_morta_atual == 0L |
        nzchar(as.character(rel$trajetorias$coletas_com_ocorrencia_ano_atual))
    ),
    all(
      is.na(rel$trajetorias$ano_anterior) |
        nzchar(as.character(rel$trajetorias$coletas_amostradas_ano_anterior))
    ),
    all(
      is.na(rel$trajetorias$n_pontos_seca_morta_anterior) |
        rel$trajetorias$n_pontos_seca_morta_anterior == 0L |
        nzchar(as.character(rel$trajetorias$coletas_com_ocorrencia_ano_anterior))
    ),
    all(file.info(list.files(pasta, full.names = TRUE))$size > 0L)
  )
  data.table(
    unidade, n_linhas = nrow(dados),
    n_coletas_seca_morta = uniqueN(rel$operacional$COLETA),
    n_pontos_seca_morta = uniqueN(ocorrencias$linha_indice),
    n_eventos_linha_forma = nrow(ocorrencias),
    n_suspeitas_falso_positivo = rel$operacional[
      classificacao_triagem == "suspeita_de_falso_positivo", .N
    ],
    n_falta_contexto = rel$operacional[
      classificacao_triagem == "falta_de_contexto_para_interpretacao", .N
    ],
    leitura_s = round(leitura_s, 3L),
    diagnostico_s = round(diagnostico_s, 3L),
    relatorio_s = round(relatorio_s, 3L)
  )
}))

stopifnot(
  resumo[unidade == "PNCV", n_coletas_seca_morta] == 404L,
  resumo[unidade == "PNCV", n_eventos_linha_forma] == 2408L
)
print(resumo)
cat("TEST_V295_SECA_MORTA_REAL_OK\n")
