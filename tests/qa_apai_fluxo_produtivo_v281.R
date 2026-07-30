args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Uso: Rscript tests/qa_apai_fluxo_produtivo_v281.R SCRIPT ZIP_ROD02 ZIP_ROD03",
    call. = FALSE
  )
}

script_path <- normalizePath(args[[1L]], mustWork = TRUE)
zip_paths <- normalizePath(args[2:3], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

# Carrega todas as definições de função, inclusive módulos tardios do contrato
# e da publicação, sem executar o pipeline principal. Ao encontrar uma função,
# não percorre seu corpo; portanto helpers locais continuam locais.
carregar_funcoes_script <- function(path) {
  carregar_expr <- function(expr) {
    if (!is.call(expr)) return(invisible(NULL))
    if (
      identical(expr[[1L]], as.name("<-")) &&
        length(expr) >= 3L &&
        is.symbol(expr[[2L]]) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      eval(expr, envir = .GlobalEnv)
      return(invisible(NULL))
    }
    for (ii in seq_along(expr)) try(carregar_expr(expr[[ii]]), silent = TRUE)
    invisible(NULL)
  }
  for (expr in parse(path, keep.source = FALSE, encoding = "UTF-8")) carregar_expr(expr)
  invisible(TRUE)
}
carregar_funcoes_script(script_path)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- unique(c(
  ".id", "id", "ID", "uuid", "UUID", "uuid_registro", "UUID_REGISTRO",
  "coleta_uuid", "COLETA_UUID", "COLETA", "PROTOCOLO", "arquivo_origem",
  "linha_indice", "linha_origem", "linha_origem_registros_corrig",
  "ordem_linha_original", "arquivo_fonte", "source_file",
  "MONITORA_HABITOS_CANONICOS_PRESERVADOS",
  "ANO", "DATA_MONITORA_PARSEADA", "num_placa_formatado",
  "DATA DO REGISTRO", "DATA DO RECEBIMENTO", "ULTIMA EDICAO",
  "data_do_registro", "data_do_recebimento", "ultima_edicao"
))
MONITORA_OPCAO_CHECKPOINTS_GRANULARES_CORRECOES <- "N"
MONITORA_OPCAO_PULAR_RECALCULO_DATA_HORA_SEM_ALTERACAO <- "S"
MONITORA_REAPLICAR_CORRECOES_ANTERIORES <- FALSE
MONITORA_REPLAY_DIAGNOSTICO_NAO_ABORTAR <- FALSE
MONITORA_REPLAY_SEMANTICO_EM_EXECUCAO <- FALSE
MONITORA_CORRECOES_REAPLICADAS_PRE_PAINEL <- FALSE
MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES <- FALSE
MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
MONITORA_REGISTROS_CORRIG_CONTRATO_VALIDADO_XLSFORM21 <- FALSE
MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
MONITORA_FALHAR_PRODUTOS_FINAIS_AUSENTES <- FALSE

.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())

monitora_perf_registrar_checkpoint <- function(...) invisible(NULL)
monitora_log_registrar_evento <- function(...) invisible(NULL)
monitora_correcao_console_msg <- function(...) invisible(NULL)
monitora_cadeia_dados_relatorio_gerar <- function(...) invisible(NULL)
monitora_fwrite <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!length(names(x))) {
    file.create(path)
    return(invisible(path))
  }
  data.table::fwrite(x, path, ...)
  invisible(path)
}

categorias_contrato <- monitora_contrato_categorias_movimento(forcar = TRUE)
assert(
  setequal(
    categorias_contrato$categoria,
    c("nativa", "exotica", "seca_morta", "outra_forma_vida")
  ),
  "contrato único embutido não resolveu as quatro categorias de produção"
)

resultados <- vector("list", length(zip_paths))
for (zz in seq_along(zip_paths)) {
  zip_path <- zip_paths[[zz]]
  lista_zip <- utils::unzip(zip_path, list = TRUE)
  entrada_zip <- lista_zip$Name[grepl("/input/registros_corrig\\.csv$", lista_zip$Name)]
  saida_publica_zip <- lista_zip$Name[
    grepl("/output/01_produtos_dados/registros_corrig\\.csv$", lista_zip$Name)
  ]
  fila_zip <- lista_zip$Name[
    grepl(
      "/output/02_painel_correcoes/operacoes_sessao/cache_sessao/correcoes_campos_.*\\.csv$",
      lista_zip$Name
    )
  ]
  assert(length(entrada_zip) == 1L, paste0(basename(zip_path), ": input não único"))
  assert(length(saida_publica_zip) == 1L, paste0(basename(zip_path), ": output público não único"))
  assert(length(fila_zip) == 1L, paste0(basename(zip_path), ": fila operacional real não única"))

  run_dir <- tempfile(paste0("qa_fluxo_v281_rod0", zz + 1L, "_"))
  dir.create(run_dir, recursive = TRUE)
  utils::unzip(
    zip_path,
    files = c(entrada_zip, fila_zip),
    exdir = run_dir,
    junkpaths = TRUE
  )
  entrada <- file.path(run_dir, "registros_corrig.csv")
  fila_completa <- file.path(run_dir, basename(fila_zip))
  dt_in <- fread(
    entrada,
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    colClasses = "character",
    showProgress = FALSE
  )
  fila <- fread(
    fila_completa,
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    colClasses = "character",
    showProgress = FALSE
  )
  fila_triout <- fila[grepl("^TRIOUT", as.character(id_correcao))]
  assert(nrow(fila_triout) == 1L, paste0(basename(zip_path), ": fila real não tem um único TRIOUT"))
  assert(
    as.integer(fila_triout$n_linhas_alvo[[1L]]) == if (zz == 1L) 3L else 2L,
    paste0(basename(zip_path), ": cardinalidade TRIOUT da fila real divergiu")
  )

  fila_aplicar <- file.path(run_dir, "correcoes_campos_fila_real_completa.csv")
  fwrite(fila, fila_aplicar, na = "NA")

  MONITORA_EXEC_ID <- paste0("QA_FLUXO_V281_ROD0", zz + 1L)
  MONITORA_OUTPUT_DIR <- file.path(run_dir, "output_v281")
  MONITORA_LOG_DIR <- file.path(run_dir, "log_v281")
  MONITORA_CORRECOES_DIR <- file.path(
    MONITORA_OUTPUT_DIR,
    "02_painel_correcoes",
    "operacoes_sessao"
  )
  MONITORA_INPUT_DIR <- dirname(entrada)
  MONITORA_ARQUIVO_CORRECOES_CAMPOS <- fila_aplicar
  dir.create(MONITORA_OUTPUT_DIR, recursive = TRUE)
  dir.create(MONITORA_LOG_DIR, recursive = TRUE)
  dir.create(MONITORA_CORRECOES_DIR, recursive = TRUE)

  MONITORA_PERSISTENCIA_DERIVACOES_PENDENTES <- FALSE
  MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
  if (exists("MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA", inherits = FALSE)) {
    rm(MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA, envir = .GlobalEnv)
  }
  if (exists("MONITORA_PLANO_CORRECOES_EFETIVO_ULTIMO", inherits = FALSE)) {
    rm(MONITORA_PLANO_CORRECOES_EFETIVO_ULTIMO, envir = .GlobalEnv)
  }

  # Caminho de produção com a fila REAL COMPLETA de cada rota: arquivo ->
  # preflight/transação -> operações concorrentes -> TRIOUT -> finalização
  # única -> fechamento hierárquico -> persistência.
  dt_final <- monitora_correcao_aplicar_arquivo(
    dt_in,
    arquivo_correcao = fila_aplicar,
    dicionario = NULL
  )
  coletas_excluidas <- unique(as.character(
    fila[
      tolower(trimws(as.character(acao))) %in% c("excluir_coleta", "exclusao_coleta"),
      coleta
    ]
  ))
  coletas_excluidas <- coletas_excluidas[!is.na(coletas_excluidas) & nzchar(coletas_excluidas)]
  ch_in <- monitora_correcao_colunas_chave(dt_in)
  n_excluidas_esperado <- if (
    length(coletas_excluidas) &&
      !is.na(ch_in$coleta) &&
      ch_in$coleta %in% names(dt_in)
  ) {
    sum(as.character(dt_in[[ch_in$coleta]]) %in% coletas_excluidas)
  } else {
    0L
  }
  assert(
    nrow(dt_final) == nrow(dt_in) - n_excluidas_esperado,
    paste0(basename(zip_path), ": aplicar_arquivo divergiu da cardinalidade da fila completa")
  )
  plano_final <- get0(
    "MONITORA_PLANO_CORRECOES_EFETIVO_ULTIMO",
    ifnotfound = data.table(),
    inherits = TRUE
  )
  assert(
    nrow(as.data.table(plano_final)[grepl("^TRIOUT", as.character(id_correcao))]) == 1L,
    paste0(basename(zip_path), ": plano efetivo não preservou a operação TRIOUT")
  )

  ch <- monitora_correcao_colunas_chave(dt_final)
  linhas_apai <- which(
    as.character(dt_final[[ch$coleta]]) == "17626" &
      as.character(dt_final[[ch$ponto_amostral]]) %in% c("11", "23")
  )
  assert(length(linhas_apai) == 2L, paste0(basename(zip_path), ": alvos APAI ausentes após aplicar_arquivo"))
  cols_info <- monitora_correcao_colunas_limpeza_outras_formas(dt_final, NULL)
  col_exotica <- cols_info[
    classe == "lista_principal_forma_vida" & categoria == "exotica",
    coluna
  ][1L]
  col_mm <- names(dt_final)[
    grepl(
      "outra.*especie.*arbusto.*exotic.*inferior",
      monitora_correcao_normalizar_nome_coluna(names(dt_final)),
      perl = TRUE
    )
  ]
  assert(length(col_mm) == 1L, paste0(basename(zip_path), ": coluna MM não única"))
  assert(
    all(as.character(dt_final[[col_exotica]][linhas_apai]) == "arbusto_abaixo") &&
      all(as.character(dt_final[[col_mm]][linhas_apai]) == "MM"),
    paste0(basename(zip_path), ": estado final produtivo não preservou arbusto_abaixo/MM")
  )

  resumo_especifico <- monitora_correcao_auditar_persistencia_limpeza_outras_formas(
    dt_final,
    arquivo_correcao = fila_aplicar,
    dicionario = NULL,
    contexto = paste0("qa_fluxo_produtivo_rod0", zz + 1L),
    abortar = TRUE
  )
  assert(
    resumo_especifico$linhas_com_residuo_total[[1L]] == 0L &&
      resumo_especifico$n_linhas_alvo_ausentes_nao_justificadas[[1L]] == 0L,
    paste0(basename(zip_path), ": gate específico produtivo não zerou")
  )

  # Materialização canônica + releitura pós-exportação pelo motor de produção.
  caminho_solicitado <- file.path(MONITORA_OUTPUT_DIR, "registros_corrig.csv")
  monitora_publicacao_aa_exportar_registros_corrig_aprovado(
    dt_final,
    caminho_solicitado,
    contexto = paste0("qa_fluxo_produtivo_rod0", zz + 1L),
    abortar = TRUE
  )
  candidatos_export <- c(
    caminho_solicitado,
    file.path(MONITORA_OUTPUT_DIR, "01_produtos_dados", "registros_corrig.csv")
  )
  exportado <- candidatos_export[file.exists(candidatos_export)][1L]
  assert(
    length(exportado) == 1L && file.info(exportado)$size > 0L,
    paste0(basename(zip_path), ": checkpoint registros_corrig não materializado")
  )
  dt_exportado <- fread(
    exportado,
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    colClasses = "character",
    showProgress = FALSE
  )
  ch_exp <- monitora_correcao_colunas_chave(dt_exportado)
  linhas_exp <- which(
    as.character(dt_exportado[[ch_exp$coleta]]) == "17626" &
      as.character(dt_exportado[[ch_exp$ponto_amostral]]) %in% c("11", "23")
  )
  assert(
    nrow(dt_exportado) == nrow(dt_final) &&
      all(as.character(dt_exportado[[col_exotica]][linhas_exp]) == "arbusto_abaixo") &&
      all(as.character(dt_exportado[[col_mm]][linhas_exp]) == "MM"),
    paste0(basename(zip_path), ": export/checkpoint não preservou o estado corrigido")
  )

  # Congelamento transversal: contra o produto público gerado pela mesma fila,
  # a nova build só pode divergir nas duas células causais da lista exótica.
  public_dir <- file.path(run_dir, "produto_publico_v280")
  dir.create(public_dir, recursive = TRUE)
  utils::unzip(
    zip_path,
    files = saida_publica_zip,
    exdir = public_dir,
    junkpaths = TRUE
  )
  dt_publico <- fread(
    file.path(public_dir, "registros_corrig.csv"),
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    colClasses = "character",
    showProgress = FALSE
  )
  # Os estados internos de publicação são recalculados pelo harness e não
  # integram o contrato dos dados de campo comparado. Além disso,
  # os cabeçalhos HTML acumulam um nível de escape de aspas a cada round-trip
  # CSV, sem alterar sua identidade semântica. A comparação alinha apenas esses
  # artefatos de transporte e continua estrita para todas as células de dados.
  colunas_internas_publicacao <- c(
    "monitora_status_registros_corrig",
    "monitora_pendencia_impeditiva",
    "monitora_pendencia_impeditiva_tipo",
    "monitora_pendencia_impeditiva_msg",
    "monitora_pendencia_derivacao_interna"
  )
  cols_exportados <- setdiff(names(dt_exportado), colunas_internas_publicacao)
  cols_publicos <- setdiff(names(dt_publico), colunas_internas_publicacao)
  chave_cabecalho <- function(x) gsub('"{2,}', '"', x, perl = TRUE)
  chaves_exportadas <- chave_cabecalho(cols_exportados)
  chaves_publicas <- chave_cabecalho(cols_publicos)
  assert(
    !anyDuplicated(chaves_exportadas) &&
      !anyDuplicated(chaves_publicas) &&
      setequal(chaves_exportadas, chaves_publicas),
    paste0(
      basename(zip_path),
      ": conjunto semântico de colunas divergiu do produto público congelado; n_extras=",
      length(setdiff(chaves_exportadas, chaves_publicas)),
      "; exemplo_extra=",
      encodeString(head(setdiff(chaves_exportadas, chaves_publicas), 1L), quote = "'"),
      "; n_ausentes=",
      length(setdiff(chaves_publicas, chaves_exportadas)),
      "; exemplo_ausente=",
      encodeString(head(setdiff(chaves_publicas, chaves_exportadas), 1L), quote = "'")
    )
  )
  if (
    MONITORA_COL_ROW_ID %in% names(dt_exportado) &&
      MONITORA_COL_ROW_ID %in% names(dt_publico)
  ) {
    ordem_publica <- match(
      as.character(dt_exportado[[MONITORA_COL_ROW_ID]]),
      as.character(dt_publico[[MONITORA_COL_ROW_ID]]),
      nomatch = 0L
    )
    assert(
      !any(ordem_publica == 0L) && !anyDuplicated(ordem_publica),
      paste0(basename(zip_path), ": identidade operacional divergiu do produto público")
    )
    dt_publico <- dt_publico[ordem_publica]
  }
  ordem_colunas_publicas <- match(chaves_exportadas, chaves_publicas)
  assert(
    !anyNA(ordem_colunas_publicas),
    paste0(basename(zip_path), ": alinhamento semântico de cabeçalhos ficou incompleto")
  )
  diffs <- rbindlist(lapply(seq_along(cols_exportados), function(jj) {
    cc <- cols_exportados[[jj]]
    cc_publico <- cols_publicos[[ordem_colunas_publicas[[jj]]]]
    novo <- as.character(dt_exportado[[cc]])
    antigo <- as.character(dt_publico[[cc_publico]])
    novo[is.na(novo) | novo %in% c("", "NA")] <- "<NA>"
    antigo[is.na(antigo) | antigo %in% c("", "NA")] <- "<NA>"
    ii <- which(novo != antigo)
    if (!length(ii)) return(NULL)
    data.table(linha = ii, atributo = cc, antes = antigo[ii], depois = novo[ii])
  }), fill = TRUE)
  diffs_indevidas <- diffs[
    !(chave_cabecalho(atributo) == chave_cabecalho(col_exotica) & linha %in% linhas_exp)
  ]
  assert(
    !nrow(diffs_indevidas),
    paste0(
      basename(zip_path),
      ": mudança fora das duas células causais; ",
      paste(
        head(
          diffs_indevidas[, .N, by = atributo][order(-N)],
          10L
        )[, paste0(atributo, "=", N)],
        collapse = " | "
      ),
      "; exemplos=",
      paste(
        head(diffs_indevidas, 3L)[, paste0(atributo, "[", linha, "]:", antes, "=>", depois)],
        collapse = " | "
      )
    )
  )
  assert(
    nrow(diffs) == 2L &&
      all(chave_cabecalho(diffs$atributo) == chave_cabecalho(col_exotica)) &&
      all(diffs$linha %in% linhas_exp),
    paste0(basename(zip_path), ": diff causal contra v2.8.0 não foi exatamente 2 células")
  )

  persist_export <- monitora_correcao_auditar_releitura_pos_export(
    exportado,
    contexto = paste0("qa_fluxo_produtivo_pos_export_rod0", zz + 1L)
  )
  falhas_triout_export <- persist_export[
    grepl("^TRIOUT", as.character(id_correcao)) &
      grepl("^(erro|falha|bloqueada)", as.character(status_persistencia))
  ]
  assert(
    !nrow(falhas_triout_export),
    paste0(basename(zip_path), ": releitura pós-export ainda falhou para TRIOUT")
  )

  resultados[[zz]] <- data.table(
    fixture = basename(zip_path),
    n_linhas = nrow(dt_exportado),
    n_alvos = length(linhas_apai),
    residuos = resumo_especifico$linhas_com_residuo_total[[1L]],
    falhas_triout_pos_export = nrow(falhas_triout_export)
  )
}

resultado <- rbindlist(resultados)
cat(
  "QA_APAI_FLUXO_PRODUTIVO_V281_OK ",
  paste(
    paste0(
      resultado$fixture,
      ":linhas=", resultado$n_linhas,
      ";alvos=", resultado$n_alvos,
      ";residuos=", resultado$residuos,
      ";falhas_pos_export=", resultado$falhas_triout_pos_export
    ),
    collapse = " | "
  ),
  "\n",
  sep = ""
)
