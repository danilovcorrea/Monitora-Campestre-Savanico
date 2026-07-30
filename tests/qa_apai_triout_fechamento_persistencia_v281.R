args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Uso: Rscript tests/qa_apai_triout_fechamento_persistencia_v281.R SCRIPT ZIP_ROD02 ZIP_ROD03",
    call. = FALSE
  )
}

script_path <- normalizePath(args[[1L]], mustWork = TRUE)
zip_paths <- normalizePath(args[2:3], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

exprs <- parse(script_path, keep.source = FALSE, encoding = "UTF-8")
for (expr in exprs) {
  if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
  rhs <- expr[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
}

# O módulo de publicação que contém os aliases é deliberadamente tardio no
# script e fica dentro de um bloco de execução. Carregar somente estas quatro
# definições reais, sem executar o pipeline nem criar um contrato de teste.
funcoes_aliases <- c(
  "monitora_validados_aliases_xlsform_historico",
  "monitora_validados_aliases_adicionais",
  "monitora_validados_unir_aliases",
  "monitora_validados_aliases"
)
carregar_funcoes_nomeadas <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  if (
    identical(expr[[1L]], as.name("<-")) &&
      length(expr) >= 3L &&
      is.symbol(expr[[2L]]) &&
      as.character(expr[[2L]]) %in% funcoes_aliases &&
      is.call(expr[[3L]]) &&
      identical(expr[[3L]][[1L]], as.name("function"))
  ) {
    eval(expr, envir = .GlobalEnv)
    return(invisible(NULL))
  }
  for (ii in seq_along(expr)) {
    try(carregar_funcoes_nomeadas(expr[[ii]]), silent = TRUE)
  }
  invisible(NULL)
}
for (expr in exprs) carregar_funcoes_nomeadas(expr)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())

# O carregador de QA avalia somente funções para não iniciar o pipeline. Por
# isso, a tabela embutida que alimenta o resolvedor normal de categorias não é
# materializada. Este adaptador de teste resolve apenas a coluna-pai física a
# partir do próprio mapa canônico do script; token/categoria/alias continuam
# sendo exatamente os produzidos pelas funções de contrato sob teste.
monitora_correcao_coluna_forma_vida <- function(dt, categoria) {
  cat_val <- as.character(categoria)[1L]
  mapa <- monitora_correcao_mapa_colunas_canonicas(
    dt,
    contexto = "qa_resolucao_pai",
    gravar = FALSE,
    incluir_preenchidos = FALSE
  )
  cand <- mapa[
    papel_coluna == "lista_principal_forma_vida" &
      categoria == cat_val,
    coluna_registros_corrig
  ]
  cand <- unique(cand[!is.na(cand) & nzchar(cand) & cand %in% names(dt)])
  if (length(cand)) cand[[1L]] else NA_character_
}

tmp_aud <- tempfile("qa_v281_persist_")
dir.create(tmp_aud, recursive = TRUE)
MONITORA_LOG_DIR <- file.path(tmp_aud, "log")
MONITORA_CORRECOES_DIR <- file.path(tmp_aud, "correcoes")
MONITORA_EXEC_ID <- "qa_v281"
dir.create(MONITORA_LOG_DIR, recursive = TRUE)
dir.create(MONITORA_CORRECOES_DIR, recursive = TRUE)
monitora_fwrite <- function(x, file, ...) invisible(file)
monitora_log_registrar_evento <- function(...) invisible(NULL)
monitora_correcao_gravar_resumo_operacoes_atomicas <- function(...) data.table()

# A regra de persistência continua compatível com append_token em geral, mas
# TRIOUT não pode aceitar o reaparecimento de um token que removeu. O condicional
# permite usar o mesmo arquivo como prova RED contra a v2.8.0, cuja assinatura
# ainda não possuía o modo destrutivo.
if ("modo_multiselect" %in% names(formals(monitora_correcao_valores_equivalentes))) {
  assert(
    monitora_correcao_valores_equivalentes(
      "arbusto_abaixo outra", "arbusto_abaixo", "forma_vida_exotica",
      modo_multiselect = "superset_compativel"
    ),
    "comparação histórica superset deixou de preservar compatibilidade"
  )
  assert(
    !monitora_correcao_valores_equivalentes(
      "arbusto_abaixo outra", "arbusto_abaixo", "forma_vida_exotica",
      modo_multiselect = "superset_sem_tokens_proibidos",
      tokens_proibidos = "outra"
    ),
    "persistência TRIOUT aceitou o token removido outra no valor final"
  )
  assert(
    monitora_correcao_valores_equivalentes(
      "arbusto_abaixo arvore_abaixo", "arbusto_abaixo", "forma_vida_exotica",
      modo_multiselect = "superset_sem_tokens_proibidos",
      tokens_proibidos = "outra"
    ),
    "persistência TRIOUT bloqueou token válido acrescentado pelo fechamento contratual"
  )
}

# Precedência estrutural independente do caso APAI.
rotulos_estrutura <- c(
  "Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)",
  "Outra espécie de arbusto exótico tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)",
  "Outra espécie de árvore exótica tocando a vareta com DAP menor que 5cm: (amostragem/registro)",
  "Outra espécie de árvore exótica tocando a vareta com DAP igual ou maior que 5cm: (amostragem/registro)"
)
tokens_estrutura <- monitora_correcao_token_associado_coluna(rotulos_estrutura)
assert(
  identical(tokens_estrutura, c("arbusto_abaixo", "arbusto_acima", "arvore_abaixo", "arvore_acima")),
  paste0("precedência estrutural incorreta: ", paste(tokens_estrutura, collapse = " | "))
)
assert(
  identical(
    monitora_correcao_token_associado_coluna("amostragem/registro/exotica_outros_outra_sp"),
    "outros"
  ),
  "token atual outros do XLSForm 2025 foi confundido com o legado singular outra"
)

dt_ambiguo <- setNames(
  data.table("arbusto_abaixo", "X", "exotica"),
  c(
    "Formas de vida de plantas exóticas: (amostragem/registro)",
    "Outra espécie de arbusto exótico INVENTADA tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)",
    "Encostam na vareta: (amostragem/registro)"
  )
)
regra_ambigua <- monitora_correcao_contrato_fechamento_hierarquico(dt_ambiguo)[
  campo_inferior == "Outra espécie de arbusto exótico INVENTADA tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)"
]
assert(
  nrow(regra_ambigua) == 1L &&
    regra_ambigua$status_regra[[1L]] == "nao_derivavel_relacao_ambigua" &&
    is.na(regra_ambigua$token[[1L]]),
  "campo 'Outra espécie' sem alias exato ainda gerou mutação ascendente"
)

# `outros` é choice vigente de forma_vida_exotica no XLSForm 2025. Não é
# resíduo legado, não pode ser removido por TRIOUT e deve continuar sustentado
# pelo fechamento a partir de seu campo de espécie.
dt_outros_atual <- data.table(
  `amostragem/registro/forma_vida_exotica` = "outros",
  `amostragem/registro/especies_exotica_outros` = "agave_americana_exotica_outros",
  `amostragem/registro/exotica_outros_outra_sp` = "Agave sp.",
  `amostragem/registro/tipo_forma_vida` = "exotica"
)
cols_outros_atual <- monitora_correcao_colunas_limpeza_outras_formas(dt_outros_atual, NULL)
assert(
  !length(monitora_correcao_linhas_residuo_outras_formas(
    dt_outros_atual,
    1L,
    cols_outros_atual
  )),
  "token atual outros foi classificado como resíduo legado"
)
triout_outros_atual <- monitora_correcao_aplicar_limpeza_outras_formas_atomica(
  dt_outros_atual,
  1L,
  id_correcao = "TRIOUT_QA_OUTROS_ATUAL"
)
assert(
  !isTRUE(triout_outros_atual$falha) &&
    dt_outros_atual[["amostragem/registro/forma_vida_exotica"]][[1L]] == "outros" &&
    dt_outros_atual[["amostragem/registro/exotica_outros_outra_sp"]][[1L]] == "Agave sp.",
  "TRIOUT removeu choice/campo de espécie atuais do ramo exotica/outros"
)

# Caches da v2.8.0 podem coexistir na mesma sessão R durante o desenvolvimento.
# Entradas antigas deliberadamente erradas não podem ser reutilizadas pela
# chave versionada da v2.8.1.
dt_cache <- setNames(
  data.table("arbusto_abaixo", "MM", "exotica"),
  c(
    "Formas de vida de plantas exóticas: (amostragem/registro)",
    rotulos_estrutura[[1L]],
    "Encostam na vareta: (amostragem/registro)"
  )
)
assign(
  monitora_correcao_mapa_colunas_cache_key(dt_cache, "estrutura"),
  data.table(
    coluna_registros_corrig = names(dt_cache),
    coluna_norm = monitora_correcao_normalizar_nome_coluna(names(dt_cache)),
    papel_coluna = c("lista_principal_forma_vida", "especie_nome_popular", "campo_superior_tipo_forma_vida"),
    categoria = c("exotica", "exotica", NA_character_),
    token_associado = c(NA_character_, "outra", NA_character_)
  ),
  envir = .MONITORA_MAPA_COLUNAS_CANONICAS_CACHE
)
assign(
  monitora_correcao_mapa_colunas_cache_key(dt_cache, "fechamento_hierarquico_r24"),
  data.table(
    campo_inferior = rotulos_estrutura[[1L]],
    papel_inferior = "especie_nome_popular",
    categoria = "exotica",
    token = "outra",
    campo_superior = names(dt_cache)[[1L]],
    papel_superior = "lista_principal_forma_vida",
    status_regra = "ok"
  ),
  envir = .MONITORA_FECHAMENTO_HIERARQUICO_CACHE
)
regra_cache_v281 <- monitora_correcao_contrato_fechamento_hierarquico(dt_cache)[
  campo_inferior == rotulos_estrutura[[1L]]
]
assert(
  nrow(regra_cache_v281) == 1L && regra_cache_v281$token[[1L]] == "arbusto_abaixo",
  "v2.8.1 reutilizou cache estrutural/hierárquico incompatível da v2.8.0"
)

resultados <- vector("list", length(zip_paths))
for (zz in seq_along(zip_paths)) {
  zip_path <- zip_paths[[zz]]
  lista_zip <- utils::unzip(zip_path, list = TRUE)
  alvo <- lista_zip$Name[grepl("/input/registros_corrig\\.csv$", lista_zip$Name)]
  assert(length(alvo) == 1L, paste0(basename(zip_path), ": input/registros_corrig.csv não único"))
  exdir <- tempfile(paste0("qa_apai_v281_", zz, "_"))
  dir.create(exdir, recursive = TRUE)
  utils::unzip(zip_path, files = alvo, exdir = exdir, junkpaths = TRUE)
  dt <- fread(
    file.path(exdir, "registros_corrig.csv"),
    encoding = "UTF-8",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )

  ch <- monitora_correcao_colunas_chave(dt)
  assert(
    !is.na(ch$coleta) && !is.na(ch$ponto_amostral),
    paste0(basename(zip_path), ": chaves COLETA/ponto não resolvidas")
  )
  linhas_apai <- which(
    as.character(dt[[ch$coleta]]) == "17626" &
      as.character(dt[[ch$ponto_amostral]]) %in% c("11", "23")
  )
  assert(length(linhas_apai) == 2L, paste0(basename(zip_path), ": casos 17626/11 e 17626/23 ausentes"))

  cols_info <- monitora_correcao_colunas_limpeza_outras_formas(dt, NULL)
  linhas_residuo <- monitora_correcao_linhas_residuo_outras_formas(dt, seq_len(nrow(dt)), cols_info)
  assert(
    all(linhas_apai %in% linhas_residuo),
    paste0(basename(zip_path), ": casos APAI não constam do resíduo inicial")
  )
  col_exotica <- cols_info[
    classe == "lista_principal_forma_vida" & categoria == "exotica",
    coluna
  ][1L]
  col_especie <- names(dt)[
    grepl(
      "outra.*especie.*arbusto.*exotic.*inferior",
      monitora_correcao_normalizar_nome_coluna(names(dt)),
      perl = TRUE
    )
  ]
  assert(length(col_especie) == 1L, paste0(basename(zip_path), ": campo específico MM não único"))
  assert(
    all(monitora_correcao_token_presente_vec(dt[[col_exotica]][linhas_apai], "outra")),
    paste0(basename(zip_path), ": defeito causal singular outra não reproduzido")
  )
  assert(
    all(as.character(dt[[col_especie]][linhas_apai]) == "MM"),
    paste0(basename(zip_path), ": conteúdo legítimo MM não reproduzido")
  )

  res <- monitora_correcao_aplicar_limpeza_outras_formas_atomica(
    dt,
    linhas_residuo,
    id_correcao = paste0("TRIOUT_APAI_V281_ROD0", zz + 1L)
  )
  assert(!isTRUE(res$falha), paste0(basename(zip_path), ": aplicação TRIOUT falhou"))
  assert(
    !length(monitora_correcao_linhas_residuo_outras_formas(dt, linhas_residuo, cols_info)),
    paste0(basename(zip_path), ": resíduo permaneceu imediatamente após TRIOUT")
  )

  regras <- monitora_correcao_contrato_fechamento_hierarquico(dt)
  regra_mm <- regras[campo_inferior == col_especie]
  assert(nrow(regra_mm) == 1L, paste0(basename(zip_path), ": relação contratual do campo MM não única"))
  assert(
    regra_mm$status_regra[[1L]] == "ok" &&
      regra_mm$categoria[[1L]] == "exotica" &&
      regra_mm$token[[1L]] == "arbusto_abaixo" &&
      regra_mm$campo_superior[[1L]] == col_exotica,
    paste0(
      basename(zip_path),
      ": campo 'Outra espécie de arbusto...' não resolveu para exotica/arbusto_abaixo; obtido=",
      paste(
        regra_mm[, paste(categoria, token, campo_superior, status_regra, sep = "/")],
        collapse = " | "
      )
    )
  )
  aliases_outros <- monitora_validados_aliases()[[
    "amostragem/registro/exotica_outros_outra_sp"
  ]]
  col_outros <- intersect(as.character(aliases_outros), names(dt))
  assert(length(col_outros) == 1L, paste0(basename(zip_path), ": alias atual exotica_outros não único"))
  regra_outros <- regras[campo_inferior == col_outros]
  assert(
    nrow(regra_outros) == 1L &&
      regra_outros$token[[1L]] == "outros" &&
      regra_outros$categoria[[1L]] == "exotica" &&
      regra_outros$status_regra[[1L]] == "ok",
    paste0(basename(zip_path), ": contrato atual outros foi bloqueado junto do legado outra")
  )

  audit_fechamento <- monitora_correcao_recalcular_superiores_vinculados(
    dt,
    linhas_residuo,
    modo_encostam = "acrescentar"
  )
  assert(
    all(as.character(dt[[col_exotica]][linhas_apai]) == "arbusto_abaixo"),
    paste0(basename(zip_path), ": fechamento reintroduziu token indevido na lista exótica")
  )
  assert(
    all(as.character(dt[[col_especie]][linhas_apai]) == "MM"),
    paste0(basename(zip_path), ": fechamento alterou o conteúdo legítimo MM")
  )
  assert(
    !length(monitora_correcao_linhas_residuo_outras_formas(dt, linhas_residuo, cols_info)),
    paste0(basename(zip_path), ": fechamento hierárquico reintroduziu resíduo TRIOUT")
  )
  audit_ascensao_indevida <- if (
    all(c("dependent_name", "token") %in% names(audit_fechamento))
  ) {
    audit_fechamento[
      dependent_name == col_especie &
        monitora_correcao_normalizar_nome_coluna(token) %in% c("outra", "outro", "outras")
    ]
  } else {
    data.table()
  }
  assert(
    !nrow(audit_ascensao_indevida),
    paste0(basename(zip_path), ": auditoria registrou ascensão indevida de outra a partir do campo MM")
  )

  estado_antes_idempotencia <- copy(dt[linhas_residuo])
  monitora_correcao_recalcular_superiores_vinculados(
    dt,
    linhas_residuo,
    modo_encostam = "acrescentar"
  )
  assert(
    identical(estado_antes_idempotencia, dt[linhas_residuo]),
    paste0(basename(zip_path), ": fechamento v2.8.1 não é idempotente")
  )

  persist_ok <- monitora_correcao_auditar_persistencia_operacoes(
    dt,
    res$audit,
    chaves = ch,
    contexto = paste0("qa_v281_rod0", zz + 1L),
    abortar = FALSE
  )
  assert(
    !nrow(persist_ok[
      grepl("^TRIOUT", as.character(id_correcao)) &
        grepl("^falha", as.character(status_persistencia))
    ]),
    paste0(basename(zip_path), ": persistência rejeitou a correção final válida")
  )

  # Prova negativa: mesmo havendo auditoria posterior compatível para a mesma
  # célula e sentinela intermediária OK, um token removido reintroduzido no
  # objeto final deve continuar como falha, sem máscara por sobreposição.
  dt_residual <- copy(dt)
  linha_teste <- linhas_apai[[1L]]
  valor_correto <- as.character(dt_residual[[col_exotica]][linha_teste])
  valor_residual <- monitora_correcao_append_token_valor(valor_correto, "outra")
  set(dt_residual, i = linha_teste, j = col_exotica, value = valor_residual)
  audit_neg <- data.table(
    id_correcao = c("TRIOUT_NEGATIVO_V281", "TRIOUT_NEGATIVO_V281", "OPERACAO_POSTERIOR_QA"),
    status = c("aplicada", "auditoria_ok", "aplicada"),
    atributo = c(col_exotica, "__limpar_outras_formas_vida__", col_exotica),
    linha_indice = c(linha_teste, NA_integer_, linha_teste),
    valor_antes = c(valor_residual, "1", valor_correto),
    valor_depois = c(valor_correto, "0", valor_residual)
  )
  persist_neg <- monitora_correcao_auditar_persistencia_operacoes(
    dt_residual,
    audit_neg,
    chaves = ch,
    contexto = paste0("qa_v281_neg_rod0", zz + 1L),
    abortar = FALSE
  )
  linha_triout_neg <- persist_neg[id_correcao == "TRIOUT_NEGATIVO_V281"]
  assert(
    nrow(linha_triout_neg) == 1L &&
      linha_triout_neg$status_persistencia[[1L]] == "falha_valor_nao_persistiu" &&
      linha_triout_neg$modo_comparacao[[1L]] == "tokens_multiselect_superset_sem_tokens_removidos",
    paste0(
      basename(zip_path),
      ": persistência mascarou o resíduo reintroduzido; obtido=",
      paste(
        linha_triout_neg[, paste(status_persistencia, modo_comparacao, valor_final, sep = "/")],
        collapse = " | "
      )
    )
  )

  resultados[[zz]] <- data.table(
    fixture = basename(zip_path),
    n_linhas = nrow(dt),
    residuos_iniciais = length(linhas_residuo),
    casos_apai = length(linhas_apai),
    residuos_finais = length(
      monitora_correcao_linhas_residuo_outras_formas(dt, linhas_residuo, cols_info)
    )
  )
}

resultado <- rbindlist(resultados)
cat(
  "QA_APAI_TRIOUT_FECHAMENTO_PERSISTENCIA_V281_OK ",
  paste(
    paste0(
      resultado$fixture,
      ":residuos=", resultado$residuos_iniciais,
      "->", resultado$residuos_finais,
      ";casos_apai=", resultado$casos_apai
    ),
    collapse = " | "
  ),
  "\n",
  sep = ""
)
