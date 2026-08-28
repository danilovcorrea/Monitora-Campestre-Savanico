#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.17-dev_r02_20260825_contrato_unico_fechamento_contexto.R",
  mustWork = TRUE
)

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

### Carrega somente definições de funções, inclusive as declaradas dentro de
### blocos tardios, sem iniciar o pipeline do script.
arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    eval(x, env)
  }
  for (ii in seq_along(x)[-1L]) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

### Ambientes globais normalmente criados no carregamento do script.
env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$MONITORA_COL_UUID_REGISTRO_CANONICO <- "MONITORA_UUID_REGISTRO_CANONICO"
env$.MONITORA_MAPA_COLUNAS_CANONICAS_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_FECHAMENTO_HIERARQUICO_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_CONTRATO_MOVIMENTO_CACHE <- new.env(parent = emptyenv())
env$.MONITORA_QA_CACHES <- new.env(parent = emptyenv())
env$monitora_publicacao_ae_cache_env <- function(namespace = "geral") {
  chave <- gsub("[^[:alnum:]_]+", "_", as.character(namespace)[1L])
  if (!exists(chave, envir = env$.MONITORA_QA_CACHES, inherits = FALSE)) {
    assign(chave, new.env(parent = emptyenv()), envir = env$.MONITORA_QA_CACHES)
  }
  get(chave, envir = env$.MONITORA_QA_CACHES, inherits = FALSE)
}
env$monitora_fwrite <- function(x, file, ...) invisible(file)
env$monitora_diag_rel_write_dt <- function(x, arquivo) invisible(arquivo)
env$monitora_log_registrar_evento <- function(...) invisible(NULL)
env$monitora_correcao_xlsform_meta_atual <- function(...) list(opcoes = data.table(
  list_name = c("forma_vida_seca_morta", "tipos_impacto_manejo_uso", "tipos_impacto_manejo_uso"),
  name = c("graminoide", "incendio", "queima_prescrita"),
  label = c("Graminoide", "Incêndio", "Queima prescrita")
))

### RED 1 — caso causal PNSC: texto aberto da espécie não pode criar o token
### histórico `outra` na lista principal de formas exóticas.
col_pai_exotica <- "amostragem/registro/forma_vida_exotica"
col_erva_outra <- "Outra espécie de erva não graminoide exótica: (amostragem/registro)"
dt_pnsc <- data.table(
  pai = "graminoide erva_nao_graminoide",
  especie_aberta = "Braquiária",
  encostam = "exotica"
)
setnames(dt_pnsc, c("pai", "especie_aberta", "encostam"), c(
  col_pai_exotica, col_erva_outra, "amostragem/registro/tipo_forma_vida"
))
regras_pnsc <- env$monitora_correcao_contrato_fechamento_hierarquico(dt_pnsc)
regra_erva <- regras_pnsc[campo_inferior == col_erva_outra]
assert(nrow(regra_erva) == 1L, "PNSC: relação da espécie aberta não foi resolvida uma única vez")
assert(
  regra_erva$status_regra[[1L]] == "ok" &&
    regra_erva$categoria[[1L]] == "exotica" &&
    regra_erva$token[[1L]] == "erva_nao_graminoide" &&
    regra_erva$campo_superior[[1L]] == col_pai_exotica,
  paste0(
    "PNSC: fechamento não veio do relevant do contrato único; obtido=",
    paste(regra_erva[, paste(categoria, token, campo_superior, status_regra, sep = "/")], collapse = " | ")
  )
)
env$monitora_correcao_recalcular_superiores_vinculados(
  dt_pnsc, 1L, modo_encostam = "acrescentar"
)
assert(
  identical(dt_pnsc[[col_pai_exotica]][[1L]], "graminoide erva_nao_graminoide"),
  "PNSC: texto Braquiária injetou token espúrio na lista principal"
)

### RED 2 — cabeçalhos reais pós-importação: os quatro atributos superiores de
### impacto devem ser resolvidos pelo contrato, e não por regex sobre names().
col_impacto <- "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)"
col_tipos <- "Qual(is)? (impact_manejo_uso)"
col_outro <- "Outros tipos de manejo ou uso: (impact_manejo_uso)"
col_descricao <- "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)"
fixture_contexto <- data.table(
  UC = "Parque Teste", EA = "EA-01", UA = "UA-029_VgCS", ANO = "2024",
  COLETA = "13022", form_veg = "savanica",
  impacto = "sim", tipos = "queima_prescrita", outro = "",
  descricao = "Queima prescrita em 2023"
)
setnames(fixture_contexto, c("impacto", "tipos", "outro", "descricao"), c(
  col_impacto, col_tipos, col_outro, col_descricao
))
ocorrencias_contexto <- data.table(
  linha_indice = 1L,
  forma_de_vida_detectada = "graminoide"
)
saida_contexto <- tempfile("qa_v2917_contexto_")
dir.create(saida_contexto)
rel_contexto <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
  fixture_contexto, ocorrencias_contexto, saida_contexto, "pre_painel"
)
assert(nrow(rel_contexto$operacional) == 1L, "Contexto: relatório operacional não materializou a COLETA")
assert(
  rel_contexto$operacional$impacto_manejo_uso[[1L]] == "sim" &&
    rel_contexto$operacional$tipos_impacto_manejo_uso[[1L]] == "queima_prescrita" &&
    rel_contexto$operacional$contexto_fogo[[1L]] == "fogo_explicito_em_campo_estruturado",
  paste0(
    "Contexto: campos reais foram tratados como ausentes; obtido=",
    paste(
      rel_contexto$operacional$impacto_manejo_uso[[1L]],
      rel_contexto$operacional$tipos_impacto_manejo_uso[[1L]],
      rel_contexto$operacional$contexto_fogo[[1L]], sep = "/"
    )
  )
)
analitico_contexto <- env$monitora_relatorios_analiticos_contexto_impactos(fixture_contexto)
assert(
  nrow(analitico_contexto$resumo[token == "queima_prescrita"]) == 1L &&
    nrow(analitico_contexto$resumo[token == "fogo_qualquer_contexto"]) == 1L &&
    all(analitico_contexto$resolucao_colunas$status_resolucao == "resolvido_unico"),
  "Contexto analítico: consumidor não reutilizou a resolução contratual central"
)

### Matriz das 14 formas exóticas abertas válidas atuais. O ramo dependente
### `outros` permanece no XLSForm, mas o token não pertence às choices do pai
### e, por isso, deve ser fail-closed. O teste deriva os
### campos e as relações do próprio contrato e verifica path, name, label e
### alias, impedindo que uma nova forma fique fora da cobertura por esquecimento
### de uma lista hard-coded no teste ou no consumidor.
cu <- env$monitora_contrato_unico_indices_cache(validar = TRUE, forcar = TRUE)
attrs <- data.table::copy(cu$indices$por_atributo_canonico)
deps <- data.table::copy(cu$indices$por_dependencia)
if ("arquivo_xlsform" %in% names(deps)) {
  deps_atual <- deps[grepl("21FEV25|2025", arquivo_xlsform, ignore.case = TRUE)]
  if (nrow(deps_atual)) deps <- deps_atual
}
tokens_abertos_esperados <- c(
  "graminoide", "erva_nao_graminoide", "arbusto_abaixo", "arbusto_acima",
  "arvore_abaixo", "arvore_acima", "bambu", "lianas",
  "ervas_de_passarinho", "palmeira", "bromelioide", "cactacea",
  "orquidea", "samambaia"
)
nomes_abertos_esperados <- c(
  graminoide = "exotica_graminoide_outra_sp",
  erva_nao_graminoide = "exotica_erva_outra_sp",
  arbusto_abaixo = "exotica_arbusto_abaixo_outra_sp",
  arbusto_acima = "exotica_arbusto_acima_outra_sp",
  arvore_abaixo = "exotica_arvore_abaixo_outra_sp",
  arvore_acima = "exotica_arvore_acima_outra_sp",
  bambu = "exotica_bambu_outra_sp",
  lianas = "exotica_lianas_outra_sp",
  ervas_de_passarinho = "exotica_ervas_de_passarinho_outra_sp",
  palmeira = "exotica_palmeira_outra_sp",
  bromelioide = "exotica_bromelioide_outra_sp",
  cactacea = "exotica_cactacea_outra_sp",
  orquidea = "exotica_orquidea_outra_sp",
  samambaia = "exotica_samambaia_outra_sp"
)
rel_abertas <- data.table(
  token = names(nomes_abertos_esperados),
  dependent_name = unname(nomes_abertos_esperados)
)
rel_abertas <- merge(
  rel_abertas,
  attrs[, .(
    dependent_name = name_curto, caminho_registro,
    label = label_2025_sem_html
  )],
  by = "dependent_name", all = FALSE
)
assert(
  nrow(rel_abertas) == 14L && setequal(rel_abertas$token, tokens_abertos_esperados),
  paste0("Matriz: contrato não expôs exatamente as 14 formas abertas válidas: ", paste(rel_abertas$token, collapse = " | "))
)
aliases_cu <- data.table::copy(cu$indices$por_alias_normalizado)

for (ii in seq_len(nrow(rel_abertas))) {
  rr <- rel_abertas[ii]
  alias_rr <- aliases_cu[caminho_registro == rr$caminho_registro, alias]
  alias_rr <- alias_rr[!is.na(alias_rr) & nzchar(alias_rr)]
  variantes <- unique(c(rr$caminho_registro, rr$dependent_name, rr$label, alias_rr[1L]))
  variantes <- variantes[!is.na(variantes) & nzchar(variantes)]
  for (cabecalho in variantes) {
    dt_matriz <- data.table(
      pai = rr$token,
      filho = paste0("valor_livre_", rr$token),
      encostam = "exotica"
    )
    setnames(dt_matriz, c("pai", "filho", "encostam"), c(
      col_pai_exotica, cabecalho, "amostragem/registro/tipo_forma_vida"
    ))
    regra <- env$monitora_correcao_contrato_fechamento_hierarquico(dt_matriz)[
      campo_inferior == cabecalho
    ]
    assert(
      nrow(regra) == 1L && regra$status_regra[[1L]] == "ok" &&
        regra$token[[1L]] == rr$token,
      paste0("Matriz: ", rr$token, " não resolveu por ", cabecalho)
    )
    antes <- copy(dt_matriz)
    env$monitora_correcao_recalcular_superiores_vinculados(
      dt_matriz, 1L, modo_encostam = "acrescentar"
    )
    assert(
      identical(dt_matriz[[col_pai_exotica]][[1L]], rr$token),
      paste0("Matriz: fechamento alterou indevidamente o pai já correto de ", rr$token)
    )
    estado_idempotencia <- copy(dt_matriz)
    env$monitora_correcao_recalcular_superiores_vinculados(
      dt_matriz, 1L, modo_encostam = "acrescentar"
    )
    assert(
      identical(estado_idempotencia, dt_matriz),
      paste0("Matriz: fechamento não idempotente para ", rr$token, " / ", cabecalho)
    )
  }
}

### O dependent legado `exotica_outros_outra_sp` não autoriza criar `outros`
### em forma_vida_exotica, pois essa choice inexiste no pai vigente.
dt_orfao <- data.table(
  pai = "graminoide", filho = "Urochloa sp.", encostam = "exotica"
)
setnames(dt_orfao, names(dt_orfao), c(
  col_pai_exotica, "amostragem/registro/exotica_outros_outra_sp",
  "amostragem/registro/tipo_forma_vida"
))
regra_orfa <- env$monitora_correcao_contrato_fechamento_hierarquico(dt_orfao)[
  campo_inferior == "amostragem/registro/exotica_outros_outra_sp"
]
assert(
  nrow(regra_orfa) == 1L &&
    regra_orfa$status_regra[[1L]] == "bloqueada_token_fora_dominio_choices_pai" &&
    !isTRUE(regra_orfa$token_valido_no_pai[[1L]]),
  "Matriz: ramo órfão `outros` não falhou fechado contra as choices do pai."
)
antes_orfao <- copy(dt_orfao)
env$monitora_correcao_recalcular_superiores_vinculados(
  dt_orfao, 1L, modo_encostam = "acrescentar"
)
assert(identical(dt_orfao, antes_orfao), "Matriz: ramo órfão alterou o registro sem migração inequívoca.")

### Duplicar duas representações físicas do mesmo atributo deve bloquear a
### relação, em vez de escolher a primeira coluna pela ordem do arquivo.
rr_dup <- rel_abertas[token == "erva_nao_graminoide"][1L]
alias_dup <- aliases_cu[caminho_registro == rr_dup$caminho_registro, alias][1L]
dt_dup <- data.table(
  pai = "erva_nao_graminoide",
  filho_path = "Braquiária",
  filho_alias = "Braquiária",
  encostam = "exotica"
)
setnames(dt_dup, c("pai", "filho_path", "filho_alias", "encostam"), c(
  col_pai_exotica, rr_dup$caminho_registro, alias_dup,
  "amostragem/registro/tipo_forma_vida"
))
regra_dup <- env$monitora_correcao_contrato_fechamento_hierarquico(dt_dup)
assert(
  !nrow(regra_dup[status_regra == "ok" & token == "erva_nao_graminoide"]),
  "Matriz: conflito entre path e alias físicos escolheu arbitrariamente uma coluna"
)

### Foto e campo inventado não participam do fechamento; um valor livre jamais
### pode ser interpretado como token.
dt_negativo <- data.table(
  pai = "graminoide", inventado = "arbusto_abaixo", foto = "imagem.jpg",
  encostam = "exotica"
)
setnames(dt_negativo, c("pai", "inventado", "foto", "encostam"), c(
  col_pai_exotica,
  "Outra espécie de erva não graminoide exótica INVENTADA: (amostragem/registro)",
  "amostragem/registro/foto_forma_vida_exotica_desconhecida02",
  "amostragem/registro/tipo_forma_vida"
))
neg <- env$monitora_correcao_contrato_fechamento_hierarquico(dt_negativo)
assert(!nrow(neg[status_regra == "ok"]), "Matriz: campo inventado ou foto gerou fechamento")

### Matriz dos quatro atributos de contexto nas quatro representações
### contratuais. A ordem física é embaralhada para provar independência da
### posição das colunas.
paths_contexto <- c(
  pai = "amostragem/registro/impacto_manejo_uso",
  tipos = "amostragem/registro/tipos_impacto_manejo_uso",
  outro = "amostragem/registro/tipos_impacto_manejo_uso_outro",
  descricao = "amostragem/registro/tipos_impacto_manejo_uso_descricao"
)
attrs_ctx <- attrs[match(unname(paths_contexto), caminho_registro)]
for (representacao in c("caminho_registro", "name_curto", "label_2025_sem_html", "alias")) {
  nomes_ctx <- if (representacao == "alias") {
    vapply(unname(paths_contexto), function(path) {
      aa <- aliases_cu[caminho_registro == path, alias]
      aa <- aa[!is.na(aa) & nzchar(aa)]
      aa[[1L]]
    }, character(1L))
  } else as.character(attrs_ctx[[representacao]])
  dt_ctx <- data.table(
    c1 = "sim", c2 = "incendio queima_prescrita",
    c3 = "fogo observado", c4 = "queima prescrita em 2023"
  )
  setnames(dt_ctx, paste0("c", 1:4), nomes_ctx)
  setcolorder(dt_ctx, rev(names(dt_ctx)))
  res_ctx <- env$monitora_contrato_unico_resolver_contexto_impactos(dt_ctx)
  assert(
    nrow(res_ctx) == 4L && all(res_ctx$status_resolucao == "resolvido_unico") &&
      setequal(res_ctx$coluna, nomes_ctx),
    paste0("Contexto: matriz falhou para representação ", representacao)
  )
}

### Ausência e conflito físico são estados indeterminados explícitos; nunca
### podem ser rebaixados silenciosamente para "sem contexto informado".
fixture_sem_campos <- fixture_contexto[, setdiff(names(fixture_contexto), c(
  col_impacto, col_tipos, col_outro, col_descricao
)), with = FALSE]
rel_sem_campos <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
  fixture_sem_campos, ocorrencias_contexto, tempfile("qa_ctx_abs_"), "pre_painel"
)
assert(
  rel_sem_campos$operacional$contexto_fogo[[1L]] ==
    "contexto_indeterminado_por_falha_de_resolucao",
  "Contexto: ausência de colunas foi confundida com ausência de contexto declarado"
)
fixture_vazio <- copy(fixture_contexto)
for (cc in c(col_impacto, col_tipos, col_outro, col_descricao)) set(fixture_vazio, j = cc, value = "")
rel_vazio <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
  fixture_vazio, ocorrencias_contexto, tempfile("qa_ctx_vazio_"), "pre_painel"
)
assert(
  rel_vazio$operacional$contexto_fogo[[1L]] == "sem_contexto_informado",
  "Contexto: campos resolvidos e vazios deixaram de representar contexto não informado"
)

cat("OK: contrato único governa fechamento hierárquico e contexto de impactos.\n")
