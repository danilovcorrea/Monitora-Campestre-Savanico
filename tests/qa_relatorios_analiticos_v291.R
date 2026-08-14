#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script <- if (length(args) >= 1L) args[[1L]] else
  "monitora_campsav_alvo_global_v2.9.3.R"
fonte <- if (length(args) >= 2L) args[[2L]] else
  Sys.getenv("MONITORA_QA_OUTPUT_FONTE", unset = "")
dir_qa_base <- if (length(args) >= 3L) args[[3L]] else file.path("tmp", "pdfs")
ativar_sentinel <- identical(
  toupper(trimws(if (length(args) >= 5L) args[[5L]] else
    Sys.getenv("MONITORA_QA_SENTINEL2", unset = "S"))),
  "S"
)
dir_qa_fixo <- if (length(args) >= 4L) {
  trimws(args[[4L]])
} else {
  trimws(Sys.getenv("MONITORA_QA_DIR_FIXO", unset = ""))
}

if (!file.exists(script)) stop("Script candidato ausente: ", script, call. = FALSE)
if (!dir.exists(fonte)) stop("Output-fonte ausente: ", fonte, call. = FALSE)
dir.create(dir_qa_base, recursive = TRUE, showWarnings = FALSE)
dir_qa <- if (nzchar(dir_qa_fixo)) {
  dir_qa_fixo
} else {
  tempfile("v290_relatorios_qa_", tmpdir = dir_qa_base)
}
dir.create(dir_qa, recursive = TRUE, showWarnings = FALSE)
### O processo Windows iniciado a partir do WSL pode herdar `C:\\Windows` como
### diretório de trabalho. Fixar o diretório temporário de QA impede que um
### dispositivo gráfico implícito tente criar `Rplots.pdf` em área protegida.
setwd(normalizePath(dir_qa, winslash = "/", mustWork = TRUE))

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(rmarkdown)
  library(pagedown)
  library(digest)
})

falhar <- function(...) stop(paste0(...), call. = FALSE)
assert <- function(condicao, mensagem) if (!isTRUE(condicao)) falhar(mensagem)

### Carrega somente definições funcionais do módulo, sem executar o pipeline.
### A extração percorre a árvore sintática porque o script público possui blocos
### protegidos e condicionais em torno das definições.
arvore <- parse(file = script, keep.source = FALSE)
funcoes <- new.env(parent = globalenv())
coletar_funcoes <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  op <- as.character(expr[[1L]])[1L]
  if (op %in% c("<-", "=") && length(expr) >= 3L && is.symbol(expr[[2L]])) {
    nome <- as.character(expr[[2L]])
    rhs <- expr[[3L]]
    definicao_funcional <- is.call(rhs) &&
      identical(as.character(rhs[[1L]]), "function")
    definicao_funcional <- definicao_funcional ||
      identical(nome, "monitora_relatorios_analiticos_resolver_pandoc")
    if (isTRUE(definicao_funcional) && (
        startsWith(nome, "monitora_relatorios_analiticos_") ||
        startsWith(nome, "monitora_diag_seca_morta_") ||
        nome %in% c(
          "monitora_arquivo_retentativas",
          "monitora_arquivo_hash_transacao",
          "monitora_arquivo_publicar_candidato",
          "monitora_relatorio_classe_portugues",
          "monitora_relatorio_rotulo_metrica",
          "monitora_relatorio_rotulo_grupo",
          "monitora_relatorio_rotulo_formacao",
          "monitora_correcao_colunas_chave",
          "monitora_correcao_colunas_forma_vida_categoria",
          "monitora_correcao_normalizar_nome_coluna",
          "monitora_relatorio_exoticas_tem_token",
          "monitora_relatorio_exoticas_normalizar_token",
          "monitora_diag_rel_write_dt"
        ))) {
      eval(expr, envir = funcoes)
    }
    return(invisible(NULL))
  }
  for (ii in seq_along(expr)[-1L]) coletar_funcoes(expr[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar_funcoes))

esperadas <- c(
  "monitora_relatorios_analiticos_esforco",
  "monitora_relatorios_analiticos_mapas",
  "monitora_relatorios_analiticos_gerar",
  "monitora_relatorios_analiticos_validar_editorial",
  "monitora_relatorio_rotulo_formacao"
)
assert(all(vapply(esperadas, exists, logical(1L), envir = funcoes, inherits = FALSE)),
       "Nem todas as funções do módulo foram extraídas do script candidato.")

list2env(as.list.environment(funcoes, all.names = TRUE), envir = .GlobalEnv)
linhas_cabecalho_script <- readLines(script, n = 500L, warn = FALSE, encoding = "UTF-8")
extrair_constante_script <- function(nome, padrao) {
  rx <- paste0("^[[:space:]]*", nome, "[[:space:]]*<-[[:space:]]*\"")
  linha <- grep(rx, linhas_cabecalho_script, value = TRUE)[1L]
  if (is.na(linha) || !nzchar(linha)) return(padrao)
  sub('^[^\"]*\"([^\"]+)\".*$', "\\1", linha, perl = TRUE)
}
MONITORA_SCRIPT_VERSAO <- extrair_constante_script(
  "MONITORA_SCRIPT_VERSAO",
  "versão não identificada"
)
MONITORA_SCRIPT_BUILD_ID <- extrair_constante_script(
  "MONITORA_SCRIPT_BUILD_ID",
  "build não identificado"
)
MONITORA_CAMINHO_NAVEGADOR_PDF <- Sys.getenv(
  "MONITORA_CAMINHO_NAVEGADOR_PDF",
  unset = ""
)
MONITORA_COL_ROW_ID <- ".monitora_row_id"
assert(
  identical(monitora_relatorios_analiticos_situacao_dados("CONTRATO VALIDADO"), "Validado") &&
    identical(monitora_relatorios_analiticos_situacao_dados("QA"), "Em validação") &&
    identical(monitora_relatorios_analiticos_situacao_dados("CONSULTAR AUDITORIAS"), "Em validação") &&
    identical(monitora_relatorios_analiticos_situacao_dados("BLOQUEADO"), "Não validado"),
  "Mapeamento editorial da situação dos dados divergente do contrato fechado."
)

### Gate contra especialização geográfica: o módulo não pode conter nomes de
### UCs, IDs de cenas, tiles ou coordenadas dos datasets usados na homologação.
funcoes_orbitais <- c(
  "monitora_relatorios_analiticos_bbox_satelite",
  "monitora_relatorios_analiticos_consultar_sentinel2",
  "monitora_relatorios_analiticos_baixar_mapa_satelite_sentinel2",
  "monitora_relatorios_analiticos_gerar_mapa_satelite"
)
codigo_orbital <- paste(vapply(
  funcoes_orbitais,
  function(nome) paste(deparse(get(nome, envir = funcoes)), collapse = "\n"),
  character(1L)
), collapse = "\n")
assert(
  !grepl(
    paste(c(
      "FNCS", "Contendas", "Mapinguari", "PNM",
      "24LTK", "20MLS", "20MMS", "S2A_", "S2B_", "S2C_",
      "-41.0", "-63.8", "-13.9", "-7.8"
    ), collapse = "|"),
    codigo_orbital,
    ignore.case = TRUE,
    perl = TRUE
  ),
  "O módulo orbital contém especialização geográfica ou cena fixa."
)
fixture_bbox_a <- data.table(
  long_ini = c(-50.01, -50.00),
  long_fin = c(-49.99, -49.98),
  lon_meio = c(-50.00, -49.99),
  lat_ini = c(-10.01, -10.00),
  lat_fin = c(-9.99, -9.98),
  lat_meio = c(-10.00, -9.99)
)
fixture_bbox_b <- copy(fixture_bbox_a)
fixture_bbox_b[, c("long_ini", "long_fin", "lon_meio") :=
  lapply(.SD, function(x) x + 15), .SDcols = c("long_ini", "long_fin", "lon_meio")]
bbox_a <- monitora_relatorios_analiticos_bbox_satelite(fixture_bbox_a)
bbox_b <- monitora_relatorios_analiticos_bbox_satelite(fixture_bbox_b)
assert(
  !isTRUE(all.equal(unname(bbox_a), unname(bbox_b))) &&
    bbox_a[["xmin"]] < min(fixture_bbox_a$long_ini) &&
    bbox_a[["xmax"]] > max(fixture_bbox_a$long_fin) &&
    bbox_b[["xmin"]] < min(fixture_bbox_b$long_ini) &&
    bbox_b[["xmax"]] > max(fixture_bbox_b$long_fin),
  "A extensão orbital não foi derivada dinamicamente das coordenadas de entrada."
)

ler <- function(rel, obrigatorio = TRUE) {
  p <- file.path(fonte, rel)
  if (!file.exists(p)) {
    if (isTRUE(obrigatorio)) falhar("Produto-fonte ausente: ", p)
    return(data.table())
  }
  fread(p, encoding = "UTF-8", showProgress = FALSE)
}

registros <- ler("01_produtos_dados/registros_corrig.csv")
stat <- ler("01_produtos_dados/registros_corrig_stat.csv")
periodo <- ler("05_estatisticas/estatistica_pareada_periodo_editorial.csv", FALSE)
linha_base <- ler("05_estatisticas/estatisticas_mudanca_linha_base.csv", FALSE)
composicao <- ler("05_estatisticas/estatisticas_composicao_linha_base.csv", FALSE)
composicao_periodo <- ler("05_estatisticas/estatisticas_composicao_geral_ano_a_ano.csv", FALSE)
config <- ler("05_estatisticas/estatisticas_mudanca_config.csv", FALSE)
cob_categ <- ler("05_estatisticas/cob_veg_categ.csv")
cob_nat <- ler("05_estatisticas/cob_veg_form_vida_nat.csv")
cob_exot <- ler("05_estatisticas/cob_veg_form_vida_exot.csv", FALSE)
prop_herb_lenh <- ler("05_estatisticas/prop_rel_herb_lenh.csv", FALSE)
cob_herb_lenh <- ler("05_estatisticas/cob_veg_herb_lenh.csv", FALSE)
prop_categ <- ler("05_estatisticas/prop_rel_categ.csv", FALSE)
prop_nat <- ler("05_estatisticas/prop_rel_form_vida_nat.csv", FALSE)
prop_exot <- ler("05_estatisticas/prop_rel_form_vida_exot.csv", FALSE)
prop_seca <- ler("05_estatisticas/prop_rel_form_vida_seca_morta.csv", FALSE)
cob_seca <- ler("05_estatisticas/cob_veg_form_vida_seca_morta.csv", FALSE)
prop_material <- ler("05_estatisticas/prop_rel_material_botanico.csv", FALSE)
cob_material <- ler("05_estatisticas/cob_veg_material_botanico.csv", FALSE)

resultado <- monitora_relatorios_analiticos_gerar(
  registros = registros,
  stat = stat,
  mudanca_periodo = periodo,
  mudanca_linha_base = linha_base,
  composicao_periodo = composicao_periodo,
  composicao_linha_base = composicao,
  config_stat = config,
  cob_categ = cob_categ,
  cob_nat = cob_nat,
  cob_exot = cob_exot,
  output_dir = dir_qa,
  formatos = c("rmd", "md", "html", "docx", "pdf"),
  mapa_satelite = ativar_sentinel,
  fonte_mapa_satelite = "SENTINEL2_PUBLICO",
  status_validacao = "QA",
  prop_herb_lenh = prop_herb_lenh,
  cob_herb_lenh = cob_herb_lenh,
  prop_categ = prop_categ,
  prop_nat = prop_nat,
  prop_exot = prop_exot,
  prop_seca = prop_seca,
  cob_seca = cob_seca,
  prop_material = prop_material,
  cob_material = cob_material
)

dir_rel <- resultado$diretorio
assert(dir.exists(dir_rel), "Diretório final dos relatórios não foi criado.")
indice <- fread(file.path(dir_rel, "indice_relatorios_analiticos.csv"), encoding = "UTF-8")
assert(nrow(indice) == 10L, "O índice não contém os dez documentos esperados.")
assert(all(indice$existe), "Há documento ausente no índice.")
assert(all(indice$tamanho_bytes > 0), "Há documento vazio no índice.")
assert(setequal(indice$formato, c("rmd", "md", "html", "docx", "pdf")),
       "Conjunto de formatos divergente.")
assert(setequal(indice$versao_editorial, c("sintético", "detalhado")),
       "Versões editoriais divergentes.")

auditoria_pdf_isolado <- fread(
  file.path(dir_rel, "auditoria_renderizacao_pdf_isolada.csv"),
  encoding = "UTF-8"
)
assert(
  nrow(auditoria_pdf_isolado) == 2L &&
    all(auditoria_pdf_isolado$ok) &&
    all(auditoria_pdf_isolado$status_processo == 0L) &&
    all(auditoria_pdf_isolado$tamanho_bytes > 1000),
  "Renderização PDF isolada não concluiu e validou os dois relatórios."
)
performance_relatorios <- fread(
  file.path(dir_rel, "performance_relatorios_analiticos.csv"),
  encoding = "UTF-8"
)
etapas_performance_obrigatorias <- c(
  "relatorios_analiticos_esforco",
  "relatorios_analiticos_conteudo",
  "relatorios_analiticos_docx_sintetico",
  "relatorios_analiticos_html_sintetico",
  "relatorios_analiticos_pdf_sintetico",
  "relatorios_analiticos_docx_detalhado",
  "relatorios_analiticos_html_detalhado",
  "relatorios_analiticos_pdf_detalhado",
  "relatorios_analiticos_finalizacao"
)
assert(
  all(etapas_performance_obrigatorias %in% performance_relatorios$etapa) &&
    all(is.finite(performance_relatorios$duracao_seg)) &&
    all(performance_relatorios$duracao_seg >= 0),
  "Performance interna de DOCX, HTML e PDF não foi materializada integralmente."
)

series_anuais <- fread(
  file.path(dir_rel, "series_anuais_relatorios_por_ua.csv"),
  encoding = "UTF-8"
)
assert(nrow(series_anuais) > 0L, "Séries anuais alinhadas à UA não foram materializadas.")
assert(all(c(
  "media_percent", "ci_lower_percent", "ci_upper_percent", "n_UA",
  "unidade_analitica", "estimando_descritivo"
) %in% names(series_anuais)), "Séries anuais não contêm estimando, IC95% e esforço por UA.")
assert(all(series_anuais$unidade_analitica == "UA"), "Unidade analítica anual divergente de UA.")

indice_graficos <- fread(
  file.path(dir_rel, "indice_selecao_graficos.csv"),
  encoding = "UTF-8"
)
paineis_inferenciais <- indice_graficos[
  grepl("^inferencias_", id) & disponivel == TRUE
]
if (data.table::uniqueN(stat$ANO) >= 2L) {
  pares_inferenciais_esperados <- unique(periodo[
    !is.na(grupo_grafico) & nzchar(grupo_grafico) &
      !is.na(tipo_metrica) & nzchar(tipo_metrica),
    paste(grupo_grafico, tipo_metrica, sep = "__")
  ])
  pares_inferenciais_gerados <- unique(paineis_inferenciais[
    , paste(tema, metrica, sep = "__")
  ])
  assert(
    setequal(pares_inferenciais_gerados, pares_inferenciais_esperados),
    paste0(
      "Os painéis inferenciais não correspondem exatamente aos temas/métricas ",
      "com testes estatísticos disponíveis (esperados=", length(pares_inferenciais_esperados),
      "; gerados=", length(pares_inferenciais_gerados), ")."
    )
  )
  assert(
    all(paineis_inferenciais$estatistica_incorporada == TRUE) &&
      all(paineis_inferenciais$n_resultados_estatisticos > 0L),
    "Painel inferencial sem estatística incorporada ou sem resultados."
  )
  assert(
    !any(grepl("(^|;[[:space:]]*)NA($|;)", paineis_inferenciais$classes_incorporadas)) &&
      any(grepl("Mudança na composição", paineis_inferenciais$classes_incorporadas, fixed = TRUE)),
    "Há classe inferencial não mapeada ou a mudança composicional não foi representada visualmente."
  )
  auditoria_estatistica <- fread(
    file.path(dir_rel, "auditoria_integracao_estatistica_graficos_relatorio.csv"),
    encoding = "UTF-8"
  )
  assert(
    nrow(auditoria_estatistica) == nrow(periodo) + nrow(composicao_periodo),
    "Auditoria dos gráficos não cobre exatamente os testes por categoria e de composição."
  )
  assert(
    all(auditoria_estatistica$associacao_nao_causal %in% TRUE) &&
      all(nzchar(auditoria_estatistica$hash_conteudo_resultado)),
    "Cautela causal ou hash de rastreabilidade ausente na integração estatística."
  )
} else {
  assert(nrow(paineis_inferenciais) == 0L, "Campanha única gerou painel inferencial temporal indevido.")
  aplicabilidade <- fread(file.path(dir_rel, "auditoria_aplicabilidade_inferencia_temporal.csv"))
  assert(
    identical(aplicabilidade$estado[[1L]], "nao_aplicavel_serie_temporal_uma_campanha") &&
      !isTRUE(aplicabilidade$comparacao_temporal_executada[[1L]]),
    "Campanha única não declarou a inaplicabilidade da inferência temporal."
  )
}
if (data.table::uniqueN(stat$ANO) >= 2L) assert(
  setequal(
    unique(na.omit(auditoria_estatistica$classe_periodo)),
    unique(c(na.omit(periodo$classe_mudanca), na.omit(composicao_periodo$classe_mudanca_composicao)))
  ),
  "Alguma classe inferencial disponível foi omitida dos painéis."
)
auditoria_robustez <- fread(
  file.path(dir_rel, "auditoria_robustez_inferencial_relatorio.csv"),
  encoding = "UTF-8"
)
assert(
  all(c("atendido", "requer validação por indicador") %in% auditoria_robustez$status) &&
    (data.table::uniqueN(stat$ANO) == 1L || "limitação explícita" %in% auditoria_robustez$status),
  "Auditoria de robustez não explicita implementação, limitações e validação de margens."
)
docx_relatorios <- file.path(dir_qa, indice[formato == "docx", caminho_relativo])
assert(
  length(docx_relatorios) == 2L && all(file.info(docx_relatorios)$size > 100000L),
  "DOCX sintético ou detalhado ausente/vazio."
)
assert(
  all(vapply(
    docx_relatorios,
    function(arq) sum(grepl("^word/media/", utils::unzip(arq, list = TRUE)$Name)) >= 4L,
    logical(1L)
  )),
  "Um DOCX não incorporou as figuras do relatório."
)
assert(
  all(vapply(docx_relatorios, function(arq) {
    dir_xml <- tempfile("qa_docx_xml_")
    dir.create(dir_xml, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dir_xml, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(arq, files = "word/document.xml", exdir = dir_xml)
    xml <- paste(readLines(
      file.path(dir_xml, "word", "document.xml"),
      warn = FALSE,
      encoding = "UTF-8"
    ), collapse = "")
    contar <- function(padrao) {
      pos <- gregexpr(padrao, xml, perl = TRUE)[[1L]]
      if (identical(pos[[1L]], -1L)) 0L else length(pos)
    }
    n_linhas <- contar("<w:tr(?:[ >])")
    n_linhas > 0L && contar("<w:cantSplit(?:[ />])") == n_linhas
  }, logical(1L))),
  "Um DOCX permite dividir linhas de tabela entre páginas."
)

esforco <- fread(
  file.path(dir_rel, "esforco_amostral_por_uc_formacao_ano.csv"),
  encoding = "UTF-8"
)
assert(!"n_transectos" %in% names(esforco), "A coluna redundante de transecções reapareceu.")
assert(all(c("n_UAs_amostradas", "n_pontos_amostrais") %in% names(esforco)),
       "Colunas corrigidas de esforço ausentes.")

ponto_col <- "ponto_amostral (amostragem/registro)"
assert(ponto_col %in% names(registros), "Fixture não contém ponto amostral.")
ponto_num <- suppressWarnings(as.numeric(registros[[ponto_col]]))
validos <- is.finite(ponto_num) & ponto_num >= 1 & ponto_num <= 101 &
  abs(ponto_num - round(ponto_num)) < 1e-8
pontos_independentes <- unique(registros[validos, .(
  UC = as.character(UC),
  UA = as.character(UA),
  ANO = as.integer(ANO),
  COLETA = as.character(COLETA),
  ponto = as.integer(round(ponto_num[validos]))
)])
assert(sum(esforco$n_pontos_amostrais) == nrow(pontos_independentes),
       "A contagem de pontos amostrais diverge da recontagem independente.")

continuidade <- fread(file.path(dir_rel, "continuidade_uas.csv"), encoding = "UTF-8")
assert(data.table::uniqueN(continuidade$UA) == data.table::uniqueN(pontos_independentes$UA),
       "Cardinalidade física de UAs diverge na continuidade.")
assert(all(continuidade$percentual_campanhas > 0 & continuidade$percentual_campanhas <= 100),
       "Percentual de continuidade fora do domínio.")

graficos_editoriais <- fread(
  file.path(dir_rel, "indice_selecao_graficos.csv"),
  encoding = "UTF-8"
)
assert(all(c(
  "id", "arquivo_relatorio", "disponivel", "n_linhas_dados_plot",
  "rotulos_editoriais"
) %in% names(graficos_editoriais)), "Índice de gráficos editoriais incompleto.")
assert(all(
  graficos_editoriais[disponivel == TRUE, n_linhas_dados_plot] > 0L
), "Um gráfico sem dados foi selecionado para os relatórios.")
assert(all(
  graficos_editoriais[disponivel == TRUE, rotulos_editoriais]
), "Um gráfico selecionado não passou pelo contrato de rótulos editoriais.")
graficos_essenciais <- c("categorias_temporal", "formas_nativas_recente")
graficos_essenciais <- c(
  graficos_essenciais,
  "herbaceas_lenhosas_cobertura", "herbaceas_lenhosas_proporcao",
  "categorias_proporcao", "formas_nativas_proporcao"
)
if (nrow(cob_exot)) graficos_essenciais <- c(graficos_essenciais, "formas_exoticas_temporal")
if (nrow(prop_exot)) graficos_essenciais <- c(graficos_essenciais, "formas_exoticas_proporcao")
if (nrow(cob_seca)) graficos_essenciais <- c(graficos_essenciais, "formas_secas_mortas_cobertura")
if (nrow(prop_seca)) graficos_essenciais <- c(graficos_essenciais, "formas_secas_mortas_proporcao")
if (nrow(cob_material)) graficos_essenciais <- c(graficos_essenciais, "material_botanico_cobertura")
if (nrow(prop_material)) graficos_essenciais <- c(graficos_essenciais, "material_botanico_proporcao")
if (data.table::uniqueN(stat$ANO) > 1L && nrow(periodo)) {
  graficos_essenciais <- c(graficos_essenciais, "mudancas_prioritarias")
}
assert(all(
  graficos_essenciais %in% graficos_editoriais[disponivel == TRUE, id]
), "Gráficos editoriais essenciais ausentes.")
assert(all(file.exists(file.path(
  dir_rel,
  graficos_editoriais[disponivel == TRUE, arquivo_relatorio]
))), "Um PNG editorial indexado está ausente.")

rotulos <- fread(file.path(dir_rel, "dicionario_rotulos_relatorio.csv"), encoding = "UTF-8")
assert(all(rotulos$valido_para_apresentacao), "Dicionário contém rótulo editorial inválido.")
formacoes_esperadas <- unique(monitora_relatorio_rotulo_formacao(
  stat$form_veg,
  inicial_maiuscula = TRUE
))
formacoes_esperadas <- formacoes_esperadas[!is.na(formacoes_esperadas) & nzchar(formacoes_esperadas)]
assert(
  all(formacoes_esperadas %in% rotulos$rotulo),
  "Uma formação presente no dataset não foi resolvida pelo dicionário editorial."
)

textos <- unlist(lapply(
  list.files(dir_rel, pattern = "\\.(Rmd|md)$", full.names = TRUE),
  readLines,
  warn = FALSE,
  encoding = "UTF-8"
))
linhas_visiveis <- textos[!grepl("`", textos, fixed = TRUE)]
for (codigo in c(
  "savanica", "reducao", "mudanca_composicao", "proporcao_relativa",
  "formas_vida_secas_mortas", "material_botanico", "categorias_gerais"
)) {
  assert(!any(grepl(
    paste0("(^|[^[:alnum:]_])", codigo, "([^[:alnum:]_]|$)"),
    linhas_visiveis,
    ignore.case = TRUE,
    perl = TRUE
  )), paste0("Código interno exposto no texto: ", codigo))
}
assert(any(grepl("Nº de pontos amostrais", textos, fixed = TRUE)),
       "Rótulo corrigido de pontos amostrais não aparece.")
assert(!any(grepl("| Transecções |", textos, fixed = TRUE)),
       "Coluna Transecções reapareceu nos documentos.")
assert(
  any(grepl("# Resumo executivo", textos, fixed = TRUE)) &&
    any(grepl("## Achados prioritários", textos, fixed = TRUE)) &&
    any(grepl("# Esforço amostral por UC, formação e ano", textos, fixed = TRUE)) &&
    !any(grepl("# Síntese executiva", textos, fixed = TRUE)) &&
    !any(grepl("## Mensagens principais", textos, fixed = TRUE)),
  "Títulos do relatório sintético não foram harmonizados com o detalhado."
)
assert(
  all(file.exists(file.path(dir_rel, c(
    "matriz_interpretacao_ecologica_e_evidencias.csv",
    "achados_hipoteses_e_linhas_de_pesquisa.csv",
    "ocorrencias_seca_morta_linha_forma_relatorio_analitico.csv",
    "resumo_seca_morta_em_revisao_relatorio_analitico.csv"
  )))),
  "Produtos editáveis do núcleo ecológico ou de seca/morta estão ausentes."
)
assert(
  any(grepl("não demonstra|não prova", textos, perl = TRUE)) &&
    any(grepl(
      "Resultado observado, hipótese compatível e evidência necessária",
      textos, fixed = TRUE
    )) &&
    any(grepl("Hipóteses, evidências e gestão", textos, fixed = TRUE)) &&
    any(grepl("Cobertura e proporção relativa", textos, fixed = TRUE)),
  "Separação entre resultado, hipótese e evidência necessária não foi materializada."
)
assert(
  any(grepl("Situação dos dados:", textos, fixed = TRUE)) &&
    any(grepl("Em validação", textos, fixed = TRUE)),
  "Situação editorial 'Em validação' ausente para a execução em homologação."
)
assert(
  !any(grepl(
    "Situação dos dados:.*QA|<span class=\"status\">QA</span>",
    textos,
    perl = TRUE
  )),
  "Código interno QA foi exposto na situação dos dados."
)

manifesto <- fread(file.path(dir_rel, "manifesto_relatorios_analiticos.csv"), encoding = "UTF-8")
valor_manifesto <- setNames(manifesto$valor, manifesto$item)
assert(as.integer(valor_manifesto[["n_pontos_amostrais"]]) == nrow(pontos_independentes),
       "Manifesto diverge na contagem de pontos.")
assert(identical(valor_manifesto[["status_validacao"]], "Em validação"),
       "Manifesto não usa o vocabulário editorial fechado para situação dos dados.")
assert(identical(valor_manifesto[["mapa_satelite_chave_persistida"]], "FALSE"),
       "Manifesto indica persistência indevida de chave.")
if (isTRUE(ativar_sentinel)) {
  status_satelite <- fread(
    file.path(dir_rel, "auditoria_mapa_satelite.csv"),
    encoding = "UTF-8"
  )
  assert(status_satelite$solicitado[[1L]],
         "A homologação exigiu Sentinel-2, mas a consulta foi registrada como não solicitada.")
  assert(status_satelite$gerado[[1L]], paste0(
    "Mapa Sentinel-2 não foi gerado: ",
    status_satelite$motivo[[1L]]
  ))
  assert(
    grepl("Sentinel-2", status_satelite$provedor[[1L]], fixed = TRUE),
    "Provedor público Sentinel-2 não foi registrado."
  )
  assert(
    !is.na(status_satelite$data_aquisicao[[1L]]) &&
      nzchar(status_satelite$data_aquisicao[[1L]]),
    "Data de aquisição Sentinel-2 ausente."
  )
  assert(
    is.finite(status_satelite$nuvens_area_pct[[1L]]),
    "Cobertura local de nuvens/sombras não foi registrada."
  )
  assert(
    is.finite(status_satelite$janela_busca_dias[[1L]]) &&
      status_satelite$janela_busca_dias[[1L]] >= 60L &&
      is.finite(status_satelite$n_janelas_consultadas[[1L]]) &&
      status_satelite$n_janelas_consultadas[[1L]] >= 1L,
    "Janela progressiva e número de consultas Sentinel-2 não foram auditados."
  )
  assert(
    !status_satelite$chave_persistida[[1L]] &&
      !status_satelite$url_credencial_persistida[[1L]],
    "A auditoria Sentinel-2 indicou credencial persistida."
  )
  assert(
    file.exists(file.path(dir_rel, "figuras", "mapa_continuidade_uas_satelite.png")),
    "PNG Sentinel-2 auditado está ausente."
  )
  status_limite <- fread(
    file.path(dir_rel, "auditoria_limite_uc_oficial.csv"),
    encoding = "UTF-8"
  )
  assert(
    status_limite$localizado[[1L]] &&
      status_limite$estados_localizados[[1L]] &&
      status_limite$biomas_localizados[[1L]] &&
      status_limite$localizador_completo[[1L]] &&
      all(c("rede", "UC", "estados", "biomas") %in%
        trimws(strsplit(status_limite$componentes_localizador[[1L]], "\\|", perl = TRUE)[[1L]])),
    paste0("Localizador cartográfico incompleto: ", status_limite$motivo[[1L]])
  )
  metadados_mgb2 <- fread(
    file.path(dir_rel, "metadados_cartograficos_mgb2.csv"),
    encoding = "UTF-8"
  )
  assert(
    all(c("responsavel", "codigo_epsg", "imagem_processamento") %in%
      metadados_mgb2$elemento_mgb2) &&
      metadados_mgb2[elemento_mgb2 == "responsavel", valor][[1L]] == "CBC/ICMBio",
    "Metadados cartográficos MGB 2.0 auxiliares estão incompletos."
  )
  assert(
    file.exists(file.path(dir_rel, "metadados_cartograficos_mgb2.json")),
    "Metadados cartográficos JSON ausentes."
  )
  candidatos_satelite <- fread(
    file.path(dir_rel, "auditoria_candidatos_mapa_satelite.csv"),
    encoding = "UTF-8"
  )
  assert(sum(candidatos_satelite$selecionada) == 1L,
         "Auditoria não identifica exatamente uma aquisição selecionada.")
  assert(
    identical(valor_manifesto[["mapa_satelite_solicitado"]], "TRUE") &&
      identical(valor_manifesto[["mapa_satelite_gerado"]], "TRUE"),
    "Manifesto não confirma solicitação e geração do mapa Sentinel-2."
  )
}

stat_multi <- rbind(stat, copy(stat[1L])[, UC := "Outra UC de QA"])
erro_multi <- tryCatch({
  monitora_relatorios_analiticos_gerar(
    registros, stat_multi, periodo, linha_base, composicao_periodo, composicao, config,
    cob_categ, cob_nat, cob_exot, dir_qa,
    formatos = c("rmd", "md"), mapa_satelite = FALSE
  )
  NULL
}, error = identity)
assert(inherits(erro_multi, "error") &&
       grepl("exatamente uma UC", conditionMessage(erro_multi), fixed = TRUE),
       "Gate de UC única não bloqueou agregação mult UC.")

destino_sat <- file.path(dir_qa, "mapa_opcao_n_nao_deve_existir.png")
resultado_sat_n <- monitora_relatorios_analiticos_gerar_mapa_satelite(
  data.table(
    long_ini = -41.01,
    lat_ini = -13.01,
    long_fin = -41.00,
    lat_fin = -13.00,
    lon_meio = -41.005,
    lat_meio = -13.005,
    classe_continuidade_label = "Amostrada em todas as campanhas",
    formacao_label = "Savânica"
  ),
  destino_sat,
  file.path(dir_qa, "cache_nao_deve_existir"),
  ativado = FALSE,
  fonte = "SENTINEL2_PUBLICO"
)
assert(
  !resultado_sat_n$status$solicitado[[1L]] &&
    !resultado_sat_n$status$gerado[[1L]] &&
    !file.exists(destino_sat) &&
    !dir.exists(file.path(dir_qa, "cache_nao_deve_existir")),
  "A opção N materializou mapa, cache ou trabalho remoto."
)

cat(
  "QA_RELATORIOS_ANALITICOS_V291_OK\n",
  "DIR_QA=", normalizePath(dir_qa, winslash = "/", mustWork = TRUE), "\n",
  "DIR_RELATORIO=", normalizePath(dir_rel, winslash = "/", mustWork = TRUE), "\n",
  "N_DOCUMENTOS=", nrow(indice), "\n",
  "N_PONTOS=", nrow(pontos_independentes), "\n",
  "N_UAS=", data.table::uniqueN(pontos_independentes$UA), "\n",
  "DURACAO_RELATORIOS_S=", round(resultado$duracao_segundos, 3L), "\n",
  sep = ""
)
