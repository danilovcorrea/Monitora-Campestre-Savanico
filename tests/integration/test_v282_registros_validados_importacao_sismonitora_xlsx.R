#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(openxlsx)
})

assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

capturar_erro <- function(expr) {
  tryCatch({
    force(expr)
    ""
  }, error = conditionMessage)
}

repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
candidato <- Sys.getenv(
  "MONITORA_TESTE_SCRIPT_IMPORTACAO_SISMONITORA",
  unset = file.path(repo, "monitora_campsav_alvo_global_v2.8.2.R")
)
baseline <- file.path(repo, "monitora_campsav_alvo_global_v2.8.1.R")
fixture <- Sys.getenv(
  "MONITORA_TESTE_FNCS_2026_REGISTROS_VALIDADOS",
  unset = ""
)

assert(file.exists(candidato), "Script sob teste da importação SISMONITORA não encontrado.")
assert(file.exists(baseline), "Baseline pública v2.8.1 não encontrada.")
assert(
  nzchar(fixture) && file.exists(fixture),
  paste0(
    "Defina MONITORA_TESTE_FNCS_2026_REGISTROS_VALIDADOS com o caminho da ",
    "fixture autorizada de registros_validados FNCS."
  )
)
invisible(parse(candidato))
assert(
  identical(
    digest(file = baseline, algo = "sha256"),
    "ef8d6db8fd45af9746b015bdbaa6d865b133f69da56aea80562d6c5a490031ba"
  ),
  "Baseline pública v2.8.1 foi alterada."
)

linhas_script <- readLines(candidato, warn = FALSE)
inicio <- grep("^monitora_importacao_sismonitora_modelo_sha256 <-", linhas_script) - 1L
fim <- grep("^monitora_registros_validados_exportar <-", linhas_script) - 1L
assert(length(inicio) == 1L && length(fim) == 1L && fim > inicio, "Bloco do gerador não localizado.")

ambiente_desligado <- new.env(parent = .GlobalEnv)
ambiente_desligado$MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- FALSE
eval(parse(text = linhas_script[inicio:fim]), envir = ambiente_desligado)
assert(
  !exists(
    "monitora_planilha_importacao_sismonitora_gerar",
    envir = ambiente_desligado,
    inherits = FALSE
  ),
  "Com a opção N, o módulo XLSX não deve ser materializado em memória."
)

ambiente <- new.env(parent = .GlobalEnv)
ambiente$MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- TRUE
eval(parse(text = linhas_script[inicio:fim]), envir = ambiente)
ambiente$MONITORA_SCRIPT_VERSAO <- "2.8.2"

fonte <- fread(
  fixture,
  colClasses = "character",
  na.strings = NULL,
  check.names = FALSE,
  encoding = "UTF-8",
  showProgress = FALSE
)
fncs_2026 <- fonte[
  uc == "Floresta Nacional de Contendas do Sincorá" &
    ciclo == "Ciclo-2026-VgCS" &
    campanha == "Campanha-2026-VgCS"
]
assert(nrow(fncs_2026) == 5858L, "Fixture FNCS 2026 deve conter 5.858 linhas.")
assert(uniqueN(fncs_2026$coleta) == 58L, "Fixture FNCS 2026 deve conter 58 coletas.")
assert(
  fncs_2026[, .N, by = coleta][, all(N == 101L)],
  "Cada coleta FNCS 2026 deve conter 101 registros."
)
uuid_fonte_antes <- fncs_2026[, .(
  coleta_uuid,
  uuid,
  uuid_registro = get("amostragem/registro/uuid")
)]
assert(
  sum(nzchar(uuid_fonte_antes$coleta_uuid)) == 5858L &&
    uniqueN(uuid_fonte_antes$coleta_uuid) == 58L &&
    sum(nzchar(uuid_fonte_antes$uuid)) == 5858L &&
    uniqueN(uuid_fonte_antes$uuid) == 58L &&
    sum(nzchar(uuid_fonte_antes$uuid_registro)) == 5858L &&
    uniqueN(uuid_fonte_antes$uuid_registro) == 5858L,
  "Fixture não contém a linhagem UUID esperada para provar a preservação da fonte."
)
schema <- data.table(atributo = names(fncs_2026))

saida <- tempfile(pattern = "qa_v282_fncs2026_xlsx_")
dir.create(file.path(saida, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida, "logs"), recursive = TRUE)
on.exit(unlink(saida, recursive = TRUE, force = TRUE), add = TRUE)
fonte_execucao <- file.path(saida, "01_produtos_dados", "registros_validados.csv")
fwrite(
  fncs_2026,
  fonte_execucao,
  sep = ",",
  quote = "auto",
  qmethod = "double",
  na = "",
  bom = TRUE,
  encoding = "UTF-8",
  eol = "\n"
)

ambiente$MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
erro_fonte_antiga <- capturar_erro(
  ambiente$monitora_planilha_importacao_sismonitora_gerar(
    registros_validados = fncs_2026,
    output_dir = saida,
    log_dir = file.path(saida, "logs"),
    exec_id = "qa_fonte_antiga",
    schema = schema,
    fonte_csv = fonte_execucao
  )
)
assert(
  grepl("execução corrente|preexistente", erro_fonte_antiga),
  "Gate contra registros_validados.csv preexistente não bloqueou."
)

ambiente$MONITORA_REGISTROS_VALIDADOS_GERADO <- TRUE
tempo <- system.time(
  resultado <- ambiente$monitora_planilha_importacao_sismonitora_gerar(
    registros_validados = fncs_2026,
    output_dir = saida,
    log_dir = file.path(saida, "logs"),
    exec_id = "qa_v282_fncs2026_xlsx",
    schema = schema,
    fonte_csv = fonte_execucao
  )
)
assert(unname(tempo[["elapsed"]]) < 10, "Geração focal do XLSX excedeu 10 segundos.")
arquivo <- resultado$produto
assert(file.exists(arquivo) && file.info(arquivo)$size > 0L, "XLSX focal não foi gerado.")
assert(
  identical(basename(arquivo), "registros_validados_importacao_sismonitora.xlsx"),
  "Nome canônico do produto divergente."
)
assert(
  identical(
    normalizePath(arquivo, winslash = "/", mustWork = TRUE),
    normalizePath(
      file.path(saida, "01_produtos_dados", "registros_validados_importacao_sismonitora.xlsx"),
      winslash = "/",
      mustWork = TRUE
    )
  ),
  "Produto foi gravado fora do diretório canônico."
)

abas <- getSheetNames(arquivo)
assert(
  identical(abas, c("Preenchimento", "Opções válidas", "Campos Comuns")),
  "As três abas do modelo 21FEV25 não foram preservadas na ordem original."
)
headers <- ambiente$monitora_importacao_sismonitora_headers(schema)
planilha <- read.xlsx(
  arquivo,
  sheet = "Preenchimento",
  colNames = FALSE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE,
  check.names = FALSE
)
assert(nrow(planilha) == 5860L && ncol(planilha) == 115L, "Dimensão física da aba Preenchimento incorreta.")
headers_lidos <- as.character(unlist(planilha[1L, ], use.names = FALSE))
divergencias_header <- which(headers_lidos != headers)
assert(
  identical(headers_lidos, headers),
  paste0(
    "Cabeçalho de 115 colunas divergente; posições=",
    paste(utils::head(divergencias_header, 10L), collapse = ","),
    "; esperado=", paste(utils::head(headers[divergencias_header], 3L), collapse = " | "),
    "; obtido=", paste(utils::head(headers_lidos[divergencias_header], 3L), collapse = " | ")
  )
)
assert(identical(headers_lidos[2L], "uc"), "A coluna obrigatória uc não está na segunda posição.")
assert(
  all(as.character(unlist(planilha[2L, ], use.names = FALSE)) == "Label não informado"),
  "Linha de rótulos do modelo não foi preservada."
)

preenchimento <- as.data.table(planilha[3:nrow(planilha), , drop = FALSE])
setnames(preenchimento, headers)
for (nm in names(preenchimento)) {
  set(preenchimento, j = nm, value = as.character(preenchimento[[nm]]))
  preenchimento[is.na(get(nm)), (nm) := ""]
}
assert(
  nrow(preenchimento) == 5858L && ncol(preenchimento) == 115L,
  "Dimensão dos dados da planilha focal incorreta."
)

opcoes <- read.xlsx(
  arquivo,
  sheet = "Opções válidas",
  colNames = FALSE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE,
  rows = 1:4
)
esperado_opcoes <- c(
  "Coluna: opções (separadas por vírgula)",
  paste0("uc: ", unique(fncs_2026$uc), ", "),
  paste0("estacao_amostral: ", paste(sort(unique(fncs_2026$ea)), collapse = ", "), ", "),
  paste0("unidade_amostral: ", paste(sort(unique(fncs_2026$ua)), collapse = ", "), ", ")
)
assert(
  identical(as.character(opcoes[[1L]]), esperado_opcoes),
  "UC, estações ou unidades amostrais não foram materializadas em Opções válidas."
)

inicio_bloco <- ((seq_len(nrow(preenchimento)) - 1L) %% 101L) == 0L
registro_headers <- grep("^amostragem/registro/", headers, value = TRUE)
comuns <- setdiff(headers, c(registro_headers, "coletor/cpf", "coletor/nome"))
assert(
  all(vapply(comuns, function(nm) {
    idx <- which(nzchar(trimws(preenchimento[[nm]])))
    all(inicio_bloco[idx])
  }, logical(1L))),
  "Há campo comum repetido fora da primeira linha de bloco."
)
assert(
  sum(nzchar(preenchimento$uc)) == 58L &&
    all(preenchimento$uc[inicio_bloco] == "Floresta Nacional de Contendas do Sincorá"),
  "A coluna uc deve ocorrer uma vez e com valor exato em cada coleta."
)
assert(
  !any(nzchar(preenchimento$observacoes_gerais)),
  "observacoes_gerais deve ser omitido da planilha enquanto o importador não suportar XPath regex."
)
observacoes_fonte <- sort(unique(
  fncs_2026$observacoes_gerais[nzchar(trimws(fncs_2026$observacoes_gerais))]
))
observacoes_auditoria <- sort(unique(
  resultado$auditoria$observacoes_gerais_original[
    resultado$auditoria$observacoes_gerais_omitida_importacao
  ]
))
assert(
  identical(observacoes_auditoria, observacoes_fonte),
  "A auditoria não preservou integralmente observacoes_gerais omitido da carga."
)
assert(
  !any(nzchar(preenchimento$uuid)) &&
    !any(nzchar(preenchimento[["amostragem/registro/uuid"]])),
  "No modo de inclusão, as duas colunas UUID devem existir e ter todas as células de dados vazias."
)
assert(
  identical(
    fncs_2026[, .(
      coleta_uuid,
      uuid,
      uuid_registro = get("amostragem/registro/uuid")
    )],
    uuid_fonte_antes
  ),
  "A geração da planilha alterou UUIDs no objeto-fonte registros_validados."
)
assert(
  all(matrix(
    as.integer(preenchimento[["amostragem/registro/ponto_amostral"]]),
    nrow = 101L
  ) == 1:101),
  "Ordem dos pontos 1:101 não foi preservada em todos os blocos."
)
horas <- preenchimento[["data_hora/hora"]][nzchar(preenchimento[["data_hora/hora"]])]
assert(
  length(horas) == 58L &&
    all(grepl("^[0-2][0-9]-[0-5][0-9]-[0-5][0-9]$", horas)),
  "Horas não estão em HH-MM-SS."
)
cpfs <- preenchimento[["coletor/cpf"]][nzchar(preenchimento[["coletor/cpf"]])]
assert(all(grepl("^[0-9]{11}$", cpfs)), "CPF perdeu o contrato textual de 11 dígitos.")
assert(any(substr(cpfs, 1L, 1L) == "0"), "Fixture não comprovou preservação de zero inicial em CPF.")

modelo_temporario <- tempfile(fileext = ".xlsx")
ambiente$monitora_importacao_sismonitora_modelo_materializar(modelo_temporario)
dir_modelo <- tempfile(pattern = "qa_modelo_")
dir_produto <- tempfile(pattern = "qa_produto_")
dir.create(dir_modelo)
dir.create(dir_produto)
on.exit(unlink(c(modelo_temporario, dir_modelo, dir_produto), recursive = TRUE, force = TRUE), add = TRUE)
componentes_intocados <- c(
  "[Content_Types].xml",
  "xl/workbook.xml",
  "xl/styles.xml",
  "xl/sharedStrings.xml",
  "xl/worksheets/sheet3.xml"
)
utils::unzip(modelo_temporario, files = componentes_intocados, exdir = dir_modelo)
utils::unzip(arquivo, files = componentes_intocados, exdir = dir_produto)
assert(
  all(vapply(componentes_intocados, function(rel) {
    identical(
      digest(file = file.path(dir_modelo, rel), algo = "sha256"),
      digest(file = file.path(dir_produto, rel), algo = "sha256")
    )
  }, logical(1L))),
  "Componentes congelados do modelo XLSX foram alterados."
)

manifesto <- resultado$manifesto
assert(
  manifesto$n_abas == 3L &&
    manifesto$n_coletas == 58L &&
    manifesto$n_linhas_dados == 5858L &&
    manifesto$n_linhas_fisicas_preenchimento == 5860L &&
    manifesto$n_colunas_preenchimento == 115L &&
    isTRUE(manifesto$coluna_uc_incluida) &&
    isTRUE(manifesto$opcoes_contextuais_preenchidas) &&
    manifesto$n_observacoes_gerais_omitidas_importacao ==
      sum(resultado$auditoria$observacoes_gerais_omitida_importacao) &&
    isTRUE(manifesto$observacoes_gerais_preservadas_na_auditoria) &&
    identical(manifesto$modo, "inclusao_registros_novos") &&
    isTRUE(manifesto$colunas_uuid_mantidas) &&
    manifesto$coleta_uuid_preenchidos_na_fonte == 58L &&
    manifesto$uuid_raiz_preenchidos_na_fonte == 58L &&
    manifesto$uuid_registros_preenchidos_na_fonte == 5858L &&
    manifesto$uuid_raiz_preenchidos_na_planilha == 0L &&
    manifesto$uuid_registros_preenchidos_na_planilha == 0L &&
    isTRUE(manifesto$uuid_fonte_preservados_registros_validados) &&
    identical(
      manifesto$status,
      "aprovado_estruturalmente_para_homologacao_modo_inclusao"
    ),
  "Manifesto focal divergente."
)
assert(
  identical(manifesto$sha256_fonte, digest(file = fonte_execucao, algo = "sha256")) &&
    identical(manifesto$sha256_produto, digest(file = arquivo, algo = "sha256")),
  "Hashes do manifesto não correspondem aos arquivos."
)
assert(
  identical(
    manifesto$sha256_modelo_referencia,
    "c7a9914d539cf09fd256ad765447c6983e5102e5fad81502b34a7ee66273a956"
  ),
  "Referência criptográfica do modelo 21FEV25 divergiu."
)

coleta_teste <- fncs_2026[coleta == fncs_2026$coleta[1L]]
erro_100 <- capturar_erro(ambiente$monitora_importacao_sismonitora_preparar(coleta_teste[-1L], schema))
assert(grepl("exatamente 101", erro_100), "Gate de cardinalidade 101 não bloqueou.")

conflito_comum <- copy(coleta_teste)
conflito_comum[2L, form_veg := paste0(form_veg[1L], "_conflito")]
erro_comum <- capturar_erro(
  ambiente$monitora_importacao_sismonitora_preparar(conflito_comum, schema)
)
assert(grepl("mais de um valor", erro_comum), "Gate de campo comum ambíguo não bloqueou.")

uuid_vazio <- copy(coleta_teste)
uuid_vazio[, coleta_uuid := ""]
uuid_vazio[, uuid := ""]
uuid_vazio[, `amostragem/registro/uuid` := ""]
preparado_sem_uuid_fonte <- ambiente$monitora_importacao_sismonitora_preparar(
  uuid_vazio,
  schema
)
assert(
  !any(nzchar(preparado_sem_uuid_fonte$dados$uuid)) &&
    !any(nzchar(preparado_sem_uuid_fonte$dados[["amostragem/registro/uuid"]])) &&
    preparado_sem_uuid_fonte$auditoria$coleta_uuid_fonte == "" &&
    preparado_sem_uuid_fonte$auditoria$uuid_raiz_fonte == "" &&
    preparado_sem_uuid_fonte$auditoria$n_uuid_registro_preenchidos_fonte == 0L &&
    preparado_sem_uuid_fonte$auditoria$n_uuid_registro_omitidos_importacao == 0L,
  "Entrada equivalente a formulário de papel, sem UUIDs, não foi aceita corretamente."
)

outro_contexto <- fonte[
  uc == "Floresta Nacional de Contendas do Sincorá" &
    (ciclo != "Ciclo-2026-VgCS" | campanha != "Campanha-2026-VgCS")
][1L]
assert(nrow(outro_contexto) == 1L, "Fixture não contém segundo contexto para o gate adversarial.")
erro_contexto <- capturar_erro(
  ambiente$monitora_importacao_sismonitora_preparar(
    rbindlist(list(coleta_teste, outro_contexto), use.names = TRUE, fill = TRUE),
    schema
  )
)
assert(grepl("único contexto|mais de um valor", erro_contexto), "Gate de contexto único não bloqueou.")

assert(
  any(grepl(
    'MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- "N"',
    linhas_script,
    fixed = TRUE
  )),
  "Produto deve permanecer desligado por padrão."
)
assert(
  any(grepl(
    "if (isTRUE(MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA))",
    linhas_script,
    fixed = TRUE
  )),
  "A geração não está isolada por um gate booleano barato."
)
assert(
  any(grepl(
    "A geração de registros_validados_importacao_sismonitora.xlsx exige MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS",
    linhas_script,
    fixed = TRUE
  )),
  "Dependência de configuração em registros_validados.csv não está explícita."
)

cat(
  "PASS v2.8.2 registros_validados_importacao_sismonitora.xlsx FNCS 2026;",
  "coletas=58; linhas_dados=5858; colunas=115; abas=3; uc=incluida;",
  "modo=inclusao; uuid_planilha=0;",
  sprintf("tempo=%.3fs;", unname(tempo[["elapsed"]])),
  paste0("sha256=", manifesto$sha256_produto, "\n")
)
