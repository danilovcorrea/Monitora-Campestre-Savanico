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
  "MONITORA_TESTE_SCRIPT_IMPORTACAO_SISMONITORA_MULTICONTEXTO",
  unset = file.path(
    repo,
    "monitora_campsav_alvo_global_v2.9.1.R"
  )
)
baseline_publica <- file.path(
  repo,
  "monitora_campsav_alvo_global_v2.9.0.R"
)
fixture <- Sys.getenv(
  "MONITORA_TESTE_MULTICONTEXTO_REGISTROS_VALIDADOS",
  unset = ""
)
qa_saida_persistente <- Sys.getenv(
  "MONITORA_TESTE_V291_XLSX_SAIDA_PERSISTENTE",
  unset = ""
)
qa_saida <- function(tag, prefix) {
  if (nzchar(qa_saida_persistente)) {
    return(file.path(qa_saida_persistente, tag))
  }
  tempfile(pattern = prefix)
}

assert(file.exists(candidato), "Script candidato multicontexto não encontrado.")
assert(file.exists(baseline_publica), "Baseline pública v2.9.0 não encontrada.")
assert(
  identical(
    digest(file = baseline_publica, algo = "sha256"),
    "d1f323ce43a5e02058a9b6ae7521245aab6fd6d1cce8d32999b4339f527e6020"
  ),
  "A baseline pública v2.9.0 foi alterada."
)
assert(
  nzchar(fixture) && file.exists(fixture),
  paste0(
    "Defina MONITORA_TESTE_MULTICONTEXTO_REGISTROS_VALIDADOS com uma fixture ",
    "autorizada que contenha pelo menos dois contextos UC + ciclo + campanha."
  )
)
invisible(parse(candidato))

linhas_script <- readLines(candidato, warn = FALSE)
inicio <- grep(
  "^monitora_importacao_sismonitora_modelo_sha256 <-",
  linhas_script
) - 1L
fim <- grep("^monitora_registros_validados_exportar <-", linhas_script) - 1L
assert(
  length(inicio) == 1L && length(fim) == 1L && fim > inicio,
  "Bloco do gerador SISMONITORA não localizado."
)

ambiente_desligado <- new.env(parent = .GlobalEnv)
ambiente_desligado$MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- FALSE
eval(parse(text = linhas_script[inicio:fim]), envir = ambiente_desligado)
assert(
  !exists(
    "monitora_planilha_importacao_sismonitora_gerar",
    envir = ambiente_desligado,
    inherits = FALSE
  ),
  "Com a opção N, o módulo XLSX não deve ser materializado."
)

ambiente <- new.env(parent = .GlobalEnv)
ambiente$MONITORA_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- TRUE
eval(parse(text = linhas_script[inicio:fim]), envir = ambiente)
linhas_cabecalho <- readLines(candidato, n = 500L, warn = FALSE, encoding = "UTF-8")
linha_versao <- grep(
  "^[[:space:]]*MONITORA_SCRIPT_VERSAO[[:space:]]*<-[[:space:]]*\"",
  linhas_cabecalho,
  value = TRUE
)[1L]
versao_candidata <- if (is.na(linha_versao) || !nzchar(linha_versao)) {
  "2.9.1"
} else {
  sub('^[^\"]*\"([^\"]+)\".*$', "\\1", linha_versao, perl = TRUE)
}
ambiente$MONITORA_SCRIPT_VERSAO <- paste0(versao_candidata, "_uuid_qa")

fonte <- fread(
  fixture,
  colClasses = "character",
  na.strings = NULL,
  check.names = FALSE,
  encoding = "UTF-8",
  showProgress = FALSE
)
chaves_contexto <- c("uc", "ciclo", "campanha")
assert(
  all(chaves_contexto %in% names(fonte)),
  "Fixture sem as chaves UC, ciclo e campanha."
)
contextos_esperados <- fonte[, .(
  n_linhas = .N,
  n_coletas = uniqueN(coleta)
), by = chaves_contexto]
setorderv(contextos_esperados, chaves_contexto)
assert(
  nrow(contextos_esperados) >= 2L,
  "Fixture deve conter pelo menos dois contextos."
)
assert(
  fonte[, .N, by = c(chaves_contexto, "coleta")][, all(N == 101L)],
  "Cada coleta da fixture deve possuir 101 pontos."
)
schema <- data.table(atributo = names(fonte))
hash_objeto_antes <- digest(fonte, algo = "sha256")

saida <- qa_saida("uuid_removido_multicontexto", "qa_v291_uuid_removido_")
dir.create(file.path(saida, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida, "logs"), recursive = TRUE)
if (!nzchar(qa_saida_persistente)) {
  on.exit(unlink(saida, recursive = TRUE, force = TRUE), add = TRUE)
}
fonte_execucao <- file.path(
  saida,
  "01_produtos_dados",
  "registros_validados.csv"
)
fwrite(
  fonte,
  fonte_execucao,
  sep = ",",
  quote = "auto",
  qmethod = "double",
  na = "",
  bom = TRUE,
  encoding = "UTF-8",
  eol = "\n"
)

ambiente$MONITORA_REGISTROS_VALIDADOS_GERADO <- TRUE
tempo_multi <- system.time(
  resultado <- ambiente$monitora_planilha_importacao_sismonitora_gerar(
    registros_validados = fonte,
    output_dir = saida,
    log_dir = file.path(saida, "logs"),
    exec_id = "qa_v290_r03_multicontexto",
    schema = schema,
    fonte_csv = fonte_execucao
  )
)

assert(
  length(resultado$produto) == nrow(contextos_esperados),
  "Não foi gerado exatamente um XLSX por contexto."
)
assert(
  all(file.exists(resultado$produto)) &&
    all(file.info(resultado$produto)$size > 0L),
  "Há XLSX contextual ausente ou vazio."
)
assert(
  !file.exists(file.path(
    saida,
    "01_produtos_dados",
    "registros_validados_importacao_sismonitora.xlsx"
  )),
  "O nome sem sufixo não deve representar fonte multicontexto."
)
assert(
  !anyDuplicated(basename(resultado$produto)) &&
    all(grepl(
      "^registros_validados_importacao_sismonitora__ctx-[0-9]{3}-[a-f0-9]{10}__uc-[a-z0-9-]+__ciclo-[a-z0-9-]+__campanha-[a-z0-9-]+\\.xlsx$",
      basename(resultado$produto)
    )),
  "Nomes multicontexto não são inequívocos, seguros ou determinísticos."
)
assert(
  identical(hash_objeto_antes, digest(fonte, algo = "sha256")),
  "A geração alterou o objeto-fonte registros_validados."
)
assert(
  unname(tempo_multi[["elapsed"]]) < 30,
  "Geração multicontexto focal excedeu 30 segundos."
)

contextos_reordenados <- ambiente$monitora_importacao_sismonitora_contextos(
  fonte[sample(.N)]
)
assert(
  identical(
    contextos_reordenados$nome_arquivo,
    basename(resultado$produto)
  ),
  "Os nomes dos produtos mudaram com a ordem das linhas de entrada."
)

manifesto <- resultado$manifesto
assert(
  nrow(manifesto) == nrow(contextos_esperados) &&
    identical(manifesto$contexto_ordem, seq_len(nrow(manifesto))) &&
    all(manifesto$n_contextos_execucao == nrow(contextos_esperados)) &&
    identical(manifesto$uc, contextos_esperados$uc) &&
    identical(manifesto$ciclo, contextos_esperados$ciclo) &&
    identical(manifesto$campanha, contextos_esperados$campanha) &&
    identical(manifesto$n_linhas_dados, contextos_esperados$n_linhas) &&
    identical(manifesto$n_coletas, contextos_esperados$n_coletas),
  "Manifesto não representa exatamente os contextos da fonte."
)
assert(
  all(manifesto$sha256_fonte == digest(file = fonte_execucao, algo = "sha256")) &&
    identical(
      manifesto$sha256_produto,
      unname(vapply(
        resultado$produto,
        function(path) digest(file = path, algo = "sha256"),
        character(1L)
      ))
    ),
  "Hashes de fonte ou produtos divergiram no manifesto."
)
assert(
  all(manifesto$n_abas == 3L) &&
    all(manifesto$n_colunas_preenchimento == 115L) &&
    all(manifesto$uuid_raiz_preenchidos_na_planilha == 0L) &&
    all(manifesto$uuid_registros_preenchidos_na_planilha == 0L),
  "Invariantes estruturais ou de UUID divergiram no manifesto."
)
assert(
  identical(
    sort(unique(resultado$auditoria$contexto_id)),
    sort(manifesto$contexto_id)
  ) &&
    nrow(resultado$auditoria) == sum(contextos_esperados$n_coletas),
  "Auditoria não cobre todos os contextos e coletas."
)

headers <- ambiente$monitora_importacao_sismonitora_headers(schema)
colunas_focais <- match(
  c(
    "uc",
    "amostragem/registro/ponto_amostral",
    "amostragem/registro/uuid",
    "uuid"
  ),
  headers
)
for (ii in seq_along(resultado$produto)) {
  arquivo <- resultado$produto[ii]
  assert(
    identical(
      getSheetNames(arquivo),
      c("Preenchimento", "Opções válidas", "Campos Comuns")
    ),
    paste0("Abas divergentes em ", basename(arquivo), ".")
  )
  planilha <- read.xlsx(
    arquivo,
    sheet = "Preenchimento",
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  assert(
    nrow(planilha) == manifesto$n_linhas_dados[ii] + 2L,
    paste0("Número de linhas divergente em ", basename(arquivo), ".")
  )
  planilha <- planilha[, colunas_focais, drop = FALSE]
  dados <- as.data.table(planilha[3:nrow(planilha), , drop = FALSE])
  setnames(
    dados,
    c(
      "uc",
      "ponto_amostral",
      "uuid_registro",
      "uuid_raiz"
    )
  )
  for (nm in names(dados)) {
    set(dados, j = nm, value = as.character(dados[[nm]]))
    dados[is.na(get(nm)), (nm) := ""]
  }
  inicio_bloco <- ((seq_len(nrow(dados)) - 1L) %% 101L) == 0L
  assert(
    sum(nzchar(dados$uc)) == manifesto$n_coletas[ii] &&
      all(dados$uc[inicio_bloco] == manifesto$uc[ii]),
    paste0("UC não está isolada por contexto em ", basename(arquivo), ".")
  )
  assert(
    all(matrix(
      as.integer(dados$ponto_amostral),
      nrow = 101L
    ) == 1:101),
    paste0("Pontos 1:101 divergiram em ", basename(arquivo), ".")
  )
  assert(
    !any(nzchar(dados$uuid_registro)) &&
      !any(nzchar(dados$uuid_raiz)),
    paste0("UUID preenchido no modo de inclusão em ", basename(arquivo), ".")
  )
}

### Compatibilidade: uma única combinação mantém o nome canônico já publicado.
contexto_unico <- fonte[
  uc == contextos_esperados$uc[nrow(contextos_esperados)] &
    ciclo == contextos_esperados$ciclo[nrow(contextos_esperados)] &
    campanha == contextos_esperados$campanha[nrow(contextos_esperados)]
]
saida_unica <- qa_saida("uuid_removido_contexto_unico", "qa_v291_contexto_unico_")
dir.create(file.path(saida_unica, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida_unica, "logs"), recursive = TRUE)
if (!nzchar(qa_saida_persistente)) {
  on.exit(unlink(saida_unica, recursive = TRUE, force = TRUE), add = TRUE)
}
fonte_unica_csv <- file.path(
  saida_unica,
  "01_produtos_dados",
  "registros_validados.csv"
)
fwrite(contexto_unico, fonte_unica_csv, na = "", bom = TRUE, encoding = "UTF-8")
tempo_unico <- system.time(
resultado_unico <- ambiente$monitora_planilha_importacao_sismonitora_gerar(
    registros_validados = contexto_unico,
    output_dir = saida_unica,
    log_dir = file.path(saida_unica, "logs"),
    exec_id = "qa_v290_r03_contexto_unico",
    schema = schema,
    fonte_csv = fonte_unica_csv,
    remover_uuid = TRUE
  )
)
assert(
  length(resultado_unico$produto) == 1L &&
    identical(
      basename(resultado_unico$produto),
      "registros_validados_importacao_sismonitora.xlsx"
    ) &&
    unname(tempo_unico[["elapsed"]]) < 12,
  "Compatibilidade ou performance do contexto único divergiu."
)

### Modo opcional: preservar os UUIDs exatamente como estão na fonte.
saida_preserva <- qa_saida("uuid_preservado_contexto_unico", "qa_v291_uuid_preservado_")
dir.create(file.path(saida_preserva, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida_preserva, "logs"), recursive = TRUE)
if (!nzchar(qa_saida_persistente)) {
  on.exit(unlink(saida_preserva, recursive = TRUE, force = TRUE), add = TRUE)
}
fonte_preserva_csv <- file.path(
  saida_preserva,
  "01_produtos_dados",
  "registros_validados.csv"
)
fwrite(contexto_unico, fonte_preserva_csv, na = "", bom = TRUE, encoding = "UTF-8")
hash_preserva_antes <- digest(contexto_unico, algo = "sha256")
resultado_preserva <- ambiente$monitora_planilha_importacao_sismonitora_gerar(
  registros_validados = contexto_unico,
  output_dir = saida_preserva,
  log_dir = file.path(saida_preserva, "logs"),
  exec_id = "qa_v291_uuid_preservado",
  schema = schema,
  fonte_csv = fonte_preserva_csv,
  remover_uuid = FALSE
)
assert(
  identical(hash_preserva_antes, digest(contexto_unico, algo = "sha256")),
  "O modo de preservação alterou a fonte em memória."
)
assert(
  identical(resultado_preserva$manifesto$politica_uuid_aplicada, "preservar") &&
    identical(resultado_preserva$manifesto$opcao_remover_uuid, "N") &&
    all(resultado_preserva$manifesto$uuid_registros_preenchidos_na_planilha > 0L),
  "Manifesto não documentou a preservação dos UUIDs."
)
planilha_preserva <- read.xlsx(
  resultado_preserva$produto,
  sheet = "Preenchimento",
  colNames = FALSE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE
)
col_uuid <- match(c("amostragem/registro/uuid", "uuid"), headers)
uuid_planilha <- as.data.table(
  planilha_preserva[3:nrow(planilha_preserva), col_uuid, drop = FALSE]
)
setnames(uuid_planilha, c("uuid_registro", "uuid_raiz"))
for (nm in names(uuid_planilha)) {
  set(uuid_planilha, j = nm, value = as.character(uuid_planilha[[nm]]))
  uuid_planilha[is.na(get(nm)), (nm) := ""]
}
coletas_ordem <- resultado_preserva$auditoria$coleta
esperado_registro <- unlist(lapply(coletas_ordem, function(id) {
  bloco <- contexto_unico[coleta == id]
  ponto <- as.integer(bloco[["amostragem/registro/ponto_amostral"]])
  as.character(bloco[order(ponto)][["amostragem/registro/uuid"]])
}), use.names = FALSE)
esperado_raiz <- unlist(lapply(coletas_ordem, function(id) {
  valor <- unique(as.character(contexto_unico[coleta == id]$uuid))
  c(valor[1L], rep("", 100L))
}), use.names = FALSE)
assert(
  identical(uuid_planilha$uuid_registro, esperado_registro) &&
    identical(uuid_planilha$uuid_raiz, esperado_raiz),
  "UUIDs da planilha não coincidem exatamente com a fonte."
)

### O preparador continua aceitando apenas um contexto por arquivo.
coleta_contexto_1 <- fonte[
  uc == contextos_esperados$uc[1L] &
    ciclo == contextos_esperados$ciclo[1L] &
    campanha == contextos_esperados$campanha[1L],
  unique(coleta)[1L]
]
coleta_contexto_2 <- fonte[
  uc == contextos_esperados$uc[2L] &
    ciclo == contextos_esperados$ciclo[2L] &
    campanha == contextos_esperados$campanha[2L],
  unique(coleta)[1L]
]
duas_coletas <- rbindlist(list(
  fonte[
    uc == contextos_esperados$uc[1L] &
      ciclo == contextos_esperados$ciclo[1L] &
      campanha == contextos_esperados$campanha[1L] &
      coleta == coleta_contexto_1
  ],
  fonte[
    uc == contextos_esperados$uc[2L] &
      ciclo == contextos_esperados$ciclo[2L] &
      campanha == contextos_esperados$campanha[2L] &
      coleta == coleta_contexto_2
  ]
), use.names = TRUE, fill = TRUE)
erro_preparador <- capturar_erro(
  ambiente$monitora_importacao_sismonitora_preparar(duas_coletas, schema)
)
assert(
  grepl("único contexto|mais de um valor", erro_preparador),
  "Gate interno de um contexto por XLSX deixou de bloquear mistura."
)

### Falha em qualquer partição não pode publicar as anteriores.
contextos_minimos <- rbindlist(list(
  fonte[
    uc == contextos_esperados$uc[1L] &
      ciclo == contextos_esperados$ciclo[1L] &
      campanha == contextos_esperados$campanha[1L]
  ][coleta == coleta_contexto_1],
  fonte[
    uc == contextos_esperados$uc[2L] &
      ciclo == contextos_esperados$ciclo[2L] &
      campanha == contextos_esperados$campanha[2L]
  ][coleta == coleta_contexto_2][-1L]
), use.names = TRUE, fill = TRUE)
saida_falha <- tempfile(pattern = "qa_v290_r03_transacao_")
dir.create(file.path(saida_falha, "01_produtos_dados"), recursive = TRUE)
dir.create(file.path(saida_falha, "logs"), recursive = TRUE)
on.exit(unlink(saida_falha, recursive = TRUE, force = TRUE), add = TRUE)
fonte_falha_csv <- file.path(
  saida_falha,
  "01_produtos_dados",
  "registros_validados.csv"
)
fwrite(contextos_minimos, fonte_falha_csv, na = "", bom = TRUE, encoding = "UTF-8")
erro_transacao <- capturar_erro(
  ambiente$monitora_planilha_importacao_sismonitora_gerar(
    registros_validados = contextos_minimos,
    output_dir = saida_falha,
    log_dir = file.path(saida_falha, "logs"),
    exec_id = "qa_v290_r03_transacao",
    schema = schema,
    fonte_csv = fonte_falha_csv
  )
)
assert(
  grepl("exatamente 101", erro_transacao) &&
    !length(list.files(
      file.path(saida_falha, "01_produtos_dados"),
      pattern = "importacao_sismonitora.*[.]xlsx$",
      full.names = TRUE
    )),
  "Falha em um contexto publicou produto parcial."
)

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
    'MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "S"',
    linhas_script,
    fixed = TRUE
  )),
  "A política de UUID deve permanecer segura por padrão (remoção no XLSX)."
)

cat(
  "PASS v", versao_candidata, " importação SISMONITORA UUID configurável;",
  paste0("contextos=", nrow(contextos_esperados), ";"),
  paste0("coletas=", sum(contextos_esperados$n_coletas), ";"),
  paste0("linhas=", sum(contextos_esperados$n_linhas), ";"),
  sprintf("tempo_multi=%.3fs;", unname(tempo_multi[["elapsed"]])),
  sprintf("tempo_unico=%.3fs\n", unname(tempo_unico[["elapsed"]]))
)
