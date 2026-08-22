#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.16.R",
  mustWork = TRUE
)
baseline <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else "monitora_campsav_alvo_global_v2.9.15.R",
  mustWork = TRUE
)
exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

linhas <- readLines(script, warn = FALSE, encoding = "UTF-8")
base_linhas <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")
arvore <- parse(file = script, keep.source = FALSE)
exigir(
  length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)),
  "A candidata deixou de ser uma única expressão externa base::evalq."
)

inicio <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  exigir(!is.na(a) && !is.na(b), "Bloco inicial não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
exigir(
  identical(inicio(linhas), inicio(base_linhas)),
  "A seção congelada do RStudio anterior às variáveis manuais divergiu da v2.9.15."
)

for (trecho in c(
  'MONITORA_SCRIPT_VERSAO <- "2.9.16"',
  'MONITORA_SCRIPT_BUILD_ID <- "v2.9.16-20260821-r02"',
  'monitora_correcao_validar_ua_contratual <- function',
  '"^UA-(00[1-9]|0[1-9][0-9]|[1-9][0-9]{2})_VgCS$"',
  'return("texto_ua_contratual")',
  'placeholder = "UA-004_VgCS"',
  'monitora_incorporacao_publicar_diagnostico_filho <- function',
  'monitora_incorporacao_auditoria_completude_filho <- function',
  'wrapper_child <- file.path(dir_tmp, "preparar_novas_coletas.R")',
  '"do.call(Sys.setenv, as.list(monitora_child_env))"',
  'pattern = "\\\\.xlsx?$"',
  'monitora_correcao_operacao_toca_desconhecida <- function',
  '"acrescentar_texto"',
  'monitora_relatorios_analiticos_docx_auditar_figuras <- function',
  'auditoria_incorporacao_paineis_evidencia_relatorio.csv',
  'monitora_relatorios_analiticos_materializar_capa_docx <- function',
  'monitora_relatorios_analiticos_mosaico_previews <- function',
  'preview_multitemporal',
  'cache_persistente_reutilizado',
  'monitora_linhagem_inventario_sessoes_importar <- function',
  'inventario_sessoes_sha256',
  'session_inventory_count',
  'ordem_sessao_herdada_aux'
)) exigir(grepl(trecho, texto, fixed = TRUE), paste0("Revisão focal ausente: ", trecho))

inicio_incorporacao <- grep(
  '^monitora_incorporacao_preparar_checkpoint_isolado <- function', linhas
)[1L]
fim_incorporacao <- grep(
  '^monitora_incorporacao_novas_coletas_executar <- function', linhas
)[1L]
exigir(
  is.finite(inicio_incorporacao) && is.finite(fim_incorporacao) &&
    fim_incorporacao > inicio_incorporacao,
  "Bloco de preparação isolada não localizado."
)
bloco_incorporacao <- paste(
  linhas[inicio_incorporacao:(fim_incorporacao - 1L)], collapse = "\n"
)
exigir(
  !grepl("env = env_child", bloco_incorporacao, fixed = TRUE),
  "A chamada incompatível system2(..., env=env_child) permaneceu."
)

alvos <- c(
  "monitora_correcao_acao_normalizar",
  "monitora_correcao_hash_texto",
  "monitora_correcao_atributo_eh_ua",
  "monitora_correcao_validar_ua_contratual",
  "monitora_correcao_validar_formato_valor",
  "monitora_incorporacao_coluna",
  "monitora_incorporacao_chave_coleta",
  "monitora_incorporacao_ponto_numero",
  "monitora_incorporacao_uuid_registro",
  "monitora_incorporacao_colunas_fato",
  "monitora_incorporacao_fingerprint",
  "monitora_incorporacao_classificar",
  "monitora_incorporacao_auditoria_completude_filho",
  "monitora_incorporacao_novas_coletas_executar"
)
env <- new.env(parent = globalenv())
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      as.character(x[[2L]]) %in% alvos && is.call(x[[3L]]) &&
      identical(as.character(x[[3L]][[1L]])[1L], "function")) {
    eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
exigir(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)),
       "Nem todas as funções focais foram carregadas.")

validas <- c("UA-001_VgCS", "UA-004_VgCS", "UA-010_VgCS", "UA-099_VgCS", "UA-100_VgCS", "UA-999_VgCS")
invalidas <- c("", "UA-000_VgCS", "UA-1000_VgCS", "UA-04_VgCS", "004", "ua-004_vgcs", "UA-004", " UA-004_VgCS")
exigir(all(vapply(validas, function(x) env$monitora_correcao_validar_ua_contratual(x)$ok, logical(1L))),
       "Ao menos uma UA válida foi rejeitada.")
exigir(!any(vapply(invalidas, function(x) env$monitora_correcao_validar_ua_contratual(x)$ok, logical(1L))),
       "Ao menos uma UA fora do contrato foi aceita.")
exigir(env$monitora_correcao_atributo_eh_ua(c("UA", "unidade_amostral")),
       "O atributo UA não foi reconhecido.")
exigir(!env$monitora_correcao_atributo_eh_ua(c("UC", "Qual a UA?")),
       "O reconhecimento de UA produziu falso positivo.")

### O valor novo não pertence ao domínio observado; deve ser aceito pelo padrão
### contratual e não pelo fallback do dataset.
ua_nova <- env$monitora_correcao_validar_formato_valor(
  "UA-004_VgCS", "select_one", "update", "unidade_amostral",
  meta_xls = NULL, escolhas_fallback = "UA-019_VgCS", atributo = "UA"
)
exigir(isTRUE(ua_nova$ok), "UA contratual ainda foi restringida ao domínio observado.")
ua_invalida <- env$monitora_correcao_validar_formato_valor(
  "UA-1000_VgCS", "select_one", "update", "unidade_amostral",
  meta_xls = NULL, escolhas_fallback = "UA-019_VgCS", atributo = "UA"
)
exigir(!isTRUE(ua_invalida$ok) && identical(ua_invalida$status, "bloqueada_formato_ua"),
       "Formato inválido de UA não foi bloqueado pelo servidor.")

### Leitura da auditoria de completude que o processo filho deixa antes de o
### diretório temporário ser removido.
tmp <- tempfile("v2916_completude_")
dir.create(file.path(tmp, "output", "03_auditorias", "completude"), recursive = TRUE)
aud <- data.table(
  COLETA = c("1", "2"), completa_101 = c("TRUE", "FALSE"),
  pontos_ausentes = c("", "89")
)
fwrite(
  aud,
  file.path(
    tmp, "output", "03_auditorias", "completude",
    "auditoria_completude_101_pontos_por_coleta_pre_painel.csv"
  )
)
lida <- env$monitora_incorporacao_auditoria_completude_filho(tmp)
unlink(tmp, recursive = TRUE, force = TRUE)
exigir(nrow(lida) == 2L && identical(as.character(lida$pontos_ausentes[[2L]]), "89"),
       "Auditoria de completude do filho não foi recuperada.")

### Atomicidade e idempotência: uma COLETA completa entra uma vez; a mesma
### entrada materializada é ignorada; divergências e incompletude bloqueiam.
ponto_col <- "ponto_amostral (amostragem/registro)"
base_teste <- data.table(
  UC = "UC teste", CICLO = "Ciclo-2026_VgCS",
  CAMPANHA = "Campanha-2026_VgCS", COLETA = "1",
  UA = "UA-001_VgCS", valor_factual = sprintf("v%03d", 1:101)
)
base_teste[, (ponto_col) := as.character(1:101)]
base_teste[, MONITORA_UUID_REGISTRO_CANONICO := sprintf(
  "11111111-1111-4111-8111-%012d", 1:101
)]

duplicata <- env$monitora_incorporacao_classificar(base_teste, copy(base_teste))
exigir(
  nrow(duplicata$conflitos) == 0L && nrow(duplicata$novas) == 0L &&
    identical(duplicata$resumo$classificacao[[1L]], "duplicata_identica_ignorada"),
  "A reexecução idêntica não foi tratada de modo idempotente."
)

nova_teste <- copy(base_teste)
nova_teste[, `:=`(
  COLETA = "2", UA = "UA-002_VgCS",
  MONITORA_UUID_REGISTRO_CANONICO = sprintf(
    "22222222-2222-4222-8222-%012d", 1:101
  )
)]
nova <- env$monitora_incorporacao_classificar(base_teste, nova_teste)
exigir(
  nrow(nova$conflitos) == 0L && nrow(nova$novas) == 101L &&
    identical(nova$resumo$classificacao[[1L]], "nova_coleta_completa"),
  "Uma COLETA integralmente nova e completa não foi liberada."
)

base_pos <- rbindlist(list(base_teste, nova_teste), use.names = TRUE, fill = TRUE)
repetida <- env$monitora_incorporacao_classificar(base_pos, nova_teste)
exigir(
  nrow(repetida$conflitos) == 0L && nrow(repetida$novas) == 0L &&
    identical(repetida$resumo$classificacao[[1L]], "duplicata_identica_ignorada"),
  "A COLETA já incorporada seria anexada novamente."
)

divergente <- copy(nova_teste)
divergente[1L, valor_factual := "valor_divergente"]
conflito <- env$monitora_incorporacao_classificar(base_pos, divergente)
exigir(
  nrow(conflito$conflitos) == 1L && nrow(conflito$novas) == 0L &&
    identical(conflito$resumo$classificacao[[1L]], "conflito_contexto_sobreposto"),
  "Divergência factual em contexto já materializado não foi bloqueada."
)

incompleta <- env$monitora_incorporacao_classificar(base_teste, nova_teste[-89L])
exigir(
  nrow(incompleta$conflitos) == 1L && nrow(incompleta$novas) == 0L &&
    identical(incompleta$resumo$classificacao[[1L]], "conflito_coleta_incompleta") &&
    grepl("89", incompleta$resumo$pontos_ausentes[[1L]], fixed = TRUE),
  "COLETA incompleta não foi bloqueada atomicamente com o ponto ausente."
)

### Caminho rápido: quando a opção está em N, a função deve retornar antes de
### qualquer varredura, subprocesso ou escrita.
env$MONITORA_INCORPORAR_NOVAS_COLETAS <- FALSE
sentinela <- data.table(x = 1:3)
retorno <- env$monitora_incorporacao_novas_coletas_executar(sentinela)
exigir(identical(retorno, sentinela), "Caminho rápido N alterou os dados.")

exigir(!grepl("2.9.16-dev", texto, fixed = TRUE), "Marcador dev permaneceu no script público.")
exigir(!grepl("dev_r[0-9]+", texto, perl = TRUE), "Revisão interna permaneceu no script público.")
exigir(!grepl("dev-r[0-9]+", texto, perl = TRUE), "Build interno permaneceu no script público.")
exigir(!grepl("bolsist", texto, ignore.case = TRUE), "Termo específico de vínculo permaneceu no script.")
for (hardcode in c("C:/scr_test", "C:\\\\scr_test")) {
  exigir(!grepl(hardcode, texto, fixed = TRUE), paste0("Hardcode local introduzido: ", hardcode))
}

for (arquivo in c(
  "README.md", "CHANGELOG.md", "GUIA_USUARIO_v2.9.16.md",
  "RELEASE_NOTES_v2.9.16.md", "VERSION",
  "monitora_campsav_alvo_global.R", "R_monitora_campsav_alvo_global.R",
  "R/monitora_campsav_alvo_global.R",
  "releases/v2.9.16/SHA256SUMS.txt",
  "release_assets/v2.9.16/SHA256SUMS.txt"
)) exigir(file.exists(arquivo), paste0("Arquivo de release ausente: ", arquivo))

exigir(
  identical(trimws(readLines("VERSION", warn = FALSE)), "2.9.16"),
  "VERSION não contém exatamente 2.9.16."
)
hashes <- unname(tools::md5sum(c(
  script,
  "monitora_campsav_alvo_global.R",
  "R_monitora_campsav_alvo_global.R",
  "R/monitora_campsav_alvo_global.R",
  "releases/v2.9.16/monitora_campsav_alvo_global_v2.9.16.R",
  "release_assets/v2.9.16/monitora_campsav_alvo_global_v2.9.16.R"
)))
exigir(length(unique(hashes)) == 1L, "Cópias públicas do script não são byte a byte idênticas.")

cat("TEST_V2916_RELEASE_CONTRACT_OK\n")
