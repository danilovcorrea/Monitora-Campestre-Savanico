args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Uso: Rscript <helpers> <candidata> <saida_nova>")
suppressPackageStartupMessages(library(data.table))
source(normalizePath(args[[1L]], mustWork = TRUE), local = FALSE, encoding = "UTF-8")
candidata <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
saida <- normalizePath(args[[3L]], winslash = "/", mustWork = FALSE)
if (dir.exists(saida) || file.exists(saida)) stop("Destino do teste já existe; nada sobrescrito.")

carregado <- monitora_test_funcoes(candidata)
e <- carregado$env
e$MONITORA_SCRIPT_VERSAO <- "2.9.26"
e$MONITORA_LOG_DIR <- file.path(saida, "log")
gerados <- e$monitora_manual_usuario_gerar(saida, versao = "2.9.26", formatos = "html")
rmd <- file.path(saida, "manual_usuario_v2.9.26.Rmd")
html <- file.path(saida, "manual_usuario_v2.9.26.html")
if (!all(file.exists(c(rmd, html))) || any(file.info(c(rmd, html))$size <= 0L)) stop("Rmd/HTML do manual não foram gerados.")

texto <- paste(readLines(rmd, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
obrigatorios <- c(
  "O que pode e o que não pode ser editado", "Planilhas XLSX com abas adicionais",
  "uma única aba biológica", "Roteiros integrais por finalidade", "Mapa dos diretórios de saída",
  "Tratamentos herdados e atuais", "Época das campanhas e linha de base",
  "material botânico antes de 2025", "pares insuficientes", "qfield_input/",
  "zoom_max_real", "um círculo de 500 m por UA", "auditoria_recorte_circular.csv",
  "Teste obrigatório do projeto QField", "nunca deve ser alterado ou flexibilizado"
)
ausentes <- obrigatorios[!vapply(obrigatorios, grepl, logical(1L), x = texto, fixed = TRUE)]
if (length(ausentes)) stop("Conteúdo obrigatório ausente: ", paste(ausentes, collapse = "; "))
obsoletos <- c("qfield_entrada/<uc_normalizada>", "Um MBTiles direto é detalhe")
presentes <- obsoletos[vapply(obsoletos, grepl, logical(1L), x = texto, fixed = TRUE)]
if (length(presentes)) stop("Conteúdo obsoleto reapareceu: ", paste(presentes, collapse = "; "))

dados <- file.path(saida, "dados_apoio")
cfg <- fread(file.path(dados, "configuracao_inicial.csv"))
rotinas <- fread(file.path(dados, "rotinas_integrais.csv"))
diretorios <- fread(file.path(dados, "mapa_diretorios_saida.csv"))
roteiro <- fread(file.path(dados, "roteiro_operacional_usuario.csv"))
if (nrow(cfg) < 52L || anyDuplicated(cfg$variavel) || nrow(rotinas) != 11L ||
    nrow(diretorios) != 12L || nrow(roteiro) < 15L) stop("Tabelas de apoio incompletas ou duplicadas.")
for (x in list(cfg, rotinas, diretorios, roteiro)) {
  if (anyNA(x) || any(!nzchar(trimws(as.character(unlist(x)))))) stop("Tabela de apoio contém célula vazia.")
}
if (!all(c("MONITORA_OPCAO_GERAR_PROJETO_QFIELD", "MONITORA_OPCAO_IMPORTAR_CAMADAS_QFIELD",
    "MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO", "MONITORA_RAIO_VALIDACAO_ESPACIAL_M") %in% cfg$variavel)) {
  stop("Configurações críticas não estão documentadas.")
}
if (!grepl("Projeto QField", paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE)) {
  stop("HTML não contém a seção QField.")
}
if (!all(normalizePath(c(rmd, html), winslash = "/") %in% normalizePath(gerados, winslash = "/"))) {
  stop("Gerador não retornou os produtos esperados.")
}

linhas <- readLines(candidata, warn = FALSE, encoding = "UTF-8")
bytes_crlf <- file.info(candidata)$size + length(linhas)
if (bytes_crlf >= 5 * 1024^2) stop("Candidata excederia 5 MiB em CRLF.")
cat(sprintf(
  "TEST_V2926_MANUAL_ATUALIZADO_OK; cfg=%d; rotinas=%d; diretorios=%d; roteiro=%d; Rmd=%d; HTML=%d; CRLF=%d; margem=%d\n",
  nrow(cfg), nrow(rotinas), nrow(diretorios), nrow(roteiro), file.info(rmd)$size,
  file.info(html)$size, bytes_crlf, 5 * 1024^2 - bytes_crlf
))
