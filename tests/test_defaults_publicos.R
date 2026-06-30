script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")

defaults_esperados <- c(
  MONITORA_MODO_EXECUCAO = '"completo"',
  MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES = '"N"',
  MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = '"N"',
  MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS = '"N"',
  MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS = '"S"',
  MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL = '"S"',
  MONITORA_OPCAO_GERAR_MANUAL_USUARIO = '"S"',
  MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO = '"S"'
)

normalizar_rhs <- function(x) {
  x <- sub("#.*$", "", x)
  trimws(x)
}

falhas <- character(0)

for (nome in names(defaults_esperados)) {
  padrao <- paste0("^\\s*", nome, "\\s*<-\\s*(.+?)\\s*$")
  hits <- grep(padrao, linhas, perl = TRUE, value = TRUE)
  if (!length(hits)) {
    falhas <- c(falhas, paste0(nome, ": atribuicao direta nao encontrada"))
    next
  }

  valor <- sub(padrao, "\\1", hits[1L], perl = TRUE)
  valor <- normalizar_rhs(valor)
  esperado <- defaults_esperados[[nome]]

  if (!identical(valor, esperado)) {
    falhas <- c(falhas, paste0(nome, ": esperado ", esperado, ", encontrado ", valor))
  }
}

if (length(falhas)) {
  stop(
    paste(c("Defaults publicos seguros divergentes:", paste0("- ", falhas)), collapse = "\n"),
    call. = FALSE
  )
}

cat("DEFAULTS_PUBLICOS_OK\n")
