script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Script principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

trecho_entre <- function(linhas, inicio_padrao, fim_padrao = NULL, apos_inicio = TRUE) {
  ini <- grep(inicio_padrao, linhas, perl = TRUE)
  if (!length(ini)) return(character())
  ini <- ini[1L]
  if (is.null(fim_padrao)) return(linhas[ini:length(linhas)])
  fim_rel <- grep(fim_padrao, linhas[seq.int(ini + as.integer(apos_inicio), length(linhas))], perl = TRUE)
  if (!length(fim_rel)) return(linhas[ini:length(linhas)])
  fim <- ini + as.integer(apos_inicio) + fim_rel[1L] - 2L
  linhas[ini:max(ini, fim)]
}

# -----------------------------------------------------------------------
# Contexto: o script so usa chamadas qualificadas cli::..., nunca
# library(cli)/require(cli). Nesse cenario, variaveis de glue dos formatos
# de progresso (pb_percent, pb_current, pb_total, pb_eta, pb_status,
# pb_elapsed, pb_name) e helpers de estilo (col_green, symbol) so existem
# no namespace do cli quando referenciadas sem prefixo `cli::`, elas nao
# existem no ambiente avaliado pelo `{}` do formato "custom" -> "object
# 'pb_percent' not found", abortando o script no inicio (RStudio).
# -----------------------------------------------------------------------
bloco_formatos <- paste(trecho_entre(
  linhas,
  "^if \\(identical\\(MONITORA_PROGRESSO_CLI_LAYOUT, \"barra_completa\"\\)\\)",
  "^MONITORA_PROGRESSO_ALVO_CHECKPOINT\\s*<-"
), collapse = "\n")
exigir(nzchar(bloco_formatos), "Bloco de definicao de MONITORA_PROGRESSO_CLI_FORMAT/_DONE nao encontrado.")

# Nenhum token pb_* ou helper de estilo cli pode aparecer sem o prefixo
# `cli::` dentro das chaves de glue do formato.
proibidos <- c(
  "\\{pb_percent\\}", "\\{pb_current\\}", "\\{pb_total\\}", "\\{pb_eta\\}",
  "\\{pb_status\\}", "\\{pb_elapsed\\}", "\\{pb_name\\}", "\\{pb_bar\\}",
  "\\{col_green\\(", "\\{symbol\\$"
)
for (pad in proibidos) {
  if (grepl(pad, bloco_formatos, perl = TRUE)) {
    falhar(paste0("Formato cli de progresso ainda usa token sem qualificar 'cli::' (padrao ", pad, "); isso derruba o script com 'object not found' porque cli nao esta anexado via library(cli)."))
  }
}

# Os tokens devem existir, mas sempre qualificados.
exigir_fixo <- function(x, padrao, mensagem) if (!grepl(padrao, x, fixed = TRUE)) falhar(mensagem)
for (tok in c("cli::pb_percent", "cli::pb_current", "cli::pb_total", "cli::pb_eta", "cli::pb_status", "cli::pb_elapsed", "cli::pb_bar")) {
  exigir_fixo(bloco_formatos, tok, paste0("Formato cli de progresso deve usar ", tok, " (qualificado) em pelo menos um layout."))
}
exigir_fixo(bloco_formatos, "cli::col_green(cli::symbol$tick)", "Formato cli de progresso concluido deve usar cli::col_green(cli::symbol$tick) (qualificado).")

# -----------------------------------------------------------------------
# Falha ao iniciar/atualizar a barra cli nunca pode abortar o script: os tres
# tryCatch relevantes nao podem conter stop() dentro do handler de erro, e
# devem sempre cair para utils::txtProgressBar().
# -----------------------------------------------------------------------
bloco_iniciar_cli <- paste(trecho_entre(
  linhas,
  "^monitora_progresso_iniciar_cli\\s*<-\\s*function",
  "^monitora_progresso_iniciar\\s*<-\\s*function"
), collapse = "\n")
exigir(nzchar(bloco_iniciar_cli), "monitora_progresso_iniciar_cli() nao encontrada.")
exigir(!grepl("stop\\(", bloco_iniciar_cli, perl = TRUE), "monitora_progresso_iniciar_cli() nao pode conter stop(): falha da barra cli nao pode abortar o script.")
exigir(
  lengths(regmatches(bloco_iniciar_cli, gregexpr("monitora_progresso_iniciar_txt\\(\\)", bloco_iniciar_cli, perl = TRUE))) >= 2L,
  "monitora_progresso_iniciar_cli() deve cair para monitora_progresso_iniciar_txt() tanto na falha de criacao quanto na falha de atualizacao inicial da barra cli."
)

bloco_atualizar_backend <- paste(trecho_entre(
  linhas,
  "^monitora_progresso_atualizar_backend\\s*<-\\s*function",
  "^monitora_progresso_definir_valor\\s*<-\\s*function"
), collapse = "\n")
exigir(nzchar(bloco_atualizar_backend), "monitora_progresso_atualizar_backend() nao encontrada.")
exigir(!grepl("stop\\(", bloco_atualizar_backend, perl = TRUE), "monitora_progresso_atualizar_backend() nao pode conter stop(): falha durante atualizacao da barra cli nao pode abortar o script.")
exigir_fixo(bloco_atualizar_backend, "monitora_progresso_iniciar_txt()", "monitora_progresso_atualizar_backend() deve cair para monitora_progresso_iniciar_txt() quando a atualizacao cli falhar.")

# -----------------------------------------------------------------------
# MONITORA_PROGRESSO_BACKEND="txt" deve continuar funcionando (nao foi
# alterado por este hotfix).
# -----------------------------------------------------------------------
exigir_fixo(texto, 'monitora_progresso_iniciar_txt <- function() {', "monitora_progresso_iniciar_txt() nao encontrada.")
exigir_fixo(texto, 'MONITORA_PROGRESSO_BACKEND_ATIVO <<- "txt"', "monitora_progresso_iniciar_txt() deve continuar definindo o backend ativo como txt.")
exigir_fixo(texto, 'utils::txtProgressBar(min = 0, max = MONITORA_PROGRESSO_TOTAL, style = 3)', "monitora_progresso_iniciar_txt() deve continuar usando utils::txtProgressBar().")

cat("HOTFIX_V2_6_2_PROGRESSO_CLI_PB_PERCENT_ESTATICA_OK\n")
