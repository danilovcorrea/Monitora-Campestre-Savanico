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

add_corr_linhas <- trecho_entre(
  linhas,
  "shiny::observeEvent\\(input\\$add_corr",
  "shiny::observeEvent\\(input\\$add_mv_lote"
)
exigir(length(add_corr_linhas) > 20L, "Observer add_corr nao localizado.")
add_corr <- paste(add_corr_linhas, collapse = "\n")
add_corr_norm <- gsub("[[:space:]]+", " ", add_corr)

exigir(
  grepl("monitora_painel_iniciar_botao\\(\"add_corr\"", add_corr, perl = TRUE) &&
    grepl("monitora_painel_liberar_botao\\(\"add_corr\"\\)", add_corr, perl = TRUE),
  "add_corr deve ter guarda de processamento e liberacao no on.exit."
)
exigir(grepl("acao_val\\s*<-\\s*monitora_painel_valor\\(input\\$acao\\)", add_corr, perl = TRUE), "add_corr deve sanear input$acao.")
exigir(grepl("confirmar_abrangencia_val\\s*<-\\s*isTRUE\\(input\\$confirmar_abrangencia\\)", add_corr, perl = TRUE), "add_corr deve sanear confirmacao booleana com isTRUE.")
exigir(!grepl("if\\s*\\([^\\n]*(input\\$acao|input\\$confirmar_abrangencia|input\\$escopo|input\\$valor_novo|input\\$valor_original)", add_corr, perl = TRUE), "add_corr ainda contem if com input direto potencialmente NA/NULL.")
exigir(!grepl("&&\\s*input\\$|\\|\\|\\s*input\\$", add_corr, perl = TRUE), "add_corr ainda usa input direto em expressao booleana composta.")
exigir(grepl("showNotification|monitora_painel_notificar|monitora_painel_bloquear_operacao", add_corr, perl = TRUE), "add_corr deve avisar usuario e retornar em entradas invalidas.")
exigir(grepl("return(NULL)", add_corr, fixed = TRUE), "add_corr deve retornar sem erro em validacoes bloqueadas.")
exigir(grepl('isTRUE(acao_val %in% c("clear", "update"))', add_corr, fixed = TRUE), "add_corr deve proteger %in% critico com isTRUE.")

defaults <- regmatches(texto, gregexpr("MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS\\s*<-\\s*\"S\"|MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL\\s*<-\\s*\"S\"", texto, perl = TRUE))[[1L]]
exigir(any(grepl("MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS", defaults)), "Default publico de VALIDAR_ESPACIAL_COLETAS deve ser S.")
exigir(any(grepl("MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL", defaults)), "Default publico de ABRIR_ABA_VALIDACAO_ESPACIAL deve ser S.")
exigir(
  grepl("MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL_ENV", texto, fixed = TRUE) &&
    grepl("identical\\(MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS, \"S\"\\)", texto, perl = TRUE),
  "VALIDAR_ESPACIAL_COLETAS=S deve ativar aba espacial por default quando nao houver ambiente explicito."
)
exigir(
  grepl("Validação espacial ativa; aba/mapa espacial", texto, fixed = TRUE),
  "Console deve informar validacao espacial ativa e estado da aba/mapa."
)
exigir(
  grepl("inicio_plotavel", texto, fixed = TRUE) &&
    grepl("fim_plotavel", texto, fixed = TRUE) &&
    grepl("COLETA(s) com ao menos um ponto plotável", texto, fixed = TRUE),
  "Mapa deve tolerar coordenadas parciais e registrar aviso."
)

cat("HOTFIX_V2_6_2_ADD_CORR_ESPACIAL_ESTATICA_OK\n")
