# Definições produtivas, sem executar leitura, painel ou publicação.
monitora_v308_carregar <- function(script = "monitora_campsav_alvo_global_v3.0.8.R") {
  source("tools/v307/carregar.R", local = TRUE)
  e <- monitora_v307_carregar(script)
  if (exists("MONITORA_V308_DEFINICAO_RELATORIO", e, inherits = FALSE)) {
    eval(e$MONITORA_V308_DEFINICAO_RELATORIO, envir = e)
  }
  e
}
