# Carrega definições para testes sem executar leitura, painel ou publicação.
monitora_v307_carregar <- function(script) {
  source("tools/v305/carregar.R", local=TRUE)
  e <- monitora_v305_carregar(script)
  s <- readLines(script, encoding="UTF-8", warn=FALSE)
  inicio <- grep("^base::eval\\(base::parse\\(text=rawToChar", s)
  if (length(inicio) != 1L) stop("Módulo da candidata ausente ou ambíguo.")
  fim <- inicio + which(grepl("envir=environment()", s[(inicio+1L):length(s)], fixed=TRUE))[1L]
  eval(parse(text=paste(s[inicio:fim], collapse="\n"), encoding="UTF-8"), envir=e)
  if (exists("MONITORA_V307_DEFINICAO_COMPARACAO", envir=e, inherits=FALSE)) {
    eval(e$MONITORA_V307_DEFINICAO_COMPARACAO, envir=e)
  }
  for (name in c("MONITORA_SCRIPT_VERSAO", "MONITORA_SCRIPT_BUILD_ID")) {
    line <- s[grepl(paste0("^",name," <- "), s)]
    stopifnot(length(line)==1L)
    eval(parse(text=line), envir=e)
  }
  e
}
