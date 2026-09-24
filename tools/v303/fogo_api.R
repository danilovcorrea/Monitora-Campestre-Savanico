monitora_fogo_api <- function(url, parametros = list(), etapa = "consulta") {
  numero <- function(nome, padrao, minimo, maximo) {
    n <- suppressWarnings(as.numeric(Sys.getenv(nome, as.character(padrao))))
    if (length(n) != 1L || !is.finite(n) || n < minimo || n > maximo) padrao else n
  }
  limite <- numero("MONITORA_FOGO_TIMEOUT_SEG", 90, 1, 600)
  conexao <- min(limite, numero("MONITORA_FOGO_CONEXAO_TIMEOUT_SEG", 30, 1, 120))
  tentativas <- as.integer(numero("MONITORA_FOGO_TENTATIVAS", 4, 1, 6))
  inicio <- Sys.time(); ultimo <- ""; status <- NA_integer_
  for (tentativa in seq_len(tentativas)) {
    ti <- Sys.time(); pulso <- ti; status <- NA_integer_; transitorio <- TRUE
    monitora_operacao_msg("Fogo", etapa, "; tentativa ", tentativa, "/", tentativas,
      "; conexão até ", conexao, "s; operação até ", limite, "s; URL: ", url)
    progresso <- function(down, up) {
      agora <- Sys.time()
      if (as.numeric(difftime(agora, pulso, units = "secs")) >= 15) {
        monitora_operacao_msg("Fogo", etapa, ": aguardando conexão/resposta há ",
          round(as.numeric(difftime(agora, ti, units = "secs"))), "s nesta tentativa; limite ", limite, "s.")
        pulso <<- agora
      }
      TRUE
    }
    z <- tryCatch({
      resp <- httr::POST(url, body = c(list(f = "json"), parametros), encode = "form",
        httr::timeout(limite), httr::config(connecttimeout = conexao, noprogress = FALSE, progressfunction = progresso))
      status <- httr::status_code(resp)
      transitorio <- status %in% c(408L, 425L, 429L) || status >= 500L
      httr::stop_for_status(resp)
      transitorio <- TRUE
      texto <- httr::content(resp, as = "text", encoding = "UTF-8")
      obj <- jsonlite::fromJSON(texto, simplifyVector = FALSE)
      if (!is.list(obj)) stop("Resposta JSON não é um objeto válido.")
      if (!is.null(obj$error)) {
        codigo <- suppressWarnings(as.integer(obj$error$code))
        transitorio <- length(codigo) == 1L && !is.na(codigo) && (codigo %in% c(408L, 425L, 429L) || codigo >= 500L)
        stop(jsonlite::toJSON(obj$error, auto_unbox = TRUE))
      }
      if (isTRUE(obj$exceededTransferLimit)) {
        transitorio <- FALSE
        stop("A API truncou a resposta; reduzir o lote.")
      }
      list(obj = obj, texto = texto)
    }, error = function(e) { ultimo <<- conditionMessage(e); NULL })
    segundos <- as.numeric(difftime(Sys.time(), ti, units = "secs"))
    linha <- data.frame(timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), etapa = etapa,
      url = url, tentativa = tentativa, http = status, duracao_seg = round(segundos, 3),
      status = if (!is.null(z)) "ok" else if (transitorio) "falha_transitoria" else "falha_nao_retentavel",
      motivo = if (!is.null(z)) "" else ultimo)
    pasta <- get0("MONITORA_LOG_DIR", ifnotfound = "log", inherits = TRUE)
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
    arquivo <- file.path(pasta, paste0("consultas_fogo_", get0("MONITORA_EXEC_ID", ifnotfound = "ensaio", inherits = TRUE), ".csv"))
    data.table::fwrite(linha, arquivo, append = file.exists(arquivo), col.names = !file.exists(arquivo))
    if (!is.null(z)) {
      monitora_operacao_msg("Fogo", etapa, ": HTTP ", status, ", concluída em ", round(segundos, 1), "s.")
      return(z)
    }
    monitora_operacao_msg("Fogo", etapa, ": falha após ", round(segundos, 1), "s; HTTP ",
      if (is.na(status)) "sem resposta" else status, "; ", ultimo)
    if (!transitorio || tentativa == tentativas) break
    espera <- min(15, 2^(tentativa - 1L))
    monitora_operacao_msg("Fogo", "Nova tentativa em ", espera, "s; auditoria: ", arquivo)
    Sys.sleep(espera)
  }
  stop("Fogo: consulta pública não concluída [", etapa, "] após ", tentativa,
    " tentativa(s), ", round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1),
    "s. URL: ", url, "; causa: ", ultimo, "; auditoria: ", arquivo, call. = FALSE)
}
