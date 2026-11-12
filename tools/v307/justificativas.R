# Continuidade de justificativas v3.0.7. Este módulo não executa o pipeline.
# Eventos são imutáveis; aliases são apenas uma projeção da identidade.

monitora_v307_justificativas_valor <- function(x, nome, padrao = "") {
  z <- x[[nome]]
  if (is.null(z) || !length(z) || is.na(z[[1L]])) padrao else as.character(z[[1L]])
}

monitora_v307_justificativas_avisar <- function(mensagem, chave) {
  exec <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  chave <- paste(exec, chave, mensagem, sep = "\034")
  anteriores <- getOption("monitora.v307.justificativas.avisos", character())
  if (!chave %in% anteriores) {
    options(monitora.v307.justificativas.avisos = c(anteriores, chave))
    warning(mensagem, call. = FALSE)
  }
  invisible(NULL)
}

monitora_v307_justificativas_memoria_sha256 <- function(registros) {
  x <- as.data.frame(data.table::copy(data.table::as.data.table(registros)), stringsAsFactors = FALSE)
  for (cc in names(x)) if (is.character(x[[cc]]) || is.factor(x[[cc]])) x[[cc]] <- enc2utf8(as.character(x[[cc]]))
  # Fingerprint operacional, separado do contrato de IDs e do hash físico de saída.
  digest::digest(serialize(x, NULL, version = 2L), algo = "sha256", serialize = FALSE)
}

monitora_v307_justificativas_registrar_base <- function(registros, origem = "painel", continuar = FALSE) {
  exec <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  if (is.na(exec) || !nzchar(exec)) stop("Registro da base de justificativas exige exec_id atual.", call. = FALSE)
  sha <- monitora_v307_justificativas_memoria_sha256(registros)
  anterior <- get0("MONITORA_V307_JUSTIFICATIVAS_BASE", ifnotfound = NULL, inherits = TRUE)
  ancestrais <- if (isTRUE(continuar) && !is.null(anterior) && identical(monitora_v307_justificativas_valor(anterior, "exec_id"), exec)) unique(c(anterior$memorias_autorizadas, anterior$memoria_sha256)) else character()
  origem <- as.character(origem)[1L]
  base <- list(exec_id = exec, memoria_sha256 = sha, memorias_autorizadas = unique(c(ancestrais, sha)), origem = origem,
    memoria_confirmada = origem %in% c("painel", "preview_confirmado", "pos_painel"), n_registros = nrow(registros))
  assign("MONITORA_V307_JUSTIFICATIVAS_BASE", base, envir = environment(monitora_v307_justificativas_registrar_base))
  invisible(base)
}

monitora_v307_justificativas_registrar_cache_validado <- function(memoria_sha256, input_sha256, config_sha256, validado = FALSE) {
  exec <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  hashes <- c(memoria_sha256, input_sha256, config_sha256)
  if (!isTRUE(validado) || length(hashes) != 3L || anyNA(hashes) || any(!grepl("^[a-f0-9]{64}$", hashes)) || is.na(exec) || !nzchar(exec)) stop("O vínculo do histórico exige cache validado e hashes completos da base/input/configuração.", call. = FALSE)
  prova <- list(ok = TRUE, exec_id = exec, memoria_sha256 = memoria_sha256, input_sha256 = input_sha256, config_sha256 = config_sha256)
  assign("MONITORA_V307_CACHE_VALIDADO", prova, envir = environment(monitora_v307_justificativas_registrar_cache_validado))
  invisible(prova)
}

monitora_v307_justificativas_fingerprint_brutos <- function(
    arquivos = NULL, input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE)
) {
  raiz <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  if (is.null(arquivos)) arquivos <- list.files(raiz, pattern = "[.](csv|zip|xlsx?|rds)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (!length(arquivos)) return(list(sha256 = "", arquivos = list()))
  paths <- normalizePath(arquivos, winslash = "/", mustWork = TRUE)
  if (any(!startsWith(paths, paste0(raiz, "/"))) || any(dir.exists(paths))) stop("Fingerprint dos brutos exige arquivos físicos dentro do input atual.", call. = FALSE)
  rel <- substring(paths, nchar(raiz) + 2L)
  base <- basename(paths)
  excluir <- grepl("(^|/)(linhagem|oraculo_replay|referencia_replay|diagnostico_run_referencia|output|log|plots_png)(/|$)", rel, ignore.case = TRUE) |
    grepl("^(registros_corrig|justificativas|pendencias_justificadas|pendencias_remanescentes_com_justificativas|correcoes_|auditoria_|aplicacoes_correcoes|metadados_sess|inventario_sess|incorporacoes_novas|manifesto_|sum_|cob_|prop_rel_|relatorio_execucao|duplicidades_|conflitos_)", base, ignore.case = TRUE)
  controle <- get0("monitora_io_eh_csv_controle_correcao", mode = "function", ifnotfound = NULL, inherits = TRUE)
  if (!is.null(controle)) excluir <- excluir | controle(base)
  paths <- paths[!excluir]; rel <- rel[!excluir]
  ordem <- order(enc2utf8(rel), method = "radix"); paths <- paths[ordem]; rel <- rel[ordem]
  if (anyDuplicated(rel)) stop("Inventário dos brutos contém caminhos duplicados.", call. = FALSE)
  itens <- lapply(seq_along(paths), function(i) list(path = rel[i], sha256 = monitora_v307_justificativas_hash(paths[i], arquivo = TRUE), bytes = as.double(file.info(paths[i])$size)))
  monitora_v307_justificativas_fingerprint_inventario(itens)
}

monitora_v307_justificativas_fingerprint_inventario <- function(itens) {
  if (!length(itens)) return(list(sha256 = "", arquivos = list()))
  paths <- vapply(itens, function(f) monitora_v307_justificativas_valor(f, "path"), character(1L))
  if (any(!nzchar(paths)) || any(grepl("(^/|^[A-Za-z]:|(^|/)[.][.](/|$))", paths)) || anyDuplicated(paths)) stop("Caminhos inválidos/ambíguos no inventário dos brutos.", call. = FALSE)
  sha <- vapply(itens, function(f) monitora_v307_justificativas_valor(f, "sha256"), character(1L))
  if (any(!grepl("^[a-f0-9]{64}$", sha))) stop("Hash inválido no inventário dos brutos.", call. = FALSE)
  ordem <- order(enc2utf8(paths), method = "radix"); itens <- itens[ordem]
  assinatura <- monitora_v307_justificativas_pacote(c("monitora_brutos_v1", unlist(lapply(itens, function(f) c(monitora_v307_justificativas_valor(f, "path"), monitora_v307_justificativas_valor(f, "sha256"), monitora_v307_justificativas_valor(f, "bytes"))), use.names = FALSE)))
  list(sha256 = monitora_v307_justificativas_hash(assinatura), arquivos = itens)
}

monitora_v307_justificativas_registrar_replay <- function(arquivo_ledger, validado = FALSE, arquivos_brutos = NULL) {
  solicitado <- isTRUE(get0("MONITORA_REAPLICAR_CORRECOES_ANTERIORES", ifnotfound = FALSE, inherits = TRUE)) ||
    identical(toupper(as.character(get0("MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES", ifnotfound = "N", inherits = TRUE))[1L]), "S")
  if (!isTRUE(validado) || !solicitado) stop("Vínculo de justificativas por replay exige replay solicitado e semanticamente validado.", call. = FALSE)
  fp <- monitora_v307_justificativas_fingerprint_brutos(arquivos_brutos)
  if (!nzchar(fp$sha256)) stop("Replay de justificativas exige brutos físicos identificados.", call. = FALSE)
  prova <- list(exec_id = as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE)),
    brutos_sha256 = fp$sha256, arquivos = fp$arquivos, ledger_path = normalizePath(arquivo_ledger, winslash = "/", mustWork = TRUE),
    ledger_sha256 = monitora_v307_justificativas_hash(arquivo_ledger, arquivo = TRUE))
  assign("MONITORA_V307_JUSTIFICATIVAS_REPLAY", prova, envir = environment(monitora_v307_justificativas_registrar_replay))
  invisible(prova)
}

monitora_v307_justificativas_hash <- function(x, arquivo = FALSE) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Justificativas exigem digest para SHA-256.", call. = FALSE)
  if (isTRUE(arquivo)) {
    if (length(x) != 1L || !file.exists(x) || dir.exists(x)) stop("Arquivo de justificativas ausente: ", x, call. = FALSE)
    return(digest::digest(file = x, algo = "sha256"))
  }
  digest::digest(charToRaw(enc2utf8(as.character(x))), algo = "sha256", serialize = FALSE)
}

monitora_v307_justificativas_pacote <- function(x) {
  z <- enc2utf8(as.character(x)); z[is.na(z)] <- ""
  paste0(nchar(z, type = "bytes"), ":", z, collapse = "")
}

monitora_v307_justificativas_identidade <- function(x) {
  x <- data.table::as.data.table(x)
  n <- nrow(x)
  campo <- function(nm) {
    z <- if (nm %in% names(x)) as.character(x[[nm]]) else rep("", n)
    z[is.na(z)] <- ""; enc2utf8(trimws(z))
  }
  alvo_legado <- campo("monitora_row_id")
  sem_alvo <- !nzchar(alvo_legado)
  alvo_legado[sem_alvo] <- paste(campo("COLETA"), campo("linha_indice"), campo("UC"), campo("EA"), campo("UA"), campo("ANO"), sep = "|")[sem_alvo]
  categoria <- campo("ocorrencia_categoria")
  categoria[!nzchar(categoria)] <- campo("categoria_superior")[!nzchar(categoria)]
  token <- campo("ocorrencia_token")
  token[!nzchar(token)] <- campo("forma_de_vida_detectada")[!nzchar(token)]
  token[!nzchar(token)] <- campo("token_invalido")[!nzchar(token)]
  atributo <- campo("ocorrencia_atributo")
  atributo[!nzchar(atributo)] <- campo("atributo_problema")[!nzchar(atributo)]
  atributo[!nzchar(atributo)] <- campo("caminho_registro")[!nzchar(atributo)]
  tipo <- campo("tipo_ocorrencia")
  agregado <- tipo %in% c("formacao_vegetacional_inconsistente_ua", "formacao_nao_uniforme_na_coleta", "formacao_divergente_mesma_ua_ano", "mudanca_formacao_entre_anos")
  alvo <- alvo_legado
  alvo[agregado] <- paste0("COLETA::", campo("COLETA")[agregado])
  campos <- data.table::data.table(
    tipo_ocorrencia = tipo, COLETA = campo("COLETA"), alvo = alvo,
    categoria = categoria, token = token, atributo = atributo,
    UC = campo("UC"), EA = campo("EA"), UA = campo("UA"), ANO = campo("ANO"),
    CICLO = campo("CICLO"), CAMPANHA = campo("CAMPANHA")
  )
  chave <- rep(monitora_v307_justificativas_pacote("monitora_ocorrencia_v1"), n)
  for (cc in names(campos)) chave <- paste0(chave, nchar(campos[[cc]], type = "bytes"), ":", campos[[cc]])
  sha <- vapply(chave, monitora_v307_justificativas_hash, character(1L))
  canonico <- paste0("occ_v1_", substr(sha, 1L, 24L))
  assinatura_legada <- paste(tipo, campo("COLETA"), alvo_legado, categoria, token, atributo, sep = "\034")
  legacy <- function(marca) vapply(assinatura_legada, function(z) {
    z <- enc2utf8(z); Encoding(z) <- marca
    # Reprodução local do contrato legado; o helper global não é alterado.
    z <- paste(as.character(z), collapse = "|")
    paste0("occ_", substr(digest::digest(z, algo = "sha256"), 1L, 24L))
  }, character(1L))
  out <- data.table::data.table(
    ocorrencia_id = canonico, assinatura_sha256 = sha,
    identidade_compativel = nzchar(tipo) & nzchar(campo("COLETA")) & (!sem_alvo | agregado),
    alias_utf8 = legacy("UTF-8"), alias_unknown = legacy("unknown")
  )
  if (n && anyDuplicated(canonico)) {
    colisao <- out[, data.table::uniqueN(assinatura_sha256), by = ocorrencia_id][V1 > 1L]
    if (nrow(colisao)) stop("Colisão na identidade canônica de ocorrência.", call. = FALSE)
  }
  out[]
}

monitora_v307_justificativas_ocorrencia_id <- function(x) monitora_v307_justificativas_identidade(x)$ocorrencia_id

monitora_v307_justificativas_aliases_template <- function() data.table::data.table(
  alias_ocorrencia_id = character(), ocorrencia_id_canonico = character(),
  identidade_schema = character(), assinatura_sha256 = character(), origem_alias = character()
)

monitora_v307_justificativas_aliases_validar <- function(x) {
  x <- data.table::copy(data.table::as.data.table(x)); modelo <- monitora_v307_justificativas_aliases_template()
  if (!ncol(x) && !nrow(x)) return(modelo)
  if (!identical(names(x), names(modelo))) stop("Schema de aliases de justificativas inválido.", call. = FALSE)
  for (cc in names(x)) data.table::set(x, j = cc, value = as.character(x[[cc]]))
  if (nrow(x)) {
    if (anyNA(x) || any(!nzchar(x$alias_ocorrencia_id)) ||
        any(!grepl("^occ_v1_[a-f0-9]{24}$", x$ocorrencia_id_canonico)) ||
        any(x$identidade_schema != "monitora_ocorrencia_v1") ||
        any(!grepl("^[a-f0-9]{64}$", x$assinatura_sha256)) ||
        any(x$ocorrencia_id_canonico != paste0("occ_v1_", substr(x$assinatura_sha256, 1L, 24L)))) {
      stop("Aliases de justificativas possuem campos inválidos.", call. = FALSE)
    }
    conflito <- x[, .(n_id = data.table::uniqueN(ocorrencia_id_canonico), n_chave = data.table::uniqueN(assinatura_sha256)), by = alias_ocorrencia_id]
    if (any(conflito$n_id != 1L | conflito$n_chave != 1L)) stop("Alias de justificativa ambíguo; não é permitido parear por rótulo ou tipo.", call. = FALSE)
    canonicos <- x[, data.table::uniqueN(assinatura_sha256), by = ocorrencia_id_canonico]
    if (any(canonicos$V1 != 1L)) stop("Identidade canônica de justificativas conflitante.", call. = FALSE)
    x <- unique(x, by = c("alias_ocorrencia_id", "ocorrencia_id_canonico"))
  }
  x[]
}

monitora_v307_justificativas_aliases <- function(ocorrencias, origem = "diagnostico_atual") {
  oc <- data.table::as.data.table(ocorrencias); id <- monitora_v307_justificativas_identidade(oc)
  if (!nrow(id)) return(monitora_v307_justificativas_aliases_template())
  if ("ocorrencia_id" %in% names(oc)) {
    fornecido <- as.character(oc$ocorrencia_id)
    valido <- is.na(fornecido) | !nzchar(fornecido) | fornecido == id$ocorrencia_id | fornecido == id$alias_utf8 | fornecido == id$alias_unknown
    if (!all(valido)) stop("O identificador fornecido não corresponde aos campos completos da ocorrência.", call. = FALSE)
  }
  partes <- lapply(c("ocorrencia_id", "alias_utf8", "alias_unknown"), function(cc) data.table::data.table(
    alias_ocorrencia_id = id[[cc]], ocorrencia_id_canonico = id$ocorrencia_id,
    identidade_schema = "monitora_ocorrencia_v1", assinatura_sha256 = id$assinatura_sha256,
    origem_alias = paste0(origem, ":", cc)
  ))
  monitora_v307_justificativas_aliases_validar(data.table::rbindlist(partes))
}

monitora_v307_justificativas_template <- function() data.table::data.table(
  evento_justificativa_id = character(), evento_lote_id = character(), ordem_no_lote = character(),
  n_ocorrencias_lote = character(), ocorrencia_id = character(), status_evento = character(),
  timestamp_evento = character(), exec_id = character(), script_versao = character(),
  responsavel = character(), tipo_justificativa = character(), justificativa = character(),
  tipo_ocorrencia = character(), COLETA = character(), UC = character(), EA = character(),
  UA = character(), ANO = character(), evento_origem_id = character()
)

monitora_v307_justificativas_eventos_validar <- function(x, origem = "histórico") {
  x <- data.table::copy(data.table::as.data.table(x)); modelo <- monitora_v307_justificativas_template()
  if (!ncol(x) && !nrow(x)) return(modelo)
  if (!identical(names(x), names(modelo))) stop("Schema desconhecido/incompleto de justificativas em ", origem, ".", call. = FALSE)
  for (cc in names(x)) {
    z <- as.character(x[[cc]]); z[is.na(z)] <- ""
    data.table::set(x, j = cc, value = z)
  }
  if (!nrow(x)) return(x)
  obrigatorios <- c("evento_justificativa_id", "ocorrencia_id", "status_evento", "timestamp_evento", "responsavel", "tipo_justificativa", "justificativa", "tipo_ocorrencia", "COLETA")
  if (any(vapply(obrigatorios, function(cc) any(!nzchar(trimws(x[[cc]]))), logical(1L)))) stop("Evento incompleto de justificativa em ", origem, ".", call. = FALSE)
  if (any(!x$status_evento %in% c("vigente", "encerrada_por_resolucao", "revogada", "orfa"))) stop("Status de justificativa desconhecido em ", origem, ".", call. = FALSE)
  x <- unique(x, by = names(modelo))
  if (anyDuplicated(x$evento_justificativa_id)) stop("Mesmo evento_justificativa_id com conteúdo conflitante em ", origem, ".", call. = FALSE)
  fechados <- x[status_evento %in% c("encerrada_por_resolucao", "revogada", "orfa")]
  if (nrow(fechados)) {
    idx <- match(fechados$evento_origem_id, x$evento_justificativa_id)
    if (anyNA(idx) || anyDuplicated(fechados$evento_origem_id) ||
        any(x$status_evento[idx] != "vigente") || any(x$ocorrencia_id[idx] != fechados$ocorrencia_id) ||
        any(fechados$timestamp_evento < x$timestamp_evento[idx])) {
      stop("Encerramentos de justificativas não possuem origens append-only íntegras em ", origem, ".", call. = FALSE)
    }
  }
  x[]
}

monitora_v307_justificativas_csv_ler <- function(path) {
  if (!file.exists(path) || dir.exists(path) || is.na(file.info(path)$size) || file.info(path)$size == 0) stop("CSV de justificativas ausente ou vazio: ", path, call. = FALSE)
  # O scanner de read.csv converte CRLF dentro de aspas para LF. Protegemos
  # exclusivamente CR internos; delimitadores físicos LF/CRLF continuam válidos.
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(239, 187, 191)))) bytes <- bytes[-(1:3)]
  txt <- rawToChar(bytes)
  if (!validUTF8(txt)) stop("CSV de justificativas não possui bytes UTF-8 válidos: ", path, call. = FALSE)
  token <- paste0("__MONITORA_CR_", monitora_v307_justificativas_hash(txt), "__")
  while (grepl(token, txt, fixed = TRUE)) token <- paste0(token, "_")
  aspas <- which(bytes == as.raw(34L)); cr <- which(bytes == as.raw(13L))
  internos <- cr[findInterval(cr, aspas) %% 2L == 1L]
  if (length(internos)) {
    partes <- vector("list", length(internos) * 2L + 1L); inicio <- 1L
    for (i in seq_along(internos)) {
      fim <- internos[i] - 1L
      partes[[2L * i - 1L]] <- if (fim >= inicio) rawToChar(bytes[inicio:fim]) else ""
      partes[[2L * i]] <- token; inicio <- internos[i] + 1L
    }
    partes[[length(partes)]] <- if (inicio <= length(bytes)) rawToChar(bytes[inicio:length(bytes)]) else ""
    txt <- paste0(unlist(partes, use.names = FALSE), collapse = "")
  }
  Encoding(txt) <- "UTF-8"
  out <- tryCatch(withCallingHandlers(data.table::as.data.table(utils::read.csv(
    text = txt, colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = NULL, comment.char = "", blank.lines.skip = TRUE
  )), warning = function(w) stop(conditionMessage(w), call. = FALSE)),
  error = function(e) stop("Falha explícita ao ler justificativas em ", path, ": ", conditionMessage(e), call. = FALSE))
  if (length(internos)) for (cc in names(out)) data.table::set(out, j = cc, value = gsub(token, "\r", out[[cc]], fixed = TRUE))
  out[]
}

monitora_v307_justificativas_json_ler <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Justificativas exigem jsonlite.", call. = FALSE)
  tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) stop("Manifesto de justificativas ilegível: ", path, ": ", conditionMessage(e), call. = FALSE))
}

monitora_v307_justificativas_snapshot_legado_ler <- function(path) {
  x <- monitora_v307_justificativas_csv_ler(path)
  # Contrato de leitura do snapshot legado, comprovado pela auditoria v3.0.6:
  # na.strings=c("", "NA"). Aplicar só aos campos diagnósticos, nunca aos
  # eventos nem aos textos livres de autoria/justificativa. A cópia física do
  # snapshot permanece intacta e a política é registrada na recuperação.
  campos <- intersect(c("tipo_ocorrencia", "rotulo_ocorrencia", "COLETA", "linha_indice", "monitora_row_id",
    "ocorrencia_token", "ocorrencia_atributo", "ocorrencia_categoria", "ocorrencia_status", "ocorrencia_detalhe",
    "UC", "EA", "UA", "CICLO", "CAMPANHA", "ANO", "fonte_indice"), names(x))
  for (cc in campos) {
    z <- x[[cc]]; z[!is.na(z) & z %in% c("", "NA")] <- NA_character_
    data.table::set(x, j = cc, value = z)
  }
  x[]
}

monitora_v307_justificativas_juntar <- function(partes, aliases = FALSE) {
  partes <- Filter(function(x) !is.null(x) && ncol(data.table::as.data.table(x)), partes)
  x <- if (length(partes)) data.table::rbindlist(partes, use.names = TRUE, fill = FALSE) else if (isTRUE(aliases)) monitora_v307_justificativas_aliases_template() else monitora_v307_justificativas_template()
  if (isTRUE(aliases)) monitora_v307_justificativas_aliases_validar(x) else monitora_v307_justificativas_eventos_validar(x)
}

monitora_v307_justificativas_preservar_atributos <- function(resultado, origem) {
  for (nm in c("monitora_v307_aliases", "monitora_v307_complemento_lotes")) {
    valor <- attr(origem, nm, exact = TRUE)
    if (!is.null(valor)) attr(resultado, nm) <- data.table::copy(valor)
  }
  resultado
}

monitora_v307_justificativas_caminho <- function(dir_lin, declarado, nome) {
  declarado <- as.character(declarado)[1L]
  aceitos <- c(nome, paste0("linhagem/", nome), paste0("02_painel_correcoes/linhagem/", nome))
  if (is.na(declarado) || !declarado %in% aceitos) stop("Caminho de justificativas não autorizado no manifesto: ", declarado, call. = FALSE)
  file.path(dir_lin, nome)
}

monitora_v307_justificativas_validar_importacao <- function(
    dir_lin, manifesto, arquivo_registros = NULL, data_sha256 = NULL, revision_id = NULL
) {
  man <- manifesto
  val <- function(nm, padrao = "") monitora_v307_justificativas_valor(man, nm, padrao)
  if (!identical(val("justificativas_schema"), "monitora_justificativas_v1") ||
      !identical(val("justificativas_identity_schema"), "monitora_ocorrencia_v1")) stop("Schema/versionamento de justificativas desconhecido.", call. = FALSE)
  if (!is.null(arquivo_registros)) data_sha256 <- monitora_v307_justificativas_hash(arquivo_registros, arquivo = TRUE)
  if (is.null(data_sha256)) data_sha256 <- val("output_data_sha256", val("justificativas_data_sha256"))
  if (is.null(revision_id)) revision_id <- val("revision_id", val("justificativas_revision_id"))
  if (length(data_sha256) != 1L || is.na(data_sha256) || !grepl("^[a-f0-9]{64}$", data_sha256) ||
      !identical(data_sha256, val("justificativas_data_sha256")) ||
      !nzchar(revision_id) || !identical(as.character(revision_id), val("justificativas_revision_id"))) stop("Justificativas pertencem a outra base/revisão de linhagem.", call. = FALSE)
  conferir <- function(campo, nome, contagem) {
    path <- monitora_v307_justificativas_caminho(dir_lin, val(paste0(campo, "_path")), nome)
    esperado <- val(paste0(campo, "_sha256"))
    if (!grepl("^[a-f0-9]{64}$", esperado) || !identical(monitora_v307_justificativas_hash(path, arquivo = TRUE), esperado)) stop("Hash divergente de justificativas: ", nome, call. = FALSE)
    x <- monitora_v307_justificativas_csv_ler(path)
    nd <- val(contagem)
    if (!grepl("^(0|[1-9][0-9]*)$", nd) || !identical(as.double(nrow(x)), as.double(nd))) stop("Contagem divergente de justificativas: ", nome, call. = FALSE)
    list(dados = x, path = path)
  }
  h <- conferir("justificativas", "justificativas_pendencias_consolidada.csv", "justificativas_event_count")
  a <- conferir("justificativas_aliases", "justificativas_ocorrencias_aliases.csv", "justificativas_aliases_count")
  hist <- monitora_v307_justificativas_eventos_validar(h$dados, h$path)
  # Deduplicação não pode encobrir cardinalidade incorreta no contrato físico.
  if (nrow(hist) != nrow(h$dados)) stop("Histórico assinado contém evento duplicado.", call. = FALSE)
  aliases <- monitora_v307_justificativas_aliases_validar(a$dados)
  if (nrow(aliases) != nrow(a$dados)) stop("Arquivo assinado de aliases contém duplicatas.", call. = FALSE)
  if (nrow(hist) && any(!hist$ocorrencia_id %in% aliases$alias_ocorrencia_id)) stop("Histórico assinado contém ocorrência sem identidade/alias comprovado.", call. = FALSE)
  n_oc <- val("justificativas_ocorrencias_count")
  if (!grepl("^(0|[1-9][0-9]*)$", n_oc) || as.double(n_oc) != data.table::uniqueN(hist$ocorrencia_id)) stop("Contagem de ocorrências justificadas diverge do manifesto.", call. = FALSE)
  if (nzchar(val("justificativas_snapshot_path"))) {
    sp <- val("justificativas_snapshot_path")
    if (!identical(sp, "pendencias_justificadas.csv")) stop("Caminho do snapshot de justificativas inválido.", call. = FALSE)
    sx <- conferir("justificativas_snapshot", sp, "justificativas_snapshot_count")$dados
    if (!all(c("ocorrencia_id", "status_justificativa") %in% names(sx)) || anyDuplicated(sx$ocorrencia_id) || any(!sx$status_justificativa %in% c("vigente", "sem_justificativa"))) stop("Schema/conteúdo do snapshot de justificativas inválido.", call. = FALSE)
  }
  fontes <- list(list(path = h$path, sha256 = val("justificativas_sha256")))
  declaradas <- man$justificativas_fontes
  if (!is.null(declaradas) && length(declaradas)) for (f in declaradas) {
    sha <- monitora_v307_justificativas_valor(f, "sha256")
    rel <- monitora_v307_justificativas_valor(f, "path")
    aceitos <- paste0("justificativas_fontes/", c(sha, substr(sha, 1L, 24L)), ".csv")
    if (!grepl("^[a-f0-9]{64}$", sha) || !rel %in% aceitos) stop("Proveniência física de justificativas inválida.", call. = FALSE)
    path <- file.path(dir_lin, rel)
    if (!identical(monitora_v307_justificativas_hash(path, arquivo = TRUE), sha)) stop("Hash divergente de fonte histórica preservada.", call. = FALSE)
    fontes[[length(fontes) + 1L]] <- list(path = path, sha256 = sha)
  }
  recpath <- val("justificativas_recuperacao_path")
  recuperacao <- NULL
  if (nzchar(recpath)) {
    if (!identical(recpath, "manifesto_recuperacao_justificativas.json")) stop("Caminho de recuperação inválido.", call. = FALSE)
    recfile <- file.path(dir_lin, recpath)
    if (!identical(monitora_v307_justificativas_hash(recfile, arquivo = TRUE), val("justificativas_recuperacao_sha256"))) stop("Manifesto de recuperação de justificativas divergente.", call. = FALSE)
    recuperacao <- list(path = recfile, sha256 = val("justificativas_recuperacao_sha256"))
    rm <- monitora_v307_justificativas_json_ler(recfile)
    prova <- file.path(dir_lin, "justificativas_snapshot_recuperacao.csv")
    if (!identical(monitora_v307_justificativas_hash(prova, arquivo = TRUE), monitora_v307_justificativas_valor(rm, "source_snapshot_sha256"))) stop("Prova de snapshot da recuperação diverge do manifesto.", call. = FALSE)
  }
  attr(hist, "monitora_v307_aliases") <- aliases
  attr(hist, "monitora_v307_fontes") <- fontes
  attr(hist, "monitora_v307_recuperacao") <- recuperacao
  if (nzchar(val("justificativas_replay_schema"))) {
    fp <- monitora_v307_justificativas_fingerprint_inventario(man$justificativas_brutos_arquivos)
    if (!identical(val("justificativas_replay_schema"), "monitora_justificativas_replay_v1") ||
        !nzchar(fp$sha256) || !identical(fp$sha256, val("justificativas_brutos_sha256")) ||
        !grepl("^[a-f0-9]{64}$", val("justificativas_replay_ledger_sha256"))) stop("Contrato de replay das justificativas é inválido.", call. = FALSE)
    attr(hist, "monitora_v307_replay_prova") <- list(brutos_sha256 = fp$sha256, arquivos = fp$arquivos, ledger_sha256 = val("justificativas_replay_ledger_sha256"))
  }
  attr(hist, "monitora_v307_contexto") <- list(status = "historico_assinado_validado", data_sha256 = data_sha256, revision_id = revision_id)
  hist[]
}

monitora_v307_justificativas_contexto <- function(
    input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE), arquivo_registros = NULL
) {
  arq <- list.files(input_dir, pattern = "^registros_corrig.*[.]csv$", full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
  if (length(arq) > 1L) stop("Contexto de justificativas ambíguo: input exige exatamente um registros_corrig*.csv.", call. = FALSE)
  if (!is.null(arquivo_registros)) {
    if (length(arquivo_registros) != 1L || !file.exists(arquivo_registros) || length(arq) != 1L ||
        !identical(normalizePath(arquivo_registros, winslash = "/", mustWork = TRUE), normalizePath(arq, winslash = "/", mustWork = TRUE))) stop("Arquivo validado pela importação não corresponde ao registros_corrig*.csv único do input.", call. = FALSE)
    arq <- normalizePath(arquivo_registros, winslash = "/", mustWork = TRUE)
  }
  hash <- if (length(arq) == 1L) monitora_v307_justificativas_hash(arq, arquivo = TRUE) else ""
  global <- as.character(get0("MONITORA_LINHAGEM_INPUT_SHA256", ifnotfound = "", inherits = TRUE))[1L]
  if (length(arq) == 1L && !is.na(global) && nzchar(global)) {
    if (nzchar(hash) && !identical(hash, global)) stop("O hash físico de entrada diverge do contexto de justificativas.", call. = FALSE)
    hash <- global
  }
  base <- get0("MONITORA_V307_JUSTIFICATIVAS_BASE", ifnotfound = NULL, inherits = TRUE)
  exec <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  memoria <- if (!is.null(base) && identical(monitora_v307_justificativas_valor(base, "exec_id"), exec)) monitora_v307_justificativas_valor(base, "memoria_sha256") else ""
  manpath <- file.path(input_dir, "linhagem", "manifesto_linhagem.json")
  rev <- if (file.exists(manpath)) monitora_v307_justificativas_valor(monitora_v307_justificativas_json_ler(manpath), "revision_id") else ""
  memorias <- if (nzchar(memoria)) unique(c(base$memorias_autorizadas, memoria)) else character()
  replay <- get0("MONITORA_V307_JUSTIFICATIVAS_REPLAY", ifnotfound = NULL, inherits = TRUE)
  if (is.null(replay) || !identical(monitora_v307_justificativas_valor(replay, "exec_id"), exec)) replay <- NULL
  cache <- get0("MONITORA_V307_CACHE_VALIDADO", ifnotfound = NULL, inherits = TRUE)
  if (is.null(cache) || !isTRUE(cache$ok) || !identical(monitora_v307_justificativas_valor(cache, "exec_id"), exec)) cache <- NULL
  conf_fn <- get0("monitora_v307_cache_configuracao_sha256", mode = "function", ifnotfound = NULL, inherits = TRUE)
  input_fn <- get0("monitora_v307_cache_input_sha256", mode = "function", ifnotfound = NULL, inherits = TRUE)
  config_hash <- if (!is.null(conf_fn)) as.character(conf_fn())[1L] else ""
  input_hash <- if (!is.null(input_fn)) as.character(input_fn(input_dir = input_dir))[1L] else ""
  list(data_sha256 = hash, memoria_sha256 = memoria, memorias_autorizadas = memorias, memoria_confirmada = nzchar(memoria) && isTRUE(base$memoria_confirmada),
    revision_id = rev, arquivo_registros = if (length(arq) == 1L) arq else NULL, replay = replay,
    cache = cache, cache_input_sha256 = input_hash, cache_config_sha256 = config_hash)
}

monitora_v307_justificativas_ler_historico <- function(
    input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE),
    output_dir = get0("MONITORA_OUTPUT_DIR", ifnotfound = "output", inherits = TRUE),
    avisar = TRUE, arquivo_registros = NULL
) {
  dir_lin <- file.path(input_dir, "linhagem")
  if (file.exists(file.path(input_dir, "justificativas_pendencias_consolidada.csv"))) stop("Histórico de justificativas no input raiz não é uma entrada autorizada; use input/linhagem com manifesto verificável.", call. = FALSE)
  path_man <- file.path(dir_lin, "manifesto_linhagem.json")
  man <- if (file.exists(path_man)) monitora_v307_justificativas_json_ler(path_man) else list()
  ctx <- monitora_v307_justificativas_contexto(input_dir, arquivo_registros)
  partes <- list(); aliases <- list(); fontes <- list(); recuperacoes <- list(); estados <- character(); provas_replay <- list()
  adicionar <- function(h) {
    partes[[length(partes) + 1L]] <<- h
    aliases[[length(aliases) + 1L]] <<- attr(h, "monitora_v307_aliases")
    fontes <<- c(fontes, attr(h, "monitora_v307_fontes"))
    r <- attr(h, "monitora_v307_recuperacao")
    if (!is.null(r)) recuperacoes[[length(recuperacoes) + 1L]] <<- r
    pr <- attr(h, "monitora_v307_replay_prova")
    if (!is.null(pr)) provas_replay[[length(provas_replay) + 1L]] <<- pr
    estados <<- c(estados, monitora_v307_justificativas_valor(attr(h, "monitora_v307_contexto"), "status"))
  }
  declarado <- nzchar(monitora_v307_justificativas_valor(man, "justificativas_schema"))
  hist_path <- file.path(dir_lin, "justificativas_pendencias_consolidada.csv")
  if (declarado) {
    rev <- monitora_v307_justificativas_valor(man, "revision_id")
    if (nzchar(ctx$data_sha256)) {
      adicionar(monitora_v307_justificativas_validar_importacao(dir_lin, man, data_sha256 = ctx$data_sha256, revision_id = rev))
    } else {
      prova <- ctx$replay
      if (is.null(prova) || !identical(monitora_v307_justificativas_valor(man, "justificativas_replay_schema"), "monitora_justificativas_replay_v1")) stop("Justificativas sem registros_corrig exigem prova física de brutos+ledger de replay; legado requer recuperação explícita/oráculo validado.", call. = FALSE)
      fp <- monitora_v307_justificativas_fingerprint_brutos()
      firmado <- monitora_v307_justificativas_fingerprint_inventario(man$justificativas_brutos_arquivos)
      if (!identical(fp$sha256, prova$brutos_sha256) || !identical(fp$sha256, firmado$sha256) ||
          !identical(fp$sha256, monitora_v307_justificativas_valor(man, "justificativas_brutos_sha256")) ||
          !identical(monitora_v307_justificativas_hash(prova$ledger_path, arquivo = TRUE), prova$ledger_sha256) ||
          !identical(prova$ledger_sha256, monitora_v307_justificativas_valor(man, "justificativas_replay_ledger_sha256"))) stop("Prova de replay das justificativas diverge dos brutos/ledger físicos atuais.", call. = FALSE)
      adicionar(monitora_v307_justificativas_validar_importacao(dir_lin, man, data_sha256 = monitora_v307_justificativas_valor(man, "output_data_sha256"), revision_id = rev))
    }
  } else if (file.exists(file.path(dir_lin, "manifesto_recuperacao_justificativas.json"))) {
    adicionar(monitora_v307_justificativas_recuperacao_ler(dir_lin, man, ctx))
  } else if (file.exists(hist_path)) {
    stop("Histórico legado de justificativas sem contrato: execute recuperação explícita em cópia com manifesto e snapshot de origem.", call. = FALSE)
  } else if (file.exists(path_man) || !is.null(ctx$arquivo_registros)) {
    estados <- c(estados, "historico_legado_nao_declarado")
    if (isTRUE(avisar)) monitora_v307_justificativas_avisar("A linhagem legada não declara o histórico de justificativas. Sua ausência não comprova que não existam decisões anteriores; recuperação auditável pode ser necessária.", path_man)
  }
  # Apenas raízes operacionais conhecidas. Cada uma exige um manifesto próprio;
  # ele só é elegível para a execução atual, ou para a mesma base+revisão.
  dirs <- unique(c(file.path(output_dir, "02_painel_correcoes", "operacoes"),
    file.path(output_dir, "02_painel_correcoes", "operacoes_sessao"),
    file.path(output_dir, "correcoes_campos"),
    as.character(get0("MONITORA_CORRECOES_DIR", ifnotfound = file.path(output_dir, "correcoes_campos"), inherits = TRUE))[1L]))
  exec_atual <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  for (d in dirs) {
    pm <- file.path(d, "manifesto_justificativas.json")
    ph <- file.path(d, "justificativas_pendencias_consolidada.csv")
    if (!file.exists(pm)) {
      if (file.exists(ph)) stop("Histórico operacional sem manifesto de continuidade: ", ph, ". Recuperação explícita é obrigatória.", call. = FALSE)
      next
    }
    om <- monitora_v307_justificativas_json_ler(pm)
    mesmo_exec <- nzchar(exec_atual) && identical(exec_atual, monitora_v307_justificativas_valor(om, "justificativas_exec_id")) &&
      nzchar(ctx$memoria_sha256) && monitora_v307_justificativas_valor(om, "justificativas_memoria_sha256") %in% ctx$memorias_autorizadas
    rev_ctx <- if (is.na(ctx$revision_id)) "" else ctx$revision_id
    mesma_base <- nzchar(ctx$data_sha256) && identical(ctx$data_sha256, monitora_v307_justificativas_valor(om, "justificativas_input_data_sha256")) &&
      identical(rev_ctx, monitora_v307_justificativas_valor(om, "justificativas_parent_revision_id"))
    mesmo_cache <- FALSE
    if (!nzchar(ctx$data_sha256) && !is.null(ctx$cache) && nzchar(ctx$memoria_sha256)) {
      brutos <- monitora_v307_justificativas_fingerprint_brutos(input_dir = input_dir)
      mesmo_cache <- nzchar(brutos$sha256) && isTRUE(om$justificativas_memoria_confirmada) &&
        ctx$cache$memoria_sha256 %in% ctx$memorias_autorizadas &&
        identical(ctx$cache$memoria_sha256, monitora_v307_justificativas_valor(om, "justificativas_memoria_sha256")) &&
        identical(ctx$cache_input_sha256, ctx$cache$input_sha256) &&
        identical(ctx$cache_input_sha256, monitora_v307_justificativas_valor(om, "justificativas_cache_input_sha256")) &&
        identical(ctx$cache_config_sha256, ctx$cache$config_sha256) &&
        identical(ctx$cache_config_sha256, monitora_v307_justificativas_valor(om, "justificativas_cache_config_sha256"))
    }
    if (!mesmo_exec && !mesma_base && !mesmo_cache) {
      if (!nzchar(ctx$data_sha256)) {
        nao_vazio <- monitora_v307_justificativas_validar_importacao(d, om)
        if (nrow(nao_vazio)) stop("Histórico confirmado não corresponde ao cache/base atual. Use o checkpoint registros_corrig correspondente com sua pasta linhagem; não é permitido substituir esse histórico por vazio.", call. = FALSE)
        next
      }
      if (isTRUE(avisar)) monitora_v307_justificativas_avisar(paste0("Histórico de outra base/execução ignorado no output: ", d), pm)
      next
    }
    adicionar(monitora_v307_justificativas_validar_importacao(d, om))
  }
  # O objeto confirmado em memória só é aceito se a publicação operacional,
  # explicitamente registrada, ainda confere fisicamente com seu manifesto.
  out <- monitora_v307_justificativas_juntar(partes)
  attr(out, "monitora_v307_aliases") <- monitora_v307_justificativas_juntar(aliases, aliases = TRUE)
  attr(out, "monitora_v307_fontes") <- fontes
  attr(out, "monitora_v307_recuperacoes") <- recuperacoes
  if (length(provas_replay)) {
    # Publicações operacionais podem ser mais novas, mas não perder a origem bruta.
    inventario <- list()
    for (p in provas_replay) for (f in p$arquivos) {
      nome <- monitora_v307_justificativas_valor(f, "path")
      ant <- inventario[[nome]]
      if (!is.null(ant) && !identical(monitora_v307_justificativas_valor(ant, "sha256"), monitora_v307_justificativas_valor(f, "sha256"))) stop("Fontes brutas históricas conflitantes para o mesmo caminho.", call. = FALSE)
      inventario[[nome]] <- f
    }
    fp <- monitora_v307_justificativas_fingerprint_inventario(unname(inventario))
    attr(out, "monitora_v307_replay_prova") <- list(brutos_sha256 = fp$sha256, arquivos = fp$arquivos, ledger_sha256 = provas_replay[[length(provas_replay)]]$ledger_sha256)
  }
  attr(out, "monitora_v307_contexto") <- list(status = if (length(estados)) paste(unique(estados), collapse = ";") else "sem_historico_em_entrada_nova", data_sha256 = ctx$data_sha256, revision_id = ctx$revision_id)
  out[]
}

monitora_v307_justificativas_projetar <- function(ocorrencias, historico, aliases = NULL) {
  oc <- data.table::copy(data.table::as.data.table(ocorrencias))
  id <- monitora_v307_justificativas_identidade(oc)
  aa <- monitora_v307_justificativas_juntar(list(aliases, attr(historico, "monitora_v307_aliases"), monitora_v307_justificativas_aliases(oc)), aliases = TRUE)
  oc[, ocorrencia_id := id$ocorrencia_id]
  if (anyDuplicated(oc$ocorrencia_id)) stop("Diagnóstico contém identidades de ocorrência duplicadas.", call. = FALSE)
  hist <- monitora_v307_justificativas_eventos_validar(historico)
  hist_proj <- data.table::copy(hist)
  hist_proj[, ocorrencia_id_canonico := aa$ocorrencia_id_canonico[match(ocorrencia_id, aa$alias_ocorrencia_id)]]
  if (nrow(hist_proj)) {
    hist_proj[, ordem_status_tmp := match(status_evento, c("vigente", "encerrada_por_resolucao", "revogada", "orfa"))]
    data.table::setorderv(hist_proj, c("timestamp_evento", "ordem_status_tmp", "evento_justificativa_id"))
    ult <- hist_proj[!is.na(ocorrencia_id_canonico), .SD[.N], by = ocorrencia_id_canonico]
  } else ult <- hist_proj
  # Campos de estado eventualmente recebidos de um snapshot anterior não são evidência.
  cols_estado <- c("evento_justificativa_id", "evento_lote_id", "ordem_no_lote", "n_ocorrencias_lote", "status_justificativa", "responsavel_justificativa", "tipo_justificativa", "justificativa", "timestamp_justificativa", "ocorrencia_id_historico", "status_ultimo_evento")
  for (cc in intersect(cols_estado, names(oc))) oc[, (cc) := NULL]
  idx <- match(oc$ocorrencia_id, ult$ocorrencia_id_canonico)
  oc[, `:=`(
    evento_justificativa_id = ult$evento_justificativa_id[idx], evento_lote_id = ult$evento_lote_id[idx],
    ordem_no_lote = ult$ordem_no_lote[idx], n_ocorrencias_lote = ult$n_ocorrencias_lote[idx],
    status_justificativa = ifelse(!is.na(idx) & ult$status_evento[idx] == "vigente", "vigente", "sem_justificativa"),
    status_ultimo_evento = ult$status_evento[idx], ocorrencia_id_historico = ult$ocorrencia_id[idx],
    responsavel_justificativa = ult$responsavel[idx], tipo_justificativa = ult$tipo_justificativa[idx],
    justificativa = ult$justificativa[idx], timestamp_justificativa = ult$timestamp_evento[idx]
  )]
  list(historico = hist, pendencias_remanescentes = oc[], pendencias_a_justificar = oc[status_justificativa != "vigente"],
    aliases = aa, historico_projetado = hist_proj[], identidade_compativel = all(id$identidade_compativel))
}

monitora_v307_justificativas_preflight <- function(eventos_sessao, ocorrencias_atuais, historico = NULL) {
  tryCatch({
    h <- if (is.null(historico)) monitora_v307_justificativas_ler_historico() else historico
    sess <- monitora_v307_justificativas_eventos_validar(eventos_sessao, "sessão")
    if (!nrow(sess)) return(list(ok = TRUE, erro = "", dados = sess))
    if (any(sess$status_evento != "vigente") || anyDuplicated(sess$ocorrencia_id) || any(nchar(trimws(sess$justificativa)) < 20L)) stop("Novas justificativas devem ser vigentes, únicas e conter pelo menos 20 caracteres.", call. = FALSE)
    est <- monitora_v307_justificativas_projetar(ocorrencias_atuais, h)
    ids <- est$aliases$ocorrencia_id_canonico[match(sess$ocorrencia_id, est$aliases$alias_ocorrencia_id)]
    if (anyNA(ids) || any(!ids %in% est$pendencias_remanescentes$ocorrencia_id) || anyDuplicated(ids)) stop("Há justificativa fora do diagnóstico atual ou identidade ambígua.", call. = FALSE)
    vigentes <- est$pendencias_remanescentes[status_justificativa == "vigente", ocorrencia_id]
    # Repetir exatamente um evento confirmado é idempotente; outro evento é duplicata.
    repetidos <- sess$evento_justificativa_id %in% h$evento_justificativa_id
    if (any(repetidos)) monitora_v307_justificativas_juntar(list(h, sess[repetidos]))
    if (any(ids %in% vigentes & !repetidos)) stop("Há ocorrência com justificativa histórica vigente; uma nova justificativa exige revisão/revogação explícita.", call. = FALSE)
    complemento <- attr(eventos_sessao, "monitora_v307_complemento_lotes", exact = TRUE)
    # Lotes reconstruídos por exclusão são operações novas da sessão. Suas
    # origens encerradas conservam os lotes originais e não são reconstituídas.
    if (!is.null(complemento)) complemento <- data.table::as.data.table(complemento)[evento_lote_id %in% sess$evento_lote_id]
    lotes_validar <- monitora_v307_justificativas_juntar(list(sess, complemento))
    lotes <- lotes_validar[, {
      ord <- suppressWarnings(as.integer(ordem_no_lote)); card <- suppressWarnings(as.integer(n_ocorrencias_lote))
      .(ok = !anyNA(ord) && !anyNA(card) && identical(sort(ord), seq_len(.N)) && all(card == .N) &&
        data.table::uniqueN(timestamp_evento) == 1L && data.table::uniqueN(responsavel) == 1L &&
        data.table::uniqueN(tipo_justificativa) == 1L && data.table::uniqueN(justificativa) == 1L)
    }, by = evento_lote_id]
    if (any(!lotes$ok) || any(!nzchar(sess$evento_lote_id))) stop("Lote de justificativas incompleto ou com metadados divergentes.", call. = FALSE)
    attr(sess, "monitora_v307_aliases") <- attr(eventos_sessao, "monitora_v307_aliases", exact = TRUE)
    attr(sess, "monitora_v307_complemento_lotes") <- attr(eventos_sessao, "monitora_v307_complemento_lotes", exact = TRUE)
    list(ok = TRUE, erro = "", dados = sess)
  }, error = function(e) list(ok = FALSE, erro = conditionMessage(e), dados = eventos_sessao))
}

monitora_v307_justificativas_estado <- function(ocorrencias, eventos_sessao = NULL) {
  h <- monitora_v307_justificativas_ler_historico()
  validacao <- monitora_v307_justificativas_preflight(eventos_sessao, ocorrencias, h)
  if (!isTRUE(validacao$ok)) stop("Estado de justificativas inválido: ", validacao$erro, call. = FALSE)
  candidato <- monitora_v307_justificativas_juntar(list(h, validacao$dados))
  aa <- monitora_v307_justificativas_juntar(list(attr(h, "monitora_v307_aliases"), attr(eventos_sessao, "monitora_v307_aliases")), aliases = TRUE)
  est <- monitora_v307_justificativas_projetar(ocorrencias, candidato, aa)
  est$historico <- h
  est$historico_com_sessao <- candidato
  est$contexto <- attr(h, "monitora_v307_contexto")
  est
}

monitora_v307_justificativas_resolucao_permitida <- function(ocorrencias, tipos) {
  completo <- isTRUE(attr(ocorrencias, "monitora_diagnostico_completo", exact = TRUE))
  familias <- as.character(attr(ocorrencias, "monitora_familias_avaliadas", exact = TRUE))
  compativel <- all(monitora_v307_justificativas_identidade(ocorrencias)$identidade_compativel)
  completo && compativel && length(familias) > 0L && all("*" %in% familias | tipos %in% familias)
}

monitora_v307_justificativas_encerrar <- function(originais, timestamp_evento) {
  z <- data.table::copy(originais)
  if (!nrow(z)) return(z)
  origem <- z$evento_justificativa_id
  ids <- vapply(origem, function(id) paste0("jst_", substr(monitora_v307_justificativas_hash(monitora_v307_justificativas_pacote(c("justificativa_encerramento_v1", id, timestamp_evento))), 1L, 24L)), character(1L))
  z[, `:=`(evento_justificativa_id = ids, evento_origem_id = origem,
    status_evento = "encerrada_por_resolucao", timestamp_evento = timestamp_evento,
    exec_id = as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE)),
    script_versao = as.character(get0("MONITORA_SCRIPT_VERSAO", ifnotfound = "", inherits = TRUE)))]
  z[]
}

monitora_v307_justificativas_reconciliar_sessao <- function(
    eventos_sessao, ocorrencias_atuais, timestamp_evento = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")
) {
  ini <- proc.time()[["elapsed"]]
  sess <- monitora_v307_justificativas_eventos_validar(eventos_sessao, "sessão em reconciliação")
  est <- monitora_v307_justificativas_projetar(ocorrencias_atuais, sess, attr(eventos_sessao, "monitora_v307_aliases"))
  ids <- est$aliases$ocorrencia_id_canonico[match(sess$ocorrencia_id, est$aliases$alias_ocorrencia_id)]
  ausentes <- which(!is.na(ids) & !ids %in% est$pendencias_remanescentes$ocorrencia_id)
  permitidas <- if (length(ausentes)) vapply(ausentes, function(i) monitora_v307_justificativas_resolucao_permitida(ocorrencias_atuais, sess$tipo_ocorrencia[i]), logical(1L)) else logical()
  resolver <- ausentes[permitidas]
  orig <- sess[resolver]
  fech <- monitora_v307_justificativas_encerrar(orig, timestamp_evento)
  # Não reconstruir IDs ou cardinalidades de eventos já emitidos.
  # Ativas continuam com seus lotes originais; só novas operações são validadas como lotes completos.
  ativas <- if (length(resolver)) sess[-resolver] else sess
  attr(ativas, "monitora_v307_aliases") <- est$aliases
  attr(ativas, "monitora_v307_complemento_lotes") <- monitora_v307_justificativas_juntar(list(attr(eventos_sessao, "monitora_v307_complemento_lotes", exact = TRUE), orig))
  eventos_encerrados <- monitora_v307_justificativas_juntar(list(orig, fech))
  attr(eventos_encerrados, "monitora_v307_aliases") <- est$aliases
  aud <- data.table::data.table(
    ocorrencia_id = orig$ocorrencia_id, evento_justificativa_id = orig$evento_justificativa_id,
    evento_encerramento_id = fech$evento_justificativa_id, tipo_ocorrencia = orig$tipo_ocorrencia,
    COLETA = orig$COLETA, UC = orig$UC, EA = orig$EA, UA = orig$UA, ANO = orig$ANO,
    resultado_reconciliacao = rep("encerrada_por_resolucao", nrow(orig)), timestamp_reconciliacao = rep(timestamp_evento, nrow(orig))
  )
  list(ativas = ativas, eventos_encerrados = eventos_encerrados, auditoria = aud,
    n_ativas = nrow(ativas), n_encerradas = nrow(orig), tempo_s = proc.time()[["elapsed"]] - ini)
}

monitora_v307_justificativas_preparar <- function(
    ocorrencias_atuais, eventos_sessao = NULL, eventos_encerrados_sessao = NULL
) {
  h <- monitora_v307_justificativas_ler_historico()
  v <- monitora_v307_justificativas_preflight(eventos_sessao, ocorrencias_atuais, h)
  if (!isTRUE(v$ok)) stop("Candidato de justificativas rejeitado: ", v$erro, call. = FALSE)
  encerrados <- monitora_v307_justificativas_eventos_validar(eventos_encerrados_sessao, "encerramentos da sessão")
  if (nrow(encerrados)) {
    if (!monitora_v307_justificativas_resolucao_permitida(ocorrencias_atuais, unique(encerrados$tipo_ocorrencia))) stop("Resolução requer diagnóstico completo, famílias avaliadas e identidade compatível.", call. = FALSE)
    proj <- monitora_v307_justificativas_projetar(ocorrencias_atuais, encerrados, attr(h, "monitora_v307_aliases"))
    presentes <- proj$historico_projetado$ocorrencia_id_canonico %in% proj$pendencias_remanescentes$ocorrencia_id
    if (any(presentes)) stop("Uma justificativa encerrada ainda pertence ao diagnóstico atual.", call. = FALSE)
  }
  hist <- monitora_v307_justificativas_juntar(list(h, v$dados, encerrados))
  aliases <- monitora_v307_justificativas_juntar(list(attr(h, "monitora_v307_aliases"), attr(eventos_sessao, "monitora_v307_aliases"), attr(eventos_encerrados_sessao, "monitora_v307_aliases"), monitora_v307_justificativas_aliases(ocorrencias_atuais)), aliases = TRUE)
  est <- monitora_v307_justificativas_projetar(ocorrencias_atuais, hist, aliases)
  ult <- est$historico_projetado[!is.na(ocorrencia_id_canonico)]
  if (nrow(ult)) ult <- ult[, .SD[.N], by = ocorrencia_id_canonico]
  ausentes <- ult[status_evento == "vigente" & !ocorrencia_id_canonico %in% est$pendencias_remanescentes$ocorrencia_id]
  if (nrow(ausentes)) {
    pode <- vapply(ausentes$tipo_ocorrencia, function(tipo) monitora_v307_justificativas_resolucao_permitida(ocorrencias_atuais, tipo), logical(1L))
    orig <- hist[match(ausentes$evento_justificativa_id[pode], hist$evento_justificativa_id)]
    if (nrow(orig)) hist <- monitora_v307_justificativas_juntar(list(hist, monitora_v307_justificativas_encerrar(orig, format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"))))
  }
  est <- monitora_v307_justificativas_projetar(ocorrencias_atuais, hist, aliases)
  dir_saida <- file.path(get0("MONITORA_OUTPUT_DIR", ifnotfound = "output", inherits = TRUE), "02_painel_correcoes", "operacoes")
  list(historico = hist, pendencias_remanescentes = est$pendencias_remanescentes, aliases = aliases,
    fontes = attr(h, "monitora_v307_fontes"), recuperacoes = attr(h, "monitora_v307_recuperacoes"), replay_prova = attr(h, "monitora_v307_replay_prova"),
    contexto = attr(h, "monitora_v307_contexto"),
    arquivo_historico = file.path(dir_saida, "justificativas_pendencias_consolidada.csv"),
    arquivo_pendencias = file.path(dir_saida, "pendencias_justificadas.csv"))
}

monitora_v307_justificativas_csv_escrever <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(data.table::as.data.table(x), path, na = "", quote = "auto", bom = FALSE)
  lido <- monitora_v307_justificativas_csv_ler(path)
  esperado <- data.table::as.data.table(x)
  if (!identical(names(lido), names(esperado)) || nrow(lido) != nrow(esperado)) stop("Candidato de justificativas não preservou schema/contagem em ", basename(path), ": esperado ", nrow(esperado), "x", ncol(esperado), "; observado ", nrow(lido), "x", ncol(lido), ".", call. = FALSE)
  for (cc in names(esperado)) {
    a <- as.character(esperado[[cc]]); a[is.na(a)] <- ""
    if (!identical(enc2utf8(lido[[cc]]), enc2utf8(a))) stop("Candidato de justificativas alterou conteúdo: ", cc, call. = FALSE)
  }
  invisible(path)
}

monitora_v307_justificativas_copiar <- function(origem, destino, sha = NULL) {
  if (is.null(sha)) sha <- monitora_v307_justificativas_hash(origem, arquivo = TRUE)
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (!identical(normalizePath(origem, mustWork = TRUE), normalizePath(destino, mustWork = FALSE))) {
    if (!isTRUE(file.copy(origem, destino, overwrite = TRUE, copy.mode = FALSE))) stop("Falha ao preservar fonte histórica de justificativas.", call. = FALSE)
  }
  if (!identical(monitora_v307_justificativas_hash(destino, arquivo = TRUE), sha)) stop("Cópia de fonte histórica alterou bytes.", call. = FALSE)
  invisible(destino)
}

monitora_v307_justificativas_fontes_planejar <- function(fontes) {
  if (!length(fontes)) return(character())
  hashes <- vapply(fontes, function(f) as.character(f$sha256)[1L], character(1L))
  if (anyNA(hashes) || any(!grepl("^[a-f0-9]{64}$", hashes))) stop("SHA256 incompleto de fonte histórica.", call. = FALSE)
  rel <- paste0("justificativas_fontes/", substr(hashes, 1L, 24L), ".csv")
  if (anyDuplicated(rel[!duplicated(hashes)])) stop("Colisão de prefixo no caminho de fontes históricas; nenhum arquivo pode ser sobrescrito.", call. = FALSE)
  rel
}

monitora_v307_justificativas_fonte_copiar <- function(origem, destino, sha) {
  if (!identical(monitora_v307_justificativas_hash(origem, arquivo = TRUE), sha)) stop("Fonte histórica mudou antes da cópia imutável.", call. = FALSE)
  if (file.exists(destino)) {
    if (!identical(monitora_v307_justificativas_hash(destino, arquivo = TRUE), sha)) stop("Colisão de prefixo no destino de fonte histórica; arquivo existente preservado.", call. = FALSE)
    return(invisible(destino))
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(origem, destino, overwrite = FALSE, copy.mode = FALSE)) ||
      !identical(monitora_v307_justificativas_hash(destino, arquivo = TRUE), sha)) stop("Falha na cópia imutável da fonte histórica.", call. = FALSE)
  invisible(destino)
}

monitora_v307_justificativas_materializar <- function(dir_destino, preparado, data_sha256, revision_id, snapshot = FALSE) {
  if (!grepl("^[a-f0-9]{64}$", data_sha256) || !nzchar(revision_id)) stop("Publicação de justificativas exige hash de dados e revisão.", call. = FALSE)
  h <- monitora_v307_justificativas_eventos_validar(preparado$historico)
  a <- monitora_v307_justificativas_aliases_validar(preparado$aliases)
  if (nrow(h) && any(!h$ocorrencia_id %in% a$alias_ocorrencia_id)) stop("Publicação contém histórico sem identidade comprovada.", call. = FALSE)
  rel_fontes <- monitora_v307_justificativas_fontes_planejar(preparado$fontes)
  dir.create(dir_destino, recursive = TRUE, showWarnings = FALSE)
  ph <- file.path(dir_destino, "justificativas_pendencias_consolidada.csv")
  pa <- file.path(dir_destino, "justificativas_ocorrencias_aliases.csv")
  # Sem eventos novos, a fonte física inteira também pode ser preservada literalmente.
  copiado <- FALSE
  for (f in preparado$fontes) {
    if (!file.exists(f$path) || !identical(monitora_v307_justificativas_hash(f$path, arquivo = TRUE), f$sha256)) stop("Fonte histórica mudou após o preparo.", call. = FALSE)
    candidato <- monitora_v307_justificativas_eventos_validar(monitora_v307_justificativas_csv_ler(f$path))
    if (isTRUE(all.equal(as.data.frame(candidato), as.data.frame(h), check.attributes = FALSE))) {
      monitora_v307_justificativas_copiar(f$path, ph, f$sha256); copiado <- TRUE; break
    }
  }
  if (!copiado) monitora_v307_justificativas_csv_escrever(h, ph)
  monitora_v307_justificativas_csv_escrever(a, pa)
  fontes <- list(); ja <- character()
  for (i in seq_along(preparado$fontes)) {
    f <- preparado$fontes[[i]]
    if (f$sha256 %in% ja) next
    rel <- rel_fontes[i]
    monitora_v307_justificativas_fonte_copiar(f$path, file.path(dir_destino, rel), f$sha256)
    fontes[[length(fontes) + 1L]] <- list(path = rel, sha256 = f$sha256); ja <- c(ja, f$sha256)
  }
  ctx <- monitora_v307_justificativas_contexto()
  man <- list(
    justificativas_schema = "monitora_justificativas_v1", justificativas_identity_schema = "monitora_ocorrencia_v1",
    justificativas_path = basename(ph), justificativas_sha256 = monitora_v307_justificativas_hash(ph, arquivo = TRUE),
    justificativas_event_count = as.integer(nrow(h)), justificativas_ocorrencias_count = as.integer(data.table::uniqueN(h$ocorrencia_id)),
    justificativas_aliases_path = basename(pa), justificativas_aliases_sha256 = monitora_v307_justificativas_hash(pa, arquivo = TRUE), justificativas_aliases_count = as.integer(nrow(a)),
    justificativas_data_sha256 = data_sha256, justificativas_revision_id = revision_id,
    justificativas_exec_id = as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE)),
    justificativas_input_data_sha256 = ctx$data_sha256,
    justificativas_parent_revision_id = if (is.na(ctx$revision_id)) "" else ctx$revision_id,
    justificativas_memoria_sha256 = ctx$memoria_sha256,
    justificativas_memoria_confirmada = isTRUE(ctx$memoria_confirmada),
    justificativas_cache_input_sha256 = ctx$cache_input_sha256,
    justificativas_cache_config_sha256 = ctx$cache_config_sha256,
    justificativas_binding_schema = if (identical(data_sha256, ctx$memoria_sha256)) "monitora_base_memoria_v1" else "monitora_arquivo_sha256_v1",
    justificativas_fontes = fontes
  )
  if (isTRUE(snapshot)) {
    ps <- file.path(dir_destino, "pendencias_justificadas.csv")
    monitora_v307_justificativas_csv_escrever(preparado$pendencias_remanescentes, ps)
    man$justificativas_snapshot_path <- basename(ps)
    man$justificativas_snapshot_sha256 <- monitora_v307_justificativas_hash(ps, arquivo = TRUE)
    man$justificativas_snapshot_count <- as.integer(nrow(preparado$pendencias_remanescentes))
  }
  if (!is.null(preparado$replay_prova)) {
    pr <- preparado$replay_prova
    fp <- monitora_v307_justificativas_fingerprint_inventario(pr$arquivos)
    ledger_hash <- monitora_v307_justificativas_valor(pr, "ledger_sha256")
    if (!nzchar(fp$sha256) || !grepl("^[a-f0-9]{64}$", ledger_hash)) stop("Prova de replay incompleta na publicação de justificativas.", call. = FALSE)
    man$justificativas_replay_schema <- "monitora_justificativas_replay_v1"
    man$justificativas_brutos_sha256 <- fp$sha256
    man$justificativas_brutos_arquivos <- fp$arquivos
    man$justificativas_replay_ledger_sha256 <- ledger_hash
  }
  recs <- preparado$recuperacoes
  if (length(recs)) {
    hashes <- unique(vapply(recs, function(r) r$sha256, character(1L)))
    if (length(hashes) != 1L) stop("Manifestos distintos de recuperação exigem reconciliação explícita.", call. = FALSE)
    rr <- recs[[1L]]
    # A prova bruta do snapshot acompanha permanentemente seu manifesto de recuperação.
    recman <- monitora_v307_justificativas_json_ler(rr$path)
    prova <- file.path(dirname(rr$path), "justificativas_snapshot_recuperacao.csv")
    monitora_v307_justificativas_copiar(prova, file.path(dir_destino, basename(prova)), monitora_v307_justificativas_valor(recman, "source_snapshot_sha256"))
    destino_rec <- file.path(dir_destino, "manifesto_recuperacao_justificativas.json")
    monitora_v307_justificativas_copiar(rr$path, destino_rec, rr$sha256)
    man$justificativas_recuperacao_path <- basename(destino_rec)
    man$justificativas_recuperacao_sha256 <- rr$sha256
  }
  jsonlite::write_json(man, file.path(dir_destino, "manifesto_justificativas.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
  monitora_v307_justificativas_validar_importacao(dir_destino, man, data_sha256 = data_sha256, revision_id = revision_id)
  man
}

monitora_v307_justificativas_publicar_diretorio <- function(stage, destino) {
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  arquivos <- list.files(stage, recursive = TRUE, all.files = FALSE, full.names = FALSE, include.dirs = FALSE)
  # Manifesto é o marcador de publicação e sempre é promovido por último.
  arquivos <- c(setdiff(arquivos, "manifesto_justificativas.json"), intersect(arquivos, "manifesto_justificativas.json"))
  fontes <- startsWith(arquivos, "justificativas_fontes/")
  for (i in which(fontes & file.exists(file.path(destino, arquivos)))) {
    if (!identical(monitora_v307_justificativas_hash(file.path(stage, arquivos[i]), arquivo = TRUE), monitora_v307_justificativas_hash(file.path(destino, arquivos[i]), arquivo = TRUE))) stop("Colisão de prefixo no destino de fonte histórica; publicação bloqueada sem sobrescrever arquivos.", call. = FALSE)
  }
  backup <- tempfile("justificativas_backup_"); dir.create(backup)
  existiam <- file.exists(file.path(destino, arquivos)); promovidos <- rep(FALSE, length(arquivos)); ok <- FALSE
  for (i in which(existiam)) monitora_v307_justificativas_copiar(file.path(destino, arquivos[i]), file.path(backup, arquivos[i]))
  on.exit({
    if (!ok) {
      for (i in seq_along(arquivos)) if (promovidos[i]) {
        if (existiam[i]) monitora_v307_justificativas_copiar(file.path(backup, arquivos[i]), file.path(destino, arquivos[i])) else unlink(file.path(destino, arquivos[i]), force = TRUE)
      }
    }
    # Se a restauração falhar, o erro interrompe antes de apagar o backup.
    unlink(backup, recursive = TRUE)
  }, add = TRUE)
  for (i in seq_along(arquivos)) {
    path <- file.path(destino, arquivos[i]); dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (fontes[i] && file.exists(path)) next
    # Registrar antes da cópia protege também um destino parcialmente escrito.
    promovidos[i] <- TRUE
    if (fontes[i]) monitora_v307_justificativas_fonte_copiar(file.path(stage, arquivos[i]), path, monitora_v307_justificativas_hash(file.path(stage, arquivos[i]), arquivo = TRUE)) else monitora_v307_justificativas_copiar(file.path(stage, arquivos[i]), path)
  }
  ok <- TRUE
  invisible(destino)
}

monitora_v307_justificativas_persistir_preparado <- function(preparado) {
  obrigatorios <- c("historico", "pendencias_remanescentes", "aliases", "arquivo_historico", "arquivo_pendencias")
  if (!is.list(preparado) || !all(obrigatorios %in% names(preparado))) stop("Candidato preparado de justificativas inválido.", call. = FALSE)
  destino <- dirname(preparado$arquivo_historico)
  esperado <- file.path(get0("MONITORA_OUTPUT_DIR", ifnotfound = "output", inherits = TRUE), "02_painel_correcoes", "operacoes")
  if (!identical(normalizePath(destino, mustWork = FALSE), normalizePath(esperado, mustWork = FALSE))) stop("Destino operacional de justificativas não autorizado.", call. = FALSE)
  ctx <- monitora_v307_justificativas_contexto()
  data_hash <- ctx$data_sha256
  if (!nzchar(data_hash)) data_hash <- ctx$memoria_sha256
  if (!nzchar(data_hash)) stop("Publicação de justificativas bloqueada: registre explicitamente a base atual antes do painel; CSV residual do output não comprova essa base.", call. = FALSE)
  exec_id <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  if (is.na(exec_id) || !nzchar(exec_id)) stop("Publicação de justificativas exige exec_id da sessão atual.", call. = FALSE)
  stage <- tempfile("justificativas_candidato_"); dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  man <- monitora_v307_justificativas_materializar(stage, preparado, data_hash, paste0("sessao_", exec_id), snapshot = TRUE)
  monitora_v307_justificativas_publicar_diretorio(stage, destino)
  resultado <- preparado; resultado$manifesto_justificativas <- man
  assign("MONITORA_PENDENCIAS_JUSTIFICATIVAS_ULTIMO", resultado, envir = .GlobalEnv)
  invisible(resultado)
}

monitora_v307_justificativas_exportar <- function(dir_lin, output_data_sha256, revision_id, preparado = NULL, ledger_sha256 = NULL) {
  if (is.null(preparado)) {
    h <- monitora_v307_justificativas_ler_historico()
    preparado <- list(historico = h, aliases = attr(h, "monitora_v307_aliases"), fontes = attr(h, "monitora_v307_fontes"), recuperacoes = attr(h, "monitora_v307_recuperacoes"), replay_prova = attr(h, "monitora_v307_replay_prova"))
  }
  fp_atual <- monitora_v307_justificativas_fingerprint_brutos()
  inventario <- list()
  for (f in c(preparado$replay_prova$arquivos, fp_atual$arquivos)) {
    nome <- monitora_v307_justificativas_valor(f, "path"); ant <- inventario[[nome]]
    if (!is.null(ant) && !identical(monitora_v307_justificativas_valor(ant, "sha256"), monitora_v307_justificativas_valor(f, "sha256"))) stop("Brutos novos conflitam com a proveniência herdada; não sobrescrever inventário.", call. = FALSE)
    inventario[[nome]] <- f
  }
  fp <- monitora_v307_justificativas_fingerprint_inventario(unname(inventario))
  ledger_path <- file.path(dir_lin, "correcoes_semanticas_consolidada.csv")
  if (file.exists(ledger_path)) {
    fisico <- monitora_v307_justificativas_hash(ledger_path, arquivo = TRUE)
    if (!is.null(ledger_sha256) && !identical(ledger_sha256, fisico)) stop("Ledger físico diverge do vínculo de replay solicitado.", call. = FALSE)
    ledger_sha256 <- fisico
  }
  if (nzchar(fp$sha256) && !is.null(ledger_sha256)) preparado$replay_prova <- list(brutos_sha256 = fp$sha256, arquivos = fp$arquivos, ledger_sha256 = ledger_sha256)
  stage <- tempfile("justificativas_linhagem_"); dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  man <- monitora_v307_justificativas_materializar(stage, preparado, output_data_sha256, revision_id)
  monitora_v307_justificativas_publicar_diretorio(stage, dir_lin)
  man
}

monitora_v307_justificativas_candidato_linhagem <- function() {
  h <- monitora_v307_justificativas_ler_historico()
  list(historico = h, aliases = attr(h, "monitora_v307_aliases"), fontes = attr(h, "monitora_v307_fontes"), recuperacoes = attr(h, "monitora_v307_recuperacoes"), replay_prova = attr(h, "monitora_v307_replay_prova"))
}

monitora_v307_justificativas_assinaturas <- function(preparado = NULL) {
  if (is.null(preparado)) preparado <- monitora_v307_justificativas_candidato_linhagem()
  stage <- tempfile("assinaturas_justificativas_"); dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  man <- monitora_v307_justificativas_materializar(stage, preparado, paste(rep("0", 64L), collapse = ""), "candidato_sem_promocao")
  man[c("justificativas_sha256", "justificativas_aliases_sha256", "justificativas_event_count", "justificativas_aliases_count")]
}

monitora_v307_justificativas_adicionar_lote_atomico <- function(eventos_sessao, novo_lote, ocorrencias_atuais) {
  atual <- monitora_v307_justificativas_eventos_validar(eventos_sessao, "sessão")
  tryCatch({
    novo <- monitora_v307_justificativas_eventos_validar(novo_lote, "novo lote")
    if (!nrow(novo)) stop("O lote preparado está vazio.", call. = FALSE)
    aa <- monitora_v307_justificativas_juntar(list(attr(eventos_sessao, "monitora_v307_aliases"), attr(novo_lote, "monitora_v307_aliases"), monitora_v307_justificativas_aliases(ocorrencias_atuais)), aliases = TRUE)
    canonico <- function(z) aa$ocorrencia_id_canonico[match(z, aa$alias_ocorrencia_id)]
    if (length(intersect(canonico(atual$ocorrencia_id), canonico(novo$ocorrencia_id)))) stop("Há ocorrência já justificada nesta sessão.", call. = FALSE)
    candidato <- monitora_v307_justificativas_juntar(list(atual, novo))
    attr(candidato, "monitora_v307_aliases") <- aa
    attr(candidato, "monitora_v307_complemento_lotes") <- attr(eventos_sessao, "monitora_v307_complemento_lotes")
    v <- monitora_v307_justificativas_preflight(candidato, ocorrencias_atuais)
    if (!isTRUE(v$ok)) stop(v$erro, call. = FALSE)
    list(ok = TRUE, erro = "", dados = v$dados)
  }, error = function(e) list(ok = FALSE, erro = conditionMessage(e), dados = eventos_sessao))
}

monitora_v307_justificativas_recuperacao_ler <- function(dir_lin, manifesto_linhagem, contexto) {
  path <- file.path(dir_lin, "manifesto_recuperacao_justificativas.json")
  rec <- monitora_v307_justificativas_json_ler(path)
  val <- function(nm) monitora_v307_justificativas_valor(rec, nm)
  if (!identical(val("recovery_schema"), "monitora_recuperacao_justificativas_v1") ||
      !identical(val("snapshot_missing_policy"), "monitora_snapshot_legado_na_v1") ||
      !identical(val("base_data_sha256"), contexto$data_sha256) ||
      !identical(val("base_revision_id"), monitora_v307_justificativas_valor(manifesto_linhagem, "revision_id")) ||
      !identical(val("base_data_sha256"), monitora_v307_justificativas_valor(manifesto_linhagem, "output_data_sha256"))) stop("Recuperação de justificativas pertence a outra base/revisão.", call. = FALSE)
  ph <- file.path(dir_lin, "justificativas_pendencias_consolidada.csv")
  ps <- file.path(dir_lin, "justificativas_snapshot_recuperacao.csv")
  pa <- file.path(dir_lin, "justificativas_ocorrencias_aliases.csv")
  if (!identical(monitora_v307_justificativas_hash(ph, arquivo = TRUE), val("source_history_sha256")) ||
      !identical(monitora_v307_justificativas_hash(ps, arquivo = TRUE), val("source_snapshot_sha256")) ||
      !identical(monitora_v307_justificativas_hash(pa, arquivo = TRUE), val("aliases_sha256"))) stop("Hash da recuperação de justificativas diverge da fonte declarada.", call. = FALSE)
  hist <- monitora_v307_justificativas_eventos_validar(monitora_v307_justificativas_csv_ler(ph), "recuperação")
  snap <- monitora_v307_justificativas_snapshot_legado_ler(ps)
  aa <- monitora_v307_justificativas_aliases_validar(monitora_v307_justificativas_csv_ler(pa))
  comprovados <- monitora_v307_justificativas_aliases(snap, "snapshot_recuperacao")
  if (!setequal(paste(aa$alias_ocorrencia_id, aa$ocorrencia_id_canonico, aa$assinatura_sha256), paste(comprovados$alias_ocorrencia_id, comprovados$ocorrencia_id_canonico, comprovados$assinatura_sha256)) ||
      any(!hist$ocorrencia_id %in% aa$alias_ocorrencia_id)) stop("Aliases da recuperação não são comprovados pelo snapshot completo.", call. = FALSE)
  for (item in list(c("event_count", nrow(hist)), c("aliases_count", nrow(aa)), c("snapshot_count", nrow(snap)))) {
    if (!grepl("^(0|[1-9][0-9]*)$", val(item[1L])) || as.double(val(item[1L])) != as.double(item[2L])) stop("Contagem inválida no manifesto de recuperação.", call. = FALSE)
  }
  attr(hist, "monitora_v307_aliases") <- aa
  attr(hist, "monitora_v307_fontes") <- list(list(path = ph, sha256 = val("source_history_sha256")))
  attr(hist, "monitora_v307_recuperacao") <- list(path = path, sha256 = monitora_v307_justificativas_hash(path, arquivo = TRUE))
  attr(hist, "monitora_v307_contexto") <- list(status = "historico_legado_recuperado_explicitamente", data_sha256 = contexto$data_sha256, revision_id = val("base_revision_id"))
  hist[]
}

monitora_v307_justificativas_recuperar <- function(
    arquivo_historico, arquivo_snapshot, arquivo_registros, arquivo_manifesto_fonte, destino_linhagem
) {
  # Uso explícito: nunca procura pastas vizinhas e nunca edita as fontes.
  fonte <- monitora_v307_justificativas_json_ler(arquivo_manifesto_fonte)
  data_hash <- monitora_v307_justificativas_hash(arquivo_registros, arquivo = TRUE)
  revision <- monitora_v307_justificativas_valor(fonte, "revision_id")
  if (!nzchar(revision) || !identical(data_hash, monitora_v307_justificativas_valor(fonte, "output_data_sha256"))) stop("A base de recuperação não corresponde ao manifesto da fonte.", call. = FALSE)
  destino_manpath <- file.path(destino_linhagem, "manifesto_linhagem.json")
  if (!file.exists(destino_manpath)) stop("A cópia de destino deve conter seu manifesto de linhagem legado.", call. = FALSE)
  destino_man <- monitora_v307_justificativas_json_ler(destino_manpath)
  if (!identical(revision, monitora_v307_justificativas_valor(destino_man, "revision_id")) || !identical(data_hash, monitora_v307_justificativas_valor(destino_man, "output_data_sha256"))) stop("O destino não preserva exatamente a revisão/base comprovada da fonte.", call. = FALSE)
  hist <- monitora_v307_justificativas_eventos_validar(monitora_v307_justificativas_csv_ler(arquivo_historico), "fonte legada de recuperação")
  snap <- monitora_v307_justificativas_snapshot_legado_ler(arquivo_snapshot)
  completos <- c("tipo_ocorrencia", "COLETA", "monitora_row_id", "ocorrencia_token", "ocorrencia_atributo", "ocorrencia_categoria", "UC", "EA", "UA", "ANO", "CICLO", "CAMPANHA", "ocorrencia_id")
  if (!all(completos %in% names(snap)) || anyDuplicated(snap$ocorrencia_id)) stop("Snapshot de recuperação não contém chaves diagnósticas completas e únicas.", call. = FALSE)
  aa <- monitora_v307_justificativas_aliases(snap, "snapshot_recuperacao")
  if (any(!hist$ocorrencia_id %in% aa$alias_ocorrencia_id)) stop("Há evento histórico sem pareamento determinístico único no snapshot.", call. = FALSE)
  est <- monitora_v307_justificativas_projetar(snap, hist, aa)
  if ("status_justificativa" %in% names(snap) && !identical(as.character(est$pendencias_remanescentes$status_justificativa), as.character(snap$status_justificativa))) stop("O estado histórico não confere com o snapshot comprovante.", call. = FALSE)
  nomes <- c("justificativas_pendencias_consolidada.csv", "justificativas_ocorrencias_aliases.csv", "justificativas_snapshot_recuperacao.csv", "manifesto_recuperacao_justificativas.json")
  if (any(file.exists(file.path(destino_linhagem, nomes)))) stop("O destino já contém artefatos de justificativas; recuperação não sobrescreve decisões.", call. = FALSE)
  stage <- tempfile("recuperacao_justificativas_"); dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  hh <- monitora_v307_justificativas_hash(arquivo_historico, arquivo = TRUE); hs <- monitora_v307_justificativas_hash(arquivo_snapshot, arquivo = TRUE)
  monitora_v307_justificativas_copiar(arquivo_historico, file.path(stage, nomes[1L]), hh)
  monitora_v307_justificativas_copiar(arquivo_snapshot, file.path(stage, nomes[3L]), hs)
  monitora_v307_justificativas_csv_escrever(aa, file.path(stage, nomes[2L]))
  rec <- list(
    recovery_schema = "monitora_recuperacao_justificativas_v1", base_data_sha256 = data_hash, base_revision_id = revision,
    snapshot_missing_policy = "monitora_snapshot_legado_na_v1",
    source_history_path = normalizePath(arquivo_historico, winslash = "/", mustWork = TRUE), source_history_sha256 = hh,
    source_snapshot_path = normalizePath(arquivo_snapshot, winslash = "/", mustWork = TRUE), source_snapshot_sha256 = hs,
    source_manifest_path = normalizePath(arquivo_manifesto_fonte, winslash = "/", mustWork = TRUE), source_manifest_sha256 = monitora_v307_justificativas_hash(arquivo_manifesto_fonte, arquivo = TRUE),
    aliases_sha256 = monitora_v307_justificativas_hash(file.path(stage, nomes[2L]), arquivo = TRUE),
    event_count = nrow(hist), aliases_count = nrow(aa), snapshot_count = nrow(snap),
    metodo = "IDs literal/UTF8/unknown reproduzidos exclusivamente das chaves diagnósticas completas; eventos originais preservados byte a byte",
    timestamp_recuperacao = format(Sys.time(), "%Y-%m-%d %H:%M:%OS6 %z")
  )
  jsonlite::write_json(rec, file.path(stage, nomes[4L]), pretty = TRUE, auto_unbox = TRUE, na = "null")
  monitora_v307_justificativas_recuperacao_ler(stage, destino_man, list(data_sha256 = data_hash))
  monitora_v307_justificativas_publicar_diretorio(stage, destino_linhagem)
  invisible(rec)
}
