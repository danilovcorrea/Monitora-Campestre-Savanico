# Contrato do cache pré-painel. Somente definições; nenhuma abertura de UI.
monitora_v307_cache_erro <- function(...) stop("Cache de painel bloqueado: ", ..., call. = FALSE)

monitora_v307_cache_configuracao <- function(configuracao = NULL) {
  if (is.null(configuracao)) configuracao <- list(
    versao_modulo = get0("MONITORA_VALIDACAO_ESPACIAL_VERSAO_MODULO", ifnotfound = "sem_versao", inherits = TRUE),
    validar_espacial = get0("MONITORA_VALIDAR_ESPACIAL_COLETAS", ifnotfound = FALSE, inherits = TRUE),
    raio_m = get0("MONITORA_RAIO_VALIDACAO_ESPACIAL_M", ifnotfound = NA_real_, inherits = TRUE)
  )
  required <- c("versao_modulo", "validar_espacial", "raio_m")
  if (!is.list(configuracao) || anyDuplicated(names(configuracao)) || !all(required %in% names(configuracao))) {
    monitora_v307_cache_erro("configuração espacial explícita incompleta; regenere o cache com a mesma entrada na candidata atual.")
  }
  modulo <- configuracao$versao_modulo
  espacial <- configuracao$validar_espacial
  raio <- configuracao$raio_m
  if (!is.character(modulo) || length(modulo) != 1L || is.na(modulo) || !nzchar(modulo) ||
      !is.logical(espacial) || length(espacial) != 1L || is.na(espacial) ||
      !is.numeric(raio) || length(raio) != 1L ||
      (!is.na(raio) && (!is.finite(raio) || raio <= 0)) || (espacial && is.na(raio))) {
    monitora_v307_cache_erro("configuração espacial inválida; regenere o cache com a mesma entrada na candidata atual.")
  }
  list(versao_modulo = enc2utf8(modulo), validar_espacial = espacial, raio_m = as.double(raio))
}

monitora_v307_cache_relativo_validar <- function(x) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) monitora_v307_cache_erro("chave relativa ausente.")
  x <- gsub("\\\\", "/", enc2utf8(x))
  if (any(grepl("(^/|^[A-Za-z]:|(^|/)[.][.]?(/|$)|//)", x))) monitora_v307_cache_erro("chave relativa absoluta, ambígua ou com traversal.")
  if (anyDuplicated(x) || (.Platform$OS.type == "windows" && anyDuplicated(tolower(x)))) monitora_v307_cache_erro("chaves relativas duplicadas/ambíguas.")
  x
}

monitora_v307_cache_arquivos <- function(input_dir, legado = FALSE) {
  if (!dir.exists(input_dir)) monitora_v307_cache_erro("input atual ausente.")
  root <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  paths <- list.files(root, full.names = TRUE, recursive = TRUE, all.files = !isTRUE(legado), no.. = TRUE)
  paths <- paths[!dir.exists(paths)]
  if (isTRUE(legado)) paths <- paths[grepl("[.](csv|xlsx|xls|zip)$", paths, ignore.case = TRUE)]
  if (!length(paths)) monitora_v307_cache_erro("input atual não contém arquivos físicos para comprovar a base.")
  actual <- vapply(paths, normalizePath, character(1L), winslash = "/", mustWork = TRUE)
  prefix <- paste0(sub("/+$", "", root), "/")
  inside <- startsWith(if (.Platform$OS.type == "windows") tolower(actual) else actual,
                       if (.Platform$OS.type == "windows") tolower(prefix) else prefix)
  if (!all(inside)) monitora_v307_cache_erro("arquivo de input aponta para fora de sua raiz física.")
  rel <- monitora_v307_cache_relativo_validar(substring(actual, nchar(prefix) + 1L))
  if (anyDuplicated(actual) || (.Platform$OS.type == "windows" && anyDuplicated(tolower(actual)))) monitora_v307_cache_erro("input possui aliases físicos ambíguos.")
  idx <- order(enc2utf8(rel), method = "radix")
  data.frame(path = rel[idx], arquivo = actual[idx], bytes = as.double(file.info(actual[idx])$size), stringsAsFactors = FALSE)
}

monitora_v307_cache_hash <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) monitora_v307_cache_erro("digest indisponível para conferir SHA256.")
  digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 2L)
}

monitora_v307_cache_configuracao_sha256 <- function(configuracao = NULL) {
  monitora_v307_cache_hash(monitora_v307_cache_configuracao(configuracao))
}

monitora_v307_cache_input_sha256 <- function(
    input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE)) {
  brutos <- get0("monitora_v307_justificativas_fingerprint_brutos", mode = "function", ifnotfound = NULL, inherits = TRUE)
  if (is.null(brutos)) monitora_v307_cache_erro("helper comum de fingerprint dos brutos indisponível.")
  raw <- brutos(input_dir = input_dir)$sha256
  if (is.character(raw) && length(raw) == 1L && nzchar(raw)) return(raw)
  # Incremental sem brutos também precisa de uma prova válida. Este fallback
  # não transforma um checkpoint em autorização de continuidade pelo ramo bruto.
  monitora_v307_cache_hash(monitora_v307_cache_inputs_validar(monitora_v307_cache_inputs(input_dir)))
}

monitora_v307_cache_prova_validada <- function(cache_obj, input_dir, configuracao) {
  memoria <- get0("monitora_v307_justificativas_memoria_sha256", mode = "function", ifnotfound = NULL, inherits = TRUE)
  brutos <- get0("monitora_v307_justificativas_fingerprint_brutos", mode = "function", ifnotfound = NULL, inherits = TRUE)
  if (is.null(memoria) || is.null(brutos)) monitora_v307_cache_erro("helpers comuns de proveniência indisponíveis.")
  list(ok = TRUE, memoria_sha256 = memoria(cache_obj$registros_corrig),
    input_sha256 = monitora_v307_cache_input_sha256(input_dir),
    brutos_sha256 = brutos(input_dir = input_dir)$sha256,
    config_sha256 = monitora_v307_cache_configuracao_sha256(configuracao))
}

monitora_v307_cache_dados_hash <- function(dados) {
  if (!is.data.frame(dados) || !ncol(dados) || !nrow(dados) || anyDuplicated(names(dados))) monitora_v307_cache_erro("tabela de registros ausente, vazia ou com schema ambíguo.")
  # Retira somente a infraestrutura mutável de data.table; valores, tipos,
  # ordem de linhas/colunas e atributos das colunas continuam protegidos.
  cols <- as.list(as.data.frame(dados, stringsAsFactors = FALSE))
  for (i in seq_along(cols)) {
    if (is.list(cols[[i]]) && !inherits(cols[[i]], "POSIXlt")) monitora_v307_cache_erro("coluna de registros com estrutura não suportada.")
    if (is.character(cols[[i]])) cols[[i]] <- enc2utf8(cols[[i]])
  }
  names(cols) <- enc2utf8(names(cols))
  # as.list.data.frame pode conservar o selfref do data.table lido de RDS.
  # É infraestrutura transitória, reparada por as.data.table(), sem alterar dados.
  attr(cols, ".internal.selfref") <- NULL
  monitora_v307_cache_hash(list(schema = "monitora_cache_dados_v1", n_linhas = nrow(dados), colunas = cols))
}

monitora_v307_cache_inputs <- function(input_dir) {
  a <- monitora_v307_cache_arquivos(input_dir)
  lapply(seq_len(nrow(a)), function(i) list(path = enc2utf8(a$path[i]), bytes = a$bytes[i],
    sha256 = digest::digest(file = a$arquivo[i], algo = "sha256")))
}

monitora_v307_cache_inputs_validar <- function(inputs) {
  if (!is.list(inputs) || !length(inputs)) monitora_v307_cache_erro("prova dos inputs ausente.")
  valid <- vapply(inputs, function(x) is.list(x) && all(c("path", "bytes", "sha256") %in% names(x)) && !anyDuplicated(names(x)), logical(1L))
  if (!all(valid)) monitora_v307_cache_erro("prova de inputs incompleta.")
  paths <- vapply(inputs, function(x) if (is.character(x$path) && length(x$path) == 1L) x$path else NA_character_, character(1L))
  paths <- monitora_v307_cache_relativo_validar(paths)
  bytes <- vapply(inputs, function(x) if (is.numeric(x$bytes) && length(x$bytes) == 1L) as.double(x$bytes) else NA_real_, numeric(1L))
  sha <- vapply(inputs, function(x) if (is.character(x$sha256) && length(x$sha256) == 1L) x$sha256 else NA_character_, character(1L))
  if (anyNA(bytes) || any(!is.finite(bytes) | bytes < 0 | bytes != floor(bytes)) || anyNA(sha) || any(!grepl("^[a-f0-9]{64}$", sha))) monitora_v307_cache_erro("tamanho/SHA256 inválido na prova dos inputs.")
  idx <- order(enc2utf8(paths), method = "radix")
  lapply(idx, function(i) list(path = paths[i], bytes = bytes[i], sha256 = sha[i]))
}

monitora_v307_cache_manifesto_produtor_validar <- function(cache_obj, input_dir) {
  man <- cache_obj$manifesto_inputs
  required <- c("arquivo", "tamanho", "mtime", "md5")
  if (!is.data.frame(man) || !nrow(man) || anyDuplicated(names(man)) || !all(required %in% names(man))) monitora_v307_cache_erro("inventário do produtor ausente/incompleto.")
  paths <- gsub("\\\\", "/", enc2utf8(as.character(man$arquivo)))
  if (anyNA(paths) || any(!nzchar(paths)) || any(grepl("(^|/)[.][.]?(/|$)", paths)) || anyDuplicated(paths)) monitora_v307_cache_erro("inventário do produtor duplicado ou com traversal.")
  sizes <- suppressWarnings(as.double(as.character(man$tamanho)))
  md5 <- as.character(man$md5)
  if (anyNA(sizes) || any(!is.finite(sizes) | sizes < 0 | sizes != floor(sizes)) || anyNA(md5) || any(!grepl("^[a-f0-9]{32}$", md5))) monitora_v307_cache_erro("inventário do produtor inválido.")
  actual <- vapply(paths, function(p) tryCatch(normalizePath(p, winslash="/", mustWork=TRUE), error=function(e) ""), character(1L))
  current <- monitora_v307_cache_arquivos(input_dir, legado=TRUE)
  if (any(!nzchar(actual)) || anyDuplicated(actual) || !setequal(actual,current$arquivo)) monitora_v307_cache_erro("inventário do produtor não corresponde ao input físico atual.")
  pos <- match(current$arquivo,actual)
  observed <- unname(as.character(tools::md5sum(current$arquivo)))
  if (anyNA(observed) || !identical(current$bytes,sizes[pos]) || !identical(observed,md5[pos])) monitora_v307_cache_erro("conteúdo/tamanho do input mudou antes da assinatura do cache.")
  invisible(TRUE)
}

monitora_v307_cache_payload_hash <- function(cache_obj) {
  sem_selfref <- function(x) {
    if (!is.list(x)) return(x)
    attrs <- attributes(x)
    attrs[[".internal.selfref"]] <- NULL
    out <- lapply(seq_along(x), function(i) sem_selfref(x[[i]]))
    attributes(out) <- lapply(attrs, sem_selfref)
    out
  }
  fields <- c("registros_corrig", "meta_xls", "relatorios_pre", "validacao_espacial_pre")
  payload <- setNames(lapply(fields,function(name)sem_selfref(cache_obj[[name]])),fields)
  monitora_v307_cache_hash(list(schema="monitora_cache_payload_v1",payload=payload))
}

monitora_v307_cache_assinar <- function(cache_obj,
    input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE), configuracao = NULL) {
  if (!is.list(cache_obj) || is.null(cache_obj$meta_xls)) monitora_v307_cache_erro("estrutura de cache incompleta.")
  cfg <- monitora_v307_cache_configuracao(configuracao)
  data_hash <- monitora_v307_cache_dados_hash(cache_obj$registros_corrig)
  # A leitura de inputs usada pelo produtor não pode ter ficado obsoleta antes
  # de assinar a nova prova; não exige a configuração legada nem a sua soma.
  monitora_v307_cache_manifesto_produtor_validar(cache_obj, input_dir)
  inputs <- monitora_v307_cache_inputs_validar(monitora_v307_cache_inputs(input_dir))
  cache_obj$versao_cache <- "painel_cache_v307"
  cache_obj$configuracao_espacial <- cfg
  cache_obj$monitora_cache_v307 <- list(schema = "monitora_cache_v307_v1", dados_sha256 = data_hash,
    payload_sha256 = monitora_v307_cache_payload_hash(cache_obj),
    n_linhas = as.integer(nrow(cache_obj$registros_corrig)), n_colunas = as.integer(ncol(cache_obj$registros_corrig)),
    inputs = inputs, inputs_sha256 = monitora_v307_cache_hash(inputs),
    configuracao_espacial = cfg)
  cache_obj
}

monitora_v307_cache_validar <- function(cache_obj,
    input_dir = get0("MONITORA_INPUT_DIR", ifnotfound = "input", inherits = TRUE), configuracao = NULL) {
  if (!is.list(cache_obj) || is.null(cache_obj$meta_xls)) monitora_v307_cache_erro("estrutura incompleta.")
  cfg <- monitora_v307_cache_configuracao(configuracao)
  if (!identical(cache_obj$versao_cache,"painel_cache_v307")) monitora_v307_cache_erro("cache legado sem prova forte independente do payload; regenere o cache com a mesma entrada na candidata atual.")
  data_hash <- monitora_v307_cache_dados_hash(cache_obj$registros_corrig)
  proof <- cache_obj$monitora_cache_v307
  modern <- identical(cache_obj$versao_cache, "painel_cache_v307") || !is.null(proof)
  if (modern) {
    required <- c("schema", "dados_sha256", "payload_sha256", "n_linhas", "n_colunas", "inputs", "inputs_sha256", "configuracao_espacial")
    if (!is.list(proof) || anyDuplicated(names(proof)) || !all(required %in% names(proof)) || !identical(proof$schema, "monitora_cache_v307_v1")) monitora_v307_cache_erro("prova SHA256 ausente/incompleta; regenere o cache com a mesma entrada.")
    if (!identical(as.character(proof$dados_sha256), data_hash) ||
        !identical(as.double(proof$n_linhas), as.double(nrow(cache_obj$registros_corrig))) ||
        !identical(as.double(proof$n_colunas), as.double(ncol(cache_obj$registros_corrig)))) monitora_v307_cache_erro("dados/schema do RDS divergem da prova SHA256.")
    if (!identical(as.character(proof$payload_sha256),monitora_v307_cache_payload_hash(cache_obj))) monitora_v307_cache_erro("payload semântico do RDS diverge da prova SHA256.")
    declared <- monitora_v307_cache_inputs_validar(proof$inputs)
    if (!identical(as.character(proof$inputs_sha256), monitora_v307_cache_hash(declared))) monitora_v307_cache_erro("prova SHA256 dos inputs foi alterada.")
    current <- monitora_v307_cache_inputs_validar(monitora_v307_cache_inputs(input_dir))
    if (!identical(declared, current)) monitora_v307_cache_erro("inputs físicos atuais diferem da prova SHA256 do cache.")
    if (!identical(monitora_v307_cache_configuracao(proof$configuracao_espacial), cfg) ||
        !identical(monitora_v307_cache_configuracao(cache_obj$configuracao_espacial), cfg)) monitora_v307_cache_erro("configuração espacial atual difere do cache.")
    return(invisible(monitora_v307_cache_prova_validada(cache_obj, input_dir, cfg)))
  }
  monitora_v307_cache_erro("prova forte de cache ausente; regenere o cache.")
}
