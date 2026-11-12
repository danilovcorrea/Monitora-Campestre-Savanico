# Compactação restrita a cópias não canônicas cujo destino projetado excede 210.
monitora_v307_caminhos_longo <- function(output_dir, relativo) {
  relativo <- chartr("\\", "/", relativo)
  startsWith(relativo, "99_legacy_compat/") &&
    nchar(monitora_caminho_windows_projetado(file.path(output_dir, relativo), output_dir), type = "chars") > 210L
}

monitora_v307_caminhos_local <- function(path, raiz) {
  if (!monitora_v307_caminho_contido(monitora_v307_caminho_canonico(path), raiz))
    stop("Artefato legado aponta para fora do output.", call. = FALSE)
  atual <- path
  repeat {
    link <- Sys.readlink(atual)
    if (!is.na(link) && nzchar(link)) stop("Aliases simbólicos não são aceitos na compactação legada.", call. = FALSE)
    pai <- dirname(atual)
    if (identical(pai, atual)) break
    atual <- pai
  }
  invisible(TRUE)
}

monitora_v307_caminhos_validar_mapa <- function(mapa) {
  campos <- c("exec_id", "contexto", "caminho_origem", "caminho_origem_relativo", "destino_legado_relativo",
    "destino_relativo", "fonte_preservada_relativa", "sha256", "sha256_atual", "estado_editorial", "conteudo_imutavel", "bytes", "hash_verificado")
  if (!all(campos %in% names(mapa)) || anyDuplicated(names(mapa))) stop("Mapa cumulativo de compatibilidade incompleto.", call. = FALSE)
  for (nm in c("sha256", "sha256_atual")) if (anyNA(mapa[[nm]]) || any(!grepl("^[a-f0-9]{64}$", mapa[[nm]])))
    stop("SHA256 inválido no mapa cumulativo legado.", call. = FALSE)
  for (nm in c("caminho_origem_relativo", "destino_legado_relativo", "destino_relativo", "fonte_preservada_relativa")) {
    r <- chartr("\\", "/", mapa[[nm]])
    if (anyNA(r) || any(!nzchar(r)) || any(grepl("(^/|^[A-Za-z]:|(^|/)[.][.]?(/|$))", r)))
      stop("Rota ambígua no mapa cumulativo legado.", call. = FALSE)
  }
  if (any(!grepl("^99_legacy_compat/c/[a-f0-9]{24}([.].*)?$", mapa$destino_relativo)) ||
      any(!grepl("^99_legacy_compat/c/orig_[a-f0-9]{24}([.].*)?$", mapa$fonte_preservada_relativa)))
    stop("Mapa legado aponta para fora das cópias de compatibilidade.", call. = FALSE)
  if (anyNA(mapa$conteudo_imutavel) || any(!mapa$conteudo_imutavel %in% c("TRUE", "FALSE")))
    stop("Proteção editorial ausente no mapa legado.", call. = FALSE)
  if (anyNA(mapa$estado_editorial) || any(!mapa$estado_editorial %in% c("aguardando_reescrita", "bytes_inalterados", "referencias_reescritas")))
    stop("Estado editorial inválido no mapa legado.", call. = FALSE)
  invisible(TRUE)
}

monitora_v307_caminhos_mover <- function(origem, relativo, output_dir, exec_id, contexto,
    envir = environment(monitora_v307_caminhos_mover),
    injetar_falha = function(etapa, tx) invisible(NULL)) {
  relativo <- chartr("\\", "/", relativo)
  if (!monitora_v307_caminhos_longo(output_dir, relativo)) {
    mov <- monitora_output_mover_arquivo(origem, file.path(output_dir, relativo))
    mov$destino_relativo <- relativo
    return(mov)
  }
  if (grepl("(^|/)[.][.]?(/|$)", relativo)) stop("Destino legado ambíguo.", call. = FALSE)
  raiz <- monitora_v307_caminho_canonico(output_dir)
  origem <- monitora_v307_caminho_canonico(origem)
  if (!monitora_v307_caminho_contido(origem, raiz) || !file.exists(origem) || dir.exists(origem))
    stop("Origem de compatibilidade deve ser arquivo do output.", call. = FALSE)
  origem_rel <- substring(origem, nchar(raiz) + 2L)
  legadas <- c("99_legacy_compat/", "correcoes_campos/", "validacao_espacial/", "cache_painel/",
    "relatorios_validacao/", "03_auditorias/relatorios_validacao/", "contrato/", "pipes/")
  if (grepl("/", origem_rel, fixed = TRUE) && !any(startsWith(origem_rel, legadas)))
    stop("Compactação legada não pode retirar produtos canônicos.", call. = FALSE)
  mapa_path <- file.path(output_dir, "99_legacy_compat", "mapa.csv")
  monitora_v307_caminhos_local(file.path(output_dir, "99_legacy_compat", "c"), raiz)
  monitora_v307_caminhos_local(mapa_path, raiz)
  hash <- function(p) digest::digest(file = p, algo = "sha256")
  sha <- hash(origem)
  chave <- paste0(enc2utf8(relativo), "\n", sha)
  ext <- tools::file_ext(relativo)
  if (nzchar(ext)) ext <- paste0(".", ext)
  novo <- NULL
  for (i in 0:1024) {
    id <- digest::digest(paste0(chave, "\n", i), algo = "sha256", serialize = FALSE)
    candidato <- file.path("99_legacy_compat", "c", paste0(substr(id, 1L, 24L), ext))
    original_rel <- file.path("99_legacy_compat", "c", paste0("orig_", substr(id, 1L, 24L), ext))
    path <- file.path(output_dir, candidato)
    preservado <- file.path(output_dir, original_rel)
    monitora_v307_caminhos_local(path, raiz)
    monitora_v307_caminhos_local(preservado, raiz)
    if (monitora_v307_caminhos_longo(output_dir, original_rel))
      stop("Raiz declarada não permite cópia legada curta dentro de 210 caracteres.", call. = FALSE)
    livre <- function(p) !file.exists(p) && !dir.exists(p) || (!dir.exists(p) && identical(hash(p), sha))
    if (livre(path) && livre(preservado)) {
      novo <- candidato; break
    }
  }
  if (is.null(novo)) stop("Colisões impediram preservar o histórico legado.", call. = FALSE)
  destino <- file.path(output_dir, novo)
  if (anyDuplicated(vapply(c(origem, destino, preservado, mapa_path), monitora_v307_caminho_canonico, character(1L))))
    stop("Origem, derivado, fonte preservada e mapa devem ser distintos.", call. = FALSE)
  mapa <- if (file.exists(mapa_path)) data.table::fread(mapa_path, colClasses = "character", na.strings = NULL, encoding = "UTF-8") else data.table::data.table()
  if (file.exists(mapa_path)) monitora_v307_caminhos_validar_mapa(mapa)
  log_dir <- get0("MONITORA_LOG_DIR", envir = envir, inherits = TRUE)
  if (!is.character(log_dir) || length(log_dir) != 1L || is.na(log_dir) || !nzchar(log_dir))
    stop("LOG_DIR necessário para preservar a movimentação legada.", call. = FALSE)
  protegidos <- monitora_v307_transacao_destinos(output_dir, envir = envir)$protegidos
  md5 <- unname(as.character(tools::md5sum(origem)))
  esperado <- NULL
  monitora_v307_transacao_executar(
    destinos = list(arquivos = c(origem, destino, preservado, mapa_path), raizes = character(), superficiais = character(), protegidos = protegidos),
    preflight = function() TRUE, envir = envir, diagnostico_dir = file.path(log_dir, "transacoes_caminhos"),
    acao = function(tx) {
      dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
      injetar_falha("antes_copia", tx)
      if (!file.exists(preservado) && !isTRUE(file.copy(origem, preservado, overwrite = FALSE, copy.date = TRUE, copy.mode = TRUE)))
        stop("Falha ao preservar os bytes originais do legado.", call. = FALSE)
      if (!file.exists(destino) && !isTRUE(file.copy(origem, destino, overwrite = FALSE, copy.date = TRUE, copy.mode = TRUE)))
        stop("Falha ao copiar legado para destino curto.", call. = FALSE)
      injetar_falha("apos_copia", tx)
      if (!identical(hash(destino), sha) || !identical(hash(origem), sha) || !identical(hash(preservado), sha))
        stop("Bytes legados divergiram do SHA256.", call. = FALSE)
      linha <- data.table::data.table(exec_id = as.character(exec_id), contexto = as.character(contexto)[1L],
        caminho_origem = origem, caminho_origem_relativo = substring(origem, nchar(raiz) + 2L),
        destino_legado_relativo = relativo, destino_relativo = chartr("\\", "/", novo),
        fonte_preservada_relativa = chartr("\\", "/", original_rel),
        sha256 = sha, sha256_atual = sha, estado_editorial = "aguardando_reescrita",
        conteudo_imutavel = as.character(monitora_v307_editorial_imutavel(origem)),
        bytes = as.character(file.info(origem)$size), hash_verificado = "TRUE")
      # Se uma raiz declarada maior exigir outra compactação, as rotas antigas
      # continuam apontando para o arquivo atual sem perder origem/SHA originais.
      if (nrow(mapa)) mapa[destino_relativo == linha$caminho_origem_relativo & sha256 == sha, destino_relativo := linha$destino_relativo]
      acumulado <- unique(data.table::rbindlist(list(mapa, linha), use.names = TRUE, fill = TRUE),
        by = c("caminho_origem_relativo", "destino_relativo", "sha256"), fromLast = TRUE)
      esperado <<- acumulado
      monitora_fwrite(acumulado, mapa_path, na = "")
      injetar_falha("apos_mapa", tx)
      if (unlink(origem, force = TRUE) != 0L || file.exists(origem)) stop("Falha ao retirar legado já preservado.", call. = FALSE)
      injetar_falha("apos_retirada", tx)
      list(ok = TRUE, md5_origem = md5, md5_destino = md5,
        acao = "compactado_sha256_verificado", destino_relativo = novo)
    },
    validar = function() identical(hash(destino), sha) && identical(hash(preservado), sha) && file.exists(mapa_path) &&
      identical(as.data.frame(data.table::fread(mapa_path, colClasses = "character", na.strings = NULL)), as.data.frame(esperado))
  )
}

monitora_v307_caminhos_compactar_existentes <- function(output_dir, exec_id, contexto) {
  arquivos <- list.files(file.path(output_dir, "99_legacy_compat"), recursive = TRUE, full.names = TRUE)
  arquivos <- arquivos[!dir.exists(arquivos)]
  linhas <- list()
  for (p in arquivos) {
    rel <- substring(p, nchar(sub("/+$", "", output_dir)) + 2L)
    if (!monitora_v307_caminhos_longo(output_dir, rel)) next
    mov <- monitora_v307_caminhos_mover(p, rel, output_dir, exec_id, contexto)
    linhas[[length(linhas) + 1L]] <- data.table::data.table(caminho_origem = p,
      caminho_origem_relativo = rel, destino_relativo = mov$destino_relativo,
      hash_verificado = mov$ok, md5_origem = mov$md5_origem, md5_destino = mov$md5_destino)
  }
  if (length(linhas)) data.table::rbindlist(linhas) else data.table::data.table()
}

monitora_v307_caminhos_candidatos <- function(candidatos, output_dir) {
  rel <- chartr("\\", "/", substring(candidatos, nchar(output_dir) + 2L))
  arquivo <- file.path(output_dir, "99_legacy_compat", "mapa.csv")
  imutaveis <- character()
  if (file.exists(arquivo)) {
    mapa <- data.table::fread(arquivo, colClasses = "character", na.strings = NULL)
    monitora_v307_caminhos_validar_mapa(mapa)
    imutaveis <- mapa$destino_relativo[mapa$conteudo_imutavel == "TRUE"]
  }
  candidatos[rel != "99_legacy_compat/mapa.csv" & !grepl("^99_legacy_compat/c/orig_[a-f0-9]{24}[.]?", rel) & !rel %in% imutaveis]
}

monitora_v307_caminhos_preparar_edicao <- function(output_dir,
    envir = environment(monitora_v307_caminhos_preparar_edicao)) {
  arquivo <- file.path(output_dir, "99_legacy_compat", "mapa.csv")
  if (!file.exists(arquivo)) return(NULL)
  raiz <- monitora_v307_caminho_canonico(output_dir)
  monitora_v307_caminhos_local(arquivo, raiz)
  mapa <- data.table::fread(arquivo, colClasses = "character", na.strings = NULL)
  monitora_v307_caminhos_validar_mapa(mapa)
  for (nm in c("fonte_preservada_relativa", "destino_relativo")) {
    paths <- file.path(output_dir, mapa[[nm]])
    invisible(lapply(paths, monitora_v307_caminhos_local, raiz = raiz))
    sha <- vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), character(1L), USE.NAMES = FALSE)
    if (!identical(sha, mapa[[if (nm == "fonte_preservada_relativa") "sha256" else "sha256_atual"]]))
      stop("Legado divergiu do hash registrado antes da reescrita editorial.", call. = FALSE)
  }
  list(output = raiz, exec_id = get0("MONITORA_EXEC_ID", envir = envir, inherits = TRUE),
    mapa_sha256 = digest::digest(file = arquivo, algo = "sha256"))
}

monitora_v307_caminhos_finalizar <- function(output_dir, prova = NULL,
    envir = environment(monitora_v307_caminhos_finalizar)) {
  arquivo <- file.path(output_dir, "99_legacy_compat", "mapa.csv")
  if (!file.exists(arquivo)) return(invisible(NULL))
  raiz <- monitora_v307_caminho_canonico(output_dir)
  monitora_v307_caminhos_local(arquivo, raiz)
  mapa <- data.table::fread(arquivo, colClasses = "character", na.strings = NULL)
  monitora_v307_caminhos_validar_mapa(mapa)
  invisible(lapply(file.path(output_dir, c(mapa$fonte_preservada_relativa, mapa$destino_relativo)),
    monitora_v307_caminhos_local, raiz = raiz))
  hash <- function(rel) digest::digest(file = file.path(output_dir, rel), algo = "sha256")
  originais <- vapply(mapa$fonte_preservada_relativa, hash, character(1L), USE.NAMES = FALSE)
  if (!identical(originais, mapa$sha256)) stop("Bytes originais legados foram alterados.", call. = FALSE)
  mapa[, sha256_atual := vapply(destino_relativo, hash, character(1L), USE.NAMES = FALSE)]
  if (any(mapa$conteudo_imutavel == "TRUE" & mapa$sha256_atual != mapa$sha256))
    stop("Conteúdo editorialmente imutável foi alterado.", call. = FALSE)
  antes <- data.table::fread(arquivo, colClasses = "character", na.strings = NULL)$sha256_atual
  if (!identical(mapa$sha256_atual, antes) && (!is.list(prova) ||
      !identical(prova$output, monitora_v307_caminho_canonico(output_dir)) ||
      !identical(prova$exec_id, get0("MONITORA_EXEC_ID", envir = envir, inherits = TRUE)) ||
      !identical(prova$mapa_sha256, digest::digest(file = arquivo, algo = "sha256"))))
    stop("Reescrita legada exige prova explícita dos hashes anteriores.", call. = FALSE)
  mapa[, estado_editorial := ifelse(sha256_atual == sha256, "bytes_inalterados", "referencias_reescritas")]
  monitora_v307_transacao_executar(
    destinos = list(arquivos = arquivo, raizes = character(), superficiais = character(),
      protegidos = monitora_v307_transacao_destinos(output_dir, envir = envir)$protegidos),
    preflight = function() TRUE, envir = envir,
    diagnostico_dir = file.path(get0("MONITORA_LOG_DIR", envir = envir, inherits = TRUE), "transacoes_caminhos"),
    acao = function(tx) monitora_fwrite(mapa, arquivo, na = ""),
    validar = function() identical(as.data.frame(data.table::fread(arquivo, colClasses = "character", na.strings = NULL)), as.data.frame(mapa))
  )
  invisible(mapa)
}

monitora_v307_caminhos_mapa_atual <- function(mapa, atual = NULL,
    output_dir = get0("MONITORA_OUTPUT_DIR", ifnotfound = "output", inherits = TRUE),
    exec_id = get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE)) {
  arquivo <- file.path(output_dir, "99_legacy_compat", "mapa.csv")
  duravel <- data.table::data.table()
  if (file.exists(arquivo)) {
    duravel <- data.table::fread(arquivo, colClasses = "character", na.strings = NULL)
    monitora_v307_caminhos_validar_mapa(duravel)
  }
  todos <- data.table::rbindlist(list(mapa, duravel, atual), fill = TRUE, use.names = TRUE)
  if (!nrow(todos) || !all(c("caminho_origem_relativo", "destino_relativo") %in% names(todos))) return(todos)
  # Só as rotas que passaram pelo encurtamento recebem vínculo atual explícito.
  compactos <- !is.na(todos$destino_relativo) & startsWith(todos$destino_relativo, "99_legacy_compat/c/")
  afetados <- unique(stats::na.omit(todos$caminho_origem_relativo[compactos]))
  id_caminhos_atual <- as.character(exec_id)
  corrente <- if ("exec_id" %in% names(mapa)) mapa[!is.na(mapa$exec_id) & mapa$exec_id == id_caminhos_atual] else NULL
  if (!is.null(atual) && nrow(atual)) {
    atual <- data.table::copy(atual)
    ii <- which(atual$caminho_origem_relativo %in% afetados)
    if (length(ii)) data.table::set(atual, i = ii, j = "exec_id", value = rep(id_caminhos_atual, length(ii)))
  }
  resultado <- data.table::rbindlist(list(mapa, duravel, corrente, atual), fill = TRUE, use.names = TRUE)
  ultimo <- !duplicated(resultado$caminho_origem_relativo, fromLast = TRUE)
  resultado[!resultado$caminho_origem_relativo %in% afetados | ultimo]
}
