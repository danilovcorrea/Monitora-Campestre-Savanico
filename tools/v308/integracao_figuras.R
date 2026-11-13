# Paginacao documental: uma identidade analitica, varios arquivos fisicos.
monitora_v308_falhar_paginas <- function(...) stop("Gate de paginas de evidencia: ", ..., call. = FALSE)

monitora_v308_caminho_evidencia <- function(arquivo, dir_relatorio = NULL) {
  if (length(arquivo) != 1L || is.na(arquivo) || !nzchar(arquivo)) monitora_v308_falhar_paginas("caminho ausente.")
  absoluto <- grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", arquivo)
  alvo <- if (!absoluto && !is.null(dir_relatorio)) file.path(dir_relatorio, arquivo) else arquivo
  if (!file.exists(alvo) || isTRUE(file.info(alvo)$isdir) || file.info(alvo)$size <= 0) monitora_v308_falhar_paginas("arquivo ausente ou vazio: ", arquivo)
  alvo <- normalizePath(alvo, winslash = "/", mustWork = TRUE)
  if (!is.null(dir_relatorio)) {
    raiz <- paste0(sub("/+$", "", normalizePath(dir_relatorio, winslash = "/", mustWork = TRUE)), "/")
    if (!startsWith(alvo, raiz)) monitora_v308_falhar_paginas("arquivo fora do relatorio: ", arquivo)
  }
  alvo
}

monitora_v308_relativo_evidencia <- function(arquivo, dir_relatorio) {
  raiz <- paste0(sub("/+$", "", normalizePath(dir_relatorio, winslash = "/", mustWork = TRUE)), "/")
  alvo <- monitora_v308_caminho_evidencia(arquivo, dir_relatorio)
  substring(alvo, nchar(raiz) + 1L)
}

monitora_v308_ler_paginas_evidencia <- function(arquivo, dir_relatorio = NULL) {
  canonico <- monitora_v308_caminho_evidencia(arquivo, dir_relatorio)
  manifests <- monitora_v308_evidencia_manifestos(canonico)
  presentes <- file.exists(c(manifests$paginas, manifests$celulas))
  if (any(presentes) && !all(presentes)) monitora_v308_falhar_paginas("manifesto incompleto: ", basename(canonico))
  if (!any(presentes)) {
    paginas <- data.table::data.table(arquivo_canonico = basename(canonico), arquivo = basename(canonico),
      pagina = 1L, n_paginas = 1L, n_resultados = NA_integer_, md5 = unname(tools::md5sum(canonico)))
    return(list(paginas = paginas, celulas = NULL, arquivos = canonico, canonico = canonico, manifesto = FALSE))
  }
  paginas <- data.table::fread(manifests$paginas, encoding = "UTF-8", showProgress = FALSE)
  campos <- c("arquivo_canonico", "arquivo", "pagina", "n_paginas", "n_resultados", "md5", "sha256",
    "celulas_arquivo", "celulas_md5", "celulas_sha256", "fonte_pt", "largura_pol", "altura_pol", "fit")
  if (!all(campos %in% names(paginas)) || !nrow(paginas) || anyNA(paginas[, ..campos])) monitora_v308_falhar_paginas("esquema/valores invalidos no manifesto.")
  if (any(paginas$arquivo_canonico != basename(canonico)) || anyDuplicated(paginas$arquivo) ||
      !identical(as.integer(paginas$pagina), seq_len(nrow(paginas))) ||
      any(paginas$n_paginas != nrow(paginas)) || paginas$arquivo[1L] != basename(canonico) ||
      any(basename(paginas$arquivo) != paginas$arquivo) || any(grepl("[/\\\\]", paginas$arquivo))) {
    monitora_v308_falhar_paginas("identidade, ordem ou caminhos de paginas invalidos.")
  }
  if (any(!is.finite(paginas$fonte_pt) | paginas$fonte_pt < 7) ||
      any(!is.finite(paginas$largura_pol) | paginas$largura_pol <= 0 | paginas$largura_pol > 6.65 + 1e-9) ||
      any(!is.finite(paginas$altura_pol) | paginas$altura_pol <= 0 | paginas$altura_pol > 8 + 1e-9) ||
      any(!as.logical(paginas$fit)) || any(paginas$n_resultados <= 0 | paginas$n_resultados != as.integer(paginas$n_resultados))) {
    monitora_v308_falhar_paginas("legibilidade, dimensoes ou contagens fisicas invalidas.")
  }
  arquivos <- file.path(dirname(canonico), paginas$arquivo)
  if (any(!file.exists(arquivos)) || any(file.info(arquivos)$size <= 0)) monitora_v308_falhar_paginas("continuacao ausente/vazia.")
  md5 <- unname(tools::md5sum(arquivos))
  sha <- vapply(arquivos, function(p) digest::digest(file = p, algo = "sha256"), character(1L))
  if (anyNA(md5) || !identical(as.character(paginas$md5), md5) || !identical(as.character(paginas$sha256), unname(sha))) {
    monitora_v308_falhar_paginas("PNG alterado ou manifesto desatualizado.")
  }
  mapa_md5 <- unname(tools::md5sum(manifests$celulas))
  mapa_sha <- digest::digest(file = manifests$celulas, algo = "sha256")
  if (any(paginas$celulas_arquivo != basename(manifests$celulas)) ||
      any(paginas$celulas_md5 != mapa_md5) || any(paginas$celulas_sha256 != mapa_sha)) {
    monitora_v308_falhar_paginas("mapa de resultados alterado/desvinculado.")
  }
  celulas <- data.table::fread(manifests$celulas, encoding = "UTF-8", showProgress = FALSE)
  ids <- c("arquivo_canonico", "input_row", "cell_id", "pagina", "arquivo")
  if (!all(ids %in% names(celulas)) || !nrow(celulas) || anyNA(celulas[, ..ids]) ||
      any(celulas$arquivo_canonico != basename(canonico)) || anyDuplicated(celulas$input_row) || anyDuplicated(celulas$cell_id) ||
      any(!nzchar(celulas$cell_id)) || !identical(sort(as.integer(celulas$input_row)), seq_len(nrow(celulas))) ||
      any(celulas$input_row != as.integer(celulas$input_row)) || sum(paginas$n_resultados) != nrow(celulas)) {
    monitora_v308_falhar_paginas("particao dos resultados nao e completa/exclusiva.")
  }
  pos <- match(celulas$pagina, paginas$pagina)
  if (anyNA(pos) || any(celulas$arquivo != paginas$arquivo[pos]) ||
      !identical(tabulate(pos, nbins = nrow(paginas)), as.integer(paginas$n_resultados))) {
    monitora_v308_falhar_paginas("celulas nao correspondem as paginas declaradas.")
  }
  list(paginas = paginas, celulas = celulas, arquivos = arquivos, canonico = canonico, manifesto = TRUE)
}

monitora_v308_chave_conjunto_evidencia <- function(info) {
  hashes <- as.character(info$paginas$md5)
  if (length(hashes) == 1L) return(hashes)
  digest::digest(paste0("paginas-v1:", length(hashes), ":", paste(hashes, collapse = "|")), algo = "md5", serialize = FALSE)
}

monitora_v308_completar_indice_graficos <- function(graficos, dir_relatorio) {
  z <- data.table::copy(graficos)
  if (!nrow(z) || !all(c("id", "disponivel", "arquivo_relatorio") %in% names(z))) return(z)
  z[, `:=`(n_paginas_evidencia = NA_integer_, arquivos_paginas_evidencia = NA_character_, hash_md5_conjunto_paginas = NA_character_)]
  alvos <- which(grepl("^inferencias_", z$id) & z$disponivel %in% TRUE)
  for (i in alvos) {
    info <- monitora_v308_ler_paginas_evidencia(z$arquivo_relatorio[i], dir_relatorio)
    if (!info$manifesto) monitora_v308_falhar_paginas("manifestos obrigatorios para uma identidade inferencial da candidata.")
    if ("hash_md5" %in% names(z) && !is.na(z$hash_md5[i]) && nzchar(z$hash_md5[i]) && z$hash_md5[i] != info$paginas$md5[1L]) {
      monitora_v308_falhar_paginas("hash canonico diverge do indice tematico.")
    }
    if (info$manifesto && "n_resultados_estatisticos" %in% names(z) &&
        (is.na(z$n_resultados_estatisticos[i]) || z$n_resultados_estatisticos[i] != nrow(info$celulas))) {
      monitora_v308_falhar_paginas("contagem de resultados do tema diverge do mapa de paginas.")
    }
    relativos <- vapply(info$arquivos, monitora_v308_relativo_evidencia, character(1L), dir_relatorio = dir_relatorio)
    data.table::set(z, i, "n_paginas_evidencia", nrow(info$paginas))
    data.table::set(z, i, "arquivos_paginas_evidencia", paste(relativos, collapse = ";"))
    data.table::set(z, i, "hash_md5_conjunto_paginas", monitora_v308_chave_conjunto_evidencia(info))
  }
  z
}

monitora_v308_expandir_figuras <- function(conteudo, dir_relatorio) {
  linhas <- unlist(lapply(as.character(conteudo), function(x) {
    if (is.na(x)) monitora_v308_falhar_paginas("linha documental ausente.")
    if (!nzchar(x)) "" else strsplit(paste0(x, "\n"), "\n", fixed = TRUE)[[1L]]
  }), use.names = FALSE)
  saida <- character(); cerca <- ""
  for (linha in linhas) {
    if (grepl("^\\s*(```+|~~~+)", linha)) {
      marca <- substr(trimws(linha), 1L, 3L)
      if (!nzchar(cerca)) cerca <- marca else if (identical(cerca, marca)) cerca <- ""
    }
    if (nzchar(cerca) || !startsWith(linha, "<figure") || grepl('data-monitora-evidencia-pagina="', linha, fixed = TRUE)) {
      saida <- c(saida, linha); next
    }
    m <- regmatches(linha, regexec('^(<figure[^>]*>)(<img src="([^"]+)"[^>]*>)<figcaption>(.*)</figcaption></figure>$', linha))[[1L]]
    if (length(m) != 5L) { saida <- c(saida, linha); next }
    relativo <- m[4L]; origem <- if (grepl("^(/|[A-Za-z]:[/\\\\])", relativo)) relativo else file.path(dir_relatorio, relativo)
    manifests <- monitora_v308_evidencia_manifestos(origem)
    if (!any(file.exists(c(manifests$paginas, manifests$celulas)))) { saida <- c(saida, linha); next }
    info <- monitora_v308_ler_paginas_evidencia(origem, dir_relatorio)
    if (nrow(info$paginas) == 1L) { saida <- c(saida, linha); next }
    relativos <- vapply(info$arquivos, monitora_v308_relativo_evidencia, character(1L), dir_relatorio = dir_relatorio)
    refs <- vapply(relativos, function(p) paste0("[[figura:fig-", substr(digest::digest(p, algo = "sha256", serialize = FALSE), 1L, 16L), "]]"), character(1L))
    citar <- if (length(refs) == 2L) paste(refs, collapse = " e ") else paste0(paste(head(refs, -1L), collapse = ", "), " e ", tail(refs, 1L))
    saida <- c(saida, "", paste0("O conjunto completo é apresentado nas ", citar, "."), "")
    for (i in seq_along(relativos)) {
      abertura <- sub(">$", paste0(' data-monitora-evidencia-pagina="', i, '">'), m[2L])
      imagem <- sub(paste0('src="', relativo, '"'), paste0('src="', relativos[i], '"'), m[3L], fixed = TRUE)
      legenda <- paste0(sub("[.]$", "", m[5L]), " — parte ", i, " de ", length(relativos), ".")
      saida <- c(saida, paste0(abertura, imagem, "<figcaption>", legenda, "</figcaption></figure>"), "")
    }
  }
  saida
}

monitora_v308_validar_paginas_documentais <- function(graficos, dir_relatorio, conteudo_detalhado, conteudo_sintetico,
    arquivo_auditoria = file.path(dir_relatorio, "auditoria_paginas_evidencias_documento.csv")) {
  z <- data.table::as.data.table(data.table::copy(graficos))
  if (!nrow(z) || !all(c("id", "arquivo_relatorio", "disponivel", "incorporada_relatorio_detalhado") %in% names(z))) return(invisible(data.table::data.table()))
  textos <- c(detalhado = paste(conteudo_detalhado, collapse = "\n"), sintetico = paste(conteudo_sintetico, collapse = "\n"))
  contar <- function(arquivo, texto) {
    achados <- gregexpr(paste0('src="', arquivo, '"'), texto, fixed = TRUE)[[1L]]
    if (achados[1L] < 0L) 0L else length(achados)
  }
  audit <- list(); erros <- character()
  for (i in which(grepl("^inferencias_", z$id) & z$disponivel %in% TRUE)) {
    info <- monitora_v308_ler_paginas_evidencia(z$arquivo_relatorio[i], dir_relatorio)
    if (!info$manifesto) monitora_v308_falhar_paginas("manifestos obrigatorios para uma identidade inferencial da candidata.")
    relativos <- vapply(info$arquivos, monitora_v308_relativo_evidencia, character(1L), dir_relatorio = dir_relatorio)
    nd <- vapply(relativos, contar, integer(1L), texto = textos[['detalhado']])
    ns <- vapply(relativos, contar, integer(1L), texto = textos[['sintetico']])
    esperado <- as.integer(isTRUE(z$incorporada_relatorio_detalhado[i]))
    if (any(nd != esperado) || any(ns != 0L)) erros <- c(erros, paste0(z$id[i], ": ocorrencia documental divergente"))
    if (info$manifesto && "n_resultados_estatisticos" %in% names(z) && z$n_resultados_estatisticos[i] != nrow(info$celulas)) erros <- c(erros, paste0(z$id[i], ": contagem estatistica divergente"))
    if ("status_documental" %in% names(z) && isTRUE(z$status_documental[i] == "duplicata_exata_representada_no_relatorio")) {
      r <- which(z$id == z$painel_representante[i] & z$incorporada_relatorio_detalhado %in% TRUE)
      if (length(r) != 1L || monitora_v308_chave_conjunto_evidencia(info) !=
          monitora_v308_chave_conjunto_evidencia(monitora_v308_ler_paginas_evidencia(z$arquivo_relatorio[r], dir_relatorio))) {
        erros <- c(erros, paste0(z$id[i], ": conjunto duplicado sem representante integral"))
      }
    }
    audit[[length(audit) + 1L]] <- data.table::data.table(plot_id = z$id[i], arquivo_canonico = z$arquivo_relatorio[i],
      arquivo = relativos, pagina = info$paginas$pagina, n_paginas = nrow(info$paginas),
      n_resultados_pagina = info$paginas$n_resultados, md5 = info$paginas$md5,
      ocorrencias_detalhado = nd, ocorrencias_sintetico = ns, ocorrencias_esperadas_detalhado = esperado,
      integro = nd == esperado & ns == 0L)
  }
  resultado <- data.table::rbindlist(audit, use.names = TRUE, fill = TRUE)
  if (!is.null(arquivo_auditoria) && nzchar(arquivo_auditoria)) data.table::fwrite(resultado, arquivo_auditoria, bom = TRUE, na = "")
  if (length(erros)) monitora_v308_falhar_paginas(paste(unique(erros), collapse = "; "))
  invisible(resultado)
}
