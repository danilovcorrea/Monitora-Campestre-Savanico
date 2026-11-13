# Renderizacao paginada; as tabelas cientificas recebidas permanecem intactas.
monitora_v308_evidencia_manifestos <- function(arquivo) {
  chave <- substr(digest::digest(enc2utf8(basename(arquivo)), algo = "sha256", serialize = FALSE), 1L, 16L)
  base <- file.path(dirname(arquivo), paste0("ev_", chave))
  list(paginas = paste0(base, "_paginas.csv"), celulas = paste0(base, "_celulas.csv"))
}

monitora_v308_evidencia_publicar <- function(fontes, destinos, remover = character(), falhar_apos = NULL) {
  stopifnot(length(fontes) == length(destinos), !anyDuplicated(destinos), all(file.exists(fontes)))
  todos <- unique(c(destinos, remover))
  if (any(dir.exists(todos))) stop("Destino da evidencia e um diretorio.", call. = FALSE)
  backup <- tempfile("ev_backup_", tmpdir = dirname(destinos[[1L]]))
  dir.create(backup)
  limpar_backup <- TRUE
  on.exit(if (limpar_backup) unlink(backup, recursive = TRUE), add = TRUE)
  existiam <- file.exists(todos)
  copias <- file.path(backup, as.character(seq_along(todos)))
  sha <- function(p) digest::digest(file = p, algo = "sha256")
  originais_sha <- rep(NA_character_, length(todos))
  for (i in which(existiam)) if (!file.copy(todos[[i]], copias[[i]], copy.mode = FALSE)) {
    stop("Nao foi possivel preservar a evidencia anterior antes da publicacao.", call. = FALSE)
  }
  for (i in which(existiam)) {
    originais_sha[[i]] <- sha(todos[[i]])
    if (!identical(originais_sha[[i]], sha(copias[[i]]))) stop("Backup da evidencia anterior diverge em SHA256.", call. = FALSE)
  }
  data.table::fwrite(data.table::data.table(destino = todos, backup = basename(copias), existia = existiam, sha256 = originais_sha), file.path(backup, "inventario.csv"), bom = TRUE)
  if (!is.null(falhar_apos) && (!is.numeric(falhar_apos) || length(falhar_apos) != 1L || !is.finite(falhar_apos) ||
      falhar_apos != as.integer(falhar_apos) || falhar_apos < 0L || falhar_apos > length(fontes))) stop("Ponto de falha de teste invalido.", call. = FALSE)
  erro <- tryCatch({
    if (!is.null(falhar_apos) && falhar_apos == 0L) stop("Falha simulada antes da publicacao.", call. = FALSE)
    for (i in seq_along(fontes)) {
      tmp <- tempfile("ev_novo_", tmpdir = dirname(destinos[[i]]))
      ok <- file.copy(fontes[[i]], tmp, copy.mode = FALSE)
      if (ok && file.exists(destinos[[i]])) ok <- unlink(destinos[[i]]) == 0L
      if (ok) ok <- file.rename(tmp, destinos[[i]])
      if (ok) ok <- identical(sha(fontes[[i]]), sha(destinos[[i]]))
      if (!ok) {
        unlink(tmp)
        stop("Falha ao publicar a familia de figuras.", call. = FALSE)
      }
      if (!is.null(falhar_apos) && i == falhar_apos) stop("Falha simulada durante a publicacao.", call. = FALSE)
    }
    for (arq in setdiff(remover, destinos)) if (file.exists(arq) && unlink(arq) != 0L) {
      stop("Falha ao remover uma continuacao obsoleta da figura.", call. = FALSE)
    }
    NULL
  }, error = identity)
  if (!is.null(erro)) {
    limpar_backup <- FALSE
    restaurados <- vapply(seq_along(todos), function(i) {
      tryCatch({
        if (existiam[[i]]) isTRUE(file.copy(copias[[i]], todos[[i]], overwrite = TRUE, copy.mode = FALSE)) && identical(sha(todos[[i]]), originais_sha[[i]])
        else !file.exists(todos[[i]]) || unlink(todos[[i]]) == 0L
      }, error = function(e) FALSE)
    }, logical(1L))
    if (!all(restaurados)) {
      limpar_backup <- FALSE
      stop("Publicacao falhou e a restauracao da familia ficou incompleta: ", conditionMessage(erro),
        ". Backup e inventario preservados em ", normalizePath(backup, winslash = "/", mustWork = TRUE), call. = FALSE)
    }
    limpar_backup <- TRUE
    stop(erro)
  }
  invisible(TRUE)
}

monitora_v305_figura_evidencia <- function(dados, titulo, arquivo, paleta, cfg_num,
                                          fonte_min_pt = 7, largura = 6.65, altura_max = 8) {
  if (!is.data.frame(dados)) stop("A evidencia exige uma tabela de resultados.", call. = FALSE)
  if (length(fonte_min_pt) != 1L || !is.finite(fonte_min_pt) || fonte_min_pt < 7 ||
      length(largura) != 1L || !is.finite(largura) || largura <= 0 || largura > 6.65 ||
      length(altura_max) != 1L || !is.finite(altura_max) || altura_max <= 0 || altura_max > 8) {
    stop("Evidencia exige fonte >=7 pt e pagina de ate 6,65 x 8 polegadas.", call. = FALSE)
  }
  x <- data.table::copy(data.table::as.data.table(dados))
  meta <- c("arquivo_canonico", "input_row", "cell_id", "pagina", "arquivo")
  if (any(meta %in% names(x))) stop("Coluna de origem reservada para o mapa de evidencia: ", paste(intersect(meta, names(x)), collapse = ", "), call. = FALSE)
  paginas_vazias <- data.table::data.table(arquivo_canonico = character(), arquivo = character(), pagina = integer(), n_paginas = integer(), n_resultados = integer(), md5 = character(),
    sha256 = character(), celulas_arquivo = character(), celulas_md5 = character(), celulas_sha256 = character(), fonte_pt = numeric(), largura_pol = numeric(), altura_pol = numeric(),
    fit = logical(), fit_w = logical(), fit_h = logical(), largura_celula_min_mm = numeric(), altura_celula_min_mm = numeric(), texto_largura_max_mm = numeric(), texto_altura_max_mm = numeric(),
    rotulo_indicador_altura_max_mm = numeric(), rotulo_periodo_largura_max_mm = numeric(), cabecalho_largura_max_mm = numeric(), cabecalho_altura_total_mm = numeric())
  if (!nrow(x)) {
    for (cc in meta) data.table::set(x, j = cc, value = if (cc %in% c("input_row", "pagina")) integer() else character())
    ans <- character()
    attr(ans, "paginas") <- paginas_vazias
    attr(ans, "celulas") <- x
    return(invisible(ans))
  }
  obrig <- c("Formação", "Indicador", "Periodo", "classe_periodo_label", "rotulo_celula")
  if (length(setdiff(obrig, names(x)))) stop("Campos ausentes na evidencia: ", paste(setdiff(obrig, names(x)), collapse = ", "), call. = FALSE)
  for (cc in c("Formação", "Indicador", "Periodo")) {
    if (any(is.na(x[[cc]]) | !nzchar(trimws(as.character(x[[cc]]))))) stop("Coordenada ausente ou vazia na evidencia: ", cc, call. = FALSE)
  }
  if (length(arquivo) != 1L || is.na(arquivo) || !nzchar(arquivo) || tolower(tools::file_ext(arquivo)) != "png") {
    stop("A evidencia exige um caminho PNG unico.", call. = FALSE)
  }
  codificar <- function(v) {
    z <- enc2utf8(as.character(v)); falta <- is.na(v); z[falta] <- ""
    paste0(ifelse(falta, "N", "S"), nchar(z, type = "bytes"), ":", z)
  }
  chave <- function(cols) do.call(paste, c(lapply(x[, ..cols], codificar), sep = "|"))
  coord <- chave(c("Formação", "Indicador", "Periodo"))
  semantica <- c("grupo_grafico", "tipo_metrica", "tipo_resultado", "form_veg", "categoria", "ano_1", "ano_2")
  ids_base <- if (all(semantica %in% names(x))) chave(semantica) else coord
  if (anyDuplicated(coord) || anyDuplicated(ids_base)) stop("Coordenadas ou chaves duplicadas no painel de evidencia.", call. = FALSE)
  ids <- paste0("cell_", vapply(ids_base, digest::digest, character(1L), algo = "sha256", serialize = FALSE))
  if (anyDuplicated(ids)) stop("Colisao nos identificadores das celulas.", call. = FALSE)
  mostrar <- function(v) { z <- enc2utf8(as.character(v)); z[is.na(z) | !nzchar(z)] <- "(não informado)"; z }
  ordenar <- function(v) {
    usados <- unique(mostrar(v))
    niveis <- if (is.factor(v)) enc2utf8(levels(v)) else character()
    if (is.factor(v)) c(intersect(niveis, usados), sort(setdiff(usados, niveis), method = "radix"))
    else sort(usados, method = "radix")
  }
  formas <- ordenar(x$Formação); indicadores <- ordenar(x$Indicador); periodos <- ordenar(x$Periodo)
  d <- data.table::data.table(.form = mostrar(x$Formação), .ind = mostrar(x$Indicador), .per = mostrar(x$Periodo),
    .label = as.character(x$rotulo_celula), .classe = as.character(x$classe_periodo_label))
  d[is.na(.label), .label := "NA"]
  quebrar <- function(v, n) vapply(as.character(v), function(z) {
    partes <- strsplit(z, "\n", fixed = TRUE)[[1L]]
    paste(unlist(lapply(partes, function(s) {
      linhas <- strwrap(s, width = n)
      if (!length(linhas)) return("")
      unlist(lapply(linhas, function(l) substring(l, seq.int(1L, max(1L, nchar(l)), by = n), pmin(seq.int(1L, max(1L, nchar(l)), by = n) + n - 1L, nchar(l)))))
    })), collapse = "\n")
  }, character(1L))
  # Mesma familia, resolucao e dispositivo das paginas; nenhuma medicao abre PDF.
  medida_dev <- tempfile(fileext = ".png")
  grDevices::png(medida_dev, width = 1, height = 1, units = "in", res = 300, type = "cairo")
  medida_numero <- grDevices::dev.cur()
  on.exit({ if (medida_numero %in% grDevices::dev.list()) grDevices::dev.off(medida_numero); unlink(medida_dev) }, add = TRUE)
  fisico <- function(v, n, limite_mm, pt = fonte_min_pt, bold = FALSE) {
    vapply(as.character(v), function(s) {
      if (is.na(s)) s <- "NA"
      limite <- n
      repeat {
        z <- quebrar(s, limite)
        q <- grid::textGrob(z, gp = grid::gpar(fontsize = pt, fontfamily = "sans", fontface = if (bold) "bold" else "plain", lineheight = .95))
        w <- grid::convertWidth(grid::grobWidth(q), "mm", valueOnly = TRUE)
        if (is.finite(w) && w < limite_mm) return(unname(z))
        if (limite == 1L) stop("Um glifo nao cabe na largura fisica da pagina.", call. = FALSE)
        limite <- max(1L, min(limite - 1L, floor(limite * limite_mm / w * .95)))
      }
    }, character(1L), USE.NAMES = FALSE)
  }
  canvas_mm <- largura * 25.4
  ind_label <- function(v) fisico(v, 27L, canvas_mm * .25)
  per_label <- function(v) fisico(v, 15L, canvas_mm * .2, bold = TRUE)
  titulo_pt <- max(9, fonte_min_pt)
  construir <- function(ii, parte = "Parte 000 de 000") {
    z <- data.table::copy(d[ii])
    z[, `:=`(.form = factor(.form, levels = intersect(formas, unique(.form))),
      .ind = factor(.ind, levels = intersect(indicadores, unique(.ind))),
      .per = factor(.per, levels = intersect(periodos, unique(.per))))]
    z[, .ord := ids[ii]]
    data.table::setorder(z, .form, .per, .ind, .ord)
    p <- ggplot2::ggplot(z, ggplot2::aes(.per, .ind, fill = .classe)) +
      ggplot2::geom_tile(colour = "white", linewidth = .3) +
      ggplot2::geom_text(ggplot2::aes(label = .label), size = fonte_min_pt / ggplot2::.pt, lineheight = .9, family = "sans", colour = "#17231E") +
      ggplot2::facet_grid(rows = ggplot2::vars(.form), scales = "free_y", space = "free_y",
        labeller = ggplot2::labeller(.form = function(v) fisico(v, 18L, canvas_mm * .2, bold = TRUE))) +
      ggplot2::scale_y_discrete(labels = ind_label) +
      ggplot2::scale_x_discrete(labels = per_label) +
      ggplot2::scale_fill_manual(values = paleta, drop = TRUE, na.value = "#F0F0F0",
        labels = function(v) fisico(v, 28L, canvas_mm * .26)) +
      ggplot2::labs(x = "Período comparado", y = NULL, fill = NULL,
        title = paste(fisico(titulo, 68L, canvas_mm * .9, titulo_pt, TRUE), parte, sep = "\n"),
        subtitle = fisico("Cada célula: efeito [IC95%], n e q (FDR-BH). LB = linha de base acumulada; AUM/RED/EST/INC/PAR = aumento/redução/estabilidade/inconclusivo/pares insuficientes; MUD/EST-C = mudança/estabilidade da composição; H = distância de Hellinger.", 101L, canvas_mm * .9),
        caption = fisico(paste0("Mudança direcional: q ≤ ", cfg_num("alpha", .05), ", IC95% sem cruzar zero e efeito mínimo de ", cfg_num("efeito_minimo_pp", 2), " p.p. Estabilidade: IC95% contido em ± ", cfg_num("margem_equivalencia_pp", 5), " p.p. Valores completos da linha de base permanecem na auditoria. Associação temporal não implica causalidade."), 105L, canvas_mm * .9)) +
      ggplot2::theme_minimal(base_size = fonte_min_pt, base_family = "sans") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = titulo_pt, face = "bold", colour = "#174B3B", lineheight = .95),
        plot.title.position = "plot", plot.subtitle = ggplot2::element_text(size = fonte_min_pt, lineheight = .95),
        plot.caption = ggplot2::element_text(size = fonte_min_pt, hjust = 0, lineheight = .95), plot.caption.position = "plot",
        panel.grid = ggplot2::element_blank(), panel.spacing = grid::unit(2, "mm"),
        axis.text.x = ggplot2::element_text(size = fonte_min_pt, face = "bold"),
        axis.text.y = ggplot2::element_text(size = fonte_min_pt, lineheight = .9),
        strip.text.y = ggplot2::element_text(size = fonte_min_pt, face = "bold", angle = 0),
        legend.position = "bottom", legend.text = ggplot2::element_text(size = fonte_min_pt),
        legend.key.height = grid::unit(3, "mm"), legend.key.width = grid::unit(3, "mm"), plot.margin = ggplot2::margin(4, 4, 4, 4)) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3, byrow = TRUE))
    list(plot = p, dados = z)
  }
  medir <- function(obj) {
    dev <- tempfile(fileext = ".png")
    grDevices::png(dev, width = largura, height = altura_max, units = "in", res = 300, type = "cairo")
    numero <- grDevices::dev.cur()
    on.exit({ if (numero %in% grDevices::dev.list()) grDevices::dev.off(numero); unlink(dev) }, add = TRUE)
    b <- ggplot2::ggplot_build(obj$plot); g <- ggplot2::ggplot_gtable(b)
    grid::grid.newpage(); grid::grid.draw(g); grid::grid.force()
    nomes <- unique(grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name)
    lay <- b$layout$layout; z <- obj$dados
    cw <- ch <- rep(NA_real_, nrow(z))
    for (j in seq_len(nrow(lay))) {
      # O vinculo usa a chave do facet, nunca a posicao de uma contagem externa.
      nome <- nomes[grepl(paste0("^panel-", lay$COL[[j]], "-", lay$ROW[[j]], "[.]"), nomes)]
      if (length(nome) != 1L) stop("Viewport do facet nao identificado univocamente.", call. = FALSE)
      grid::seekViewport(nome)
      w <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
      h <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
      grid::upViewport(0)
      par <- b$layout$panel_params[[as.integer(lay$PANEL[[j]])]]
      xr <- diff(par$x.range); yr <- diff(par$y.range)
      if (length(xr) != 1L || length(yr) != 1L || !is.finite(xr + yr) || xr <= 0 || yr <= 0) stop("Escala fisica do facet invalida.", call. = FALSE)
      hit <- as.character(z$.form) == as.character(lay$.form[[j]])
      cw[hit] <- w / xr; ch[hit] <- h / yr
    }
    texto <- function(v, dim, bold = FALSE, lineheight = .9, pt = fonte_min_pt) vapply(v, function(s) {
      q <- grid::textGrob(s, gp = grid::gpar(fontsize = pt, fontfamily = "sans", fontface = if (bold) "bold" else "plain", lineheight = lineheight))
      if (dim == "w") grid::convertWidth(grid::grobWidth(q), "mm", valueOnly = TRUE)
      else grid::convertHeight(grid::grobHeight(q), "mm", valueOnly = TRUE)
    }, numeric(1L))
    tw <- texto(z$.label, "w"); th <- texto(z$.label, "h")
    ah <- texto(ind_label(z$.ind), "h")
    aw <- texto(per_label(z$.per), "w", TRUE, 1.2)
    cab <- c(texto(obj$plot$labels$title, "w", TRUE, .95, titulo_pt), texto(c(obj$plot$labels$subtitle, obj$plot$labels$caption), "w", FALSE, .95))
    cab_h <- sum(c(texto(obj$plot$labels$title, "h", TRUE, .95, titulo_pt), texto(c(obj$plot$labels$subtitle, obj$plot$labels$caption), "h", FALSE, .95)))
    if (any(!is.finite(c(cab, cab_h))) || max(cab) >= canvas_mm * .94 || cab_h >= altura_max * 25.4 - 15) stop("Cabecalho nao cabe na pagina com fonte legivel.", call. = FALSE)
    valido <- all(is.finite(c(cw, ch, tw, th, ah, aw))) && all(cw > 0 & ch > 0)
    fw <- valido && all(pmax(tw, aw) < .93 * cw)
    fh <- valido && all(pmax(th, ah) < .93 * ch)
    list(fit = fw && fh, fit_w = fw, fit_h = fh,
      largura_celula_min_mm = min(cw), altura_celula_min_mm = min(ch),
      texto_largura_max_mm = max(tw), texto_altura_max_mm = max(th),
      rotulo_indicador_altura_max_mm = max(ah), rotulo_periodo_largura_max_mm = max(aw),
      cabecalho_largura_max_mm = max(cab), cabecalho_altura_total_mm = cab_h)
  }
  repartir <- function(ii) {
    med <- medir(construir(ii))
    if (isTRUE(med$fit)) return(list(ii))
    pp <- intersect(periodos, unique(d$.per[ii])); yy <- intersect(indicadores, unique(d$.ind[ii]))
    por_periodo <- !med$fit_w && length(pp) > 1L
    if (por_periodo || (length(yy) < 2L && length(pp) > 1L)) {
      corte <- floor(length(pp) / 2L)
      a <- ii[d$.per[ii] %in% pp[seq_len(corte)]]
    } else if (length(yy) > 1L) {
      corte <- floor(length(yy) / 2L)
      a <- ii[d$.ind[ii] %in% yy[seq_len(corte)]]
    } else stop("Uma celula de evidencia nao cabe com fonte de ", fonte_min_pt, " pt: ", ids[ii[[1L]]], call. = FALSE)
    c(repartir(a), repartir(setdiff(ii, a)))
  }
  inicial <- seq_len(nrow(x))
  primeiro <- medir(construir(inicial))
  plano <- if (isTRUE(primeiro$fit)) list(inicial) else unlist(lapply(formas, function(f) repartir(which(d$.form == f))), recursive = FALSE)
  todos <- unlist(plano, use.names = FALSE)
  if (length(todos) != nrow(x) || anyDuplicated(todos) || !setequal(todos, inicial)) stop("Paginacao nao reconciliou todas as celulas.", call. = FALSE)
  n <- length(plano)
  objetos <- lapply(seq_len(n), function(i) construir(plano[[i]], paste0("Parte ", i, " de ", n)))
  medidas <- lapply(objetos, medir)
  if (!all(vapply(medidas, function(v) isTRUE(v$fit), logical(1L)))) stop("Uma pagina final nao passou na verificacao fisica.", call. = FALSE)
  manifestos <- monitora_v308_evidencia_manifestos(arquivo)
  prefixo <- sub("_paginas[.]csv$", "", basename(manifestos$paginas))
  caminhos <- c(arquivo, if (n > 1L) file.path(dirname(arquivo), paste0(prefixo, "_p", sprintf("%03d", 2:n), ".png")) else character())
  antigos <- character()
  if (file.exists(manifestos$paginas)) {
    old <- data.table::fread(manifestos$paginas, showProgress = FALSE)
    if (!all(c("arquivo_canonico", "arquivo") %in% names(old)) || any(old$arquivo_canonico != basename(arquivo))) stop("Manifesto anterior pertence a outra familia.", call. = FALSE)
    permitidos <- old$arquivo == basename(arquivo) | grepl(paste0("^", prefixo, "_p[0-9]+[.]png$"), old$arquivo)
    if (anyNA(permitidos) || !all(permitidos)) stop("Caminho invalido no manifesto anterior.", call. = FALSE)
    antigos <- file.path(dirname(arquivo), old$arquivo)
  }
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("ev_stage_", tmpdir = dirname(arquivo)); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  fontes_png <- file.path(stage, basename(caminhos))
  for (i in seq_len(n)) ggplot2::ggsave(fontes_png[[i]], objetos[[i]]$plot,
    device = function(filename, width, height, bg, ...) grDevices::png(filename, width = width, height = height, units = "in", res = 300, bg = bg, type = "cairo"),
    width = largura, height = altura_max, dpi = 300, bg = "white", limitsize = FALSE)
  if (!all(file.exists(fontes_png)) || any(file.info(fontes_png)$size <= 0)) stop("Uma pagina nao foi materializada.", call. = FALSE)
  mapa <- data.table::copy(x)
  mapa[, `:=`(arquivo_canonico = basename(arquivo), input_row = inicial, cell_id = ids, pagina = NA_integer_, arquivo = NA_character_)]
  for (i in seq_len(n)) mapa[plano[[i]], `:=`(pagina = i, arquivo = basename(caminhos[[i]]))]
  if (anyNA(mapa$pagina) || anyDuplicated(mapa$cell_id)) stop("Mapa de celulas incompleto.", call. = FALSE)
  mapa_stage <- file.path(stage, basename(manifestos$celulas))
  data.table::fwrite(mapa, mapa_stage, bom = TRUE, na = "", eol = "\n")
  paginas <- data.table::rbindlist(lapply(seq_len(n), function(i) {
    m <- medidas[[i]]
    data.table::as.data.table(c(list(arquivo_canonico = basename(arquivo), arquivo = basename(caminhos[[i]]), pagina = i, n_paginas = n,
      n_resultados = length(plano[[i]]), md5 = unname(tools::md5sum(fontes_png[[i]])),
      sha256 = digest::digest(file = fontes_png[[i]], algo = "sha256"), celulas_arquivo = basename(manifestos$celulas),
      celulas_md5 = unname(tools::md5sum(mapa_stage)), celulas_sha256 = digest::digest(file = mapa_stage, algo = "sha256"),
      fonte_pt = fonte_min_pt, largura_pol = largura, altura_pol = altura_max), m))
  }))
  if (sum(paginas$n_resultados) != nrow(x) || anyNA(paginas$md5)) stop("Manifesto fisico nao reconciliado.", call. = FALSE)
  paginas_stage <- file.path(stage, basename(manifestos$paginas))
  data.table::fwrite(paginas, paginas_stage, bom = TRUE, na = "", eol = "\n")
  # O indice legado continua disponivel; as provas de familia sao os dois manifestos.
  legado <- data.table::copy(paginas)
  legado[, `:=`(resultados = n_resultados, periodos = vapply(plano, function(ii) data.table::uniqueN(d$.per[ii]), integer(1L)),
    linhas = vapply(plano, function(ii) nrow(unique(d[ii, .(.form, .ind)])), integer(1L)),
    largura_celula_mm = largura_celula_min_mm, altura_celula_mm = altura_celula_min_mm,
    texto_largura_mm = texto_largura_max_mm, texto_altura_mm = texto_altura_max_mm)]
  legado_path <- file.path(dirname(dirname(arquivo)), "layout_evidencias.csv")
  if (file.exists(legado_path)) {
    anterior <- data.table::fread(legado_path, showProgress = FALSE)
    keep <- !(anterior$arquivo %in% basename(c(antigos, caminhos)))
    if ("arquivo_canonico" %in% names(anterior)) keep <- keep & (is.na(anterior$arquivo_canonico) | anterior$arquivo_canonico != basename(arquivo))
    legado <- data.table::rbindlist(list(anterior[which(keep)], legado), fill = TRUE)
  }
  legado_stage <- file.path(stage, "layout_evidencias.csv")
  data.table::fwrite(legado, legado_stage, bom = TRUE, na = "", eol = "\n")
  # O manifesto de paginas e publicado por ultimo; todo erro restaura o conjunto.
  monitora_v308_evidencia_publicar(c(fontes_png, mapa_stage, legado_stage, paginas_stage),
    c(caminhos, manifestos$celulas, legado_path, manifestos$paginas), setdiff(antigos, caminhos))
  attr(caminhos, "paginas") <- paginas
  attr(caminhos, "celulas") <- mapa
  invisible(caminhos)
}
