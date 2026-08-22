#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
})

falhar <- function(...) stop(..., call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) falhar("Informe ao menos um diretório de execução.")

md5_seguro <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) return(NA_character_)
  unname(as.character(tools::md5sum(path)))
}

uc_da_run <- function(output_dir) {
  candidatos <- c(
    file.path(output_dir, "01_produtos_dados", "registros_validados.csv"),
    file.path(output_dir, "01_produtos_dados", "registros_corrig.csv")
  )
  for (f in candidatos[file.exists(candidatos)]) {
    cab <- names(fread(f, nrows = 0L, check.names = FALSE, showProgress = FALSE))
    col <- intersect(c("uc", "UC"), cab)
    if (!length(col)) next
    valor <- as.character(fread(
      f, nrows = 1L, select = col[[1L]], colClasses = "character",
      check.names = FALSE, showProgress = FALSE
    )[[1L]][[1L]])
    if (!is.na(valor) && nzchar(trimws(valor))) return(trimws(valor))
  }
  falhar("Não foi possível identificar a UC em ", output_dir)
}

substituir_texto <- function(path, mapa) {
  if (!file.exists(path) || !length(mapa)) return(FALSE)
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  y <- x
  for (ii in seq_len(nrow(mapa))) {
    y <- gsub(mapa$origem[ii], mapa$destino[ii], y, fixed = TRUE)
  }
  if (identical(x, y)) return(FALSE)
  writeLines(y, path, useBytes = TRUE)
  TRUE
}

processar_run <- function(run_dir) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  output_dir <- file.path(run_dir, "output")
  if (!dir.exists(output_dir)) falhar("output ausente: ", run_dir)
  uc <- uc_da_run(output_dir)
  alteracoes <- list()
  mapa <- data.table(origem = character(), destino = character())

  registrar_movimento <- function(origem, destino, tipo, nome_logico) {
    h_antes <- md5_seguro(origem)
    dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(destino)) falhar("Destino já existe: ", destino)
    if (!file.rename(origem, destino)) falhar("Falha ao mover: ", origem)
    h_depois <- md5_seguro(destino)
    if (!identical(h_antes, h_depois)) falhar("Hash mudou ao mover: ", origem)
    origem_rel <- substring(origem, nchar(output_dir) + 2L)
    destino_rel <- substring(destino, nchar(output_dir) + 2L)
    mapa <<- rbindlist(list(
      mapa,
      data.table(
        origem = gsub("\\\\", "/", origem_rel),
        destino = gsub("\\\\", "/", destino_rel)
      ),
      data.table(origem = basename(origem), destino = basename(destino))
    ), use.names = TRUE)
    alteracoes[[length(alteracoes) + 1L]] <<- data.table(
      tipo = tipo,
      nome_logico = nome_logico,
      caminho_anterior = gsub("\\\\", "/", origem_rel),
      caminho_atual = gsub("\\\\", "/", destino_rel),
      comprimento_anterior = nchar(origem),
      comprimento_atual = nchar(destino),
      md5_antes = h_antes,
      md5_depois = h_depois,
      conteudo_preservado = identical(h_antes, h_depois)
    )
    invisible(destino)
  }

  dir_ra <- file.path(output_dir, "08_relatorios_analiticos")
  dirs_uc <- if (dir.exists(dir_ra)) {
    list.dirs(dir_ra, recursive = FALSE, full.names = TRUE)
  } else character()
  for (dir_uc in dirs_uc) {
    docs <- list.files(
      dir_uc,
      pattern = "^relatorio_analitico_(sintetico|detalhado)_.*\\.(Rmd|md|html|docx|pdf)$",
      full.names = TRUE,
      ignore.case = FALSE
    )
    if (!length(docs) || max(nchar(docs)) <= 240L) next
    bases <- tools::file_path_sans_ext(basename(docs))
    periodo <- sub(
      ".*_([0-9]{4}(?:-[0-9]{4})?)$", "\\1", bases[[1L]], perl = TRUE
    )
    if (!grepl("^[0-9]{4}(?:-[0-9]{4})?$", periodo)) periodo <- "periodo"
    chave_uc <- paste0(nchar(enc2utf8(uc), type = "bytes"), ":", enc2utf8(uc))
    dir_id <- paste0(
      "uc-", substr(digest(chave_uc, algo = "sha256", serialize = FALSE), 1L, 10L)
    )
    dir_novo <- file.path(dir_ra, dir_id)
    if (dir.exists(dir_novo)) falhar("Diretório compacto já existe: ", dir_novo)

    for (doc in docs) {
      editorial <- if (grepl("_sintetico_", basename(doc), fixed = TRUE)) {
        "sintetico"
      } else {
        "detalhado"
      }
      ext <- tools::file_ext(doc)
      novo <- file.path(dir_uc, paste0("analitico_", editorial, ".", ext))
      registrar_movimento(
        doc, novo, paste0("relatorio_", editorial), basename(doc)
      )
    }
    origem_dir_rel <- substring(dir_uc, nchar(output_dir) + 2L)
    destino_dir_rel <- substring(dir_novo, nchar(output_dir) + 2L)
    if (!file.rename(dir_uc, dir_novo)) falhar("Falha ao compactar diretório: ", dir_uc)
    for (jj in seq_along(alteracoes)) {
      caminho_atual <- alteracoes[[jj]]$caminho_atual[[1L]]
      if (startsWith(caminho_atual, paste0(origem_dir_rel, "/"))) {
        caminho_atual <- paste0(
          destino_dir_rel,
          substring(caminho_atual, nchar(origem_dir_rel) + 1L)
        )
        alteracoes[[jj]][, `:=`(
          caminho_atual = gsub("\\\\", "/", caminho_atual),
          comprimento_atual = nchar(file.path(output_dir, caminho_atual))
        )]
      }
    }
    mapa <- rbindlist(list(
      mapa,
      data.table(
        origem = gsub("\\\\", "/", origem_dir_rel),
        destino = gsub("\\\\", "/", destino_dir_rel)
      )
    ), use.names = TRUE)

    idx_rel <- file.path(dir_novo, "indice_relatorios_analiticos.csv")
    if (file.exists(idx_rel)) {
      idx <- fread(idx_rel, encoding = "UTF-8")
      idx[, `:=`(
        uc = uc,
        periodo = periodo,
        diretorio_contexto = dir_id,
        caminho_fisico_compactado = TRUE
      )]
      if (!("nome_logico" %in% names(idx))) {
        idx[, nome_logico := vapply(seq_len(.N), function(ii) {
          linha <- alteracoes[[which(vapply(
            alteracoes,
            function(z) basename(z$caminho_atual) == basename(caminho_relativo[ii]),
            logical(1L)
          ))[1L]]]
          if (is.null(linha) || !nrow(linha)) basename(caminho_relativo[ii]) else linha$nome_logico[[1L]]
        }, character(1L))]
      }
      fwrite(idx, idx_rel, bom = TRUE, na = "")
    }
  }

  manifesto_path <- file.path(
    output_dir, "00_manifesto_execucao",
    "manifesto_planilha_importacao_sismonitora.csv"
  )
  if (file.exists(manifesto_path)) {
    man <- fread(manifesto_path, encoding = "UTF-8")
    if (!("produto_logico" %in% names(man))) man[, produto_logico := produto]
    for (ii in seq_len(nrow(man))) {
      origem <- file.path(output_dir, man$caminho_relativo[ii])
      if (!file.exists(origem) || nchar(origem) <= 210L) next
      novo_nome <- paste0("sis_", man$contexto_id[ii], ".xlsx")
      if (nchar(file.path(dirname(origem), novo_nome)) > 210L) {
        hash_ctx <- sub("^ctx-[0-9]{3}-", "", man$contexto_id[ii])
        novo_nome <- paste0("s_", hash_ctx, ".xlsx")
      }
      destino <- file.path(dirname(origem), novo_nome)
      registrar_movimento(
        origem, destino, "planilha_sismonitora", man$produto_logico[ii]
      )
      man[ii, `:=`(
        produto = novo_nome,
        caminho_relativo = paste0("01_produtos_dados/", novo_nome)
      )]
    }
    fwrite(man, manifesto_path, bom = TRUE, na = "")
  }

  dir_validacao <- file.path(output_dir, "07_relatorio_validacao")
  docs_validacao <- if (dir.exists(dir_validacao)) list.files(
    dir_validacao,
    pattern = "^relatorio_validacao_consolidado_.*\\.(Rmd|md|html|pdf|json)$",
    full.names = TRUE
  ) else character()
  if (length(docs_validacao) && any(
    nchar(docs_validacao[grepl("\\.(html|pdf)$", docs_validacao)]) > 240L
  )) {
    nomes_logicos_validacao <- basename(docs_validacao)
    for (ii in seq_along(docs_validacao)) {
      ext <- tools::file_ext(docs_validacao[ii])
      registrar_movimento(
        docs_validacao[ii],
        file.path(dir_validacao, paste0("validacao_consolidado.", ext)),
        "relatorio_validacao",
        nomes_logicos_validacao[ii]
      )
    }
    meta_caminho <- file.path(
      dir_validacao, "dados_apoio", "metadados_caminho_relatorio.csv"
    )
    fwrite(data.table(
      nome_logico = paste(nomes_logicos_validacao, collapse = " | "),
      base_fisica = "validacao_consolidado",
      caminho_fisico_compactado = TRUE
    ), meta_caminho, bom = TRUE, na = "")
  }

  mapa <- unique(mapa[nzchar(origem) & origem != destino])
  dirs_texto <- c(
    file.path(output_dir, "00_manifesto_execucao"),
    file.path(output_dir, "03_auditorias"),
    file.path(output_dir, "07_relatorio_validacao", "dados_apoio"),
    file.path(output_dir, "08_relatorios_analiticos"),
    file.path(run_dir, "log")
  )
  textos <- unique(unlist(lapply(dirs_texto[dir.exists(dirs_texto)], function(d) {
    list.files(d, pattern = "\\.(csv|txt)$", recursive = TRUE, full.names = TRUE)
  })))
  textos <- setdiff(textos, manifesto_path)
  alterados_texto <- textos[vapply(
    textos, substituir_texto, logical(1L), mapa = mapa
  )]

  auditoria <- if (length(alteracoes)) {
    rbindlist(alteracoes, fill = TRUE, use.names = TRUE)
  } else {
    data.table(
      tipo = "sem_alteracao_necessaria", nome_logico = "",
      caminho_anterior = "", caminho_atual = "",
      comprimento_anterior = NA_integer_, comprimento_atual = NA_integer_,
      md5_antes = "", md5_depois = "", conteudo_preservado = TRUE
    )
  }
  auditoria[, `:=`(
    build = "v2.9.16-20260822-r04",
    uc = uc,
    atualizado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )]
  aud_path <- file.path(
    output_dir, "00_manifesto_execucao", "caminhos_office_v2916.csv"
  )
  dir.create(dirname(aud_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(aud_path)) {
    auditoria <- unique(rbindlist(list(
      fread(aud_path, encoding = "UTF-8"), auditoria
    ), fill = TRUE, use.names = TRUE), by = c("tipo", "caminho_anterior", "caminho_atual"))
  }
  for (ii in which(nzchar(auditoria$caminho_atual))) {
    atual_abs <- file.path(output_dir, auditoria$caminho_atual[ii])
    if (file.exists(atual_abs)) next
    candidatos <- list.files(
      output_dir,
      pattern = paste0("^", gsub("([.])", "\\\\\\1", basename(auditoria$caminho_atual[ii])), "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(candidatos) == 1L) {
      auditoria[ii, `:=`(
        caminho_atual = gsub("\\\\", "/", substring(
          candidatos, nchar(output_dir) + 2L
        )),
        comprimento_atual = nchar(candidatos),
        md5_depois = md5_seguro(candidatos),
        conteudo_preservado = identical(md5_antes, md5_seguro(candidatos))
      )]
    }
  }
  fwrite(auditoria, aud_path, bom = TRUE, na = "")

  indice_path <- file.path(output_dir, "indice_produtos.csv")
  if (!file.exists(indice_path)) falhar("Índice ausente: ", indice_path)
  ind <- fread(indice_path, encoding = "UTF-8")
  for (ii in seq_len(nrow(mapa))) {
    ind[, caminho_relativo := gsub(
      mapa$origem[ii], mapa$destino[ii], caminho_relativo, fixed = TRUE
    )]
    if ("conteudo_identico_a" %in% names(ind)) {
      ind[, conteudo_identico_a := gsub(
        mapa$origem[ii], mapa$destino[ii], conteudo_identico_a, fixed = TRUE
      )]
    }
  }
  existentes <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
  existentes <- gsub("\\\\", "/", existentes)
  if (!("caminhos_office_v2916.csv" %in% basename(ind$caminho_relativo))) {
    nova <- ind[NA_integer_]
    nova[, `:=`(
      exec_id = as.character(ind$exec_id[[1L]]),
      contexto = "atualizacao_focal_caminhos_office_v2916",
      caminho_relativo = "00_manifesto_execucao/caminhos_office_v2916.csv",
      arquivo = "caminhos_office_v2916.csv",
      categoria = "00_manifesto_execucao",
      raiz = FALSE,
      permitido_na_raiz = FALSE,
      papel_produto = "auditoria",
      status_canonico = "canonico_na_categoria",
      produto_dados_canonico = FALSE,
      hash_verificavel = TRUE
    )]
    ind <- rbindlist(list(ind, nova), fill = TRUE, use.names = TRUE)
  }
  novos_rel <- setdiff(existentes, ind$caminho_relativo)
  for (rel_novo in novos_rel) {
    nova <- ind[NA_integer_]
    path_novo <- file.path(output_dir, rel_novo)
    nova[, `:=`(
      exec_id = as.character(ind$exec_id[[1L]]),
      contexto = "atualizacao_focal_caminhos_office_v2916",
      caminho_relativo = rel_novo,
      arquivo = basename(rel_novo),
      categoria = dirname(rel_novo),
      raiz = dirname(rel_novo) == ".",
      permitido_na_raiz = basename(rel_novo) %in% c("README_OUTPUT.txt", "indice_produtos.csv"),
      papel_produto = if (grepl("^07_relatorio_validacao/", rel_novo)) "relatorio_validacao" else "auditoria",
      status_canonico = "canonico_na_categoria",
      produto_dados_canonico = FALSE,
      tamanho_bytes = as.numeric(file.info(path_novo)$size),
      md5 = md5_seguro(path_novo),
      hash_verificavel = TRUE
    )]
    ind <- rbindlist(list(ind, nova), fill = TRUE, use.names = TRUE)
  }
  ind[, `:=`(
    arquivo = basename(caminho_relativo),
    categoria = dirname(caminho_relativo)
  )]
  caminhos_abs <- file.path(output_dir, ind$caminho_relativo)
  ext <- tolower(tools::file_ext(caminhos_abs))
  limites <- ifelse(
    ext %in% c("xls", "xlsx", "csv"), 210L,
    ifelse(ext %in% c("doc", "docx", "pdf", "html", "htm"), 240L, NA_integer_)
  )
  ind[, `:=`(
    comprimento_caminho_caracteres = nchar(caminhos_abs),
    limite_recomendado_windows = limites,
    situacao_caminho_office = ifelse(
      is.na(limites), "nao_aplicavel",
      ifelse(nchar(caminhos_abs) <= limites, "apto_abertura_windows", "revisar_caminho_windows")
    )
  )]
  mudou <- unique(c(
    alterados_texto,
    aud_path,
    unlist(lapply(alteracoes, function(z) file.path(output_dir, z$caminho_atual)))
  ))
  mudou_rel <- gsub("\\\\", "/", substring(mudou, nchar(output_dir) + 2L))
  for (rel in intersect(mudou_rel, ind$caminho_relativo)) {
    jj <- which(ind$caminho_relativo == rel)
    path <- file.path(output_dir, rel)
    ind[jj, `:=`(
      tamanho_bytes = as.numeric(file.info(path)$size),
      md5 = md5_seguro(path),
      hash_verificavel = TRUE
    )]
  }
  jj_self <- which(ind$caminho_relativo == "indice_produtos.csv")
  if (length(jj_self)) ind[jj_self, `:=`(
    tamanho_bytes = NA_real_, md5 = NA_character_, hash_verificavel = FALSE
  )]
  setorder(ind, caminho_relativo)
  fwrite(ind, indice_path, bom = TRUE, na = "")

  atuais <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
  atuais <- gsub("\\\\", "/", atuais)
  if (!setequal(atuais, ind$caminho_relativo)) {
    falhar("Índice não corresponde ao inventário final em ", run_dir)
  }
  manuais <- atuais[tolower(tools::file_ext(atuais)) %in% c(
    "doc", "docx", "pdf", "html", "htm", "xls", "xlsx"
  )]
  caminhos_manuais <- file.path(output_dir, manuais)
  limites_manuais <- ifelse(
    tolower(tools::file_ext(manuais)) %in% c("xls", "xlsx"), 210L, 240L
  )
  criticos <- caminhos_manuais[nchar(caminhos_manuais) > limites_manuais]
  docs_movidos <- as.character(unlist(lapply(alteracoes, function(z) {
    if (grepl("^relatorio_|^planilha_", z$tipo)) file.path(output_dir, z$caminho_atual) else character()
  }), use.names = FALSE))
  docs_ooxml <- docs_movidos[
    tolower(tools::file_ext(docs_movidos)) %in% c("docx", "xlsx") &
      file.exists(docs_movidos)
  ]
  for (f in docs_ooxml) {
    status <- system2("unzip", c("-tqq", shQuote(f)), stdout = FALSE, stderr = FALSE)
    if (!identical(status, 0L)) falhar("OOXML inválido após atualização: ", f)
  }
  cat(sprintf(
    "ATUALIZADO\t%s\tmovimentos=%d\tmanuais_criticos=%d\n",
    basename(run_dir), nrow(auditoria[tipo != "sem_alteracao_necessaria"]),
    length(criticos)
  ))
  invisible(list(auditoria = auditoria, criticos = criticos))
}

resultados <- lapply(args, processar_run)
if (any(vapply(resultados, function(x) length(x$criticos), integer(1L)) > 0L)) {
  warning("Permanecem documentos de acesso manual acima do orçamento; consulte o índice.")
}
