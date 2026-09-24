# Estado por execução; handlers locais não alteram opções nem handlers do RStudio.
monitora_operacao_msg <- function(etapa, ...) {
  message("[", format(Sys.time(), "%H:%M:%S"), "][", etapa, "] ", paste0(..., collapse = ""))
}
monitora_aviso_registrar <- function(w) {
  estado <- get0("MONITORA_AVISOS_ESTADO", inherits = TRUE)
  if (is.null(estado) || isTRUE(estado$gravando)) return(invisible(NULL))
  estado$gravando <- TRUE
  on.exit(estado$gravando <- FALSE)
  etapa <- estado$etapa
  msg <- conditionMessage(w)
  chamada <- paste(deparse(conditionCall(w), width.cutoff = 160L), collapse = " ")
  chave <- paste(etapa, msg, chamada, sep = "\r")
  i <- match(chave, estado$chaves)
  if (is.na(i)) {
    estado$chaves <- c(estado$chaves, chave)
    i <- length(estado$chaves)
    estado$itens[[i]] <- data.frame(etapa = etapa, mensagem = msg, chamada = chamada,
      ocorrencias = 0L, primeiro = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), ultimo = "")
    monitora_operacao_msg("AVISO", msg, " | etapa: ", etapa, "; detalhes em log/avisos_execucao_*.csv")
  }
  estado$itens[[i]]$ocorrencias <- estado$itens[[i]]$ocorrencias + 1L
  estado$itens[[i]]$ultimo <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  pasta <- get0("MONITORA_LOG_DIR", ifnotfound = NULL, inherits = TRUE)
  if (!is.null(pasta) && dir.exists(pasta)) {
    arquivo <- file.path(pasta, paste0("avisos_execucao_", get0("MONITORA_EXEC_ID", ifnotfound = estado$id, inherits = TRUE), ".csv"))
    tryCatch(utils::write.csv(do.call(rbind, estado$itens), arquivo, row.names = FALSE, fileEncoding = "UTF-8"),
      error = function(e) message("[AVISO] Falha ao persistir avisos: ", conditionMessage(e)))
  }
  invisible(NULL)
}
monitora_relatorios_resolver_fase <- function(base_dir, fase) {
  legado <- file.path(base_dir, if (fase == "pre_painel") "relatorios_pre_painel" else "relatorios_pos_correcoes")
  raiz <- if (basename(base_dir) %in% c("correcoes_campos", "02_painel_correcoes")) dirname(base_dir) else base_dir
  organizado <- file.path(raiz, "02_painel_correcoes", "relatorios_apoio_tematicos", fase)
  caminhos <- unique(c(legado, organizado))
  arqs <- unique(unlist(lapply(caminhos[dir.exists(caminhos)], list.files, full.names = TRUE, recursive = FALSE)))
  arqs <- as.character(arqs)
  arqs <- arqs[file.exists(arqs) & !dir.exists(arqs)]
  if (!length(arqs)) return(setNames(character(), character()))
  nomes <- basename(arqs)
  for (n in unique(nomes[duplicated(nomes)])) {
    if (length(unique(as.character(tools::md5sum(arqs[nomes == n])))) != 1L)
      stop("Comparação pré/pós: fontes divergentes para ", fase, "/", n, "; resolver a duplicidade antes de comparar.", call. = FALSE)
  }
  setNames(arqs[!duplicated(nomes)], nomes[!duplicated(nomes)])
}
monitora_qfield_orientar_entrada <- function(base_dir, entrada_dir) {
  invertida <- file.path(base_dir, "input_qfield")
  if (dir.exists(invertida) && length(list.files(invertida, recursive = TRUE))) {
    monitora_qfield_avisar(paste0("Encontrada a pasta input_qfield, que não é lida. Coloque os insumos em ",
      entrada_dir, " e gere novamente o projeto para incorporá-los. Nenhum arquivo foi movido automaticamente."))
  }
}
