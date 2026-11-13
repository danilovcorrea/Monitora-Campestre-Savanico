# Prova documental focada: nao chama gerar(), estatistica, painel ou importacao.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
if (length(args) < 3L) stop("Uso: candidata.R destino_novo pasta_PNB_proporcao [pasta_PNB_cobertura]", call. = FALSE)
script <- normalizePath(args[1L], winslash = "/", mustWork = TRUE)
destino <- args[2L]
if (dir.exists(destino) && length(list.files(destino, all.files = TRUE, no.. = TRUE))) stop("Destino de prova ja contem arquivos; nao sobrescrever.", call. = FALSE)
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
destino <- normalizePath(destino, winslash = "/", mustWork = TRUE)
source("tools/v308/carregar.R")
e <- monitora_v308_carregar(script)
e$MONITORA_BASE_DIR <- dirname(destino)
e$MONITORA_OUTPUT_DIR <- destino
e$MONITORA_LOG_DIR <- file.path(destino, "log")
e$MONITORA_INPUT_DIR <- file.path(destino, "input_nao_utilizado")
e$MONITORA_EXEC_ID <- paste0("v308_documental_", format(Sys.time(), "%Y%m%d_%H%M%S"))
e$MONITORA_LOG_EXECUCAO <- data.table()
dir.create(e$MONITORA_LOG_DIR, showWarnings = FALSE)
inicio <- Sys.time()
hash <- function(p) digest::digest(file = p, algo = "sha256")
fontes <- args[-c(1L, 2L)]
registros <- list(); graficos <- list(); figuras <- character()
for (pasta in fontes) {
  pasta <- normalizePath(pasta, winslash = "/", mustWork = TRUE)
  manifests <- list.files(pasta, pattern = "_paginas[.]csv$", full.names = TRUE)
  if (length(manifests) != 1L) stop("Pasta deve conter uma familia PNB completa: ", pasta)
  paginas <- fread(manifests, encoding = "UTF-8")
  canonico <- file.path(pasta, unique(paginas$arquivo_canonico))
  info <- e$monitora_v308_ler_paginas_evidencia(canonico)
  stopifnot(info$manifesto)
  metrica <- if (grepl("proporcao", basename(pasta), ignore.case = TRUE)) "proporcao" else "cobertura"
  esperado <- if (metrica == "proporcao") 131L else 152L
  stopifnot(nrow(info$celulas) == esperado)
  nova <- file.path(destino, "figuras", paste0("PNB_", metrica))
  dir.create(nova, recursive = TRUE)
  sidecars <- e$monitora_v308_evidencia_manifestos(canonico)
  copiar <- c(info$arquivos, sidecars$paginas, sidecars$celulas)
  antes <- vapply(copiar, hash, character(1L))
  stopifnot(all(file.copy(copiar, nova, overwrite = FALSE)), identical(antes, vapply(file.path(nova, basename(copiar)), hash, character(1L))))
  relativo <- paste0("figuras/PNB_", metrica, "/", basename(canonico))
  legenda <- paste0("Evidência temporal do Parque Nacional de Brasília — ", if (metrica == "proporcao") "proporção relativa" else "cobertura vegetal",
    ". Todas as ", esperado, " comparações recebidas permanecem nas páginas deste conjunto, com seus efeitos, intervalos, pares e classificações originais.")
  figuras <- c(figuras, paste0("## ", if (metrica == "proporcao") "Proporção relativa" else "Cobertura vegetal"),
    e$monitora_relatorios_analiticos_figura(relativo, legenda))
  graficos[[length(graficos) + 1L]] <- data.table(id = paste0("inferencias_PNB_", metrica), disponivel = TRUE,
    arquivo_relatorio = relativo, hash_md5 = info$paginas$md5[1L], n_resultados_estatisticos = esperado,
    incorporada_relatorio_detalhado = TRUE, status_documental = "selecionada_relatorio_detalhado", painel_representante = paste0("inferencias_PNB_", metrica))
  registros[[length(registros) + 1L]] <- list(origem = pasta, arquivos = copiar, sha256_antes = unname(antes),
    metrica = metrica, n_resultados = esperado, n_paginas = nrow(info$paginas), arquivo_relativo = relativo,
    pagina_arquivos = paste0("figuras/PNB_", metrica, "/", info$paginas$arquivo),
    fonte_pt = info$paginas$fonte_pt, largura_pol = info$paginas$largura_pol, altura_pol = info$paginas$altura_pol,
    pagina_sha256 = info$paginas$sha256)
}
g <- rbindlist(graficos)
g <- e$monitora_v308_completar_indice_graficos(g, destino)
fwrite(g, file.path(destino, "indice_selecao_graficos.csv"), bom = TRUE)

# Avalia somente expressoes editoriais literais da propria candidata.
achar <- function(node, nome) {
  if (!is.call(node)) return(list())
  if (identical(node[[1L]], as.name("<-")) && is.symbol(node[[2L]]) && identical(as.character(node[[2L]]), nome)) return(list(node))
  unlist(lapply(as.list(node)[-1L], achar, nome = nome), recursive = FALSE)
}
ambiente <- new.env(parent = e)
ambiente$dir_relatorio <- destino
ambiente$uc <- "Parque Nacional de Brasília"
ambiente$periodo <- "prova documental de resultados previamente calculados"
ambiente$status_validacao <- "Verificação focada de paginação"
ambiente$logos_relatorio <- e$monitora_relatorios_analiticos_materializar_logos(destino)
for (nome in c("css", "yaml", "capa")) {
  expressions <- achar(body(e$monitora_relatorios_analiticos_gerar), nome)
  stopifnot(length(expressions) == 1L)
  eval(expressions[[1L]], envir = ambiente)
}
css_path <- file.path(destino, "estilo_relatorio_analitico.css")
writeLines(ambiente$css, css_path, useBytes = TRUE)
conteudo <- c(ambiente$yaml("Prova documental focada PNB"), ambiente$capa("detalhado"),
  "# Evidências temporais do PNB", "",
  "Este documento verifica a inclusão e a leitura das páginas das figuras. Utiliza exclusivamente resultados e imagens previamente calculados; não executa os testes estatísticos nem o pipeline da UC.", "",
  figuras, "", "# Referências e rastreabilidade", "",
  "As tabelas e os mapas de células da prova preservam a origem, o identificador e a página de cada comparação. As hipóteses continuam associativas e não estabelecem causalidade.")
expandido <- e$monitora_v308_expandir_figuras(conteudo, destino)
e$monitora_v308_validar_paginas_documentais(g, destino, expandido, character())
produtos <- e$monitora_relatorios_analiticos_renderizar(conteudo, "prova_documental_PNB", destino, css_path,
  c("rmd", "md", "html", "docx", "pdf"), navegador_pdf = e$monitora_relatorios_analiticos_resolver_navegador(),
  versao_editorial = "detalhado")
fwrite(produtos, file.path(destino, "PRODUTOS_PROVA_DOCUMENTAL.csv"), bom = TRUE)
for (fonte in registros) stopifnot(identical(fonte$sha256_antes, unname(vapply(fonte$arquivos, hash, character(1L)))))
esperados <- file.path(destino, paste0("prova_documental_PNB.", c("Rmd", "md", "html", "docx", "pdf")))
ok <- all(file.exists(esperados)) && all(file.info(esperados)$size > 100)
recibo <- list(status = if (ok) "RENDERED_PENDING_PHYSICAL_INDEPENDENT_CHECK" else "RENDER_FAILED",
  candidate = script, candidate_sha256 = hash(script), version = e$MONITORA_SCRIPT_VERSAO,
  started_utc = format(inicio, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), elapsed_seconds = as.numeric(difftime(Sys.time(), inicio, units = "secs")),
  sources = registros, expected_images = sum(vapply(registros, function(x) x$n_paginas, integer(1L))),
  expected_results = sum(vapply(registros, function(x) x$n_resultados, integer(1L))),
  files = esperados, source_files_unchanged = TRUE, scientific_pipeline_executed = FALSE)
jsonlite::write_json(recibo, file.path(destino, "PROVA_DOCUMENTAL_PNB.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
if (!ok) stop("Renderer nao entregou todos os formatos; consultar auditorias de erros.", call. = FALSE)
cat("PASS_RENDER_DOCUMENTAL", destino, "\n")
