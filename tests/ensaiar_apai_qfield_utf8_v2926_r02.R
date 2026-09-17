args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop("Uso: Rscript <helpers> <candidata> <rodada_APAI> <mbtiles_z18> <saida_isolada> <nome_mbtiles_arbitrario>")
}
suppressPackageStartupMessages(library(data.table))
source(normalizePath(args[[1L]], mustWork = TRUE), local = FALSE, encoding = "UTF-8")
candidata <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
rodada <- normalizePath(args[[3L]], winslash = "/", mustWork = TRUE)
detalhe <- normalizePath(args[[4L]], winslash = "/", mustWork = TRUE)
destino <- normalizePath(args[[5L]], winslash = "/", mustWork = FALSE)
nome_mbtiles <- args[[6L]]
if (dir.exists(destino) || file.exists(destino)) stop("O destino isolado já existe; nada será sobrescrito.")
if (!grepl("^[A-Za-z0-9_. -]+[.]mbtiles$", nome_mbtiles, ignore.case = TRUE)) stop("Nome arbitrário de ensaio inválido.")

carregado <- monitora_test_funcoes(candidata)
env <- carregado$env
entrada <- file.path(destino, "qfield_input")
dir.create(entrada, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "output"), recursive = TRUE, showWarnings = FALSE)
mbtiles_copiado <- file.path(entrada, nome_mbtiles)
if (!file.copy(detalhe, mbtiles_copiado, overwrite = FALSE)) stop("Falha ao simular a cópia do MBTiles.")

produto <- file.path(rodada, "output")
env$MONITORA_BASE_DIR <- rodada
registros <- env$monitora_io_ler_csv_texto(file.path(produto, "01_produtos_dados", "registros_corrig_stat.csv"))
validacao <- env$monitora_io_ler_csv_texto(file.path(produto, "04_validacao_espacial", "pos_painel", "validacao_espacial_coletas.csv"))
consensos <- env$monitora_io_ler_csv_texto(file.path(produto, "04_validacao_espacial", "pos_painel", "validacao_espacial_consenso_ua.csv"))
if (uniqueN(registros$UC) != 1L || uniqueN(registros$UA) != 39L) stop("População APAI inesperada no ensaio.")
if (!validUTF8(registros$UC[[1L]]) || Encoding(registros$UC[[1L]]) != "UTF-8") stop("Leitor produtivo não marcou a UC como UTF-8.")

centros <- consensos[consenso_valido == TRUE & consenso_ambiguo == FALSE,
  .(lon = (inicio_lon_consenso + fim_lon_consenso) / 2,
    lat = (inicio_lat_consenso + fim_lat_consenso) / 2)]
if (nrow(centros) < 2L) stop("Consensos insuficientes para criar a estrada fictícia.")
linha_1 <- matrix(c(centros$lon[1L] - 0.002, centros$lat[1L] - 0.001,
                    centros$lon[1L] + 0.002, centros$lat[1L] + 0.001), ncol = 2L, byrow = TRUE)
linha_2 <- matrix(c(centros$lon[2L] - 0.001, centros$lat[2L] + 0.002,
                    centros$lon[2L] + 0.001, centros$lat[2L] - 0.002), ncol = 2L, byrow = TRUE)
estradas <- sf::st_sf(nome = c("Estrada fictícia A", "Estrada fictícia B"),
  observacao = "Camada sintética exclusiva da homologação",
  geometry = sf::st_sfc(sf::st_linestring(linha_1), sf::st_linestring(linha_2), crs = 4326))
kml_copiado <- file.path(entrada, "rede viaria ficticia.kml")
sf::st_write(estradas, kml_copiado, layer = "estradas_simuladas", driver = "KML", quiet = TRUE)
if (!file.exists(kml_copiado)) stop("KML fictício não foi materializado.")

hash_mbtiles_antes <- digest::digest(mbtiles_copiado, algo = "sha256", file = TRUE)
hash_kml_antes <- digest::digest(kml_copiado, algo = "sha256", file = TRUE)
inspecao_fonte <- env$monitora_qfield_inspecionar_mbtiles(mbtiles_copiado)
if (inspecao_fonte$papel[[1L]] != "detalhe" || inspecao_fonte$zoom_max[[1L]] != 18L ||
    inspecao_fonte$criterio_classificacao[[1L]] != "zoom_max_real=18") {
  stop("MBTiles não foi identificado como detalhe pelo zoom real.")
}

locale_anterior <- Sys.getlocale("LC_CTYPE")
on.exit(try(Sys.setlocale("LC_CTYPE", locale_anterior), silent = TRUE), add = TRUE)
if (!identical(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")), "C")) stop("Não foi possível ativar LC_CTYPE=C.")

resultado <- env$monitora_qfield_gerar(
  registros = registros, output_dir = file.path(destino, "output"), base_dir = destino,
  ativado = TRUE, importar = TRUE, adquirir_sentinel = FALSE,
  origem_ensaio = "APAI: simulação da leitura produtiva UTF-8; candidata v2.9.26 r02",
  validacao_espacial = validacao, consensos_espaciais = consensos
)
if (!identical(Sys.getlocale("LC_CTYPE"), "C")) stop("O módulo QField não restaurou o locale anterior.")
print(resultado)
if (nrow(resultado) != 1L || resultado$status[[1L]] != "gerado_para_homologacao_qfield") {
  stop("Candidata QField não concluiu: ", resultado$motivo[[1L]])
}
pacote <- dirname(resultado$projeto[[1L]])

recorte <- fread(file.path(pacote, "auditoria_recorte_circular.csv"))
imagens <- fread(file.path(pacote, "auditoria_imagens.csv"))
camadas <- fread(file.path(pacote, "auditoria_camadas.csv"))
cobertura <- fread(file.path(pacote, "auditoria_cobertura.csv"))
if (nrow(recorte) != 1L || recorte$acao[[1L]] != "recorte_gerado" || recorte$raio_m[[1L]] != 500 ||
    recorte$tiles_recortados[[1L]] < 1L || recorte$tiles_borda[[1L]] < 1L ||
    recorte$tiles_descartados[[1L]] < 1L || recorte$formato[[1L]] != "WebP RGBA, qualidade 80") {
  stop("O recorte físico circular não passou no gate.")
}
if (nrow(imagens) != 1L || imagens$papel[[1L]] != "detalhe" || imagens$zoom_max[[1L]] != 18L ||
    imagens$criterio_classificacao[[1L]] != "zoom_max_real=18") stop("Auditoria da classificação da imagem divergiu.")
if (nrow(camadas[grepl("rede viaria ficticia[.]kml", origem, fixed = FALSE) & feicoes == 2L]) != 1L) {
  stop("KML fictício de estradas não foi incorporado integralmente.")
}
arquivo_detalhe <- imagens$arquivo[[1L]]
if (!all(cobertura[arquivo == arquivo_detalhe]$cobertura_pixel)) stop("Imagem detalhada não cobre todos os extremos anuais.")

gpkg_recorte <- file.path(pacote, "recorte_imagens_500m.gpkg")
if (!file.exists(gpkg_recorte) || !setequal(sf::st_layers(gpkg_recorte)$name,
    c("uas_buffer_500m", "uas_pontos_medios"))) stop("GeoPackage interno do recorte incompleto.")
buffers <- sf::st_read(gpkg_recorte, layer = "uas_buffer_500m", quiet = TRUE)
pontos <- sf::st_read(gpkg_recorte, layer = "uas_pontos_medios", quiet = TRUE)
if (nrow(buffers) != 39L || nrow(pontos) != 39L || anyDuplicated(buffers$UA) ||
    !setequal(as.character(buffers$UA), unique(as.character(registros$UA))) ||
    any(abs(as.numeric(sf::st_area(buffers)) - pi * 500^2) / (pi * 500^2) > 0.02)) {
  stop("Buffers internos não correspondem a um círculo de 500 m por UA.")
}

centros_esperados <- consensos[, .(UA, lon = (inicio_lon_consenso + fim_lon_consenso) / 2,
  lat = (inicio_lat_consenso + fim_lat_consenso) / 2)]
centros_gerados <- as.data.table(sf::st_drop_geometry(pontos))[, .(UA, lon = lon_medio, lat = lat_medio)]
comparacao <- merge(centros_esperados, centros_gerados, by = "UA", suffixes = c("_esperado", "_gerado"))
if (nrow(comparacao) != 39L || any(abs(comparacao$lon_esperado - comparacao$lon_gerado) > 1e-12) ||
    any(abs(comparacao$lat_esperado - comparacao$lat_gerado) > 1e-12)) {
  stop("Centros dos buffers divergem dos consensos espaciais aceitos.")
}

inspecao_saida <- env$monitora_qfield_inspecionar_mbtiles(file.path(pacote, "mapas", arquivo_detalhe))
if (inspecao_saida$formato[[1L]] != "webp" || inspecao_saida$recorte_circular_m[[1L]] != "500" ||
    inspecao_saida$recorte_uc[[1L]] != unique(registros$UC) ||
    inspecao_saida$recorte_n_uas[[1L]] != "39" ||
    inspecao_saida$recorte_buffers_sha256[[1L]] != recorte$sha256_buffers_geometria[[1L]]) {
  stop("Metadados internos do MBTiles recortado não comprovam UC/buffers.")
}

doc <- xml2::read_xml(resultado$projeto[[1L]], options = "NONET")
if (length(xml2::xml_find_all(doc, ".//projectlayers/maplayer[layername='Google Satellite']")) != 1L ||
    length(xml2::xml_find_all(doc, ".//projectlayers/maplayer[contains(datasource,'camadas_adicionais.gpkg')]")) < 1L ||
    length(xml2::xml_find_all(doc, ".//projectlayers/maplayer[contains(datasource,'mapas/') or contains(datasource,'mapas\\') ]")) < 1L) {
  stop("QGS não contém imagem, camada adicional e base online esperadas.")
}
zip_info <- zip::zip_list(resultado$zip[[1L]])
obrigatorios <- c(basename(resultado$projeto[[1L]]), "recorte_imagens_500m.gpkg",
  "auditoria_recorte_circular.csv", "dados/camadas_adicionais.gpkg", paste0("mapas/", arquivo_detalhe))
if (!all(obrigatorios %in% zip_info$filename)) stop("ZIP QField não contém os produtos esperados.")
if (!identical(hash_mbtiles_antes, digest::digest(mbtiles_copiado, algo = "sha256", file = TRUE)) ||
    !identical(hash_kml_antes, digest::digest(kml_copiado, algo = "sha256", file = TRUE))) {
  stop("Um arquivo colocado pela usuária em qfield_input foi alterado.")
}
if (dir.exists(file.path(destino, "qfield_entrada")) ||
    dir.exists(file.path(entrada, env$monitora_qfield_slug(unique(registros$UC))))) {
  stop("A simulação criou dependência de pasta legada ou subpasta da UC.")
}

cat(sprintf(
  paste0("APAI_QFIELD_UTF8_V2926_R02_OK; locale=C; zoom=%d; UAs=%d; tiles=%d/%d; ",
    "redução=%.1f%%; KML=2 feições; cobertura=%d extremos; ZIP=%d bytes; fontes intactas; projeto=%s\n"),
  inspecao_fonte$zoom_max[[1L]], nrow(buffers), recorte$tiles_recortados[[1L]], recorte$tiles_fonte[[1L]],
  recorte$reducao_pct[[1L]], nrow(cobertura[arquivo == arquivo_detalhe]), resultado$bytes_zip[[1L]],
  resultado$projeto[[1L]]
))
