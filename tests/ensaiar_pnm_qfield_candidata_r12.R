args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 7L) stop("Uso: Rscript <helpers> <candidata> <rodada_PNM> <mbtiles_JPEG> <buffers_GPKG> <acessos_GPKG> <saida_isolada>")
suppressPackageStartupMessages(library(data.table))
source(normalizePath(args[[1L]], mustWork = TRUE), local = FALSE, encoding = "UTF-8")
candidata <- normalizePath(args[[2L]], mustWork = TRUE)
rodada <- normalizePath(args[[3L]], mustWork = TRUE)
detalhe <- normalizePath(args[[4L]], mustWork = TRUE)
buffers <- normalizePath(args[[5L]], mustWork = TRUE)
acessos <- normalizePath(args[[6L]], mustWork = TRUE)
destino <- normalizePath(args[[7L]], mustWork = FALSE)
env <- monitora_test_funcoes(candidata)$env
slug <- "parque_nacional_mapinguari"
entrada <- file.path(destino, "qfield_entrada", slug)
dir.create(file.path(entrada, "imagens"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(destino, "output"), recursive = TRUE, showWarnings = FALSE)
origem_projeto <- file.path(rodada, "qfield_entrada", slug, "projeto_qfield.csv")
ref <- file.path(rodada, "output", "09_qfield", "parque_nacional_mapinguari_f44869b6", "projeto_20260915_214719_c1e95248", "projeto")
origem_regional <- file.path(ref, "mapas", "sentinel_contexto.mbtiles")
origem_auditoria <- file.path(ref, "auditoria_imagens.csv")
stopifnot(file.exists(origem_projeto), file.exists(origem_regional), file.exists(origem_auditoria))
stopifnot(file.copy(origem_projeto, file.path(entrada, "projeto_qfield.csv"), overwrite = FALSE))
stopifnot(file.copy(detalhe, file.path(entrada, "detalhe_original_jpeg.mbtiles"), overwrite = FALSE))
stopifnot(file.copy(buffers, file.path(entrada, "recorte_imagens_500m.gpkg"), overwrite = FALSE))
stopifnot(file.copy(acessos, file.path(entrada, "PNM_acessos_trajetos_faixas.gpkg"), overwrite = FALSE))
stopifnot(file.copy(origem_regional, file.path(entrada, "imagens", "sentinel_contexto.mbtiles"), overwrite = FALSE))
fwrite(data.table(arquivo = "PNM_acessos_trajetos_faixas.gpkg", camada = "acessos_trajetos_faixas", papel = "acesso"), file.path(entrada, "camadas_qfield.csv"))
audit <- fread(origem_auditoria)
regional <- audit[arquivo == "sentinel_contexto.mbtiles", .(arquivo, papel, fonte, licenca, resolucao_nativa_m, data_imagem, ativo)]
if (nrow(regional) != 1L) stop("Sentinel regional de origem ambígua.")
fwrite(regional, file.path(entrada, "imagens", "fontes_imagens.csv"))
fonte_sha <- digest::digest(detalhe, algo = "sha256", file = TRUE)
buffer_sha <- digest::digest(buffers, algo = "sha256", file = TRUE)
acesso_sha <- digest::digest(acessos, algo = "sha256", file = TRUE)
registros <- fread(file.path(rodada, "output", "01_produtos_dados", "registros_corrig_stat.csv"), encoding = "UTF-8")
validacao <- fread(file.path(rodada, "output", "04_validacao_espacial", "pos_painel", "validacao_espacial_coletas.csv"), encoding = "UTF-8")
resultado <- env$monitora_qfield_gerar(registros = registros, output_dir = file.path(destino, "output"),
  base_dir = destino, ativado = TRUE, importar = TRUE, adquirir_sentinel = FALSE,
  origem_ensaio = "PNM: QField somente, candidata r12; não altera rodada original",
  validacao_espacial = validacao)
print(resultado)
if (nrow(resultado) != 1L || resultado$status[[1L]] != "gerado_para_homologacao_qfield") stop("Candidata QField não concluiu: ", resultado$motivo[[1L]])
pacote <- dirname(resultado$projeto[[1L]])
recorte <- fread(file.path(pacote, "auditoria_recorte_circular.csv"))
camadas <- fread(file.path(pacote, "auditoria_camadas.csv"))
cobertura <- fread(file.path(pacote, "auditoria_cobertura.csv"))
online <- fread(file.path(pacote, "auditoria_camadas_online.csv"))
if (nrow(recorte) != 1L || recorte$raio_m[[1L]] != 500 || recorte$tiles_recortados[[1L]] < 1L ||
    nrow(camadas[nome_exibido == "PNM_acessos" & papel == "acesso" & feicoes == 18L]) != 1L ||
    nrow(cobertura[arquivo == "02_detalhe.mbtiles"]) != 524L ||
    any(!cobertura[arquivo == "02_detalhe.mbtiles"]$cobertura_pixel)) stop("Corte, acessos ou cobertura não passaram no gate.")
if (nrow(online) != 1L || online$camada[[1L]] != "Google Satellite" || online$requer_internet[[1L]] != "S" ||
    online$empacota_tiles[[1L]] != "N" || online$ativo_inicial[[1L]] != "N") stop("Auditoria da camada online divergiu.")
doc <- xml2::read_xml(resultado$projeto[[1L]], options = "NONET")
mapa_online <- xml2::xml_find_all(doc, ".//projectlayers/maplayer[layername='Google Satellite']")
arvore_online <- xml2::xml_find_all(doc, ".//layer-tree-layer[@name='Google Satellite']")
if (length(mapa_online) != 1L || length(arvore_online) != 1L ||
    xml2::xml_text(xml2::xml_find_first(mapa_online, "./provider")) != "wms" ||
    xml2::xml_attr(arvore_online, "checked") != "Qt::Unchecked") stop("Google Satellite online ausente ou inválido no QGS.")
fonte_online <- xml2::xml_text(xml2::xml_find_first(mapa_online, "./datasource"))
if (!grepl("type=xyz", fonte_online, fixed = TRUE) || !grepl("https://mt1.google.com/vt/", fonte_online, fixed = TRUE) ||
    grepl("(?:^|[&?])(key|token|session)=", fonte_online, perl = TRUE, ignore.case = TRUE)) stop("Fonte online inválida ou contém credencial.")
if (length(xml2::xml_find_all(doc, ".//ProjectDisplaySettings[@CoordinateType='MapGeographic']")) != 1L) stop("Coordenadas geográficas decimais não preservadas.")
mapas <- list.files(file.path(pacote, "mapas"), pattern = "[.]mbtiles$", full.names = FALSE)
if (!setequal(mapas, c("01_regional.mbtiles", "02_detalhe.mbtiles"))) stop("Google gerou ou substituiu arquivo offline indevidamente.")
zip_info <- zip::zip_list(resultado$zip[[1L]])
if (!all(c(basename(resultado$projeto[[1L]]), "auditoria_camadas_online.csv", "mapas/01_regional.mbtiles", "mapas/02_detalhe.mbtiles") %in% zip_info$filename)) stop("ZIP QField incompleto.")
if (!identical(fonte_sha, digest::digest(detalhe, algo = "sha256", file = TRUE)) ||
    !identical(buffer_sha, digest::digest(buffers, algo = "sha256", file = TRUE)) ||
    !identical(acesso_sha, digest::digest(acessos, algo = "sha256", file = TRUE))) stop("Fonte original alterada no ensaio.")
cat(sprintf("CANDIDATA_QFIELD_R12_OK; Google online; recorte %.1f%%; ZIP %s bytes; acessos 18; extremos cobertos 524; originais intactos\n",
  recorte$reducao_pct[[1L]], resultado$bytes_zip[[1L]]))
