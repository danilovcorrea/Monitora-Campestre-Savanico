#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
candidata <- normalizePath(
  if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.27.R",
  mustWork = TRUE
)
base_dir <- dirname(candidata)
source(file.path(base_dir, "tests", "helpers_test_funcoes.R"), encoding = "UTF-8")
carregada <- monitora_test_funcoes(candidata)
e <- carregada$env
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

locale_anterior <- Sys.getlocale("LC_CTYPE")
on.exit(try(Sys.setlocale("LC_CTYPE", locale_anterior), silent = TRUE), add = TRUE)

desmarcar_utf8 <- function(x) {
  x <- enc2utf8(x)
  Encoding(x) <- "unknown"
  x
}

uc <- desmarcar_utf8(paste0(intToUtf8(0x00C1L), "rea de Prote", intToUtf8(0x00E7L), intToUtf8(0x00E3L), "o Ambiental do Ibirapuit", intToUtf8(0x00E3L)))
assert(validUTF8(uc), "Fixture deveria conter bytes UTF-8 válidos.")
tracos <- c(
  "-",
  intToUtf8(0x2013L),
  intToUtf8(0x2014L),
  intToUtf8(0x2212L),
  paste0(intToUtf8(0x2014L), intToUtf8(0x2014L))
)
Encoding(tracos) <- "unknown"
entrada_ausencia <- c(NA_character_, "", " NA ", paste0(" ", tracos, " "), "texto preservado")
categoria <- desmarcar_utf8(paste0("vegeta", intToUtf8(0x00E7L), intToUtf8(0x00E3L), "o"))
dt <- data.frame(UC = uc, categoria = categoria, n = 1L, stringsAsFactors = FALSE)
fonte_bytes <- lapply(dt[c("UC", "categoria")], charToRaw)
fonte_encoding <- vapply(dt[c("UC", "categoria")], Encoding, character(1))
csv_fixture <- tempfile(fileext = ".csv")
csv_texto <- enc2utf8(paste0("UC,categoria\n", uc, ",", categoria, "\n"))
writeBin(charToRaw(csv_texto), csv_fixture)

locale_c <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
assert(identical(locale_c, "C"), "Não foi possível ativar LC_CTYPE=C para o gate.")

e$MONITORA_BASE_DIR <- dirname(csv_fixture)
lida <- e$monitora_io_ler_csv_texto(csv_fixture)
assert(validUTF8(lida$UC) && Encoding(lida$UC) == "UTF-8", "O leitor real não marcou a UC UTF-8.")
assert(validUTF8(lida$categoria) && Encoding(lida$categoria) == "UTF-8", "O leitor real não marcou a categoria UTF-8.")

destino <- tempfile(fileext = ".qgs")
e$monitora_qfield_escrever_qgs(
  destino = destino,
  uc = uc,
  camadas = list(),
  rasters = list(),
  bbox = c(-1000, -1000, 1000, 1000)
)
assert(file.exists(destino) && file.info(destino)$size > 0L, "QGS não foi materializado.")
doc <- xml2::read_xml(destino, options = "NONET")
titulo <- xml2::xml_text(xml2::xml_find_first(doc, "/qgis/title"))
contem_raw <- function(todo, parte) {
  todo <- as.integer(todo); parte <- as.integer(parte)
  if (!length(parte) || length(parte) > length(todo)) return(FALSE)
  any(vapply(seq_len(length(todo) - length(parte) + 1L), function(i) {
    identical(todo[i:(i + length(parte) - 1L)], parte)
  }, logical(1)))
}
assert(contem_raw(charToRaw(enc2utf8(titulo)), charToRaw(uc)), "Título UTF-8 da UC não foi preservado no QGS.")

saida_ausencia <- e$monitora_publicacao_ae_ausencia_fisica_na(entrada_ausencia)
assert(all(saida_ausencia[seq_len(length(saida_ausencia) - 1L)] == "NA"), "Ausências físicas não foram normalizadas.")
assert(identical(tail(saida_ausencia, 1L), "texto preservado"), "Texto substantivo foi alterado pelo sanitizador.")

normalizada <- e$monitora_relatorios_analiticos_dt(dt)
assert(identical(lapply(dt[c("UC", "categoria")], charToRaw), fonte_bytes), "A normalização do relatório alterou os bytes da fonte.")
assert(identical(vapply(dt[c("UC", "categoria")], Encoding, character(1)), fonte_encoding), "A normalização do relatório alterou a marcação da fonte.")
assert(identical(charToRaw(normalizada$UC), charToRaw(dt$UC)), "A normalização alterou os bytes do valor da UC.")
assert(validUTF8(normalizada$UC) && Encoding(normalizada$UC) == "UTF-8", "A UC do relatório não recebeu marcação UTF-8 explícita.")

invalido <- rawToChar(as.raw(0xFFL))
Encoding(invalido) <- "unknown"
erro <- tryCatch({ e$monitora_qfield_utf8(invalido); "" }, error = conditionMessage)
assert(nzchar(erro) && grepl("bytes inv", erro, fixed = TRUE), "Bytes realmente inválidos não foram bloqueados.")

cat(sprintf(
  "TEST_V2927_UTF8_WINDOWS_OK; locale=%s; qgs_bytes=%d; titulo=%s\n",
  locale_c, file.info(destino)$size, enc2utf8(titulo)
))
