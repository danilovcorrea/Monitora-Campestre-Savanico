#!/usr/bin/env Rscript
# Fixtures sintéticas + funções reais; não executa pipeline, Shiny ou dados institucionais.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args)) args[1L] else ".", mustWork = TRUE)
env <- new.env(parent = globalenv())
sys.source(file.path(repo, "tools/v307/justificativas.R"), envir = env)
baseline <- file.path(repo, "monitora_campsav_alvo_global_v3.0.6.R")
arvore <- parse(baseline, keep.source = FALSE)
reais <- c("monitora_correcao_hash_texto", "monitora_pendencias_ocorrencia_id", "monitora_pendencias_justificativas_criar_lote",
  "monitora_pendencias_justificativas_normalizar_sessao", "monitora_pendencias_justificativas_reconstituir_lotes",
  "monitora_pendencias_justificativas_excluir_atomico", "normalizar_fila_checkpoint",
  "monitora_painel_checkpoint_dt", "monitora_painel_checkpoint_assinatura_filas", "monitora_painel_checkpoint_fingerprint_base",
  "monitora_pendencias_justificativas_checkpoint_recuperavel", "monitora_arquivo_retentativas",
  "monitora_arquivo_hash_transacao", "monitora_arquivo_publicar_candidato")
old <- new.env(parent = env)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  if (as.character(x[[1L]])[1L] %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) && is.call(x[[3L]]) && identical(x[[3L]][[1L]], as.name("function"))) {
    if (as.character(x[[2L]]) %in% reais) eval(x, old)
    if (identical(as.character(x[[2L]]), "monitora_correcao_painel")) coletar(x[[3L]][[3L]])
    return(invisible(NULL))
  }
  for (i in seq_along(x)[-1L]) coletar(x[[i]])
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(reais, exists, logical(1L), envir = old, inherits = FALSE)))
env$monitora_pendencias_ocorrencia_id <- env$monitora_v307_justificativas_ocorrencia_id
env$monitora_pendencias_justificativas_template <- env$monitora_v307_justificativas_template
env$monitora_pendencias_justificativas_validar_sessao <- env$monitora_v307_justificativas_preflight
# A criação de eventos usa literalmente o corpo real da baseline.
criar_real <- old$monitora_pendencias_justificativas_criar_lote
environment(criar_real) <- env
env$monitora_correcao_hash_texto <- old$monitora_correcao_hash_texto

temporarios <- tempfile("v307_justificativas_"); dir.create(temporarios)
caso_n <- 0L; checks <- 0L
checar <- function(cond) { stopifnot(isTRUE(cond)); checks <<- checks + 1L }
falha <- function(expr, padrao = NULL) {
  erro <- tryCatch({ force(expr); NULL }, error = conditionMessage)
  checar(!is.null(erro) && (is.null(padrao) || grepl(padrao, erro, ignore.case = TRUE)))
  invisible(erro)
}
novo_caso <- function(input_csv = TRUE) {
  caso_n <<- caso_n + 1L
  root <- file.path(temporarios, paste0("caso_", caso_n)); dir.create(root)
  env$MONITORA_INPUT_DIR <- file.path(root, "input"); dir.create(env$MONITORA_INPUT_DIR)
  env$MONITORA_OUTPUT_DIR <- file.path(root, "output"); dir.create(env$MONITORA_OUTPUT_DIR)
  env$MONITORA_CORRECOES_DIR <- file.path(env$MONITORA_OUTPUT_DIR, "correcoes_campos")
  env$MONITORA_EXEC_ID <- paste0("exec_", caso_n)
  env$MONITORA_SCRIPT_VERSAO <- "3.0.7-dev"
  env$MONITORA_LINHAGEM_INPUT_SHA256 <- ""
  env$MONITORA_LINHAGEM_PARENT_REVISION <- ""
  if (exists("MONITORA_V307_JUSTIFICATIVAS_BASE", envir = .GlobalEnv, inherits = FALSE)) rm("MONITORA_V307_JUSTIFICATIVAS_BASE", envir = .GlobalEnv)
  options(monitora.v307.justificativas.avisos = character())
  if (input_csv) writeLines(c("registro,COLETA", "A,C1"), file.path(env$MONITORA_INPUT_DIR, "registros_corrig.csv"), useBytes = TRUE)
  invisible(root)
}
ocorrencias <- function(n = 2L) data.table(
  tipo_ocorrencia = rep(c("forma_vida_exotica_com_especie", "seca_morta_em_revisao"), length.out = n),
  rotulo_ocorrencia = "Condição de revisão", COLETA = paste0("C", seq_len(n)),
  linha_indice = seq_len(n), monitora_row_id = paste0("row_", seq_len(n)),
  ocorrencia_token = "Exótica", ocorrencia_atributo = "espécie <b>\"A\"</b>",
  ocorrencia_categoria = "Herbáceas", ocorrencia_status = "revisao", ocorrencia_detalhe = "detalhe\r\ninterno",
  UC = "UC Teste", EA = "EA1", UA = "UA1", CICLO = "1", CAMPANHA = "2", ANO = "2026", fonte_indice = "fixture"
)
lote <- function(oc, ts = "2026-11-11 12:00:00.000001") {
  z <- copy(oc)
  if (!"ocorrencia_id" %in% names(z)) z[, ocorrencia_id := env$monitora_v307_justificativas_ocorrencia_id(.SD)]
  r <- suppressWarnings(criar_real(z, "Responsável teste", "pendencia_legitima", "Justificativa sintética suficiente para validar a continuidade.", ts))
  if (!isTRUE(r$ok)) stop(r$erro)
  attr(r$dados, "monitora_v307_aliases") <- env$monitora_v307_justificativas_aliases(z)
  r$dados
}
suprimir <- function(expr) suppressWarnings(force(expr))
persistir <- function(oc, sess = NULL) suprimir(env$monitora_v307_justificativas_persistir_preparado(env$monitora_v307_justificativas_preparar(oc, sess)))
exportar <- function(rev, preparado = NULL) {
  dir_lin <- file.path(env$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem")
  base <- file.path(env$MONITORA_INPUT_DIR, "registros_corrig.csv")
  sha <- env$monitora_v307_justificativas_hash(base, arquivo = TRUE)
  campos <- suprimir(env$monitora_v307_justificativas_exportar(dir_lin, sha, rev, preparado))
  jsonlite::write_json(c(list(lineage_schema = "monitora_linhagem_v2", revision_id = rev, output_data_sha256 = sha), campos), file.path(dir_lin, "manifesto_linhagem.json"), pretty = TRUE, auto_unbox = TRUE)
  list(dir_lin = dir_lin, base = base, campos = campos)
}

# Estabilidade de bytes UTF-8, sem mexer no hash global real, e compatibilidade legada.
novo_caso()
oc <- ocorrencias()
id <- env$monitora_v307_justificativas_identidade(oc)
oc_unknown <- copy(oc)
for (cc in names(oc_unknown)) if (is.character(oc_unknown[[cc]])) { v <- oc_unknown[[cc]]; Encoding(v) <- "unknown"; set(oc_unknown, j = cc, value = v) }
checar(identical(id$ocorrencia_id, env$monitora_v307_justificativas_ocorrencia_id(oc_unknown)))
oc_utf8 <- copy(oc)
for (cc in names(oc_utf8)) if (is.character(oc_utf8[[cc]])) { v <- enc2utf8(oc_utf8[[cc]]); Encoding(v) <- "UTF-8"; set(oc_utf8, j = cc, value = v) }
checar(identical(unname(id$alias_utf8), unname(old$monitora_pendencias_ocorrencia_id(oc_utf8))))
checar(identical(unname(id$alias_unknown), unname(old$monitora_pendencias_ocorrencia_id(oc_unknown))))
checar(!identical({z <- enc2utf8("exótica"); Encoding(z) <- "UTF-8"; old$monitora_correcao_hash_texto(z)}, {z <- enc2utf8("exótica"); Encoding(z) <- "unknown"; old$monitora_correcao_hash_texto(z)}))
# Sentinelas CSV de ausências do snapshot legado não são conteúdo diagnóstico.
# O leitor genérico/eventos continua literal; só a projeção de recuperação
# aplica o mesmo na.strings=c("", "NA") da prova autêntica da auditoria.
sentinelas <- ocorrencias()
sentinelas[, `:=`(ocorrencia_categoria = NA_character_, CICLO = NA_character_, CAMPANHA = NA_character_)]
for (cc in names(sentinelas)) if (is.character(sentinelas[[cc]])) { z <- enc2utf8(sentinelas[[cc]]); Encoding(z) <- "unknown"; set(sentinelas, j = cc, value = z) }
sentinelas[, ocorrencia_id := old$monitora_pendencias_ocorrencia_id(.SD)]
sentinelas[, justificativa := "NA"]
sentinel_csv <- file.path(temporarios, "snapshot_legado_sentinelas.csv")
fwrite(sentinelas, sentinel_csv, na = "NA")
sentinel_hash <- env$monitora_v307_justificativas_hash(sentinel_csv, arquivo = TRUE)
literal <- env$monitora_v307_justificativas_csv_ler(sentinel_csv)
falha(env$monitora_v307_justificativas_aliases(literal), "identificador fornecido")
interpretado <- env$monitora_v307_justificativas_snapshot_legado_ler(sentinel_csv)
aa_sentinelas <- env$monitora_v307_justificativas_aliases(interpretado)
checar(all(interpretado$ocorrencia_id %in% aa_sentinelas$alias_ocorrencia_id))
checar(all(is.na(interpretado$ocorrencia_categoria)) && all(interpretado$justificativa == "NA"))
checar(all(literal$ocorrencia_categoria == "NA") && all(literal$justificativa == "NA"))
checar(identical(sentinel_hash, env$monitora_v307_justificativas_hash(sentinel_csv, arquivo = TRUE)))

# Validação real opcional, somente leitura, em caminho explicitamente fornecido.
# Uso: Rscript tests/test_v307_justificativas.R <repo> <snapshot_APAI_em_copia>
if (length(args) >= 2L) {
  snapshot_real <- normalizePath(args[2L], mustWork = TRUE)
  hash_real <- env$monitora_v307_justificativas_hash(snapshot_real, arquivo = TRUE)
  real <- env$monitora_v307_justificativas_snapshot_legado_ler(snapshot_real)
  ids_reais <- env$monitora_v307_justificativas_identidade(real)
  checar(all(real$ocorrencia_id == ids_reais$alias_utf8 | real$ocorrencia_id == ids_reais$alias_unknown))
  for (flag in c("UTF-8", "unknown")) {
    r <- copy(real)
    for (cc in names(r)) if (is.character(r[[cc]])) { z <- enc2utf8(r[[cc]]); Encoding(z) <- flag; set(r, j = cc, value = z) }
    reproduzidos <- old$monitora_pendencias_ocorrencia_id(r)
    checar(identical(unname(reproduzidos), unname(if (flag == "UTF-8") ids_reais$alias_utf8 else ids_reais$alias_unknown)))
  }
  checar(nrow(env$monitora_v307_justificativas_aliases(real)) >= nrow(real))
  checar(identical(hash_real, env$monitora_v307_justificativas_hash(snapshot_real, arquivo = TRUE)))
  cat(sprintf("APAI somente leitura: %d ocorrências, IDs reais compatíveis=%d; reproduções autênticas UTF8/unknown conferidas; bytes fonte preservados.\n", nrow(real), sum(real$ocorrencia_id == ids_reais$alias_utf8 | real$ocorrencia_id == ids_reais$alias_unknown)))
}

# Processo R realmente novo e novo Source mantêm a identidade nova.
fixture <- file.path(temporarios, "oc.rds"); saveRDS(oc, fixture)
runner <- file.path(temporarios, "fresh.R"); saida <- file.path(temporarios, "fresh.rds")
writeLines(c(sprintf("source(%s)", deparse(file.path(repo, "tools/v307/justificativas.R"))), sprintf("saveRDS(monitora_v307_justificativas_ocorrencia_id(readRDS(%s)), %s)", deparse(fixture), deparse(saida))), runner)
checar(system2(file.path(R.home("bin"), "Rscript"), runner, stdout = FALSE, stderr = FALSE) == 0L)
checar(identical(readRDS(saida), id$ocorrencia_id))
sys.source(file.path(repo, "tools/v307/justificativas.R"), envir = env)
checar(identical(env$monitora_v307_justificativas_ocorrencia_id(oc), id$ocorrencia_id))

# Formação agregada não depende da primeira linha física da COLETA.
form <- copy(oc[1]); form[, tipo_ocorrencia := "formacao_vegetacional_inconsistente_ua"]
form2 <- copy(form); form2[, `:=`(monitora_row_id = "outra_linha", linha_indice = 400L)]
checar(identical(env$monitora_v307_justificativas_ocorrencia_id(form), env$monitora_v307_justificativas_ocorrencia_id(form2)))
mudou <- copy(oc); mudou[, COLETA := paste0(COLETA, "_nova")]
checar(!any(env$monitora_v307_justificativas_ocorrencia_id(mudou) %in% id$ocorrencia_id))
ambigua <- copy(oc); ambigua[, CAMPANHA := "outra"]
falha(env$monitora_v307_justificativas_juntar(list(env$monitora_v307_justificativas_aliases(oc), env$monitora_v307_justificativas_aliases(ambigua)), aliases = TRUE), "ambíguo")

# A -> B -> C transporta somente registros_corrig e a pasta linhagem inteira.
sess <- lote(oc)
p <- persistir(oc, sess)
sha_original <- env$monitora_v307_justificativas_hash(p$arquivo_historico, arquivo = TRUE)
est <- suprimir(env$monitora_v307_justificativas_estado(oc))
checar(nrow(est$historico) == 2L && nrow(est$pendencias_a_justificar) == 0L)
checar(all(est$pendencias_remanescentes$status_justificativa == "vigente"))
checar(isTRUE(suprimir(env$monitora_v307_justificativas_preflight(lote(mudou), mudou))$ok))
# Outro evento para ocorrência já vigente é bloqueado antes da publicação.
duplicata <- copy(sess); duplicata[, `:=`(evento_justificativa_id = paste0(evento_justificativa_id, "_dup"), evento_lote_id = paste0(evento_lote_id, "_dup"))]
checar(!suprimir(env$monitora_v307_justificativas_preflight(duplicata, oc))$ok)
assinaturas <- suprimir(env$monitora_v307_justificativas_assinaturas())
ex <- exportar("rev_A")
checar(identical(assinaturas$justificativas_sha256, ex$campos$justificativas_sha256))
checar(length(ex$campos$justificativas_fontes) > 0L && all(vapply(ex$campos$justificativas_fontes,
  function(f) grepl("^justificativas_fontes/[a-f0-9]{24}[.]csv$", f$path) && nchar(f$sha256) == 64L &&
    identical(env$monitora_v307_justificativas_hash(file.path(ex$dir_lin, f$path), arquivo = TRUE), f$sha256), logical(1L))))
# Compatibilidade do carrier antigo de64 caracteres; B/C voltam a exportar24.
carrier_antigo <- file.path(temporarios, "carrier_nome64", "linhagem")
dir.create(carrier_antigo, recursive = TRUE)
invisible(file.copy(list.files(ex$dir_lin, full.names = TRUE), carrier_antigo, recursive = TRUE))
man_antigo <- env$monitora_v307_justificativas_json_ler(file.path(carrier_antigo, "manifesto_linhagem.json"))
for (i in seq_along(man_antigo$justificativas_fontes)) {
  f <- man_antigo$justificativas_fontes[[i]]
  rel64 <- paste0("justificativas_fontes/", f$sha256, ".csv")
  checar(isTRUE(file.rename(file.path(carrier_antigo, f$path), file.path(carrier_antigo, rel64))))
  man_antigo$justificativas_fontes[[i]]$path <- rel64
}
for (nm in c("manifesto_linhagem.json", "manifesto_justificativas.json")) jsonlite::write_json(man_antigo, file.path(carrier_antigo, nm), auto_unbox = TRUE)
checar(nrow(env$monitora_v307_justificativas_validar_importacao(carrier_antigo, man_antigo)) == 2L)
ex$dir_lin <- carrier_antigo
for (revisao in c("rev_B", "rev_C")) {
  ex_anterior <- ex
  novo_caso()
  file.copy(ex_anterior$base, file.path(env$MONITORA_INPUT_DIR, "registros_corrig.csv"), overwrite = TRUE)
  file.copy(ex_anterior$dir_lin, env$MONITORA_INPUT_DIR, recursive = TRUE)
  est <- env$monitora_v307_justificativas_estado(oc)
  checar(nrow(est$historico) == 2L && nrow(est$pendencias_a_justificar) == 0L)
  checar(identical(env$monitora_v307_justificativas_hash(file.path(env$MONITORA_INPUT_DIR, "linhagem", "justificativas_pendencias_consolidada.csv"), arquivo = TRUE), sha_original))
  ex <- exportar(revisao)
  checar(all(vapply(ex$campos$justificativas_fontes, function(f) grepl("/[a-f0-9]{24}[.]csv$", f$path), logical(1L))))
}

# Raiz de120 e de127 caracteres: caminhos novos ficam<=210 (antigos247/248).
for (raiz_n in c(120L, 127L)) for (subdir in c("linhagem", "operacoes")) {
  rel <- file.path("02_painel_correcoes", subdir, "justificativas_fontes", paste0(substr(sha_original, 1L, 24L), ".csv"))
  checar(nchar(file.path(paste(rep("x", raiz_n), collapse = ""), rel)) <= 210L)
}
prefixo <- paste(rep("a", 24L), collapse = "")
colisoes <- list(list(sha256 = paste0(prefixo, paste(rep("b", 40L), collapse = ""))),
  list(sha256 = paste0(prefixo, paste(rep("c", 40L), collapse = ""))))
falha(env$monitora_v307_justificativas_fontes_planejar(colisoes), "Colisão de prefixo")
checar(length(env$monitora_v307_justificativas_fontes_planejar(list(colisoes[[1]], colisoes[[1]]))) == 2L)
# Colisão no destino bloqueia toda publicação antes de alterar qualquer arquivo.
collision_stage <- file.path(temporarios, "prefix_stage"); collision_dest <- file.path(temporarios, "prefix_dest")
dir.create(file.path(collision_stage, "justificativas_fontes"), recursive = TRUE)
dir.create(file.path(collision_dest, "justificativas_fontes"), recursive = TRUE)
writeLines("arquivo novo", file.path(collision_stage, "a.csv")); writeLines("arquivo anterior", file.path(collision_dest, "a.csv"))
archive_stage <- file.path(collision_stage, "justificativas_fontes", paste0(substr(sha_original, 1L, 24L), ".csv"))
archive_dest <- file.path(collision_dest, "justificativas_fontes", basename(archive_stage))
writeLines("fonte nova com mesmo prefixo", archive_stage); writeLines("fonte anterior distinta", archive_dest)
before_collision <- vapply(c(archive_dest, file.path(collision_dest, "a.csv")), env$monitora_v307_justificativas_hash, character(1L), arquivo = TRUE)
falha(env$monitora_v307_justificativas_publicar_diretorio(collision_stage, collision_dest), "Colisão de prefixo")
checar(identical(before_collision, vapply(c(archive_dest, file.path(collision_dest, "a.csv")), env$monitora_v307_justificativas_hash, character(1L), arquivo = TRUE)))
falha(env$monitora_v307_justificativas_fonte_copiar(archive_stage, archive_dest, env$monitora_v307_justificativas_hash(archive_stage, arquivo = TRUE)), "Colisão de prefixo")
checar(identical(before_collision[1L], vapply(archive_dest, env$monitora_v307_justificativas_hash, character(1L), arquivo = TRUE)))

# O contrato público aceita registros_corrig*.csv único, inclusive nome alternativo.
base_canonica <- file.path(env$MONITORA_INPUT_DIR, "registros_corrig.csv")
base_alternativa <- file.path(env$MONITORA_INPUT_DIR, "registros_corrig_APAI_2026.CSV")
checar(isTRUE(file.rename(base_canonica, base_alternativa)))
env$MONITORA_LINHAGEM_INPUT_SHA256 <- env$monitora_v307_justificativas_hash(base_alternativa, arquivo = TRUE)
checar(nrow(env$monitora_v307_justificativas_ler_historico(arquivo_registros = base_alternativa)) == 2L)
checar(nrow(env$monitora_v307_justificativas_estado(oc)$pendencias_a_justificar) == 0L)
base_ambigua <- file.path(env$MONITORA_INPUT_DIR, "registros_corrig_OUTRO.csv")
invisible(file.copy(base_alternativa, base_ambigua))
falha(env$monitora_v307_justificativas_ler_historico(arquivo_registros = base_alternativa), "ambíguo")
unlink(base_ambigua)
falha(env$monitora_v307_justificativas_ler_historico(arquivo_registros = ex$base), "não corresponde")
invisible(file.rename(base_alternativa, base_canonica))
env$MONITORA_LINHAGEM_INPUT_SHA256 <- ""

# Diagnóstico parcial/família omitida não transforma ausência técnica em resolução.
parcial <- oc[1L]
prep <- env$monitora_v307_justificativas_preparar(parcial)
checar(!any(prep$historico$status_evento == "encerrada_por_resolucao"))
attr(parcial, "monitora_diagnostico_completo") <- TRUE
attr(parcial, "monitora_familias_avaliadas") <- oc$tipo_ocorrencia[1L]
prep <- env$monitora_v307_justificativas_preparar(parcial)
checar(!any(prep$historico$status_evento == "encerrada_por_resolucao"))
attr(parcial, "monitora_familias_avaliadas") <- unique(oc$tipo_ocorrencia)
prep <- env$monitora_v307_justificativas_preparar(parcial)
checar(sum(prep$historico$status_evento == "encerrada_por_resolucao") == 1L)
checar(identical(prep$historico[status_evento == "vigente", evento_justificativa_id], est$historico$evento_justificativa_id))
env$monitora_v307_justificativas_persistir_preparado(prep)
prep2 <- env$monitora_v307_justificativas_preparar(parcial)
checar(sum(prep2$historico$status_evento == "encerrada_por_resolucao") == 1L)
reapareceu <- env$monitora_v307_justificativas_estado(oc)
checar(reapareceu$pendencias_remanescentes[COLETA == "C2", status_justificativa] == "sem_justificativa")
checar(nrow(env$monitora_v307_justificativas_estado(mudou)$pendencias_a_justificar) == 2L)

# Mesma saída, novo evento: canônico acumula e conserva fontes históricas físicas.
nova <- ocorrencias(3L)
nova[, COLETA := paste0(COLETA, "_extra")]
todos <- rbind(oc, nova)
novos <- lote(nova, "2026-11-11 13:00:00.000001")
p2 <- persistir(todos, novos)
checar(nrow(p2$historico) == 6L)
checar(file.exists(file.path(dirname(p2$arquivo_historico), "justificativas_fontes", paste0(substr(sha_original, 1L, 24L), ".csv"))))
checar(nrow(env$monitora_v307_justificativas_estado(todos)$pendencias_a_justificar) == 1L)

# Hash, contagem, schema, CSV truncado e conteúdo conflitante falham explicitamente.
man <- env$monitora_v307_justificativas_json_ler(file.path(ex$dir_lin, "manifesto_linhagem.json"))
man_count <- man; man_count$justificativas_event_count <- 99L
falha(env$monitora_v307_justificativas_validar_importacao(ex$dir_lin, man_count), "Contagem")
man_schema <- man; man_schema$justificativas_schema <- "schema_desconhecido"
falha(env$monitora_v307_justificativas_validar_importacao(ex$dir_lin, man_schema), "Schema")
man_base <- man; man_base$justificativas_data_sha256 <- paste(rep("e", 64L), collapse = "")
falha(env$monitora_v307_justificativas_validar_importacao(ex$dir_lin, man_base), "base/revisão")
man_hash <- man; man_hash$justificativas_sha256 <- paste(rep("f", 64L), collapse = "")
falha(env$monitora_v307_justificativas_validar_importacao(ex$dir_lin, man_hash), "Hash")
conflito <- copy(sess); conflito[1L, justificativa := "Outro conteúdo conflitante para o mesmo evento."]
falha(env$monitora_v307_justificativas_juntar(list(sess, conflito)), "conflitante")
csv_ruim <- file.path(temporarios, "truncado.csv"); writeLines(c("a,b", "\"texto não terminado,b"), csv_ruim)
falha(env$monitora_v307_justificativas_csv_ler(csv_ruim), "Falha explícita")
falha(env$monitora_v307_justificativas_eventos_validar(data.table(evento_justificativa_id = "x")), "Schema")

# Legado ausente alerta uma única vez; input raiz nunca é dado/histórico aceito.
novo_caso()
avisos <- 0L
for (i in 1:3) withCallingHandlers(env$monitora_v307_justificativas_ler_historico(), warning = function(w) { avisos <<- avisos + 1L; invokeRestart("muffleWarning") })
checar(avisos == 1L)
env$monitora_v307_justificativas_csv_escrever(sess, file.path(env$MONITORA_INPUT_DIR, "justificativas_pendencias_consolidada.csv"))
falha(env$monitora_v307_justificativas_ler_historico(), "input raiz")

# Recuperação legada é explícita, pareada por campos completos, preserva bytes.
novo_caso()
legado_oc <- copy(oc); legado_oc[, ocorrencia_id := id$alias_utf8]
legado <- lote(legado_oc)
fonte_dir <- file.path(temporarios, "fonte_legada"); dir.create(fonte_dir)
hist_legado <- file.path(fonte_dir, "historico.csv"); snap_legado <- file.path(fonte_dir, "snapshot.csv")
env$monitora_v307_justificativas_csv_escrever(legado, hist_legado)
legado_oc[, status_justificativa := "vigente"]
env$monitora_v307_justificativas_csv_escrever(legado_oc, snap_legado)
base <- file.path(env$MONITORA_INPUT_DIR, "registros_corrig.csv")
sha <- env$monitora_v307_justificativas_hash(base, arquivo = TRUE)
legacy_man <- list(lineage_schema = "monitora_linhagem_v2", revision_id = "rev_legada", output_data_sha256 = sha)
man_fonte <- file.path(fonte_dir, "manifesto_linhagem.json"); jsonlite::write_json(legacy_man, man_fonte, auto_unbox = TRUE)
lin <- file.path(env$MONITORA_INPUT_DIR, "linhagem"); dir.create(lin)
file.copy(man_fonte, file.path(lin, basename(man_fonte)))
env$monitora_v307_justificativas_recuperar(hist_legado, snap_legado, base, man_fonte, lin)
checar(identical(env$monitora_v307_justificativas_hash(hist_legado, arquivo = TRUE), env$monitora_v307_justificativas_hash(file.path(lin, "justificativas_pendencias_consolidada.csv"), arquivo = TRUE)))
checar(identical(env$monitora_v307_justificativas_estado(oc_unknown)$historico$ocorrencia_id, legado$ocorrencia_id))
checar(nrow(env$monitora_v307_justificativas_estado(oc_unknown)$pendencias_a_justificar) == 0L)
falha(env$monitora_v307_justificativas_recuperar(hist_legado, snap_legado, base, man_fonte, lin), "já contém")
ex_rec <- exportar("rev_recuperada")
checar(file.exists(file.path(ex_rec$dir_lin, "manifesto_recuperacao_justificativas.json")))
checar(file.exists(file.path(ex_rec$dir_lin, "justificativas_snapshot_recuperacao.csv")))
invisible(env$monitora_v307_justificativas_validar_importacao(ex_rec$dir_lin, env$monitora_v307_justificativas_json_ler(file.path(ex_rec$dir_lin, "manifesto_linhagem.json"))))

# Bruto sem checkpoint não toma CSV obsoleto do output como vínculo.
novo_caso(input_csv = FALSE)
obsoleto <- file.path(env$MONITORA_OUTPUT_DIR, "01_produtos_dados", "registros_corrig.csv")
dir.create(dirname(obsoleto), recursive = TRUE); writeLines("CSV residual de outra base", obsoleto)
sess <- lote(oc)
prep <- env$monitora_v307_justificativas_preparar(oc, sess)
falha(env$monitora_v307_justificativas_persistir_preparado(prep), "registre explicitamente")
env$monitora_v307_justificativas_registrar_base(data.table(valor = "base bruta atual"), origem = "teste_bruto")
env$monitora_v307_justificativas_persistir_preparado(prep)
checar(nrow(env$monitora_v307_justificativas_estado(oc)$pendencias_a_justificar) == 0L)
# A transformação pós-painel, explicitamente declarada, mantém a prova anterior.
env$monitora_v307_justificativas_registrar_base(data.table(valor = "base corrigida pós-painel"), origem = "pos_painel", continuar = TRUE)
checar(nrow(env$monitora_v307_justificativas_estado(oc)$pendencias_a_justificar) == 0L)
env$monitora_v307_justificativas_persistir_preparado(env$monitora_v307_justificativas_preparar(oc))
checar(nrow(env$monitora_v307_justificativas_estado(oc)$historico) == 2L)
env$monitora_v307_justificativas_registrar_base(data.table(valor = "OUTRA base bruta"), origem = "teste_outra_base")
falha(env$monitora_v307_justificativas_estado(oc), "checkpoint registros_corrig")

# Cache bruto em outra execução: igualdade de memória sozinha não autoriza.
# O marcador é emitido pelo validador de cache, nunca pela mera presença do RDS.
sys.source(file.path(repo, "tools/v307/cache.R"), envir = env)
novo_caso(input_csv = FALSE)
bruto_cache <- file.path(env$MONITORA_INPUT_DIR, "bruto.csv"); writeLines(c("registro", "base_cache"), bruto_cache)
env$MONITORA_VALIDACAO_ESPACIAL_VERSAO_MODULO <- "fixture_cache_v1"
env$MONITORA_VALIDAR_ESPACIAL_COLETAS <- FALSE
env$MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 150
base_cache <- data.table(valor = "base confirmada do cache")
env$monitora_v307_justificativas_registrar_base(base_cache, origem = "painel")
persistir(oc, lote(oc))
op_cache <- file.path(env$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "operacoes", "justificativas_pendencias_consolidada.csv")
op_hash <- env$monitora_v307_justificativas_hash(op_cache, arquivo = TRUE)
prova_cache <- list(memoria_sha256 = env$monitora_v307_justificativas_memoria_sha256(base_cache),
  input_sha256 = env$monitora_v307_cache_input_sha256(), config_sha256 = env$monitora_v307_cache_configuracao_sha256())
env$MONITORA_EXEC_ID <- paste0(env$MONITORA_EXEC_ID, "_novo_source")
env$MONITORA_V307_JUSTIFICATIVAS_BASE <- NULL; env$MONITORA_V307_CACHE_VALIDADO <- NULL
env$monitora_v307_justificativas_registrar_base(base_cache, origem = "painel")
falha(env$monitora_v307_justificativas_estado(oc), "checkpoint registros_corrig")
falha(do.call(env$monitora_v307_justificativas_registrar_cache_validado, prova_cache), "cache validado")
do.call(env$monitora_v307_justificativas_registrar_cache_validado, c(prova_cache, list(validado = TRUE)))
checar(nrow(env$monitora_v307_justificativas_estado(oc)$pendencias_a_justificar) == 0L)
env$MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 240
falha(env$monitora_v307_justificativas_estado(oc), "checkpoint registros_corrig")
env$MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 150
writeLines("input mudou", bruto_cache)
falha(env$monitora_v307_justificativas_estado(oc), "checkpoint registros_corrig")
writeLines(c("registro", "base_cache"), bruto_cache)
# A publicação após uma prévia com correções confirma a memória NOVA.
env$monitora_v307_justificativas_registrar_base(data.table(valor = "base corrigida na prévia"), origem = "preview_confirmado", continuar = TRUE)
persistir(oc)
env$MONITORA_EXEC_ID <- paste0(env$MONITORA_EXEC_ID, "_outro_source")
env$MONITORA_V307_JUSTIFICATIVAS_BASE <- NULL; env$MONITORA_V307_CACHE_VALIDADO <- NULL
env$monitora_v307_justificativas_registrar_base(base_cache, origem = "painel")
do.call(env$monitora_v307_justificativas_registrar_cache_validado, c(prova_cache, list(validado = TRUE)))
falha(env$monitora_v307_justificativas_estado(oc), "checkpoint registros_corrig")
checar(identical(op_hash, env$monitora_v307_justificativas_hash(op_cache, arquivo = TRUE)))

# Replay v307: mesmos brutos físicos + ledger físico validado, nunca só hash declarado.
novo_caso(input_csv = FALSE)
bruto <- file.path(env$MONITORA_INPUT_DIR, "bruto.csv"); writeLines(c("registro,COLETA", "A,C1"), bruto)
env$monitora_v307_justificativas_registrar_base(data.table(valor = "bruto para replay"))
persistir(oc, lote(oc))
lin_saida <- file.path(env$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "linhagem"); dir.create(lin_saida)
ledger_saida <- file.path(lin_saida, "correcoes_semanticas_consolidada.csv"); writeLines(c("event_id", "evento_correto"), ledger_saida)
sha_dados <- env$monitora_v307_justificativas_hash(bruto, arquivo = TRUE)
campos_replay <- env$monitora_v307_justificativas_exportar(lin_saida, sha_dados, "rev_replay")
checar(identical(campos_replay$justificativas_replay_schema, "monitora_justificativas_replay_v1"))
checar(identical(campos_replay$justificativas_replay_ledger_sha256, env$monitora_v307_justificativas_hash(ledger_saida, arquivo = TRUE)))
man_replay <- c(list(lineage_schema = "monitora_linhagem_v2", revision_id = "rev_replay", output_data_sha256 = sha_dados), campos_replay)
jsonlite::write_json(man_replay, file.path(lin_saida, "manifesto_linhagem.json"), auto_unbox = TRUE)
bruto_original <- bruto; linhagem_original <- lin_saida
novo_caso(input_csv = FALSE)
invisible(file.copy(bruto_original, file.path(env$MONITORA_INPUT_DIR, "bruto.csv")))
invisible(file.copy(linhagem_original, env$MONITORA_INPUT_DIR, recursive = TRUE))
ledger_replay <- file.path(env$MONITORA_INPUT_DIR, "linhagem", "correcoes_semanticas_consolidada.csv")
falha(env$monitora_v307_justificativas_estado(oc), "prova física")
env$MONITORA_REAPLICAR_CORRECOES_ANTERIORES <- TRUE
falha(env$monitora_v307_justificativas_registrar_replay(ledger_replay), "semanticamente validado")
env$monitora_v307_justificativas_registrar_replay(ledger_replay, validado = TRUE)
checar(nrow(env$monitora_v307_justificativas_estado(oc)$pendencias_a_justificar) == 0L)
bruto_replay <- file.path(env$MONITORA_INPUT_DIR, "bruto.csv")
writeLines("bruto alterado", bruto_replay)
falha(env$monitora_v307_justificativas_estado(oc), "Prova de replay")
invisible(file.copy(bruto_original, bruto_replay, overwrite = TRUE))
write("ledger alterado", file = ledger_replay, append = TRUE)
falha(env$monitora_v307_justificativas_estado(oc), "Prova de replay")
invisible(file.copy(file.path(linhagem_original, "correcoes_semanticas_consolidada.csv"), ledger_replay, overwrite = TRUE))
legado_sem_prova <- man_replay[!grepl("^justificativas_(replay|brutos)", names(man_replay))]
jsonlite::write_json(legado_sem_prova, file.path(env$MONITORA_INPUT_DIR, "linhagem", "manifesto_linhagem.json"), auto_unbox = TRUE)
falha(env$monitora_v307_justificativas_estado(oc), "legado requer recuperação")
env$MONITORA_REAPLICAR_CORRECOES_ANTERIORES <- FALSE

# Rollback local: falha na segunda cópia restaura os dois destinos existentes.
stage <- file.path(temporarios, "rollback_stage"); dest <- file.path(temporarios, "rollback_dest")
dir.create(stage); dir.create(dest)
for (nm in c("a.csv", "b.csv")) { writeLines(paste0("novo_", nm), file.path(stage, nm)); writeLines(paste0("anterior_", nm), file.path(dest, nm)) }
hashes_antes <- vapply(file.path(dest, c("a.csv", "b.csv")), env$monitora_v307_justificativas_hash, character(1L), arquivo = TRUE)
copiar_real <- env$monitora_v307_justificativas_copiar
injetar <- TRUE
env$monitora_v307_justificativas_copiar <- function(origem, destino, sha = NULL) {
  if (isTRUE(injetar) && identical(destino, file.path(dest, "b.csv"))) { injetar <<- FALSE; writeLines("parcial", destino); stop("falha injetada na segunda publicação") }
  copiar_real(origem, destino, sha)
}
falha(env$monitora_v307_justificativas_publicar_diretorio(stage, dest), "injetada")
env$monitora_v307_justificativas_copiar <- copiar_real
checar(identical(hashes_antes, vapply(file.path(dest, c("a.csv", "b.csv")), env$monitora_v307_justificativas_hash, character(1L), arquivo = TRUE)))

# Lote de sessão parcialmente resolvido: eventos originais não são reescritos.
novo_caso()
oc3 <- ocorrencias(3L); sess3 <- lote(oc3)
atual3 <- oc3[-1L]; attr(atual3, "monitora_diagnostico_completo") <- TRUE; attr(atual3, "monitora_familias_avaliadas") <- "*"
rec3 <- env$monitora_v307_justificativas_reconciliar_sessao(sess3, atual3, "2026-11-11 14:00:00.000001")
checar(rec3$n_ativas == 2L && rec3$n_encerradas == 1L)
checar(isTRUE(suprimir(env$monitora_v307_justificativas_preflight(rec3$ativas, atual3))$ok))
pre3 <- suprimir(env$monitora_v307_justificativas_preparar(atual3, rec3$ativas, rec3$eventos_encerrados))
checar(nrow(pre3$historico) == 4L && all(sess3$evento_justificativa_id %in% pre3$historico$evento_justificativa_id))
rec3_repetida <- env$monitora_v307_justificativas_reconciliar_sessao(rec3$ativas, atual3, "2026-11-11 14:01:00.000001")
checar(rec3_repetida$n_encerradas == 0L && isTRUE(suprimir(env$monitora_v307_justificativas_preflight(rec3_repetida$ativas, atual3))$ok))
normalizada <- old$monitora_pendencias_justificativas_normalizar_sessao(rec3_repetida$ativas)
checar(identical(attr(normalizada, "monitora_v307_complemento_lotes"), attr(rec3$ativas, "monitora_v307_complemento_lotes")))
# Corpo real de reconstituir com a única adaptação de retorno usada na candidata.
reconstituir_real <- old$monitora_pendencias_justificativas_reconstituir_lotes
old$monitora_pendencias_justificativas_reconstituir_lotes <- function(x, lotes_afetados) env$monitora_v307_justificativas_preservar_atributos(reconstituir_real(x, lotes_afetados), x)
remover <- suprimir(old$monitora_pendencias_justificativas_excluir_atomico(normalizada, normalizada$evento_justificativa_id[1L], atual3))
checar(isTRUE(remover$ok) && nrow(remover$dados) == 1L)
checar(identical(attr(remover$dados, "monitora_v307_complemento_lotes"), attr(rec3$ativas, "monitora_v307_complemento_lotes")))
oc_nova <- ocorrencias(1L); oc_nova[, COLETA := "NOVA_COLETA"]
atual_mais <- rbind(atual3, oc_nova); attr(atual_mais, "monitora_diagnostico_completo") <- TRUE; attr(atual_mais, "monitora_familias_avaliadas") <- "*"
adicionar <- suprimir(env$monitora_v307_justificativas_adicionar_lote_atomico(remover$dados, lote(oc_nova, "2026-11-11 14:02:00.000001"), atual_mais))
checar(isTRUE(adicionar$ok) && nrow(adicionar$dados) == 2L)
checkpoint <- old$monitora_pendencias_justificativas_checkpoint_recuperavel(adicionar$dados, rec3$eventos_encerrados,
  auditoria_reconciliacao = rec3$auditoria, fingerprint_base = old$monitora_painel_checkpoint_fingerprint_base(atual_mais))
rec_cp <- readRDS(checkpoint)
checar(identical(rec_cp$filas_sha256, old$monitora_painel_checkpoint_assinatura_filas(rec_cp$correcoes_sessao, rec_cp$correcoes_solicitadas,
  rec_cp$correcoes_historico_intencoes, rec_cp$correcoes_espaciais, rec_cp$justificativas_ativas, rec_cp$justificativas_encerradas)))
rec_ativas <- old$normalizar_fila_checkpoint(rec_cp$justificativas_ativas, env$monitora_v307_justificativas_template())
rec_encerradas <- old$normalizar_fila_checkpoint(rec_cp$justificativas_encerradas, env$monitora_v307_justificativas_template())
checar(isTRUE(all.equal(attr(rec_ativas, "monitora_v307_aliases"), attr(adicionar$dados, "monitora_v307_aliases"), check.attributes = FALSE)))
checar(isTRUE(all.equal(attr(rec_ativas, "monitora_v307_complemento_lotes"), attr(adicionar$dados, "monitora_v307_complemento_lotes"), check.attributes = FALSE)))
pre_cp <- suprimir(env$monitora_v307_justificativas_preparar(atual_mais, rec_ativas, rec_encerradas))
checar(all(rec3$eventos_encerrados$evento_justificativa_id %in% pre_cp$historico$evento_justificativa_id))
adulterado <- copy(rec_ativas); attr(adulterado, "monitora_v307_complemento_lotes") <- NULL
checar(!identical(rec_cp$filas_sha256, old$monitora_painel_checkpoint_assinatura_filas(rec_cp$correcoes_sessao, rec_cp$correcoes_solicitadas,
  rec_cp$correcoes_historico_intencoes, rec_cp$correcoes_espaciais, adulterado, rec_cp$justificativas_encerradas)))

# Justificar mantém diagnóstico/flag impeditiva e não altera dados biológicos.
novo_caso()
imp <- ocorrencias(1L); imp[, `:=`(tipo_ocorrencia = "pendencia_espacial__coordenada_invalida_ou_ausente", impeditiva = TRUE)]
antes <- copy(imp); persistir(imp, lote(imp))
imp_est <- suprimir(env$monitora_v307_justificativas_estado(imp))$pendencias_remanescentes
checar(nrow(imp_est) == 1L && isTRUE(imp_est$impeditiva) && identical(imp, antes))

cat(sprintf("OK v3.0.7 justificativas: %d verificações; UTF8/unknown, processo novo, A->B->C, histórico imutável, recuperação, conflitos, diagnóstico parcial e bruto sem checkpoint.\n", checks))
unlink(temporarios, recursive = TRUE)
