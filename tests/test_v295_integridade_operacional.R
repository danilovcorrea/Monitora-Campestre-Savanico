#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(
  if (length(args)) args[[1L]] else
    "monitora_campsav_alvo_global_v2.9.5.R",
  mustWork = TRUE
)

arvore <- parse(file = script, keep.source = FALSE)
env <- new.env(parent = globalenv())
alvos <- c(
  "monitora_diag_seca_morta_gravar_relatorio_operacional",
  "monitora_relatorio_exoticas_normalizar_token",
  "monitora_ponte_pre_painel_regras_contrato_unico",
  "monitora_pendencias_justificativas_template",
  "monitora_pendencias_justificativas_normalizar_sessao",
  "monitora_pendencias_justificativas_validar_sessao",
  "monitora_pendencias_justificativas_criar_lote",
  "monitora_pendencias_justificativas_adicionar_lote_atomico",
  "monitora_pendencias_justificativas_reconstituir_lotes",
  "monitora_pendencias_justificativas_excluir_atomico",
  "monitora_pendencias_justificativas_publicar_par_atomico",
  "monitora_arquivo_hash_transacao",
  "monitora_arquivo_retentativas",
  "monitora_arquivo_publicar_candidato"
)
coletar <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    nome <- as.character(x[[2L]])
    if (nome %in% alvos) eval(x, env)
    return(invisible(NULL))
  }
  for (ii in seq_along(x)[-1L]) coletar(x[[ii]])
  invisible(NULL)
}
invisible(lapply(as.list(arvore), coletar))
stopifnot(all(vapply(alvos, exists, logical(1L), envir = env, inherits = FALSE)))

env$monitora_diag_rel_write_dt <- function(x, arquivo) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), arquivo, bom = TRUE)
  invisible(arquivo)
}
env$monitora_correcao_normalizar_nome_coluna <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(gsub("(^_+|_+$)", "", gsub("[^[:alnum:]]+", "_", x)))
}
env$monitora_correcao_colunas_chave <- function(dt) list(
  uc = "UC", ea = "EA", ua = "UA", ano = "ANO", coleta = "COLETA"
)
env$monitora_contrato_unico_resolver_contexto_impactos <- function(dt, ...) {
  cols <- c(
    pai = "impact_manejo_uso", tipos = "tipos_impacto_manejo_uso",
    outro = "tipos_impacto_manejo_uso_outro",
    descricao = "tipos_impacto_manejo_uso_descricao"
  )
  data.table(
    papel_contexto = names(cols),
    caminho_registro = unname(cols),
    coluna = data.table::fifelse(unname(cols) %in% names(dt), unname(cols), NA_character_),
    status_resolucao = data.table::fifelse(
      unname(cols) %in% names(dt), "resolvido_unico", "ausente_no_dataset"
    )
  )
}
env$monitora_correcao_xlsform_meta_atual <- function(...) list(opcoes = data.table(
  list_name = c(
    "forma_vida_seca_morta", "forma_vida_seca_morta",
    "tipos_impacto_manejo_uso", "tipos_impacto_manejo_uso"
  ),
  name = c("graminoide", "arbusto_abaixo", "incendio", "erosao"),
  label = c("Graminoide", "Arbusto abaixo de 2 m", "Incêndio", "Erosão")
))
env$monitora_correcao_hash_texto <- function(x) {
  vapply(as.character(x), digest::digest, character(1L), algo = "sha256", serialize = FALSE)
}
env$monitora_fwrite <- function(x, arquivo, ...) fwrite(as.data.table(x), arquivo, na = "")

fixture <- data.table(
  UC = "UC Teste", EA = "EA1",
  UA = rep(c("UA1", "UA2"), each = 6L),
  ANO = rep(rep(c("2025", "2026"), each = 3L), 2L),
  COLETA = rep(c("C1", "C2", "C3", "C4"), each = 3L),
  form_veg = "savanica",
  impact_manejo_uso = rep(c("sim", "nao", "sim", ""), each = 3L),
  tipos_impacto_manejo_uso = rep(c("incendio", "", "erosao", ""), each = 3L),
  tipos_impacto_manejo_uso_outro = "",
  tipos_impacto_manejo_uso_descricao = rep(c("queima observada", "", "erosão", ""), each = 3L)
)
ocorrencias <- data.table(
  linha_indice = c(1L, 2L, 4L, 7L, 10L),
  forma_de_vida_detectada = c(
    "graminoide", "arbusto_abaixo", "arbusto_abaixo", "arbusto_abaixo", "graminoide"
  )
)
dir_saida <- tempfile("v295_seca_morta_")
dir.create(dir_saida)
resultado <- env$monitora_diag_seca_morta_gravar_relatorio_operacional(
  fixture, ocorrencias, dir_saida, "apos_painel"
)

stopifnot(
  nrow(resultado$operacional) == 4L,
  nrow(resultado$por_ano) == 2L,
  nrow(resultado$por_ua) == 4L,
  nrow(resultado$por_forma) >= 3L,
  nrow(resultado$trajetorias) == 8L,
  resultado$operacional[COLETA == "C1", n_pontos_seca_morta] == 2L,
  resultado$operacional[COLETA == "C1", n_eventos_linha_forma] == 2L,
  resultado$operacional[COLETA == "C1", contexto_fogo] ==
    "fogo_explicito_em_campo_estruturado",
  resultado$operacional[COLETA == "C3", contexto_fogo] ==
    "outro_impacto_ou_manejo_sem_fogo_explicito",
  resultado$operacional[COLETA == "C4", contexto_fogo] ==
    "sem_contexto_informado",
  resultado$operacional[COLETA == "C2", trajetoria_lenhosa] ==
    "ocorrencia_recorrente",
  resultado$operacional[COLETA == "C4", trajetoria_herbacea] ==
    "nova_ocorrencia_na_serie",
  all(c("classificacao_triagem", "criterio_principal", "criterios_atendidos") %in% names(resultado$operacional)),
  all(c("coletas_amostradas", "coletas_com_ocorrencia") %in% names(resultado$por_ano)),
  all(c("coletas_amostradas", "coletas_com_ocorrencia", "coletas_com_ocorrencia_herbacea", "coletas_com_ocorrencia_lenhosa") %in% names(resultado$por_ua)),
  "coletas_com_ocorrencia" %in% names(resultado$por_forma),
  all(c(
    "coletas_amostradas_ano_atual", "coletas_com_ocorrencia_ano_atual",
    "coletas_amostradas_ano_anterior", "coletas_com_ocorrencia_ano_anterior"
  ) %in% names(resultado$trajetorias)),
  resultado$trajetorias[UA == "UA1" & ANO == "2026" & grupo_estrutural == "lenhosa", coletas_amostradas_ano_atual] == "C2",
  resultado$trajetorias[UA == "UA1" & ANO == "2026" & grupo_estrutural == "lenhosa", coletas_com_ocorrencia_ano_anterior] == "C1"
)

oc_just <- data.table(
  ocorrencia_id = paste0("occ_", 1:3),
  tipo_ocorrencia = c("seca_morta", "seca_morta", "pendencia_espacial"),
  COLETA = c("C1", "C2", "C3"), UC = "UC Teste", EA = "EA1",
  UA = c("UA1", "UA1", "UA2"), ANO = c("2025", "2026", "2026")
)
lote_atomico <- env$monitora_pendencias_justificativas_criar_lote(
  oc_just, "Responsável teste", "documentacao_insuficiente",
  "Documentação disponível insuficiente para resolver a pendência.",
  "2026-08-11 10:00:00.000000"
)
stopifnot(
  isTRUE(lote_atomico$ok), nrow(lote_atomico$dados) == 3L,
  data.table::uniqueN(lote_atomico$dados$evento_lote_id) == 1L,
  all(lote_atomico$dados$n_ocorrencias_lote == 3L)
)
adicao_atomica <- env$monitora_pendencias_justificativas_adicionar_lote_atomico(
  env$monitora_pendencias_justificativas_template(), lote_atomico$dados, oc_just
)
stopifnot(isTRUE(adicao_atomica$ok), nrow(adicao_atomica$dados) == 3L)
adicao_duplicada <- env$monitora_pendencias_justificativas_adicionar_lote_atomico(
  adicao_atomica$dados, lote_atomico$dados[1L], oc_just
)
stopifnot(!isTRUE(adicao_duplicada$ok), identical(adicao_duplicada$dados, adicao_atomica$dados))
exclusao_parcial <- env$monitora_pendencias_justificativas_excluir_atomico(
  adicao_atomica$dados, adicao_atomica$dados$evento_justificativa_id[1L], oc_just
)
stopifnot(
  isTRUE(exclusao_parcial$ok), exclusao_parcial$n_excluidas == 1L,
  nrow(exclusao_parcial$dados) == 2L,
  identical(sort(as.integer(exclusao_parcial$dados$ordem_no_lote)), 1:2),
  all(as.integer(exclusao_parcial$dados$n_ocorrencias_lote) == 2L),
  data.table::uniqueN(exclusao_parcial$dados$evento_lote_id) == 1L
)
evento_legado <- data.table(
  evento_justificativa_id = "jst_legado_1", ocorrencia_id = "occ_1",
  status_evento = "vigente", timestamp_evento = "2026-08-10 10:00:00.000000",
  exec_id = "exec_legado", script_versao = "2.9.2",
  responsavel = "Responsável legado", tipo_justificativa = "outro",
  justificativa = "Justificativa legada suficientemente detalhada para migração.",
  tipo_ocorrencia = "seca_morta", COLETA = "C1", UC = "UC Teste",
  EA = "EA1", UA = "UA1", ANO = "2025", evento_origem_id = NA_character_
)
evento_legado_migrado <- env$monitora_pendencias_justificativas_validar_sessao(
  evento_legado, oc_just
)
stopifnot(
  isTRUE(evento_legado_migrado$ok),
  grepl("^jlt_legado_", evento_legado_migrado$dados$evento_lote_id),
  evento_legado_migrado$dados$ordem_no_lote == 1L,
  evento_legado_migrado$dados$n_ocorrencias_lote == 1L
)
oc_just_404 <- data.table(
  ocorrencia_id = sprintf("occ_perf_%04d", 1:404),
  tipo_ocorrencia = "seca_morta_em_revisao",
  COLETA = sprintf("C%04d", 1:404), UC = "UC Teste", EA = "EA1",
  UA = sprintf("UA%04d", 1:404), ANO = "2026"
)
tempo_lote_404 <- system.time({
  lote_404 <- env$monitora_pendencias_justificativas_criar_lote(
    oc_just_404, "Responsável teste", "pendencia_legitima",
    "Ocorrências revisadas em lote com documentação de campo equivalente.",
    "2026-08-11 11:00:00.000000"
  )
  adicao_404 <- env$monitora_pendencias_justificativas_adicionar_lote_atomico(
    env$monitora_pendencias_justificativas_template(), lote_404$dados, oc_just_404
  )
})[["elapsed"]]
tempo_exclusao_404 <- system.time({
  exclusao_404 <- env$monitora_pendencias_justificativas_excluir_atomico(
    adicao_404$dados, adicao_404$dados$evento_justificativa_id, oc_just_404
  )
})[["elapsed"]]
stopifnot(
  isTRUE(lote_404$ok), isTRUE(adicao_404$ok), nrow(adicao_404$dados) == 404L,
  isTRUE(exclusao_404$ok), nrow(exclusao_404$dados) == 0L,
  tempo_lote_404 < 2, tempo_exclusao_404 < 2
)
estado_invalido <- data.table::copy(adicao_atomica$dados)
estado_invalido[1L, ocorrencia_id := "occ_inexistente"]
validacao_invalida <- env$monitora_pendencias_justificativas_validar_sessao(
  estado_invalido, oc_just
)
stopifnot(!isTRUE(validacao_invalida$ok))

dir_atomico <- tempfile("v295_justificativas_atomicas_")
dir.create(dir_atomico)
arq_hist_atomico <- file.path(dir_atomico, "historico.csv")
arq_snap_atomico <- file.path(dir_atomico, "snapshot.csv")
fwrite(data.table(estado = "anterior"), arq_hist_atomico)
fwrite(data.table(estado = "anterior"), arq_snap_atomico)
snapshot_teste <- data.table(ocorrencia_id = oc_just$ocorrencia_id, status_justificativa = "vigente")
env$monitora_pendencias_justificativas_publicar_par_atomico(
  adicao_atomica$dados, snapshot_teste, arq_hist_atomico, arq_snap_atomico
)
stopifnot(
  nrow(fread(arq_hist_atomico)) == 3L,
  nrow(fread(arq_snap_atomico)) == 3L
)
fwrite(data.table(estado = "anterior_historico"), arq_hist_atomico)
fwrite(data.table(estado = "anterior_snapshot"), arq_snap_atomico)
contador_rename <- 0L
env$file.rename <- function(from, to) {
  contador_rename <<- contador_rename + 1L
  ### A implementação atual faz retentativas curtas no Windows; falhar toda a
  ### janela da segunda publicação (e não apenas uma chamada) força o rollback.
  if (contador_rename >= 4L && contador_rename <= 9L) return(FALSE)
  base::file.rename(from, to)
}
falha_publicacao <- tryCatch({
  env$monitora_pendencias_justificativas_publicar_par_atomico(
    adicao_atomica$dados, snapshot_teste, arq_hist_atomico, arq_snap_atomico
  )
  NULL
}, error = identity)
stopifnot(
  inherits(falha_publicacao, "error"),
  fread(arq_hist_atomico)$estado[[1L]] == "anterior_historico",
  fread(arq_snap_atomico)$estado[[1L]] == "anterior_snapshot"
)
rm("file.rename", envir = env)

arquivos_esperados <- file.path(dir_saida, paste0(c(
  "relatorio_operacional_seca_morta_",
  "resumo_seca_morta_por_ano_",
  "resumo_seca_morta_por_ua_ano_",
  "resumo_seca_morta_por_forma_vida_",
  "trajetorias_seca_morta_por_ua_",
  "metadados_relatorio_operacional_seca_morta_"
), "apos_painel.csv"))
stopifnot(all(file.exists(arquivos_esperados)), all(file.info(arquivos_esperados)$size > 0L))

env$monitora_validacao_regras_por_perfil_contrato_unico <- function(...) data.table(
  regra = "pendencia_impeditiva_teste",
  fonte_operacional_atual = "monitora_diag_rel_gerar_ocorrencias"
)
env$monitora_diag_rel_catalogo_ocorrencias_base <- function() data.table(
  tipo_ocorrencia = c("pendencia_impeditiva_teste", "diagnostico_revisao_teste"),
  severidade = c("impeditiva", "revisao")
)
env$monitora_fwrite <- function(x, arquivo, ...) fwrite(as.data.table(x), arquivo, bom = TRUE)
env$monitora_correcao_console_msg <- function(...) invisible(NULL)
eventos_ponte <- data.table()
env$monitora_log_registrar_evento <- function(tipo, severidade, ...) {
  eventos_ponte <<- rbind(eventos_ponte, data.table(tipo, severidade))
  invisible(NULL)
}
env$MONITORA_CORRECOES_DIR <- file.path(dir_saida, "correcoes")
ponte <- env$monitora_ponte_pre_painel_regras_contrato_unico(contrato = list())
stopifnot(
  identical(ponte$status, "cobertura_ok_com_diagnosticos_complementares"),
  nrow(ponte$divergencias) == 0L,
  nrow(ponte$diagnosticos_complementares) == 1L,
  ponte$diagnosticos_complementares$identificador[[1L]] == "diagnostico_revisao_teste",
  nrow(eventos_ponte) == 1L,
  eventos_ponte$severidade[[1L]] == "INFO"
)

codigo <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('shiny::tabPanel(\n            "Correções de registros"', codigo, fixed = TRUE),
  grepl('shiny::tabPanel(\n            "Equipe da COLETA"', codigo, fixed = TRUE),
  grepl('shiny::tabPanel(\n            "Validação espacial"', codigo, fixed = TRUE),
  grepl('shiny::tabPanel(\n            "Justificar pendências"', codigo, fixed = TRUE),
  grepl('Filtrar e selecionar pendências', codigo, fixed = TRUE),
  grepl('just_adicionar_rotulos_lote', codigo, fixed = TRUE),
  grepl('just_selecionar_filtradas', codigo, fixed = TRUE),
  grepl('just_sessao_excluir_selecionadas', codigo, fixed = TRUE),
  grepl('monitora_pendencias_justificativas_adicionar_lote_atomico', codigo, fixed = TRUE),
  grepl('geoservicos.inde.gov.br/geoserver/ICMBio/ows', codigo, fixed = TRUE),
  grepl('copy_of_limite_ucs_federais_082026.zip', codigo, fixed = TRUE) == FALSE
)

cat(sprintf(
  "TEST_V295_INTEGRIDADE_OPERACIONAL_OK lote_404=%.3fs exclusao_404=%.3fs\n",
  tempo_lote_404, tempo_exclusao_404
))
