# Integração entre os diagnósticos do contrato e a continuidade de decisões.
# Não redefine regras biológicas nem decide aptidão de registros_validados.
monitora_v307_justificativas_marcar_diagnostico <- function(
    x, completo = FALSE, espacial = FALSE
) {
  x <- data.table::copy(data.table::as.data.table(x))
  familias <- character()
  if (isTRUE(completo)) {
    familias <- as.character(monitora_diag_rel_catalogo_ocorrencias_base()$tipo_ocorrencia)
    if (isTRUE(espacial)) familias <- c(familias,
      paste0("pendencia_espacial__", c("coordenada_invalida_ou_ausente",
        "coordenadas_conflitantes_na_coleta", "referencia_insuficiente",
        "referencia_temporal_ambigua", "pendencia_inicio_divergente",
        "pendencia_fim_divergente", "pendencia_ambos_divergentes",
        "possivel_transecto_invertido", "possivel_ua_trocada")),
      paste0("alerta_espacial__", c("alerta_raio_rigoroso", "alerta_acuracia_gps",
        "alerta_comprimento_transecto", "alerta_referencia_temporal_ambigua",
        "alerta_referencia_insuficiente", "alerta_referencia_limitada",
        "alerta_proximidade_outra_ua", "alerta_serie_temporal_secundaria",
        "alerta_ua_consenso_coincidente")))
  }
  attr(x, "monitora_diagnostico_completo") <- isTRUE(completo)
  attr(x, "monitora_familias_avaliadas") <- unique(familias)
  x
}

# O acumulador do painel precisa conservar a prova de ocorrências que já
# desapareceram. rbindlist sozinho descarta esses atributos de origem.
monitora_v307_justificativas_acumular_encerramentos <- function(anterior, novos) {
  aliases <- monitora_v307_justificativas_juntar(list(
    attr(anterior, "monitora_v307_aliases", exact = TRUE),
    attr(novos, "monitora_v307_aliases", exact = TRUE)
  ), aliases = TRUE)
  out <- monitora_v307_justificativas_juntar(list(anterior, novos))
  attr(out, "monitora_v307_aliases") <- aliases
  out
}

monitora_v307_publicacao_preflight <- function() {
  monitora_v307_validacao_exigir(monitora_v307_relatorios_preflight(),
    "Publicação bloqueada por relatório não regenerado.")
  monitora_v307_transacao_preflight(verificacoes = list(function() {
    h <- monitora_v307_justificativas_ler_historico()
    monitora_v307_justificativas_eventos_validar(h)
    monitora_v307_justificativas_aliases_validar(attr(h, "monitora_v307_aliases"))
    TRUE
  }))
  invisible(TRUE)
}

monitora_v307_publicacao_validar <- function(output_dir) {
  lin <- file.path(output_dir, "02_painel_correcoes", "linhagem")
  path <- file.path(lin, "manifesto_linhagem.json")
  if (!file.exists(path)) stop("Publicação bloqueada: manifesto final ausente.", call. = FALSE)
  man <- monitora_v307_justificativas_json_ler(path)
  atual <- as.character(get0("MONITORA_EXEC_ID", ifnotfound = "", inherits = TRUE))[1L]
  if (!identical(as.character(man$exec_id), atual)) stop("Publicação não pertence à execução atual.", call. = FALSE)
  arquivo <- file.path(output_dir, "01_produtos_dados", "registros_corrig.csv")
  hash <- monitora_v307_justificativas_hash(arquivo, arquivo = TRUE)
  if (!identical(as.character(man$output_data_sha256), hash)) stop("Dados publicados divergem do manifesto.", call. = FALSE)
  for (par in list(c("ledger_sha256", "correcoes_semanticas_consolidada.csv"),
      c("sanitizacoes_automaticas_sha256", "auditoria_sanitizacoes_automaticas.csv"),
      c("metadados_sessoes_sha256", "metadados_sessoes_painel_consolidado.csv"),
      c("inventario_sessoes_sha256", "inventario_sessoes_linhagem.csv"),
      c("incorporacoes_novas_coletas_sha256", "incorporacoes_novas_coletas.csv"))) {
    if (!identical(as.character(man[[par[1L]]]), monitora_v307_justificativas_hash(file.path(lin, par[2L]), arquivo = TRUE))) {
      stop("Componente publicado diverge do manifesto: ", par[2L], call. = FALSE)
    }
  }
  monitora_v307_justificativas_validar_importacao(lin, man,
    data_sha256 = hash, revision_id = as.character(man$revision_id))
  TRUE
}
