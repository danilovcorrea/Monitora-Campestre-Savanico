#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args)) args[[1L]] else "monitora_campsav_alvo_global_v2.9.24-dev_r05.R", mustWork = TRUE)
zip_sismonitora <- if (length(args) >= 2L) normalizePath(args[[2L]], mustWork = TRUE) else NA_character_
baseline <- normalizePath(if (length(args) >= 3L) args[[3L]] else "monitora_campsav_alvo_global_v2.9.23.R", mustWork = TRUE)
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

arvore <- parse(file = script, keep.source = FALSE, encoding = "UTF-8")
arvore_base <- parse(file = baseline, keep.source = FALSE, encoding = "UTF-8")
assert(length(arvore) == 1L && identical(arvore[[1L]][[1L]], quote(base::evalq)), "A candidata deixou de ser uma única expressão externa base::evalq.")
linhas_c <- readLines(script, warn = FALSE, encoding = "UTF-8")
linhas_b <- readLines(baseline, warn = FALSE, encoding = "UTF-8")
cabecalho_c <- head(linhas_c, 40L)
assert(any(cabecalho_c == "### Versão candidata do script: 2.9.24-dev"), "Cabeçalho não identifica a candidata v2.9.24-dev.")
assert(any(cabecalho_c == "### Baseline pública de origem: v2.9.23"), "Cabeçalho não identifica a baseline pública v2.9.23.")
assert(any(cabecalho_c == "### Versão pública vigente: 2.9.23"), "Cabeçalho não identifica a versão pública vigente v2.9.23.")
assert(any(linhas_c == 'MONITORA_SCRIPT_VERSAO <- "2.9.24-dev"') && any(linhas_c == 'MONITORA_SCRIPT_BUILD_ID <- "v2.9.24-dev-20260909-r05"'), "Constantes executáveis divergem da candidata declarada no cabeçalho.")
inicio_congelado <- function(x) {
  a <- match("base::evalq({", x)
  b <- match("### BLOCO OPERACIONAL PRINCIPAL - EDITE AQUI ---------------------------------", x)
  assert(!is.na(a) && !is.na(b), "Bloco inicial congelado não localizado.")
  z <- x[a:(b - 1L)]
  z <- sub('MONITORA_SCRIPT_VERSAO <- ".*"', 'MONITORA_SCRIPT_VERSAO <- "<VERSAO>"', z)
  z <- sub('MONITORA_SCRIPT_BUILD_ID <- ".*"', 'MONITORA_SCRIPT_BUILD_ID <- "<BUILD>"', z)
  z
}
assert(identical(inicio_congelado(linhas_c), inicio_congelado(linhas_b)), "Inicialização congelada do RStudio foi alterada.")
encontrar_funcao <- function(x, nome) {
  achados <- list()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    if (as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L && is.symbol(no[[2L]]) &&
        identical(as.character(no[[2L]]), nome) && is.call(no[[3L]]) && identical(as.character(no[[3L]][[1L]])[1L], "function")) {
      achados[[length(achados) + 1L]] <<- no[[3L]]
    }
    if (length(no) > 1L) for (ii in 2:length(no)) visitar(no[[ii]])
    invisible(NULL)
  }
  visitar(x)
  assert(length(achados) == 1L, paste0("Função contratual não unívoca: ", nome))
  achados[[1L]]
}
for (nome_contrato in c("monitora_correcao_hex_para_raw", "monitora_correcao_xlsforms_embutidos")) {
  assert(identical(encontrar_funcao(arvore[[1L]], nome_contrato), encontrar_funcao(arvore_base[[1L]], nome_contrato)), paste0("Contrato único alterado em ", nome_contrato, "."))
}
bytes_crlf <- file.info(script)$size + length(linhas_c)
assert(bytes_crlf < 5 * 1024^2, "A candidata excederia 5 MiB no RStudio/Windows em CRLF.")
env <- new.env(parent = globalenv())
carregar_funcoes <- function(x) {
  if (!is.call(x)) return(invisible(NULL))
  op <- as.character(x[[1L]])[1L]
  if (op %in% c("<-", "=") && length(x) >= 3L && is.symbol(x[[2L]]) &&
      is.call(x[[3L]]) && identical(as.character(x[[3L]][[1L]]), "function")) {
    try(eval(x, env), silent = TRUE)
  }
  if (length(x) > 1L) for (ii in 2:length(x)) try(carregar_funcoes(x[[ii]]), silent = TRUE)
  invisible(NULL)
}
invisible(lapply(as.list(arvore), carregar_funcoes))

env$MONITORA_CORRECAO_COLUNAS_PROTEGIDAS <- character(0)
env$MONITORA_COL_ROW_ID <- "MONITORA_ROW_ID"
env$monitora_privacidade_caminho_entrada <- function(x) basename(x)
env$monitora_publicacao_b_correcoes_dir <- function() tempdir()
env$monitora_fwrite <- function(...) invisible(TRUE)

# Gate 1 — exportação XLSX nova: aba biológica por esquema, não pela posição.
if (!is.na(zip_sismonitora)) {
  dir_xlsx <- tempfile("gate_xlsx_")
  dir.create(dir_xlsx)
  utils::unzip(zip_sismonitora, exdir = dir_xlsx)
  planilhas <- list.files(dir_xlsx, pattern = "\\.xlsx$", full.names = TRUE)
  aud_abas <- rbindlist(lapply(planilhas, env$monitora_io_selecionar_aba_xlsx), fill = TRUE)
  assert(nrow(aud_abas) == 3L, "Não foram auditadas as três planilhas reais do ZIP SISMONITORA.")
  assert(all(aud_abas$aba_selecionada == "data"), "A aba biológica real não foi selecionada pelo esquema.")
  linhas <- vapply(seq_len(nrow(aud_abas)), function(i) {
    nrow(readxl::read_excel(planilhas[i], sheet = aud_abas$aba_selecionada[i], col_types = "text"))
  }, integer(1L))
  assert(identical(sort(linhas), c(303L, 2525L, 7272L)), "Contagem das linhas das abas biológicas divergiu do oráculo auditado.")
  assert(sum(linhas) == 10100L, "Total real de registros XLSX diferente de 10.100.")
}

# Gate 2 — codificação: CP1252 só é aceita com round-trip exato; UTF-8 não muda.
arq_cp <- tempfile(fileext = ".csv")
writeBin(as.raw(c(charToRaw("UUID,COLETA,UC,CICLO,CAMPANHA,UA,descricao\n1,A,U,C,C,1,Esp"), 0xE9, charToRaw("cie\n"))), arq_cp)
cp <- env$monitora_io_ler_csv_texto(arq_cp, colClasses = "character")
assert(validUTF8(cp$descricao[1L]) && identical(cp$descricao[1L], "Espécie"), "Conversão CP1252 para UTF-8 não foi exata.")
aud_cp <- attr(cp, "monitora_auditoria_codificacao")
assert(aud_cp$n_celulas_convertidas == 1L && aud_cp$status == "convertido_sem_perda_para_UTF-8", "Conversão CP1252 não foi auditada.")
arq_ruim <- tempfile(fileext = ".csv")
writeBin(as.raw(c(charToRaw("UUID,COLETA,UC,CICLO,CAMPANHA,UA,descricao\n1,A,U,C,C,1,"), 0x81, charToRaw("\n"))), arq_ruim)
res_byte <- tryCatch(env$monitora_io_ler_csv_texto(arq_ruim, colClasses = "character"), error = identity)
if (inherits(res_byte, "error")) {
  assert(grepl("sem perda", conditionMessage(res_byte), fixed = TRUE), "Falha de codificação não trouxe diagnóstico explícito.")
} else {
  volta_byte <- iconv(res_byte$descricao[1L], from = "UTF-8", to = "WINDOWS-1252", sub = NA_character_)
  assert(!is.na(volta_byte) && identical(charToRaw(volta_byte), as.raw(0x81)), "Byte aceito no Windows não preservou round-trip exato.")
}

# Gate 3 — painel: uma única representação física contratual e conflito bloqueante.
canonico <- "amostragem/registro/forma_serrapilheira"
rotulo <- "Materiais botânicos em decomposição no solo observados: (amostragem/registro)"
d <- data.table(id = 1:3, tmp = c(NA_character_, NA_character_, NA_character_), rot = c("material_inundado", "serrapilheira", NA_character_))
setnames(d, c("tmp", "rot"), c(canonico, rotulo))
res <- env$monitora_correcao_vincular_representacoes_contratuais_painel(d, "gate")
assert(rotulo %in% names(res$dt) && !(canonico %in% names(res$dt)), "Coluna vazia concorrente não foi removida no pré-painel.")
assert(identical(res$dt[[rotulo]], c("material_inundado", "serrapilheira", NA_character_)), "Valores de material botânico não foram preservados literalmente.")
d_conf <- data.table(a = "serrapilheira", b = "material_inundado")
setnames(d_conf, c("a", "b"), c(canonico, rotulo))
antes_conf <- copy(d_conf)
erro_conf <- tryCatch({ env$monitora_correcao_vincular_representacoes_contratuais_painel(d_conf, "gate"); "" }, error = conditionMessage)
assert(grepl("divergentes", erro_conf) && identical(d_conf, antes_conf), "Divergência entre representações não foi bloqueada antes de mutação.")
canonico_outros <- "amostragem/registro/forma_vida_outros"
rotulo_outros <- "forma_vida_outros"
d_atomico <- data.table(a = NA_character_, b = "serrapilheira", c = "liquen", d = "fungo")
setnames(d_atomico, c("a", "b", "c", "d"), c(canonico, rotulo, canonico_outros, rotulo_outros))
antes_atomico <- copy(d_atomico)
erro_atomico <- tryCatch({ env$monitora_correcao_vincular_representacoes_contratuais_painel(d_atomico, "gate"); "" }, error = conditionMessage)
assert(grepl("divergentes", erro_atomico) && identical(d_atomico, antes_atomico), "Conflito na segunda chave deixou conciliação parcial da primeira chave.")

# Gate 4 — fila: intenção aceita só é persistida após a conciliação sem conflito.
texto_script <- paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ini_fila <- regexpr("monitora_painel_adicionar_ops_pendentes <- function", texto_script, fixed = TRUE)[1L]
trecho_fila <- substr(texto_script, ini_fila, nchar(texto_script))
fim_fila_rel <- regexpr("monitora_painel_ocorrencia_tipo_por_operacao <- function", trecho_fila, fixed = TRUE)[1L]
bloco_fila <- substr(trecho_fila, 1L, fim_fila_rel - 1L)
pos_conflito <- regexpr("if (rec$n_conflitos > 0L)", bloco_fila, fixed = TRUE)[1L]
pos_persistencia <- regexpr("rv$correcoes_solicitadas <-", bloco_fila, fixed = TRUE)[1L]
assert(pos_conflito > 0L && pos_persistencia > pos_conflito, "A fila ainda persiste intenção antes do gate semântico.")
assert(length(gregexpr("rv$correcoes_solicitadas <-", bloco_fila, fixed = TRUE)[[1L]]) == 1L, "Há mais de um ponto de persistência da intenção na adição à fila.")

# Gate 4b — grafias históricas da mesma UC só se conciliam na camada estatística.
dir_uc <- tempfile("gate_uc_estatistica_"); dir.create(dir_uc)
fwrite_stub <- env$monitora_fwrite
env$monitora_fwrite <- function(x, file, ...) data.table::fwrite(x, file, ...)
env$MONITORA_OUTPUT_DIR <- dir_uc
uc_fonte <- data.table(
  UC = c(
    "Refúgio de Vida Silvestre das Veredas do Oeste Baiano",
    "Refúgio da Vida Silvestre das Veredas do Oeste Baiano"
  ),
  ANO = c(2022L, 2026L),
  COLETA = c("A", "B")
)
uc_fonte_antes <- copy(uc_fonte)
uc_stat <- copy(uc_fonte)
assert(isTRUE(env$monitora_stat_reconciliar_grafias_uc(uc_stat, dir_uc)), "Grafias que diferem somente por de/da não foram reconciliadas.")
assert(uniqueN(uc_stat$UC) == 1L && unique(uc_stat$UC) == "Refúgio da Vida Silvestre das Veredas do Oeste Baiano", "A grafia da campanha mais recente não prevaleceu na camada estatística.")
assert(identical(uc_fonte, uc_fonte_antes), "A reconciliação estatística alterou a fonte.")
arq_aud_uc <- file.path(dir_uc, "03_auditorias", "estatisticas", "auditoria_reconciliacao_grafias_uc_estatisticas.csv")
assert(file.exists(arq_aud_uc), "A reconciliação de grafias da UC não foi auditada.")
aud_uc <- fread(arq_aud_uc, encoding = "UTF-8")
assert(nrow(aud_uc) == 2L && all(!aud_uc$alterou_dados_fonte) && all(aud_uc$status == "reconciliada_na_camada_estatistica"), "Auditoria da reconciliação de grafias da UC está incompleta.")
uc_distintas <- data.table(UC = c("Parque Nacional de Brasília", "Reserva Biológica da Contagem"), ANO = 2026L)
assert(!isTRUE(env$monitora_stat_reconciliar_grafias_uc(uc_distintas, dir_uc)) && uniqueN(uc_distintas$UC) == 2L, "UCs materialmente distintas foram conciliadas indevidamente.")
harm_uc <- env$monitora_relatorios_analiticos_harmonizar_uc_registros(
  uc_fonte,
  "Refúgio da Vida Silvestre das Veredas do Oeste Baiano"
)
assert(uniqueN(harm_uc$registros$UC) == 1L && isTRUE(harm_uc$alterado), "Cópia interna do relatório não foi harmonizada.")
assert(identical(uc_fonte, uc_fonte_antes) && all(!harm_uc$auditoria$alterou_registros_corrig_fonte), "Harmonização do relatório alterou registros_corrig de origem.")
erro_harm <- tryCatch({
  env$monitora_relatorios_analiticos_harmonizar_uc_registros(
    uc_distintas,
    "Parque Nacional de Brasília"
  )
  ""
}, error = conditionMessage)
assert(grepl("diferem materialmente", erro_harm, fixed = TRUE), "Cópia do relatório aceitou UCs materialmente distintas.")
env$monitora_fwrite <- fwrite_stub

# Gate 5 — relatório de validação: vazios não viram operações e totais são separados.
env$MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA <- data.table()
env$MONITORA_AUDITORIA_CORRECOES_ESPACIAIS_ULTIMA <- data.table()
assert(nrow(env$monitora_doc_operacoes_dt()) == 0L, "Auditoria vazia criou operação fantasma.")
ops <- data.table(
  origem_documental = c("trilha_semantica_sessao_atual", "sanitizacao_encostam_desconhecida_pre_painel", "auditoria_correcoes_campos"),
  event_id = c("E1", NA, NA), id_correcao = c("C1", NA, NA),
  valor_antes = c("a", "x", "a"), valor_depois = c("b", "y", "b"),
  status = c("aplicada", "aplicada", "auditada"), tipo_correcao = "", contexto = ""
)
rex <- env$monitora_doc_resumo_executivo_tratamentos(ops)
assert(rex[Categoria == "Modificações do bolsista — sessão atual", `Nº de modificações comprovadas`] == 1L, "Modificação do bolsista não foi individualizada.")
assert(rex[Categoria == "Modificações automáticas", `Nº de modificações comprovadas`] == 1L, "Modificação automática não foi individualizada.")
assert(rex[Categoria == "Auditoria, recuperação e conciliação", `Nº de modificações comprovadas`] == 0L, "Auditoria voltou a ser contada como correção.")
dir_aud <- tempfile("auditoria_execucao_"); dir.create(dir_aud)
fwrite(data.table(id = 1:2), file.path(dir_aud, "auditoria_gate_EXEC_GATE.csv"))
fwrite(data.table(id = 1:5), file.path(dir_aud, "auditoria_gate_EXEC_ANTERIOR.csv"))
aud_exec <- env$monitora_doc_auditorias_execucao_resumo(dir_aud, "EXEC_GATE")
rex_aud <- env$monitora_doc_resumo_executivo_tratamentos(ops, aud_exec)
assert(aud_exec$n_arquivos == 1L && aud_exec$n_registros == 2L, "Contagem de auditorias misturou execuções ou não contou registros reais.")
assert(rex_aud[Categoria == "Auditoria, recuperação e conciliação", `Nº de registros de auditoria`] == 2L && rex_aud[Categoria == "Auditoria, recuperação e conciliação", `Nº de arquivos de auditoria`] == 1L, "Resumo executivo não materializou o somatório técnico separado.")
fontes <- env$monitora_doc_resumo_fontes_entrada(c("/tmp/input/novo.xlsx", "/tmp/input/registros_corrig.csv", "/tmp/input/linhagem/manifesto.csv"))
assert(fontes[Categoria == "Arquivos da rodada atual", `Nº de arquivos`] == 1L && fontes[Categoria == "Arquivos herdados de rodadas anteriores", `Nº de arquivos`] == 2L, "Arquivos atuais e herdados não foram separados.")

# Gate 6 — tabelas analíticas: mesmo ano, n explícito, ordem padronizada e UAs não ocultadas.
cob <- data.table(ANO = c(2025L, 2026L), form_veg = "campestre", categoria = "solo_nu", categoria_label = "Solo exposto ou rochas", cobertura_percent = c(50, 2), n_UA = c(8L, 10L), n = c(400L, 20L))
prop_antiga <- data.table(ANO = 2025L, form_veg = "campestre", categoria = "solo_nu", categoria_label = "Solo exposto ou rochas", prop_percent = 9, n_UA = 8L, n = 72L)
tab_sem_mix <- env$tabela_estado_nucleo(cob, prop_antiga, "Categoria", 8L, TRUE)
comparadores_cob <- data.table(ANO = 2026L, form_veg = "campestre", categoria = c("arbusto_abaixo", "arbusto_acima"), categoria_label = c("Arbusto nativo < 50cm", "Arbusto nativo ≥ 50cm"), cobertura_percent = c(8.6, 12), n_UA = 24L, n = c(209L, 291L))
comparadores_prop <- data.table(ANO = 2026L, form_veg = "campestre", categoria = c("arbusto_abaixo", "arbusto_acima"), categoria_label = c("Arbusto nativo < 50cm", "Arbusto nativo ≥ 50cm"), prop_percent = c(10.4, 14.5), n_UA = 24L, n = c(209L, 291L))
tab_comparadores <- env$tabela_estado_nucleo(comparadores_cob, comparadores_prop, "Forma", 8L)
assert(nrow(tab_comparadores) == 2L && data.table::uniqueN(tab_comparadores$Forma) == 2L && setequal(tab_comparadores$`Nº de registros`, c(209L, 291L)), "Classes < e ≥ colidiram na junção de cobertura e proporção.")
assert(tab_sem_mix$`Cobertura (%)` == "2,0" && !(tab_sem_mix$`Proporção relativa (%)` %in% c("9", "9,0")), "Tabela misturou cobertura recente com proporção antiga.")
prop <- data.table(ANO = 2026L, form_veg = "campestre", categoria = "solo_nu", categoria_label = "Solo exposto ou rochas", prop_percent = 3, n_UA = 9L, n = 30L)
tab <- env$tabela_estado_nucleo(cob, prop, "Categoria", 8L, TRUE)
assert(all(c("Nº de UAs — cobertura", "Nº de UAs — composição", "Nº de registros", "Nº de pontos com presença", "Cobertura (%)", "Proporção relativa (%)") %in% names(tab)), "Tabela ocultou divergência de UAs ou contagens n.")
assert(match("Nº de registros", names(tab)) < match("Cobertura (%)", names(tab)) && match("Cobertura (%)", names(tab)) < match("Proporção relativa (%)", names(tab)), "Ordem Nº de UAs/registros/Cobertura/Proporção não foi aplicada.")

# Gate 7 — solo/rochas: categoria exata e somente o ano mais recente.
env$evidencias_hipoteses <- data.table()
solo <- data.table(
  ANO = c(2025L, 2026L, 2026L), categoria = c("solo_nu", "solo_nu", "herbacea"),
  categoria_label = c("Solo exposto ou rochas", "Solo exposto ou rochas", "Herbáceas"),
  cobertura_percent = c(50, 2, 93.5)
)
env$adicionar_estado_positivo(solo, "Solo nu/rochas", "Presença de solo nu ou rochas no estado mais recente", "solo_nu")
assert(nrow(env$evidencias_hipoteses) == 1L && grepl("2,0%", env$evidencias_hipoteses$evidencia, fixed = TRUE), "Evidência de solo/rochas ainda usa máximo de outra categoria ou ano.")
assert(!grepl("93,5%", env$evidencias_hipoteses$evidencia, fixed = TRUE), "Valor de herbáceas contaminou a evidência de solo/rochas.")

# Gates estáticos de escopo editorial e campanha única.
assert(!grepl("# Esforço amostral por UC, formação e ano", texto_script, fixed = TRUE), "Título redundante 'por UC' permanece no relatório de uma única UC.")
assert(grepl("limits = if (data.table::uniqueN(x$ANO) == 1L)", texto_script, fixed = TRUE), "Limite explícito do eixo x para campanha única não foi implementado.")
assert(!grepl("n contatos registrados", texto_script, fixed = TRUE) && grepl("Nº de registros", texto_script, fixed = TRUE), "Rótulo público da contagem não foi revisado integralmente.")
assert(
  grepl("Como ler as contagens", texto_script, fixed = TRUE) &&
    grepl("essa coluna não representa o número de UAs com presença", texto_script, fixed = TRUE) &&
    grepl("Nº de UAs — cobertura", texto_script, fixed = TRUE) &&
    grepl("Nº de UAs — composição", texto_script, fixed = TRUE) &&
    grepl("o número de registros pode ser superior ao número de pontos com presença", texto_script, fixed = TRUE),
  "Explicação pública de UAs, registros e pontos com presença está ausente."
)
assert(!grepl("Nenhum contexto de fogo foi localizado", texto_script, fixed = TRUE) && grepl("cada contexto foi informado", texto_script, fixed = TRUE), "Texto sem contexto de fogo ainda introduz regra técnica desnecessária.")
assert(!grepl("os pares de símbolos e C derivam", texto_script, fixed = TRUE) && grepl("tem_simbolos_plotados", texto_script, fixed = TRUE), "Legenda das figuras ainda declara símbolos inexistentes ou deixou de ser condicional.")
assert(!grepl("O diagnóstico não impeditivo materializou", texto_script, fixed = TRUE) && grepl("texto_seca_morta_leitor", texto_script, fixed = TRUE), "Texto de seca/morta permanece orientado ao desenvolvedor.")
assert(grepl("length(anos) == 1L", texto_script, fixed = TRUE) && !grepl('" pontos amostrais válidos** e esforço anual de "', texto_script, fixed = TRUE), "Resumo executivo ainda produz faixa artificial de esforço para campanha única.")

# Gate 8 — materialização do relatório de validação e dos quadros executivos.
dir_doc <- tempfile("gate_relatorio_validacao_")
dir.create(file.path(dir_doc, "input", "linhagem"), recursive = TRUE)
dir.create(file.path(dir_doc, "output"), recursive = TRUE)
dir.create(file.path(dir_doc, "log"), recursive = TRUE)
writeLines("fonte atual", file.path(dir_doc, "input", "novo.xlsx"))
writeLines("checkpoint", file.path(dir_doc, "input", "registros_corrig.csv"))
writeLines("sidecar", file.path(dir_doc, "input", "linhagem", "manifesto.csv"))
env$MONITORA_INPUT_DIR <- file.path(dir_doc, "input")
env$MONITORA_OUTPUT_DIR <- file.path(dir_doc, "output")
env$MONITORA_LOG_DIR <- file.path(dir_doc, "log")
env$MONITORA_SCRIPT_VERSAO <- "2.9.24-dev"
env$MONITORA_SCRIPT_BUILD_ID <- "v2.9.24-dev-20260909-r05"
env$MONITORA_MODO_EXECUCAO <- "completo"
env$MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS <- FALSE
env$MONITORA_REGISTROS_VALIDADOS_GERADO <- FALSE
env$MONITORA_TRILHA_SEMANTICA_SESSAO <- copy(ops[1L])
doc_dt <- data.table(COLETA = "A", UC = "UC de teste", EA = "EA1", UA = "UA1", ANO = "2026")
arquivos_doc <- env$monitora_relatorio_validacao_consolidado_gerar(
  doc_dt, output_dir = env$MONITORA_OUTPUT_DIR, log_dir = env$MONITORA_LOG_DIR,
  exec_id = "gate_v2924", responsavel = "QA", instituicao = "ICMBio", formatos = character()
)
assert(length(arquivos_doc) >= 2L && all(file.exists(arquivos_doc)), "Relatório de validação não foi materializado.")
dir_apoio <- file.path(env$MONITORA_OUTPUT_DIR, "07_relatorio_validacao", "dados_apoio")
fontes_doc <- fread(file.path(dir_apoio, "resumo_fontes_entrada_atual_e_herdada.csv"), encoding = "UTF-8")
trat_doc <- fread(file.path(dir_apoio, "resumo_executivo_tratamentos_por_natureza.csv"), encoding = "UTF-8")
assert(fontes_doc[Categoria == "Arquivos da rodada atual", `Nº de arquivos`] == 1L && fontes_doc[Categoria == "Arquivos herdados de rodadas anteriores", `Nº de arquivos`] == 2L, "Relatório materializado não separou entrada atual e herdada.")
assert(trat_doc[Categoria == "Modificações do bolsista — sessão atual", `Nº de modificações comprovadas`] == 1L, "Relatório materializado não separou modificações do bolsista.")
rotulos_utf8 <- env$monitora_doc_title_case_utf8(c("pré-painel", "normalização automática", "pós-correção/fechamento"))
assert(all(validUTF8(rotulos_utf8)) && identical(rotulos_utf8, c("Pré-Painel", "Normalização Automática", "Pós-Correção/Fechamento")), "Capitalização editorial deixou de preservar UTF-8 no Windows.")
arq_sanitizacoes_leitura <- file.path(dir_apoio, "sanitizacoes_automaticas_para_leitura.csv")
conteudo_sanitizacoes <- readChar(arq_sanitizacoes_leitura, nchars = file.info(arq_sanitizacoes_leitura)$size, useBytes = TRUE)
assert(validUTF8(conteudo_sanitizacoes), "Relatório de sanitizações materializou bytes UTF-8 inválidos.")

cat("TEST_V2924_DEV_AUDITORIA_20260908_OK\n")
