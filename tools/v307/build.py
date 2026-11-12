"""Candidata derivada literalmente da release v3.0.6; fontes públicas intactas."""
from pathlib import Path
import base64
import argparse
import hashlib
import json
import lzma
import re
import subprocess
import tempfile
import textwrap
from datetime import datetime
from zoneinfo import ZoneInfo

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / "monitora_campsav_alvo_global_v3.0.6.R"
BASE_SHA = "454cb8f1d6f74ec8df2695236add1206080f869a3209c8a09aa92af9b6186d45"


def function_span(text, name):
    match = re.search(r"^" + re.escape(name) + r"\s*<-\s*function\b", text, re.M)
    if not match:
        raise AssertionError(f"Função ausente: {name}")
    first = text.index("{", match.end())
    depth, quote, escaped, comment = 0, None, False, False
    for pos in range(first, len(text)):
        char = text[pos]
        if comment:
            if char == "\n":
                comment = False
            continue
        if quote:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            continue
        if char == "#":
            comment = True
        elif char in ('"', "'", "`"):
            quote = char
        elif char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return match.start(), first, pos + 1
    raise AssertionError(f"Corpo não balanceado: {name}")


class Candidate:
    def __init__(self, text):
        self.text = text
        self.changes = []

    def replace(self, old, new, count=1, label=""):
        actual = self.text.count(old)
        assert actual == count, (label or old[:90], actual, count)
        self.text = self.text.replace(old, new)
        self.changes.append(label or old[:90])

    def body(self, name, body):
        start, brace, end = function_span(self.text, name)
        self.text = self.text[:brace + 1] + "\n" + body.rstrip() + "\n}" + self.text[end:]
        self.changes.append(name)

    def patch_function(self, name, transform):
        start, _, end = function_span(self.text, name)
        old = self.text[start:end]
        new = transform(old)
        assert new != old, name
        self.text = self.text[:start] + new + self.text[end:]
        self.changes.append(name)


def unique(text, old, new):
    assert text.count(old) == 1, (old[:100], text.count(old))
    return text.replace(old, new)


def module_without_comments(module):
    # As fontes legíveis são preservadas. Só o payload embutido perde comentários;
    # uma comparação de AST em R comprova identidade antes de gerar o arquivo.
    out = []
    quote, escaped, comment = None, False, False
    for char in module:
        if comment:
            if char == "\n":
                comment = False
                out.append(char)
            continue
        if quote:
            out.append(char)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
        elif char == "#":
            comment = True
        else:
            out.append(char)
            if char in ('"', "'", "`"):
                quote = char
    clean = "".join(out)
    with tempfile.TemporaryDirectory(prefix="monitora_v307_ast_") as directory:
        original, payload = [Path(directory) / p for p in ("original.R", "payload.R")]
        original.write_text(module, encoding="utf-8")
        payload.write_text(clean, encoding="utf-8")
        result = subprocess.run(["Rscript", "--vanilla", "-e",
            'a<-commandArgs(TRUE);stopifnot(identical(parse(a[1],keep.source=FALSE,encoding="UTF-8"),parse(a[2],keep.source=FALSE,encoding="UTF-8")))',
            str(original), str(payload)], capture_output=True, text=True)
        assert result.returncode == 0, result.stderr
    return clean


def build(revision=None):
    if revision is None:
        number = 1
        while (ROOT / f"monitora_campsav_alvo_global_v3.0.7-dev_r{number:02d}.R").exists():
            number += 1
        revision = f"r{number:02d}"
    assert re.fullmatch(r"r[0-9]{2,}", revision), revision
    version = f"3.0.7-dev_{revision}"
    dest = ROOT / f"monitora_campsav_alvo_global_v{version}.R"
    baseline_bytes = BASE.read_bytes()
    assert hashlib.sha256(baseline_bytes).hexdigest() == BASE_SHA
    c = Candidate(baseline_bytes.decode("utf-8"))
    c.replace("# Versão 3.0.6 —", f"# Versão {version} —")
    c.replace('MONITORA_SCRIPT_VERSAO <- "3.0.6"', f'MONITORA_SCRIPT_VERSAO <- "{version}"')
    c.replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.6-20260929-r01"',
              f'MONITORA_SCRIPT_BUILD_ID <- "v{version}-{datetime.now(ZoneInfo("America/Sao_Paulo")):%Y%m%d}"')
    # Mantém o corpo e o instante de definição do comparador. A expressão só é
    # avaliada na posição original; o payload comprimido guarda a mesma AST.
    relocated_name = "monitora_relatorios_comparar_pre_pos_correcoes"
    start, _, end = function_span(c.text, relocated_name)
    relocated = c.text[start:end]
    c.text = (c.text[:start] +
        'base::eval(MONITORA_V307_DEFINICAO_COMPARACAO, envir=environment())' + c.text[end:])
    c.changes.append(relocated_name + ": definição intacta comprimida, avaliada na posição original")
    sources = [ROOT / "tools/v307" / name for name in (
        "execucao.R", "justificativas.R", "integracao.R", "protecao_editorial.R", "cache.R", "relatorios.R", "caminhos.R", "estilo_docx.R")]
    source_bytes = {p: p.read_bytes() for p in sources}
    module = module_without_comments("\n".join(source_bytes[p].decode("utf-8") for p in sources) +
        "\nMONITORA_V307_DEFINICAO_COMPARACAO <- quote({\n" + relocated + "\n})\n")
    encoded = base64.b64encode(lzma.compress(module.encode("utf-8"), preset=9)).decode("ascii")
    chunks = ",\n".join('"' + part + '"' for part in textwrap.wrap(encoded, 12000))
    loader = ('# MONITORA_V307_MODULO_EMBUTIDO\n'
              'base::eval(base::parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(\n' + chunks +
              '\n)),type="xz")),encoding="UTF-8"),envir=environment())\n')
    guard = ('if (!exists("MONITORA_EXEC_ID", inherits = TRUE) || !nzchar(as.character(get("MONITORA_EXEC_ID", inherits = TRUE))[1L])) {\n'
             '  MONITORA_EXEC_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")\n}')
    c.replace(guard, loader + 'monitora_v307_execucao_iniciar()')
    # O contrato legado de hash continua literalmente preservado.
    c.body("monitora_pendencias_ocorrencia_id", "  monitora_v307_justificativas_ocorrencia_id(x)")
    c.body("monitora_pendencias_justificativas_ler_historico", "  monitora_v307_justificativas_ler_historico()")
    c.body("monitora_pendencias_justificativas_preparar", "  monitora_v307_justificativas_preparar(ocorrencias_atuais, eventos_sessao, eventos_encerrados_sessao)")
    c.body("monitora_pendencias_justificativas_persistir_preparado", "  monitora_v307_justificativas_persistir_preparado(preparado)")
    c.body("monitora_pendencias_justificativas_validar_sessao", "  monitora_v307_justificativas_preflight(eventos_sessao, ocorrencias_atuais)")
    c.body("monitora_pendencias_justificativas_reconciliar_sessao", "  monitora_v307_justificativas_reconciliar_sessao(eventos_sessao, ocorrencias_atuais, timestamp_evento)")
    c.patch_function("monitora_pendencias_justificativas_criar_lote", lambda text: unique(text,
        '  list(ok = TRUE, erro = "", dados = validacao$dados)',
        '  attr(validacao$dados,"monitora_v307_aliases") <- monitora_v307_justificativas_aliases(oc)\n'
        '  list(ok = TRUE, erro = "", dados = validacao$dados)'))
    c.body("monitora_pendencias_justificativas_adicionar_lote_atomico",
           "  monitora_v307_justificativas_adicionar_lote_atomico(eventos_sessao, novo_lote, ocorrencias_atuais)")
    c.patch_function("monitora_pendencias_justificativas_reconstituir_lotes", lambda text: unique(text,
        "  monitora_pendencias_justificativas_normalizar_sessao(out)\n}",
        "  monitora_v307_justificativas_preservar_atributos(\n"
        "    monitora_pendencias_justificativas_normalizar_sessao(out), x)\n}"))
    c.patch_function("monitora_io_eh_csv_controle_correcao", lambda text: unique(
        text, '    "correcoes_semanticas",',
        '    "justificativas|pendencias_justificadas|pendencias_remanescentes_com_justificativas",\n'
        '    "|correcoes_semanticas",'))
    c.replace("^auditoria_correcoes_campos_[0-9]{8}_[0-9]{6}[.]csv$",
              "^auditoria_correcoes_campos_[0-9]{8}_[0-9]{6}(_[0-9]{3,})?[.]csv$")
    c.patch_function("monitora_output_reescrever_referencias", lambda text: unique(text,
        "  candidatos_norm <- gsub(",
        "  candidatos <- monitora_v307_editorial_candidatos_mutaveis(candidatos)\n"
        "  candidatos_norm <- gsub("))
    c.patch_function("monitora_cache_painel_salvar", lambda text: unique(text,
        '  saveRDS(obj, arq, compress = "xz")',
        '  obj <- monitora_v307_cache_assinar(obj)\n'
        '  saveRDS(obj, arq, compress = "xz")'))
    c.patch_function("monitora_controlador_abrir_painel_cache", lambda text: unique(text,
        "  cand_linhagem_cache <- list.files(",
        "  prova_cache <- monitora_v307_cache_validar(cache_obj)\n"
        "  monitora_v307_justificativas_registrar_cache_validado(prova_cache$memoria_sha256,\n"
        "    prova_cache$input_sha256, prova_cache$config_sha256, validado=isTRUE(prova_cache$ok))\n"
        "  cand_linhagem_cache <- list.files("))
    integrate_ui(c)
    integrate_reports(c)
    integrate_paths(c)
    integrate_publication(c)
    c.patch_function("monitora_relatorios_analiticos_renderizar", lambda text: unique(text,
        "      monitora_relatorios_analiticos_docx_auditar_figuras(\n        docx_candidato,",
        "      monitora_v307_docx_estilo_aplicar(docx_candidato)\n"
        "      monitora_relatorios_analiticos_docx_auditar_figuras(\n        docx_candidato,"))
    data = c.text.encode("utf-8")
    sizes = {"bytes_lf": len(data), "bytes_crlf": len(data.replace(b"\n", b"\r\n"))}
    assert max(sizes.values()) <= 5_000_000, sizes
    if dest.exists():
        assert dest.read_bytes() == data, f"Revisão preservada; use outra revisão: {dest}"
    else:
        dest.write_bytes(data)
    out = ROOT / "artifacts/v307"
    out.mkdir(parents=True, exist_ok=True)
    metadata = json.dumps({"version": version, "candidate": str(dest), "baseline_sha256": BASE_SHA,
        "candidate_sha256": hashlib.sha256(data).hexdigest(), **sizes,
        "module_sha256": hashlib.sha256(module.encode("utf-8")).hexdigest(),
        "module_comments_removed_ast_identical": True,
        "relocated_unchanged": {relocated_name: hashlib.sha256(relocated.encode("utf-8")).hexdigest()},
        "sources": {str(p.relative_to(ROOT)): hashlib.sha256(source_bytes[p]).hexdigest() for p in sources},
        "patches": c.changes}, ensure_ascii=False, indent=2) + "\n"
    per_revision = out / f"BUILD_{revision}.json"
    if per_revision.exists():
        assert per_revision.read_text(encoding="utf-8") == metadata, "Manifesto de revisão preservado"
    else:
        per_revision.write_text(metadata, encoding="utf-8")
    (out / "BUILD.json").write_text(metadata, encoding="utf-8")
    print(json.dumps({"candidate": str(dest), **sizes, "sha256": hashlib.sha256(data).hexdigest()}))


def integrate_ui(c):
    c.patch_function("monitora_correcao_painel", lambda text: unique(text,
        'monitora_correcao_painel <- function(dt, meta_xls = NULL, arquivo_saida = MONITORA_ARQUIVO_CORRECOES_CAMPOS) {',
        'monitora_correcao_painel <- function(dt, meta_xls = NULL, arquivo_saida = MONITORA_ARQUIVO_CORRECOES_CAMPOS) {\n'
        '  monitora_v307_justificativas_registrar_base(dt, origem="painel")'))
    c.replace('''    just_ocorrencias_base <- shiny::reactive({''', '''    monitora_painel_justificativas_universo <- function(ocorrencias = NULL) {
    if (is.null(ocorrencias)) ocorrencias <- rv$ocorrencias_idx
    x <- data.table::copy(data.table::as.data.table(ocorrencias))
    prova <- attr(x,"monitora_diagnostico_completo",exact=TRUE)
    if (!is.null(prova)) {
      if (isTRUE(prova) && !isTRUE(rv$preview_dirty)) return(x)
      return(monitora_v307_justificativas_marcar_diagnostico(x,completo=FALSE,espacial=FALSE))
    }
    monitora_v307_justificativas_marcar_diagnostico(x,
      completo=!isTRUE(rv$preview_dirty) && isTRUE(get0("MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK", ifnotfound=FALSE, inherits=TRUE)),
      espacial=isTRUE(get0("MONITORA_VALIDAR_ESPACIAL_COLETAS", ifnotfound=FALSE, inherits=TRUE)) && nrow(data.table::as.data.table(rv$preview_validacao_espacial))>0L)
    }
    just_ocorrencias_base <- shiny::reactive({''')
    c.replace('''    just_ocorrencias_base <- shiny::reactive({
    rv$preview_revision
    x <- data.table::copy(data.table::as.data.table(rv$ocorrencias_idx))
    if (!nrow(x)) return(x)''', '''    just_ocorrencias_base <- shiny::reactive({
    rv$preview_revision
    x <- data.table::copy(data.table::as.data.table(rv$ocorrencias_idx))''')
    c.replace('''    data.table::setorder(x, tipo_ocorrencia, UC, EA, UA, ANO, COLETA, linha_indice, na.last = TRUE)
    x[]
    })
    just_ocorrencias_exibidas''', '''    ordem_just <- intersect(c("tipo_ocorrencia","UC","EA","UA","ANO","COLETA","linha_indice"), names(x))
    if (length(ordem_just)) data.table::setorderv(x, ordem_just, na.last=TRUE)
    monitora_painel_justificativas_universo(x)
    })
    just_ocorrencias_exibidas''')
    c.replace('''    if (is.null(ocorrencias)) ocorrencias <- just_ocorrencias_base()
    ocorrencias <- data.table::as.data.table(ocorrencias)''', '''    ocorrencias <- monitora_painel_justificativas_universo(ocorrencias)''')
    c.replace('''      acumulados <- data.table::rbindlist(list(
        data.table::as.data.table(rv$justificativas_encerradas_sessao),
        rec$eventos_encerrados
      ), fill = TRUE, use.names = TRUE)
      rv$justificativas_encerradas_sessao <- unique(
        acumulados, by = "evento_justificativa_id"
      )''', '''      rv$justificativas_encerradas_sessao <- monitora_v307_justificativas_acumular_encerramentos(
        rv$justificativas_encerradas_sessao, rec$eventos_encerrados
      )''')
    c.replace('''      ocorrencias_atuais = data.table::as.data.table(rv$ocorrencias_idx),
      eventos_sessao = data.table::as.data.table(rv$justificativas_sessao),''', '''      ocorrencias_atuais = monitora_painel_justificativas_universo(rv$ocorrencias_idx),
      eventos_sessao = data.table::as.data.table(rv$justificativas_sessao),''')
    c.patch_function("monitora_correcao_painel", lambda text: unique(text,
        '    if (!nrow(z)) return(template)',
        '    if (!nrow(z)) return(monitora_v307_justificativas_preservar_atributos(template, z))'))
    c.replace('''    rv$ocorrencias_idx <- idx_atual[]
    rv$ocorrencias_resumo <- resumo_idx_atual[]
    monitora_painel_reconciliar_justificativas_sessao(''', '''    monitora_v307_justificativas_registrar_base(estado, origem="preview_confirmado", continuar=TRUE)
    idx_atual <- monitora_v307_justificativas_marcar_diagnostico(idx_atual,
      completo=TRUE, espacial=isTRUE(get0("MONITORA_VALIDAR_ESPACIAL_COLETAS",ifnotfound=FALSE,inherits=TRUE)) && nrow(validacao_esp_atual)>0L)
    rv$ocorrencias_idx <- idx_atual[]
    rv$ocorrencias_resumo <- resumo_idx_atual[]
    monitora_painel_reconciliar_justificativas_sessao(''')
    old = '''    just_ocorrencias_exibidas <- shiny::reactive({
    x <- just_ocorrencias_base()'''
    new = '''    just_estado <- shiny::reactive({
    monitora_v307_justificativas_estado(just_ocorrencias_base(), rv$justificativas_sessao)
    })
    just_ocorrencias_por_justificar <- shiny::reactive({
    just_estado()$pendencias_a_justificar
    })
    just_ocorrencias_exibidas <- shiny::reactive({
    x <- just_ocorrencias_por_justificar()'''
    c.replace(old, new)
    c.replace('''    just_ids_selecionados <- shiny::reactive({
    x <- just_ocorrencias_base()''', '''    just_ids_selecionados <- shiny::reactive({
    x <- just_ocorrencias_por_justificar()''')
    c.replace('''    shiny::observe({
    x <- just_ocorrencias_base()''', '''    shiny::observe({
    x <- just_ocorrencias_por_justificar()''')
    c.replace('''    total <- nrow(just_ocorrencias_base())
    exibidas <- nrow(just_ocorrencias_exibidas())''', '''    total <- nrow(just_ocorrencias_por_justificar())
    exibidas <- nrow(just_ocorrencias_exibidas())''')
    c.replace('shiny::h4("Filtrar e selecionar pendências"),',
              'shiny::h4("Pendências que aguardam justificativa"),')
    c.replace('mensagem = "Nenhuma ocorrência diagnóstica remanescente nesta avaliação."',
              'mensagem = "Nenhuma ocorrência aguarda justificativa nesta avaliação."')
    c.replace('''          shiny::h4("Justificativas adicionadas nesta sessão"),''', '''          shiny::h4("Ocorrências com justificativa vigente"),
          shiny::helpText("Estas ocorrências permanecem no diagnóstico e já possuem justificativa. Elas não precisam de novo registro."),
          monitora_painel_dt_output("just_tabela_historico"),
          shiny::h4("Justificativas adicionadas nesta sessão"),''')
    marker = '    just_sessao_proxy <- DT::dataTableProxy("just_tabela_sessao", session = session)'
    c.replace(marker, '''    output$just_tabela_historico <- DT::renderDT({
    x <- just_estado()$pendencias_remanescentes
    if (nrow(x)) x <- x[status_justificativa == "vigente"]
    if (!nrow(x)) return(DT::datatable(data.table::data.table(mensagem="Nenhuma justificativa vigente nesta avaliação."),selection="none",rownames=FALSE))
    cols <- intersect(c("rotulo_ocorrencia","COLETA","UC","EA","UA","ANO","status_justificativa","tipo_justificativa","justificativa","responsavel_justificativa","timestamp_justificativa"), names(x))
    DT::datatable(x[, ..cols], selection="none", options=monitora_painel_dt_options(), rownames=FALSE)
    }, server=TRUE)
''' + marker)
    c.replace('justificativas = c("just_tabela_ocorrencias", "just_tabela_sessao"),',
              'justificativas = c("just_tabela_ocorrencias", "just_tabela_sessao", "just_tabela_historico"),')


def integrate_reports(c):
    def wrap(text):
        anchor = '  fase_dir <- file.path(base_dir, paste0("relatorios_", fase))'
        assert text.count(anchor) == 1
        at = text.index(anchor) + len(anchor)
        body = unique(text[at:-1], '      }\n    }\n    )',
                      '      }\n      stop(e)\n    }\n    )')
        return (text[:at] + '\n  monitora_v307_relatorios_executar(base_dir, fase, function() {' +
                body + '\n  })\n}')
    c.patch_function("monitora_relatorios_suporte_painel_gravar", wrap)


def integrate_paths(c):
    def movers(text):
        pattern = r"(?m)^([ \t]*)mov <- monitora_output_mover_arquivo\(ff, (?:file.path\(output_dir, destino_rel\)|destino)\)$"
        def replacement(match):
            indent = match[1]
            return (indent + "mov <- monitora_v307_caminhos_mover(ff, destino_rel, output_dir, exec_id, contexto)\n" +
                    indent + "destino_rel <- mov$destino_relativo\n" +
                    indent + "destino <- file.path(output_dir, destino_rel)")
        text, count = re.subn(pattern, replacement, text)
        assert count == 2, count
        return text
    for name in ("monitora_output_consolidar_legados_ja_organizados",
                 "monitora_output_migrar_diretorios_legados", "monitora_output_organizar_produtos"):
        c.patch_function(name, movers)
    def organizer(text):
        old = "  aud_dirs <- monitora_output_migrar_diretorios_legados(output_dir, exec_id, contexto)"
        text = unique(text, old,
            "  aud_curto <- monitora_v307_caminhos_compactar_existentes(output_dir, exec_id, contexto)\n" + old +
            "\n  aud_dirs <- data.table::rbindlist(list(aud_curto, aud_dirs), fill=TRUE, use.names=TRUE)")
        text = unique(text,
            "  MONITORA_OUTPUT_MAPA_MOVIMENTACOES <<- data.table::rbindlist(list(mapa_anterior, mapa_atual), fill = TRUE, use.names = TRUE)",
            "  MONITORA_OUTPUT_MAPA_MOVIMENTACOES <<- monitora_v307_caminhos_mapa_atual(\n"
            "    data.table::rbindlist(list(mapa_anterior, mapa_atual), fill=TRUE, use.names=TRUE), mapa_atual, output_dir, exec_id)")
        return unique(text, "  monitora_output_reescrever_referencias(output_dir, mapa_total)",
            "  prova_caminhos <- monitora_v307_caminhos_preparar_edicao(output_dir)\n"
            "  monitora_output_reescrever_referencias(output_dir, mapa_total)\n"
            "  monitora_v307_caminhos_finalizar(output_dir, prova_caminhos)")
    c.patch_function("monitora_output_organizar_produtos", organizer)
    c.patch_function("monitora_output_reescrever_referencias", lambda text: unique(text,
        "  candidatos <- monitora_v307_editorial_candidatos_mutaveis(candidatos)",
        "  candidatos <- monitora_v307_editorial_candidatos_mutaveis(candidatos)\n"
        "  candidatos <- monitora_v307_caminhos_candidatos(candidatos, output_dir)"))


def integrate_publication(c):
    c.patch_function("monitora_publicacao_aa_exportar_registros_corrig_aprovado", lambda text: unique(text,
        "  dt <- data.table::as.data.table(registros_corrig)",
        "  dt <- data.table::copy(data.table::as.data.table(registros_corrig))"))
    c.replace('    MONITORA_ARQUIVO_CORRECOES_CAMPOS_REAPLICADO <<- arquivo_replay\n',
        '    monitora_v307_justificativas_registrar_replay(arquivo_replay,\n'
        '      validado=isTRUE(get0("MONITORA_CORRECOES_REAPLICADAS_PRE_PAINEL",ifnotfound=FALSE,inherits=TRUE)) && !isTRUE(MONITORA_REPLAY_DIAGNOSTICO_NAO_ABORTAR))\n'
        '    MONITORA_ARQUIVO_CORRECOES_CAMPOS_REAPLICADO <<- arquivo_replay\n')
    # O histórico declarado precisa ser validado no início da importação, não
    # apenas quando a primeira tabela reativa solicitar seus dados.
    c.patch_function("monitora_linhagem_importar_incremental", lambda text: unique(text,
        '  monitora_linhagem_inventario_sessoes_importar(input_dir = input_dir, manifesto = man)',
        '  monitora_v307_justificativas_ler_historico(input_dir=input_dir, arquivo_registros=arquivo_registros)\n'
        '  monitora_linhagem_inventario_sessoes_importar(input_dir = input_dir, manifesto = man)'))
    # Persistência pós-diagnóstico recebe prova explícita de quais famílias
    # foram avaliadas. Módulo ausente não é confundido com resolução.
    c.replace('    monitora_pendencias_justificativas_persistir(idx_pendencias_pos, eventos_sessao = NULL)',
        '    idx_pendencias_pos <- monitora_v307_justificativas_marcar_diagnostico(idx_pendencias_pos,\n'
        '      completo=isTRUE(get0("MONITORA_OCORRENCIAS_DIAGNOSTICAS_INTEGRIDADE_OK", ifnotfound=FALSE, inherits=TRUE)),\n'
        '      espacial=nrow(validacao_espacial_pos)>0L)\n'
        '    monitora_v307_justificativas_registrar_base(registros_corrig, origem="pos_painel", continuar=TRUE)\n'
        '    monitora_pendencias_justificativas_persistir(idx_pendencias_pos, eventos_sessao = NULL)')

    def attach_manifest(text):
        anchor = '  arq_manifesto <- file.path(dir_lin, "manifesto_linhagem.json")'
        prefix = ('  candidato_just <- monitora_v307_justificativas_candidato_linhagem()\n'
                  '  hashes_just <- monitora_v307_justificativas_assinaturas(candidato_just)\n')
        text = unique(text, '  revision <- paste0("rev_", substr(monitora_correcao_hash_texto(c(',
                      prefix + '  revision <- paste0("rev_", substr(monitora_correcao_hash_texto(c(\n'
                      '    hashes_just$justificativas_sha256, hashes_just$justificativas_aliases_sha256,')
        extra = ('  manifesto <- c(manifesto, monitora_v307_justificativas_exportar(\n'
                 '    dir_lin, output_hash, revision, preparado=candidato_just))\n')
        # finalizar declara o caminho após montar a lista; reassinar já o tem.
        if text.startswith("monitora_linhagem_finalizar"):
            return unique(text, anchor, extra + anchor)
        return unique(text, '  jsonlite::write_json(manifesto, arq_manifesto, pretty = TRUE, auto_unbox = TRUE, na = "null")',
                      extra + '  jsonlite::write_json(manifesto, arq_manifesto, pretty = TRUE, auto_unbox = TRUE, na = "null")')

    c.patch_function("monitora_linhagem_finalizar", attach_manifest)
    c.patch_function("monitora_linhagem_reassinar_pos_organizacao", attach_manifest)

    def wrap_region(start_anchor, end_anchor, output, path, label, partial=False):
        anchor_start = c.text.index(start_anchor) + len(start_anchor)
        end = c.text.index(end_anchor, anchor_start)
        body = c.text[anchor_start:end]
        assert body.strip() and "monitora_linhagem_finalizar(" in body, label
        refresh = ('  monitora_linhagem_reassinar_pos_organizacao(' + output + ', contexto="' + label + '")\n')
        if partial:
            refresh = '  if (identical(basename(produto), "registros_corrig.csv")) ' + refresh.lstrip()
        validate = 'monitora_v307_publicacao_validar(' + output + ')'
        if partial:
            validate = 'if (identical(basename(produto), "registros_corrig.csv")) ' + validate + ' else TRUE'
        wrapped = ('\n  monitora_v307_transacao_executar(function(tx) {\n' + body + refresh +
            '  }, destinos=monitora_v307_transacao_destinos(' + output + ', ' + path + '),\n'
            '    preflight=function() monitora_v307_publicacao_preflight(),\n'
            '    validar=function() ' + validate + ',\n'
            '    diagnostico_dir=file.path(MONITORA_LOG_DIR,"publicacao_transacional"))\n')
        c.text = c.text[:anchor_start] + wrapped + c.text[end:]
        c.changes.append(label)

    # Checkpoint contém medições/releitura entre exportação e próxima etapa.
    checkpoint_start = '''  if (identical(basename(produto), "registros_corrig.csv") &&
    exists("monitora_publicacao_aa_exportar_registros_corrig_aprovado", mode = "function")) {'''
    pos = c.text.index(checkpoint_start)
    # A âncora imediatamente anterior termina antes do if a ser protegido.
    before = c.text[:pos]
    checkpoint_anchor = before[before.rfind('\n  if (exists("monitora_perf_registrar_checkpoint"'):]
    assert checkpoint_anchor.count('checkpoint_materializacao_registros_corrig_inicio') == 1
    wrap_region(checkpoint_anchor,
        '  if (isTRUE(get0("MONITORA_GERAR_REGISTROS_VALIDADOS", ifnotfound = FALSE, inherits = TRUE)) &&',
        'MONITORA_OUTPUT_DIR', 'caminho_produto', 'checkpoint_transacional', partial=True)
    wrap_region('  caminho_registros_corrig_preanalises <- file.path(MONITORA_OUTPUT_DIR, "registros_corrig.csv")\n',
        '  if (exists("monitora_pipe_contrato_relatorio_optin_seguro", mode = "function")) {',
        'MONITORA_OUTPUT_DIR', 'caminho_registros_corrig_preanalises', 'pre_analises_transacional')
    wrap_region('  caminho_registros_corrig_export <- file.path("registros_corrig.csv")\n',
        '  if (isTRUE(get0("MONITORA_GERAR_REGISTROS_VALIDADOS", ifnotfound = FALSE, inherits = TRUE)) &&',
        'MONITORA_OUTPUT_DIR', 'caminho_registros_corrig_export', 'exportacao_final_transacional')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--revision", help="Revisão nova; por padrão usa próximo rNN livre")
    build(parser.parse_args().revision)
