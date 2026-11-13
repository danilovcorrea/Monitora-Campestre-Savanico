"""Candidata autônoma, derivada da release pública v3.0.7 por patches conferidos."""
from pathlib import Path
import argparse
import base64
import hashlib
import importlib.util
import json
import lzma
import re
import subprocess
import tempfile
import textwrap

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / "monitora_campsav_alvo_global_v3.0.7.R"
BASE_SHA = "db6333c5e275af245e7d8d069f8bdeb259acf1cae4d46de7187b53fe475816d2"
spec = importlib.util.spec_from_file_location("v307_builder", ROOT / "tools/v307/build.py")
helpers = importlib.util.module_from_spec(spec)
spec.loader.exec_module(helpers)


def build(revision="r04"):
    assert re.fullmatch(r"r[0-9]{2,}", revision)
    raw = BASE.read_bytes()
    assert hashlib.sha256(raw).hexdigest() == BASE_SHA, "Baseline pública divergente."
    c = helpers.Candidate(raw.decode("utf-8"))
    version = "3.0.8-dev_" + revision
    c.replace("# Versão 3.0.7 —", f"# Versão {version} —")
    c.replace('MONITORA_SCRIPT_VERSAO <- "3.0.7"', f'MONITORA_SCRIPT_VERSAO <- "{version}"')
    c.replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.7-20261112-r01"',
              f'MONITORA_SCRIPT_BUILD_ID <- "v{version}-20261113"')

    # Patches de integração explícitos; o módulo é instalado no mesmo ponto legado.
    patcher = ROOT / "tools/v308/patches.py"
    if patcher.exists():
        ps = importlib.util.spec_from_file_location("v308_patches", patcher)
        pp = importlib.util.module_from_spec(ps)
        ps.loader.exec_module(pp)
        pp.apply(c, helpers)
    else:
        raise AssertionError("Patches de integração ainda não disponíveis.")

    start = c.text.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(')
    begin = c.text.index("\n", start) + 1
    payload = re.match(r'(?:"[A-Za-z0-9+/=]+",?\n)+', c.text[begin:])
    assert payload
    old_module = lzma.decompress(base64.b64decode("".join(re.findall('"([^"]+)"', payload[0])))).decode("utf-8")
    m = helpers.Candidate(old_module)
    first, _, last = helpers.function_span(m.text, "monitora_v305_figura_evidencia")
    m.text = m.text[:first] + m.text[last:]
    sources = [ROOT / "tools/v308" / f for f in ("evidencia.R", "integracao_figuras.R")]
    additions = "\n".join(p.read_text(encoding="utf-8") for p in sources)

    # Como na v307: guarda a AST e avalia exatamente na posição original.
    name = "monitora_relatorios_analiticos_gerar"
    a, _, z = helpers.function_span(c.text, name)
    definition = c.text[a:z]
    replacement = "base::eval(MONITORA_V308_DEFINICAO_RELATORIO, envir=environment())"
    # A função ocorre depois do módulo; sua compactação não altera os offsets acima.
    assert a > begin + len(payload[0])
    c.text = c.text[:a] + replacement + c.text[z:]
    module = m.text + "\n" + additions + "\nMONITORA_V308_DEFINICAO_RELATORIO <- quote({\n" + definition + "\n})\n"
    clean = helpers.module_without_comments(module)  # igualdade AST obrigatória
    encoded = base64.b64encode(lzma.compress(clean.encode("utf-8"), preset=9)).decode("ascii")
    chunks = ",\n".join('"' + part + '"' for part in textwrap.wrap(encoded, 12000)) + "\n"
    c.text = c.text[:begin] + chunks + c.text[begin + len(payload[0]):]
    result = c.text.encode("utf-8")
    crlf_size = len(result) + result.count(b"\n")
    assert max(len(result), crlf_size) <= 5_000_000, (len(result), crlf_size)
    with tempfile.TemporaryDirectory(prefix="monitora_v308_parse_") as t:
        p = Path(t) / "candidate.R"
        p.write_bytes(result)
        r = subprocess.run(["Rscript", "--vanilla", "-e",
            'p<-parse(commandArgs(TRUE)[1],keep.source=FALSE,encoding="UTF-8");stopifnot(length(p)==1L,identical(p[[1]][[1]],quote(base::evalq)))', str(p)],
            capture_output=True, text=True)
        assert r.returncode == 0, r.stderr
    target = ROOT / f"monitora_campsav_alvo_global_v{version}.R"
    target.write_bytes(result)
    receipt = dict(version=version, baseline=BASE.name, baseline_sha256=BASE_SHA,
        candidate=target.name, sha256=hashlib.sha256(result).hexdigest(), bytes_lf=len(result),
        bytes_crlf=crlf_size, maximum_bytes=5_000_000, compression_ast_equal=True,
        source_sha256={str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},
        patches=c.changes, public_aliases_changed=False)
    for name in ("BUILD.json", f"BUILD_{revision}.json"):
        (ROOT / "tools/v308" / name).write_text(json.dumps(receipt, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(receipt, ensure_ascii=False))
    return target


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--revision", default="r04")
    build(parser.parse_args().revision)
