"""Congela a versão pública somente após os gates finais das três UCs."""
from pathlib import Path
import argparse
import hashlib
import json

parser = argparse.ArgumentParser()
parser.add_argument("gates", type=Path)
args = parser.parse_args()
repo = Path(__file__).resolve().parents[1]
candidate = repo / "monitora_campsav_alvo_global_v3.0.5-rc01.R"
raw = candidate.read_bytes()
sha = hashlib.sha256(raw).hexdigest()
for uc in ("FNB", "PNI", "PNM"):
    gate = json.loads((args.gates / uc / "GATES_FINAIS.json").read_text())
    assert gate["status"] == "PASS", uc
    assert gate["script_sha256"] == sha, (uc, "fonte diferente da homologada")
    assert gate["revisao_visual"] == "APROVADA", uc

replacements = {
    b"# Vers\xc3\xa3o 3.0.5-rc01 \xe2\x80\x94": b"# Vers\xc3\xa3o 3.0.5 \xe2\x80\x94",
    b'MONITORA_SCRIPT_VERSAO <- "3.0.5-rc01"': b'MONITORA_SCRIPT_VERSAO <- "3.0.5"',
    b'MONITORA_SCRIPT_BUILD_ID <- "v3.0.5-rc01-20260929-r12"': b'MONITORA_SCRIPT_BUILD_ID <- "v3.0.5-20260929-r01"',
}
public = raw
for old, new in replacements.items():
    assert public.count(old) == 1, old
    public = public.replace(old, new)
reverse = public
for old, new in replacements.items():
    reverse = reverse.replace(new, old)
assert reverse == raw
assert len(public) < 5_000_000
assert len(public.replace(b"\n", b"\r\n")) < 5_000_000
for name in ("monitora_campsav_alvo_global_v3.0.5.R", "R_monitora_campsav_alvo_global.R",
             "monitora_campsav_alvo_global.R", "R/monitora_campsav_alvo_global.R"):
    (repo / name).write_bytes(public)
result = {"status": "PASS", "versao": "3.0.5", "build": "v3.0.5-20260929-r01",
          "sha256_candidata": sha, "sha256": hashlib.sha256(public).hexdigest(),
          "bytes_lf": len(public), "bytes_crlf": len(public.replace(b"\n", b"\r\n")),
          "alteracoes": "Somente três identificadores de versão/build; reversibilidade byte a byte confirmada."}
(args.gates / "PROMOCAO.json").write_text(json.dumps(result, ensure_ascii=False, indent=2))
print(json.dumps(result, ensure_ascii=False))
