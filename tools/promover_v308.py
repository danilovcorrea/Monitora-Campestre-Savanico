"""Promove a candidata auditada alterando exclusivamente seus identificadores.

A autorização desta publicação dispensa novas homologações. Isso não converte
limitações conhecidas em gates aprovados; a decisão deve preservá-las.
"""
from pathlib import Path
import argparse
import hashlib
import json

VERSION = "3.0.8"
BUILD = "v3.0.8-20261113-r01"
CANDIDATE_SHA = "9b1db7075c09eaf3b0c23611b88e1c5fd88f085d506ddcf157eddf146a57f402"
BASELINE_SHA = "db6333c5e275af245e7d8d069f8bdeb259acf1cae4d46de7187b53fe475816d2"
PUBLIC_SHA = "0dcbacc8841d9fd58e5b9fcd3c500589bc6f14ce2f2b0d31c3b3d1eeaf820ca1"
REPLACEMENTS = (
    ("# Versão 3.0.8-dev_r04 —", "# Versão 3.0.8 —"),
    ('MONITORA_SCRIPT_VERSAO <- "3.0.8-dev_r04"', 'MONITORA_SCRIPT_VERSAO <- "3.0.8"'),
    ('MONITORA_SCRIPT_BUILD_ID <- "v3.0.8-dev_r04-20261113"',
     'MONITORA_SCRIPT_BUILD_ID <- "v3.0.8-20261113-r01"'),
)


def sha(raw):
    return hashlib.sha256(raw).hexdigest()


def project(raw):
    assert sha(raw) == CANDIDATE_SHA, "Candidata diferente da auditada."
    raw.decode("utf-8")
    assert b"\r" not in raw, "A referência deve estar em LF."
    public = raw
    for old, new in REPLACEMENTS:
        old, new = old.encode(), new.encode()
        assert public.count(old) == 1
        public = public.replace(old, new)
    reverse = public
    for old, new in reversed(REPLACEMENTS):
        old, new = old.encode(), new.encode()
        assert reverse.count(new) == 1
        reverse = reverse.replace(new, old)
    assert reverse == raw, "A promoção modificou outros bytes."
    assert sha(public) == PUBLIC_SHA, "Hash independente da projeção não confere."
    assert max(len(public), len(public) + public.count(b"\n")) <= 5_000_000
    return public


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--candidate", type=Path, required=True)
    p.add_argument("--decision", type=Path, required=True)
    p.add_argument("--receipt", type=Path, required=True)
    a = p.parse_args()
    decision = json.loads(a.decision.read_text(encoding="utf-8"))
    required = {"publication_authorized": True, "candidate_sha256": CANDIDATE_SHA,
                "new_homologation_requested": False, "full_homologation": False}
    assert all(decision.get(k) == v for k, v in required.items()), "Autorização incompatível."
    assert decision.get("known_limits"), "Limitações devem permanecer documentadas."
    root = Path(__file__).resolve().parents[1]
    public = project(a.candidate.read_bytes())
    names = (f"monitora_campsav_alvo_global_v{VERSION}.R", "monitora_campsav_alvo_global.R",
             "R_monitora_campsav_alvo_global.R", "R/monitora_campsav_alvo_global.R")
    assert (root / "VERSION").read_text().strip() in ("3.0.7", VERSION)
    for name in names:
        path = root / name
        assert not path.exists() or sha(path.read_bytes()) in (BASELINE_SHA, PUBLIC_SHA), name
    for name in names:
        (root / name).write_bytes(public)
    (root / "VERSION").write_text(VERSION + "\n", encoding="utf-8")
    assert all((root / n).read_bytes() == public for n in names)
    receipt = dict(version=VERSION, build=BUILD, candidate_sha256=CANDIDATE_SHA,
        public_sha256=PUBLIC_SHA, bytes_lf=len(public), bytes_crlf=len(public)+public.count(b"\n"),
        maximum_decimal_bytes=5_000_000, reversible_three_identifiers_only=True,
        aliases_identical=list(names), decision_sha256=sha(a.decision.read_bytes()),
        new_homologation_performed=False, full_homologation=False)
    a.receipt.write_text(json.dumps(receipt, ensure_ascii=False, indent=2)+"\n", encoding="utf-8")
    print(json.dumps(receipt, ensure_ascii=False))


if __name__ == "__main__":
    main()
