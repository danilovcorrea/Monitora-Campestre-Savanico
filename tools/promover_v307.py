"""Prepara a projeção pública e só materializa os aliases após parecer aprovado.

A preparação padrão grava somente no staging. O parecer pertence à revisão
coordenadora e deve declarar explicitamente o aceite da rodada manual, a revisão
independente e a inexistência de gates impeditivos restantes.
"""

from pathlib import Path
import argparse
import hashlib
import json


VERSION = "3.0.7"
BUILD = "v3.0.7-20261112-r01"
CANDIDATE_SHA256 = "1e9553262e04592b6f7dfdb0634641fca734a818d1a31fc464301cf9f639cf99"
BASELINE_SHA256 = "454cb8f1d6f74ec8df2695236add1206080f869a3209c8a09aa92af9b6186d45"
LIMIT = 5_000_000
REPLACEMENTS = (
    ("# Versão 3.0.7-dev_r09 —", "# Versão 3.0.7 —"),
    ('MONITORA_SCRIPT_VERSAO <- "3.0.7-dev_r09"', 'MONITORA_SCRIPT_VERSAO <- "3.0.7"'),
    ('MONITORA_SCRIPT_BUILD_ID <- "v3.0.7-dev_r09-20261112"',
     'MONITORA_SCRIPT_BUILD_ID <- "v3.0.7-20261112-r01"'),
)


def sha256(raw):
    return hashlib.sha256(raw).hexdigest()


def project(raw):
    if sha256(raw) != CANDIDATE_SHA256:
        raise ValueError("A candidata difere do SHA256 aprovado para preparação.")
    raw.decode("utf-8")
    if b"\r" in raw:
        raise ValueError("A candidata de referência deve ter terminações LF.")
    public = raw
    for old, new in REPLACEMENTS:
        old, new = old.encode("utf-8"), new.encode("utf-8")
        if public.count(old) != 1:
            raise ValueError("Identificador ausente ou ambíguo na candidata.")
        public = public.replace(old, new)
    reverse = public
    for old, new in reversed(REPLACEMENTS):
        reverse = reverse.replace(new.encode("utf-8"), old.encode("utf-8"), 1)
    if reverse != raw:
        raise ValueError("A reversão dos três identificadores alterou outros bytes.")
    if len(public) > LIMIT or len(public.replace(b"\n", b"\r\n")) > LIMIT:
        raise ValueError("O arquivo excede 5.000.000 bytes em LF ou CRLF.")
    return public


def validate_decision(path):
    decision = json.loads(path.read_text(encoding="utf-8"))
    required = {
        "status": "PASS",
        "candidate_sha256": CANDIDATE_SHA256,
        "promotion_approved": True,
        "manual_APAI_approved": True,
        "independent_review": "PASS",
        "blocking_gates_remaining": [],
    }
    for key, expected in required.items():
        if key not in decision or decision[key] != expected:
            raise ValueError(f"Parecer não autoriza a promoção: {key}.")
    return sha256(path.read_bytes())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--staging-dir", type=Path)
    parser.add_argument("--promote", type=Path, metavar="PARECER_JSON",
                        help="Materializa versão e aliases somente com parecer PASS explícito.")
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[1]
    staging = args.staging_dir or repo / ".preparacao_v307"
    if staging.resolve() == repo:
        raise ValueError("A preparação exige um diretório de staging separado.")
    public = project(args.candidate.read_bytes())
    destination = staging / "public" / f"monitora_campsav_alvo_global_v{VERSION}.R"
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(public)
    result = {
        "status": "PROJECAO_PREPARADA_SEM_PROMOCAO",
        "version": VERSION,
        "build": BUILD,
        "candidate_sha256": CANDIDATE_SHA256,
        "public_sha256": sha256(public),
        "bytes_lf": len(public),
        "bytes_crlf": len(public.replace(b"\n", b"\r\n")),
        "decimal_limit": LIMIT,
        "reversible_three_identifiers_only": True,
        "public_version_applied": False,
    }
    if args.promote is not None:
        decision_sha = validate_decision(args.promote)
        names = (f"monitora_campsav_alvo_global_v{VERSION}.R",
                 "monitora_campsav_alvo_global.R", "R_monitora_campsav_alvo_global.R",
                 "R/monitora_campsav_alvo_global.R")
        for name in names:
            path = repo / name
            if path.exists() and sha256(path.read_bytes()) not in (BASELINE_SHA256, sha256(public)):
                raise ValueError(f"Destino tem bytes não previstos; preservado: {name}.")
        previous_version = (repo / "VERSION").read_text(encoding="utf-8").strip()
        if previous_version not in ("3.0.6", VERSION):
            raise ValueError("VERSION não corresponde à baseline ou à projeção.")
        for name in names:
            path = repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(public)
        (repo / "VERSION").write_text(VERSION + "\n", encoding="utf-8")
        if any((repo / name).read_bytes() != public for name in names):
            raise ValueError("As cópias públicas não são idênticas.")
        result.update({"status": "PROMOCAO_LOCAL_APLICADA", "public_version_applied": True,
                       "decision_sha256": decision_sha, "identical_aliases": list(names)})
    (staging / "PROJECAO_PUBLICA.json").write_text(
        json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(result, ensure_ascii=False))


if __name__ == "__main__":
    main()
