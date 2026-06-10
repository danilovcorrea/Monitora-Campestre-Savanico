#!/usr/bin/env python3
from pathlib import Path
import re
import csv
import subprocess

root = Path(".")
scripts = sorted(root.glob("*.R"))

def git_first_date(path):
    try:
        out = subprocess.check_output(
            ["git", "log", "--follow", "--format=%ad", "--date=short", "--", str(path)],
            text=True,
            stderr=subprocess.DEVNULL
        ).strip().splitlines()
        return out[-1] if out else ""
    except Exception:
        return ""

def count_pattern(text, pattern):
    return len(re.findall(pattern, text, flags=re.I | re.M))

rows = []
for p in scripts:
    txt = p.read_text(encoding="utf-8", errors="replace")
    lines = txt.splitlines()
    comments = [l for l in lines if l.strip().startswith("#")]
    functions = count_pattern(txt, r"<-\s*function\s*\(")
    n_lines = len(lines)
    n_comments = len(comments)
    long_comments = sum(1 for l in comments if len(l) > 100)
    pt_terms = count_pattern(
        txt,
        r"\b(auditoria|deduplic|linha de base|relatório|estatística|composição|performance|robust|padroniza|compatibilidade|inferencial)\b"
    )
    en_terms = count_pattern(
        txt,
        r"\b(debug|fix|temporary|todo|warning|label|plot|backup|version|test|legacy|helper|workflow)\b"
    )
    ai_style = count_pattern(
        txt,
        r"\b(robust|defensiv|semântic|rastreabilidad|reprodutib|modular|pipeline|inferencial|bootstrap|permutação|Benjamini|Bray-Curtis)\b"
    )
    version_refs = count_pattern(txt, r"\bv[0-9]{1,3}\b|vers[aã]o")
    score = (
        functions * 2
        + long_comments
        + pt_terms * 2
        + ai_style * 3
        + version_refs
        - en_terms
    )
    rows.append({
        "arquivo": p.name,
        "primeira_data_git": git_first_date(p),
        "linhas": n_lines,
        "comentarios": n_comments,
        "comentarios_longos": long_comments,
        "funcoes": functions,
        "termos_pt_metodologicos": pt_terms,
        "termos_ingles_dev": en_terms,
        "sinais_estilo_ia_ou_consolidacao": ai_style,
        "referencias_versao": version_refs,
        "score_mudanca_estilo": score,
    })

outdir = Path("reports")
outdir.mkdir(exist_ok=True)

csv_path = outdir / "diagnostico_fase_ia.csv"
if rows:
    with csv_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

md_path = outdir / "diagnostico_fase_ia.md"
with md_path.open("w", encoding="utf-8") as f:
    f.write("# Diagnóstico de mudança de estilo e possível fase com apoio de IA\n\n")
    f.write("Este relatório usa heurísticas textuais. Ele não comprova uso de IA, mas ajuda a identificar pontos de inflexão no estilo do script.\n\n")
    f.write("| Arquivo | Data Git | Linhas | Comentários | Funções | Sinais consolidação/IA | Score |\n")
    f.write("|---|---:|---:|---:|---:|---:|---:|\n")
    for r in rows:
        f.write(
            f"| {r['arquivo']} | {r['primeira_data_git']} | {r['linhas']} | "
            f"{r['comentarios']} | {r['funcoes']} | "
            f"{r['sinais_estilo_ia_ou_consolidacao']} | {r['score_mudanca_estilo']} |\n"
        )

print("Relatórios gerados:")
print(f"- {csv_path}")
print(f"- {md_path}")
