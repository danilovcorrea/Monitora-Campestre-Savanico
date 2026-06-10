#!/usr/bin/env python3
"""
Revisão editorial e versionamento público dos scripts R do repositório
Monitora-Campestre-Savanico.

Modos:
  audit: apenas gera relatório de comentários e versionamento.
  apply: aplica revisão determinística, opcionalmente assistida por IA, e atualiza VERSION/CHANGELOG.

A revisão é conservadora: por padrão, altera apenas comentários e metadados textuais,
sem modificar código ativo.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import os
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

ROOT_IGNORE = {
    ".git", ".github", "renv", ".Rproj.user", "input", "output", "extracted", "log", "logs", "reports"
}

COMMENT_POLICY = """
Reescreva o comentário em português do Brasil, com linguagem técnica clara, objetiva e útil para usuários e desenvolvimento.
Preserve nomes de objetos, funções, arquivos, pacotes, colunas e siglas.
Remova referências comparativas a versões internas, testes antigos ou correções anteriores.
Não adicione informação metodológica que não esteja no texto original.
Retorne apenas o comentário reescrito, sem prefixo #.
""".strip()

COMMON_REPLACEMENTS = [
    (r"\bremove objects\b", "remove objetos temporários"),
    (r"\bkml export\b", "exportação KML"),
    (r"\blabels? to names correction\b", "correção de rótulos para nomes"),
    (r"\bplot\b", "gráfico"),
    (r"\bplots\b", "gráficos"),
    (r"\bcheck\b", "verificação"),
    (r"\bdata frame\b", "tabela de dados"),
    (r"\bdataframe\b", "tabela de dados"),
    (r"\binput files?\b", "arquivos de entrada"),
    (r"\boutput files?\b", "arquivos de saída"),
    (r"\bbaseline\b", "linha de base"),
    (r"\bsummary\b", "resumo"),
    (r"\bstatistics\b", "estatísticas"),
    (r"\bstats\b", "estatísticas"),
    (r"\bperformance\b", "performance"),
    (r"\bmemory\b", "memória"),
    (r"\bdebug\b", "depuração"),
]

VERSION_NOISE_PATTERNS = [
    r"\bv\d+\b.*\b(corrig|ajust|vers|script|anterior|nova|novo|fix|test)",
    r"\b(corrig|ajust|alterad|implementad).{0,40}\bv\d+\b",
    r"\bchatgpt\b",
    r"\bvers[aã]o anterior\b",
    r"\bteste antigo\b",
]

HEADER_RE = re.compile(r"^\s*#+\s*(Vers[aã]o|Versão pública|Version)\s*:", re.IGNORECASE)

@dataclass
class CommentRecord:
    path: str
    line: int
    kind: str
    text: str
    issue: str


def iter_r_files(root: Path) -> Iterable[Path]:
    for path in root.rglob("*.R"):
        parts = set(path.relative_to(root).parts)
        if parts & ROOT_IGNORE:
            continue
        yield path


def split_code_comment(line: str) -> Tuple[str, Optional[str]]:
    """Divide linha R em código e comentário, ignorando # dentro de strings simples/duplas."""
    in_single = False
    in_double = False
    escape = False
    for i, ch in enumerate(line):
        if escape:
            escape = False
            continue
        if ch == "\\":
            escape = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "#" and not in_single and not in_double:
            return line[:i], line[i:]
    return line, None


def detect_comment_issues(text: str) -> List[str]:
    low = text.lower()
    issues = []
    if re.search(r"\b(the|this|that|remove|check|export|labels?|plot|baseline|summary|statistics|memory|debug)\b", low):
        issues.append("possível comentário em inglês")
    if any(re.search(p, low) for p in VERSION_NOISE_PATTERNS):
        issues.append("referência interna de versão/teste")
    if "TODO" in text or "FIXME" in text:
        issues.append("marcador técnico pendente")
    if len(text.strip()) > 180:
        issues.append("comentário longo")
    return issues


def deterministic_rewrite(comment_body: str) -> str:
    original = comment_body.strip()
    text = original

    low = text.lower()
    if any(re.search(p, low) for p in VERSION_NOISE_PATTERNS):
        return ""

    for pattern, repl in COMMON_REPLACEMENTS:
        text = re.sub(pattern, repl, text, flags=re.IGNORECASE)

    text = re.sub(r"\s+", " ", text).strip()
    text = re.sub(r"\bv(\d+)\b", r"versão interna \1", text, flags=re.IGNORECASE)
    return text


def ai_rewrite_comment(comment_body: str) -> str:
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        return deterministic_rewrite(comment_body)
    try:
        from openai import OpenAI
        client = OpenAI(api_key=api_key)
        resp = client.responses.create(
            model="gpt-4.1-mini",
            input=[
                {"role": "system", "content": COMMENT_POLICY},
                {"role": "user", "content": comment_body.strip()},
            ],
            temperature=0.1,
        )
        out = resp.output_text.strip()
        out = re.sub(r"^#+\s*", "", out).strip()
        if any(re.search(p, out.lower()) for p in VERSION_NOISE_PATTERNS):
            return deterministic_rewrite(out)
        return out
    except Exception:
        return deterministic_rewrite(comment_body)


def rewrite_file_comments(path: Path, use_ai: bool) -> bool:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
    changed = False
    new_lines: List[str] = []

    for line in lines:
        newline = "\n" if line.endswith("\n") else ""
        raw = line[:-1] if newline else line
        code, comment = split_code_comment(raw)

        if comment is None:
            new_lines.append(line)
            continue

        # Só altera comentários em linha própria. Comentários inline são preservados,
        # salvo substituições determinísticas simples, para evitar alterar código.
        if code.strip():
            body = comment.lstrip("#").strip()
            revised = deterministic_rewrite(body)
            if revised and revised != body:
                prefix = comment[: len(comment) - len(comment.lstrip("#"))]
                new_lines.append(f"{code}{prefix} {revised}{newline}")
                changed = True
            else:
                new_lines.append(line)
            continue

        prefix_ws = raw[: len(raw) - len(raw.lstrip())]
        hash_count = len(comment) - len(comment.lstrip("#"))
        hashes = "#" * max(hash_count, 1)
        body = comment.lstrip("#").strip()

        if not body:
            new_lines.append(line)
            continue

        revised = ai_rewrite_comment(body) if use_ai else deterministic_rewrite(body)
        if revised == "":
            changed = True
            continue
        if revised != body:
            new_lines.append(f"{prefix_ws}{hashes} {revised}{newline}")
            changed = True
        else:
            new_lines.append(line)

    if changed:
        path.write_text("".join(new_lines), encoding="utf-8")
    return changed


def audit_comments(root: Path, out_dir: Path) -> List[CommentRecord]:
    records: List[CommentRecord] = []
    for path in sorted(iter_r_files(root)):
        rel = path.relative_to(root).as_posix()
        for idx, line in enumerate(path.read_text(encoding="utf-8", errors="replace").splitlines(), start=1):
            code, comment = split_code_comment(line)
            if not comment:
                continue
            body = comment.lstrip("#").strip()
            if not body:
                continue
            kind = "inline" if code.strip() else "linha"
            issues = detect_comment_issues(body)
            for issue in issues:
                records.append(CommentRecord(rel, idx, kind, body[:300], issue))

    out_dir.mkdir(parents=True, exist_ok=True)
    csv_path = out_dir / "auditoria_comentarios.csv"
    with csv_path.open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["arquivo", "linha", "tipo", "problema", "comentario"])
        for r in records:
            w.writerow([r.path, r.line, r.kind, r.issue, r.text])

    md_path = out_dir / "auditoria_comentarios.md"
    by_file = {}
    for r in records:
        by_file.setdefault(r.path, 0)
        by_file[r.path] += 1
    with md_path.open("w", encoding="utf-8") as f:
        f.write("# Auditoria de comentários dos scripts R\n\n")
        f.write(f"Data da auditoria: {dt.date.today().isoformat()}\n\n")
        f.write(f"Total de ocorrências: {len(records)}\n\n")
        f.write("## Ocorrências por arquivo\n\n")
        for path, count in sorted(by_file.items()):
            f.write(f"- `{path}`: {count}\n")
        f.write("\n## Critérios verificados\n\n")
        f.write("- possível comentário em inglês;\n")
        f.write("- referência interna a versões/testes;\n")
        f.write("- marcadores técnicos pendentes;\n")
        f.write("- comentários longos que podem exigir revisão manual.\n")
    return records


def ensure_version_files(root: Path, public_version: str) -> None:
    version = public_version.strip().removeprefix("v")
    (root / "VERSION").write_text(f"{version}\n", encoding="utf-8")

    changelog = root / "CHANGELOG.md"
    today = dt.date.today().isoformat()
    header = f"## [v{version}] - {today}"
    if changelog.exists():
        txt = changelog.read_text(encoding="utf-8", errors="replace")
        if header not in txt and f"[v{version}]" not in txt:
            insert = f"""
{header}

### Adicionado

- Adoção de versionamento público semântico.
- Revisão editorial automatizada dos comentários dos scripts R.
- Relatório de auditoria de comentários em `reports/`.

### Alterado

- Padronização dos cabeçalhos de versionamento dos scripts.
- Organização da documentação de versão pública.

### Observações

- Versões anteriores datadas permanecem preservadas como histórico pré-versionamento semântico.

""".lstrip()
            txt = re.sub(r"(# Changelog\s*)", r"\1\n" + insert + "\n", txt, count=1)
            changelog.write_text(txt, encoding="utf-8")
    else:
        changelog.write_text(f"""# Changelog

{header}

### Adicionado

- Adoção de versionamento público semântico.
- Revisão editorial dos comentários dos scripts R.
- Relatório de auditoria de comentários.

### Observações

- Versões anteriores datadas permanecem preservadas como histórico pré-versionamento semântico.
""", encoding="utf-8")


def update_headers(root: Path, public_version: str, current_script: str = "") -> None:
    version = public_version.strip().removeprefix("v")
    current_path = (root / current_script).resolve() if current_script else None

    for path in iter_r_files(root):
        rel = path.relative_to(root).as_posix()
        is_current = current_path is not None and path.resolve() == current_path
        text = path.read_text(encoding="utf-8", errors="replace")
        lines = text.splitlines()

        # Remove linhas antigas explícitas de versão no cabeçalho inicial, mas apenas nas 40 primeiras linhas.
        head = []
        body_start = 0
        for i, line in enumerate(lines[:40]):
            if HEADER_RE.search(line):
                continue
            head.append(line)
            body_start = i + 1
        rest = lines[body_start:]

        if is_current:
            block = [
                "### Programa Monitora - Componente Campestre Savânico",
                "### Script: Alvo Global - tratamento, análise e visualização de dados",
                f"### Versão pública: v{version}",
                f"### Data da versão: {dt.date.today().isoformat()}",
                "### Status: versão pública estável",
                "",
            ]
        else:
            block = [
                "### Programa Monitora - Componente Campestre Savânico",
                "### Script histórico preservado para rastreabilidade",
                "### Versão pública: pré-versionamento semântico",
                "### Status: histórico / não recomendado para novas análises",
                "",
            ]

        # Evita duplicar bloco se já existir.
        if "Versão pública:" not in "\n".join(lines[:20]):
            new_text = "\n".join(block + lines) + "\n"
            path.write_text(new_text, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--mode", choices=["audit", "apply"], default="audit")
    parser.add_argument("--public-version", default="2.0.0")
    parser.add_argument("--current-script", default="")
    parser.add_argument("--use-ai", action="store_true")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    reports = root / "reports"

    records = audit_comments(root, reports)

    if args.mode == "apply":
        ensure_version_files(root, args.public_version)
        update_headers(root, args.public_version, args.current_script)
        changed = []
        for path in sorted(iter_r_files(root)):
            if rewrite_file_comments(path, args.use_ai):
                changed.append(path.relative_to(root).as_posix())
        (reports / "arquivos_alterados.txt").write_text("\n".join(changed) + "\n", encoding="utf-8")
        audit_comments(root, reports)

    print(f"Arquivos R avaliados: {sum(1 for _ in iter_r_files(root))}")
    print(f"Ocorrências registradas: {len(records)}")
    print(f"Relatórios em: {reports}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
