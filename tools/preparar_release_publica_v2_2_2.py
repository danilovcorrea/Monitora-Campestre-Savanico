#!/usr/bin/env python3
from pathlib import Path
import argparse
import datetime
import re

def read(path):
    return Path(path).read_text(encoding="utf-8-sig", errors="replace")

def write(path, text):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text.rstrip() + "\n", encoding="utf-8", newline="\n")

def replace_section(md, header, body):
    pattern = rf"(^## {re.escape(header)}\n)(.*?)(?=^## |\Z)"
    repl = rf"\1\n{body.strip()}\n"
    new, n = re.subn(pattern, repl, md, flags=re.M | re.S)
    if n == 0:
        new = md.rstrip() + f"\n\n## {header}\n\n{body.strip()}\n"
    return new

def clean_script(txt, tag):
    txt = txt.replace("\r\n", "\n").replace("\r", "\n")

    txt = re.sub(
        r"### Versão pública: v[^\n]+",
        f"### Versão pública: {tag}",
        txt,
        count=1,
    )

    txt = re.sub(
        r"### Destaques da versão pública v[^\n]+:",
        f"### Destaques da versão pública {tag}:",
        txt,
        count=1,
    )

    replacements = {
        "v2.2.2-candidata-integridade-perf16": tag,
        "v2.2.2-candidata": tag,
        "candidata_integridade_perf16_habito_condicional": tag.replace(".", "_"),
        "### Função mantida apenas por compatibilidade interna/legada.\n":
            "### Função preservada para compatibilidade com chamadas existentes.\n",
        "### Compatibilidade com versões anteriores: a v2.2.1 não lê XLSForms externos.\n### Todas as regras utilizadas pelo painel são carregadas da seção embutida acima.\n":
            "### As regras utilizadas pelo painel são carregadas dos metadados XLSForm embutidos.\n",
        "### O progresso usa marcos absolutos ponderados pelo tempo observado nas execuções v2.2.x.\n":
            "### O progresso usa marcos absolutos ponderados por etapas típicas de execução.\n",
        "# Compatibilidade: chamadas antigas não alteram denominador nem imprimem mensagens.\n":
            "# Chamadas preservadas não alteram denominador nem imprimem mensagens.\n",
        "# Mantida apenas para compatibilidade com trechos antigos. O progresso real é por marcos absolutos.\n":
            "# O progresso real é controlado por marcos absolutos.\n",
        "## compatibilidade interna e eventual auditoria de nomes legados.\n":
            "## auditoria de nomes legados, quando necessária.\n",
    }

    for old, new in replacements.items():
        txt = txt.replace(old, new)

    txt = re.sub(
        r"\n### Exportação dos arquivos KML\.\n"
        r"### Método robusto e rápido: gravação manual de KML\.\n"
        r"### Justificativa:\n"
        r"### - evita data\.table::rbindlist\(\) sobre colunas sfc/sfg com classes diferentes;\n"
        r"### - evita sf::st_write\(\) para geometria mista POINT \+ LINESTRING;\n"
        r"### - reduz overhead de GDAL para produtos KML simples\.\n",
        "\n### Exportação dos arquivos KML.\n"
        "### Gravação manual de KML para produtos simples com geometrias de ponto e linha.\n",
        txt,
        flags=re.S,
    )

    return txt

def update_readme(root, version, tag):
    path = root / "README.md"
    md = read(path) if path.exists() else "# Monitora Campestre-Savânico\n"

    md = replace_section(md, "Versão pública atual", f"""
{tag}

A versão `{tag}` consolida a linha pública posterior à `v2.2.1` e estabiliza o Painel de validação - correções assistidas de `registros_corrig`, com foco em integridade transacional, triagem operacional de formas de vida exóticas, auditoria semântica pré/pós-correção e melhoria de performance.

A versão mantém o relatório de ocorrência de formas de vida exóticas da `v2.2.1`, o Painel de validação e correções assistidas da `v2.2.0`, e o fluxo analítico, estatístico e gráfico consolidado na série `v2.1.x`.
""")

    md = replace_section(md, "Script recomendado para uso", f"""
Use preferencialmente:

* `monitora_campsav_alvo_global.R`: cópia pública atual com nome padronizado em minúsculas.
* `MONITORA_CAMPSAV_Alvo_Global.R`: nome público histórico equivalente.
* `R/monitora_campsav_alvo_global.R`: cópia mantida na estrutura interna do projeto.
* `monitora_campsav_alvo_global_{tag}.R`: cópia versionada da versão pública atual.
* `releases/{tag}/monitora_campsav_alvo_global_{tag}.R`: cópia congelada da versão pública `{tag}`.

Na publicação da `{tag}`, essas cinco cópias públicas do script devem ter conteúdo idêntico.
""")

    md = replace_section(md, f"Principais recursos da {tag}", """
* Validação transacional de grupos de correção no painel.
* Bloqueio integral de grupos quando qualquer operação obrigatória falha.
* Triagem de exóticas por vínculo estrito entre `Encostam`, forma de vida e espécie.
* Separação de registros com `exotica` em `Encostam` sem forma de vida exótica detalhada.
* Tabela unificada de triagem para exóticas, desconhecidas e outras formas de vida.
* Deduplicação imediata no painel e deduplicação defensiva na aplicação.
* Localização acelerada por `linha_indice`.
* Trava de hábito para formas condicionais.
* Mapa canônico estrutural de colunas cacheado.
* Auditoria semântica pré/pós-correção.
* Melhorias de performance, checkpoints, progresso textual e exportação de CSVs.
* Preservação do relatório de ocorrência de formas de vida exóticas.
* Preservação do Painel de validação - correções assistidas.
* Preservação do fluxo estatístico e gráfico consolidado.
""")

    md = replace_section(md, "Uso básico", f"""
1. Clone ou baixe este repositório.
2. Coloque os arquivos de entrada em `input/`.
3. Execute o script completo no RStudio ou por `Rscript`.
4. Consulte os produtos em `output/` e as auditorias em `log/`.

Exemplo:

    Rscript MONITORA_CAMPSAV_Alvo_Global.R

Também é possível executar a versão pública específica:

    Rscript monitora_campsav_alvo_global_{tag}.R
""")

    write(path, md)

def update_changelog(root, version, tag, date):
    path = root / "CHANGELOG.md"
    old = read(path) if path.exists() else "# Changelog\n"

    entry = f"""
## [{tag}] - {date}

### Destaques

- Publica a versão `{tag}` após a `v2.2.1`.
- Estabiliza o Painel de validação - correções assistidas de `registros_corrig`.
- Mantém o relatório de ocorrência de formas de vida exóticas, o painel assistido e o fluxo analítico, estatístico e gráfico consolidado.

### Adicionado

- Pré-validação transacional de grupos de correção.
- Auditoria semântica pré/pós-correção.
- Tabela unificada de triagem do painel.
- Localização acelerada por `linha_indice`.
- Deduplicação defensiva por assinatura semântica.

### Alterado

- Triagem de exóticas passa a exigir vínculo operacional estrito entre `Encostam`, forma de vida e espécie.
- Hábito passa a ser aceito apenas para formas condicionais.
- Mapa canônico estrutural de colunas passa a ser cacheado.
- Comentários do script foram revisados para remover menções a versões internas e comentários interlocutórios.

### Corrigido

- Bloqueio de correções parciais em movimentos assistidos.
- Redução de reintrodução de tokens residuais após movimentos exótica → nativa.
- Tratamento mais seguro de CSVs vazios, warnings de exportação e objetos temporários.
- Melhoria de checkpoints, progresso textual e controle de recursos.
"""

    if f"## [{tag}]" in old:
        new = re.sub(
            rf"## \[{re.escape(tag)}\].*?(?=^## \[|\Z)",
            entry.strip() + "\n\n",
            old,
            flags=re.M | re.S,
        )
    else:
        pos = old.find("## [")
        if pos == -1:
            new = old.rstrip() + "\n\n" + entry.strip() + "\n"
        else:
            new = old[:pos].rstrip() + "\n\n" + entry.strip() + "\n\n" + old[pos:].lstrip()

    write(path, new)

def release_notes(root, version, tag):
    notes = f"""# Release {tag}

Versão pública posterior à `v2.2.1`, com revisão de integridade, auditoria e performance do Painel de validação - correções assistidas de `registros_corrig`.

## Destaques

- Validação transacional de grupos de correção no painel.
- Bloqueio integral de operações quando qualquer etapa obrigatória falha.
- Triagem de exóticas por vínculo estrito entre `Encostam`, forma de vida e espécie.
- Separação de registros com `exotica` em `Encostam` sem forma de vida exótica detalhada.
- Tabela unificada de triagem para exóticas, desconhecidas e outras formas de vida.
- Deduplicação imediata no painel e deduplicação defensiva na aplicação.
- Localização acelerada por `linha_indice`.
- Trava de hábito para formas condicionais.
- Mapa canônico estrutural de colunas cacheado.
- Auditoria semântica pré/pós-correção.
- Melhorias de performance, progresso textual, checkpoints e exportação de CSVs.

## Arquivo principal

monitora_campsav_alvo_global_{tag}.R

## Cópias sincronizadas

monitora_campsav_alvo_global_{tag}.R
monitora_campsav_alvo_global.R
MONITORA_CAMPSAV_Alvo_Global.R
R/monitora_campsav_alvo_global.R
releases/{tag}/monitora_campsav_alvo_global_{tag}.R
"""
    write(root / f"RELEASE_NOTES_{tag}.md", notes)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--candidate", required=True)
    ap.add_argument("--version", required=True)
    ap.add_argument("--date", default=str(datetime.date.today()))
    args = ap.parse_args()

    root = Path.cwd()
    version = args.version
    tag = f"v{version}"

    candidate = Path(args.candidate).expanduser().resolve()
    if not candidate.exists():
        raise SystemExit(f"ERRO: candidato não encontrado: {candidate}")

    script = clean_script(read(candidate), tag)

    targets = [
        root / f"monitora_campsav_alvo_global_{tag}.R",
        root / "monitora_campsav_alvo_global.R",
        root / "MONITORA_CAMPSAV_Alvo_Global.R",
        root / "R" / "monitora_campsav_alvo_global.R",
        root / "releases" / tag / f"monitora_campsav_alvo_global_{tag}.R",
    ]

    for target in targets:
        write(target, script)

    write(root / "VERSION", version)
    update_readme(root, version, tag)
    update_changelog(root, version, tag, args.date)
    release_notes(root, version, tag)

    print(f"Preparação concluída para {tag}")
    print("Arquivos atualizados:")
    for t in targets:
        print(f"  {t}")
    print(f"  {root / 'README.md'}")
    print(f"  {root / 'CHANGELOG.md'}")
    print(f"  {root / 'VERSION'}")
    print(f"  {root / f'RELEASE_NOTES_{tag}.md'}")

if __name__ == "__main__":
    main()
