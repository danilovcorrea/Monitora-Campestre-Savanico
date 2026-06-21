from pathlib import Path
import os
import re
import shutil
import hashlib
import zipfile
from datetime import date

repo = Path.cwd()
version = os.environ.get("VERSION_NOVA", "2.4.0")
tag = os.environ.get("TAG_NOVA", f"v{version}")
candidate = Path(os.environ["CANDIDATE"]).expanduser().resolve()

if not candidate.exists():
    raise SystemExit(f"Arquivo candidato não encontrado: {candidate}")

release_dir = repo / "releases" / tag
release_dir.mkdir(parents=True, exist_ok=True)
(repo / "R").mkdir(exist_ok=True)

public_versioned = repo / f"monitora_campsav_alvo_global_v{version}.R"
public_unversioned = repo / "monitora_campsav_alvo_global.R"
public_legacy = repo / "MONITORA_CAMPSAV_Alvo_Global.R"
public_r_dir = repo / "R" / "monitora_campsav_alvo_global.R"
public_r_root = repo / "R_monitora_campsav_alvo_global.R"

release_script = release_dir / f"monitora_campsav_alvo_global_v{version}.R"
release_copy = release_dir / f"release_copy_monitora_campsav_alvo_global_v{version}.R"
release_r_root = release_dir / "R_monitora_campsav_alvo_global.R"

text = candidate.read_text(encoding="utf-8")

# Cabeçalho público e padrões operacionais.
text = re.sub(
    r"### Versão pública:\s*.*",
    f"### Versão pública: v{version}",
    text,
    count=1
)

text = re.sub(
    r"(?m)^MONITORA_MODO_EXECUCAO\s*<-\s*['\"].*?['\"]",
    'MONITORA_MODO_EXECUCAO <- "completo"',
    text,
    count=1
)

text = re.sub(
    r"(?m)^MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES\s*<-\s*['\"].*?['\"]",
    'MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"',
    text,
    count=1
)

# Atualiza o bloco de destaques públicos do script, se existir.
destaques_v240 = f"""### Destaques da versão pública v{version}:
### - Modos de execução para desenvolvimento, validação e execução parcial.
### - Painel de correções com operações semânticas atômicas.
### - Exclusão de COLETAS em lote como operação única auditável.
### - Movimento de formas de vida e substituição de desconhecida com aplicação transacional.
### - Movimento em lote de formas de vida por COLETAS, com relatório de ambiguidades.
### - Limpeza atômica de outras formas de vida.
### - Fila do painel coerente com as ações do usuário.
### - Notificações de início e conclusão para operações demoradas.
### - Travas contra duplo clique e duplicidade semântica.
### - Auditoria de persistência pós-aplicação e pós-exportação.
### - Sincronização final de Encostam/tipo_forma_vida com os campos inferiores finais.
### - Comparação pré/pós-correções robusta a diferenças de classe em relatórios auxiliares.

"""

text = re.sub(
    r"### Destaques da versão pública v[0-9]+\.[0-9]+\.[0-9]+:.*?(?=### Destaques consolidados do script:)",
    destaques_v240,
    text,
    flags=re.S,
)

# Remoção de nomes internos de arquivo e marcas de desenvolvimento.
text = re.sub(
    r"monitora_campsav_alvo_global_v2\.4\.0_candidata[^\s'\"]*\.R",
    f"monitora_campsav_alvo_global_v{version}.R",
    text,
)

text = re.sub(r"_candidata_[A-Za-z0-9_]+", "", text)
text = re.sub(r"_rev[0-9]+", "", text)

# Ajustes editoriais pontuais para comentários públicos.
replacements = {
    "### Na rev11, linhas de MVLOTE ambíguas eram adicionadas ao pós-teste, mas":
        "### Linhas de MVLOTE ambíguas devem ser avaliadas de forma coerente no pré e no pós-teste, pois",
    "### O lab de persistência mostrou que recálculos intermediários podiam deixar":
        "### A auditoria de persistência mostrou que recálculos intermediários podiam deixar",
    "### LAB":
        "### Diagnóstico",
}
for old, new in replacements.items():
    text = text.replace(old, new)

# Falha se comentários públicos ainda contiverem referências internas.
forbidden_comment_patterns = [
    r"candidata",
    r"\brev[0-9]+\b",
    r"_rev[0-9]+",
    r"\blab\b",
    r"vers[aã]o interna",
    r"arquivo candidato",
    r"debug tempor[aá]rio",
]

bad_comments = []
for i, line in enumerate(text.splitlines(), 1):
    stripped = line.strip()
    if stripped.startswith("#"):
        low = stripped.lower()
        if any(re.search(p, low, flags=re.I) for p in forbidden_comment_patterns):
            bad_comments.append((i, line))

if bad_comments:
    print("Comentários públicos ainda contêm termos internos proibidos:")
    for i, line in bad_comments[:120]:
        print(f"{i}: {line}")
    raise SystemExit("Revise/remova os comentários acima antes da publicação.")

# Grava cópias canônicas.
for path in [
    public_versioned,
    public_unversioned,
    public_legacy,
    public_r_dir,
    public_r_root,
    release_script,
    release_copy,
    release_r_root,
]:
    path.write_text(text, encoding="utf-8")

(repo / "VERSION").write_text(version + "\n", encoding="utf-8")

release_notes = f"""# {tag} - Modos de execução e operações atômicas no painel

Publicação pública `{tag}` do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Inclui `MONITORA_MODO_EXECUCAO` como chave central para alternar entre execução completa, execução sem PNG, estatísticas sem gráficos, parada controlada após `registros_corrig.csv` e fluxo `painel_e_parar`.
- Mantém como padrão público `MONITORA_MODO_EXECUCAO <- "completo"` e `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.
- Adiciona operações semânticas atômicas no painel para exclusão de COLETAS, movimento de formas de vida, substituição de desconhecida, limpeza de outras formas de vida e movimento em lote de formas de vida por COLETAS.
- Reduz a fragmentação da fila do painel: ações do usuário são exibidas como operações semânticas, enquanto itens técnicos permanecem disponíveis para auditoria.
- Adiciona notificações de início e conclusão para operações potencialmente demoradas, com trava contra duplo clique e bloqueio de duplicidade semântica.
- Implementa movimento em lote de formas de vida para mover formas entre nativa, exótica e seca/morta, migrando automaticamente apenas ocorrências seguras e preservando casos ambíguos em relatório específico.
- Acrescenta auditorias de persistência pós-aplicação e pós-exportação para garantir que as correções persistam em `registros_corrig.csv`.
- Implementa sincronização final de `Encostam`/`tipo_forma_vida` como contrato derivado dos campos inferiores finais.
- Reforça a comparação pré/pós-correções com normalização defensiva de tipos em relatórios auxiliares, evitando falhas de `rbindlist()` por classes divergentes em campos contextuais.
- Mantém relatórios de ambiguidades com campos de localização: EA, UA, ciclo, campanha, ano, data, ponto amostral e ponto metro.

## Validação operacional

A versão foi validada com execução parcial `painel_e_parar`, incluindo:

- exclusões atômicas de COLETAS;
- movimento em lote de formas de vida;
- substituição de forma desconhecida;
- limpeza atômica de outras formas de vida;
- sincronização final de `Encostam`;
- auditorias pós-aplicação e pós-exportação sem falhas;
- exportação de `output/registros_corrig.csv` com persistência confirmada.

## Arquivos principais

- `monitora_campsav_alvo_global_v{version}.R`
- `monitora_campsav_alvo_global.R`
- `MONITORA_CAMPSAV_Alvo_Global.R`
- `R/monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
"""

release_notes_path = release_dir / f"RELEASE_NOTES_{tag}.md"
release_notes_path.write_text(release_notes, encoding="utf-8")

readme = repo / "README.md"
old_readme = readme.read_text(encoding="utf-8") if readme.exists() else "# Monitora-Campestre-Savanico\n"

current_block = f"""<!-- MONITORA_RELEASE_ATUAL_START -->
## Versão atual

**Versão pública atual:** `{tag}`

**Script principal:** `monitora_campsav_alvo_global_v{version}.R`

A versão `{tag}` consolida os modos de execução para desenvolvimento, execução parcial e validação do painel, além de operações atômicas de curadoria com auditoria de persistência no objeto e no `registros_corrig.csv` exportado.

### Modos de execução

- `completo`: executa todo o pipeline.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` sem abrir o painel e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.

O padrão público permanece:

- `MONITORA_MODO_EXECUCAO <- "completo"`
- `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`

### Principais recursos da versão `{tag}`

- Painel com operações semânticas atômicas e fila coerente com as ações do usuário.
- Exclusão atômica de COLETAS em lote.
- Movimento atômico de formas de vida e substituição assistida de forma desconhecida.
- Movimento em lote de formas de vida por COLETAS, com migração segura e relatório de ambiguidades.
- Limpeza atômica de outras formas de vida.
- Notificações de início e conclusão para operações demoradas.
- Trava contra duplo clique e duplicidade semântica.
- Auditoria de persistência pós-aplicação e pós-exportação.
- Sincronização final de `Encostam`/`tipo_forma_vida` com os campos inferiores finais.
- Comparação pré/pós-correções robusta a diferenças de classe em campos contextuais.

<!-- MONITORA_RELEASE_ATUAL_END -->
"""

if "<!-- MONITORA_RELEASE_ATUAL_START -->" in old_readme and "<!-- MONITORA_RELEASE_ATUAL_END -->" in old_readme:
    new_readme = re.sub(
        r"<!-- MONITORA_RELEASE_ATUAL_START -->.*?<!-- MONITORA_RELEASE_ATUAL_END -->",
        current_block.strip(),
        old_readme,
        flags=re.S,
    )
else:
    lines = old_readme.splitlines()
    if lines and lines[0].startswith("#"):
        new_readme = lines[0] + "\n\n" + current_block.strip() + "\n\n" + "\n".join(lines[1:]).lstrip()
    else:
        new_readme = current_block.strip() + "\n\n" + old_readme

readme.write_text(new_readme, encoding="utf-8")

changelog = repo / "CHANGELOG.md"
old_changelog = changelog.read_text(encoding="utf-8") if changelog.exists() else "# Changelog\n"

entry = f"""## [{tag}] - {date.today().isoformat()}

### Adicionado
- `MONITORA_MODO_EXECUCAO` com modos `completo`, `sem_png`, `estatisticas_sem_graficos`, `ate_registros_corrig` e `painel_e_parar`.
- Operações semânticas atômicas para exclusão de COLETAS, movimento de formas de vida, substituição de desconhecida, limpeza de outras formas e movimento em lote de formas de vida.
- Movimento em lote de formas de vida por COLETAS com migração segura e relatório de ambiguidades.
- Notificações de início/conclusão e trava contra duplo clique no painel.
- Auditoria de persistência pós-aplicação e pós-exportação.
- Sincronização final de `Encostam`/`tipo_forma_vida` a partir dos campos inferiores finais.

### Alterado
- Fila do painel passa a exibir operações semânticas coerentes com as ações do usuário.
- Relatórios de comparação pré/pós-correções normalizam tipos auxiliares antes de `rbindlist()`.
- Comentários internos do script foram revisados para remover referências a candidatas, revisões internas e justificativas interlocutórias.

### Corrigido
- Persistência de operações atômicas em `registros_corrig.csv`.
- Divergências reais de `Encostam` após correções sobrepostas.
- Falhas de `rbindlist()` por classes divergentes em relatórios auxiliares.
- Continuação silenciosa quando o painel encerra sem ação explícita.

"""

if f"## [{tag}]" not in old_changelog and f"## {tag}" not in old_changelog:
    if old_changelog.startswith("# Changelog"):
        changelog.write_text(
            "# Changelog\n\n" + entry + old_changelog.replace("# Changelog", "", 1).lstrip(),
            encoding="utf-8"
        )
    else:
        changelog.write_text(entry + "\n" + old_changelog, encoding="utf-8")

for src in [repo / "README.md", repo / "CHANGELOG.md", repo / "VERSION"]:
    shutil.copy2(src, release_dir / src.name)

sha_targets = [
    public_versioned,
    public_unversioned,
    public_legacy,
    public_r_dir,
    public_r_root,
    release_script,
    release_copy,
    release_r_root,
    repo / "README.md",
    repo / "CHANGELOG.md",
    repo / "VERSION",
    release_notes_path,
]

sha_lines = []
for path in sha_targets:
    data = path.read_bytes()
    sha_lines.append(f"{hashlib.sha256(data).hexdigest()}  {path.relative_to(repo).as_posix()}")

sha_path = repo / "SHA256SUMS.txt"
sha_path.write_text("\n".join(sha_lines) + "\n", encoding="utf-8")
shutil.copy2(sha_path, release_dir / "SHA256SUMS.txt")

zip_path = release_dir / f"Monitora-Campestre-Savanico_{tag}_release.zip"
with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    for path in sha_targets + [sha_path]:
        if path.exists():
            zf.write(path, path.relative_to(repo).as_posix())

# Verificação final contra termos internos no script público.
script_public = public_versioned.read_text(encoding="utf-8")
whole_script_forbidden = [
    r"candidata",
    r"_rev[0-9]+",
    r"\brev[0-9]+\b",
    r"\blab\b",
    r"vers[aã]o interna",
]
bad_terms = []
for pattern in whole_script_forbidden:
    if re.search(pattern, script_public, flags=re.I):
        bad_terms.append(pattern)

if bad_terms:
    raise SystemExit("Termos internos remanescentes no script público: " + ", ".join(bad_terms))

print("Preparação concluída.")
print(f"Versão: {tag}")
print(f"Script público: {public_versioned}")
print(f"Release dir: {release_dir}")
print(f"Release notes: {release_notes_path}")
print(f"ZIP: {zip_path}")
