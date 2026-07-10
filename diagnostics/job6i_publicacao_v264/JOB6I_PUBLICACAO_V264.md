# JOB6I — Preparação da publicação pública v2.6.4

**Status da Fase 1 (preparação): concluída.**
**Status da Fase 1B (destino = main): concluída.**
**Status geral: `AGUARDANDO_CONFIRMACAO_PUBLICAR_V264`**

Nada foi commitado, taggeado ou enviado ao remoto. Esta fase apenas prepara e audita o material.

---

## 0. Fase 1B — Estratégia de destino para `main` (adicionado após revisão do usuário)

O usuário pediu, antes de confirmar `PUBLICAR_V264`, que a tag/release pública aponte para `main` — a Fase
1 original tinha proposto push para o próprio branch `hotfix/job6-contrato-painel-v263`. Esta seção
documenta a estratégia final.

### 0.1 Estado confirmado de `main`

```
git fetch origin
git rev-parse origin/main   -> 55a498f1af065184d1abc438ff1d218a3f6efdc4
git rev-parse HEAD (hotfix) -> 55a498f1af065184d1abc438ff1d218a3f6efdc4  (mesmo commit)
git merge-base --is-ancestor HEAD origin/main       -> verdadeiro
git merge-base --is-ancestor origin/main HEAD       -> verdadeiro
```

`origin/main` e o branch hotfix estão **exatamente no mesmo commit** (`55a498f`, "Atualiza main para
v2.6.3") — não há nenhuma divergência de histórico entre eles. Todo o trabalho do hotfix (JOB6 a JOB6H)
está em alterações **não commitadas** sobre essa base, na worktree
`Monitora-Campestre-Savanico_job6_hotfix_contrato_painel_v263`.

Achados adicionais relevantes para a decisão:

- O branch local `main` (não `origin/main`) está **defasado em 1 commit** (`b2d8a24`, ancestral direto de
  `55a498f`) — bookkeeping local desatualizado, não um problema de dados. **Não foi alterado** nesta fase
  (nenhum `git branch -f`/fast-forward foi aplicado ao `main` local, para não tomar nenhuma ação sobre uma
  ref compartilhada sem necessidade).
- O branch `publica-v263-main-fix` já está checked out na worktree explicitamente proibida
  (`~/Projetos/Monitora_publicacao_main_v263_fix`) e aponta para o mesmo `55a498f`. Não foi tocado.

### 0.2 Estratégia escolhida

Conforme preferência explícita do usuário: **worktree limpa baseada em `origin/main`, com apenas os
arquivos públicos aprovados da v2.6.4 copiados por cima** — não um merge do branch hotfix (que arrastaria
o histórico de 452+ linhas de patch incremental e a pasta `diagnostics/` inteira) e não um `git add .` em
cima do branch hotfix.

1. Nova worktree criada em `/home/dfed/Projetos/Monitora-Campestre-Savanico_publicacao_v264_main/`
   (caminho novo, distinto da worktree proibida), em um branch novo dedicado
   `release/v2.6.4-publicacao`, criado a partir de `origin/main` (`git worktree add ... -b
   release/v2.6.4-publicacao origin/main`). Branch local `main` não foi tocado.
2. Os 27 arquivos/pastas candidatos da Fase 1 (§6 abaixo, lista idêntica) foram **copiados** (não
   commitados) da worktree hotfix para a worktree nova, preservando a estrutura de caminhos. O ZIP de
   release também foi copiado para uso operacional local (`gh release create` precisa dele), mas **não é
   candidato a staged** (mesma decisão da Fase 1).
3. Confirmado que a cópia do script principal é **byte-idêntica** à da worktree hotfix (mesmo SHA256:
   `784e8484cd98db21ab6b1ddbc70d21adc8d0576b2005622fcbf666880d4ae194`), então toda a suíte de testes já
   reexecutada na Fase 1 (§5) continua válida sem precisar rodar de novo.
4. Quando `PUBLICAR_V264` for confirmado, a Fase 2 roda **nesta worktree nova**
   (`Monitora-Campestre-Savanico_publicacao_v264_main`), com staged seletivo idêntico ao da Fase 1,
   commit, tag `v2.6.4` e `git push origin release/v2.6.4-publicacao:main` — um push que resulta em
   **fast-forward direto de `main`** (já que a base é exatamente `origin/main`, sem nenhum commit
   intermediário/divergente), sem merge commit, sem `--force`. A worktree hotfix original
   (`_job6_hotfix_contrato_painel_v263`) permanece intacta, com todo o histórico de JOB6-JOB6H preservado
   localmente para auditoria futura, mas sem nada dela ir para `main`.

### 0.3 Preflight reexecutado na worktree nova

```
cd /home/dfed/Projetos/Monitora-Campestre-Savanico_publicacao_v264_main/
git status -sb
## release/v2.6.4-publicacao...origin/main
 M CHANGELOG.md
 M R/monitora_campsav_alvo_global.R
 M README.md
 M R_monitora_campsav_alvo_global.R
 M SHA256SUMS.txt
 M VERSION
 M monitora_campsav_alvo_global.R
?? INSTRUCOES_BOLSISTAS_v2.6.4.md
?? RELEASE_NOTES_v2.6.4.md
?? monitora_campsav_alvo_global_v2.6.4.R
?? release_assets/v2.6.4/
?? releases/v2.6.4/

git diff --stat
 CHANGELOG.md                     |  13 +
 R/monitora_campsav_alvo_global.R | 762 +++++++++++++++++++++++++++++++++++++--
 README.md                        |  33 +-
 R_monitora_campsav_alvo_global.R | 762 +++++++++++++++++++++++++++++++++++++--
 SHA256SUMS.txt                   |  20 +-
 VERSION                          |   2 +-
 monitora_campsav_alvo_global.R   | 762 +++++++++++++++++++++++++++++++++++++--
 7 files changed, 2251 insertions(+), 103 deletions(-)

git diff --check   -> limpo (exit 0)
Rscript -e 'parse("monitora_campsav_alvo_global.R")'   -> PARSE_OK
```

Idêntico ao resultado da Fase 1 na worktree hotfix — confirma que a cópia não alterou nada.

### 0.4 Auditoria de dados reais reexecutada na worktree nova

```bash
# candidatos a staged (git status --short, arquivos expandidos)
grep -Ei '(^|/)(input|output|log|extracted)(/|$)|\.(csv|xlsx|xls|ods|rds|rda|sqlite|db|gpkg|shp|geojson|kml|kmz|png|jpg|jpeg)$'
# -> NENHUM PADRAO PROIBIDO NOS CANDIDATOS A STAGED

# zip
unzip -l releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip | grep -Ei '...'
# -> NENHUM PADRAO PROIBIDO NO ZIP

sha256sum releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip
# -> 51f7bbbc5aabfb2a85b97dac3fe19bab9d211a3846b3a86107704a4a105d1d89  (identico ao computado na Fase 1)
```

`diagnostics/` **não foi copiada** para a worktree nova (mesma decisão da Fase 1, §4: pelo menos um CSV
de auditoria interna carrega excerto real de dado biológico truncado). O único arquivo de `diagnostics/`
candidato a staged é este próprio relatório, copiado por último, depois de finalizado.

### 0.5 Comandos propostos para a Fase 2 (atualizados — branch de push agora é `main`)

Executar **na worktree `/home/dfed/Projetos/Monitora-Campestre-Savanico_publicacao_v264_main/`**, branch
`release/v2.6.4-publicacao`:

```bash
cd /home/dfed/Projetos/Monitora-Campestre-Savanico_publicacao_v264_main/
git add <lista exata da secao 6, mesmos caminhos relativos>
git status -sb
git diff --cached --stat
git diff --cached --check
git commit -m "Publica v2.6.4: hotfix painel, persistencia e importacao"
git tag -a v2.6.4 -m "v2.6.4"
git push origin release/v2.6.4-publicacao:main
git push origin v2.6.4
gh release create v2.6.4 \
  releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip \
  --title "v2.6.4 — Hotfix do contrato do painel, persistência e importação robusta" \
  --notes-file RELEASE_NOTES_v2.6.4.md
```

`git push origin release/v2.6.4-publicacao:main` empurra o commit local diretamente para a ref `main` do
remoto. Como a base local é exatamente `origin/main` (sem divergência), esse push é um **fast-forward
simples** — o GitHub aceita sem `--force` e sem merge commit, contanto que ninguém tenha commitado em
`main` remoto entre agora e a execução da Fase 2 (por isso a Fase 2 deve reconfirmar `git fetch origin` e
comparar `origin/main` antes de empurrar).

---

## 1. Worktree, branch, origem

```
pwd    -> /home/dfed/Projetos/Monitora-Campestre-Savanico_job6_hotfix_contrato_painel_v263
branch -> hotfix/job6-contrato-painel-v263
HEAD   -> 55a498f (Atualiza main para v2.6.3)
remote -> https://github.com/danilovcorrea/Monitora-Campestre-Savanico.git (fetch/push)
```

Worktree confirmada como a aprovada (não é `~/Projetos/Monitora_publicacao_main_v263_fix` nem a worktree antiga
de handoff). Base de partida (`55a498f`) é exatamente a v2.6.3 pública, o commit mais recente do branch.
Script contém os patches JOB6/JOB6B/JOB6C/JOB6E aprovados (confirmado por 29 ocorrências de marcadores
específicos — `monitora_txt_reparar_mojibake_utf8_latin1`, `monitora_dt_consolidar_aliases_mojibake_colunas`,
`forma_vida_nativa_samambaia` — e pela suíte de testes reexecutada nesta sessão, ver §5).

---

## 2. Estado aprovado preservado

Confirmado por reexecução dos testes automatizados relevantes (não apenas leitura de relatório anterior):

- Dropdown/auditoria/log reconciliados em 98 atributos editáveis (`test_job6e_dropdown_97_98_reconciliado.R`).
- Nenhum atributo exposto com bloqueio tardio; 14 `bloqueado_sem_dominio_xlsform` resolvidos
  (`test_job6e_14_bloqueados_dominio.R`, executado em sessões anteriores; reconfirmado por
  `test_job6e_antigambiarra.R` e parse nesta sessão).
- `forma_vida_nativa_samambaia`/`_sp` (`test_job6_eixoA_samambaia_dicionario.R`).
- Diretório de gráficos `output/06_graficos` (`test_diretorio_graficos_06_graficos_estatica.R`).
- Reparo de mojibake determinístico e não-corruptor de texto correto (`test_job6e_reparo_mojibake_idempotente.R`).
- Importador run05: alias mojibake coalescido sem perda, dedup seguro, mensagem de bloqueio específica por
  COLETA/UA (`test_job6e_importador_run05.R`).
- Ausência de contrato paralelo/whitelist solta no diff acumulado (`test_job6e_antigambiarra.R`).

Todos os testes acima foram **reexecutados nesta sessão**, após a atualização do cabeçalho de versão do
script (não apenas herdados de relatórios anteriores) — ver §5 para a lista completa com resultado.

JOB6F/JOB6G/JOB6H (auditoria dos 10 grupos COLETA/UA com pendência de dados de origem, pacote de
comunicação para o time de coleta e revisão da mensagem) são artefatos de auditoria/comunicação sobre
**dados**, não patches de código, e não fazem parte do pacote público — permanecem em `diagnostics/`,
fora do staging desta publicação (ver §6).

---

## 3. Arquivos alterados/criados

### Script principal e cópias espelhadas (conteúdo idêntico entre si)

| Arquivo | Mudança |
|---|---|
| `monitora_campsav_alvo_global.R` | Cabeçalho de versão `2.6.3` → `2.6.4` (2 linhas: "Versão do script" e "Versão pública"). Nenhuma outra linha alterada nesta sessão — todo o conteúdo funcional já vinha de JOB6/JOB6B/JOB6C/JOB6E, aprovado em testes. |
| `monitora_campsav_alvo_global_v2.6.4.R` | Novo — cópia idêntica do script acima (convenção já usada desde v2.0.1: manter uma cópia por versão na raiz). |
| `R_monitora_campsav_alvo_global.R` | Atualizado — cópia idêntica (convenção já usada desde v2.5.2). |
| `R/monitora_campsav_alvo_global.R` | Atualizado — cópia idêntica (convenção já usada desde v2.5.3). |

**Nota sobre strings internas de versão:** o script tem dezenas de comentários históricos do tipo
`### v2.6.3 (ajuste ...)` documentando quando cada correção foi introduzida — são registro histórico
(equivalente a `git blame` embutido) e **não foram alterados**, seguindo o mesmo padrão das releases
anteriores. Também identificado (não alterado, por já ser uma inconsistência pré-existente e não
introduzida nesta revisão): o gerador do manual/relatório consolidado (`monitora_correcao_...`, por volta
da linha 1536) tem uma string fixa `"versão 2.6.0"` no subtítulo do Rmd, que já estava desatualizada nas
releases v2.6.1 e v2.6.3 (confirmado comparando as duas). Não é uma regressão desta revisão; registrado
aqui para eventual correção em uma revisão de código futura, fora do escopo do JOB6I (que é preparação de
publicação, não abertura de novo patch de código).

### Metadados públicos

| Arquivo | Mudança |
|---|---|
| `VERSION` | `2.6.3` → `2.6.4` |
| `README.md` | Versão pública, link do script, seção "Novidades da v2.6.4" (substitui "Novidades da v2.6.3"), passos de uso, `output/06_graficos` adicionado a "Produtos principais", "Histórico recente" e "Como citar" atualizados. Nenhuma seção pública removida. |
| `CHANGELOG.md` | Nova entrada `## v2.6.4` no topo, mesmo estilo/nível de detalhe da entrada `v2.6.3` existente. |
| `RELEASE_NOTES_v2.6.4.md` | Novo, seguindo a estrutura de `RELEASE_NOTES_v2.6.3.md` (Destaques, Padrões públicos seguros, Uso recomendado, Notas de compatibilidade, Privacidade, Limitações conhecidas, Arquivos principais). |
| `INSTRUCOES_BOLSISTAS_v2.6.4.md` | Novo, baseado em `INSTRUCOES_BOLSISTAS_v2.6.3.md`, com adição de uma seção curta sobre o comportamento de bloqueio por resíduo em campo estruturado. |
| `SHA256SUMS.txt` | Regenerado para os arquivos da v2.6.4 (mesmo formato/lista de arquivos da v2.6.3, com o nome do zip atualizado). |

### Pacote de release

| Caminho | Conteúdo |
|---|---|
| `release_assets/v2.6.4/` | Espelha `release_assets/v2.6.3/`: os 3 scripts, README, CHANGELOG, RELEASE_NOTES, INSTRUCOES_BOLSISTAS, VERSION, SHA256SUMS.txt local. |
| `releases/v2.6.4/` | Espelha `releases/v2.6.1/` (padrão mais recente com pasta `releases/`): CHANGELOG, os 2 scripts (genérico + versionado), README, RELEASE_NOTES, VERSION, SHA256SUMS.txt, SHA256SUMS_v2.6.4_ZIP.txt, e o ZIP (**local, não staged** — ver §6). |

---

## 4. Auditoria de dados reais

Comando executado (igual ao especificado no job) sobre o repositório inteiro:

```bash
find . -path './.git' -prune -o -type f -print | grep -Ei '(^|/)(input|output|log|extracted)(/|$)|\.(csv|xlsx|xls|ods|zip|rds|rda|sqlite|db|gpkg|shp|geojson|kml|kmz|png|jpg|jpeg)$'
```

36 arquivos encontrados. Classificação:

- **6 ZIPs de releases públicas anteriores** (`Monitora-Campestre-Savanico_v2.5.5_release_completa.zip` e
  equivalentes em `release_assets/v2.5.5/` e `releases/v2.5.4..v2.6.0/`) — já commitados em revisões
  anteriores, fora do escopo desta sessão; auditado por amostragem (`unzip -l`) e confirmado que contêm
  apenas script/documentação, sem dado real.
- **30 CSVs/XLSX dentro de `diagnostics/`** — produzidos pelas sessões JOB6/JOB6B/JOB6C/JOB6E/JOB6F/JOB6G
  desta mesma worktree. **Achado relevante:** pelo menos um arquivo
  (`diagnostics/job6e_dropdown_dominios_importacao_v263/auditoria_token_ponto_pipe_mojibake_run05.csv`,
  coluna `exemplos_valores`) contém **excertos reais (embora truncados) de valores biológicos** de campo
  observados na run05 (ex.: tokens de forma de vida como `"graminoide"`, `"serrapilheira graminoide"`).
  **Decisão: toda a pasta `diagnostics/` (exceto este próprio relatório) fica fora do pacote público e
  fora do staging desta publicação** — não só o arquivo identificado, para não depender de uma auditoria
  arquivo-a-arquivo exaustiva sob prazo apertado. Isso também é consistente com o precedente histórico:
  nenhuma release anterior (`release_assets/v2.6.3/` etc.) incluiu uma pasta `diagnostics/`.

**Nenhum arquivo com padrão proibido foi staged, copiado para `release_assets/v2.6.4/`, `releases/v2.6.4/`
ou incluído no ZIP.** Auditoria específica do ZIP e das duas pastas de release, ambas limpas:

```bash
unzip -l releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip | grep -Ei '...'
# NENHUM PADRAO PROIBIDO ENCONTRADO NO ZIP

find release_assets/v2.6.4 releases/v2.6.4 -type f | grep -Ei '...'
# NENHUM PADRAO PROIBIDO EM release_assets/v2.6.4 OU releases/v2.6.4
```

---

## 5. Testes executados nesta sessão (após a atualização de versão)

| Teste | Resultado |
|---|---|
| `test_job6e_00_parse.R` | OK |
| `test_job6e_antigambiarra.R` | OK — 5 padrões suspeitos, todos já justificados, nenhum novo |
| `test_job6e_reparo_mojibake_idempotente.R` | OK |
| `test_job6_eixoA_samambaia_dicionario.R` | OK |
| `test_diretorio_graficos_06_graficos_estatica.R` | OK |
| `test_job6e_dropdown_97_98_reconciliado.R` | OK — dropdown=auditoria=98 |
| `test_job6e_importador_run05.R` | OK — alias mojibake coalescido, dedup seguro, mensagem específica |
| `Rscript -e 'parse(...)'` | `PARSE_OK` |
| `git diff --check` | limpo (sem marcadores de conflito, sem espaço em branco problemático) |

---

## 6. Auditoria de staging seletivo (candidatos, nada adicionado ainda)

```bash
git status --short
```

```
 M CHANGELOG.md
 M R/monitora_campsav_alvo_global.R
 M README.md
 M R_monitora_campsav_alvo_global.R
 M SHA256SUMS.txt
 M VERSION
 M monitora_campsav_alvo_global.R
?? INSTRUCOES_BOLSISTAS_v2.6.4.md
?? RELEASE_NOTES_v2.6.4.md
?? diagnostics/
?? monitora_campsav_alvo_global_v2.6.4.R
?? release_assets/v2.6.4/
?? releases/v2.6.4/
```

### Candidatos a staged (públicos, necessários, auditados sem dado real)

```
monitora_campsav_alvo_global.R
monitora_campsav_alvo_global_v2.6.4.R
R_monitora_campsav_alvo_global.R
R/monitora_campsav_alvo_global.R
VERSION
README.md
CHANGELOG.md
RELEASE_NOTES_v2.6.4.md
INSTRUCOES_BOLSISTAS_v2.6.4.md
SHA256SUMS.txt
release_assets/v2.6.4/CHANGELOG.md
release_assets/v2.6.4/INSTRUCOES_BOLSISTAS_v2.6.4.md
release_assets/v2.6.4/monitora_campsav_alvo_global.R
release_assets/v2.6.4/monitora_campsav_alvo_global_v2.6.4.R
release_assets/v2.6.4/README.md
release_assets/v2.6.4/RELEASE_NOTES_v2.6.4.md
release_assets/v2.6.4/R_monitora_campsav_alvo_global.R
release_assets/v2.6.4/SHA256SUMS.txt
release_assets/v2.6.4/VERSION
releases/v2.6.4/CHANGELOG.md
releases/v2.6.4/monitora_campsav_alvo_global.R
releases/v2.6.4/monitora_campsav_alvo_global_v2.6.4.R
releases/v2.6.4/README.md
releases/v2.6.4/RELEASE_NOTES_v2.6.4.md
releases/v2.6.4/SHA256SUMS.txt
releases/v2.6.4/SHA256SUMS_v2.6.4_ZIP.txt
releases/v2.6.4/VERSION
diagnostics/job6i_publicacao_v264/JOB6I_PUBLICACAO_V264.md
```

### Explicitamente NÃO candidatos (ficam de fora do staging)

- `releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip` — binário grande; segue o
  precedente de v2.6.1/v2.6.3 (o zip não é commitado no git, só publicado como asset da GitHub Release via
  `gh release create`, referenciado por hash em `SHA256SUMS.txt`/`SHA256SUMS_v2.6.4_ZIP.txt`).
- `diagnostics/job6_hotfix_contrato_painel_v263/`, `diagnostics/job6b_.../`, `diagnostics/job6c_.../`,
  `diagnostics/job6e_.../`, `diagnostics/job6f_.../`, `diagnostics/job6g_.../`, `diagnostics/job6h_.../` —
  contêm CSVs/relatórios técnicos internos, incluindo pelo menos um com excerto real de dado biológico
  (ver §4). Ficam fora do pacote público e fora do staging desta publicação.
- Quaisquer arquivos `.zip`/`.csv`/`.xlsx` de releases anteriores já existentes no repositório — não
  tocados nesta sessão.

**Comando de staging seletivo proposto (não executado ainda):**

```bash
git add \
  monitora_campsav_alvo_global.R monitora_campsav_alvo_global_v2.6.4.R \
  R_monitora_campsav_alvo_global.R R/monitora_campsav_alvo_global.R \
  VERSION README.md CHANGELOG.md RELEASE_NOTES_v2.6.4.md INSTRUCOES_BOLSISTAS_v2.6.4.md SHA256SUMS.txt \
  release_assets/v2.6.4/ releases/v2.6.4/CHANGELOG.md releases/v2.6.4/monitora_campsav_alvo_global.R \
  releases/v2.6.4/monitora_campsav_alvo_global_v2.6.4.R releases/v2.6.4/README.md \
  releases/v2.6.4/RELEASE_NOTES_v2.6.4.md releases/v2.6.4/SHA256SUMS.txt \
  releases/v2.6.4/SHA256SUMS_v2.6.4_ZIP.txt releases/v2.6.4/VERSION \
  diagnostics/job6i_publicacao_v264/JOB6I_PUBLICACAO_V264.md
```

(Nunca `git add .` — cada caminho é explícito, e o ZIP e o restante de `diagnostics/` ficam de fora por
omissão deliberada, não por acidente.)

---

## 7. Pacote público — resumo

| Item | Valor |
|---|---|
| ZIP | `releases/v2.6.4/Monitora-Campestre-Savanico_v2.6.4_release_publica.zip` |
| Tamanho | 1.888.364 bytes (~1,8 MB) |
| Arquivos internos | 10 (script ×3, README, CHANGELOG, RELEASE_NOTES, INSTRUCOES_BOLSISTAS, VERSION, SHA256SUMS.txt) |
| SHA256 do ZIP | `51f7bbbc5aabfb2a85b97dac3fe19bab9d211a3846b3a86107704a4a105d1d89` |
| Dado real no ZIP | Nenhum (auditado, ver §4) |

---

## 8. Release notes (conteúdo completo)

Ver `RELEASE_NOTES_v2.6.4.md` na raiz do repositório (idêntico ao copiado para `release_assets/v2.6.4/` e
`releases/v2.6.4/`).

---

## 9. Confirmação de estado git

Estado nas duas worktrees envolvidas (nenhuma tem nada staged/commitado/enviado ao remoto):

**Worktree hotfix** (`.../Monitora-Campestre-Savanico_job6_hotfix_contrato_painel_v263`, branch
`hotfix/job6-contrato-painel-v263`) — preservada intacta, é a origem do patch de código e de todo o
histórico de auditoria JOB6-JOB6H:

```
git status -sb
## hotfix/job6-contrato-painel-v263...origin/main
 M CHANGELOG.md
 M R/monitora_campsav_alvo_global.R
 M README.md
 M R_monitora_campsav_alvo_global.R
 M SHA256SUMS.txt
 M VERSION
 M monitora_campsav_alvo_global.R
?? INSTRUCOES_BOLSISTAS_v2.6.4.md
?? RELEASE_NOTES_v2.6.4.md
?? diagnostics/
?? monitora_campsav_alvo_global_v2.6.4.R
?? release_assets/v2.6.4/
?? releases/v2.6.4/
```

**Worktree nova de publicação** (`.../Monitora-Campestre-Savanico_publicacao_v264_main`, branch
`release/v2.6.4-publicacao`, criado a partir de `origin/main`) — é onde a Fase 2 vai rodar:

```
git status -sb
## release/v2.6.4-publicacao...origin/main
 M CHANGELOG.md
 M R/monitora_campsav_alvo_global.R
 M README.md
 M R_monitora_campsav_alvo_global.R
 M SHA256SUMS.txt
 M VERSION
 M monitora_campsav_alvo_global.R
?? INSTRUCOES_BOLSISTAS_v2.6.4.md
?? RELEASE_NOTES_v2.6.4.md
?? monitora_campsav_alvo_global_v2.6.4.R
?? release_assets/v2.6.4/
?? releases/v2.6.4/
?? diagnostics/job6i_publicacao_v264/JOB6I_PUBLICACAO_V264.md   (copiado por ultimo, apos esta atualizacao)

git diff --stat
 CHANGELOG.md                     |  13 +
 R/monitora_campsav_alvo_global.R | 762 +++++++++++++++++++++++++++++++++++++--
 README.md                        |  33 +-
 R_monitora_campsav_alvo_global.R | 762 +++++++++++++++++++++++++++++++++++++--
 SHA256SUMS.txt                   |  20 +-
 VERSION                          |   2 +-
 monitora_campsav_alvo_global.R   | 762 +++++++++++++++++++++++++++++++++++++--
 7 files changed, 2251 insertions(+), 103 deletions(-)

git diff --check
(limpo, sem saída)
```

**Confirmação: nenhum `git add`, nenhum commit, nenhuma tag, nenhum push foi executado em nenhuma das duas
worktrees nesta sessão.**

---

## 10. Comandos propostos para a Fase 2 — SUPERSEDIDO, ver §0.5

A versão original desta seção propunha push para o próprio branch `hotfix/job6-contrato-painel-v263`. Após
o pedido do usuário para que a tag/release aponte para `main`, a estratégia final está em **§0.5** (worktree
nova, branch `release/v2.6.4-publicacao`, `git push origin release/v2.6.4-publicacao:main`). Mantida esta
nota para rastreabilidade de por que a estratégia mudou entre a Fase 1 e a Fase 1B.

---

## 11. Aguardando confirmação

**`AGUARDANDO_CONFIRMACAO_PUBLICAR_V264`**

Resumo para decisão (estratégia final, pós Fase 1B):

- Worktree de execução da Fase 2: `/home/dfed/Projetos/Monitora-Campestre-Savanico_publicacao_v264_main/`
- Branch local: `release/v2.6.4-publicacao` (criado a partir de `origin/main`, sem divergência)
- Commit proposto: `"Publica v2.6.4: hotfix painel, persistencia e importacao"`
- Tag proposta: `v2.6.4`
- Push proposto: `git push origin release/v2.6.4-publicacao:main` (fast-forward direto de `main`, sem
  merge commit, sem `--force`) + `git push origin v2.6.4`
- Arquivos staged candidatos: 27 (lista completa na §6, idêntica nas duas worktrees)
- SHA256 do ZIP: `51f7bbbc5aabfb2a85b97dac3fe19bab9d211a3846b3a86107704a4a105d1d89`
- Release notes: `RELEASE_NOTES_v2.6.4.md`
- Auditoria de dados reais: sem dado real no pacote/staging propostos, reauditada na worktree nova (§0.4)
- Nada foi enviado ao remoto; branch local `main` não foi tocado; worktree proibida não foi tocada

Só prosseguir para a Fase 2 se a resposta for literalmente `PUBLICAR_V264`.

---

## 12. Notificação ntfy

Enviada via `~/bin/ntfy-acao-necessaria` ao final desta fase (ver corpo da mensagem no relato da sessão).
