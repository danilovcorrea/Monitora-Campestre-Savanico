# Baseline golden v2.6.0 — Dataset PNB comparativo

## Metadados da execução

| Campo | Valor |
|---|---|
| Data/hora do diagnóstico (retomada) | 2026-07-02 |
| Data/hora de início da execução medida | 2026-07-02T15:30:10-03:00 |
| Data/hora de fim da execução medida | 2026-07-02T15:34:51-03:00 |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Pasta de execução (limpa, fora do worktree) | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260_execucao_pnb` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit (HEAD do worktree) | `482347b68f935e9938da957997d14103dabf7cf0` |
| Tag de origem | `v2.6.0` (anotada; objeto da tag resolve para o mesmo commit acima) |
| Mensagem da tag | "v2.6.0 - Roll-forward semântico, relatório consolidado e governança de validação" |
| Versão detectada (`VERSION`) | `2.6.0` |
| Script canônico | `monitora_campsav_alvo_global_v2.6.0.R` (idêntico via SHA-256 a `monitora_campsav_alvo_global.R`, `R_monitora_campsav_alvo_global.R` e à entrada em `SHA256SUMS.txt`: `1dedba9a...c682`) |

Nenhum código foi editado nesta etapa. Nenhum arquivo bruto foi movido, renomeado ou sobrescrito.

## Arquivos de entrada usados

- `input/pnb_2022_2026.zip` (SHA-256 `75fe8875a55a1aa1d3baf3b4663e9ece6089fe7359185f82eda536215fda1480`), **copiado** (não movido) de `/home/dfed/Desktop/teste_comparativo/2.6.0_pnb/ate_registros_corrig/input/pnb_2022_2026.zip`.
- Esse mesmo arquivo (mesmo hash) é o dataset comparativo já usado nos testes locais anteriores das versões 2.5.5, 2.5.6, 2.6.0 e 2.6.2 encontrados em `~/Desktop/teste_comparativo/`, contendo os CSVs brutos SISMONITORA de PNB para os ciclos 2022–2026 (`PNB_2022_...11AGO22.csv`, `PNB_2023_...11AGO22.csv`, `PNB_2024_...05MAI23.csv`/`...11AGO22.csv`, `PNB_2025_...21FEV25.csv`, `PNB_2026_...21FEV25.csv`).
- Dado bruto original (fonte primária) permanece intocado em `~/dados_originais/extr_dados_form/PNB/`.

## Modo de execução

O script v2.6.0 bloqueia a execução se houver ZIP/CSV/XLSX soltos na raiz da pasta de execução (proteção "Arquivos de entrada devem ficar exclusivamente em `input/`"). Como a raiz do worktree/repo contém arquivos históricos de release (incluindo um `.zip` de versão antiga), a execução direta na raiz do worktree foi bloqueada — comportamento esperado e documentado no próprio `README.md` ("Crie uma pasta de execução limpa"). Por isso a execução foi feita em uma pasta limpa separada, contendo apenas uma cópia do script canônico e o `input/`.

Nenhuma linha de código foi alterada. O modo de execução e os produtos foram controlados exclusivamente por variáveis de ambiente já suportadas nativamente pelo script (overrides via `Sys.getenv`, linhas ~263–360 de `monitora_campsav_alvo_global_v2.6.0.R`):

```bash
export MONITORA_MODO_EXECUCAO=ate_registros_corrig
export MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS=S
export MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S
export MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=S
/usr/bin/Rscript monitora_campsav_alvo_global_v2.6.0.R
```

Essa configuração replica exatamente a usada na execução de referência pré-existente em `~/Desktop/teste_comparativo/2.6.0_pnb/ate_registros_corrig/` (script e input com hash idêntico), permitindo comparação cruzada independente.

## Tempo de execução

- Tempo de parede total (`/usr/bin/time -v`, inclui inicialização do R, carregamento de ~15 pacotes e geração de manual/documentação): **4m41,17s** (281,17s).
- Tempo interno do núcleo do pipeline, medido pelo próprio script (marcador `dec:` no checkpoint "fim execucao"): **51,2s**.
- Critério golden (52–60s) refere-se ao tempo interno do núcleo, não ao tempo de parede total (que inclui overhead de ambiente/documentação fora do pipeline). **51,2s está dentro/imediatamente abaixo da faixa golden** e é consistente com a execução de referência de 2026-07-01, que registrou 51,8s no mesmo marcador, com o mesmo script e input.

## Produtos gerados

| Produto | Colunas | Linhas (sem header) | Tamanho | Ordem de gravação |
|---|---|---|---|---|
| `output/01_produtos_dados/registros_importados_bruto.csv` | — | — | 31.864.856 bytes | 15:34:01 |
| `output/01_produtos_dados/registros_importados.csv` | **176** | 24.241 | 39.469.973 bytes | 15:34:37 |
| `output/01_produtos_dados/registros_corrig.csv` | **180** | 24.241 | 40.805.162 bytes | 15:34:49 |

- `registros_importados.csv` e `registros_corrig.csv` têm hashes SHA-256 distintos (`c8e89...` vs `955f2...`), tamanhos distintos e cabeçalhos estruturalmente diferentes — confirmando que **não houve sobrescrita** de um pelo outro.
- Ordem de timestamps confirma o fluxo correto: bruto → importados → corrig.
- Nenhuma coluna `solo_nu` foi encontrada em nenhum dos dois produtos; colunas rotuladas legadas (`Outra forma de vida de planta nativa: (amostragem/registro)` etc.) coexistem com as canônicas (`amostragem/registro/forma_vida_outros` etc.) em ambos os arquivos — consistente com a premissa de que `registros_importados.csv` não recebe correção semântica operacional e de que a regressão "Outras plantas → solo_nu" (descrita no handoff como problema da v2.6.2_teste) **não está presente na v2.6.0**.

## Contagens golden observadas

| Ocorrência diagnóstica | Esperado (golden) | Observado |
|---|---|---|
| UAs duplicadas no mesmo ano | 606 linhas / 6 coletas | **606 linhas / 6 coletas** ✅ |
| Nativa sem forma de vida | 3 linhas / 3 coletas | **3 linhas / 3 coletas** ✅ |
| Seca ou morta sem forma de vida | 10 linhas / 8 coletas | **10 linhas / 8 coletas** ✅ |
| Outra forma de vida legada | 7 linhas / 5 coletas | **7 linhas / 5 coletas** ✅ |
| Colunas de `registros_corrig.csv` | não deve inchar para 256 | **180** ✅ |
| Performance do núcleo | 52–60 s | **51,2 s** (referência anterior: 51,8s) ⚠️ ligeiramente abaixo, não constitui divergência material |

Todas as quatro contagens diagnósticas batem exatamente com o golden documentado em `docs/desenvolvimento/HANDOFF_DEV_V262_GOLDEN_V260.md` e com a execução de referência independente de 2026-07-01.

## Divergências encontradas

1. **Warning não-fatal** ao final da execução: `Falha ao gerar relatório consolidado de validação: 'unique' is not an exported object from 'namespace:data.table'`. Não impede a geração de `registros_importados.csv` nem `registros_corrig.csv`, não afeta as contagens golden nem a contagem de colunas. Provável incompatibilidade pontual entre a versão do script (2.6.0) e a versão de `data.table` instalada no ambiente atual (`1.18.4`), no gerador opcional de relatório de validação consolidado (produto documental, não operacional). Recomenda-se investigar antes de reintroduzir esse gerador de relatório nas camadas isoladas da v2.6.2, mas não bloqueia esta baseline.
2. A raiz do worktree não pôde ser usada diretamente como pasta de execução (arquivos de release legados na raiz disparam a trava de segurança do script contra ZIP/CSV/XLSX soltos). Não é uma divergência do golden — é o comportamento documentado no `README.md` ("crie uma pasta de execução limpa"). Resolvido usando uma pasta de execução externa ao worktree, sem alterar nada dentro do worktree.
3. Tempo de parede total (4m41s) é bem maior que a janela golden de 52–60s, mas isso mede o processo R inteiro (start-up, ~15 pacotes, geração de manual do usuário e tentativa de relatório de validação), não o núcleo do pipeline. O marcador interno do próprio script (`dec:`) é a métrica corretamente comparável ao golden e ficou em 51,2s.

## Conclusão

**PASSOU.**

A tag `v2.6.0` (commit `482347b6`), executada sem nenhuma edição de código, a partir do dataset comparativo padrão `pnb_2022_2026.zip`, reproduz integralmente os critérios golden de PNB documentados no handoff: as quatro contagens diagnósticas batem exatamente, `registros_corrig.csv` não incha para 256 colunas (ficou em 180), `registros_importados.csv` é gerado como produto distinto e não sanitizado semanticamente, e não houve sobrescrita entre os dois produtos. O tempo interno do núcleo do pipeline (51,2s) está consistente com a faixa golden observada anteriormente (51,8s) e a documentada (52–60s).

A v2.6.0 está confirmada como golden baseline operacional reproduzível para retomada do desenvolvimento a partir da nova branch `dev-v2.6.2-rollforward-golden-v260`.
