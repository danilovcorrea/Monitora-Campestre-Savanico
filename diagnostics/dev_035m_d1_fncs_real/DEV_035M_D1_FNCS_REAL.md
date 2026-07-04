# Dev 03.5M-D1 — Localização de evidência FNCS real e preparação de validação segura

## 1. Resumo executivo

**Localizada evidência real, segura e conclusiva** do padrão de pipe condicional/esparso em produção: o arquivo `/home/dfed/Desktop/03jul_dev/2.6.0_FNCS_error_import.zip` (run real de `registros_importados.csv` sobre `FNCS_23_24_25_26.zip`, datada de 2026-07-03 07:13–07:15) contém os relatórios de auditoria de pipe já gerados pelo próprio script (`auditoria_pipes_*.csv`, `pipe_indeterminado_*.csv`) confirmando, com contagens agregadas e só o token contratual genérico `"terrestre"` (nunca dado real de célula além disso), que:

- A coluna **"A bromélia observada é: (amostragem/registro)"** tem **304–405 linhas** com resíduo de pipe classificado `pipe_indeterminado` (nunca resolvido, comportamento seguro já vigente desde o Hotfix 03.5G2), com padrões de **2 a 25 tokens `"terrestre"` repetidos** por valor — exatamente o padrão estrutural do caso condicional/esparso.
- A coluna **"A cactácea observada é: (amostragem/registro)"** tem o mesmo padrão, em menor escala (1 linha, `"terrestre|terrestre"`).
- **Orquídea e samambaia não aparecem** nesta auditoria específica (0 colunas correspondentes na lista de 16 colunas auditadas nesta run — não há evidência de pipe residual para essas duas formas neste dataset específico).
- O erro real de bloqueio desta run **não foi causado pela bromélia/cactácea** (ambas ficaram em `pipe_indeterminado`, nunca bloqueante) — foi causado por 5 colunas estruturadas diferentes (as listas principais de forma de vida nativa/exótica/seca-morta e 2 campos de texto "outra forma de vida"), um problema já tratado pelos Hotfixes 03.5C–03.5E, anteriores a esta etapa.

Este é, com alta confiança, o **mesmo run que gerou os números citados na Auditoria 03.5F** (`n_linhas_pipe_indeterminadas = 414`): 9 (liana) + 304 (bromélia) + 101 (`__dup2`) = **414**, batendo exatamente.

A regra de obrigatoriedade dos campos vinculados (bromelioide/cactacea/orquidea/samambaia × nativa/exótica/seca-morta, valores `epifita`/`terrestre`/`rupicola`) foi **confirmada como já presente no contrato** — tanto na tabela de choices do XLSForm embutido (`list_name` = `forma_vida_<categoria>_<forma>`, valores `epifita`/`terrestre`/`rupicola` confirmados para bromelioide, cactácea e samambaia) quanto na regra padrão já codificada em `monitora_correcao_dependencias_padrao()` (`required = "yes"` para as 12 combinações categoria×forma).

## 2. Objetivo

Localizar ou preparar evidência real (não hipotética) do padrão de pipe condicional/esparso documentado no Hotfix 03.5G2 (COLETA 11168), para decidir se já há base suficiente para planejar 03.5M-D (migração operacional parcial), sem migrar nada nesta etapa.

## 3. Especificação revisada de atributos vinculados obrigatórios (confirmação contratual)

| Item da especificação | Confirmado no contrato embutido? |
|---|---|
| Formas abrangidas: bromelioide, cactacea, orquidea, samambaia | **Sim** — as 4 aparecem como `dependent_name` em `monitora_correcao_dependencias_padrao()` (`formas_cond <- c("bromelioide", "cactacea", "orquidea", "samambaia")`) e como `list_name`/campo no dump XLSForm embutido |
| 12 campos vinculados (`forma_vida_<categoria>_<forma>` × nativa/exótica/seca-morta) | **Sim** — todos os 12 aparecem no dump XLSForm com `type = "select_one forma_vida_<categoria>_<forma>"` |
| Valores válidos: `epifita`, `terrestre`, `rupicola` | **Sim** — confirmado na tabela `opcoes` embutida para `forma_vida_nativa_bromelioide`, `forma_vida_nativa_cactacea`, `forma_vida_nativa_samambaia` (e variantes exótica/seca-morta) — os 3 valores aparecem com os labels exatos "Epífita (que se desenvolve sobre outras plantas)", "Terrestre", "Rupícola (que se desenvolve sobre rochas)" |
| Obrigatoriedade (`required = yes`) quando a forma aparece na lista principal correspondente | **Sim, já codificado** — `monitora_correcao_dependencias_padrao()` (`monitora_campsav_alvo_global_v2.6.0.R`, ~L6206) gera `required := "yes"` para as 12 combinações via `data.table::CJ(categoria = cats, token = formas_cond)`, com `relevant := "selected(${forma_vida_<categoria>}, '<token>')"` |
| `_sp` (espécie) não obrigatório em geral | Não reverificado linha a linha nesta etapa (fora do escopo estrito de localizar evidência FNCS); consistente com o que já foi observado nas auditorias 03.5A–H2 (campos `_sp` sempre com `required` vazio no dump) |
| `_outra_sp` obrigatório só quando "outra espécie" selecionada (exótica) | Não reverificado nesta etapa — recomendado para 03.5M-D, se a migração avançar para os campos de espécie |

Nenhuma alteração foi feita ao contrato ou a nenhuma função — isto é só confirmação de leitura.

## 4. Locais pesquisados

- `find` por nomes de arquivo/diretório contendo `fncs`, `11168`, `bromel*`, `cactacea`, `orquidea`, `samambaia`, `hotfix_035g*` em `/home/dfed/Desktop` e `/home/dfed/Projetos` (recursivo, só leitura).
- `/mnt/windows/Users/danil/` — confirmado montado e acessível; localizados vários diretórios/arquivos reais relacionados a FNCS no OneDrive do usuário (dados de produção legítimos, fora do escopo desta busca — não abertos, só a existência foi observada via `find`).
- `grep` por `11168`/`bromel`/`cactacea`/`orquidea`/`samambaia` em todos os `console_*.txt` de `/home/dfed/Desktop/03jul_dev/*` e `/home/dfed/Desktop/*`.
- Inspeção do conteúdo do ZIP `2.6.0_FNCS_error_import.zip` via `unzip -l`/`unzip -p` (listagem e leitura de arquivos individuais sem extração completa).
- Extração **seletiva e temporária** (fora do repositório, em `/tmp/.../scratchpad/inspecao_fncs_zip/`, **apagada ao final**) de só 4 arquivos pequenos do ZIP: os 3 CSVs de auditoria de pipe já gerados pelo próprio script (`auditoria_pipes_*.csv`, 32KB; `pipe_indeterminado_*.csv`, 160KB; `bloqueio_pipe_residual_*.csv`, 3.2MB, headers só) e 1 console log (8.5KB). **O CSV de dados brutos dentro do mesmo ZIP (`registros_importados_bruto.csv`, 80MB, dado real de produção) nunca foi extraído nem lido.**

## 5. Candidatos FNCS encontrados

| Caminho | Natureza |
|---|---|
| `/home/dfed/Desktop/03jul_dev/2.6.0_FNCS_error_import.zip` | **Usado** — run real com falha de exportação por resíduo de pipe; contém os 3 CSVs de auditoria de pipe já gerados pelo próprio script |
| `/home/dfed/Desktop/02jul/FNCS/*.csv` (4 arquivos, 2023-2026) | Dados brutos FNCS por ano — não abertos |
| `/home/dfed/Desktop/03jul_dev/{35g2,cor_pipeline,corrigido,cor_virgula,original}/input/FNCS_23_24_25_26.zip` (múltiplas cópias) | Dados brutos de entrada de runs de desenvolvimento anteriores — não abertos |
| `/home/dfed/Desktop/03jul_dev/console_2.6.0_FNCS_original.txt`, `/home/dfed/Desktop/03jul_dev/corrigido/console_2.6.0_cor_virgula_FNCS.txt` | Consoles de outras runs FNCS — verificados por `grep`, sem menção a `11168`/`bromel` |
| `/mnt/windows/Users/danil/OneDrive - ICMBio/...` (vários) | Dados de produção do usuário no OneDrive — existência confirmada, **não abertos** (fora do escopo desta busca, seriam dados reais de produção sem necessidade de tocar) |

## 6. COLETA 11168 localizada?

**Não diretamente confirmada por identificador** — os arquivos de auditoria de pipe (`auditoria_pipes_*.csv`, `pipe_indeterminado_*.csv`) não têm coluna de COLETA (só `produto, coluna, linha_indice, valor_residual, classificacao, orientacao`); confirmar a COLETA exata exigiria abrir o CSV bruto de 80MB (dado real de produção), o que foi deliberadamente evitado por desproporcionalidade de risco frente ao valor marginal — já há evidência textual precisa da COLETA 11168 (Hotfix 03.5G2) e evidência agregada real e robusta do mesmo padrão estrutural (seção 7) sem precisar reidentificar a COLETA específica.

## 7. Padrão bromelioide/cactacea/orquidea/samambaia com pipe localizado?

**Sim, para bromélia e cactácea; não observado para orquídea/samambaia nesta run específica.**

| Coluna | Classificação (script) | Nº de linhas com pipe residual | Padrão de tokens observado (só o token contratual `"terrestre"`, já whitelisted) |
|---|---|---:|---|
| "A bromélia observada é: (amostragem/registro)" | `pipe_indeterminado` (nunca bloqueia, seguro) | 304–405 (duas contagens levemente diferentes entre os 2 arquivos de auditoria da mesma run) | `terrestre\|terrestre` (199×), `terrestre\|terrestre\|terrestre` (102×), 4 tokens (3×), 24 tokens (77×), 25 tokens (24×) |
| "A cactácea observada é: (amostragem/registro)" | `pipe_indeterminado` | 1 | `terrestre\|terrestre` |
| Orquídea (qualquer variante) | — | 0 | não aparece entre as 16 colunas auditadas nesta run |
| Samambaia (qualquer variante) | — | 0 | não aparece entre as 16 colunas auditadas nesta run |

Nenhum valor real além do token contratual genérico `"terrestre"` (já listado nas especificações desta tarefa como valor válido) foi transcrito.

## 8. Há evidência real suficiente para planejar 03.5M-D?

**Parcialmente.** Há evidência real robusta de que o padrão condicional/esparso (múltiplos "terrestre" repetidos, esparsos entre os 101 pontos) existe de fato em dados de produção para bromélia e cactácea — isso substitui, com uma amostra muito maior (centenas de linhas em vez de 1 caso), a necessidade de confiar só no fixture sintético da 03.5M-D0. Porém:

- A run específica analisada **já está "presa" em `pipe_indeterminado`** (nunca chegou a ser resolvida por ponto nem bloqueada) — não há, nesta run, um caso onde a resolução por ponto absoluto tenha **efetivamente rodado e falhado** sobre a bromélia (isso só aconteceu na run documentada no Hotfix 03.5G, quando o alias estava temporariamente ativo, e não deixou log disponível nos diretórios pesquisados).
- Orquídea/samambaia não têm nenhuma evidência real de pipe nesta amostra — não há como validar essas duas formas com o material disponível agora.
- Não foi possível (nem seria seguro, dado o custo/risco de abrir 80MB de dado real de produção) confirmar a COLETA exata ou cruzar com `forma_vida_nativa`/`forma_vida_exotica` para validar a elegibilidade ponto a ponto contra este dataset real específico.

## 9. Plano seguro de run FNCS real (preparado, não executado)

Configuração recomendada para uma futura run manual no RStudio (script versionado, cópia local, não este agente):

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_DIAGNOSTICO_PIPES_CONTRATO <- "S"
MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
```

Critérios de sucesso da run:
- `output/diagnosticos_pipes_contrato/` é criado.
- O relatório opt-in (03.5M-C/C2) mostra pipe residual em pelo menos uma coluna `estruturado_condicional_esparso` (bromélia/cactácea, conforme seção 7) — capturando, pela primeira vez, um **caso positivo real dentro do próprio mecanismo diagnóstico da 03.5M**, não só na auditoria de pipe legada.
- `registros_corrig.csv` é materializado normalmente.
- `registros_validados.csv` não é exigido se houver pendências impeditivas (comportamento já validado).
- Nenhuma alteração no resolvedor operacional.

Esta run **não foi executada** nesta tarefa — só preparada.

## 10. Riscos de dados reais e como foram mitigados

- Abertura de ZIP real: mitigada extraindo **só os 3 CSVs de auditoria já agregados pelo próprio script** (nunca o CSV bruto de 80MB) para um diretório temporário fora do repositório, **apagado ao final da inspeção**.
- Transcrição de valor real: mitigada reportando só contagens agregadas e o token contratual genérico `"terrestre"` (já listado como valor válido pela própria especificação desta tarefa) — nenhuma espécie, coordenada, UUID, COLETA (além do identificador já documentado 11168, citado só textualmente, nunca extraído de um arquivo real nesta tarefa) ou linha individual foi transcrita.
- Dados no OneDrive do usuário (`/mnt/windows/...`): só existência confirmada via `find`, nenhum arquivo aberto.

## 11. Confirmação: nenhum dado real foi versionado

Nenhum arquivo foi copiado para o repositório. O diretório temporário de inspeção (`/tmp/.../scratchpad/inspecao_fncs_zip/`) foi removido (`rm -rf`) ao final desta tarefa, confirmado por `ls` retornando "No such file or directory". Nenhum CSV/ZIP/XLSX foi criado em `diagnostics/` — só este relatório `.md`.

## 12. Confirmação: nenhuma chamada operacional foi alterada

**O script não foi editado nesta etapa** — não foi necessário nenhum helper diagnóstico novo; a inspeção manual (busca de arquivos + leitura agregada via R) foi suficiente, conforme preferência explícita da tarefa. `git diff --stat` (seção 14) confirma zero alterações no arquivo principal.

## 13. Recomendação objetiva

**`planejar_migracao_parcial_apenas_apos_FNCS_real`** — a evidência real localizada é forte o suficiente para confirmar que o padrão existe em produção (bromélia principalmente, cactácea em menor escala), mas ainda não substitui uma run real com o relatório opt-in (03.5M-C/C2) ativo capturando o caso *dentro do próprio mecanismo diagnóstico novo* — o plano da seção 9 é o próximo passo concreto antes de qualquer migração operacional. Órquidea/samambaia permanecem sem nenhuma evidência real — qualquer migração parcial futura deveria, no mínimo, começar por bromélia/cactácea (evidência real robusta) e tratar órquidea/samambaia como pendente de validação adicional.

## 14. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
(sem saída — nenhuma alteração)
```

## 15. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
?? diagnostics/dev_035m_d1_fncs_real/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
