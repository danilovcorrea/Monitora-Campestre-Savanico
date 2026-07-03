# Auditoria 03.5L-A — Consolidação de `registros_importados.csv`

Auditoria pura: leitura estática do script (`grep`/inspeção de trechos), sem execução, sem dado real, sem alteração de código. Base: `e1d458a feat: adiciona diagnostico observado canonico` (03.5I/J/K já mergeados).

## 1. Resumo executivo

`registros_importados.csv` **não é hoje um produto simples de importação** — é materializado em **dois checkpoints sequenciais que se sobrescrevem**, e o segundo (o que efetivamente fica em disco ao final) usa **`registros_corrig` já tokenizado**, não o objeto de importação puro. Isso significa que a "camada canônica de importação" pedida pela premissa-mãe já convive, por desenho validado (Hotfix 03.5D), com boa parte do pipeline de correção — a auditoria precisa deixar isso explícito antes de qualquer patch, porque muda o que "não aplicar sanitização final" pode significar na prática.

Achado mais importante: a resolução de pipe por **índice de ponto absoluto** que a premissa #7 proíbe para bromélia condicional/esparsa **já existe e já roda hoje** dentro do próprio fluxo de `registros_importados.csv`, via `monitora_produtos_resolver_pipes_por_ponto()`. É exatamente a mesma linhagem de código que motivou os Hotfixes 03.5C a 03.5G2. Isso não é um risco que 03.5L introduziria — é o estado atual, validado, do produto golden. A conclusão prática: **03.5L-B não pode tocar esse mecanismo**; só pode observá-lo/diagnosticá-lo.

O produto inteiro é **seletivo/opt-in**: por padrão (`MONITORA_MODO_EXECUCAO="completo"`, `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS="N"`), `registros_importados.csv` **não é gerado**. Isso importa diretamente para a estratégia de teste — uma run PNB "completa" default não exercita nada disto.

## 2. Funções atuais relacionadas a `registros_importados`

| Função | Linha (aprox.) | Papel | Observação |
|---|---:|---|---|
| `monitora_registros_importados_exportar` | L24405 (morta) / **L32712 (viva)** | Escreve `registros_importados_bruto.csv` | Duas definições; a de L24405 usa path plano antigo, a de L32712 usa `monitora_produtos_escrever_bruto_canonico`/`_path_canonico`/`_path_raiz` (padrão canônico com espelho na raiz) — só a segunda está em uso real (R sobrescreve por posição de definição) |
| `monitora_registros_importados_chaves_preferenciais` | L24478 | Lista curta hardcoded de colunas-chave para ordenação inicial | 17 nomes fixos (`COLETA`, `UC`, ..., `.id`) — não usa contrato algum |
| `monitora_registros_importados_saneado_preparar` | L24489 | **Transformação central**: dedupe de nomes duplicados, `monitora_dt_consolidar_aliases_colunas`, `make.unique(..., sep="__dup")` para sobras, reordena por `chaves_preferenciais` | Não faz sanitização de valor, não toca `solo_nu`, não resolve pipe — só estrutura de coluna |
| `monitora_registros_importados_saneado_exportar` | L24514 (morta) / **L32759 (viva)** | Orquestra: chama `saneado_preparar` → `monitora_produtos_resolver_pipes_por_ponto` → `monitora_bloquear_pipe_residual_produto` → escreve `registros_importados.csv` | Parâmetro `bloquear_pipe_residual` muda comportamento entre os 2 checkpoints (seção 3) |
| `monitora_produtos_resolver_pipes_por_ponto` | L24596 | Resolve resíduo de `\|` **por índice de ponto/COLETA absoluto**, só para colunas classificadas `pipe_residual_operacional`/`pipe_residual_revisar` | **Já usa `monitora_produtos_classificar_pipe_coluna`/`monitora_pipe_coluna_classificar_natureza` (contrato XLSForm embutido)** — não é hardcode isolado, já consulta o contrato hoje, só que sem os índices/perfis novos de 03.5J |
| `monitora_bloquear_pipe_residual_produto` | L24692 | Audita/bloqueia resíduo de `\|` remanescente após a resolução acima | Nunca bloqueia produto `*bruto*`; `pipe_indeterminado` nunca bloqueia (reporta só); `pipe_residual_operacional`/`_revisar` bloqueiam se `bloquear=TRUE` |
| `monitora_registros_importados_garantir_pre_painel` | L16890 | Guarda pré-painel: confirma que os produtos já foram materializados antes do painel abrir; nunca reconstrói | Só verifica `file.exists()` nos paths de output — não lê conteúdo como entrada de dado |
| `monitora_registros_importados_reordenar` | L24271 | Reordenação alternativa por lista de chaves | **Código morto — zero chamadores** no script inteiro |
| `monitora_registros_importados_coluna_coleta` / `_contar_coletas` | L24942/L24957 | Helpers de contagem de COLETA distinta | Usados só por `auditar_comparacao_produtos` |
| `monitora_registros_importados_auditar_comparacao_produtos` | L24975 | Compara `registros_importados` vs `registros_corrig` (linhas/COLETAs) para relatório | Read-only, 2 chamadores (L25211, L44561), gera relatório, não altera produto |
| `monitora_registros_importados_auditar_nao_aplicavel` | L25058 | Auditoria relacionada, 1 chamador (L31147) | Read-only |
| `monitora_publicacao_h_fwrite_registros_importados` | L24325 | Grava CSV robusto a cabeçalho duplicado (nomes internos únicos + cabeçalho original recomposto) | Só usada pela definição morta de `exportar` (L24405); a viva (L32712) usa `monitora_produtos_escrever_bruto_canonico` em vez disso — **possível segunda função morta**, não confirmado sem checar `monitora_produtos_escrever_bruto_canonico` internamente (fora do escopo desta auditoria) |
| `monitora_dt_consolidar_aliases_colunas` | L26349 (já mapeada na Auditoria 03.5H) | Consolidação de aliases usada dentro de `saneado_preparar` | **Já é ponto de integração natural** — 03.5J já tem `por_alias_normalizado` cobrindo o mesmo alias_map como cópia de referência |

## 3. Mapa do fluxo atual

```
registros (pós-concatenação de CSVs, pré-tudo)
  │
  ├─[gate: MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL]→ registros_importados_bruto.csv
  │                                                          (monitora_registros_importados_exportar, contexto="pos_concatenacao_csv_pre_padronizacao_ou_pre_painel")
  │
  ├─ monitora_dt_remover_colunas_tecnicas_legadas
  ├─ monitora_dt_consolidar_colunas_duplicadas
  ├─ registros <- monitora_dt_consolidar_aliases_colunas(registros)
  │
  ├─[gate: mesmo flag]→ registros_importados.csv  ["CHECKPOINT 1: PRÉ-tokenização"]
  │     saneado_exportar(registros, contexto="pos_consolidacao_aliases_importacao_pre_registros_corrig",
  │                       bloquear_pipe_residual = FALSE)
  │     → dentro: saneado_preparar (dedupe+alias+reorder) → resolver_pipes_por_ponto (corrige "|" elegível)
  │       → bloquear_pipe_residual_produto (bloquear=FALSE: só reporta, nunca aborta)
  │
  ├─ [registros_corrig criado a partir de registros]
  ├─ [cadeia padronizacao_categorias_e_material_botanico, padronizacao_formas_vida_condicionais_basicas,
  │   padronizacao_formas_vida_outras, padronizacao_especies_nativas,
  │   padronizacao_especies_exoticas_e_campos_condicionais -- todas operam sobre registros_corrig]
  ├─ monitora_correcao_corrigir_ponto_metro(registros_corrig)
  ├─ monitora_pre_painel_quarentenar_coletas_incompletas(registros_corrig)  [COLETAs incompletas saem do universo]
  │
  └─[gate: mesmo flag]→ registros_importados.csv  ["CHECKPOINT 2: PÓS-tokenização, SOBRESCREVE o checkpoint 1"]
        saneado_exportar(registros_corrig, contexto="pre_painel_pos_extracao_tokens_operacionais",
                          bloquear_pipe_residual = TRUE)
        → dentro: mesmo saneado_preparar+resolver_pipes_por_ponto, mas agora
          bloquear_pipe_residual_produto ABORTA se sobrar "|" estruturado
```

**Uso de `registros_corrig`**: SIM, diretamente — o checkpoint final (o que sobrevive em disco) usa `registros_corrig`, não `registros`. `registros_importados.csv`, no estado atual do produto golden, é substancialmente "`registros_corrig` num ponto intermediário do pipeline", não um snapshot de importação pura.

**Uso de colunas já tokenizadas**: SIM, no checkpoint 2 — toda a cadeia `padronizacao_*` já rodou sobre `registros_corrig` antes deste checkpoint.

**Pode esconder pendências?** Não identificado nenhum mecanismo de ocultação — `pipe_indeterminado` sempre é reportado (nunca bloqueado silenciosamente), `monitora_bloquear_pipe_residual_produto` audita e loga antes de decidir bloquear ou não. COLETAs quarentenadas por incompletude ficam **fora** do produto (removidas, não escondidas — mas também não aparecem, o que é uma forma de omissão a registrar: se o contrato único vier a gerar relatório de cobertura, ele deveria declarar explicitamente que COLETAs quarentenadas não estão no `registros_importados.csv` final).

## 4. Pontos de risco

| Risco | Avaliação |
|---|---|
| Alterar contagens (linhas/colunas) | **Alto se qualquer coisa nova mexer no checkpoint 2**, porque ele já é o objeto de comparação golden documentado (176 col × 24.241 linhas no baseline PNB). Qualquer reordenação/renomeação de coluna muda o hash mesmo sem mudar valor. |
| Mudar nomes de coluna | **Alto** — `chaves_preferenciais` hoje é uma lista curta e estável; se o contrato único assumir a ordenação por padrão, nomes/ordem mudam para TODO o produto, quebrando qualquer comparação externa (planilhas do bolsista, scripts de terceiros) sem aviso. |
| Duplicar atributos | Médio — `saneado_preparar` já lida com `__dup` via `make.unique`; um segundo mecanismo de dedupe do contrato único rodando em paralelo poderia gerar duplicidade de decisão (ex.: contrato único aponta 2 candidatos onde o dedupe atual já escolheu 1). |
| Esconder pendências | Baixo, **se** qualquer integração for só diagnóstica (relatório à parte). Alto se o contrato único vier a "resolver" silenciosamente uma coluna hoje marcada `pipe_indeterminado`. |
| Quebrar o painel | Médio — `monitora_registros_importados_garantir_pre_painel` depende de `registros_importados.csv` existir com conteúdo; se a geração mudar de forma incompatível, o guard já teria comportamento definido (warning, não bloqueia), mas o painel ficaria sem o produto de comparação esperado. |
| Quebrar PNB golden | **Alto** — é literalmente o dataset de referência já documentado (`diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md`), com hashes e dimensões registrados; qualquer patch precisa ser bit-a-bit invisível em modo legado (flag OFF). |
| Quebrar FNCS/bromélia/pipes | **Alto e já latente hoje** — `monitora_produtos_resolver_pipes_por_ponto` resolve por ponto absoluto para qualquer coluna que caia em `pipe_residual_operacional` via `monitora_produtos_classificar_pipe_coluna`, e essa classificação usa regex `"forma.*vida"` que capturaria bromélia condicional/esparsa (a mesma família de campo do Hotfix 03.5G) **antes mesmo de checar `pipe_residual_estruturado_elegivel`**. 03.5L-B não deve mexer nisso — é escopo de 03.5M ("migrar pipes por relevance/cardinalidade"), não de 03.5L. |
| Piorar performance | Baixo para diagnóstico opt-in (custo só quando a flag nova estiver ligada); médio se a ordenação por contrato único virar default e envolver joins/merges por linha em vez de só `setcolorder`. |

## 5. Pontos seguros de integração com o contrato único

Em ordem de segurança (mais seguro primeiro):

1. **Diagnóstico paralelo, opt-in, sem escrever no produto**: rodar `monitora_contrato_unico_diagnosticar_observado_canonico(names(registros))` (ou de `registros_corrig` no checkpoint 2) e gravar um CSV de auditoria à parte (mesmo padrão de `auditoria_registros_importados_resumo.csv`, mas um arquivo novo, ex. `auditoria_registros_importados_mapa_contrato.csv`), atrás de uma flag nova default `"N"`. Zero risco de alterar o produto.
2. **Relatório de cobertura**: comparar `names(registros_importados_final)` contra `template_sismonitora_129`/`por_atributo_canonico` e reportar o que bate/não bate — mesmo padrão de `monitora_registros_importados_auditar_comparacao_produtos`, só que usando o contrato em vez de comparar dois produtos entre si.
3. **Metadados auxiliares por coluna** (cardinalidade_operacional, status_confianca, origem_regra) anexados como **arquivo separado**, nunca como coluna nova dentro de `registros_importados.csv`.
4. **Ordenação/organização de colunas** — candidato de maior valor (atende a premissa #2 "organizada, pesquisável e comparável"), mas só como **opção explícita**, nunca substituindo `chaves_preferenciais` por padrão. Ex.: `monitora_registros_importados_saneado_preparar(dt, ordem = c("legado", "contrato_unico"))`, default `"legado"`.
5. **Mapeamento observado→canônico como sugestão, não decisão**: se o contrato único identificar candidato de rename (ex. coluna com sufixo técnico), reportar como sugestão em relatório — nunca renomear automaticamente.

**Nunca integrar em**: `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_dt_consolidar_aliases_colunas` (a função em si, não uma cópia de referência), `monitora_registros_importados_saneado_preparar`'s dedupe/`make.unique` — todos já validados, fora de escopo de 03.5L.

## 6. Proposta de patch cirúrgico para 03.5L-B

- **Funções novas** (todas puramente aditivas, mesmo padrão de 03.5I/J/K): 
  - `monitora_registros_importados_diagnostico_contrato_unico(dt, contrato = NULL, indices = NULL, contexto)` — chama `monitora_contrato_unico_diagnosticar_observado_canonico(names(dt), ...)`, grava CSV de auditoria à parte, retorna o `data.table` (não altera `dt`).
  - `monitora_registros_importados_comparar_ordem_legado_vs_contrato(dt, contrato = NULL)` — compara a ordem atual de `names(dt)` contra a ordem sugerida pelo contrato (`template_sismonitora_129` para os que batem + resto na ordem atual), reporta diffs, não aplica nada.
- **Flag nova, default OFF**: `MONITORA_OPCAO_DIAGNOSTICO_CONTRATO_UNICO_IMPORTADOS` (default `"N"`), seguindo o mesmo padrão de `Sys.getenv(..., unset=...)` já usado para as outras opções. Quando `"N"`, **zero código novo executa** no caminho de `registros_importados.csv`.
- **Pontos de integração mínimos**: 1 chamada adicional logo após cada um dos 2 `saneado_exportar()` existentes (L33791 e L37999), condicionada à flag nova, chamando só a função de diagnóstico — nunca alterando `registros`/`registros_corrig` nem o retorno de `saneado_exportar`.
- **Preservar bruto**: nenhuma mudança em `monitora_registros_importados_exportar` (bruto) nesta etapa — fica intocado.
- **Preservar importado atual em modo legado**: `monitora_registros_importados_saneado_preparar`/`saneado_exportar` continuam byte-idênticos por padrão; qualquer capacidade nova de ordenação (item 4 da seção 5) só ativa com parâmetro explícito, nunca por padrão.
- **Comparação legado vs contrato único**: produto de `monitora_registros_importados_comparar_ordem_legado_vs_contrato()` acima — relatório, não decisão.

## 7. Estratégia de testes

- **Sintético pequeno**: estender o conjunto já usado em 03.5K com casos específicos de `registros_importados` — `.id`, `MONITORA_ARQUIVO_ENTRADA`, `MONITORA_TIPO_ENTRADA` (devem cair em `tecnico_midia`/`fora_do_contrato`, nunca bloquear), e um nome com sufixo `__dup1` (deve ser reconhecido via normalização, não gerar erro).
- **Por cabeçalho**: reusar o cabeçalho de `PNSC_CIPO/2026` (já validado 129/129 em 03.5K) para exercitar `monitora_registros_importados_diagnostico_contrato_unico()` isoladamente — mas registrar explicitamente que esse cabeçalho tem o **formato de `registros_validados` (129 col)**, não o de `registros_importados.csv` real (documentado em ~176 colunas no baseline PNB, incluindo colunas brutas do XLSForm, técnicas e possivelmente `__dup`). Não é um substituto perfeito; serve para validar que a função não quebra com um cabeçalho real, não para validar cobertura completa de `registros_importados`.
- **PNSC_CIPO/2026 só até cabeçalho**: suficiente para 03.5L-B (mesma leitura de `head -n 1`, sem dado). Não é necessário além disso nesta fase.
- **Run manual completa**: **não é necessária para 03.5L-A** (esta auditoria) nem estritamente obrigatória para *escrever* o código de 03.5L-B (que pode ser validado por parse + testes isolados, mesmo padrão de 03.5I/J/K). **Será necessária depois que 03.5L-B estiver implementado**, para confirmar em ambiente real: (1) com a flag nova desligada (padrão), `registros_importados_bruto.csv` e `registros_importados.csv` saem **byte-idênticos** ao baseline PNB já documentado; (2) com a flag ligada, o diagnóstico novo é gerado sem alterar os dois produtos acima. Essa run é do usuário, no RStudio, com PNB (o único dataset com baseline hash já documentado) — não deve ser solicitada nem executada nesta subetapa nem pelo agente.
- **Produtos a ZIPar para validação** (quando a run acontecer, depois de 03.5L-B): `output/01_produtos_dados/registros_importados_bruto.csv`, `output/01_produtos_dados/registros_importados.csv`, o novo relatório de diagnóstico (`auditoria_registros_importados_mapa_contrato.csv` ou nome equivalente), e os logs de checkpoint (`auditoria_registros_importados_resumo.csv` de ambos os checkpoints).
- **Métricas a comparar**: dimensões (linhas × colunas) de `registros_importados.csv`/`_bruto.csv` idênticas ao baseline (176×24.241 documentado); **hash SHA-256 idêntico** com a flag OFF (prova de zero impacto); contagem de `pipe_indeterminado`/resíduo estruturado idêntica; tempo de execução (marcador interno `dec:`) dentro da faixa golden (~51-52s núcleo); com a flag ON, conferir que o relatório novo cobre 100% das colunas observadas (sem erro, sem NA silencioso).

## 8. Indicação objetiva sobre run manual completa

**Não é necessária agora (03.5L-A).** Também não é estritamente necessária só para *implementar* 03.5L-B (testes isolados/sintéticos/cabeçalho bastam para validar a função nova, mesmo padrão das 3 etapas anteriores). **Será necessária depois que o patch 03.5L-B estiver implementado e revisado**, como validação final antes de considerar a etapa concluída — a cargo do usuário, com PNB, comparando contra o baseline já documentado.

## 9. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
(sem saída — nenhuma alteração)
```

## 10. `git status --short`

```
?? diagnostics/auditoria_035l_registros_importados/
```
