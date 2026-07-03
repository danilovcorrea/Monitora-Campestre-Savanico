# Auditoria 03.5M-A — Pipes por relevance/cardinalidade: mapa de risco + protótipo isolado

## 1. Resumo executivo

03.5M-A audita, sem substituir nada, o mecanismo operacional atual de resolução de "|" em `registros_importados.csv`/`registros_corrig.csv` e cria um **protótipo isolado, não conectado**, que classifica qualquer coluna pela cardinalidade do contrato único (03.5I/J/K) e resolve (ou diagnostica por que não resolve) um valor pipe-separado conforme essa cardinalidade — incluindo, pela primeira vez neste projeto, uma resolução **por elegibilidade/relevance** (não por índice de ponto absoluto) para campos condicionais/esparsos como a bromélia do Hotfix 03.5G.

Achado central confirmado (já mapeado em parte na Auditoria 03.5L-A, aprofundado aqui): o mecanismo hoje em produção (`monitora_produtos_resolver_pipes_por_ponto`) resolve **todo** campo classificado `pipe_residual_operacional`/`pipe_residual_revisar` por índice de ponto absoluto — sem distinguir `select_one` completo de `select_multiple` nem de condicional/esparso. Isso é seguro hoje só porque, no vocabulário atual (`monitora_pipe_coluna_classificar_natureza`), a bromélia (e as demais formas condicionais) caem em `pipe_indeterminado`/nunca em `pipe_residual_operacional` — não porque o resolvedor saiba diferenciar; ele simplesmente nunca chega a tentar resolver esses campos porque o **classificador de natureza** os marca como indeterminados antes, não porque o **resolvedor por ponto** tenha uma noção de esparsidade. 03.5M migra exatamente essa lacuna para uma solução explícita, sem tocar no mecanismo validado.

O protótipo isolado (3 funções novas, não conectadas) foi testado com 8 cenários sintéticos cobrindo todas as 7 categorias de cardinalidade + o caso `ea`/`ua` — todos passaram, incluindo a demonstração completa da resolução por elegibilidade de um campo condicional/esparso com múltiplos pontos, replicando exatamente a semântica que teria evitado a regressão do Hotfix 03.5G.

## 2. Base de partida confirmada

- Commit base: `4f54812 fix: preserva checkpoint registros corrig com pendencias nao relacionadas`.
- A run PNB `35lc_warning.zip` (fora do escopo desta auditoria reexecutar) já confirmou que o Hotfix 03.5L-C resolveu o abort de persistência: `registros_importados_bruto.csv`, `registros_importados.csv` e `registros_corrig.csv` materializados; auditoria de persistência sem falhas (369 `ok_aplicacao_persistiu` + 6 `ok_exclusao_persistiu` + 5 `ok_persistiu_por_efeito_especifico_outras_formas` + 1 `ok_estado_final_contratual_consistente`); `registros_validados.csv` corretamente bloqueado por pendências impeditivas remanescentes. 03.5M-A parte dessa base publicada, sem reexecutar a run.
- O warning do relatório consolidado de validação (`data.table::unique`) permanece fora de escopo — nenhuma evidência encontrada nesta auditoria que o relacione a pipes/relevance/cardinalidade.

## 3. Inventário das funções de pipe atuais

| Função | Linha (aprox.) | Papel | Status |
|---|---:|---|---|
| `monitora_produtos_resolver_pipes_por_ponto` | ~24686 | **Resolve** "\|" por índice de ponto/COLETA absoluto, só para colunas `pipe_residual_operacional`/`pipe_residual_revisar` | Operacional, validado, **não tocado** |
| `monitora_bloquear_pipe_residual_produto` | ~24782 | Audita/bloqueia resíduo de "\|" remanescente após a resolução acima | Operacional, validado, **não tocado** |
| `monitora_produtos_classificar_pipe_coluna` | ~24677 (morta) / ~31917 (viva) | Classifica coluna em `pipe_esperado_em_bruto`/`pipe_residual_tecnico_tolerado`/`pipe_permitido_texto_livre`/`pipe_residual_operacional`/`pipe_residual_revisar`/`pipe_indeterminado` | Duas definições (padrão já conhecido do arquivo, ver Auditoria 03.5H); a viva já consulta o contrato via `monitora_pipe_coluna_classificar_natureza`. **Não tocada** |
| `monitora_pipe_coluna_classificar_natureza` | ~31872 | Classifica natureza (texto/estruturado/ambíguo/indeterminado) por 4 estratégias exatas contra o dump XLSForm + `monitora_pipe_aliases_campos_conhecidos` | **Não tocada** |
| `monitora_pipe_decidir_natureza_candidatos` | ~31852 | Helper: decide texto vs. estruturado vs. ambíguo a partir de candidatos de `tipo_base` | **Não tocada** |
| `monitora_pipe_aliases_campos_conhecidos` | ~31825 | Tabela pequena de aliases explícitos de pipe (liana + 2 `impact_manejo_uso`) | **Não tocada**; confirmado sem "bromélia" (teste extra) |

Nenhuma outra função no script trata "\|" como separador posicional de célula — a única outra ocorrência de `strsplit(..., "|")` no arquivo (`monitora_correcao_parse_serial_generico`, ~L12650) é um parser de lista de índices de linha para auditoria/bookkeeping do painel, não relacionado a resolução de valor de célula.

## 4. Onde há pipe posicional absoluto hoje

Só em um lugar: dentro de `monitora_produtos_resolver_pipes_por_ponto`, o laço `for (ii in idx) { toks <- strsplit(x0[ii], "|", ...); ... novo <- toks[kk] }`, onde `kk` vem de `seletor` — um índice calculado por posição da linha dentro da COLETA (via coluna de ponto amostral quando disponível, senão posição sequencial). Esse mecanismo:

- É aplicado a **qualquer** coluna classificada `pipe_residual_operacional`/`pipe_residual_revisar` pelo classificador — **sem diferenciar `select_one` de `select_multiple`**, e sem nenhuma noção de "condicional/esparso".
- Hoje não afeta campos condicionais/esparsos (bromélia etc.) só porque `monitora_pipe_coluna_classificar_natureza` os deixa em `pipe_indeterminado` (sem alias reconhecido) — é uma proteção **indireta**, não uma regra de cardinalidade explícita. Se um alias novo algum dia resolver esses campos para `pipe_residual_estruturado_elegivel` sem também resolver a esparsidade (exatamente o que aconteceu no Hotfix 03.5G), o mesmo resolvedor por ponto absoluto voltaria a corromper os dados.
- Também não distingue `select_multiple` — se um campo `select_multiple` algum dia cair em `pipe_residual_operacional` (a classificação atual permite isso via regex `forma.*vida`), o resolvedor trataria seus tokens como "um por ponto" em vez de "conjunto por linha", o que é semanticamente errado. Não há evidência de que isso já tenha acontecido em produção (nenhum caso reportado), mas é uma lacuna estrutural real, não hipotética.

## 5. Matriz de risco por cardinalidade

| Cardinalidade (contrato único) | Estratégia correta de pipe | Resolvedor atual entende? | Risco de migrar agora |
|---|---|---|---|
| `texto_livre` | Preservar como está, nunca separar | Sim, indiretamente (`pipe_permitido_texto_livre` nunca entra no resolvedor) | **Baixo** — já protegido |
| `select_multiple` | Tratar como conjunto de tokens, não posicional | **Não** — se cair em `pipe_residual_operacional`, é tratado como posicional | **Médio** — lacuna estrutural real, sem caso conhecido de dano hoje |
| `estruturado_completo_por_ponto` | Token *i* = ponto *i* (posicional simples) | Sim — é exatamente o que o resolvedor faz | **Baixo** — comportamento atual já correto para este caso |
| `estruturado_condicional_esparso` | Token *i* = *i*-ésimo ponto elegível pelo parent/relevance, não ponto absoluto | **Não** — resolvedor não tem noção de elegibilidade; só não afeta esses campos hoje porque o classificador de natureza os deixa `pipe_indeterminado` | **Alto se migrado sem o protótipo de elegibilidade** — é a causa-raiz exata do Hotfix 03.5G |
| `tecnico_midia` | Não inferir nada, nunca tratar como posicional | Parcialmente — regex de nome (`foto\|imagem\|...`) cobre a maioria, mas é heurística de nome, não do contrato | **Baixo-médio** — funciona na prática, mas por acaso de nomenclatura, não por regra |
| `fora_do_contrato` | Diagnosticar, nunca resolver | Sim — não tem `tipo_base`, então `monitora_pipe_coluna_classificar_natureza` retorna indeterminado | **Baixo** |
| `ambiguo_indeterminado` | Diagnosticar, nunca forçar | Sim — `pipe_ambiguo` tem prioridade explícita sobre heurística de nome (Hotfix 03.5C-C) | **Baixo** |

## 6. Campos que podem ser migrados com baixo risco

- Todos os `texto_livre` (ex.: liana nativa, `amostragem/registro/forma_vida_nativa_lianas`) — já preservados; migrar só formaliza a mesma decisão via contrato em vez de heurística.
- Todos os `estruturado_completo_por_ponto` (ex.: `amostragem/registro/form_veg`, `estacao_amostral`/`unidade_amostral` = `ea`/`ua`) — resolução posicional simples já é o comportamento correto e já é o que acontece hoje.
- `fora_do_contrato`/`ambiguo_indeterminado` — já corretamente nunca resolvidos; migrar só troca a fonte do diagnóstico.

## 7. Campos que exigem relevance/elegibilidade antes de migrar

Toda a família `estruturado_condicional_esparso`: bromélia, cactácea, orquídea, samambaia × nativa/exótica/seca-morta (até 12 combinações no contrato, das quais 10 têm `relevant` resolvido hoje). Migrar qualquer um desses campos para resolução automática **exige** o protótipo de elegibilidade (seção 9) com contexto real de linha (valores do campo pai `forma_vida_nativa`/`forma_vida_exotica`/`forma_vida_seca_morta` para todos os pontos da COLETA) — nunca só a cardinalidade isolada.

## 8. Campos que devem permanecer preservados como texto

Toda a categoria `texto_livre` do contrato (52 atributos no snapshot mais recente de `monitora_contrato_unico_indices()`) — nenhuma mudança de comportamento recomendada, só confirmação via contrato.

## 9. Campos ambíguos/fora do contrato

- `ambiguo_indeterminado` (7 atributos no snapshot mais recente): tipo diverge entre versões do XLSForm embutido — nunca resolver, sempre reportar.
- `fora_do_contrato` (17 atributos): metadado de pipeline (`protocolo`, `validado` etc.) — não são campos de XLSForm, `|` neles (se ocorrer) não tem semântica de ponto/token nenhuma; diagnosticar como está fora do escopo de pipe de formulário.

## 10. Protótipo isolado criado

Três funções novas em `monitora_campsav_alvo_global_v2.6.0.R`, entre `monitora_registros_importados_comparar_ordem_legado_vs_contrato` (03.5L-B) e `monitora_registros_importados_exportar` — **nenhuma chamada por nenhum consumidor operacional**, confirmado por `grep` (só se chamam entre si).

1. **`monitora_pipe_contrato_classificar_coluna(coluna, contrato, indices)`** — reaproveita `monitora_contrato_unico_diagnosticar_observado_canonico()` (03.5K) e anexa `estrategia_pipe`, uma das: `preservar_texto_livre` / `tratar_como_conjunto_tokens` / `resolver_por_ponto_absoluto_permitido` / `resolver_por_elegibilidade_relevance_necessaria` / `nao_inferir_sem_regra_explicita` / `diagnosticar_fora_do_contrato` / `diagnosticar_ambiguo_nao_forcar` / `diagnosticar_sem_match_nao_forcar`.
2. **`monitora_pipe_contrato_avaliar_relevance_simples(relevant_expr, valores_parent)`** — parser conservador que só reconhece o padrão `selected(${campo}, 'token')` (opcionalmente combinado por `and`, o único operador observado nas expressões `relevant` do contrato); qualquer outro operador (`or`/`not`/comparação) faz a função devolver `NULL` em vez de arriscar uma interpretação errada.
3. **`monitora_pipe_contrato_resolver_tokens_isolado(valor, coluna, contexto_linha, contrato, indices)`** — resolve (ou diagnostica) um valor conforme a estratégia da coluna. Para `estruturado_condicional_esparso`, exige `contexto_linha$valores_parent` (valores do campo pai para todos os pontos do grupo) e `contexto_linha$ponto_indice`; sem isso, devolve diagnóstico explícito (`depende_de_parent_relevance_contexto_insuficiente`) — nunca resolve às cegas. Quando o contexto é suficiente, mapeia token *i* → *i*-ésimo ponto elegível (não ponto absoluto), detecta divergência estrutural (`n_tokens != n_pontos_elegíveis`, a mesma condição que gerou o resíduo do Hotfix 03.5G) e recusa resolver nesse caso.

Nenhuma das três funções lê arquivo, escreve produto, ou usa `assign()`/`<<-`/`data.table::set()` fora de objetos locais da própria chamada.

## 11. Testes sintéticos realizados e resultados

Extração literal das funções reais (mesmas linhas do script, sem reescrever nada) para fora do repositório, `Rscript` puro. Nenhum dado real, nenhum PNB/FNCS, nenhuma pipeline. (Um bug de infraestrutura de teste foi corrigido no processo: o script auxiliar de extração de funções, usado só nesta sessão de trabalho, não lidava com chaves `{`/`}` literais dentro de string de regex — não afeta o código real, só a forma de testá-lo isoladamente.)

| # | Cenário | Resultado |
|---|---|---|
| 1 | Liana texto livre com pipe | `preservado_texto_livre`, valor intacto |
| 2 | `select_multiple` (`tipo_forma_vida`/Encostam) com pipe | `conjunto_tokens`, sem mapeamento posicional |
| 3 | Campo completo por ponto (`form_veg`) com/sem contexto | Com `ponto_indice`: `resolvido_por_ponto_absoluto`. Sem contexto: `sem_contexto_ponto_nao_resolvido`, não força |
| 4a | Bromélia condicional/esparsa, sem contexto de relevance | `depende_de_parent_relevance_contexto_insuficiente` — **não resolve por ponto absoluto** |
| 4b | Bromélia com contexto completo (grupo de 5 pontos, 2 elegíveis: 3 e 5; pedindo ponto 5) | `resolvido_por_elegibilidade_relevance`, retorna o 2º token (ponto 5 = 2º elegível) — correto |
| 4c | Mesmo grupo, pedindo ponto 3 (1º elegível) | Retorna o 1º token — correto |
| 4d | Mesmo grupo, pedindo ponto não elegível (ponto 1) | `ponto_nao_elegivel_valor_esperado_vazio`, resultado `NA` — correto, sem erro |
| 4e | Divergência estrutural (2 tokens, 100 "elegíveis") | `contagem_elegiveis_diverge_tokens_nao_resolvido` — recusa resolver, mesma causa-raiz do Hotfix 03.5G detectada e bloqueada |
| 5 | Campo ambíguo (`uuid`) | Classificado, `diagnosticar_fora_do_contrato` (é `fora_do_contrato` no contrato, não ambíguo neste caso — comportamento correto) |
| 6 | Campo fora do contrato (`protocolo`) | `diagnosticar_fora_do_contrato` |
| 7 | `ea`/`ua` | `estruturado_completo_por_ponto` → `resolver_por_ponto_absoluto_permitido` (resolução de `select_one` comum, **não** lógica de esparsidade indevida) |
| 8 | Coluna totalmente inventada | `sem_match` → `diagnosticar_sem_match_nao_forcar` |
| extra | Alias regressivo da 03.5G em `monitora_pipe_aliases_campos_conhecidos()` | Confirmado ausente |

Todos os 8 cenários pedidos + 1 checagem extra passaram.

## 12. Proposta de 03.5M-B

1. **Não conectar ainda ao pipeline real.** 03.5M-B deve continuar em modo diagnóstico/comparação (mesmo padrão da 03.5L-B): rodar o protótipo em paralelo ao resolvedor atual sobre um dataset já materializado (`registros_importados.csv`/`registros_corrig.csv` de uma run já feita), comparando resultado por resultado, sem escrever produto novo — só relatório.
2. **Fechar a lacuna de contexto de linha**: hoje `contexto_linha` é fornecido manualmente pelo chamador/teste; 03.5M-B precisa de uma função que **derive** `valores_parent`/`ponto_indice` a partir de um `data.table` real de uma COLETA (agrupamento por `COLETA`+ordenação por ponto), ainda como função isolada, não conectada.
3. **Cobrir `select_multiple` explicitamente** na comparação (seção 5: risco médio) — não há caso conhecido de dano hoje, mas 03.5M-B deve confirmar, via diagnóstico sobre dados já materializados, que nenhum campo `select_multiple` real cai em `pipe_residual_operacional` hoje (se cair, é achado a reportar antes de qualquer migração, não a corrigir nesta subetapa).
4. **Não substituir `monitora_produtos_resolver_pipes_por_ponto`/`monitora_bloquear_pipe_residual_produto` em 03.5M-B** — a substituição real (03.5M-C ou etapa posterior) só deve acontecer depois que a comparação diagnóstica cobrir 100% das colunas reais de pelo menos um dataset golden sem divergência inesperada.

## 13. Quando será necessária nova run PNB/FNCS

**Não nesta subetapa nem na 03.5M-B**, enquanto 03.5M-B continuar em modo diagnóstico/comparação sobre produtos já materializados (não precisa rodar o pipeline de novo, só ler um `registros_corrig.csv` já existente de uma run anterior para comparar). Uma run PNB completa só será necessária **quando houver proposta concreta de substituir** `monitora_produtos_resolver_pipes_por_ponto` operacionalmente — nesse ponto, sim, comparar hash/dimensões contra o baseline golden já documentado, mais uma run FNCS dedicada para confirmar que a bromélia (COLETA 11168, o caso original do Hotfix 03.5G) resolve corretamente pela nova lógica de elegibilidade. Nenhuma das duas é necessária para concluir 03.5M-A.

## 14. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 185 +++++++++++++++++++++++++++++++++
 1 file changed, 185 insertions(+)
```

185 inserções, **0 remoções** — confirmado puramente aditivo. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna`, `monitora_pipe_coluna_classificar_natureza` e `monitora_persist_reclassificar_triout_por_efeito_proprio` (Hotfix 03.5L-C) não aparecem no diff, exceto como contexto de hunk não modificado.

## 15. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/auditoria_035m_a_pipes_relevance_cardinalidade/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked, cópia da run de validação do usuário) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
