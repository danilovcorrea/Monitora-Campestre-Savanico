# Dev 03.5J — Índices/caches derivados do contrato único (isolado)

## Resumo

Criadas 3 funções novas em `monitora_campsav_alvo_global_v2.6.0.R`, imediatamente após `monitora_contrato_unico_embutido()` (03.5I) e antes de `monitora_registros_importados_exportar()`: `monitora_contrato_unico_normalizar_texto_seguro()` (helper), `monitora_contrato_unico_validar_estrutura_indices()` (validador estrutural) e `monitora_contrato_unico_indices()` (função principal pedida). As três são **puramente aditivas** (282 linhas inseridas, 0 removidas/alteradas) e **não são chamadas por nenhum código existente** — só aparecem em suas próprias definições. Nenhum consumidor operacional foi tocado.

Todos os 16 índices e os 6 perfis são **projeções/agrupamentos em memória do mesmo `monitora_contrato_unico_embutido()`** — nenhuma regra, alias, tipo ou label novo foi introduzido; `severidade` e `estagio_aplicavel` (as duas únicas anotações genuinamente novas) são derivadas por uma tabela de mapeamento explícita e auditável a partir de `cardinalidade_operacional`/`status_confianca`/`ambiguo_entre_versoes`, todas já calculadas em 03.5I — não competem com o contrato, só o reagrupam para consumo por etapa.

## Localização das funções novas

- `monitora_contrato_unico_normalizar_texto_seguro()` — linha 32185
- `monitora_contrato_unico_validar_estrutura_indices()` — linha 32197
- `monitora_contrato_unico_indices()` — linha 32253 (fecha em ~32456)

## Estrutura retornada por `monitora_contrato_unico_indices()`

```
list(
  indices = list( <16 data.tables, ver abaixo> ),
  perfis  = list( <6 elementos, ver abaixo> ),
  meta    = data.table(gerado_em, n_indices, n_perfis, validado, origem)
)
```

Assinatura: `monitora_contrato_unico_indices(contrato = NULL, validar = TRUE)` — se `contrato` for `NULL`, chama `monitora_contrato_unico_embutido()` internamente; todas as tabelas de entrada são copiadas com `data.table::copy()` antes de qualquer mutação (mesmo padrão de segurança do 03.5I, evita mutação por referência do contrato original ou de qualquer cache global).

## Índices criados (16/16)

| Índice | Chave (`setkey`) | O que é |
|---|---|---|
| `por_atributo_canonico` | `atributo_canonico_2025` | Cópia completa de `atributos` |
| `por_caminho_registro` | `caminho_registro` | Só atributos com path XLSForm |
| `por_name_curto` | `name_curto` | Só atributos com nome curto |
| `por_label_normalizado` | `label_normalizado` (novo, via helper) | Label sem HTML, normalizado |
| `por_alias_normalizado` | `alias_normalizado` (novo, via helper) | Todos os aliases explícitos (pipe + validados + consolidação + `ea`/`ua`) |
| `por_list_name` | `list_name` | Atributos `select_one`/`select_multiple` por domínio |
| `por_choice_name` | `(list_name, name)` | Passthrough de `choices` |
| `por_relevance_campo_pai` | `campo_pai` | Atributos com campo pai conhecido (via `dependencias` ou path) |
| `por_cardinalidade_operacional` | `cardinalidade_operacional` | As 7 categorias |
| `por_estagio_aplicavel` | `estagio` | Formato longo atributo×estágio (ver mapeamento abaixo) |
| `por_severidade` | `severidade` (novo, derivado) | `baixa`/`media`/`alta` |
| `por_origem_regra` | `origem_regra` | Rastreabilidade de onde cada atributo veio |
| `template_sismonitora_129` | ordenado por `posicao_schema129` | Projeção exata das 129 posições |
| `atributos_tecnicos_pipeline` | — | União de `tecnico_midia` + `fora_do_contrato` |
| `atributos_ambiguos_indeterminados` | — | `ambiguo_indeterminado` ou tipo ambíguo entre versões |
| `campos_condicionais_esparsos` | — | `estruturado_condicional_esparso` (inclui as 3 bromélias nativa/exótica/seca-morta) |

Mapeamento `cardinalidade_operacional → estágios aplicáveis` (tabela explícita no código, não hardcode disperso):
`texto_livre`/`select_multiple`/`estruturado_completo_por_ponto` → todos os 6 estágios; `estruturado_condicional_esparso` → até `pos_painel_corrig` (nunca `export_registros_validados`/`estatisticas_graficos` por padrão — não resolvido ainda); `tecnico_midia` → só `importacao`+`pos_painel_corrig`; `fora_do_contrato` → `pre_painel`+`painel_edicao`+`export_registros_validados` (nunca `importacao`, pois nunca vem do XLSForm); `ambiguo_indeterminado` → só `pre_painel`.

## Perfis derivados criados (6/6)

| Perfil | Conteúdo | Linhas (teste isolado) |
|---|---|---:|
| `perfil_importacao` | atributo, path, name curto, labels (com/sem HTML), aliases conhecidos (lista), cardinalidade — sem sanitização | 127 |
| `perfil_pre_painel` | cardinalidade, confiança, severidade, campo pai, relevant, `bloqueia_registros_validados` (só `TRUE` para `ambiguo_indeterminado`/confiança `baixa_ambiguo`) | 114 |
| `perfil_painel_edicao` | atributos editáveis (exclui `ambiguo_indeterminado` e `tecnico_midia`), com domínios/choices, labels, relevance/campo pai | 107 |
| `perfil_pos_painel_corrig` | atributos com cardinalidade resolvida (exclui só `ambiguo_indeterminado`), tipo, relevance, campo pai, confiança | 137 |
| `perfil_export_registros_validados` | `list(projecao = <129 linhas ordenadas>, assertivas = <5 checagens estruturais baratas>)` | 129 (projeção) / 5 (assertivas) |
| `perfil_estatisticas_graficos` | só atributos de conteúdo real (`texto_livre`/`select_multiple`/`estruturado_completo_por_ponto`) com confiança `alta`/`media` — exclui condicional/esparso ainda não resolvido | 78 |

## Contagens principais (teste isolado, sem dado real)

- 16/16 índices presentes; 6/6 perfis presentes.
- `template_sismonitora_129`: **129 linhas, posições 1:129 sem lacuna/duplicata**.
- 7/7 categorias de cardinalidade representadas em `por_cardinalidade_operacional`.
- `perfil_export_registros_validados$assertivas`: `n_posicoes_esperado=129`, `n_posicoes_observado=129`, `n_gaps_posicoes=0`, `n_duplicatas_posicoes=0`, `n_condicional_esparso_no_template=10`.

## Resultado dos testes isolados (14/14 + 2 checagens extra)

Mesmo método das etapas anteriores: extração literal das linhas reais do script (nenhuma reescrita) para um script fora do repositório, `Rscript` puro, sem PNB/FNCS/dado real.

| # | Teste | Resultado |
|---|---|---|
| 1 | Parse do script inteiro | OK |
| 2 | Definições carregam sem erro | OK |
| 3 | `monitora_contrato_unico_embutido()` chamada | OK |
| 4 | `monitora_contrato_unico_indices()` chamada (com `validar=TRUE`) | OK, validação interna passou |
| 5 | Retorno é lista estruturada (`indices`, `perfis`, `meta`) | OK |
| 6 | Todos os 16 índices mínimos presentes | OK |
| 7 | `template_sismonitora_129` com 129 posições, sem lacuna | OK |
| 8 | Aliases `ea`/`ua` presentes em `por_alias_normalizado` | OK — `ea`/`EA`→`estacao_amostral`, `ua`/`UA`→`unidade_amostral` |
| 9 | 7 categorias de cardinalidade representadas | OK |
| 10 | 6 perfis mínimos presentes | OK |
| 11 | `perfil_export_registros_validados` sem validação pesada | OK — só 5 contagens estruturais sobre o próprio contrato, nenhuma leitura de arquivo |
| 12 | Nenhuma chamada operacional alterada | OK (confirmado também via `git diff`, seção abaixo) |
| 13 | Sem leitura de arquivo externo | OK — grep no corpo das 3 funções por padrões de I/O retornou vazio |
| 14 | Não rodou PNB/FNCS | OK — nenhum dataset real foi carregado em nenhum teste |
| extra | Bromélia condicional/esparsa listada, não resolvida por ponto absoluto | OK — as 3 variantes (nativa/exótica/seca-morta) aparecem em `campos_condicionais_esparsos` com `relevant` intacto; nenhuma referência a resolução por ponto no código |
| extra | Alias regressivo da 03.5G não reintroduzido em `monitora_pipe_aliases_campos_conhecidos()` | OK — 0 ocorrências de "bromélia" na tabela bruta |
| extra (negativo) | Validador estrutural rejeita entrada incompleta | OK — `monitora_contrato_unico_validar_estrutura_indices(list(por_atributo_canonico=data.table()))` lança erro listando os 15 índices ausentes, confirmando que a validação não é um no-op |

## Confirmação: pipeline não conectado

- `git diff -- monitora_campsav_alvo_global_v2.6.0.R`: **282 inserções, 0 remoções**.
- `grep -n "monitora_contrato_unico_indices"` no script inteiro retorna só linhas dentro da própria função (definição, mensagens de erro internas, string de metadados) — nenhum outro ponto do script chama a função.
- Confirmado especificamente que `monitora_correcao_colunas_chave`, `monitora_esp_colunas_chave`, `monitora_pipe_aliases_campos_conhecidos`, `monitora_validados_aliases`, e as funções do 03.5I (`monitora_contrato_unico_embutido`, `monitora_contrato_unico_labels_sem_html`, `monitora_contrato_unico_cardinalidade_operacional`) **não aparecem no diff** — nenhuma foi modificada.

## Confirmação: nenhuma leitura de dado real / PNB/FNCS

Todos os testes rodaram exclusivamente sobre as tabelas embutidas no próprio script (dump XLSForm + schema hardcoded + aliases já existentes). Nenhum arquivo de `/home/dfed/dados_originais` foi acessado nesta tarefa.

## Riscos remanescentes para 03.5K

- **`por_estagio_aplicavel` e `por_severidade` são a primeira camada de "opinião" sobre o contrato** (decidem o que é editável, o que bloqueia, o que alimenta gráfico) — ainda não validados contra nenhum caso real; 03.5K (mapa observado→canônico com 1 dataset representativo, diagnóstico apenas) é o lugar certo para confirmar se essas classificações batem com o comportamento hoje esperado no painel antes de qualquer conexão.
- **`aliases_por_caminho`/`choices_por_list_name` usam `unique()` sobre listas** — com contrato pequeno (144 atributos) o custo é desprezível; se a fonte única crescer (ex.: mais versões de XLSForm), vale revisar se `by=` agrupado continua barato o suficiente ou se compensa pré-computar como índice adicional.
- **`perfil_painel_edicao` inclui `fora_do_contrato`** (ex.: `validado`, `obs_validacao`, campos de workflow de validação) junto com atributos de survey — é uma decisão defensável (esses campos são de fato editados no painel), mas é uma escolha de modelagem que 03.5K/03.5N devem confirmar contra o comportamento real do painel antes de qualquer conexão.
- **Nenhum destes riscos afeta o pipeline hoje** — a função não está conectada a nada.

## `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 282 ++++++++++++++++++++++++++++++++++
 1 file changed, 282 insertions(+)
```

## `git status --short`

```
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035j_indices_contrato_unico/
```

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
