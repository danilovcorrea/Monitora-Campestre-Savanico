# Dev 03.5I — `monitora_contrato_unico_embutido()` (fonte única embutida, isolada)

## Resumo

Criadas 3 funções novas em `monitora_campsav_alvo_global_v2.6.0.R`, entre `monitora_produtos_classificar_pipe_coluna()` (linha ~31843) e `monitora_registros_importados_exportar()` (agora deslocada pela inserção): `monitora_contrato_unico_labels_sem_html()`, `monitora_contrato_unico_cardinalidade_operacional()` e `monitora_contrato_unico_embutido()`. As três são **puramente aditivas** (330 linhas inseridas, 0 removidas/alteradas) e **não são chamadas por nenhum código existente** — só aparecem na própria definição e num literal de string dentro dos metadados que a função retorna. Nenhum consumidor operacional (importação, correção, pipe, painel, validados) foi tocado.

Durante os testes isolados, dois problemas reais foram encontrados e corrigidos ainda dentro do escopo desta etapa (só na função nova, sem tocar consumidores):
1. A posição 129 do schema de 129 (`uuid`, nível coleta) ficava sem representação porque o único campo XLSForm com esse `name` curto (`amostragem/registro/uuid`) já tinha sido reivindicado pela posição 126 — corrigido trocando o critério de "sem correspondência" de existência de nome para posição efetivamente atribuída.
2. Um teste meu estava incorreto (não o código): esperava zero ocorrências de "bromélia observada" em toda a tabela de aliases, mas essa string é um alias **pré-existente e legítimo** de `monitora_validados_aliases_adicionais()` (anterior ao Hotfix 03.5G, consolidado aqui exatamente como pedido). O teste foi corrigido para checar especificamente a origem `pipe_aliases_campos_conhecidos` (deve ser zero) separada de `validados_aliases` (deve conter, é esperado).

## Localização da função

- `monitora_contrato_unico_labels_sem_html()` — linha 31876
- `monitora_contrato_unico_cardinalidade_operacional()` — linha 31885
- `monitora_contrato_unico_embutido()` — linha 31908 (fecha em ~32175)

Inseridas no mesmo bloco lógico das funções de pipe/contrato já existentes (`monitora_pipe_aliases_campos_conhecidos`, `monitora_pipe_coluna_classificar_natureza`, `monitora_produtos_classificar_pipe_coluna`), antes de `monitora_registros_importados_exportar`.

## Estrutura retornada

`monitora_contrato_unico_embutido()` retorna uma **lista de 5 elementos** (não uma tabela única "mega-wide", para não explodir cardinalidade combinando atributo × choice):

| Elemento | Conteúdo |
|---|---|
| `$atributos` | 1 linha por atributo canônico. Colunas: `atributo_canonico_2025`, `caminho_registro`, `name_curto`, `label_2025_com_html`, `label_2025_sem_html`, `labels_historicos_com_html`, `labels_historicos_sem_html`, `type_xlsform`, `tipo_base`, `list_name`, `required`, `relevant`, `campo_pai`, `nivel_schema129`, `formato_schema129`, `posicao_schema129`, `cardinalidade_operacional`, `ambiguo_entre_versoes`, `status_confianca`, `origem_regra` |
| `$aliases` | Formato longo: `caminho_registro`, `alias`, `origem_alias` (`pipe_aliases_campos_conhecidos` / `validados_aliases` / `dt_consolidar_aliases_colunas_ref` / `requisito_035h3`) |
| `$choices` | Passthrough da tabela `opcoes` do dump XLSForm (já é fonte única, só adaptada) |
| `$dependencias` | União de `relevant` do XLSForm + `monitora_correcao_dependencias_padrao()`, via `monitora_correcao_unificar_dependencias()` (já existente, reaproveitada sem alteração) |
| `$meta` | 1 linha: versão canônica, contagens, timestamp, origem |

Todas as tabelas de entrada (`campos`, `opcoes`, `dependencias`, `schema129`) são copiadas com `data.table::copy()` antes de qualquer mutação — a função nunca escreve por referência no cache global (`MONITORA_PUBLICACAO_AE_XLSFORM21_*`) nem em nenhuma tabela retornada por outra função.

## Contagens principais (teste isolado, sem dado real)

| Métrica | Valor |
|---|---:|
| `n_atributos` (`$atributos`) | 144 |
| `n_aliases` (`$aliases`) | 182 |
| `nrow($choices)` | 1349 |
| `nrow($dependencias)` | 477 |
| Posições do schema-129 cobertas | **129 de 129** |
| Categorias de cardinalidade distintas presentes | **7 de 7** (`texto_livre`=52, `select_multiple`=22, `estruturado_completo_por_ponto`=6, `estruturado_condicional_esparso`=10, `tecnico_midia`=30, `fora_do_contrato`=17, `ambiguo_indeterminado`=7) |

`144` atributos = 129 do schema-129 (matched ou marcados `fora_do_contrato` quando são metadado de pipeline) + campos do XLSForm 21FEV25 que existem no formulário mas não estavam nos 129 (marcadores de repeat, `estacao_amostral`/`unidade_amostral` — estes últimos também recebem alias `ea`/`ua` explícito).

## Resultado dos testes isolados

Testes rodados via extração literal (mesmas linhas do script real, sem reescrever nada) para um script isolado fora do repositório + `Rscript` puro — mesmo método já usado nas auditorias 03.5H2/03.5H3. Nenhum dado real, nenhum PNB/FNCS, nenhum pipeline.

| # | Teste | Resultado |
|---|---|---|
| 1 | Definições carregam sem erro | OK |
| 2 | `monitora_contrato_unico_embutido()` chamada sem erro | OK |
| 3 | Retorno é uma lista com os 5 elementos esperados | OK |
| 4 | Contagem de atributos canônicos | 144 (ver tabela acima) |
| 5 | Presença dos 129 atributos do schema validado | **129/129** (após correção do bug de posição 129) |
| 6 | Aliases explícitos `ea`/`ua` presentes com origem `requisito_035h3` | OK (`ea`/`EA` → `amostragem/registro/estacao_amostral`; `ua`/`UA` → `amostragem/registro/unidade_amostral`) |
| 7 | 7 categorias de cardinalidade representadas | OK (todas as 7 aparecem) |
| — | Bromélia nativa condicional/esparsa não forçada | OK — cai naturalmente em `estruturado_condicional_esparso` por ter `relevant` não vazio, sem nenhum código especial-caso |
| — | Alias regressivo da 03.5G ausente da tabela de **pipe** | OK — 0 linhas com `origem_alias == "pipe_aliases_campos_conhecidos"` contendo "bromélia observada"; `monitora_pipe_aliases_campos_conhecidos()` bruto confirmado sem "bromélia" |
| 8 | Sem dependência de arquivo externo (grep no corpo por `read.csv`/`readxl`/`fread(file`/`readLines`/`url(`) | OK — nenhuma ocorrência |
| 9 | Nenhum consumidor operacional carregado/chamado no teste | OK — só as funções de contrato foram extraídas e usadas |

## Confirmação: pipeline não foi conectado

- `grep -n "monitora_contrato_unico_embutido"` no script inteiro retorna só a linha de definição (31908) e uma string literal dentro do próprio retorno de metadados (linha ~32171, texto descritivo, não uma chamada) — **nenhum outro ponto do script referencia ou chama a função nova**.
- `git diff -- monitora_campsav_alvo_global_v2.6.0.R` confirma **330 inserções, 0 remoções** — nenhuma linha existente foi alterada.
- Busca específica confirmou que `monitora_correcao_colunas_chave`, `monitora_esp_colunas_chave`, `monitora_pipe_aliases_campos_conhecidos` e `monitora_validados_aliases` (as 4 funções mais sensíveis a uma conexão indevida) não aparecem no diff — suas definições permanecem exatamente como estavam.

## Riscos remanescentes para 03.5J/03.5K

- **Performance ainda não medida em escala real**: os testes rodaram só sobre o dump embutido (centenas de linhas), não sobre um produto de 20 mil+ linhas. O uso de `match()` (vetorizado) evita loop linha a linha, mas o laço `for` sobre aliases (pipe + validados + consolidação de colunas, ~80 chaves) ainda é O(chaves × nrow(campos25)) via filtro repetido em `primeiro_caminho_por_norm()` — aceitável para a escala atual (~144 atributos), mas deve virar join vetorizado antes de qualquer uso em runtime (03.5J).
- **`alias_consolidacao_colunas_ref` é uma cópia literal, não uma referência viva** ao `alias_map` de `monitora_dt_consolidar_aliases_colunas()` (L26359) — se aquele alias_map for editado no futuro, esta cópia fica desatualizada silenciosamente. 03.5J/03.5K devem decidir se extraem esse alias_map para uma função própria reaproveitável por ambos os lados.
- **`campo_pai` via `dependencias$parent_name`** só cobre os casos já modelados em `dependencias` (XLSForm + regra padrão); grupos comuns (`begin_group`/`end_group`) não têm dependência explícita e caem no fallback textual (`sub("/[^/]+$", ...)`), que é só sintático, não semântico — suficiente para diagnóstico, não para decisão automática.
- **`status_confianca`/`origem_regra` ainda não distinguem "confiança média por inferência posicional, não confirmada com dado real"** (o caso específico do 3º item da Auditoria 03.5F, `__dup2`) — hoje cairia em `media_sem_schema129` ou similar, sem essa nuance específica; considerar refinar em 03.5K junto com o mapa observado→canônico.
- **Nenhum destes riscos afeta o pipeline hoje**, porque a função não está conectada a nada — ficam registrados para quando 03.5J (índices/caches) e 03.5K (mapa diagnóstico com 1 dataset real) forem executados.

## `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 330 ++++++++++++++++++++++++++++++++++
 1 file changed, 330 insertions(+)
```

## `git status --short`

```
 M monitora_campsav_alvo_global_v2.6.0.R
```

Nota: os relatórios das auditorias 03.5H/H2/H3/H4 e dos requisitos 03.5I, criados em turnos anteriores desta sessão, aparecem agora committados no histórico (`69d2c40 docs: consolida auditorias da fonte unica de contrato`) — esse commit não foi feito por mim: em nenhuma dessas tarefas anteriores `git add`/`git commit` foi executado (todas paravam em `git status --short` mostrando `??`); o commit foi criado externamente entre turnos. Nesta tarefa (03.5I dev), nenhum `git add`, `commit` ou `push` foi executado.
