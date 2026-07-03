# Dev 03.5K — Mapa observado→canônico em modo diagnóstico (isolado)

## Resumo

Criadas 2 funções novas em `monitora_campsav_alvo_global_v2.6.0.R`: `monitora_contrato_unico_construir_candidatos_texto()` (helper) e `monitora_contrato_unico_diagnosticar_observado_canonico()` (função principal pedida). Recebem **apenas nomes de coluna** (nunca dado inteiro), resolvem cada nome contra o contrato único (03.5I) e seus índices (03.5J) por 5 estratégias exatas em ordem de especificidade, e retornam um `data.table` de diagnóstico — sem forçar decisão em caso de ambiguidade ou ausência de match. Nenhuma das duas é chamada por nenhum consumidor operacional.

Durante os testes isolados com o cabeçalho real do dataset representativo, dois problemas genuínos foram encontrados e corrigidos **na função `monitora_contrato_unico_embutido()` (03.5I)**, não apenas em 03.5K — ambos documentados abaixo, nenhum escondido:

1. **Bug de acesso** (só no script de teste, não no script real): confundi o retorno de `monitora_contrato_unico_indices()` (que é `list(indices=, perfis=, meta=)`) com a sublista `indices` propriamente dita. Corrigido na função real (linha ~32502): `indices <- monitora_contrato_unico_indices(contrato)$indices`.
2. **Bug real de dados, descoberto só ao testar contra o cabeçalho do dataset real**: as posições 9/10 do schema de 129 (`ea`/`ua`) caíam no fallback "metadado de pipeline" de `monitora_contrato_unico_embutido()`, criando um atributo canônico artificial chamado literalmente `"ea"`/`"ua"` — que então **vencia** a resolução por alias no 03.5K (por estar em prioridade mais alta), fazendo colunas observadas `"ea"`/`"ua"` resolverem para si mesmas em vez de para `estacao_amostral`/`unidade_amostral`. Corrigido diretamente em `monitora_contrato_unico_embutido()`: as posições 9/10 agora são anexadas ao atributo XLSForm real (mesmo conteúdo já correto de `monitora_esp_colunas_chave()`, Auditoria 03.5H3), preservando a cobertura de 1:129 sem inventar atributo. Ver seção "Confirmação: `ea`/`ua` preservados" abaixo.

Um terceiro ajuste (enriquecimento, não correção de bug) foi feito depois de constatar, no cabeçalho real, que o dataset usa a convenção de path do **template SISMonitora** (`atributo` do schema de 129, ex. `coletor/nome`, `data_hora/data`) como cabeçalho — diferente do `caminho_registro` cru do XLSForm (`amostragem/registro/coletor/nome`). Essa string já existia implicitamente no contrato (schema129), só não estava exposta como candidato de match; foi adicionada como campo `atributo_schema129` em `monitora_contrato_unico_embutido()` e incluída como candidato de mesma força que `caminho_registro` (tier 1) em 03.5K. Isso elevou a cobertura do dataset real de 113/129 para **129/129**.

## Localização das funções novas/alteradas

- `monitora_contrato_unico_construir_candidatos_texto()` — nova, ~linha 32471
- `monitora_contrato_unico_diagnosticar_observado_canonico()` — nova, ~linha 32538
- `monitora_contrato_unico_embutido()` (03.5I) — **alterada** (não nova): 3 linhas de `:=` existentes ganharam mais um campo (`atributo_schema129`), 1 bloco novo inserido (caso especial `ea`/`ua`), 1 linha nova inserida (`status_confianca` para `origem_schema129 == "alias_035h3"`). Nenhuma linha pré-existente foi removida ou teve seu comportamento anterior alterado para os demais 127 atributos.

## Dataset representativo usado

`extr_dados_form/PNSC_CIPO/2026/PNSC_CIPO_2026_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv`, localizado via `/home/dfed/dados_originais/_inventario_monitora_20260702_165610/_matriz_regressao_refinada/melhor_por_fonte_schema.csv` — exatamente o caso `PNSC_CIPO/2026` indicado na Estratégia 03.5H4. Confirmado por hash antes de qualquer leitura:

- SHA-256 no inventário: `879e80bc...585355cf6`
- SHA-256 do arquivo em disco: idêntico (conferido via `sha256sum`)
- 8585 linhas / 129 colunas / schema `xlsform_2025_129` (per inventário)

**Só o cabeçalho foi lido** (`head -n 1`), nunca nenhuma linha de dado. Nenhum outro arquivo de `dados_originais` foi acessado. PNB e FNCS não foram usados.

## Produtos diagnósticos gerados

- `diagnostics/dev_035k_mapa_observado_canonico/DEV_035K_MAPA_OBSERVADO_CANONICO.md` (este arquivo)
- `diagnostics/dev_035k_mapa_observado_canonico/mapa_observado_canonico_035k.csv` — 129 linhas (uma por coluna observada do cabeçalho real), 18 colunas de metadados de mapeamento (ver lista abaixo); **nenhum valor de célula do dataset**, só nomes de coluna e resultados do diagnóstico.
- `diagnostics/dev_035k_mapa_observado_canonico/resumo_mapa_observado_canonico_035k.csv` — 14 métricas agregadas.

Colunas do CSV de mapa: `coluna_observada, coluna_observada_normalizada, atributo_canonico_sugerido, caminho_registro_sugerido, name_curto_sugerido, metodo_match, prioridade_match, score_match, status_match, n_candidatos, candidatos_resumo, cardinalidade_operacional, origem_regra, status_confianca, estagio_aplicavel, observacao_diagnostica, bloqueia_migracao_automatica, contexto`.

## Métodos de resolução (ordem de prioridade)

1. **`caminho_registro` exato** — inclui, desde o ajuste desta etapa, tanto o path cru do XLSForm quanto `atributo_schema129` (path do template SISMonitora), tratados como mesma força.
2. **`name_curto` exato**
3. **`label` exato** — label 2025 (com/sem HTML) + labels históricos das outras 3 versões embutidas (`" | "`-separados, explodidos)
4. **`alias` explícito** — pipe + validados (histórico+adicionais) + cópia de referência da consolidação de colunas + `ea`/`ua` (03.5H3)
5. **normalizado** — mesmo pool acima + aliases, comparados após `monitora_contrato_unico_normalizar_texto_seguro()`
6. **Fuzzy — deliberadamente não implementado.** O único helper de fuzzy/score já existente (`monitora_correcao_candidatos_coluna_xlsform`) resolve na direção oposta (campo canônico conhecido → melhor coluna de um `dt` real já carregado) e depende de `dt`/`arquivo_xlsform`/`categoria`, não de uma lista de nomes. Adaptá-lo exigiria ler dado real ou reimplementar uma segunda lógica de score — violaria "não duplicar regra fora do contrato único" e "não depender de ler dados inteiros quando só nomes bastarem". Decisão documentada, não escondida.
7. **`sem_match`/`multiplo_ambiguo`** — terminal, nunca forçado.

`status_match` é reclassificado para `fora_do_contrato` quando o atributo resolvido é metadado de pipeline (schema-129 sem correspondência XLSForm), independentemente de qual tier o achou — é informação mais útil para quem consome o diagnóstico.

## Contagens principais (dataset real, 129 colunas observadas)

| Métrica | Valor |
|---|---:|
| Total de colunas observadas | 129 |
| Match exato por path | 114 |
| Match exato por name | 0 |
| Match exato por label | 0 |
| Match por alias | 0 |
| Match por normalização | 0 |
| Ambíguas | 0 |
| Sem match | 0 |
| Fora do contrato | 15 |
| Total de atributos canônicos distintos mapeados | **129** |
| Cobertura dos 129 atributos do template no dataset avaliado | **129/129** |
| Campos condicionais/esparsos observados | 10 |
| Campos ambíguos/indeterminados observados | 4 |
| Aliases `ea`/`ua` confirmados | 2 (`ea`→`estacao_amostral`, `ua`→`unidade_amostral`, ambos presentes no cabeçalho real) |

Este dataset específico já usa a convenção de export do template SISMonitora quase integralmente (daí 0 nas categorias `exato_name`/`exato_label`/`alias`/`normalizado`/`ambíguas`/`sem_match`) — as demais estratégias foram validadas separadamente no teste sintético (seção seguinte), não porque estejam com defeito aqui.

## Resultado dos testes isolados (14/14 + checagens extra)

Mesmo método das etapas anteriores: extração literal das linhas reais do script para fora do repositório, `Rscript` puro, sem PNB/FNCS/pipeline completo.

| # | Teste | Resultado |
|---|---|---|
| 1 | Parse do script inteiro | OK |
| 2 | `monitora_contrato_unico_embutido()` | OK |
| 3 | `monitora_contrato_unico_indices()` | OK |
| 4 | Chamada com vetor sintético (9 casos cobrindo todas as estratégias) | OK |
| 5 | Path exato (`amostragem/registro/forma_vida_nativa_lianas`) | `exato_path` |
| 6 | Name exato (`forma_vida_nativa_bromelioide`) | `exato_name` |
| 7 | Alias `ea`/`ua` | Ambos resolvem para `estacao_amostral`/`unidade_amostral` (agora via `exato_path`, graças ao `atributo_schema129`; a rota de alias explícito continua funcional e é a usada quando `atributo_schema129` não está presente, ex.: `"EA"` maiúsculo isolado) |
| 8 | Campo sem match (`coluna_completamente_inventada_xyz_999`) | `sem_match`, `bloqueia_migracao_automatica=TRUE` |
| 9 | Campo potencialmente ambíguo (`uc`) | Resolvido sem ambiguidade real (`n_candidatos=1`) — testado, não uma falha |
| 10 | Bromélia condicional/esparsa não forçada | `cardinalidade_operacional="estruturado_condicional_esparso"`, `bloqueia_migracao_automatica=TRUE`, observação explícita "requer 03.5M" |
| 11 | Cabeçalho do dataset representativo | 129/129 colunas processadas, sem erro |
| 12 | Retorna `data.table` | OK para os dois casos (sintético e real) |
| 13 | Não leu/gravou valor real | OK — grep no corpo da função por padrões de I/O vazio; única leitura de arquivo em toda a sessão de teste foi o cabeçalho (fora da função, no script de teste) |
| 14 | Nenhuma chamada operacional alterada | OK (confirmado também via `git diff`) |
| extra | Alias regressivo da 03.5G não reintroduzido em `monitora_pipe_aliases_campos_conhecidos()` | OK — 0 ocorrências de "bromélia" na tabela bruta |

## Confirmação: `ea`/`ua` preservados corretamente

Antes da correção (bug 2 acima), colunas observadas `"ea"`/`"ua"` resolviam para um atributo canônico artificial chamado literalmente `"ea"`/`"ua"` (`fora_do_contrato`), **não** para `estacao_amostral`/`unidade_amostral` — uma regressão silenciosa do requisito 03.5H3 que só se manifestou ao testar contra nomes de coluna reais. Depois da correção: `atributo_canonico_2025` para `ea`/`ua` no contrato passou a ser `amostragem/registro/estacao_amostral`/`amostragem/registro/unidade_amostral` (posições 9/10 do template, `status_confianca="alta"`), e não existe mais nenhum atributo canônico chamado literalmente `"ea"`/`"ua"` no contrato. Confirmado nos testes 4 e 7 acima.

## Confirmação: bromélia condicional/esparsa não resolvida por ponto absoluto

Nenhuma linha do código de 03.5K (nem do fix em 03.5I) referencia índice de ponto, `linha ==`, ou qualquer lógica de resolução posicional. As 3 formas de bromélia (nativa/exótica/seca-morta) aparecem no dataset real com `cardinalidade_operacional="estruturado_condicional_esparso"` e `bloqueia_migracao_automatica=TRUE`, com observação diagnóstica explícita apontando para 03.5M — exatamente o comportamento pedido, sem repetir a causa-raiz do Hotfix 03.5G.

## Confirmação: nenhum dado real gravado

- `mapa_observado_canonico_035k.csv` e `resumo_mapa_observado_canonico_035k.csv` contêm só nomes de coluna (já públicos no cabeçalho do CSV original) e metadados de contrato — nenhum valor de célula, nenhuma amostra de linha.
- A única leitura de arquivo real em toda esta tarefa foi `head -n 1`/`readLines(..., n=1)` do cabeçalho — nunca uma linha de dado.
- Nenhum arquivo foi copiado para dentro do repositório; o dataset original permanece em `/home/dfed/dados_originais/`, intocado.

## Confirmação: pipeline não conectado

`git diff -- monitora_campsav_alvo_global_v2.6.0.R`:

```
 monitora_campsav_alvo_global_v2.6.0.R | 260 +++++++++++++++++++++++++++++++++-
 1 file changed, 257 insertions(+), 3 deletions(-)
```

As 3 remoções são exclusivamente dentro de `monitora_contrato_unico_embutido()` (função 03.5I criada nesta mesma sessão/branch, ainda não conectada a nada) — cada uma é uma linha de `:=` existente que ganhou mais um campo (`atributo_schema129`), sem alterar o comportamento anterior para os 127 atributos não afetados pelo caso especial `ea`/`ua`. Confirmado especificamente que nenhuma função sensível/operacional (`monitora_correcao_colunas_chave`, `monitora_esp_colunas_chave`, `monitora_pipe_aliases_campos_conhecidos`, `monitora_validados_aliases`, `monitora_validados_schema_embutido`, `monitora_correcao_xlsforms_embutidos`, `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae`) aparece no diff. `monitora_contrato_unico_diagnosticar_observado_canonico` só aparece em sua própria definição — nenhum outro ponto do script a chama.

## `git status --short`

```
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035k_mapa_observado_canonico/
```

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
