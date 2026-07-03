# Dev 03.5L-B — Diagnóstico de contrato único para `registros_importados.csv` (opt-in, flag OFF por padrão)

## Resumo

Implementado, conforme o desenho da Auditoria 03.5L-A, um diagnóstico **puramente opt-in** de contrato único sobre `registros_importados.csv`: 1 flag nova (default `"N"`), 2 funções novas (mapa observado→canônico + comparação de ordem/duplicidade), integradas nos 2 pontos de exportação já identificados na auditoria. Com a flag desligada (padrão), **zero código novo executa** — as duas funções retornam `invisible(NULL)` na primeira linha, sem ler, escrever ou alterar nada. Com a flag ligada, geram só relatório à parte; nenhuma delas reordena, renomeia ou sanitiza o produto real.

## Flag criada

```r
MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "N"
```

- Default: **`"N"` (OFF)**.
- Segue o mesmo padrão de `Sys.getenv(..., unset=...)` já usado pelas outras opções (`MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS` etc.), configurável por variável de ambiente com o mesmo nome.
- Diferença deliberada de uma flag operacional: valor inválido (fora de `"S"`/`"N"`) faz a flag **degradar silenciosamente para `"N"`** em vez de `stop()` — coerente com "nunca bloquear fluxo quando o diagnóstico falhar", já que isto é uma flag só de diagnóstico, não uma que decide geração de produto operacional.

## Funções criadas

1. **`monitora_registros_importados_diagnostico_contrato_unico(registros_importados, contexto, output_dir, log_dir, exec_id)`** — checa a flag primeiro (retorna `invisible(NULL)` se OFF); usa só `names(registros_importados)`; chama `monitora_contrato_unico_diagnosticar_observado_canonico()`; grava, só se a flag estiver ON, 3 arquivos em `output_dir/diagnosticos_contrato_unico_registros_importados/` (mapa CSV, resumo CSV, `.txt` curto) + espelha o resumo em `log_dir`; nunca altera o objeto de entrada; retorna o diagnóstico invisivelmente.
2. **`monitora_registros_importados_comparar_ordem_legado_vs_contrato(registros_importados, diagnostico, contrato, indices, contexto, output_dir, log_dir, exec_id)`** — a partir do diagnóstico já calculado, monta uma tabela de comparação (posição legada × atributo canônico sugerido × posição sugerida pelo template), identifica colunas fora do contrato, ambíguas e **duplicatas de atributo canônico** (mais de uma coluna observada apontando para o mesmo atributo); grava 2 CSVs de relatório; nunca reordena `registros_importados` de verdade — a "ordem sugerida" é só uma coluna a mais no relatório.

Ambas as funções são wrappeadas em `tryCatch` internamente (erro vira `warning()`, nunca propaga) e as chamadas de integração também são envolvidas em `try(..., silent=TRUE)` — dupla camada de segurança.

## Pontos de integração

Exatamente os 2 pontos identificados na Auditoria 03.5L-A, ambos dentro do mesmo bloco `if (isTRUE(get0("MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL", ...)))` já existente (ou seja, só roda quando `registros_importados.csv` já ia ser gerado de qualquer forma):

- **Checkpoint 1** (linha ~34028, logo após `monitora_perf_registrar_checkpoint("registros_importados_saneado", ...)`) — diagnostica `registros` (objeto pré-tokenização), `contexto="checkpoint1_pre_tokenizacao"`.
- **Checkpoint 2** (linha ~38251, logo após `monitora_perf_registrar_checkpoint("registros_importados_operacional_tokenizado", ...)`) — diagnostica `registros_corrig` (objeto pós-tokenização, o que efetivamente é gravado como `registros_importados.csv` final), `contexto="checkpoint2_pos_tokenizacao"`.

Nenhuma chamada foi adicionada na exportação do produto **bruto** (`registros_importados_bruto.csv`) — fora de escopo, conforme a própria auditoria (o bruto é um produto deliberadamente não-processado, sem correspondência 1:1 com nomes canônicos de contrato).

## Confirmações de restrição

- **Flag default é OFF**: `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "N"` — confirmado por leitura direta do código e pelo teste 2 (ver seção de testes).
- **Não mexeu em pipes**: `monitora_produtos_resolver_pipes_por_ponto` e `monitora_bloquear_pipe_residual_produto` não aparecem no diff (confirmado por `git diff | grep`).
- **Não mexeu no saneamento**: `monitora_registros_importados_saneado_preparar` não aparece no diff.
- **Não mexeu em bruto**: `monitora_registros_importados_exportar` (writer do bruto) e `monitora_publicacao_h_fwrite_registros_importados` não aparecem no diff — só a linha de assinatura de `monitora_registros_importados_exportar` aparece como contexto de hunk (linha não modificada, imediatamente após o bloco novo inserido antes dela).
- **Não alterou `registros_corrig`/painel/`registros_validados`**: nenhuma função relacionada a esses produtos foi tocada; as novas funções só *leem* `names()` do objeto, nunca gravam nele.
- **Não reordena/renomeia o produto real**: confirmado por desenho (as duas funções novas nunca chamam `setcolorder`/`setnames` sobre `registros_importados`/`registros_corrig`) e pelos testes 3 e 6 (objeto de entrada idêntico antes/depois, com a flag ON).
- **Não aplica sanitização final, não aplica `solo_nu`, não resolve bromélia por ponto absoluto**: nenhuma dessas operações existe em nenhuma das 2 funções novas — elas só classificam e reportam; a bromélia condicional/esparsa aparece no teste 5 corretamente marcada `cardinalidade_operacional="estruturado_condicional_esparso"` e `bloqueia_migracao_automatica=TRUE`, sem nenhuma tentativa de resolução.
- **Não esconde pendências**: colunas sem match e ambíguas aparecem explicitamente no relatório com `status_match`/`observacao_diagnostica` próprios, nunca omitidas.

## Resultado dos testes

Mesmo método das etapas anteriores (extração literal das funções reais do script para fora do repositório + `Rscript` puro), mais um bug de infraestrutura de teste corrigido no processo (não afeta o script real): o script auxiliar de extração de funções (usado só nesta sessão de trabalho, fora do repositório) não lidava corretamente com assinaturas de função em múltiplas linhas — corrigido antes de rodar os testes abaixo.

| # | Teste | Resultado |
|---|---|---|
| 1 | Parse do script inteiro | OK |
| 2 | Sintético com `data.table` pequeno, flag ON: path exato, alias `ea`/`ua`, campo fora do contrato, campo ambíguo | OK — todos os 6 casos resolvidos/classificados corretamente (ver tabela abaixo) |
| 3 | Objeto de entrada não alterado (names/conteúdo) | OK, com flag OFF e com flag ON |
| 4 | Flag OFF → nenhuma saída diagnóstica criada | OK — retorno `NULL`, 0 arquivos |
| 5 | Flag ON → saídas diagnósticas criadas | OK — 3 arquivos em `output_dir` + 1 espelho em `log_dir` |
| 6 | Diagnóstico usa só nomes de coluna | OK — grep no corpo da função por `registros_importados[[.]]`/`$` (leitura de valor) retornou vazio; usa só `names(registros_importados)` |
| 7 | Nenhuma chamada operacional alterada fora dos pontos condicionados | OK (confirmado via `git diff`, seção acima) |
| 8 | Não rodou PNB/FNCS/pipeline completo | OK — só `data.table` sintético vazio em toda a sessão de teste |

Conteúdo do teste sintético (6 colunas observadas):

| Coluna observada | Atributo canônico sugerido | `status_match` | `cardinalidade_operacional` | Bloqueia migração |
|---|---|---|---|---|
| `amostragem/registro/forma_vida_nativa_lianas` | mesmo path | `exato_path` | `texto_livre` | não |
| `ea` | `amostragem/registro/estacao_amostral` | `exato_path` (via `atributo_schema129`) | `estruturado_completo_por_ponto` | não |
| `ua` | `amostragem/registro/unidade_amostral` | `exato_path` | `estruturado_completo_por_ponto` | não |
| `forma_vida_nativa_bromelioide` | `amostragem/registro/forma_vida_nativa_bromelioide` | `exato_name` | `estruturado_condicional_esparso` | **sim** |
| `protocolo` | `protocolo` | `fora_do_contrato` | `fora_do_contrato` | não |
| `coluna_inventada_xyz` | — | `sem_match` | — | **sim** |

`monitora_registros_importados_comparar_ordem_legado_vs_contrato()` também testada isoladamente: gerou corretamente a coluna `posicao_contrato_sugerida`/`ordem_sugerida_rank` sem alterar `names(dt_sintetico)`, e um caso extra (`"uuid"` isolado) confirmou resolução correta para `fora_do_contrato` (mesmo comportamento já validado em 03.5K).

## Necessidade de run manual completa

**Não solicitada nesta implementação.** Depois que 03.5L-B for revisado e (quando o usuário decidir) commitado/enviado, será necessário rodar manualmente no RStudio com o dataset PNB, comparando contra o baseline golden já documentado em `diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md`, para confirmar:

1. Com a flag **OFF (padrão)**: `registros_importados_bruto.csv` e `registros_importados.csv` saem **byte-idênticos** ao baseline (mesmo hash SHA-256, mesmas dimensões 176×24.241) — prova de que a integração não tem nenhum efeito colateral em modo legado.
2. Com a flag **ON**: os relatórios novos em `output/diagnosticos_contrato_unico_registros_importados/` são gerados sem erro e sem alterar os dois produtos acima.

Essa run é do usuário; não foi executada nem solicitada nesta tarefa.

## Resultado da notificação

- `notify-iphone "Claude concluiu 03.5L-B no Monitora"` — comando **não encontrado** neste ambiente (`command not found`).
- Fallback `curl` para `https://ntfy.sh/fedora-danilo-8f43a91c-20260702` — **enviado com sucesso** (HTTP 200, resposta confirmando `topic="fedora-danilo-8f43a91c-20260702"`, mensagem entregue).

## `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 260 ++++++++++++++++++++++++++++++++++
 1 file changed, 260 insertions(+)
```

260 inserções, **0 remoções** — confirmado puramente aditivo.

## `git status --short`

```
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035l_b_diagnostico_registros_importados/
```

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
