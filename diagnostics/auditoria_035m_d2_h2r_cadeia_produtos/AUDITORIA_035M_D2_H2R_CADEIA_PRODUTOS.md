# Auditoria 03.5M-D2-H2R — Linhagem e replanejamento seguro da cadeia de produtos

## 1. Resumo executivo

A auditoria de linhagem confirma o diagnóstico que motivou a rejeição do H2B: o script é **híbrido**, não alinhado ao desenho conceitual `bruto → importado → base pré-painel/diagnósticos → corrigido → validado`. Dois achados centrais, ambos novos nesta etapa (não identificados nas auditorias H2A anteriores):

1. **`registros` (objeto bruto) é destruído deliberadamente logo após criar `registros_corrig`**: `registros -> registros_corrig; rm(registros)` (linhas 34648-34649). Isso significa que **qualquer rede de segurança tardia que dependa de reencontrar o objeto `registros` em memória (como o H2B abortado tentava fazer) está estruturalmente fadada a nunca reencontrá-lo** perto do fim do pipeline — o H2B não estava errado por acidente; ele atacava um sintoma sobre uma premissa (objeto bruto ainda disponível tardiamente) que o próprio desenho do script torna falsa por construção.
2. **`registros_importados.csv` é escrito duas vezes, com significados diferentes, sob o mesmo nome de arquivo** — uma vez a partir de `registros` (pré-`registros_corrig`, resíduo de pipe só informativo) e, mais tarde, sobrescrito a partir de `registros_corrig` já pós-tokenização (resíduo de pipe bloqueante). O próprio código documenta essa sobrescrita explicitamente ("Produto operacional de importação: **sobrescreve** registros_importados.csv..."), mas isso viola a premissa conceitual nº 2 desta tarefa ("Não deve ser sobrescrito por produto derivado de registros_corrig.csv sem novo nome/identidade").

`registros_validados.csv`, por outro lado, **já está corretamente isolado e gateado** por pendências impeditivas — não precisa de migração.

## 2. Status inicial

```
## dev-v2.6.2-h2r-cadeia-produtos
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
```
`Rscript -e 'invisible(parse(...)); cat("PARSE_OK\n")'` → `PARSE_OK`.

## 3. Commit/branch auditados

Branch: `dev-v2.6.2-h2r-cadeia-produtos`. HEAD: `730a718 fix: preserva nomes originais no bruto importado` (confirmado). O H2B abortado (backup externo em `~/Monitora_dev_backups/20260704_H2B_ABORTADO_PRE_H2R/`) não foi reaplicado — confirmado por `git diff --stat` vazio.

## 4. Veredito sobre o estado atual

**Híbrido.** Os produtos de entrada (`registros_importados_bruto.csv`) têm gate correto e independente de pendências; `registros_validados.csv` já é corretamente isolado. Mas `registros_importados.csv` mistura duas camadas conceituais diferentes sob o mesmo nome, e o objeto-fonte do snapshot bruto é destruído por desenho antes do fim do pipeline — isso não é um bug pontual, é uma característica estrutural do script que qualquer patch cirúrgico futuro precisa respeitar (não pode assumir que `registros` existirá tardiamente).

## 5. Mapa de linhagem de `registros`

| Evento | Linha | Origem/Destino |
|---|---|---|
| Criação | 31648 | `registros <- data.table::rbindlist(registros_batches, fill = TRUE, use.names = TRUE)` — nasce da concatenação dos CSVs lidos por lotes |
| Limpeza técnica | 34571 | `registros <- monitora_dt_remover_colunas_tecnicas_legadas(registros, "apos_concatenacao")` |
| Consolidação de duplicatas | 34580 | `registros <- monitora_dt_consolidar_colunas_duplicadas(registros)` |
| Consolidação de aliases | 34589 | `registros <- monitora_dt_consolidar_aliases_colunas(registros)` — ainda é snapshot técnico, sem normalização semântica |
| **Handoff para `registros_corrig`** | **34648** | `registros -> registros_corrig` (sintaxe R de atribuição à direita, equivalente a `registros_corrig <- registros`) |
| **Destruição do objeto bruto** | **34649** | `rm(registros); monitora_recurso_gc(...)` — **irreversível dentro da mesma execução; não há como recuperar o snapshot bruto depois deste ponto sem reler os CSVs de entrada** |

Produtos escritos a partir de `registros` (todos **antes** da linha 34648): `registros_importados_bruto.csv` (linha 34549, via `monitora_registros_importados_exportar`) e a **primeira** escrita de `registros_importados.csv` (linha 34604, via `monitora_registros_importados_saneado_exportar`, contexto `"pos_consolidacao_aliases_importacao_pre_registros_corrig"`).

## 6. Mapa de linhagem de `registros_corrig`

| Evento | Linha | Descrição |
|---|---|---|
| Nascimento | 34648 | Herda diretamente de `registros` (mesmo conteúdo, objeto renomeado) |
| Normalização de aspas | 34653 | `names(registros_corrig) <- str_replace_all(...)` |
| Consolidação (2ª vez) | 35144, 35153 | `monitora_dt_consolidar_colunas_duplicadas`/`monitora_dt_consolidar_aliases_colunas` novamente — sugere que a consolidação já feita em `registros` não é suficiente ou é redundante/defensiva |
| Anotação de tipo de entrada | 35159 | `monitora_io_anotar_tipo_entrada` |
| Deduplicação amostral | 35171 | `monitora_deduplicar_registros_amostrais` |
| Limpeza técnica pós-dedup | 35172 | `monitora_dt_remover_colunas_tecnicas_legadas(registros_corrig, "pos_deduplicacao")` |
| Deduplicação final por ponto/ano | 37355 | `monitora_deduplicar_final_por_ponto_ano` |
| Correção ponto_metro | 38818 | `monitora_correcao_corrigir_ponto_metro` |
| Quarentena de completude (101 pontos) | ~38797 (função `monitora_pre_painel_quarentenar_coletas_incompletas`) | Remove COLETAs incompletas — é aqui que ocorre a perda de linhas observada na run real (16968→16463) |
| Normalização condicional (`word(...,-1)`) | ~35887-36430 (não alterado nesta auditoria) | bromelioide/cactacea/orquidea/samambaia e `ponto_amostral`/`ponto_metro`/`Encostam` |
| Painel/correções assistidas | via `monitora_correcao_painel(registros_corrig, ...)` | Shiny, opcional |
| Checkpoint/exportação final | múltiplos pontos (seção 7) | `registros_corrig.csv` |

`registros_corrig` é usado como fonte por: **segunda escrita** de `registros_importados.csv` (linha 38854, pós-tokenização), painel Shiny, validação espacial (`monitora_espacial_executar`), estatísticas (`registros_corrig_stat.csv`), `registros_corrig.csv` e `registros_validados.csv`. Ou seja, `registros_corrig` é hoje o objeto "faz-tudo" central do script — exatamente o padrão "objeto mutável amplo" citado na justificativa desta tarefa.

## 7. Mapa de `registros_importados_bruto.csv`

- **Único ponto de escrita real**: linha 34549, via `monitora_registros_importados_exportar(registros, ...)`, gated por `MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL` (linha 34548).
- Fonte: `registros` (ainda bruto, pré-handoff).
- Função de escrita de baixo nível: `monitora_publicacao_h_fwrite_registros_importados()` (corrigida pelo Hotfix H1, preserva nomes originais).
- Produto canônico em `output/01_produtos_dados/`.
- **Não há tentativa tardia de reconstrução** no código atual (H2B, que tentaria isso, não foi aplicado). Se este ponto não materializar o arquivo, não há outro código no script que o faça depois — condição irreversível confirmada pela destruição do objeto-fonte na linha 34649.
- Falha aqui gera `stop()` explícito (linha ~34557-34563) — não é uma falha silenciosa.

## 8. Mapa de `registros_importados.csv`

**Dois pontos de escrita, mesma identidade de arquivo, semânticas diferentes:**

| # | Linha | Fonte | Contexto | `bloquear_pipe_residual` | Momento |
|---|---|---|---|---|---|
| 1 | 34604 | `registros` (pré-handoff) | `"pos_consolidacao_aliases_importacao_pre_registros_corrig"` | `FALSE` (informativo) | Antes de qualquer normalização de `registros_corrig` |
| 2 | 38854 | `registros_corrig` (pós-tokenização) | `"pre_painel_pos_extracao_tokens_operacionais"` | `TRUE` (bloqueante) | Depois de toda a cadeia `padronizacao_*` (incluindo o bloco `word(...,-1)`) |

O próprio código documenta a intenção (comentário linha 38842): **"Produto operacional de importação: sobrescreve `registros_importados.csv`..."** — a sobrescrita é intencional, não um bug de descuido. Mas ela viola a premissa conceitual desta tarefa: o arquivo nº 2 representa uma camada semanticamente diferente (pós-normalização operacional) do arquivo nº 1 (pré-consolidação, "produto comparável usado na curadoria por bolsista", conforme o próprio comentário da linha 34615). Ambos os usos são legítimos individualmente, mas compartilhar o mesmo nome de arquivo é o risco arquitetural central identificado nesta auditoria.

## 9. Mapa de `registros_corrig.csv`

Múltiplos pontos de escrita, todos usando `registros_corrig` como fonte:

- `monitora_execucao_gravar_checkpoint_parcial()` (linha 25248) — usado no caminho `painel_e_parar` (chamada em ~L39179) e no caminho `abrir_painel_cache`/`painel_incremental_registros_corrig` (`monitora_painel_cache_incremental_executar`, linha 26072).
- Caminho completo — `monitora_publicacao_aa_preparar_validar_registros_corrig()` + `monitora_publicacao_aa_exportar_registros_corrig_aprovado()` (chamadas em ~L39218-39230, já mapeadas na H2A).
- Ambos os caminhos passam pela mesma auditoria de pendências (`monitora_publicacao_ab_auditar_pendencias_impeditivas`), que grava `auditoria_pendencias_impeditivas_registros_corrig.csv`/resumo e seta `MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS` — **suficiente para indicar que não é produto final validado**, já que essa mesma flag bloqueia `registros_validados.csv` (seção 10).

## 10. Mapa de `registros_validados.csv`

- Função: `monitora_registros_validados_exportar()` (linha 29131).
- **Gate de pendências já correto e bem isolado** (linhas 29150-29159): se `MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS` for `TRUE`, a função retorna `invisible(list(bloqueado = TRUE, ...))` sem escrever nada — **não precisa de nenhuma migração**.
- Gate de opção: `monitora_validados_opcao_ativa()` (`MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS`).
- Idempotência: `MONITORA_REGISTROS_VALIDADOS_GERADO` evita regeneração redundante.
- Schema: `monitora_validados_schema_embutido()` — schema SISMONITORA fixo, sem colunas de status/selo adicionadas.
- **Nenhum risco de base ambígua identificado**: sempre recebe `registros_corrig` diretamente, nunca `registros_importados.csv` ou uma "base pré-painel" separada.

## 11. Consumidores por camada

| Consumidor | Objeto usado hoje | Deveria usar no desenho novo |
|---|---|---|
| Painel Shiny (`monitora_correcao_painel`) | `registros_corrig` | Mesmo — painel opera sobre o checkpoint operacional, correto |
| Relatórios pré-painel | `registros_corrig` (após tokenização) | Mesmo, mas com identidade de produto separada se "base pré-painel" virar produto persistido (ver seção 15) |
| Relatórios pós-painel | `registros_corrig` | Mesmo |
| Auditoria de pendências impeditivas | `registros_corrig` (via `monitora_publicacao_aa_preparar_validar_registros_corrig`) | Mesmo |
| Diagnóstico opt-in de pipes (03.5M-C/C2/D2) | `registros_corrig`, em múltiplos contextos (incl. pré-normalização) | Mesmo — já é o objeto certo |
| Validação espacial (`monitora_espacial_executar`) | `registros_corrig` | Mesmo |
| Estatísticas (`registros_corrig_stat.csv`) | `registros_corrig` | Mesmo |
| `registros_validados.csv` | `registros_corrig` | Mesmo (já correto) |
| `registros_importados.csv` (2ª escrita) | `registros_corrig` (pós-tokenização) | Deveria ser um produto com **nome próprio** (ex.: `base_pre_painel_operacional.csv`), não sobrescrever o nome do produto de importação (seção 15) |

**Nenhum consumidor downstream relevante lê `registros_importados.csv`/`registros_importados_bruto.csv` de volta do disco** — ambos são produtos terminais de auditoria/rastreabilidade, não realimentam o pipeline. Isso é uma boa notícia para a migração: renomear ou reestruturar esses dois produtos não deveria quebrar nenhum consumidor interno do script, só potencialmente scripts/usuários externos que dependem do nome `registros_importados.csv` (risco de compatibilidade, seção 16).

## 12. Flags relevantes

| Flag | Onde definida (default) | Onde derivada | Onde consumida |
|---|---|---|---|
| `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS` | L196 (default `"N"`), override L374-380 | `MONITORA_GERAR_REGISTROS_IMPORTADOS` (L494) | Compõe `MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL` |
| `MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL` | Derivada, L506 (`OR` entre `MONITORA_GERAR_REGISTROS_IMPORTADOS` e `MONITORA_GERAR_REGISTROS_IMPORTADOS_BRUTO_OBRIGATORIO`) | — | Gate único de L34548 (bruto), L34603 (importados 1ª escrita), L38852 (importados 2ª escrita) |
| `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS` | L133 (default `"N"`), override L365-371 | `MONITORA_GERAR_REGISTROS_VALIDADOS` (L493) | Gate de `monitora_registros_validados_exportar` (via `monitora_validados_opcao_ativa`) |
| `MONITORA_DIAGNOSTICO_PIPES_CONTRATO` | L218 (default `"N"`), override L390-396 (degrada para `"N"` se inválido) | — | `monitora_pipe_contrato_relatorio_optin()`, chamado pelo wrapper seguro em 3 pontos |
| `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS` | Análoga (03.5L-B) | — | `monitora_registros_importados_diagnostico_contrato_unico()`, chamado só na 1ª escrita de importados (L34625-34638), nunca na 2ª — **assimetria**: o diagnóstico de contrato único só roda sobre a versão pré-consolidação de `registros`, nunca sobre a versão pós-tokenização de `registros_corrig` |
| `MONITORA_MODO_EXECUCAO` | L131 (default `"completo"`), override L285-289 | `MONITORA_PARAR_APOS_REGISTROS_CORRIG` (L510), `MONITORA_MODO_FORCA_PAINEL` (L354) | Controla qual bloco de exportação de `registros_corrig.csv` roda |
| `MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE` | L522 (`FALSE`) | Setada em 3 pontos (L26073 via `<<-`, e nos 2 blocos de parada mapeados na H2A) | Gate do bloco "caminho completo" |
| Flags de painel/incremental | `MONITORA_MODOS_PAINEL_INCREMENTAL`, `MONITORA_MODOS_REGISTROS_CORRIG_ENTRADA`, `MONITORA_MODOS_REGISTROS_CORRIG_DESTINO` (L308-325) | — | Determinam se a execução usa `registros_corrig.csv` já existente como entrada em vez de reconstruir de `input/` |

## 13. Riscos para motor único

O contrato único (`monitora_contrato_unico_embutido`/`monitora_contrato_unico_indices`) já é consultado pelo diagnóstico opt-in de pipes (03.5M-C/C2/D2) e pelo diagnóstico de contrato único de importados (03.5L-B) — ambos **read-only, opt-in, default OFF**. Nenhuma migração de camada de produto deveria criar uma NOVA fonte paralela de contrato: qualquer "sidecar de status/selo" (seção 9 do checklist) deve **derivar** informação já calculada pelo contrato único/diagnósticos existentes, nunca reimplementar regras de classificação/cardinalidade. Risco concreto: se um futuro "produto base_pre_painel.csv" precisar de sua própria auditoria de pipes, ela deve reutilizar `monitora_pipe_contrato_diagnosticar_dataset()`/`resumir_diagnostico_dataset()` já existentes, não recriar lógica equivalente.

## 14. Riscos para painel

O painel consome `registros_corrig` diretamente (objeto em memória, não relê arquivo). Qualquer migração que insira uma camada nova ("base pré-painel" como objeto/produto separado) **antes** do painel não deve alterar o objeto que o painel efetivamente recebe (`registros_corrig`) nem o momento em que ele é passado — só precisa, no máximo, adicionar um passo de materialização/nomeação explícita **paralelo**, sem interpor uma transformação nova no caminho `registros_corrig → painel`. Pendências devem continuar visíveis no painel exatamente como hoje (a auditoria de pendências já roda antes do painel ter chance de ocultar algo, conforme H2A).

## 15. Riscos para validação final

`registros_validados.csv` já está corretamente isolado (seção 10) — o risco não está nele, está em garantir que nenhuma migração futura permita que `registros_importados.csv` ou uma futura "base pré-painel.csv" sejam **confundidos** com produto final validado. Recomendação: qualquer novo produto de camada intermediária deve ter nome que deixe isso inequívoco (ex.: sufixo `_operacional`/`_pre_painel`, nunca reaproveitar palavras como "validado"/"final"/"aprovado"). Status/selos devem ser associados via sidecar (arquivo `.csv` de metadados separado, referenciando `exec_id`/nome de arquivo), nunca como coluna dentro do CSV principal — consistente com a restrição already vigente para `registros_validados.csv` (schema rígido, sem colunas de selo).

## 16. Riscos para performance

- `registros_corrig` já é reaproveitado por referência/cópia conforme necessário ao longo do pipeline (padrão `data.table` já estabelecido) — nenhuma migração deveria introduzir uma releitura de disco de produtos já em memória.
- O ponto de maior risco de performance seria **tentar reconstruir `registros` (bruto) depois da linha 34649** — isso exigiria reler/reconcatenar todos os CSVs de entrada, uma operação potencialmente cara (a run real teve 16.968 linhas × 142 colunas no bruto). A migração recomendada (seção 18) evita isso completamente ao invés de contorná-lo.
- Sidecars de status/selo devem ser derivados de metadados leves já calculados (contagem de linhas/colunas, flags, resultado de pendências) — nunca variar em custo com o tamanho do dataset além de uma passada O(n) já feita por outra auditoria existente.

## 17. Riscos de compatibilidade pública

Usuários/scripts externos (ex.: rotina de curadoria por bolsista, mencionada no comentário da linha 34615) hoje esperam encontrar `registros_importados.csv` com o conteúdo da **1ª escrita** (pré-consolidação, "produto comparável usado na curadoria"). Se a migração simplesmente parar de sobrescrever esse arquivo com a versão pós-tokenização, o comportamento observado por esses usuários mudaria (o arquivo passaria a refletir só a 1ª escrita, nunca a 2ª) — **precisa de comunicação explícita e/ou período de transição com os dois nomes coexistindo** antes de qualquer remoção da sobrescrita atual.

## 18. Proposta de modelo de linhagem

| camada | produto_ou_objeto | nasce_de | momento_no_fluxo | pode_ter_pendencias | pode_sanitizar_tecnicamente | pode_sanitizar_semanticamente | pode_ser_sobrescrito | consumidores | relatorios_obrigatorios | status_sugerido | risco_atual | mudanca_recomendada |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| bruto | `registros_importados_bruto.csv` | leitura/concatenação (`registros`) | imediatamente após concatenação (L34549) | sim | não | não | não | auditoria/rastreabilidade externa | nenhum obrigatório além do `stop()` de falha de escrita | `snapshot_tecnico` | baixo | nenhuma (já correto) |
| importado (pré-consolidação) | `registros_importados.csv` — **escrita 1** | `registros` pós-alias | L34604, antes do handoff | sim (informativo) | sim (aliases/duplicatas) | não | **sim, pela escrita 2** | curadoria externa por bolsista | resumo de importação | `importado_pre_consolidacao` | **alto — identidade compartilhada com escrita 2** | renomear ou isolar (seção 19) |
| operacional pré-painel | `registros_importados.csv` — **escrita 2** | `registros_corrig` pós-tokenização | L38854, após `word(...,-1)` e cadeia `padronizacao_*` | sim (bloqueante) | sim | sim (via `padronizacao_*`) | não (é a versão final sob esse nome) | nenhum consumidor interno confirmado | auditoria de pipe residual bloqueante | `operacional_pre_painel` | **alto — mesmo nome da camada anterior** | dar produto/nome próprio (seção 19) |
| corrigido | `registros_corrig.csv` | `registros_corrig` pós-painel/correções | múltiplos checkpoints (parcial e completo) | sim (impeditivas e não-impeditivas) | sim | sim | sim (reexportado a cada checkpoint, por desenho) | painel, estatísticas, validação espacial, diagnósticos | `auditoria_pendencias_impeditivas_registros_corrig.csv` + resumo | `checkpoint_operacional` | baixo (já rotulado corretamente como checkpoint) | nenhuma |
| validado | `registros_validados.csv` | `registros_corrig` sem pendências impeditivas | pós-checkpoint, gated | não (bloqueado se houver) | não | não | sim, mas só quando `forcar=TRUE` e sem pendências | produto final/contratual | nenhuma pendência esperada por definição | `final_validado` | baixo (já corretamente isolado) | nenhuma |

## 19. Proposta de plano de migração incremental

- **H2R-A** (esta etapa): auditoria/relatório apenas — concluída.
- **H2R-B**: fixar identidade dos produtos sem alterar consumidores — documentar formalmente (em comentário no código, sem alterar lógica) que a escrita 1 e a escrita 2 de `registros_importados.csv` são camadas conceitualmente distintas, como preparação para B-seguinte.
- **H2R-C**: impedir a sobrescrita ambígua — a escrita 2 (L38852-38865) passa a gravar em um nome de arquivo **novo** (ex.: `registros_importados_operacional_pre_painel.csv`), preservando `registros_importados.csv` como resultado exclusivo da escrita 1. Requer avaliação de compatibilidade (seção 17) antes de remover a sobrescrita do nome antigo.
- **H2R-D**: garantir materialização do bruto no ponto correto — já é o caso hoje (seção 7); só formalizar teste de regressão que impeça reintrodução de qualquer tentativa tardia de reconstrução do bruto após a linha 34649.
- **H2R-E**: ajustar consumidores para a base pré-painel explícita, se necessário — como nenhum consumidor interno lê `registros_importados.csv` de volta (seção 11), esta etapa provavelmente não é necessária além de atualizar documentação/README.
- **H2R-F**: adicionar sidecar de status/selos — arquivo `.csv` de metadados separado por produto (ex.: `registros_importados_operacional_pre_painel_status.csv`), nunca coluna dentro do CSV principal.
- **H2R-G**: validação sintética — testes isolados cobrindo: escrita 1 preservada, escrita 2 sob novo nome, nenhuma coluna de status nos CSVs principais, `registros_validados.csv` inalterado.
- **H2R-H**: run real manual controlada, com console log preservado, para confirmar o comportamento em produção antes de qualquer commit definitivo.

## 20. Próximo patch mínimo recomendado

**Recomendação: H2R-C (renomear o produto da 2ª escrita), não H2R-B isoladamente.** Resposta às perguntas do checklist:

- **É seguro primeiro impedir a sobrescrita de `registros_importados.csv`?** Sim, é a mudança de maior valor e menor risco — a 2ª escrita (L38852-38865) já é uma chamada isolada, fácil de redirecionar para um novo nome de arquivo sem tocar em nenhuma lógica de tokenização/resolução de pipe.
- **É melhor criar produto novo `base_pre_painel.csv` ou nome mais específico?** Recomenda-se um nome que preserve a informação já presente no comentário do próprio código (linha 38842-38844): algo como `registros_importados_operacional_pre_painel.csv`, deixando claro que é pós-tokenização e usado só internamente antes do painel — evita confusão com a camada de "base pré-painel" conceitual mais ampla (que pode incluir diagnósticos, não só este produto específico).
- **É melhor criar apenas objeto interno sem produto novo?** Não recomendado — a 2ª escrita já existe hoje como produto persistido (é assim que a auditoria de pipe residual bloqueante é registrada); eliminar completamente o produto perderia rastreabilidade. Manter como produto, só sob nome próprio.
- **Como preservar compatibilidade com usuários atuais?** Recomenda-se um período de transição: continuar escrevendo `registros_importados.csv` (conteúdo da 1ª escrita, inalterado) **e** o novo `registros_importados_operacional_pre_painel.csv` (conteúdo que hoje sobrescreve o nome antigo), documentando a mudança no changelog/README antes de qualquer remoção definitiva da sobrescrita.
- **Como preservar o painel?** Sem impacto — o painel nunca leu `registros_importados.csv` de volta do disco; consome `registros_corrig` em memória diretamente.
- **Como preservar performance?** Sem impacto — é só uma mudança de nome de arquivo de destino na chamada já existente; nenhum recálculo adicional.

### Critérios de aceite para H2R-C

- `registros_importados.csv` (após a mudança) sempre reflete o conteúdo da 1ª escrita (pré-consolidação), nunca mais sobrescrito pela 2ª.
- Novo arquivo `registros_importados_operacional_pre_painel.csv` (ou nome equivalente decidido) recebe o conteúdo que hoje é escrito na 2ª chamada, com o mesmo `bloquear_pipe_residual=TRUE` e a mesma auditoria de pipe residual.
- Nenhuma alteração em `word(...,-1)`, resolvedores de pipe, painel, validação espacial, estatísticas ou `registros_validados.csv`.
- Nenhuma coluna de status/selo adicionada a nenhum CSV principal.
- Parse OK; teste sintético confirmando os 2 arquivos com conteúdos corretos e distintos.
- Documentação/changelog atualizados avisando sobre a mudança de significado de `registros_importados.csv` (ou sobre o novo arquivo, dependendo da estratégia de transição escolhida).

## 21. Confirmação de que não alterou lógica

Nenhuma edição foi feita a `monitora_campsav_alvo_global_v2.6.0.R` nesta tarefa — só leitura (`grep`/`Read`). O H2B abortado (backup externo) não foi reaplicado.

## 22. Confirmação de que não rodou PNB/FNCS

Confirmado — nenhuma execução do pipeline, nenhuma leitura de dado real.

## 23. Confirmação de que não usou dados reais

Confirmado — toda a auditoria foi feita por leitura estática do código-fonte.

## 24. Confirmação de que não houve git add/commit/push

Confirmado — ver seção 25.

## 25. `git status --short --branch`

```
## dev-v2.6.2-h2r-cadeia-produtos
?? diagnostics/auditoria_035m_d2_h2r_cadeia_produtos/
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

## 26. `git diff --stat`

```
$ git diff --stat
(sem saída — nenhuma alteração)
```

Nenhum `git add`, commit ou push foi executado.
