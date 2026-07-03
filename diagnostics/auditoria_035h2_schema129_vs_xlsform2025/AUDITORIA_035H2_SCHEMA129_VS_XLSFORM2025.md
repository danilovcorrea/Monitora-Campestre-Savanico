# Auditoria 03.5H2 — Schema hardcoded de 129 atributos vs. XLSForm 2025 embutido

## 1. Resumo executivo

Dos 129 atributos de `monitora_validados_schema_embutido()` (L27280–27379), **113 batem diretamente** com um campo do dump XLSForm `21FEV25_nSrC9X3` embutido em `monitora_correcao_xlsforms_embutidos()` (L3883–6139): 91 por `caminho_registro` completo (match exato) e 22 por `name` curto (equivalência direta, sem span/sufixo). Dos **16 que não bateram** por comparação literal, **14 são metadados administrativos do pipeline** (`coleta`, `data_do_registro`, `protocolo`, `validado`, `validador` etc.) que nunca existiram no XLSForm — são adicionados depois, no processo de validação — e **2 são um caso real de nomenclatura divergente**: `ea`/`ua` (schema) vs. `estacao_amostral`/`unidade_amostral` (XLSForm), o mesmo conceito sob nomes diferentes. Nenhum atributo do schema ficou genuinamente sem correspondência conceitual no formulário.

Do lado do XLSForm 2025, dos 117 campos de survey únicos (excluindo `begin_group`/`end_group`/`start`/`deviceid`/`note`), **apenas 2 ficam fora dos 129** — exatamente `estacao_amostral` e `unidade_amostral`, o mesmo par acima, já contado. Os 6 marcadores estruturais de repeat (`begin_repeat`/`end_repeat` de `coletor`, `amostragem`, `registro`) não geram coluna e são corretamente ausentes do schema.

**Conclusão prática**: o schema de 129 não é uma fonte paralela desalinhada — é, em essência, `campos(21FEV25)` restrito a linhas de survey reais + 15 atributos de metadado de pipeline que o XLSForm nunca teve. A única divergência de nomenclatura real (`ea`/`ua`) é resolvível por alias simples, não estrutural.

## 2. Metodologia

Comparação estática, sem executar o pipeline (PNB/FNCS) e sem ler dado real. As duas funções de contrato (`monitora_validados_schema_embutido`, `monitora_correcao_xlsforms_embutidos`, e sua dependência direta `monitora_correcao_fread_tsv_embutido`) foram extraídas **literalmente** (mesmas linhas do script, sem reescrever nenhuma lógica) para um arquivo isolado e carregadas com `Rscript` — é a "chamada leve de inspeção estática" autorizada pelo escopo: nenhuma outra função do script foi carregada, nenhum produto (`registros_*`) foi lido, nenhuma execução do pipeline ocorreu. As tabelas resultantes (`campos`, `opcoes`, `dependencias`, `arquivos` do XLSForm; a tabela de 129 linhas do schema) são as mesmas que o script produziria em runtime — não foram editadas nem resumidas manualmente.

Normalização usada para comparar nomes (réplica fiel de `monitora_validados_normalizar_nome`, L27572): remove tags HTML, transliterar para ASCII, minúsculas, remove sufixos `__dup[0-9]+` e `.[0-9]+`, substitui não-alfanumérico por `_`. Três estratégias de match, em ordem de especificidade: (1) `caminho_registro` completo igual ao `atributo`; (2) `name` (segmento final do path) igual ao segmento final do `atributo`; (3) `name` igual ao `atributo` completo (sem uso nesta base). Nenhum "fuzzy"/aproximado foi usado — só igualdade exata pós-normalização, coerente com a diretriz de não usar dado real e não introduzir heurística nova.

Resultado bruto (129 linhas, uma por atributo do schema, com estratégia de match e metadados do candidato encontrado) ficou em arquivo de trabalho fora do repositório (`/tmp/.../comparacao_035h2.csv`), usado só para escrever este relatório — não foi commitado nem copiado para `diagnostics/`.

## 3. Q1 — Os 129 atributos aparecem no XLSForm 2025 embutido?

113 de 129 (87,6%) aparecem diretamente. Os 16 restantes são tratados na seção 5.

## 4. Q2 — Há atributos no schema hardcoded que não existem no XLSForm 2025?

Sim, 16, mas de duas naturezas bem diferentes (ver seção 5): 14 são metadado de pipeline (nunca existiram em nenhum XLSForm, de nenhuma versão) e 2 (`ea`, `ua`) existem no XLSForm sob outro nome.

## 5. Detalhe dos 16 não-batidos

| Atributo (schema) | Nível | Classificação | Nota |
|---|---|---|---|
| `coleta` | coleta | metadado técnico necessário | Identificador da coleta no pipeline, não pergunta de formulário |
| `data_do_registro` | coleta | metadado técnico necessário | Timestamp de recebimento/registro do pipeline |
| `data_do_recebimento` | coleta | metadado técnico necessário | idem |
| `ultima_edicao` | coleta | metadado técnico necessário | Controle de edição no painel |
| `protocolo` | coleta | metadado técnico necessário | Identificador do protocolo/produto, não campo XLSForm |
| `ciclo` | coleta | metadado técnico necessário | Metadado de ciclo de monitoramento |
| `campanha` | coleta | metadado técnico necessário | idem |
| `ea` | coleta | **alias resolvível** | Corresponde a `amostragem/registro/estacao_amostral` (`select_one estacao_amostral`, L3891) — mesmo conceito, nome abreviado no schema |
| `ua` | coleta | **alias resolvível** | Corresponde a `amostragem/registro/unidade_amostral` (`select_one unidade_amostral`, L3892) — idem |
| `usuario` | coleta | metadado técnico necessário | Usuário do pipeline/planilha, não XLSForm |
| `validado` | coleta | metadado técnico necessário | Flag de validação, produto do processo de validação |
| `validador` | coleta | metadado técnico necessário | idem |
| `data_validacao` | coleta | metadado técnico necessário | idem |
| `obs_validacao` | coleta | metadado técnico necessário | idem |
| `planilha_upload` | coleta | metadado técnico necessário | Origem do upload, não XLSForm |
| `coleta_uuid` | coleta | metadado técnico necessário | UUID de coleta gerado/atribuído no pipeline |

Nenhuma dessas 16 é "divergência a investigar" ou "risco de regressão" — todas têm explicação estrutural clara e nenhuma delas é dado de survey que deveria estar chegando do XLSForm e não está.

## 6. Q3 — Há campos do XLSForm 2025 que deveriam estar no schema de 129 e não estão?

Não, além do par `ea`/`ua` já contado (que na verdade **estão** representados, só que por outro nome). Dos 117 campos de survey únicos em `21FEV25_nSrC9X3` (excluindo grupos/repeats/metadados técnicos do próprio ODK como `start`/`deviceid`/`note`), só 8 ficam de fora do conjunto de 129, e desses:
- `amostragem/registro/estacao_amostral`, `amostragem/registro/unidade_amostral` — o par `ea`/`ua` (seção 5).
- `amostragem/registro/coletor` (begin_repeat/end_repeat), `amostragem/registro/amostragem` (begin_repeat/end_repeat), `amostragem/registro/registro` (begin_repeat/end_repeat) — 6 linhas — marcadores estruturais de grupo repeat no dump, **não geram coluna própria** em nenhum produto CSV. Classificação: **esperado por template/export**.

Ou seja: **zero campos de dado real do XLSForm 2025 ficam de fora do schema de 129** sem explicação.

## 7. Q4 — A ordem dos 129 corresponde ao template/export 2025 ou a outra fonte?

Corresponde, para o bloco de `nivel = "registro"` (91 atributos, posições 36–126): a ordem segue exatamente a ordem de aparição do `caminho_registro` no dump XLSForm (`amostragem/registro/ponto_amostral` → `ponto_metro` → `tipo_forma_vida` → `forma_serrapilheira` → `forma_vida_outros` → `forma_vida_nativa` → cadeia nativa/exótica/seca-morta na mesma sequência do formulário). Não é ordem alfabética nem ordem de outra fonte.

O bloco `nivel = "coleta"` inicial (posições 1–17: `coleta`...`coleta_uuid`) e os 3 últimos (`amostragem/ponto_fim_transecto`, `amostragem/foto_ponto_final`, `uuid`) não têm ordem correspondente no XLSForm porque são majoritariamente metadado de pipeline (seção 5) — a ordem aí é decisão do template de exportação (SISMonitora), não do formulário.

## 8. Q5 — Os nomes usados no schema são `name`, `path`, label exportado, ou aliases?

Predominantemente `caminho_registro` (path completo, ex.: `amostragem/registro/forma_vida_nativa_bromelioide`) para os 91 matches exatos. Para os 22 matches por `name_curto`, o schema usa o segmento final do path sem o prefixo de grupo (ex.: schema usa `uc`, XLSForm tem `amostragem/registro/uc`) — mistura de convenção dentro da própria tabela de 129, não é sistemático. Nenhum atributo do schema usa o `label` do XLSForm como chave. `ea`/`ua` são o único caso de alias verdadeiro (nome interno do pipeline, não derivado nem do `name` nem do `caminho_registro`).

## 9. Q6 — Há divergência de tipo (`type`, `tipo_base`, `list_name`)?

O schema de 129 **não tem** colunas `type`/`tipo_base`/`list_name` — só tem `formato` (categoria de formatação de exportação: `texto`, `inteiro_texto`, `data_iso`, `uuid_texto`, `numero_texto_ponto`, `hora_hms_millis_tz`, `inteiro_texto_zero_esquerda`). Não é uma "divergência de tipo" no sentido XLSForm — é uma dimensão diferente (formato de string exportada, não tipo de pergunta). Comparando os 113 matches com o `tipo_base` real do XLSForm 2025:

| `tipo_base` (XLSForm 2025) | Nº de atributos do schema que batem nesse tipo |
|---|---|
| `text` | 52 |
| `select_multiple` | 22 |
| `select_one` | 16 |
| `image` | 12 |
| `calculate` | 4 |
| `hidden` | 3 |
| `geopoint` | 2 |
| `date` | 1 |
| `time` | 1 |

Todos os 113 são **OK/equivalente**: cada um bateu com exatamente 1 candidato no XLSForm 2025 (sem ambiguidade de tipo — a checagem de mais-de-um-candidato retornou zero casos), e o `formato` do schema é uma representação plausível do `tipo_base` correspondente (ex.: `select_one`/`select_multiple`/`text` todos exportam como `texto` — esperado num CSV achatado; `uuid` como `uuid_texto`; campos `calculate` de UUID/plaqueta formatada como `uuid_texto`/`inteiro_texto_zero_esquerda` — esperado, são campos calculados, não perguntas ao coletor). Nenhuma divergência de tipo real foi encontrada.

## 10. Q7 — Há campos técnicos/metadados no schema de 129 que não são survey fields do XLSForm?

Sim, os 14 da seção 5 (excluindo `ea`/`ua`). Todos classificados **metadado técnico necessário** — são deliberados e esperados: o produto `registros_validados.csv` precisa desses campos de controle de processo (quem validou, quando, origem do upload) além dos dados de survey.

## 11. Q8 — Há repeats/grupos que explicam divergências?

Sim: os 6 marcadores `begin_repeat`/`end_repeat` (`coletor`, `amostragem`, `registro`) explicam por que esses "nomes" aparecem no dump `campos` mas nunca poderiam aparecer como atributo de coluna no schema de 129 — são fronteiras de grupo/repeat do XLSForm, não campos de dado (classificação: **esperado por template/export**).

## 12. Q9 — Quais divergências são esperadas/justificáveis?

- As 14 de metadado de pipeline (seção 5) — esperadas, é informação que só existe depois que o dado sai do ODK.
- Os 6 marcadores de `begin_repeat`/`end_repeat` — esperados, não são campos de dado.
- A diferença de convenção de nome entre match por `caminho_completo` (91) e por `name_curto` (22) dentro do próprio schema de 129 — não é erro, mas é uma inconsistência interna que a fonte única deveria uniformizar (usar sempre `caminho_registro` completo como chave, já que é o que desambiguou 91 dos 113 casos).

## 13. Q10 — Quais divergências são risco real para `registros_validados.csv`?

Uma só, de baixo risco mas real: **`ea`/`ua` não são resolvidas automaticamente** por nenhum comparador de nome simples (nem `monitora_validados_normalizar_nome`, nem a comparação desta auditoria) — dependem de um mapeamento manual `ea↔estacao_amostral` / `ua↔unidade_amostral` que **não foi localizado** em nenhuma das tabelas de alias já inventariadas na Auditoria 03.5H (`monitora_validados_aliases_xlsform_historico`, `monitora_validados_aliases_adicionais`, `monitora_pipe_aliases_campos_conhecidos`, `monitora_dt_consolidar_aliases_colunas`). Se a fonte única vier a gerar `ea`/`ua` automaticamente a partir do dump XLSForm (em vez de manter o schema de 129 como hoje), precisa de um mapeamento explícito para esses dois — caso contrário arrisca quebrar a геração desses dois atributos em `registros_validados.csv`. Classificação: **divergência a investigar** antes de qualquer consolidação que toque nesses dois atributos.

Nenhuma outra divergência encontrada representa risco para `registros_validados.csv` hoje.

## 14. Q11 — Como isso deve alimentar a futura fonte única sem quebrar PNB/FNCS?

1. O schema de 129 **pode** ser tratado como "fonte a consolidar" (conforme já apontado na Auditoria 03.5H) com segurança para o subconjunto de 113 atributos batidos por `caminho_registro`/`name` — a fonte única pode derivá-los do dump XLSForm 2025 sem mudar semântica.
2. Os 14 atributos de metadado de pipeline **não devem** migrar para o dump XLSForm — eles não pertencem lá; a fonte única precisa de uma segunda camada (schema de exportação/pipeline) independente do XLSForm, mantendo a distinção já sugerida na Auditoria 03.5H (seção 12: "a fonte única precisará de um segundo nível... não um único achatamento").
3. Antes de qualquer geração automática de `ea`/`ua`, localizar ou criar o mapeamento explícito `ea↔estacao_amostral`, `ua↔unidade_amostral` — sem isso, migrar essas duas colunas da fonte única quebraria silenciosamente `registros_validados.csv` (mudaria de "resolvido por hardcode" para "não resolvido").
4. Preferir `caminho_registro` completo como chave canônica na fonte única (foi suficiente para 91/113 = 80% dos matches sem qualquer ambiguidade); usar `name` curto só como fallback explícito, documentado — não como estratégia primária, para não repetir a inconsistência apontada na seção 12.
5. Migrar em etapa isolada e verificável: gerar a variante nova do schema de 129 a partir do dump XLSForm + camada de metadado de pipeline, comparar coluna a coluna e posição a posição contra o schema hardcoded atual (mesmo método desta auditoria, sem rodar FNCS), e só então trocar `monitora_validados_schema_embutido()` por um adaptador — nunca as duas fontes coexistindo silenciosamente.
6. Preservar a separação `registros_importados_bruto` → `registros_importados` → `registros_corrig` → `registros_validados`: esta auditoria comparou apenas contrato, não tocou nenhum produto — a futura consolidação do schema de 129 afeta só a camada `validados`, não deve vazar para os produtos anteriores.

## 15. Nenhuma pendência escondida

O único item que fica genuinamente em aberto para o painel/consolidação futura é a seção 13 (`ea`/`ua` sem alias localizado em nenhuma tabela existente) — registrado explicitamente, não escondido, e classificado como bloqueante para qualquer automação desses dois atributos específicos.
