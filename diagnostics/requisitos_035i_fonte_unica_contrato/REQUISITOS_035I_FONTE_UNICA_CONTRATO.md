# Requisitos consolidados — 03.5I (fonte única embutida de contrato)

Consolidação das auditorias 03.5H (`diagnostics/auditoria_035h_fonte_unica_contrato/`), 03.5H2 (`diagnostics/auditoria_035h2_schema129_vs_xlsform2025/`) e 03.5H3 (`diagnostics/auditoria_035h3_alias_ea_ua/`). Documento de requisitos apenas — **nenhuma fonte única, função ou tabela foi criada nesta etapa**.

## 1. Fontes atuais a consolidar

| Fonte | Local (03.5H) | Situação |
|---|---|---|
| Dump XLSForm embutido (`campos`/`opcoes`/`dependencias`/`arquivos`, 4 versões) | `monitora_correcao_xlsforms_embutidos()` L3883 | Núcleo — já embutido, sem dependência externa |
| Schema hardcoded de 129 atributos | `monitora_validados_schema_embutido()` L27280 | 113/129 batem com o dump 2025 (03.5H2); 14 são metadado de pipeline; 2 (`ea`/`ua`) batem por alias não formalizado (03.5H3) |
| Regras de relevance/dependências | coluna `relevant` do dump + `monitora_correcao_dependencias_padrao()` L6206 (hardcode paralelo) + `monitora_validados_validar_condicionais_xlsform21()` L28496 (reimplementação independente) | 3 fontes de relevance não unificadas |
| Aliases de pipe | `monitora_pipe_aliases_campos_conhecidos()` L31735 | Isolado, 3 entradas |
| Aliases de `registros_validados` | `monitora_validados_aliases_xlsform_historico()` L27381 + `monitora_validados_aliases_adicionais()` L27498 | Isolado, ~60 entradas, não usado por pipe nem por `dt_consolidar` |
| Aliases de consolidação de colunas | `monitora_dt_consolidar_aliases_colunas()` L26349 | Isolado, 15 entradas, sem `EA` |
| Resolvedores fuzzy/por score | `monitora_correcao_candidatos_coluna_xlsform()`/`monitora_correcao_resolver_coluna_xlsform_registro()` L3491/L3556 (score por linha) + `monitora_publicacao_p_resolver_coluna_por_alias_e_perfil()` L2721 (alias+regex+perfil) + `monitora_correcao_colunas_chave()`/`monitora_esp_colunas_chave()` L2835/L15186 (candidato+regex contra `names(dt)`, duplicado, 73 vs. 4 usos) | 4 estratégias de resolução concorrentes, nenhuma compartilhada |
| Regras de choices/domínios | tabela `opcoes` do dump + `monitora_correcao_choices_xlsform()` L10139 + `monitora_validados_xlsform21_choices()` L28176 (adaptador, não duplica fonte) | Fonte única já hoje; adaptador OK |

Achados de risco a carregar para o desenho da fonte única: duas duplicações reais de função (`monitora_publicacao_c_normalizar_contratos_pre_exportacao` L7653/L8682, `monitora_produtos_classificar_pipe_coluna` L24587/L31827 — a definição antiga é código morto em ambos os casos, não apagar sem confirmar ausência de referência).

## 2. Requisitos mínimos da fonte única (schema por atributo)

Cada atributo da fonte única deve carregar, no mínimo:

1. **Atributo canônico 2025** — nome de referência único e estável.
2. **Path/caminho_registro** — o `caminho_registro` completo do dump XLSForm (chave primária recomendada; foi a que resolveu 91/113 dos casos em 03.5H2 sem ambiguidade, superior ao `name` curto).
3. **Name curto** — segmento final do path, para os casos onde o produto exporta só o nome curto (22/113 dos casos em 03.5H2).
4. **Labels históricos**, com e sem HTML (`<span style=...>`) — necessário porque `monitora_validados_aliases_adicionais()` já mapeia variantes com/sem `<span>` para os mesmos campos (ex. bromélia/cactácea/orquídea nativas).
5. **Aliases explícitos** — incluindo variantes de sufixo (`.1`, `.2`, `__dup1`, `__dup2`) e abreviações internas do pipeline sem relação textual com o XLSForm (caso `ea`/`ua`, ver seção 3).
6. **Tipo/`type_base`/`list_name`** — direto do dump, já existente.
7. **Choices/domínios** — vínculo com a tabela `opcoes` por `list_name`.
8. **Required/constraint/relevant** — a expressão `relevant` crua do XLSForm, preservada como string (não só sua interpretação hardcoded).
9. **Grupos/repeats/campo pai** — path completo já implica isso, mas o campo pai explícito (`parent_name`) evita ter que re-parsear o path a cada consumo.
10. **Cardinalidade operacional** — ver seção 4 (categoria obrigatória, não apenas type_base).
11. **Status de confiança** — herdar a distinção já usada em `monitora_pipe_coluna_classificar_natureza()` (L31780: `pipe_permitido_texto_livre` / `pipe_residual_estruturado_elegivel` / `pipe_ambiguo` / `pipe_indeterminado`), estendida para cobrir esparsidade (seção 4); registrar também os casos "confiança média (mapeamento por inferência posicional, não confirmado com dado real)" já sinalizados na Auditoria 03.5F para não perder essa nuance ao consolidar.
12. **Origem da regra** — replicar o padrão já usado na tabela `dependencias` (`fonte = "xlsform_embutido"` vs. `"regra_padrao_monitora"`), estendido a todo atributo: de qual versão/hotfix/auditoria a entrada veio, para rastreabilidade.

## 3. Requisito obrigatório — `ea`/`ua` (03.5H3)

- Registrar explicitamente `ea ↔ estacao_amostral` e `ua ↔ unidade_amostral` na fonte única, com o mesmo conteúdo já correto em `monitora_esp_colunas_chave()` (L15191-15192: candidatos `"EA"`, `"ea"`, `"Estação amostral"`, `"estacao_amostral"`, `"estacao"` / `"UA"`, `"ua"`, `"Unidade amostral"`, `"unidade_amostral"`, mais regex).
- **Não** copiar os candidatos de `ea` de `monitora_correcao_colunas_chave()` (L2843) para a fonte única sem correção — hoje apontam para um conceito diferente (`"Estrato amostral"`/`"Area elegivel"`), não para `estacao_amostral`.
- Antes de qualquer consumidor migrar para nomear a coluna a partir do XLSForm (`estacao_amostral`/`unidade_amostral`) em vez do nome curto atual (`ea`/`ua`/`EA`/`UA`), confirmar que os **73 pontos de consumo** de `monitora_correcao_colunas_chave()` (painel, relatórios, `chaves_rel`/`chaves_rel_exoticas`) continuam recebendo um valor não-`NA` — nenhum desses 73 usos hoje força erro em caso de falha de resolução (`monitora_correcao_primeira_coluna()` retorna `NA_character_` silenciosamente), então uma regressão aqui não apareceria como erro, só como dado ausente rio abaixo.
- Critério de aceite mínimo para 03.5I: gerar `chaves$ea`/`chaves$ua` a partir da fonte única e comparar, coluna a coluna, contra o resultado atual de `monitora_correcao_colunas_chave()` e `monitora_esp_colunas_chave()` nos mesmos produtos, sem executar FNCS completo.

## 4. Requisito obrigatório — cardinalidade (03.5H, seção 11 item 7)

O vocabulário atual de `monitora_pipe_coluna_classificar_natureza()` distingue só `texto` vs. `estruturado` vs. `ambíguo` vs. `indeterminado`. A fonte única precisa de categorias explícitas para:

1. **Texto livre** — já existe (`pipe_permitido_texto_livre`).
2. **Estruturado completo por ponto** — `select_one`/`select_multiple` sem relevance condicional, aplicável a toda linha de registro.
3. **Estruturado condicional/esparso** — `select_one`/`select_multiple` com `relevant` não-trivial, cuja presença real depende de outro campo e pode ser esparsa na prática de campo. **Esta é a categoria que faltava e causou a regressão do Hotfix 03.5G/03.5G2** (bromélia nativa: `tipo_base=select_one` correto no contrato, mas o resolvedor posicional não tratava a esparsidade, gerando 275 linhas de resíduo bloqueante em COLETA 11168). Não pode ser tratada como sinônimo de "estruturado completo".
4. **`select_multiple`** — hoje colapsado junto com `select_one` em "estruturado"; a fonte única deve isolar porque a semântica de resíduo de pipe (múltiplos tokens) é diferente de `select_one`.
5. **Técnico/mídia** — hoje resolvido só por regex ad-hoc em `monitora_produtos_classificar_pipe_coluna()` (`foto|imagem|image|media|arquivo_midias|anexo`, L31831), sem base no `tipo_base` do contrato (que tem `image`, `geopoint` etc. como tipos explícitos) — a fonte única deve derivar essa categoria do `tipo_base`, não de regex de nome.
6. **Fora do contrato** — colunas que não correspondem a nenhum `caminho_registro`/`name`/label conhecido em nenhuma versão embutida; hoje indistinguível de "indeterminado por lacuna de alias" (a própria causa-raiz da Auditoria 03.5F). A fonte única deve separar "não existe no contrato" de "existe mas o resolvedor não achou o alias".
7. **Ambíguo/indeterminado** — já existem (`pipe_ambiguo`, `pipe_indeterminado`), manter a prioridade já correta de `pipe_ambiguo` sobre heurística de nome (comentário do Hotfix 03.5C-C, L31835-31839).

## 5. Restrições de segurança

- **Sem dependência runtime de XLSX/CSV externo** — a fonte única deve ser embutida no script público, seguindo o padrão já usado por `monitora_correcao_xlsforms_embutidos()` (TSV embutido + `fread` em memória, sem I/O de arquivo externo).
- **Não versionar dado real** — nenhuma das auditorias 03.5H/H2/H3 leu ou copiou dado de coleta; a fonte única deve conter só contrato (schema/aliases/regras), nunca amostra de produção.
- **Não esconder pendências em `registros_importados`** — qualquer coluna que a fonte única não conseguir classificar deve continuar caindo em `pipe_indeterminado`/"fora do contrato" (reportado, não bloqueante, não silenciosamente descartado) — mesmo comportamento que already existe hoje via `monitora_bloquear_pipe_residual_produto` (categoria introduzida no Hotfix 03.5C-B).
- **Não aplicar `solo_nu` final antes do pós-painel/pós-sanitização** — invariante já documentado no script ("Encostam não pode ficar vazio ao final. `solo_nu` só é [introduzido depois das correções]", L32028-32032); a fonte única não deve antecipar essa decisão para uma etapa anterior à sanitização/painel.
- Preservar a separação `registros_importados_bruto` → `registros_importados` → `registros_corrig` → `registros_validados` em todas as etapas — a fonte única é consultada por estágio, nunca colapsa estágios (guarda já existente: `permitir_apenas_bruto` em `monitora_registros_importados_exportar`, L31846).

## 6. Requisitos de performance

- Base em `data.table`, seguindo o padrão já usado em `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` (cache + colunas normalizadas pré-computadas).
- Índices/caches derivados (`name_norm`, `caminho_norm`, `label_norm`) calculados uma vez, não recalculados por chamada — generalizar o padrão `*_norm_publicacao_ae` já existente para todas as tabelas novas (schema consolidado, alias map único).
- Sem fuzzy/score repetido em runtime — o scoring de `monitora_correcao_candidatos_coluna_xlsform()` deve continuar vetorizado sobre colunas (já é), não virar um scoring por linha.
- Sem loops linha a linha — não repetir o padrão de `monitora_correcao_xlsform_registro()` (chamada por `linha` escalar, L3426) na fonte única; resolver por join/chave vetorizado.
- Sem parse de XLSX em runtime — decorre diretamente da restrição de segurança da seção 5; a fonte única é gerada/atualizada em tempo de edição do script, nunca lida de arquivo em tempo de execução.

## 7. Plano incremental recomendado

| Etapa | Escopo |
|---|---|
| **03.5I** | Criar a fonte única embutida (schema consolidado seção 2 + `ea`/`ua` seção 3 + cardinalidade seção 4), **sem mudar nenhum consumidor existente** — só a estrutura de dados nova, isolada, não referenciada por nenhum pipeline ainda. |
| **03.5J** | Criar índices/caches derivados sobre a fonte única (seção 6) — ainda sem migrar consumidores. |
| **03.5K** | Mapa observado→canônico como **diagnóstico** (relatório, não decisão automática) comparando nomes de coluna reais de produtos atuais contra a fonte única, para descobrir lacunas de alias antes de qualquer automação (mesmo método usado nas auditorias 03.5H2/03.5H3). |
| **03.5L** | Consolidar `registros_importados` usando a fonte única (primeiro produto a migrar, por ser o mais próximo do dado bruto). |
| **03.5M** | Migrar resolução de pipe por relevance/cardinalidade (categorias da seção 4) — é a etapa que endereça a causa-raiz do Hotfix 03.5G. |
| **03.5N** | Migrar painel/`registros_validados`/relatórios (`monitora_correcao_colunas_chave`, `monitora_esp_colunas_chave`, `monitora_validados_aliases`) — última etapa, maior superfície de consumidores (73+ usos), só depois das etapas anteriores validadas. |

Cada etapa deve ter hotfix e relatório próprios (mesmo padrão 03.5D–03.5H3), com comparação de saída antes/depois sem rodar FNCS completo, e só then avançar para a próxima.

## Nenhuma pendência escondida

Este documento é só consolidação de requisitos — não decide nenhuma prioridade além da ordem já listada na seção 7, e não resolve nenhuma das divergências abertas nas auditorias anteriores (schema de 129 vs. XLSForm, `ea`/`ua`, cardinalidade condicional/esparsa). Todas permanecem registradas como requisitos de entrada para 03.5I, não como resolvidas.
