# Auditoria 03.5A — Cadeia de importação, contratos e separação de camadas

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, 19h (-03:00) |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit auditado | `e48d809` (`fix: sincroniza pendencias apos validacao Encostam`) |
| Base operacional | `v2.6.0` golden baseline + Hotfixes 01, 02, 03 |
| Escopo | Auditoria estática apenas. Nenhum código foi editado. Nenhum commit foi feito. |

## 1. Resumo executivo

A cadeia `registros_importados_bruto → registros_importados → registros_corrig → registros_validados` está **estruturalmente presente e majoritariamente bem desenhada**, com pontos de gravação isolados, guardas explícitas contra saneamento tardio, e um schema de 129 colunas e um contrato XLSForm 21FEV25 genuinamente embutidos no script (não inventados nem aproximados).

Porém a auditoria encontrou **quatro classes de problema reais, verificadas por leitura direta de código** (uma delas confirmada empiricamente com teste isolado):

1. **Motor de validação disperso, não único** (já apontado nas auditorias anteriores, reconfirmado aqui): pelo menos 4 mecanismos independentes tratam da mesma família de regras de `Encostam`/`tipo_forma_vida`, com cobertura desigual entre `registros_corrig.csv` (proteção mínima, parcialmente coberta pelos Hotfixes 02/03) e `registros_validados.csv` (proteção mais completa, mas só ativa no caminho opcional).
2. **Vazamento de correção estrutural para a camada `registros_importados.csv`**: a função `monitora_produtos_resolver_pipes_por_ponto` (chamada com `corrigir=TRUE` tanto para `registros_importados.csv` quanto para `registros_corrig.csv`) altera valores de colunas ecológicas (`Encostam`, `forma_vida_*`, espécie, hábito) quando detecta resíduos de `|` — isso ocorre **antes** de `registros_corrig.csv`, contrariando a definição de que `registros_importados.csv` "não deve corrigir Encostam".
3. **Referência hardcoded a nome de coluna rotulada, em paralelo a um resolvedor flexível usado em todo o resto do script**: o bloco de tokenização de Encostam (corrigido pelo Hotfix 01) referencia literalmente `` registros_corrig$`**Encostam** na vareta: (amostragem/registro)` ``, enquanto o restante do motor (painel, sincronização final, pendências impeditivas, Hotfixes 02/03) usa `monitora_correcao_colunas_chave()$tipo_forma_vida`, um resolvedor que aceita tanto o nome rotulado quanto o nome canônico `amostragem/registro/tipo_forma_vida`. **Confirmado empiricamente**: quando a coluna rotulada não existe no schema de origem (caso dos datasets 2025 "canônicos" testados na regressão real do Hotfix 03), esse bloco cria silenciosamente uma coluna fantasma inteiramente `NA` com o nome rotulado, sem erro e sem aviso. Em um cenário de schema misto (concatenação de anos/formulários com as duas convenções de nome), o risco é mais sério: o resolvedor flexível priorizaria a coluna canônica e ignoraria linhas cujo dado real só existe na coluna rotulada — um cenário plausível, mas não testado empiricamente nesta auditoria (classificado como RISCO/INDETERMINADO abaixo).
4. **Duplicação sistemática de definições de função** (mesmo nome definido duas vezes no arquivo, a segunda sobrescrevendo a primeira em tempo de execução): confirmado em pelo menos 6 funções centrais da cadeia de produtos, incluindo o próprio exportador de `registros_corrig.csv`. Isso não é um bug de dado, mas é um risco real de auditoria/manutenção — quem lê a primeira definição (inclusive um agente ou revisor humano) pode concluir algo diferente do comportamento real.

Nenhuma dessas quatro classes de problema invalida a baseline golden PNB já aprovada (nenhuma é exercitada pelo dataset PNB atual), mas todas são relevantes antes de confiar cegamente na cadeia para dados reais 2025/2026 com maior diversidade de schema.

## 2. Veredito por quesito

| # | Quesito | Veredito |
|---|---|---|
| A | Contrato XLSForm 2025 incorporado ao script? | **PARCIAL** |
| B | Contrato usado para checagem integral nas validações? | **PARCIAL / RISCO** |
| C | Template SISMONITORA 2025 incorporado ao script? | **OK** |
| D | Template usado para validações de estrutura/ordem/formato/domínios/vazios/padrões? | **PARCIAL** |
| E | Erro conhecido de diferença input real vs. importado/tratado está sanado? | **PARCIAL** |
| F | `registros_importados_bruto` representa leitura fiel? | **OK** |
| G | `registros_importados` é apenas organização/harmonização, sem sanitização semântica? | **PARCIAL / RISCO** |
| H | Sanitizações/correções semânticas concentradas em `registros_corrig`? | **PARCIAL** |
| I | `registros_validados` é apenas projeção final, sem sanitização semântica tardia silenciosa? | **PARCIAL** |

## 3. Tabela da cadeia de produtos

| Etapa | Produto | Função/bloco criador (efetivo) | Linhas aprox. | Transformações permitidas (definição) | Transformações observadas | Classificação |
|---|---|---|---|---|---|---|
| 1 | `registros_importados_bruto.csv` | `monitora_registros_importados_exportar` (2ª definição, efetiva) — chamada única em ~32507 | def. ~31513–31558; chamada 32507 | Metadados técnicos mínimos | `out <- copy(registros)` logo após concatenação + remoção de `.id.N` legado (~32529); nenhuma renomeação, nenhum coalesce, nenhuma alteração de valor | **OK** — leitura_tecnica |
| 2 | `registros_importados.csv` | `monitora_registros_importados_saneado_exportar` (2ª def., efetiva, ~31560) → `monitora_registros_importados_saneado_preparar` (única def., 24489) | prep. 24489–24512; export. 31560–31602; chamada 32554 | Renomear/harmonizar colunas, criar chaves, organizar labels, compatibilizar schema | (a) `monitora_dt_consolidar_colunas_duplicadas` — coalesce mecânico de colunas com nome idêntico, conflitos preservados com marcador, não sobrescreve silenciosamente; (b) `monitora_dt_consolidar_aliases_colunas` — coalesce restrito a uma lista fixa de 15 grupos de **identificação/chave** (COLETA, UC, UA, CICLO, datas, coordenadas, UUID) — **não inclui nenhum campo ecológico**; (c) reordenação de colunas por chaves preferenciais; (d) `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)` — **altera valores** de colunas que casam com `ponto\|encostam\|forma.*vida\|especie\|uuid\|habito\|observacao`, escolhendo um token por posição de ponto quando há resíduo de `\|` | (a)(b)(c): **OK** — harmonizacao_organizacional. (d): **RISCO** — sanitizacao_contratual/estrutural sobre colunas ecológicas, um passo antes do previsto |
| 3 | `registros_corrig.csv` | Exportador central `monitora_publicacao_aa_exportar_registros_corrig_aprovado` (2ª def., efetiva) | def. efetiva ~31725+ (pós Hotfix 03) | Correções do painel, sanitizações semânticas, recálculo de Encostam, fallback `solo_nu`, auditoria | Tokenização inicial de Encostam (bloco `str_replace_all`, ~32925, Hotfix 01 aplicado); `monitora_correcao_sincronizar_encostam_final`/`calcular_tipo_forma_vida_esperado` (recálculo pelo estado final das sublistas, ~8733–8909); auditoria "pendências impeditivas" (~29249+, 4 categorias, não cobre exclusividade de `solo_nu` nem `outra_forma_vida`/`serrapilheira` sem filho); Hotfixes 02/03 (validação/sanitização row-level mínima + sincronização de metadados, ~31614–31766) | **PARCIAL** — a maior parte das transformações é `correcao_semantica` legítima para esta camada; cobertura ainda incompleta (ver quesito B) |
| 4 | `registros_validados.csv` | `monitora_registros_validados_exportar` (única def., 28831) | 28831–~29050+ | Reordenar, formatar, serializar, validar; **sem** nova sanitização semântica silenciosa | Guarda explícita contra saneamento tardio (28897–28909, bloqueia se `registros_corrig` não tiver selo aprovado); construção da matriz de 129 colunas (`monitora_publicacao_aj_constroi_matriz_validados`); `monitora_validados_aplicar_regras_xlsform21` — **reconstrói** `tipo_forma_vida` a partir das sublistas finais (não é só formatação), auditado via tabela `ajustes` (não silencioso); `monitora_validados_validar_dominios_xlsform21` / `validar_condicionais_xlsform21` — validação/bloqueio; funções adicionais `monitora_registros_validados_sanitizar_forma_outros_atual`/`sanitizar_valor_forma_outros`/`sanitizar_pre_exportacao` (26752–26956) não totalmente exploradas nesta rodada, mas cujo nome indica sanitização adicional específica de `forma_outros` nesta camada | **PARCIAL** — não é silenciosa (registrada em auditoria), mas vai além de projeção pura |

## 4. Tabela de contratos

| Contrato | Onde está definido | Onde é usado | Centralizado/disperso | Cobertura | Riscos |
|---|---|---|---|---|---|
| XLSForm 21FEV25 — domínios/choices (`tipo_forma_vida`, `forma_vida_outros`, `forma_serrapilheira`, demais listas) | Dump multi-versão embutido (~linhas 4300–5350+, tabelas `arquivo_xlsform\tlist_name\tname\tlabel`) + `monitora_validados_xlsform21_meta/campo/choices` (~27097–28070) | Painel (`meta_xls$opcoes`), `monitora_validados_*`, geração de choices do movimento assistido | **Centralizado** na fonte de dados (dump único), mas **consumido de formas distintas** por módulos diferentes | Boa — domínios batem exatamente com o `.xlsx` real (verificado na auditoria anterior) | Baixo, isoladamente |
| XLSForm 21FEV25 — `required`/`relevant`/`constraint` (Encostam vazio, exclusividade `solo_nu`, filhos de `outra_forma_vida`/`serrapilheira`) | Nenhum bloco único; regras replicadas separadamente em 4 lugares (ver item 15 abaixo) | `registros_corrig`: parcialmente, via Hotfixes 02/03 + pendências impeditivas (4 categorias antigas). `registros_validados`: mais completo, via `monitora_validados_validar_condicionais_xlsform21` | **Disperso** | Desigual entre camadas | **RISCO** — regra pode valer para `registros_validados` e não para `registros_corrig` |
| Mapeamento label → token (`"Outras plantas..." → outra_forma_vida`, `"Plantas nativas" → nativa`, etc.) | Bloco `str_replace_all` hardcoded, ~32925–32956 (Hotfix 01 aplicado) | Só na criação inicial de `registros_corrig.csv`, sobre a coluna **rotulada exata** | Centralizado nesse bloco, mas **desalinhado** do resolvedor flexível usado em todo o resto do script | Corrigido para o caso identificado (Hotfix 01); estrutura ainda referencia nome de coluna hardcoded | **RISCO** — ver achado 3 do resumo executivo |
| select_multiple → conjunto de tokens (não string ordenada) | `monitora_correcao_tokenizar`/`monitora_correcao_colapsar_tokens` (2418–2429); `monitora_relatorio_exoticas_tem_token` (regex vetorizado, 2 definições, 25150 e 35174/efetiva) | Amplamente reutilizado (pendências impeditivas, Hotfixes 02/03, triagem, `monitora_validados_*`) | Razoavelmente centralizado | Boa — nenhuma comparação de string ordenada encontrada no núcleo | Baixo |

## 5. Tabela de sanitizações por etapa

| Sanitização | Antes de bruto? | Antes/durante importado? | Antes/durante corrigido? | Antes/durante validado? | Classificação |
|---|---|---|---|---|---|
| Remoção de colunas técnicas `.id.N` de execuções anteriores | Sim (única transformação antes do bruto) | — | — | — | leitura_tecnica (inofensiva) |
| Coalesce de colunas duplicadas por nome idêntico | Não | Sim | — | — | harmonizacao_organizacional |
| Coalesce de aliases de identificação/chave (COLETA/UC/UA/datas/coordenadas) | Não | Sim | — | — | harmonizacao_organizacional |
| Resolução de resíduos de pipe (`\|`) em colunas ecológicas (Encostam, forma_vida_*, espécie, hábito) | Não | **Sim** | Sim (repetida) | — | **RISCO** — mesma classe de correção ocorre uma etapa antes do previsto |
| Tokenização/padronização de Encostam (rótulo → token canônico) | Não | Não | Sim | — | correcao_semantica (correta para esta camada; Hotfix 01 corrigiu a instância conhecida) |
| Sincronização final de Encostam a partir das sublistas (`sincronizar_encostam_final`) | Não | Não | Sim | — | correcao_semantica |
| Validação row-level mínima (Hotfixes 02/03: `solo_nu` vazio/exclusivo, sincronização de pendências) | Não | Não | Sim | — | sanitizacao_contratual |
| Reconstrução de `tipo_forma_vida` a partir de sublistas finais (`aplicar_regras_xlsform21`) | Não | Não | Não (é um motor separado) | Sim | sanitizacao_contratual — **auditada, não silenciosa**, mas é uma segunda reconstrução independente da já feita em `registros_corrig` |
| Validação de domínio/condicionais XLSForm21 (bloqueio de `registros_validados`) | Não | Não | Não | Sim | sanitizacao_contratual (validação, não gera valor novo) |
| Sanitização específica de `forma_outros` (`sanitizar_forma_outros_atual`/`sanitizar_valor_forma_outros`/`sanitizar_pre_exportacao`) | Não | Não | Indeterminado (não totalmente explorada) | Indeterminado (não totalmente explorada) | **INDETERMINADO** — nome sugere sanitização semântica nesta vizinhança; não foi lida em profundidade nesta auditoria |

## 6. Lista de evidências (funções e linhas aproximadas)

- `monitora_registros_importados_exportar` — 2 definições: 24405 (morta) e 31513 (efetiva); chamada única em 32507.
- `monitora_registros_importados_saneado_preparar` — única definição, 24489–24512.
- `monitora_registros_importados_saneado_exportar` — 2 definições: 24514 (morta) e 31560 (efetiva); chamada em 32554.
- `monitora_dt_consolidar_colunas_duplicadas` — única definição, 25891–~26130.
- `monitora_dt_consolidar_aliases_colunas` — única definição, 26166–26232 (alias_map sem nenhum campo ecológico, linhas 26173–26191).
- `monitora_produtos_resolver_pipes_por_ponto` — única definição, 24596–24680; chamadas para `registros_importados.csv` (24523/31570) e `registros_corrig.csv` (na função exportadora, ~linha correspondente pós-Hotfix03).
- `monitora_produtos_classificar_pipe_coluna` — 2 definições: 24587 (morta) e 31503 (efetiva); regra crítica na linha 31509 (`ponto|encostam|forma.*vida|especie|uuid|habito|observacao` → `pipe_residual_operacional`, alterável).
- Bloco de tokenização hardcoded de Encostam — 32925–32956 (com correção do Hotfix 01 nas linhas 32940–32941).
- `monitora_correcao_calcular_tipo_forma_vida_esperado` / `monitora_correcao_sincronizar_encostam_final` — 8733–8843.
- `monitora_publicacao_ab_auditar_pendencias_impeditivas` — única definição, 29249–~29460 (4 categorias: `uas_duplicadas_mesmo_ano`, `ponto_sem_interceptacao`, `nativa/exotica/seca_morta_sem_forma_vida`, `outra_forma_vida` residual legado).
- `monitora_validar_encostam_rowlevel_minimo` (Hotfixes 02/03) — 31614–31766.
- `monitora_publicacao_aa_exportar_registros_corrig_aprovado` — 2 definições: 29918 (morta) e efetiva pós-Hotfix03 (~31725+); único funil real de gravação de `registros_corrig.csv` (3 pontos de chamada).
- `monitora_registros_validados_exportar` — única definição, 28831–~29050+; guarda contra saneamento tardio em 28897–28909.
- `monitora_validados_schema_embutido` — 27097–~27196; 129 colunas, ordem/posições conferidas contra o `.xlsx`/CSV reais (item C).
- `monitora_validados_aplicar_regras_xlsform21` / `validar_dominios_xlsform21` / `validar_condicionais_xlsform21` — 28132–28345 (já detalhadas na auditoria anterior).
- `monitora_registros_validados_sanitizar_forma_outros_atual` (26778), `monitora_registros_validados_sanitizar_valor_forma_outros` (26752), `monitora_registros_validados_sanitizar_pre_exportacao` (26956) — existência confirmada, corpo não lido em profundidade nesta rodada.
- Teste isolado empírico (Rscript, sem dados reais, sem alterar o script): referenciar e reatribuir `` dt$`**Encostam** na vareta: (amostragem/registro)` `` quando essa coluna não existe cria uma **coluna nova inteiramente `NA`**, sem erro — confirma o mecanismo do achado 3 do resumo executivo.

## 7. Pontos que precisam de Hotfix 03.5B (se houver)

1. Migrar o bloco de tokenização hardcoded de Encostam (32925–32956) para usar `monitora_correcao_colunas_chave()$tipo_forma_vida` em vez do nome de coluna rotulado literal — elimina o risco de coluna fantasma `NA` e o risco de schema misto.
2. Adicionar a `registros_corrig.csv` (via o mesmo mecanismo do Hotfix 02) as checagens já existentes só em `monitora_validados_validar_condicionais_xlsform21`: exclusividade de `solo_nu` como pendência impeditiva de primeira classe, não apenas via Hotfix 02 (que já cobre isso) — mas também garantir paridade de nomenclatura entre os dois motores (já preparado pelo Hotfix 03 para a tag `solo_nu_com_outra_categoria`).
3. Revisar se `monitora_produtos_resolver_pipes_por_ponto` deveria rodar com `corrigir=TRUE` já em `registros_importados.csv`, ou se deveria ficar restrito a `registros_corrig.csv` (mover a correção de valor para a camada correta, mantendo em `registros_importados.csv` apenas a auditoria/detecção sem alterar valores).
4. Investigar e documentar `monitora_registros_validados_sanitizar_forma_outros_atual`/`sanitizar_valor_forma_outros`/`sanitizar_pre_exportacao` (26752–26956) para confirmar se são projeção/formatação pura ou sanitização semântica adicional — hoje classificado como INDETERMINADO.
5. (Item estrutural maior, não um hotfix pontual) Consolidar os 4 motores de validação de Encostam em um único motor reutilizável pré/pós-painel, conforme já recomendado na Auditoria de Contrato XLSForm 2025 — este item permanece o mais impactante e deve ser tratado como projeto separado, não como hotfix mínimo.
6. Eliminar (ou pelo menos documentar explicitamente com comentário de alerta) o padrão de funções duplicadas encontrado nesta e nas auditorias anteriores (pelo menos 6 confirmadas), para reduzir risco de leitura/manutenção equivocada.

## 8. Plano recomendado para Hotfix 03.5B (sem implementar)

1. **Teste-golden novo antes de qualquer mudança**: criar um dataset sintético mínimo com linhas usando (a) só a coluna rotulada, (b) só a coluna canônica, (c) ambas simultaneamente (schema misto), para caracterizar empiricamente o comportamento atual antes de qualquer correção — hoje esse cenário é apenas raciocinado, não testado.
2. Migrar a referência hardcoded do bloco de tokenização (32925–32956) para o resolvedor `monitora_correcao_colunas_chave()$tipo_forma_vida`, com teste isolado comprovando que o comportamento para a coluna rotulada tradicional não muda (mesmo resultado do Hotfix 01) e que o cenário de schema canônico deixa de criar a coluna fantasma `NA`.
3. Reexecutar a baseline PNB (golden) e a regressão real (idealmente completando os datasets 03–10 pendentes desde o Hotfix 03) para confirmar ausência de regressão.
4. Só depois disso avaliar mover ou restringir `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)` para fora de `registros_importados.csv` — mudança de maior risco por tocar dois produtos simultaneamente; fazer em commit separado do item 2.
5. Tratar a unificação do motor de validação (item 5 da lista acima) como iniciativa à parte, com seu próprio plano e testes-golden dedicados — não misturar com os hotfixes cirúrgicos.

## 9. Critérios de aceite para declarar cada quesito como atendido

| Quesito | Critério de aceite |
|---|---|
| A | Todas as regras `required`/`relevant`/`constraint` do XLSForm 21FEV25 relevantes a Encostam/filhos existem em exatamente um lugar no script, referenciado por todos os consumidores. |
| B | A mesma checagem de contrato bloqueia (ou no mínimo relata de forma idêntica) tanto `registros_corrig.csv` quanto `registros_validados.csv`, sem depender de qual motor está ativo. |
| C | (já atendido) — manter sob teste de regressão sempre que o `.xlsx`/template oficial for atualizado. |
| D | Toda geração de `registros_validados.csv` falha ruidosamente (não silenciosamente) se ordem/formato/domínio divergir do schema embutido — já parcialmente verdade (`stop()` no header), estender a domínios e formatos individuais. |
| E | Um teste-golden dedicado com resposta literal "Outras plantas..." **e** com schema canônico misto passa sem criar coluna fantasma nem perder dado. |
| F | (já atendido) — manter teste de regressão que verifique hash/contagem de linhas/colunas do bruto contra a leitura direta do CSV de entrada. |
| G | `monitora_produtos_resolver_pipes_por_ponto` não altera valor de nenhuma coluna ecológica antes de `registros_corrig.csv`, ou essa exceção fica explicitamente documentada e testada como intencional. |
| H | Nenhuma transformação de valor ecológico ocorre fora de `registros_corrig.csv` (bruto e importados apenas leem/organizam). |
| I | Toda sanitização em `registros_validados.csv` está registrada em uma tabela de auditoria auditável e é idêntica (não uma segunda reimplementação independente) à decisão já tomada em `registros_corrig.csv` para o mesmo atributo. |

## 10. Conclusão

A versão atual (branch `dev-v2.6.2-rollforward-golden-v260`, commit `e48d809`) **atende parcialmente** aos quesitos auditados. A arquitetura conceitual de 4 camadas está corretamente implementada na maior parte dos casos (F e C são OK sem ressalvas), e os Hotfixes 01–03 já fecharam a lacuna mais grave conhecida antes desta auditoria. Contudo, a auditoria encontrou uma nova classe de risco não identificada anteriormente — a referência hardcoded ao nome de coluna rotulado, em contraste com o resolvedor flexível usado no resto do script, com confirmação empírica de criação de coluna fantasma `NA` quando o schema é canônico — e reconfirmou o problema de dispersão do motor de validação já apontado na auditoria de contrato XLSForm 2025. Nenhum desses achados compromete a baseline golden PNB já validada; todos devem ser tratados antes de expandir a confiança da cadeia para datasets 2025/2026 com maior diversidade de schema de exportação.
