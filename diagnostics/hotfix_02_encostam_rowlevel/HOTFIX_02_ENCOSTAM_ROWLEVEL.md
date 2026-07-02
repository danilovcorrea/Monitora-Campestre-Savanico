# Hotfix 02 — Validação/sanitização row-level mínima de Encostam para registros_corrig.csv

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, 16:03–16:32 (-03:00) |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit base | `0b0aa35` (`docs: registra baseline v260 e auditoria contrato 2025`), sobre `5646e73` (hotfix 01) e `482347b` (tag `v2.6.0`) |
| Commit depois | nenhum — alteração ainda não commitada |
| Arquivo alterado | `monitora_campsav_alvo_global_v2.6.0.R` (único arquivo tocado) |
| Diff | +134 linhas, 0 removidas |

## Funções alteradas/criadas

1. **Nova função** `monitora_validar_encostam_rowlevel_minimo()` — criada, ~95 linhas, inserida imediatamente antes da definição efetiva de `monitora_publicacao_aa_exportar_registros_corrig_aprovado`.
2. **Função existente alterada**: `monitora_publicacao_aa_exportar_registros_corrig_aprovado()` — adicionada 1 chamada à nova função (8 linhas incluindo `tryCatch` de segurança), logo após o bloco que recalcula pendências impeditivas e antes de `pendente_imp <- ...`.

### Achado importante durante a implementação

`monitora_publicacao_aa_exportar_registros_corrig_aprovado` está **definida duas vezes** no script (linha ~29918 e linha ~31604 antes da edição). Em R, a segunda definição sobrescreve a primeira em tempo de execução — portanto a primeira definição é código morto. Confirmei isso lendo ambas antes de editar, e apliquei a alteração **apenas na segunda (efetiva)**, que é a que de fato processa todas as chamadas reais (`checkpoint_parcial_registros_corrig`, `pre_analises_registros_corrig`, `exportacao_final_registros_corrig`). A primeira definição foi deixada intocada (fora do escopo deste hotfix; não é isso que está sendo consertado aqui, apenas registrado como observação).

## Ponto de inserção no fluxo

`monitora_publicacao_aa_exportar_registros_corrig_aprovado` é o **funil central único** de gravação de `registros_corrig.csv` — todas as três chamadas reais do script passam por ela antes do `data.table::fwrite`/`monitora_produtos_escrever_csv_canonico` final. A nova validação foi inserida **dentro dela**, logo depois do recálculo de pendências impeditivas (`monitora_publicacao_ab_auditar_pendencias_impeditivas`) e antes da preparação/gravação do CSV (`monitora_publicacao_ae_preparar_registros_corrig_para_csv`). Isso garante que a nova regra roda **sempre após correções automáticas e painel** e **sempre antes de qualquer produto final dependente**, exatamente como solicitado.

## Regras implementadas

| Regra | Comportamento | Corrige automaticamente? |
|---|---|---|
| A — Encostam vazio | Se `tipo_forma_vida` vazio ao final, introduz `solo_nu` | Sim — só neste ponto final |
| B — exclusividade `solo_nu` | Se `solo_nu` coexiste com outro token, remove só `solo_nu`, preserva os demais | Sim |
| C — `outra_forma_vida` sem `forma_vida_outros` | Registra violação | Não — apenas relata |
| D — `forma_vida_outros` sem token pai | Registra violação | Não — apenas relata |
| E — `serrapilheira` sem `forma_serrapilheira` | Registra violação | Não — apenas relata |
| F — `forma_serrapilheira` sem token pai | Registra violação | Não — apenas relata |

### Regras apenas relatadas (C, D, E, F) — justificativa

O escopo autorizado permitia corrigir automaticamente D/F apenas se já existisse "padrão semelhante e seguro no script". Esse padrão existe (`monitora_validados_aplicar_regras_xlsform21`, linha ~28186 e ~28182), mas ele pertence a um motor diferente (`monitora_validados_*`), com premissas, ordem de execução e efeitos colaterais próprios que não foram auditados para reuso neste ponto do fluxo. Reaproveitá-lo aqui seria acoplar dois motores distintos — proibido explicitamente pelo escopo ("não refatorar o motor `monitora_validados_*`"). Optei pela via conservadora: **relatar, não inventar valor**, minimizando risco de regressão sobre a baseline golden.

## Comparação de `select_multiple` como conjunto de tokens

Toda a lógica reutiliza os mecanismos já existentes e testados no restante do script: `monitora_correcao_tokenizar` (split por espaço, único) e `monitora_relatorio_exoticas_tem_token` (checagem vetorizada por regex de token completo, não substring). Nenhuma comparação de string ordenada foi introduzida. Verificado na checagem isolada: `"nativa exotica"` e `"exotica nativa"` não são alteradas nem tratadas como diferentes pela lógica de decisão (ambas passam por todas as 6 regras sem disparo indevido).

## Justificativa contratual (XLSForm 2025)

- Regra A/B implementam diretamente a constraint do campo `tipo_forma_vida`: `if(selected(.,'solo_nu'), count-selected(.)=1, count-selected(.)>=1)` — Encostam nunca vazio, `solo_nu` nunca compartilhado com outro token.
- Regras C–F implementam as condições `relevant`/`required` dos campos filhos `forma_vida_outros` (`selected(${tipo_forma_vida},'outra_forma_vida')`) e `forma_serrapilheira` (`selected(${tipo_forma_vida},'serrapilheira')`), hoje só verificadas no motor opcional `monitora_validados_*` (auditoria XLSForm 2025, itens 2, 5, 6).
- Esta é a mesma regra central que já existe, corretamente, em `monitora_correcao_calcular_tipo_forma_vida_esperado`/`sincronizar_encostam_final` (linhas ~8733–8909) e em `monitora_validados_aplicar_regras_xlsform21` (linha ~28186): "solo_nu só entra se nada mais restar, e sai se qualquer outra coisa aparecer". O hotfix 02 apenas estende essa proteção, de forma independente e sem tocar nas outras duas implementações, para o funil de `registros_corrig.csv`.

## Checagem mínima isolada (sem tocar o pipeline real)

Extraídas verbatim do próprio arquivo as dependências mínimas (`monitora_dt_referenciar`, `monitora_correcao_limpar_texto`, `monitora_correcao_vazio_vec`, `monitora_correcao_tokenizar`, `monitora_correcao_colapsar_tokens`, `monitora_relatorio_exoticas_normalizar_token`, `monitora_relatorio_exoticas_tem_token`) e a função nova, executadas isoladamente sobre uma tabela sintética de 9 linhas cobrindo as 6 regras + 3 controles:

| Linha | Entrada (`tipo_forma_vida`) | Resultado | Regra exercitada |
|---|---|---|---|
| 1 | `""` | `solo_nu` | A |
| 2 | `"nativa solo_nu"` | `"nativa"` (sem `solo_nu`) | B |
| 3 | `"solo_nu"` | `"solo_nu"` (inalterado) | controle |
| 4 | `"outra_forma_vida"` | inalterado (não inventa `forma_vida_outros`) | C (apenas relatada) |
| 5 | `"nativa"` (com `forma_vida_outros="musgos"`) | inalterado | D (apenas relatada) |
| 6 | `"serrapilheira"` | inalterado (não inventa `forma_serrapilheira`) | E (apenas relatada) |
| 7 | `"nativa"` (com `forma_serrapilheira` preenchido) | inalterado | F (apenas relatada) |
| 8 | `"nativa exotica"` | inalterado | controle set-based |
| 9 | `"exotica nativa"` | inalterado | controle set-based (ordem invertida) |

**Resultado: PASSOU** (todas as 9 asserções `stopifnot` corretas). Checagem adicional confirmou que o relatório `auditoria_encostam_rowlevel_minimo.csv` é gravado corretamente com as 3 linhas/regras esperadas quando há achados.

## Reexecução da baseline PNB

Mesmo comando/modo/pasta de execução limpa das etapas anteriores; `input/pnb_2022_2026.zip` intocado (hash confirmado idêntico).

| Métrica | Antes (hotfix 01) | Depois (hotfix 02) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606/6 | 606/6 | ✅ inalterado |
| Nativa sem forma de vida | 3/3 | 3/3 | ✅ inalterado |
| Seca/morta sem forma de vida | 10/8 | 10/8 | ✅ inalterado |
| Outra forma de vida legada | 7/5 | 7/5 | ✅ inalterado |
| Colunas `registros_importados.csv` | 176 | 176 | ✅ inalterado |
| Colunas `registros_corrig.csv` | 180 | 180 | ✅ inalterado (não 256) |
| Tamanho dos 3 produtos de dados (bytes) | idênticos à baseline original | byte-idênticos | ✅ |
| Tempo núcleo (`dec:` "fim execucao") | 51,1 s | 51,2 s | ✅ equivalente |
| `auditoria_encostam_rowlevel_minimo.csv` gerado? | — | **Não** (0 ocorrências das regras A-F neste dataset) | ✅ esperado |
| `registros_importados.csv` sobrescrito? | Não | Não (hashes distintos, ordem bruto→importados→corrig preservada) | ✅ |
| Warning pré-existente `data.table::unique` | presente | presente (inalterado, não relacionado) | — |
| Novos warnings/erros | — | Nenhum | ✅ |

O dataset PNB não contém nenhuma das condições cobertas pelas 6 regras (nenhum Encostam vazio remanescente, nenhuma coexistência de `solo_nu` com outro token, nenhum descompasso `outra_forma_vida`/`forma_vida_outros` ou `serrapilheira`/`forma_serrapilheira`), por isso o relatório novo não foi gerado e os três produtos de dados saíram byte-idênticos aos da execução do hotfix 01 — resultado esperado e desejável: **o hotfix 02 é neutro para a baseline golden e ativo apenas para os cenários antes desprotegidos**, validados de forma isolada.

## Riscos de regressão

- **Duplicação de execução (mitigado)**: a função exportadora central é chamada até 3 vezes por execução completa (`checkpoint_parcial`, `pre_analises`, `exportacao_final`); a nova validação roda em cada chamada. As regras A–F são idempotentes (reaplicar não muda nada além da primeira correção), então não há efeito cumulativo indevido — confirmado também pela ausência de qualquer mudança de tamanho/hash inesperada na reexecução.
- **Performance do loop da Regra B (mitigado)**: a Regra B usa um `for` apenas sobre o subconjunto pré-filtrado vetorialmente (`solo_nu` presente **e** não vazio **e** contém espaço, ou seja, mais de um token) — evita percorrer em R puro as milhares de linhas legitimamente "somente solo_nu". Tempo núcleo permaneceu em ~51 s, sem regressão de performance observável.
- **Metadados desatualizados (aceito, documentado, fora de escopo)**: se a Regra A ou B corrigir uma linha, as colunas `monitora_pendencia_impeditiva_tipo`/`monitora_pendencia_impeditiva_msg` (gravadas por `monitora_publicacao_ab_auditar_pendencias_impeditivas` **antes** desta nova validação, na mesma função) podem continuar citando o motivo antigo (ex.: "ponto_sem_interceptacao") mesmo depois do valor já ter sido corrigido. Isso é uma inconsistência de metadado, não de dado semântico, e não afeta as 4 contagens golden (que são recalculadas a partir do log de execução, não dessas colunas). Recorrigir isso exigiria reordenar/duplicar a auditoria de pendências impeditivas, fora do escopo mínimo autorizado — registrado aqui como pendência conhecida.
- **Escopo de colunas de contexto no relatório**: as colunas COLETA/UC/EA/UA/CICLO/CAMPANHA/ANO do novo relatório dependem de `monitora_correcao_colunas_chave`, já usada e testada extensivamente em outras partes do script; nenhuma alteração foi feita nessa função.

## Testes executados

1. `parse()` do arquivo inteiro após a edição — sintaxe válida.
2. Checagem isolada sintética das 6 regras + 3 controles (9 asserções) — PASSOU.
3. Checagem isolada de gravação do relatório (3 achados esperados) — PASSOU.
4. Reexecução completa da baseline PNB (modo `ate_registros_corrig`, mesma pasta de execução limpa, mesmo input) — todas as contagens golden, colunas e tamanhos de arquivo idênticos à execução anterior; nenhum novo warning/erro.

## Conclusão

**PASSOU.**

A validação/sanitização row-level mínima de Encostam foi implementada em um único ponto central (`monitora_publicacao_aa_exportar_registros_corrig_aprovado`, a definição efetiva), cobrindo as 6 regras contratuais do XLSForm 2025 solicitadas, com correção automática apenas onde segura e sem invenção de dados (regras C–F apenas relatam). A baseline PNB golden foi reexecutada sem qualquer regressão de contagem, coluna, tamanho de arquivo ou performance.

## Pendências não tratadas nesta etapa

- Unificação do motor de validação row-level (item 15 da auditoria) — este hotfix é uma camada adicional, não uma unificação.
- Sincronização das colunas `monitora_pendencia_impeditiva_tipo/_msg` após a correção automática das Regras A/B (ver "Riscos de regressão" acima).
- Correção automática das Regras D e F (adicionar token pai automaticamente) — deliberadamente não implementada nesta etapa.
- Warning `'unique' is not an exported object from 'namespace:data.table'` — inalterado, não relacionado a este hotfix.

## Status git

Nenhum commit criado. `git status --short` mostra apenas `M monitora_campsav_alvo_global_v2.6.0.R` além dos artefatos não rastreados deste diretório de relatório. Nenhum dado real, XLSX, CSV, ZIP ou artefato sensível foi staged ou criado dentro do repositório.
