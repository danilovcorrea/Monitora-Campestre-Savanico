# Auditoria 03.5H3 — Mapeamento ea↔estacao_amostral / ua↔unidade_amostral

## Resumo / veredito

**Mapeamento explícito existe, mas só em 1 de pelo menos 3 resolvedores de coluna paralelos, e não é o mais usado.** `monitora_esp_colunas_chave()` (módulo de validação espacial, L15186, 4 chamadores) mapeia corretamente `ea↔"Estação amostral"/"estacao_amostral"` e `ua↔"Unidade amostral"/"unidade_amostral"`. O resolvedor geral `monitora_correcao_colunas_chave()` (L2835, **73 chamadores** — o mais usado do script) **não mapeia `ea` para `estacao_amostral`** (usa candidatos de um conceito diferente, "Estrato amostral"/"Area elegivel") e mapeia `ua` só indiretamente via regex. As tabelas de alias de `registros_validados` (`monitora_validados_aliases_xlsform_historico`, L27391-27392) só têm `EA`/`UA`, sem `estacao_amostral`/`unidade_amostral`. Hoje isso não quebra nada porque, na prática, a coluna já chega renomeada para `ea`/`ua` antes de qualquer um desses resolvedores precisar do nome longo (ver seção 4). Risco fica registrado para a fonte única (seção 5), não é bug ativo.

## Metodologia

Só `grep -n` e leitura de trechos do script; nenhuma execução, nenhum dado real, nenhuma alteração.

## 1. Onde `ea`, `estacao_amostral`, `ua`, `unidade_amostral` aparecem

| Termo | Ocorrências relevantes |
|---|---|
| `"ea"` (chave curta) | Construtores de chave (`monitora_correcao_colunas_chave` L2843, `monitora_esp_colunas_chave` L15191), tabela de alias `validados` (L27391), schema de 129 (L27286, posição 9), `alias_map` de `monitora_dt_consolidar_aliases_colunas` — **ausente** lá, ordenação de colunas técnicas L24279, uso direto como coluna em dezenas de funções de painel/relatório (`chaves$ea`, `ch$ea`, `chaves_rel$ea`, `chaves_rel_exoticas$ea`), rename explícito em `registros_corrig` (L33048-33049: `if ("ea" %in% colnames(registros_corrig)) monitora_renomear_ou_consolidar_coluna(registros_corrig, "ea", "EA")`) |
| `"estacao_amostral"` | Só no dump XLSForm embutido (`campos`, ex. L3891: `select_one estacao_amostral`, label "Estação Amostral") e como candidato explícito dentro de `monitora_esp_colunas_chave()` (L15191) |
| `"ua"` | Mesmos locais de `ea` (construtores de chave, alias `validados` L27392, schema L27287, `alias_map` de `monitora_dt_consolidar_aliases_colunas` L26359 — **presente** lá, ao contrário de `ea`), rename explícito em `registros_corrig` (L33051-33052) |
| `"unidade_amostral"` | Dump XLSForm embutido (L3892: `select_one unidade_amostral`, label "Unidade Amostral"), candidato explícito em `monitora_esp_colunas_chave()` (L15192), e em `monitora_dt_consolidar_aliases_colunas` (L26359: `"UA" = c("ua", "unidade amostral", "amostragem/unidade_amostral")` — **path sem o segmento `registro/`**, divergente do path real do dump, `amostragem/registro/unidade_amostral`) |

## 2. Alguma tabela de alias já mapeia ea↔estacao_amostral e ua↔unidade_amostral?

Só uma, parcialmente:

| Resolvedor | `ea` → inclui `estacao_amostral`? | `ua` → inclui `unidade_amostral`? |
|---|---|---|
| `monitora_esp_colunas_chave()` (L15186–15201, módulo espacial) | **Sim**, literal + regex (`"Estação amostral"`, `"estacao_amostral"`, `"estacao"`, regex `estacao.*amostr`) | **Sim**, literal + regex |
| `monitora_correcao_colunas_chave()` (L2835–2848, geral, 73 usos) | **Não** — candidatos são `"Estrato amostral"`/`"estrato_amostral"`/`"Area elegivel"`/`"Área elegível"` (conceito de rótulo diferente do XLSForm) | Só indireto: regex `unidade.*amostral` cobriria, mas não há candidato literal |
| `monitora_validados_aliases_xlsform_historico()` (L27391-27392, usado por `registros_validados`) | **Não** — só `c("EA")` | **Não** — só `c("UA")` |
| `monitora_dt_consolidar_aliases_colunas()` (L26359) | **Não presente** (não existe entrada `"EA"` no `alias_map`) | **Sim**, mas com path incompleto (`amostragem/unidade_amostral`, falta `registro/`) |

## 3. Como esses campos são resolvidos hoje?

Nenhuma das quatro opções da pergunta isoladamente: **candidato-literal + regex contra `names(dt)` do produto já carregado em memória** (não é o schema hardcoded de 129, não é o dump XLSForm, não é `label`, não é fuzzy/score). Mecanismo duplicado em duas funções quase idênticas: `monitora_correcao_primeira_coluna()` (L2445, usada por `monitora_correcao_colunas_chave`) e `monitora_esp_resolver_coluna()` (L15166, usada só pelo módulo espacial) — mesma lógica (match exato case-insensitive, depois regex, primeira coluna do `dt` que bater), listas de candidatos mantidas independentemente.

## 4. Quais consumidores dependem desses campos?

- `monitora_correcao_colunas_chave()` — **73 chamadas** no script inteiro: filtros de painel, relatórios (`chaves_rel$ea`/`chaves_rel_exoticas$ea` em `monitora_relatorio_exoticas_*`, L25748/36157/36421), resumo de coleta, validação de linhas, resolução de espécie.
- `monitora_esp_colunas_chave()` — 4 chamadas, restritas ao módulo de validação espacial (`versão 2.5.4`, transectos/geometria).
- `registros_validados.csv` — via `monitora_validados_resolver_coluna()`/`monitora_validados_aliases()` (candidatos efetivos: só `"ea"`/`"EA"`/`"ua"`/`"UA"`, sem os nomes longos).
- Rename canônico em `registros_corrig` (L33048-33052) — converte a coluna (já resolvida por algum dos mecanismos acima) para o nome final `"EA"`/`"UA"` usado por quase tudo a jusante.

## 5. Risco se a fonte única futura usar apenas XLSForm 2025

Risco real, mas condicional — hoje não há bug porque, no fluxo atual, a coluna já chega a `registros_corrig` com nome curto (`ea`/`EA`, `ua`/`UA`) antes de qualquer resolvedor precisar do nome longo do XLSForm; é por isso que `registros_validados` funciona mesmo sem o alias longo (seção 2). **Se a fonte única substituir os candidatos hardcoded de `monitora_correcao_colunas_chave()` (73 usos) por nomes derivados diretamente do XLSForm** (`estacao_amostral`/`unidade_amostral`, que é como o dump 21FEV25 nomeia esses campos) **sem preservar `ea`/`ua`/`EA`/`UA` como aliases**, ou vice-versa (manter só os nomes curtos e perder o vínculo com o path do XLSForm), o resolvedor passa a devolver `NA_character_` silenciosamente (nenhuma das duas funções lança erro nesse caminho — `monitora_esp_resolver_coluna` só levanta erro se `obrigatoria=TRUE`, que não é o caso de `ea`/`ua`; `monitora_correcao_primeira_coluna` nunca levanta erro). Isso propagaria `NA` silenciosamente pelos 73 pontos de consumo — risco de regressão silenciosa, potencialmente maior e mais disperso do que o risco already registrado na Auditoria 03.5H2 (que via o problema só pela ótica de `registros_validados`).

## 6. Onde registrar esses aliases na futura fonte única

Como uma entrada de alias por conceito canônico (`ea`/`estacao_amostral` e `ua`/`unidade_amostral`) num **único** índice de aliases central — o mesmo "substituível" já apontado na Auditoria 03.5H (seção 9, itens `monitora_pipe_aliases_campos_conhecidos`, `monitora_dt_consolidar_aliases_colunas`, `monitora_publicacao_p_resolver_coluna_*`). `monitora_esp_colunas_chave()` já tem a definição correta pronta para servir de referência de conteúdo (candidatos + regex). O destino conceitual é que **tanto** `monitora_correcao_colunas_chave()` (73 usos) **quanto** `monitora_esp_colunas_chave()` (4 usos) **quanto** `monitora_validados_aliases()` consultem essa mesma entrada, em vez de cada um manter sua própria lista parcial — eliminando a divergência apontada na seção 2.

## 7. Necessidade de hotfix agora ou requisito para 03.5I?

**Requisito para 03.5I, não hotfix agora.** Nenhum produto está quebrado hoje: o fluxo atual funciona porque a normalização de nome de coluna já acontece antes de `chaves$ea`/`chaves$ua` serem necessários (seção 4), e isso não muda com nenhuma das auditorias 03.5H/03.5H2/03.5H3 — nenhuma alterou código. O risco descrito na seção 5 só se materializa **se e quando** a fonte única mudar a nomenclatura-fonte dos campos sem carregar esse alias — é uma pré-condição de design, não uma correção emergencial. Recomenda-se registrar como item obrigatório do escopo de 03.5I (fonte única): consolidar `ea`/`ua` num único alias compartilhado pelos três consumidores antes de qualquer um deles passar a depender do nome do XLSForm em vez do nome hardcoded atual.
