# Hotfix 03.5M-D2-H1 — Correção do vazamento de `MONITORA_COL_XXXXX` em `registros_importados_bruto.csv`

## 1. Resumo executivo

Corrigido, de forma cirúrgica (1 linha alterada), o bug identificado na 03.5M-D2: `monitora_publicacao_h_fwrite_registros_importados()` — a função que escreve `registros_importados_bruto.csv` — substituía o cabeçalho real de **todas** as colunas pelo nome técnico interno `MONITORA_COL_XXXXX`, mesmo quando os nomes originais eram únicos e bem formados (não só nos casos de duplicata que a função foi desenhada para tratar). A causa era `nomes_originais <- enc2utf8(as.character(base::names(out)))` não garantir uma cópia genuinamente independente do vetor de nomes antes de `data.table::setnames(out, nomes_internos)` mutar `out` por referência — corrompendo `nomes_originais` junto. Corrigido forçando uma cópia real via `paste0()`. Testado isoladamente em 4 cenários (nomes normais, nome vazio, nomes duplicados, releitura via `fread()`) — todos passam agora, e o valor com pipe no corpo do arquivo permanece intocado (o hotfix afeta só o cabeçalho, nunca dados).

## 2. Causa confirmada

`monitora_campsav_alvo_global_v2.6.0.R`, função `monitora_publicacao_h_fwrite_registros_importados()` (linha ~24450, antes da correção):

```r
nomes_originais <- enc2utf8(as.character(base::names(out)))
...
nomes_internos <- sprintf("MONITORA_COL_%05d", seq_along(nomes_originais))
...
data.table::setnames(out, nomes_internos)   # muta por referência
...
header <- paste(monitora_publicacao_i_csv_quote(nomes_originais), collapse = ",")  # deveria ser o nome ORIGINAL
```

`enc2utf8()` e `as.character()`, quando aplicados a um vetor que já é `character`/UTF-8 (o caso comum de `names()` de um `data.table`), podem devolver o **mesmo objeto** em memória, sem alocar uma cópia nova. `data.table::setnames()` existe precisamente para mutar nomes **por referência** (essa é sua razão de ser, para performance). Se `nomes_originais` e `names(out)` apontam para o mesmo vetor subjacente, `setnames(out, nomes_internos)` sobrescreve os dois ao mesmo tempo — corrompendo `nomes_originais`, que deveria ter preservado os nomes reais para reconstituir o cabeçalho do arquivo final.

O próprio comentário do código, já presente antes desta tarefa, previa esse risco teoricamente (*"pode vazar para o cabeçalho manual e produzir MONITORA_COL_00001... no CSV final"*) — a 03.5M-D2 confirmou, por teste isolado, que o risco se concretizava na prática, inclusive no caso mais simples (nomes únicos, sem duplicata, sem vazio).

## 3. Patch aplicado

```diff
-  nomes_originais <- enc2utf8(as.character(base::names(out)))
+  ### Hotfix 03.5M-D2-H1: enc2utf8(as.character(...)) sozinho NÃO garante uma
+  ### cópia independente (ambos podem devolver o mesmo objeto quando o vetor já
+  ### é character/UTF-8), e setnames() muta por referência -- confirmado por
+  ### teste isolado que isso corrompia nomes_originais na prática, produzindo
+  ### MONITORA_COL_00001... em TODAS as colunas do CSV final, mesmo sem
+  ### duplicatas. paste0() sempre aloca um vetor novo, forçando a cópia real.
+  nomes_originais <- paste0(enc2utf8(as.character(base::names(out))))
```

`paste0(x)` sempre aloca um novo vetor de caracteres (é uma função vetorizada de concatenação de string, nunca retorna o mesmo `SEXP` de entrada), garantindo que `nomes_originais` seja uma cópia genuinamente independente antes de `setnames()` rodar. Nenhuma outra linha da função foi alterada — o fallback interno `MONITORA_COL_%05d` continua existindo (é necessário para contornar a restrição do `data.table::fwrite()` a nomes duplicados), só não vaza mais para o arquivo final.

## 4. Teste de reprodução

Reaproveitado o mesmo harness isolado da 03.5M-D2 (função extraída verbatim do script, `Rscript` puro, sem dados reais), reexecutado **antes** da correção para confirmar que o bug ainda estava presente, depois **depois** da correção:

| Cenário | Antes do patch | Depois do patch |
|---|---|---|
| A — 60 colunas normais, sem duplicata, coluna 49 com pipe | `"MONITORA_COL_00049"` (bug) | `"amostragem/registro/forma_vida_nativa_bromelioide"` (correto) |
| B — nome de coluna 49 vazio | `"MONITORA_COL_00049"` (bug) | `""` (vazio preservado, correto) |
| C — colunas 49/50 com mesmo nome real (duplicata) | `"MONITORA_COL_00049"`/`"MONITORA_COL_00050"` (bug) | nome real duplicado preservado em ambas (correto); `colunas_duplicadas` continua detectando a duplicata |
| D — releitura do CSV final via `fread()` | `MONITORA_COL_00049` | nome real preservado |

## 5. Teste de correção (saída completa)

```
=== teste A ===
nome da coluna 49 no CSV final: amostragem/registro/forma_vida_nativa_bromelioide
todas as 60 colunas preservaram o nome original: TRUE
nenhum MONITORA_COL_ no header final: TRUE
valor com pipe preservado no corpo do arquivo (nao alterado pelo hotfix): TRUE
RESULTADO TESTE A: PASSOU

=== teste B ===
nome da coluna 49 no CSV final (nome original era vazio): ''
as outras 59 colunas preservaram nome original: TRUE
nenhuma ocorrencia de MONITORA_COL_ no header final: TRUE
RESULTADO TESTE B: PASSOU

=== teste C ===
nomes das colunas 49 e 50 no CSV final: amostragem/registro/forma_vida_nativa_bromelioide | amostragem/registro/forma_vida_nativa_bromelioide
colunas_duplicadas detectadas pela funcao: amostragem/registro/forma_vida_nativa_bromelioide
nenhum MONITORA_COL_ no header final: TRUE
RESULTADO TESTE C: PASSOU

=== teste D ===
nome da coluna 49 relido via fread(): amostragem/registro/forma_vida_nativa_bromelioide
RESULTADO TESTE D: PASSOU

TODOS OS TESTES PASSARAM: TRUE
```

## 6. Confirmação: bruto preserva nomes no teste isolado

Confirmado nos 4 cenários acima (seção 4–5) — nomes normais, vazios e duplicados são todos preservados corretamente no cabeçalho do arquivo final, tanto na escrita quanto na releitura via `fread()`.

## 7. Confirmação: fallback `MONITORA_COL_XXXXX` não é aplicado a todas as colunas

Confirmado — em nenhum dos 4 cenários testados o fallback aparece no arquivo final. O fallback (`nomes_internos`) continua existindo **internamente**, só para contornar a restrição do `data.table::fwrite()` a nomes duplicados durante a escrita do corpo do arquivo (`col.names = FALSE`) — nunca é escrito no cabeçalho, que agora usa corretamente `nomes_originais` (cópia genuína).

## 8. Confirmação: resolvedores de pipe não alterados

```
$ git diff -- monitora_campsav_alvo_global_v2.6.0.R | grep -E 'monitora_produtos_resolver_pipes_por_ponto|monitora_bloquear_pipe_residual_produto|monitora_produtos_classificar_pipe_coluna|monitora_pipe_contrato_classificar_coluna|monitora_pipe_contrato_resolver_tokens_isolado|word\(x, sep ?= ?"\|", -1\)|padronizacao_formas_vida_condicionais_basicas'
(nenhuma ocorrência)
```

Nenhuma das funções/blocos restritos aparece no diff — a única alteração está isolada dentro de `monitora_publicacao_h_fwrite_registros_importados()`.

## 9. Confirmação: bloco `word(..., -1)` não alterado

Confirmado pela mesma busca acima — sem ocorrência no diff.

## 10. Confirmação: normalização condicional não alterada

Confirmado — o bloco `padronizacao_formas_vida_condicionais_basicas` (identificado na 03.5M-D2) não aparece no diff; esta tarefa tocou exclusivamente o exportador do bruto, uma função diferente e anterior a esse bloco no pipeline.

## 11. Confirmação: não rodou PNB/FNCS/pipeline completo

Confirmado — só o harness isolado (função extraída, dados sintéticos, `Rscript` puro) foi executado.

## 12. Confirmação: não abriu painel

Confirmado — nenhuma interação com painel/Shiny nesta tarefa.

## 13. Confirmação: warning `data.table::unique`

Não investigado, não tocado nesta etapa (fora do escopo).

## 14. Recomendação objetiva para próxima etapa

Com o bug de nome corrigido, uma próxima run FNCS real (com a instrumentação `MONITORA_DIAGNOSTICO_PIPES_CONTRATO="S"` e o novo ponto de diagnóstico pré-normalização da 03.5M-D2 ativos) deve, pela primeira vez, conseguir associar corretamente o nome semântico real da coluna com pipe em `registros_importados_bruto.csv` — permitindo confirmar concretamente se o padrão observado (célula(s) com pipe) corresponde a um dos 12 campos vinculados condicionais (bromelioide/cactacea/orquidea/samambaia) e avaliar, com nome real em mãos, se o mecanismo `word(..., -1)` (identificado na 03.5M-D2, não alterado aqui) está de fato descartando valores divergentes entre pontos elegíveis. Essa run não foi executada nesta tarefa.

## 15. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 8 +++++++-
 1 file changed, 7 insertions(+), 1 deletion(-)
```

## 16. `git status --short --branch`

```
## dev-v2.6.2-rollforward-golden-v260...origin/dev-v2.6.2-rollforward-golden-v260
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035m_d2_h1_monitora_col_bruto/
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, commit ou push foi executado; `git diff --cached --name-only` retorna vazio.
