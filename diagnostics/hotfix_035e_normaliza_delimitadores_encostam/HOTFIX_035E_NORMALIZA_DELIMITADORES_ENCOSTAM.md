# Hotfix 03.5E — Normalização de delimitadores em tokens de Encostam

## 1. Resumo executivo

O Hotfix 03.5E corrige resíduo de vírgula em tokens de `Encostam`/`tipo_forma_vida`, especialmente o caso `outra_forma_vida,`, observado após a correção do checkpoint de pipe residual. A correção preserva a semântica de `select_multiple`: tokens devem ser normalizados como conjunto de valores canônicos, sem vírgula residual anexada ao token.

## 2. Causa do erro

Após o Hotfix 03.5D, `registros_importados.csv` passou a ser criado corretamente no FNCS, mas ainda havia resíduos de vírgula em `Encostam`, por exemplo:

- `outra_forma_vida, nativa`
- `serrapilheira outra_forma_vida, nativa`
- `outra_forma_vida, serrapilheira nativa`

O token correto é `outra_forma_vida`, sem vírgula. Como `Encostam` é um campo `select_multiple`, delimitadores residuais não podem permanecer acoplados aos tokens.

## 3. Alteração feita

A normalização de tokens de `Encostam` foi ajustada para remover delimitadores residuais sem colar tokens distintos e sem transformar combinações como `nativa,exotica` em `nativaexotica`.

A correção preserva a regra de comparação por conjunto de tokens e mantém as proteções já existentes:

- `Encostam` vazio não deve permanecer;
- `solo_nu` não pode coexistir com outros tokens;
- `outra_forma_vida` não pode carregar vírgula;
- texto livre com `|` continua preservado;
- pipe estruturado residual continua bloqueado no checkpoint correto.

## 4. Diretrizes de desenvolvimento

A correção respeita as diretrizes de desenvolvimento do script:

- evita recomputação global desnecessária;
- preserva a cadeia bruto → importado → corrigido → validado;
- mantém a operação localizada em normalização de tokens;
- não usa `output/` ou `log/` como entrada implícita;
- não altera dados reais nem versiona produtos de execução;
- preserva a performance observada após o Hotfix 03.5D.

## 5. Evidência FNCS

Na run FNCS pós-correção:

- `registros_importados_bruto.csv`: 16766 × 182;
- `registros_importados.csv`: 16766 × 203;
- `registros_corrig.csv`: 16766 × 223;
- `Encostam` vazio: 0;
- `Encostam` com vírgula: 0;
- token `outra_forma_vida,`: 0;
- token `nativaexotica`: 0;
- `solo_nu` coexistindo com outros tokens: 0;
- pipe estruturado residual bloqueante: 0.

Comparação com a run anterior pós-03.5D:

| Métrica em Encostam | Antes | Depois |
|---|---:|---:|
| Linhas com vírgula | 248 | 0 |
| Token `outra_forma_vida,` | 248 | 0 |
| `solo_nu` coexistente | 0 | 0 |
| `Encostam` vazio | 0 | 0 |
| Token colado `nativaexotica` | 0 | 0 |

## 6. Evidência PNB golden

Na run PNB pós-correção:

- UAs duplicadas no mesmo ano: 606 linhas / 6 coletas;
- nativa sem forma: 3 / 3;
- seca/morta sem forma: 10 / 8;
- outra forma legada: 7 / 5;
- `Encostam` vazio: 0;
- `Encostam` com vírgula: 0;
- token `outra_forma_vida,`: 0;
- token `nativaexotica`: 0;
- `solo_nu` coexistindo com outros tokens: 0;
- pipe estruturado residual: 0;
- execução concluída de forma controlada.

As métricas golden obrigatórias foram preservadas.

## 7. Performance

A run FNCS pós-correção manteve performance equivalente à etapa anterior:

| Run | Soma ativa | Acumulado final |
|---|---:|---:|
| v2.6.0 original FNCS | 72,827 s | 87,709 s |
| pós-03.5D antes da vírgula | 74,537 s | 89,436 s |
| pós-correção de vírgula | 74,624 s | 89,490 s |

Diferença contra a run imediatamente anterior: aproximadamente +0,087 s na soma ativa e +0,054 s no acumulado final, sem evidência de degradação.

## 8. Fora do escopo

- Outra forma de vida legada;
- pipe indeterminado nas três colunas remanescentes;
- escopo do painel 101/84;
- motor único de validação row-level;
- funcionalidades da v2.6.2_teste;
- release pública.

## 9. Próximo passo recomendado

Após este hotfix, a próxima frente recomendada é auditar os pipes indeterminados remanescentes ou iniciar a Auditoria 04A de outra forma de vida legada, mantendo cada frente como etapa isolada.
