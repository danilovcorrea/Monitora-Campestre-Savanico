# Auditoria 03.5C-A2 — Reclassificação da resolução de resíduos de pipe em registros_importados.csv

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, noite (-03:00) |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit auditado | `2513acf` |
| Relatório anterior (não alterado) | `diagnostics/auditoria_035c_vazamento_importados/AUDITORIA_035C_VAZAMENTO_IMPORTADOS.md` |
| Escopo | Reavaliação estática + ensaio sintético adicional, sem dados reais. Nenhum código editado, nenhum commit. |

## 1. Resumo executivo

A Auditoria 03.5C-A classificou como **RISCO** o fato de `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)` alterar valores de colunas ecológicas em `registros_importados.csv`. Essa classificação partia de uma definição de camada que proibia **qualquer** alteração de valor ecológico antes de `registros_corrig.csv`.

O usuário refinou a especificação: `registros_importados.csv` **precisa** estar livre de `|` (para ser comparável e consultável), e a resolução de `|` **pode** ser aceitável ali **se for normalização técnica determinística de artefato de importação**, não correção ecológica.

Reexaminando o mecanismo sob essa lente mais precisa, a conclusão muda substancialmente: **a operação central da função é, de fato, extração posicional determinística — não decisão ecológica.** Ela não recalcula Encostam a partir de outras colunas, não insere nem remove `solo_nu` por regra, não move token entre categorias, não limpa descritores/fotos (que são explicitamente protegidos) e não aplica nenhuma regra de "o que é biologicamente correto". Ela apenas desfaz um artefato estrutural conhecido do SISMONITORA (múltiplos pontos de uma mesma COLETA tendo suas respostas achatadas numa única célula, unidas por `|`), usando o próprio número do ponto amostral daquela linha como índice para recuperar a resposta que já pertencia a ela.

Isso **reverte o veredito geral de RISCO para PARCIAL**. O que permanece como lacuna real, confirmado por um novo ensaio sintético desta etapa, é mais estreito e mais tratável: **a função não garante 100% a remoção de `|`** — quando o índice do ponto amostral não bate com o número de tokens no resíduo (descompasso estrutural no próprio dado de origem), a célula permanece com `|` sem nenhum bloqueio, alerta visível ou reprocessamento. Isso viola o requisito explícito #3 ("`registros_importados.csv` NÃO pode conter valores com tokens separados por `|`") em um caso de borda, não no caso geral.

Também ficou confirmado, examinando o fluxo de objetos do script, que a chamada equivalente em `registros_corrig.csv` **não é uma duplicação sem sentido**: `registros_corrig` é derivado de uma cópia independente de `registros` (feita antes de qualquer resolução de pipe ter ocorrido para `registros_importados.csv`), então a resolução em `registros_corrig` está resolvendo o artefato pela primeira vez para aquele objeto, não repetindo um trabalho já feito. Ela só se tornaria um no-op se a arquitetura passasse a derivar `registros_corrig` do objeto já resolvido de `registros_importados.csv` — o que **não acontece hoje** e está fora do escopo desta etapa.

## 2. Nova especificação operacional (conforme definida pelo usuário)

| Produto | Pode conter `\|`? | Pode conter correção ecológica? |
|---|---|---|
| `registros_importados_bruto.csv` | Sim — deve preservar fielmente, inclusive artefatos | Não |
| `registros_importados.csv` | **Não** — deve estar livre de `\|`, mas a remoção deve ser normalização técnica, não correção semântica | Não |
| `registros_corrig.csv` | Não (já resolvido antes) | Sim — é a camada autorizada |

## 3. Veredito revisado

**PARCIAL** (revisado de RISCO para PARCIAL).

A operação central é `normalizacao_tecnica_importacao`, compatível com a nova especificação. A classificação permanece PARCIAL, não OK, por duas lacunas concretas e delimitadas (não por a operação em si ser indevida):
1. Não há garantia de remoção completa de `|` em casos de descompasso de índice (confirmado empiricamente nesta etapa).
2. A auditoria existe (contagens antes/depois, falhas), mas nada consome esses números para bloquear ou alertar quando a remoção falha — ou seja, a garantia do requisito #3 depende de sorte estrutural do dado, não de um mecanismo que force a garantia.

## 4. Tabela de ações da função

| Ação | Coluna | Efeito | Classificação | Permitida em `registros_importados`? |
|---|---|---|---|---|
| Calcular posição do ponto dentro da COLETA (sequencial, por ordem de aparição) | `COLETA` (leitura) | Nenhuma alteração de valor; só cálculo auxiliar interno (`pos_linha`) | `derivacao_auxiliar` | Sim |
| Sobrescrever a posição pelo valor real de `ponto_amostral` quando numérico, válido (1–10000) e sem `\|` | `ponto_amostral` (leitura) | Nenhuma alteração de valor da própria coluna; refina o índice usado nas demais colunas | `derivacao_auxiliar` | Sim |
| Detectar células com `\|` literal, coluna a coluna | Qualquer coluna de texto | Nenhuma alteração; apenas seleciona quais células serão processadas | `derivacao_auxiliar` | Sim |
| Separar a célula por `\|` e extrair o token na posição calculada (`toks[seletor[i]]`) | Encostam/`tipo_forma_vida`, `forma_vida_nativa/exotica/seca_morta/outros`, `forma_serrapilheira` (via default), espécie, hábito, `ponto`/`uuid`/`observacao` | Substitui o conteúdo achatado pelo token que já pertencia àquela linha, sem inventar, comparar ou julgar o conteúdo | **`normalizacao_tecnica_importacao`** | **Sim**, sob a nova especificação |
| Deixar a célula inalterada quando o índice não existe no vetor de tokens (índice fora do range ou `NA`) | Qualquer coluna acima | `\|` **permanece** na célula — falha silenciosa em relação ao requisito de "sem `\|`" | `indeterminado`/lacuna | Não deveria acontecer, mas hoje pode |
| Proteger explicitamente foto/imagem/mídia/anexo e colunas técnicas (`MONITORA_*`, `.id`, etc.) de qualquer alteração | Fotos, colunas técnicas | Nenhuma alteração | `harmonizacao_para_consulta` (proteção, não correção) | Sim — já é o comportamento correto |
| Proteger qualquer coluna quando `produto` contém "bruto" | Todas | Nenhuma alteração | `leitura_tecnica` | N/A — não se aplica a `registros_importados.csv` |
| Gravar auditoria por coluna (antes/depois/falhas/exemplos) | — | Produto de auditoria à parte, não altera dado | `harmonizacao_para_consulta` | Sim, mas hoje não é usada para bloquear nada |

## 5. Respostas às verificações pedidas (itens 3–11 da tarefa)

- **A escolha do token por ponto é determinística e estrutural, ou ecológica?** Determinística e estrutural — usa exclusivamente `COLETA` e `ponto_amostral`, dois campos administrativos/de desenho amostral, nunca o conteúdo ecológico da própria célula ou de outras colunas.
- **A função apenas descompacta valores empilhados por `\|`, ou faz algo além?** Apenas descompacta. Não há nenhuma outra transformação de conteúdo na função.
- **Recalcula Encostam?** Não — não lê nenhuma outra coluna (`forma_vida_*`, material botânico) para decidir o valor de Encostam; cada coluna é processada isoladamente.
- **Insere/remove `solo_nu`?** Não — nunca adiciona ou remove um token; só seleciona um dos tokens **já existentes** na célula.
- **Move `nativa`/`exotica`/`seca_morta`/`outra_forma_vida` entre categorias?** Não — cada coluna é resolvida independentemente das demais; não há lógica de mover conteúdo de uma coluna para outra.
- **Limpa descritores/fotos?** Não — fotos/imagens/mídia são explicitamente protegidas (`pipe_residual_tecnico_tolerado`) e nunca alteradas por esta função.
- **Espécie/hábito mudam por extração técnica ou regra semântica?** Extração técnica — mesmo mecanismo posicional das demais colunas, sem julgamento sobre qual espécie/hábito é "correto".
- **A mesma resolução precisa ocorrer em `registros_corrig.csv`, ou deveria só herdar?** **Precisa ocorrer de novo hoje**, porque `registros_corrig` é construído a partir de uma cópia independente de `registros`, feita antes de `registros_importados.csv` ser processado (confirmado lendo o fluxo de atribuição de objetos: `registros -> registros_corrig` ocorre sobre o mesmo `registros` usado — via cópia interna — para gerar `registros_importados.csv`, então nenhum dos dois "herda" o resultado do outro). Não é uma duplicação sem sentido; é uma resolução independente e necessária dado o desenho atual dos dois pipelines. Isso só viraria um no-op genuíno se `registros_corrig` passasse a ser derivado do objeto já resolvido de `registros_importados.csv` — mudança arquitetural maior, fora do escopo desta auditoria.
- **Há auditoria suficiente?** Parcialmente. O CSV de auditoria (`auditoria_pipes_<produto>.csv`) registra `n_linhas_pipe_antes`, `n_linhas_corrigidas_por_indice_ponto`, `n_linhas_falha_correcao` e `n_linhas_pipe_depois` por coluna — dados suficientes para **detectar** o problema. Mas nada no script **lê essas contagens de volta** para bloquear a exportação ou emitir um alerta de nível superior quando `n_linhas_pipe_depois > 0`. A garantia do requisito #3 depende hoje de o dado de origem nunca ter descompasso de índice, não de um mecanismo que a torne uma garantia.

## 6. Tabela bruto → importado → corrigido

| Cenário | Bruto | Importado | Corrigido | Diferença | Classificação |
|---|---|---|---|---|---|
| Encostam com resíduo `solo_nu\|nativa\|serrapilheira`, 3 pontos, índices 1–3 corretos | Preservado | Resolvido corretamente por posição (1→`solo_nu`, 2→`nativa`, 3→`serrapilheira`) | Idêntico ao importado (resolvido de forma independente, mesmo resultado) | Sim, mas técnica | `normalizacao_tecnica_importacao` |
| Mesmo cenário, mas com `ponto_amostral` ausente/`NA` para as 3 linhas | Preservado | Resolvido pela posição sequencial dentro da COLETA (fallback), mesmo resultado | Idêntico | Sim, técnica | `normalizacao_tecnica_importacao` (fallback determinístico, ainda estrutural) |
| **Novo nesta etapa**: 3 pontos (índices 1, 2, 3), mas resíduo com apenas 2 tokens (`nativa\|exotica`) — descompasso estrutural no próprio dado | Preservado (`nativa\|exotica` nas 3 linhas) | Linhas 1–2 resolvidas (`nativa`, `exotica`); **linha 3 permanece `nativa\|exotica`, com `\|` residual** | Mesmo comportamento — residual também persistiria em `registros_corrig.csv` se o dado chegasse assim | **Confirma lacuna real**: `\|` pode sobreviver a `registros_importados.csv`, violando o requisito #3 em caso de borda | `indeterminado`/lacuna a fechar |

## 7. Conclusão

- **`registros_importados.csv` deve continuar removendo `\|`?** Sim — isso está alinhado com a especificação: é normalização técnica, não correção ecológica, e é necessária para tornar o produto comparável e consultável.
- **A remoção atual é técnica e aceitável?** Sim, no caso geral — a lógica é puramente posicional/estrutural, não introduz nem julga conteúdo ecológico.
- **Há casos em que ela vira correção semântica?** Não identificado nenhum caso onde a função em si tome uma decisão semântica. O único caso onde ela "falha" é estrutural (descompasso de índice), não semântico.
- **A auditoria atual é suficiente?** Parcialmente — suficiente para diagnosticar depois dos fatos, insuficiente para **garantir** o requisito #3 antes da entrega do produto.

## 8. Recomendação para eventual Hotfix 03.5C-B (revisado, não implementado)

**Diferente da recomendação da Auditoria 03.5C-A** (que sugeria desligar `corrigir` em `registros_importados.csv` — isso violaria o requisito #3 sob a nova especificação e não deve ser feito).

Recomendação revisada, escopo bem menor:

1. Manter `corrigir = TRUE` nas duas chamadas efetivas (`registros_importados.csv` e `registros_corrig.csv`) — ambas são necessárias e corretas sob a nova especificação.
2. Adicionar uma checagem pós-resolução, apenas para `registros_importados.csv`: após `monitora_produtos_resolver_pipes_por_ponto`, somar `n_linhas_pipe_depois` da auditoria retornada (`attr(d, "monitora_auditoria_pipes")`); se `> 0` para qualquer coluna, registrar um evento de log de severidade mais alta (ex.: "ERRO" em vez de silêncio) e, dependendo da decisão do usuário, decidir entre (a) apenas alertar de forma visível, ou (b) bloquear a materialização de `registros_importados.csv` até o descompasso ser resolvido manualmente — esta segunda opção precisa de decisão explícita do usuário antes de implementar, por ser potencialmente bloqueante.
3. Não tocar na chamada de `registros_corrig.csv` — está correta e é necessária dado o desenho atual do fluxo de objetos.
4. Testes sintéticos a criar: reaproveitar o cenário de descompasso desta auditoria (3 pontos, 2 tokens) como teste-golden negativo, confirmando que a nova checagem detecta e sinaliza o caso.
5. Regressões: baseline PNB + os 4 datasets reais já usados no Hotfix 03.5B, verificando que nenhum deles produz `n_linhas_pipe_depois > 0` hoje (se todos passarem em zero, o hotfix seria puramente preventivo, sem mudança de comportamento visível nos dados já validados).

## 9. Critérios de aceite para `registros_importados.csv` como base de consulta comparável, sem sanitização semântica

1. Nenhuma célula de `registros_importados.csv` contém o caractere `|` literal — garantido por mecanismo de bloqueio/alerta, não apenas por ausência histórica de descompasso.
2. Toda alteração de valor entre `registros_importados_bruto.csv` e `registros_importados.csv` é rastreável a uma de duas causas: (a) consolidação de coluna duplicada/alias de identificação (já auditado como OK); (b) resolução posicional de resíduo de `|` (este relatório, classificado como normalização técnica).
3. Nenhuma alteração entre bruto e importado depende do conteúdo de mais de uma coluna simultaneamente (ou seja, nenhuma "regra" cruza colunas — cada resolução é local à própria célula/coluna).
4. Nenhuma alteração entre bruto e importado depende de decisão do painel, de tabela de aliases ecológicos ou de qualquer contrato XLSForm de domínio/relevância — apenas de identificação de linha (COLETA/ponto).

## Segurança Git

```
$ git status --short
?? diagnostics/auditoria_035c_vazamento_importados/AUDITORIA_035C_A2_RECLASSIFICACAO_PIPE_IMPORTADOS.md

$ git diff --stat
(vazio)

$ git diff --name-only
(vazio)
```

Nenhuma alteração no script. Nenhum CSV/ZIP/XLSX/RDS/DB dentro do Git. Nenhum dado real copiado para o repositório — os ensaios sintéticos desta etapa existem apenas em `/tmp`. O relatório anterior (`AUDITORIA_035C_VAZAMENTO_IMPORTADOS.md`) não foi alterado.
