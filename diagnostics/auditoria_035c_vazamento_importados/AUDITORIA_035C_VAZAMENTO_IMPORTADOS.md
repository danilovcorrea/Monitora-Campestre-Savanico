# Auditoria 03.5C-A — Vazamento de sanitização semântica em registros_importados.csv

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, noite (-03:00) |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit auditado | `2513acf` (`fix: usa resolvedor flexivel para Encostam`) |
| Escopo | Auditoria estática + ensaio sintético/empírico controlado, sem dados reais. Nenhum código editado, nenhum commit. |
| Ensaio sintético | Executado em `/tmp/claude-1000/.../scratchpad/audit035c_ensaio.R`, não versionado |

## 1. Resumo executivo

A hipótese da Auditoria 03.5A estava certa e agora está **confirmada empiricamente, não apenas por leitura de código**: `monitora_produtos_resolver_pipes_por_ponto()`, chamada com `corrigir = TRUE` tanto para `registros_importados.csv` quanto para `registros_corrig.csv`, **altera o valor** de colunas ecológicas (Encostam/`tipo_forma_vida`, `forma_vida_nativa`, `forma_vida_outros`, espécie, hábito) sempre que a célula contém um resíduo de `|` (artefato de achatamento de múltiplos pontos numa única célula, herdado de exportações problemáticas do SISMONITORA). O ensaio sintético mostrou que, para as colunas testadas, o resultado em `registros_importados.csv` é **byte-idêntico** ao resultado em `registros_corrig.csv` — ou seja, a mesma correção acontece duas vezes, uma etapa antes do previsto.

**Achado novo desta etapa, mais amplo do que a Auditoria 03.5A caracterizou**: a função de classificação (`monitora_produtos_classificar_pipe_coluna`) não decide "o que corrigir" — ela decide **apenas as duas exceções que NÃO são corrigidas** (`pipe_esperado_em_bruto` e `pipe_residual_tecnico_tolerado`, este último restrito a foto/imagem/mídia/colunas técnicas). **Todo o resto — incluindo a categoria "revisar" (`pipe_residual_revisar`), que por nome sugere apenas sinalização — é efetivamente alterado quando `corrigir=TRUE`.** Isso inclui, no ensaio, `forma_serrapilheira`, que não bate com nenhum padrão explícito da função de classificação mas ainda assim foi corrigida, por cair no default `pipe_residual_revisar`. Ou seja: a superfície real de colunas alteráveis é maior do que "Encostam, forma_vida_*, espécie, hábito, ponto, uuid, observação" — é, na prática, **qualquer coluna de texto que não seja explicitamente protegida**.

**Boa notícia, também confirmada**: `registros_importados_bruto.csv` nunca é alvo dessa função (não há nenhuma chamada real com esse produto), e mesmo que fosse chamada por engano, a classificação `pipe_esperado_em_bruto` a protege integralmente — confirmado tanto por leitura de código quanto por teste forçado no ensaio.

**Também confirmado (limite do vazamento)**: fora desse mecanismo específico de resíduo de `|`, não existe nenhuma outra lógica de recálculo de Encostam, inserção de `solo_nu`, checagem de exclusividade ou checagem de consistência entre Encostam e as sublistas em nenhum ponto do caminho que gera `registros_importados.csv`. Ou seja, os cenários "Encostam vazio → solo_nu" e "solo_nu coexistindo com nativa" **só ocorrem se o valor já chegar assim no dado bruto** (nada no caminho de `registros_importados.csv` cria ou resolve isso) — quem faz isso é exclusivamente o Hotfix 02, em `registros_corrig.csv`. O vazamento real e confirmado é estritamente sobre resíduo de `|`, não sobre as regras de Encostam do Hotfix 02.

## 2. Veredito geral

**RISCO.**

`registros_importados.csv` pode sair com valores ecológicos diferentes de `registros_importados_bruto.csv` sempre que houver resíduo de `|` nos dados de origem — uma condição real e não incomum em exportações em lote do SISMONITORA (a própria função existe porque esse artefato é conhecido). Não é uma sanitização "ecológica" no sentido de aplicar uma regra biológica (não decide o que é certo taxonomicamente), mas é, sem dúvida, uma **escolha de valor** (qual token sobrevive) feita antes da camada autorizada (`registros_corrig.csv`), sem alternativa de reversão dentro de `registros_importados.csv` e sem qualquer sinal no próprio arquivo de dados (só em um CSV de auditoria à parte, em `03_auditorias/importacao/`).

## 3. Mapa da cadeia

| Produto | Objeto origem | Função criadora | Linhas aprox. | Transformações observadas |
|---|---|---|---|---|
| `registros_importados_bruto.csv` | `registros` (rbindlist pós-concatenação, só com colunas técnicas `.id.N` legadas removidas) | `monitora_registros_importados_exportar` (2ª definição, efetiva) | def. 31513–31558; chamada única 32507 | Nenhuma. Cópia direta (`out <- copy(dt)`). `monitora_produtos_resolver_pipes_por_ponto` **nunca é chamada** para este produto. |
| `registros_importados.csv` | `registros` pós-consolidação de colunas duplicadas/aliases de identificação | `monitora_registros_importados_saneado_preparar` (única def., 24489) + `monitora_registros_importados_saneado_exportar` (2ª def., efetiva, 31560) | prep. 24489–24512; export. 31560–31602; chamada 32554 | (a) consolidação de colunas duplicadas/aliases de identificação (já auditado, não ecológico); (b) reordenação de colunas; **(c) `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)` — altera valor de colunas ecológicas quando há resíduo de `\|`, confirmado no ensaio** |
| `registros_corrig.csv` | `registros_corrig` (após painel/correções/Hotfixes 01–03/03.5B) | `monitora_publicacao_aa_exportar_registros_corrig_aprovado` (2ª def., efetiva) | def. efetiva ~31769+ | Tokenização de Encostam, sincronização final, Hotfixes 02/03/03.5B, **e a mesma `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)`, chamada de novo, produzindo resultado idêntico ao já aplicado em `registros_importados.csv`** |

## 4. Mapa das chamadas de `monitora_produtos_resolver_pipes_por_ponto`

| Chamada | Linha aprox. | Objeto de entrada | Objeto de saída | `corrigir` | Etapa (função contentora) | Risco |
|---|---|---|---|---|---|---|
| 1 | 24524 | `out` (pré-Hotfix, `saneado_preparar`) | `out` | `TRUE` | `monitora_registros_importados_saneado_exportar` — **1ª definição, morta** (sobrescrita) | Nenhum — código nunca executa |
| 2 | 29978 | `dt_export` | `dt_export` | `TRUE` | `monitora_publicacao_aa_exportar_registros_corrig_aprovado` — **1ª definição, morta** | Nenhum — código nunca executa |
| 3 | 31571 | `out` (`saneado_preparar` de `registros_importados.csv`) | `out` | `TRUE` | `monitora_registros_importados_saneado_exportar` — **2ª definição, efetiva** | **RISCO real e confirmado** — altera Encostam/forma_vida_*/espécie/hábito antes de `registros_corrig.csv` |
| 4 | 31850 | `dt_export` (preparado a partir de `registros_corrig`) | `dt_export` | `TRUE` | `monitora_publicacao_aa_exportar_registros_corrig_aprovado` — **2ª definição, efetiva** | Uso legítimo — esta é a camada autorizada. Confirmado empiricamente que produz **o mesmo resultado** da chamada 3 para os mesmos dados, ou seja, a correção já havia ocorrido uma etapa antes. |

Nota: as chamadas 1 e 2 estão dentro das definições **mortas** de duas funções que existem duplicadas no script (mesmo padrão já documentado nas Auditorias 03.5A e nos Hotfixes anteriores). Não representam risco de execução, mas são ruído de auditoria — quem lê o arquivo sequencialmente encontra essas chamadas primeiro e pode concluir algo sobre o comportamento real que não se sustenta.

## 5. Tabela de colunas potencialmente alteradas

Baseado na regex de `monitora_produtos_classificar_pipe_coluna` (linha ~31503–31511) e no comportamento real confirmado (`alterar <- corrigir && classe %in% c("pipe_residual_operacional", "pipe_residual_revisar")` — ou seja, **tudo que não é explicitamente protegido é alterado**):

| Coluna / padrão | Classificação | Alterada quando `corrigir=TRUE`? | Permitido em `registros_importados`? | Permitido em `registros_corrig`? |
|---|---|---|---|---|
| Qualquer nome contendo "encostam" (a coluna rotulada de Encostam) | `pipe_residual_operacional` | **Sim** | Não (definição da camada) | Sim |
| Qualquer nome casando `forma.*vida` (nativa, exótica, seca_morta, outros) | `pipe_residual_operacional` | **Sim** | Não | Sim |
| Qualquer nome contendo "especie" | `pipe_residual_operacional` | **Sim** | Não | Sim |
| Qualquer nome contendo "habito" (sem acento — ver nota) | `pipe_residual_operacional` | **Sim** | Não | Sim |
| Qualquer nome contendo "ponto"/"uuid"/"observacao" | `pipe_residual_operacional` | **Sim** | Indeterminado (não são ecológicas, mas ainda são alteração de valor) | Sim |
| `forma_serrapilheira` (canônico ou rotulado) — **confirmado no ensaio** | `pipe_residual_revisar` (não casa nenhum padrão explícito, cai no default) | **Sim** (o default também é alterado) | Não | Sim |
| Qualquer outra coluna de texto não explicitamente protegida (nomes desconhecidos, futuros campos do protocolo) | `pipe_residual_revisar` | **Sim** | Indeterminado/Risco genérico | Sim |
| Foto/imagem/mídia/anexo | `pipe_residual_tecnico_tolerado` | Não | OK | OK |
| Colunas técnicas (`MONITORA_*`, `arquivo_*`, `origem_*`, `hash_*`, `md5`, `tipo_entrada`, `.id`) | `pipe_residual_tecnico_tolerado` | Não | OK | OK |
| Qualquer coluna quando `produto` contém "bruto" | `pipe_esperado_em_bruto` | Não (protegida por nome de produto, não de coluna) | — | — |

**Nota sobre acentuação**: a checagem usa `tolower()` sem remoção de acentos (`grepl("habito", cc)`); se o nome real da coluna de hábito no schema usar "hábito" (com acento), a correspondência pode falhar dependendo de como R trata a comparação de bytes acentuados — **indeterminado sem inspecionar o nome exato da coluna no schema real**, não testado nesta etapa.

## 6. Resultado do ensaio sintético

Construído em `/tmp` (não versionado), simulando 1 COLETA com 3 "pontos" e resíduo de `|` nas colunas ecológicas — o artefato que a função foi desenhada para desfazer:

| Cenário | Bruto | Importado | Corrigido | Diferença (bruto→importado) | Classificação |
|---|---|---|---|---|---|
| Encostam/`tipo_forma_vida` com resíduo `solo_nu\|nativa\|serrapilheira` | Preservado como veio (`solo_nu\|nativa\|serrapilheira` nas 3 linhas) | Resolvido por posição: linha 1→`solo_nu`, linha 2→`nativa`, linha 3→`serrapilheira` | Idêntico ao importado | **Sim, alterado** | `sanitizacao_contratual`/`correcao_semantica` de posicionamento (não decide biologia, mas decide qual token sobrevive por linha) |
| `solo_nu` coexistindo com `nativa` no mesmo resíduo | Ambos preservados juntos na mesma célula (não é uma violação real de exclusividade, é um artefato de exportação) | Cada token vai para uma linha diferente (não há mais coexistência na MESMA célula após a resolução) | Idêntico ao importado | Sim, alterado | `sanitizacao_contratual` — resolve o artefato, mas não é a checagem de exclusividade contratual do Hotfix 02 (essa não existe neste caminho) |
| `forma_vida_nativa` com resíduo `graminoide\|bambu\|lianas` (inconsistência potencial com Encostam) | Preservado | Resolvido por posição (uma forma por linha) | Idêntico ao importado | Sim, alterado | `correcao_semantica` |
| `forma_serrapilheira` com resíduo `serrapilheira\|fragmentos_botanicos\|material_inundado` | Preservado | Resolvido por posição, **apesar de não casar em nenhum padrão explícito da classificação** (cai no default `pipe_residual_revisar`, que também é alterado) | Idêntico ao importado | Sim, alterado | `sanitizacao_contratual` — achado novo desta etapa |
| `forma_vida_outros` com resíduo `musgos\|liquens\|fungos` | Preservado | Resolvido por posição | Idêntico ao importado | Sim, alterado | `correcao_semantica` |
| Espécie e hábito com resíduo | Preservado | Resolvido por posição | Idêntico ao importado | Sim, alterado | `correcao_semantica`/`derivacao_auxiliar` conforme o campo |
| Controle: produto forçado a conter "bruto" | Preservado | (não aplicável — chamada real nunca ocorre para este produto) | — | Não alterado (confirmado por teste forçado) | `leitura_tecnica` — protegido corretamente |
| Controle: `COLETA`/`ponto_amostral` (sem resíduo de `\|`) | Preservado | Preservado (função só age em células que já contêm `\|`) | Preservado | Não | `leitura_tecnica` |

**Resposta direta às perguntas do ensaio:**
- `registros_importados_bruto` preserva os valores? **Sim, sempre** — confirmado inclusive sob chamada forçada.
- `registros_importados` já altera algum valor ecológico? **Sim, confirmado**, mas apenas condicionalmente: só quando a célula já contém um resíduo literal de `|`. Não há alteração para células sem esse artefato.
- Quais colunas mudam? Encostam/`tipo_forma_vida`, `forma_vida_nativa` (e por extensão `exotica`/`seca_morta` pelo mesmo padrão), `forma_vida_outros`, `forma_serrapilheira` (via default, achado novo), espécie, hábito, e potencialmente qualquer coluna de texto não protegida.
- A mudança é técnica/organizacional ou semântica? **Mista**: a motivação é técnica (desfazer um artefato de achatamento de exportação), mas o efeito é a escolha de qual valor ecológico sobrevive por linha — isso é decisão de conteúdo, não apenas de formato/nome.
- Essa mudança deveria ocorrer só em `registros_corrig`? **Sim**, segundo a definição de camadas fornecida nesta auditoria. Hoje ocorre igualmente (com resultado idêntico) em `registros_importados.csv`.

## 7. Lista de evidências

- `monitora_produtos_resolver_pipes_por_ponto` — única definição, 24596–24680.
- `monitora_produtos_classificar_pipe_coluna` — 2 definições: 24587 (morta) e 31503 (efetiva); regra crítica na linha 31509 e comportamento real de "tudo que não é exceção é alterado" confirmado pela linha 24635 (`alterar <- isTRUE(corrigir) && classe %in% c("pipe_residual_operacional", "pipe_residual_revisar")`).
- `monitora_produtos_coluna_coleta_generica` / `monitora_produtos_coluna_ponto_generica` — 24571–24585 (resolução do índice de posição usado para escolher qual token sobrevive).
- Chamadas: 24524 e 29978 (mortas, dentro de definições duplicadas mortas); 31571 (`registros_importados.csv`, efetiva); 31850 (`registros_corrig.csv`, efetiva).
- `monitora_registros_importados_saneado_preparar` — única definição, 24489–24512 (já auditada: não toca colunas ecológicas, apenas identificação/chave — confirma que a única fonte de alteração ecológica no caminho de `registros_importados.csv` é a chamada 3 desta tabela).
- Ensaio sintético — `/tmp/.../scratchpad/audit035c_ensaio.R` (não versionado): confirma empiricamente que colunas ecológicas com resíduo de `|` saem diferentes entre bruto e importado, e idênticas entre importado e corrigido.

## 8. Conclusão sobre os quesitos

- **`registros_importados_bruto.csv` é fiel?** **Sim, confirmado novamente** — nenhuma chamada real o alcança, e mesmo sob chamada forçada a proteção por nome de produto funciona.
- **`registros_importados.csv` é apenas organização/harmonização?** **Não integralmente.** A consolidação de colunas/aliases é puramente organizacional (confirmado nas auditorias anteriores), mas a resolução de resíduo de `|` decide qual valor ecológico sobrevive por linha — isso ultrapassa harmonização de nome/ordem/metadado.
- **Sanitizações semânticas estão concentradas em `registros_corrig.csv`?** **Não totalmente.** Esta classe específica de correção (resolução de resíduo de `|` em colunas ecológicas) já ocorre, de forma idêntica, uma etapa antes.

## 9. Recomendação para Hotfix 03.5C-B (não implementado nesta etapa)

**O que mover/adiar/desligar:**
- Nas chamadas efetivas de `monitora_produtos_resolver_pipes_por_ponto` para `registros_importados.csv` (linha ~31571), usar `corrigir = FALSE` — a função continuaria gerando a auditoria de detecção (`auditoria_pipes_registros_importados.csv`), mas sem alterar nenhum valor nesta camada. A correção real permaneceria intacta na chamada de `registros_corrig.csv` (linha ~31850), que é a camada autorizada.
- Alternativa mais conservadora, caso desligar `corrigir` quebre alguma expectativa downstream: manter `corrigir=TRUE` em `registros_importados.csv`, mas gravar as duas versões (antes/depois) lado a lado em `registros_importados.csv` ou em um relatório de apoio, deixando claro para o bolsista que aquele valor já passou por uma escolha posicional.

**Quais chamadas alterar:** apenas a chamada 3 (linha ~31571, dentro de `monitora_registros_importados_saneado_exportar`, 2ª definição efetiva). Não tocar na chamada 4 (`registros_corrig.csv`), que é o uso correto.

**Quais testes sintéticos criar:**
1. Mesmo cenário desta auditoria (resíduo de `|` em Encostam/forma_vida_*/espécie/hábito/serrapilheira), confirmando que com `corrigir=FALSE` em `registros_importados.csv`: (a) o valor bruto com `|` é preservado; (b) a auditoria de detecção ainda é gerada; (c) `registros_corrig.csv` continua recebendo a correção normalmente.
2. Confirmar que nenhuma outra parte do pipeline downstream de `registros_importados.csv` espera que o resíduo de `|` já tenha sido resolvido nesse ponto (checar consumidores de `registros_importados.csv`, se houver).

**Quais regressões rodar:** baseline PNB completa (contagens golden, colunas, tempo) e pelo menos os mesmos 4 datasets reais 2025 já usados no Hotfix 03.5B (fora do Git), comparando explicitamente o conteúdo de `registros_importados.csv` antes/depois da mudança (não apenas `registros_corrig.csv`, que não deve mudar).

**Riscos:**
- Se algum consumidor externo (relatórios de apoio, comparação `registros_importados.csv` vs `registros_corrig.csv` usada pelo próprio script em auditorias) já espera que ambos venham resolvidos, desligar `corrigir` em `registros_importados.csv` pode introduzir divergência inesperada nessas comparações — precisa ser checado antes de implementar.
- A classificação "tudo que não é exceção é corrigido" é ampla; ao desligar `corrigir` para `registros_importados.csv`, colunas hoje corrigidas ali passam a preservar o resíduo `|` — desejável para colunas ecológicas, mas para colunas não-ecológicas que também caem em `pipe_residual_operacional`/`pipe_residual_revisar` (como `uuid`/`observacao`/`ponto`) pode ter efeitos colaterais de rastreabilidade a avaliar.

## 10. Fora do escopo desta auditoria

- Outra forma de vida legada.
- Bug de vírgula interna na tokenização (já documentado no Hotfix 03.5B).
- Escopo do painel 101/84.
- Centralização completa dos contratos/motores de validação.
- Implementação do Hotfix 03.5C-B em si — apenas recomendado, não aplicado.

## Segurança Git

```
$ git status --short
?? diagnostics/auditoria_035c_vazamento_importados/

$ git diff --stat
(vazio)

$ git diff --name-only
(vazio)
```

Nenhuma alteração no script. Nenhum CSV/ZIP/XLSX/RDS/DB dentro do Git. Nenhum dado real copiado para o repositório — o ensaio sintético e seus resultados existem apenas em `/tmp`.
