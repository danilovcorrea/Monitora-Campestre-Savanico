# Auditoria 03.5F — Classificação dos pipes indeterminados remanescentes

## 1. Resumo executivo

Após os Hotfixes 03.5D (checkpoint correto de bloqueio) e 03.5E (normalização de delimitadores em Encostam), a run FNCS pós-correção materializa `registros_importados.csv` corretamente, sem bloqueio e sem resíduo estruturado bloqueante. Restam, porém, `n_colunas_indeterminadas = 3` e `n_linhas_pipe_indeterminadas = 414` reportados por `monitora_bloquear_pipe_residual_produto` (categoria `pipe_indeterminado`, introduzida no Hotfix 03.5C-B): colunas não reconhecidas pelo resolvedor de contrato (`monitora_pipe_coluna_classificar_natureza`), portanto **não alteradas e não bloqueadas** — apenas reportadas.

Esta auditoria, puramente estática (contrato XLSForm embutido no script + tabelas de alias já existentes no próprio script, sem editar código nem rodar dados reais), conclui que **as 3 colunas não são realmente ambíguas quanto ao tipo** — todas têm tipo consistente (sem conflito) em todas as versões do XLSForm embutido. Elas caem em `pipe_indeterminado` por uma lacuna de **casamento de nome/label**, não por ambiguidade de contrato: o resolvedor de pipe (`monitora_pipe_coluna_classificar_natureza`) usa uma tabela de aliases pequena e um normalizador de nome (`monitora_correcao_normalizar_nome_coluna`) que **não** trata sufixo de grupo (`" (amostragem/registro)"` etc. embutido no label do produto mas ausente no label bruto do dump) nem sufixo de deduplicação (`__dup1`, `__dup2`, criado por `make.unique()` quando o mesmo label aparece mais de uma vez no mesmo lote de importação). Já existe, em outra parte do script (subsistema `monitora_validados_*`, usado para `registros_validados.csv`), uma tabela de aliases mais ampla (`monitora_validados_aliases_adicionais()`) que **já mapeia explicitamente** os 3 rótulos exatos observados — essa tabela não é consultada pelo resolvedor de pipe.

Classificação resumida (detalhe na seção 4/5): coluna 1 = **texto livre (A)**, alta confiança; coluna 2 = **estruturado duplicado por label de grupo (C)**, alta confiança; coluna 3 = **estruturado duplicado por label de grupo + sufixo `__dup`(C)**, confiança média (mapeamento por inferência posicional, recomenda-se verificação com dado real antes de qualquer alias).

## 2. Contexto após Hotfix 03.5E

Estado relatado pelo usuário na run FNCS pós-03.5E (não reexecutado nesta auditoria):

| Produto | Dimensões |
|---|---|
| `registros_importados_bruto.csv` | 16766 × 182 |
| `registros_importados.csv` | 16766 × 203 |
| `registros_corrig.csv` | 16766 × 223 |

Indicadores de Encostam (03.5E) todos zerados (Encostam vazio, vírgula residual, `outra_forma_vida,`, `solo_nu` coexistente, token colado) e pipe estruturado residual bloqueante = 0. Único item pendente: `n_colunas_indeterminadas = 3`, `n_linhas_pipe_indeterminadas = 414`, nas colunas:

1. `Espécie ou nome popular (Liana nativa)` — provável label completo no produto: `Espécie ou nome popular (Liana nativa) (amostragem/registro)`.
2. `A bromélia observada é` — provável label completo: `A bromélia observada é: (amostragem/registro)`.
3. `Selecione se a bromélia observada é __dup2` — provável label completo: `Selecione se a bromélia observada é: (amostragem/registro)__dup2`.

Não foi possível localizar, nas pastas de execução acessíveis a esta sessão, o relatório `pipe_indeterminado_registros_importados_csv.csv` da run específica que gerou esses 414 registros (as pastas de execução real de sessões anteriores foram limpas ou não incluíam ainda o Hotfix 03.5E). A distribuição exata de linhas entre as 3 colunas, portanto, não pôde ser confirmada nesta auditoria — ver seção 6 e seção 10 (recomendação de conferência).

## 3. Metodologia da auditoria

Toda a evidência veio de leitura estática do próprio `monitora_campsav_alvo_global_v2.6.0.R` (`grep`/leitura de trechos), sem editar nada e sem rodar o script:

1. Localização de cada rótulo exato (ou variante muito próxima) das 3 colunas no dump do contrato XLSForm embutido (`meta$campos`, a mesma fonte consultada por `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` e por `monitora_pipe_coluna_classificar_natureza()`), cobrindo as 4 versões de formulário embutidas (`03MAI24`, `05MAI23`, `21FEV25_nSrC9X3`, `ago2022`).
2. Verificação de `type_base` (`text`/`select_one`/`select_multiple`) para o `name`/`path` correspondente, em cada versão — para detectar conflito de tipo (que geraria `pipe_ambiguo`, não `pipe_indeterminado`).
3. Busca por essas mesmas variantes de rótulo nas tabelas de alias já existentes no script (`monitora_validados_aliases_novos()`, linha ~27400, e `monitora_validados_aliases_adicionais()`, linha ~27498), usadas pelo subsistema `registros_validados.csv`, para verificar se o mapeamento rótulo→campo canônico já está documentado em outro lugar do próprio script.
4. Inspeção da função de normalização usada pelo resolvedor de pipe (`monitora_correcao_normalizar_nome_coluna`, linha 2458) comparada à normalização usada pelo subsistema `validados` (`monitora_validados_normalizar_nome`, linha 27576) — a segunda remove explicitamente sufixos `__dup[0-9]+$` e `\.[0-9]+$`; a primeira não remove nenhum dos dois.
5. Busca pela origem do sufixo `__dup` (`make.unique(..., sep = "__dup")`, linhas 24503 e 26103) para confirmar o mecanismo de geração desse sufixo (deduplicação de nomes de coluna idênticos dentro do mesmo lote de importação).

Nenhum dado real foi lido, copiado ou versionado. Nenhuma execução do script (PNB, FNCS ou sintética) foi realizada nesta auditoria.

## 4. Tabela das 3 colunas indeterminadas

| # | Coluna | n_linhas com pipe | Tipo resolvido no contrato | Path provável | Ambiguidade | Classificação recomendada | Ação futura recomendada |
|---|---|---|---|---|---|---|---|
| 1 | `Espécie ou nome popular (Liana nativa)` | não discriminado (ver seção 2) | `text` (consistente nas 4 versões) | `amostragem/registro/forma_vida_nativa_lianas` | Nenhuma de tipo; só de casamento de label (sufixo de grupo ausente no dump) | **A — texto livre permitido** | Alias explícito (label completo → `pipe_permitido_texto_livre`) |
| 2 | `A bromélia observada é` | não discriminado (ver seção 2) | `select_one` (consistente nas 4 versões) | `amostragem/registro/forma_vida_nativa_bromelioide` | Label idêntico entre nativa/exótica/seca-morta na mesma versão; desambiguado por ausência de sufixo (convenção já usada pelo subsistema `validados`) | **C — duplicado por label de grupo** (resolve para B após alias) | Alias explícito, reaproveitando `monitora_validados_aliases_adicionais()` |
| 3 | `Selecione se a bromélia observada é __dup2` | não discriminado (ver seção 2) | `select_one` (consistente nas 4 versões) | provável `amostragem/registro/forma_vida_seca_morta_bromelioide` (inferência posicional, ver seção 5.3) | Mesma ambiguidade de label da coluna 2, **mais** sufixo `__dup2` (mecanismo de deduplicação de header, não de grupo) | **C — duplicado por label de grupo + sufixo `__dup`** (confiança média) | Verificar com amostra real antes de aliasar; não aliasar às cegas |

## 5. Análise individual

### 5.1 `Espécie ou nome popular (Liana nativa)`

1. **Nome exato no produto**: `Espécie ou nome popular (Liana nativa) (amostragem/registro)` (inferido pelo padrão de sufixo `" (amostragem/registro)"` usado por todos os campos irmãos do mesmo grupo, ex. `Espécie ou nome popular (Lianas) (amostragem/registro)`).
2. **Path canônico provável**: `amostragem/registro/forma_vida_nativa_lianas`.
3. **Tipo no XLSForm embutido**: `text`, em **todas** as 4 versões (`03MAI24`, `05MAI23`, `21FEV25_nSrC9X3`, `ago2022`) — sem exceção.
4. **Múltiplos candidatos no contrato?** Não — path único, tipo único.
5. **Conflito de tipo entre versões?** Não.
6. **Sufixo `__dup`/grupo?** Não tem `__dup`; a divergência é que o dump traz o label sem o sufixo `" (amostragem/registro)"` que aparece no header real do produto (esse sufixo indica o *grupo* XLSForm, não uma duplicata).
7. **O que o pipe representa?** Achatamento multi-ponto de texto livre (nome popular/espécie digitado pelo observador para cada ponto amostral onde uma liana nativa foi registrada) — mesma classe de campo textual já protegida pelo Hotfix 03.5C-B para `observacoes_gerais` e afins.
8. **Deveria ser preservado, resolvido, bloqueado ou continuar indeterminado?** **Preservado** (texto livre, `|` é conteúdo legítimo do observador, nunca deveria ser tratado como separador posicional).
9. **Menor correção segura futura**: adicionar ao alias table do resolvedor de pipe (`monitora_pipe_aliases_campos_conhecidos()`, hoje com uma única entrada) o rótulo completo observado (`"Espécie ou nome popular (Liana nativa) (amostragem/registro)"`) apontando para o `name` canônico `forma_vida_nativa_lianas` — a mesma estratégia já usada para o caso `impact_manejo_uso/tipos_impacto_manejo_uso_outro` no Hotfix 03.5C-C. Como o `type_base` é `text` em todas as versões, o resultado da classificação seria `pipe_permitido_texto_livre` (nunca `pipe_ambiguo`).
10. **Exigiria hotfix próprio ou só alias explícito?** Apenas alias explícito — não precisa de lógica nova nem de hotfix estrutural.

### 5.2 `A bromélia observada é`

1. **Nome exato no produto**: `A bromélia observada é: (amostragem/registro)` (sem sufixo numérico/grupo, consistente com o padrão de nomenclatura observado para os demais campos "irmão-0" do grupo nativa, ex. `A cactácea observada é: (amostragem/registro)`, `A orquídea observada é: (amostragem/registro)`).
2. **Path canônico provável**: `amostragem/registro/forma_vida_nativa_bromelioide`.
3. **Tipo no XLSForm embutido**: `select_one`, em **todas** as 4 versões.
4. **Múltiplos candidatos no contrato?** Sim, por **label** (não por path): o texto `"A bromélia observada é:"` (versão `05MAI23`) e `"Selecione se a bromélia observada é:"` (versão `ago2022`) são usados **literalmente idênticos** para os três campos irmãos do mesmo grupo temático — `forma_vida_nativa_bromelioide`, `forma_vida_exotica_bromelioide` e `forma_vida_seca_morta_bromelioide` — cada um com `name`/`path`/condição de relevância (`relevant`) diferentes, mas o mesmo texto de pergunta. Confirmado diretamente no dump embutido (3 linhas consecutivas por versão, mesmo texto de label, `name` diferente).
5. **Conflito de tipo entre versões?** Não — é `select_one` em todas.
6. **Sufixo `__dup`/grupo?** Não tem `__dup`. A desambiguação entre os 3 grupos não vem do sufixo `__dup`, e sim do padrão já convencionado no restante do script: variante sem sufixo numérico = grupo nativa, variante `.1` = grupo exótica, variante `.2` = grupo seca/morta (mesmo padrão usado para `A cactácea observada é:` / `.1` / `.2` e `A orquídea observada é:` / `.1` / `.2`, todos já presentes no script). A ausência de sufixo em `A bromélia observada é` (sem `.1`/`.2`) é o próprio sinal de que se trata do grupo nativa.
7. **O que o pipe representa?** Achatamento multi-ponto de uma resposta `select_one` (a mesma classe de artefato já resolvido para as 5 colunas do FNCS no Hotfix 03.5D) — não é lista de múltiplas opções (não é `select_multiple`), é o mesmo ponto respondido em vários pontos amostrais da mesma COLETA, concatenado com `|`.
8. **Deveria ser preservado, resolvido, bloqueado ou continuar indeterminado?** **Resolvido** por índice de ponto (mesma rotina `monitora_produtos_resolver_pipes_por_ponto` já usada para os demais campos estruturados) — uma vez classificado corretamente como estruturado, o resíduo, se sobrar após a resolução, deveria então **bloquear** no checkpoint pós-tokenização (comportamento já implementado pelos Hotfixes 03.5C-B/D; nada de novo precisa ser criado, só a classificação correta precisa chegar até lá).
9. **Menor correção segura futura**: alias explícito mapeando `"A bromélia observada é: (amostragem/registro)"` (sem sufixo) → `name` canônico `forma_vida_nativa_bromelioide`. Este mapeamento **já existe, testado e em uso**, no subsistema `validados` (`monitora_validados_aliases_adicionais()`, entrada `amostragem/registro/forma_vida_nativa_bromelioide`) — a correção mais segura é reaproveitar exatamente essa mesma associação rótulo→campo (não reinventar), evitando duplicar uma decisão de desambiguação que o script já tomou e já valida em produção via `registros_validados.csv`.
10. **Exigiria hotfix próprio ou só alias explícito?** Alias explícito é suficiente — a lógica de desambiguação por sufixo/grupo (`.1`/`.2`/sem sufixo) já existe e é usada com sucesso por outros campos do mesmo padrão; não é necessário criar uma lógica nova, apenas estender a tabela de aliases do resolvedor de pipe.

### 5.3 `Selecione se a bromélia observada é __dup2`

1. **Nome exato no produto**: provavelmente `Selecione se a bromélia observada é: (amostragem/registro)__dup2` (o sufixo `__dup2` é produzido por `make.unique(names(out), sep = "__dup")`, linhas 24503/26103 do script, quando o **mesmo header de coluna aparece mais de uma vez** dentro do mesmo lote/arquivo importado — `make.unique` deixa a 1ª ocorrência sem sufixo, a 2ª como `__dup1`, a 3ª como `__dup2`).
2. **Path canônico provável**: **inferido, não confirmado** — provavelmente `amostragem/registro/forma_vida_seca_morta_bromelioide` (ver item 6).
3. **Tipo no XLSForm embutido**: `select_one` para os 3 candidatos possíveis (nativa/exótica/seca-morta), nas 4 versões — nenhum conflito de tipo, apenas indefinição de **qual dos 3** é o correto.
4. **Múltiplos candidatos no contrato?** Sim — mesma ambiguidade de label da coluna 5.2, entre os 3 campos irmãos do grupo bromelioide.
5. **Conflito de tipo entre versões?** Não.
6. **Sufixo `__dup`/grupo?** Sim, `__dup2` — este é o dado mais importante para a análise. No dump embutido da versão `ago2022`, os 3 campos do grupo bromelioide (`forma_vida_nativa_bromelioide`, `forma_vida_exotica_bromelioide`, `forma_vida_seca_morta_bromelioide`) usam **o mesmo texto literal** `"Selecione se a bromélia observada é:"`, declarados nessa ordem exata no dump (nativa → exótica → seca/morta). Se o CSV bruto dessa versão de formulário replica essa mesma ordem de colunas (razoável, dado que XLSForm preserva a ordem de declaração no export), `make.unique()` produziria: 1ª ocorrência (nativa) → sem sufixo; 2ª ocorrência (exótica) → `__dup1`; 3ª ocorrência (seca/morta) → `__dup2`. Isso é consistente com a convenção já observada na coluna 5.2 (sem sufixo = nativa) e com o próprio script já tratando um caso análogo de `__dup1` em `monitora_validados_aliases_adicionais()` (linha 27521, para `foto_forma_vida_nativa_desconhecida03`).
7. **O que o pipe representa?** Mesma natureza da coluna 5.2 (achatamento multi-ponto de `select_one`), mas com incerteza adicional sobre **qual campo semântico** ela realmente é.
8. **Deveria ser preservado, resolvido, bloqueado ou continuar indeterminado?** **Continuar indeterminado até verificação** — o risco de mapear errado (ex.: tratar como seca/morta quando na verdade é exótica, se a ordem de colunas do CSV bruto não seguir exatamente a ordem de declaração do XLSForm) é maior aqui do que nas colunas 5.1/5.2, que têm evidência direta (path único ou convenção já testada em `validados`). Manter como `pipe_indeterminado` é o comportamento seguro atual (não bloqueia, não altera, apenas reporta) e deve ser preservado até confirmação.
9. **Menor correção segura futura, caso necessária**: **antes** de qualquer alias, confirmar com uma amostra real (fora do Git) se a coluna `__dup2` de fato corresponde à condição de relevância de `forma_vida_seca_morta_bromelioide` — por exemplo, checar se as linhas não vazias dessa coluna coincidem com linhas onde `Formas de vida de plantas secas ou mortas` contém o token `bromelioide` (evidência de correlação, sem precisar copiar dado real para o repositório, apenas rodar a checagem localmente e registrar a contagem/resultado). Só depois disso um alias explícito (mesmo padrão da coluna 5.2) seria seguro.
10. **Exigiria hotfix próprio ou só alias explícito?** Se a verificação da amostra confirmar a hipótese (seca/morta), basta alias explícito, igual à coluna 5.2. Se a verificação **não** confirmar, ou os dados brutos da versão `ago2022` não estiverem disponíveis para conferência, a coluna deve permanecer `pipe_indeterminado` (comportamento seguro já vigente) até nova evidência.

## 6. Risco de falso positivo/falso negativo

- **Falso positivo (classificar como estruturado-resolvível quando não é)**: risco baixo para as colunas 5.1 e 5.2 — ambas têm evidência direta e não ambígua (tipo consistente em 4 versões; para 5.2, o alias sem-sufixo→nativa já é usado e presumivelmente validado pelo subsistema `registros_validados.csv` em produção). Risco **maior** para 5.3, cuja associação ao campo seca/morta é inferida pela ordem de declaração no XLSForm, não confirmada por dado real — por isso a recomendação de verificação antes de aliasar.
- **Falso negativo (deixar como indeterminado algo que já poderia ser resolvido com segurança)**: baixo custo — `pipe_indeterminado` nunca altera nem bloqueia dado, apenas gera uma linha de auditoria a mais; manter 5.1/5.2 como estão (sem o alias) não têm nenhum risco de corrupção de dado, apenas deixa de aproveitar uma melhoria de precisão já disponível.
- **Risco de reintroduzir o erro do Hotfix 03.5C-C** (regex de nome legado sobrepor uma classificação de contrato): mitigado pela ordem de checagem já existente no classificador (`pipe_ambiguo` e `pipe_permitido_texto_livre` são checados antes do regex de nome legado); qualquer novo alias deve continuar entrando **antes** dessa checagem, seguindo o padrão já estabelecido.
- Nenhum dos três casos analisados apresenta conflito de `type_base` entre versões do XLSForm — portanto nenhum deles deveria, em princípio, virar `pipe_ambiguo`; a ambiguidade observada é de **rótulo compartilhado entre campos irmãos do mesmo grupo temático**, não de tipo do mesmo campo.

## 7. Aderência às diretrizes de desenvolvimento

1. **data.table**: não aplicável a esta auditoria (somente leitura); qualquer alias futuro seguiria o padrão já em `monitora_pipe_aliases_campos_conhecidos()` (tabela `data.table` pequena, sem varredura).
2. **Evitar varreduras linha a linha/recomputação global**: esta auditoria não executou o script nem processou dados; toda a evidência veio de `grep` estático sobre o próprio código-fonte.
3. **Operações atômicas, auditáveis, reversíveis**: não se aplica (nenhuma alteração de código foi feita); a recomendação para o futuro hotfix (se houver) é, por natureza, um acréscimo pequeno e reversível a uma tabela de alias já existente.
4. **Separação de camadas (bruto/cache/correções/produtos finais)**: preservada — a auditoria não tocou em nenhuma etapa do pipeline.
5. **Não usar output/log como entrada implícita**: esta auditoria tentou localizar relatórios de execuções reais anteriores **apenas como evidência de apoio** (não como insumo operacional do script) e não encontrou nenhum disponível; a conclusão não depende de nenhum arquivo de output/log, apenas do código-fonte do script.
6. **Comentários/documentação sem referência interna desnecessária**: este relatório cita apenas nomes de função já públicos no próprio script e hotfixes já documentados em `diagnostics/`.

Nenhuma diretriz foi violada; nenhuma sugeriria violação se a correção futura (alias explícito) for implementada como descrito.

## 8. Conclusão

- **Podem ser reclassificadas como texto livre permitido (categoria A)**: coluna 1 (`Espécie ou nome popular (Liana nativa)`) — alta confiança, `type_base = text` sem exceção em nenhuma versão.
- **Precisam de alias explícito (categoria C, resolvendo para estruturado uma vez aliasado)**: coluna 2 (`A bromélia observada é`) — alta confiança, reaproveitando o alias já validado em `monitora_validados_aliases_adicionais()`.
- **Devem permanecer indeterminadas até verificação (categoria C, confiança média, não aliasar às cegas)**: coluna 3 (`Selecione se a bromélia observada é __dup2`) — mapeamento para `forma_vida_seca_morta_bromelioide` é a hipótese mais provável (inferência posicional bem fundamentada), mas não deve virar alias sem confirmação com amostra real.
- **É necessário Hotfix 03.5G?** Sim, porém **pequeno e de baixo risco**: um acréscimo de 2 (ou 3, se a coluna 3 for confirmada) entradas em `monitora_pipe_aliases_campos_conhecidos()`, seguindo exatamente o padrão já usado pelo Hotfix 03.5C-C — não é necessária nenhuma mudança na lógica de classificação, no resolvedor de pipe, na guarda de bloqueio (03.5D) ou em Encostam (03.5E). Recomenda-se tratar a coluna 3 separadamente (ou como item condicional do mesmo hotfix, aplicado só após a verificação da seção 5.3, item 9) para não misturar uma correção de alta confiança com uma de confiança média no mesmo commit.

## 9. Fora do escopo

- Edição de código do script (não realizada nesta auditoria, por instrução explícita).
- Alteração do resolvedor de pipes (`monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`) — inalterado.
- Alteração de Encostam/tokens (Hotfix 03.5E) — inalterado.
- Outra forma de vida legada.
- Escopo do painel 101/84.
- Motor único de validação centralizado.
- Funcionalidades da v2.6.2_teste.
- Release pública.
- Execução de PNB ou regressão FNCS completa (não realizada; toda a evidência é estática).
- Confirmação definitiva do mapeamento da coluna 3 com dado real (fica como próximo passo, não como parte desta auditoria).

## 10. Próximo passo recomendado

1. Se o usuário quiser avançar para Hotfix 03.5G: implementar apenas os aliases de alta confiança (colunas 1 e 2) em `monitora_pipe_aliases_campos_conhecidos()`, com testes sintéticos mínimos (sem dados reais) confirmando que os dois rótulos exatos passam a resolver para `pipe_permitido_texto_livre` (coluna 1) e para o `name` canônico correto com `type_base = select_one` (coluna 2), sem alterar o comportamento de nenhuma outra coluna.
2. Antes de aliasar a coluna 3, rodar localmente (fora do Git, sem copiar dado real para o repositório) uma checagem de correlação entre a coluna `__dup2` e `forma_vida_seca_morta` conforme descrito na seção 5.3, item 9, e reportar apenas a contagem/resultado (não os dados).
3. Caso a checagem da coluna 3 confirme a hipótese, incluir o terceiro alias no mesmo Hotfix 03.5G ou em um incremento seguinte; caso não confirme, manter a coluna 3 como `pipe_indeterminado` e documentar o motivo.
4. Não é necessário rodar PNB/FNCS completos apenas para esta auditoria; a validação real de qualquer Hotfix 03.5G resultante deve seguir o mesmo roteiro de regressão (PNB golden + FNCS real) já usado nos Hotfixes 03.5D/03.5E.

## Git

```
$ git status --short
?? diagnostics/auditoria_035f_pipe_indeterminado/

$ git diff --stat
(sem alterações de código — apenas novo relatório)

$ git diff --name-only
(vazio — nenhum arquivo rastreado foi modificado)
```

Nenhum código foi alterado. Nenhum dado real foi lido, copiado ou versionado. Staging previsto apenas para:
- `diagnostics/auditoria_035f_pipe_indeterminado/AUDITORIA_035F_PIPE_INDETERMINADO.md`
