# Hotfix 03.5G2 — Restringe aliases de pipe indeterminado (reversão parcial do 03.5G)

## 1. Resumo executivo

O Hotfix 03.5G adicionou 2 aliases à tabela `monitora_pipe_aliases_campos_conhecidos()`: liana nativa (texto livre) e bromélia nativa (estruturado, `select_one`). Uma run real de FNCS com esse hotfix (commit `8c98202`) **falhou** ao exportar `registros_importados.csv`, bloqueada por 275 linhas com resíduo de `"|"` na coluna da bromélia (`n_colunas_estruturado_residual = 1`, `n_linhas_pipe_estruturado_residual = 275`, `abortou_exportacao = TRUE`).

A causa não é o alias em si estar incorreto quanto ao contrato (o `tipo_base = select_one` está corretamente confirmado nas 4 versões do XLSForm embutido), mas sim que o **resolvedor de pipe por ponto é posicional/absoluto** e a coluna da bromélia é **condicional/esparsa** (só é preenchida nos pontos em que `forma_vida_nativa` contém `'bromelioide'`, via `relevant` do XLSForm). O resolvedor atual não existe para tratar esse tipo de campo — ele aplica o token N ao ponto N da repetição, não ao N-ésimo ponto elegível. Caso real observado: COLETA 11168 tem bromelioide apenas nos pontos 90 e 91, com valor bruto `"terrestre|terrestre"`; o resolvedor tenta aplicar os 2 tokens aos pontos 1 e 2 (os primeiros da repetição), deixando os demais 99 pontos sem resolução e gerando resíduo estruturado bloqueante.

Este hotfix é uma correção cirúrgica e estritamente escopada: remove apenas a entrada da bromélia da tabela de aliases, restaurando o comportamento seguro anterior (`pipe_indeterminado`, nunca bloqueia). O alias de liana nativa (texto livre, não posicional, sem esse problema) é mantido. Nenhuma lógica de resolvedor, classificação ou bloqueio foi alterada.

## 2. Origem da regressão

| Evento | Estado da tabela de aliases | Resultado da run FNCS |
|---|---|---|
| Antes do Hotfix 03.5G | Nem liana nem bromélia aliasadas | `pipe_indeterminado` para ambas — nunca bloqueia, apenas reporta |
| Hotfix 03.5G (commit `8c98202`) | Liana → `forma_vida_nativa_lianas` (text); Bromélia → `forma_vida_nativa_bromelioide` (select_one) | Bromélia passa a `pipe_residual_estruturado_elegivel` → segue para resolução posicional → **275 linhas com resíduo → exportação bloqueada** |
| Hotfix 03.5G2 (este) | Apenas liana → `forma_vida_nativa_lianas` (text) | Bromélia volta a `pipe_indeterminado` — comportamento seguro restaurado |

O Hotfix 03.5G (seção 4 do próprio relatório) já havia identificado o risco de mapear um campo estruturado para resolução posicional incorreta, mas aplicou essa cautela apenas à coluna `__dup2` (confiança de mapeamento rótulo→campo apenas média). Para a bromélia, o mapeamento rótulo→campo em si tinha confiança alta (por isso o alias foi adicionado) — o risco real, subestimado na ocasião, não estava na identificação do campo, e sim na natureza condicional/esparsa dos dados desse campo específico, que o resolvedor posicional não suporta. A run real confirmou esse risco.

## 3. Por que o alias de bromélia era inseguro apesar do tipo `select_one` correto

A classificação de natureza (`monitora_pipe_decidir_natureza_candidatos()`) decide unicamente a partir do `tipo_base` do contrato XLSForm (`text` vs. `select_one`/`select_multiple`) — ela não tem, e nunca teve, noção de **relevância condicional** (`relevant` do XLSForm) nem de **esparsidade por ponto**. Um campo `select_one` correto quanto ao contrato pode ainda assim ser inseguro para resolução posicional se:

1. O campo só é preenchido em um subconjunto dos pontos da repetição (condicionado por outra resposta, aqui `selected(${forma_vida_nativa}, 'bromelioide')`);
2. O resolvedor por ponto (`monitora_produtos_resolver_pipes_por_ponto`, inalterado por este hotfix) assume índice absoluto — token 1 vai para o ponto 1, token 2 para o ponto 2 — e não para o 1º/2º ponto **elegível**.

Ou seja: o alias estava correto sobre *o que* é o campo (contrato), mas a tabela de aliases não tem — e nunca teve — como expressar *como* esse campo deve ser resolvido (condicional vs. sequencial). Misturar as duas coisas na mesma tabela pequena e "burra" foi o erro de design do 03.5G: ela decide apenas type_base, e esse sinal é insuficiente para campos condicionais/esparsos.

## 4. Alteração feita

Em `monitora_pipe_aliases_campos_conhecidos()` (`monitora_campsav_alvo_global_v2.6.0.R`):

| Alias | Ação |
|---|---|
| `"Espécie ou nome popular (Liana nativa) (amostragem/registro)"` → `forma_vida_nativa_lianas` | **Mantido** — `text`, não posicional, sem o problema de esparsidade condicional |
| `"A bromélia observada é: (amostragem/registro)"` → `forma_vida_nativa_bromelioide` | **Removido** — `select_one` esparso/condicional, resolvedor atual não trata |
| `"Selecione se a bromélia observada é: (amostragem/registro)__dup2"` | **Sem alteração** — nunca teve alias (já era `pipe_indeterminado` desde o 03.5G, seção 4) |

A tabela `monitora_pipe_aliases_campos_conhecidos()` passa de 4 para 3 linhas (`impact_manejo_uso/tipos_impacto_manejo_uso_outro`, `tipos_impacto_manejo_uso_outro`, liana). Nenhuma outra função foi tocada: `monitora_pipe_decidir_natureza_candidatos()`, `monitora_pipe_coluna_classificar_natureza()` e o resolvedor por ponto permanecem byte-a-byte inalterados. O bloco de comentários do Hotfix 03.5G foi preservado (histórico auditável) e um novo bloco `Hotfix 03.5G2 (reversão parcial)` foi adicionado acima da função, documentando o motivo da remoção e a causa raiz (COLETA 11168).

## 5. Testes sintéticos

Executados isoladamente (scratchpad da sessão, sem dados reais), extraindo apenas as 4 funções puras envolvidas (`monitora_pipe_aliases_campos_conhecidos`, `monitora_pipe_decidir_natureza_candidatos`, `monitora_pipe_coluna_classificar_natureza`, `monitora_correcao_normalizar_nome_coluna`) diretamente do script já com o Hotfix 03.5G2 aplicado, via extração seletiva da árvore de parse — **sem dar `source()` no arquivo inteiro** (essas funções estão aninhadas dentro de um bloco `if (!(MONITORA_MODO_EXECUCAO %in% ...))` que também contém o pipeline real; um `source()` completo poderia disparar leitura/execução real, o que este hotfix explicitamente evita). `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` foi mockada com uma `meta$campos` sintética mínima, controlada, cobrindo apenas os cenários de teste — não usa o dump XLSForm real nem qualquer dado real.

| # | Teste | Esperado | Obtido | Status |
|---|---|---|---|---|
| 1 | `"Espécie ou nome popular (Liana nativa) (amostragem/registro)"` | `pipe_permitido_texto_livre` | `pipe_permitido_texto_livre` | ✅ |
| 2 | `"A bromélia observada é: (amostragem/registro)"` | `pipe_indeterminado` | `pipe_indeterminado` | ✅ |
| 3 | `"Selecione se a bromélia observada é: (amostragem/registro)__dup2"` | `pipe_indeterminado` | `pipe_indeterminado` | ✅ |
| 4 | `observacoes_gerais` (campo texto livre não relacionado, resolução direta sem alias) | `pipe_permitido_texto_livre` | `pipe_permitido_texto_livre` | ✅ |
| 5 | Campo estruturado residual sintético conhecido (resolução direta, sem alias) | `pipe_residual_estruturado_elegivel` | `pipe_residual_estruturado_elegivel` | ✅ |
| 6 | Tabela de aliases remanescente = exatamente 3 entradas (`impact_manejo_uso` x2 + liana), sem bromélia | 3 entradas, sem bromélia | 3 entradas, sem bromélia | ✅ |

**Resultado: 6/6 testes sintéticos passaram.**

O teste 6 prova estruturalmente que nenhum alias não relacionado mudou: a tabela é comparada por igualdade de conjunto contra a lista exata esperada, não apenas por ausência da bromélia.

## 6. Aderência às diretrizes de desenvolvimento

1. **data.table**: a tabela de aliases já era, e continua sendo, um `data.table::data.table()` pequeno; a alteração é a remoção de 1 elemento de cada um dos 3 vetores (`alias_coluna`, `nome_canonico`, `motivo`) — nenhuma varredura/junção nova.
2. **Evitar varreduras linha a linha/recomputação global**: nenhuma. Custo de consulta cai (3 linhas em vez de 4).
3. **Operações atômicas, auditáveis, reversíveis**: alteração isolada a uma única função/tabela, comentada e datada (`### v2.6.0 - Hotfix 03.5G2`); o histórico do Hotfix 03.5G foi preservado no comentário (não apagado), permitindo entender a decisão original e sua reversão parcial. Reverter este hotfix é reinserir a linha da bromélia.
4. **Separação bruto/importado/corrigido/validado**: preservada — a tabela de aliases é metadado estático consultado pelo classificador, não uma etapa do pipeline de dados.
5. **Não usar output/ ou log/ como entrada implícita**: não aplicável — este hotfix não lê nem escreve `output/`/`log/`; toda a evidência veio da run real relatada pelo usuário (auditoria FNCS) e do código-fonte.
6. **Remover referências internas desnecessárias**: comentários citam apenas hotfixes e relatórios já documentados em `diagnostics/`; nenhuma referência a caminhos locais, dados reais ou identificadores internos sensíveis.

Nenhuma diretriz foi violada.

## 7. Próxima solução futura recomendada

O problema estrutural — resolvedor posicional/absoluto aplicado a campos condicionais/esparsos — não foi resolvido, apenas contido (a coluna volta a `pipe_indeterminado`, segura mas não estruturada). A solução completa exigiria um **resolvedor condicional/esparso por relevância**: em vez de mapear token N → ponto N da repetição, mapear token N → N-ésimo ponto **elegível** (isto é, o ponto cuja condição `relevant` associada — aqui, `forma_vida_nativa` conter `'bromelioide'` naquele ponto específico — é verdadeira), reconstruindo a relação entre os tokens do pipe e o subconjunto real de pontos preenchidos, não a sequência absoluta 1..101. Isso exigiria acesso, no momento da resolução, ao valor da coluna condicionante (`forma_vida_nativa`) por ponto, não apenas ao valor achatado da própria coluna condicional — uma mudança de escopo maior que um hotfix cirúrgico, deliberadamente **não implementada agora**.

## 8. Fora do escopo

- Implementação do resolvedor condicional/esparso por relevância (seção 7) — deliberadamente não feita neste hotfix.
- Qualquer alteração ao resolvedor posicional existente (`monitora_produtos_resolver_pipes_por_ponto`) — inalterado.
- Qualquer alteração à guarda de bloqueio (`monitora_bloquear_pipe_residual_produto`) — inalterada.
- Alias de `__dup2` — permanece fora de escopo, como já era desde o Hotfix 03.5G.
- Execução de PNB ou FNCS completos, reais — não realizada nesta etapa (ver seção 9).
- Qualquer outro campo condicional/esparso do contrato (ex.: campos irmãos exótica/seca-morta do mesmo grupo temático) — não auditados aqui; se algum deles também tiver alias na tabela sujeito ao mesmo problema, é investigação separada.

## 9. Próximo passo de validação real (a cargo do usuário)

Não executado nesta etapa, por instrução explícita do checklist. Roteiro sugerido, igual ao já usado nos hotfixes anteriores da série 03.5:

```bash
export MONITORA_MODO_EXECUCAO=ate_registros_corrig
export MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS=S
export MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S
export MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=S
/usr/bin/Rscript monitora_campsav_alvo_global_v2.6.0.R
```

Critérios de aprovação:
- `abortou_exportacao = FALSE` para `registros_importados.csv` (a run que falhou com `8c98202` deve passar).
- `n_colunas_estruturado_residual = 0` e `n_linhas_pipe_estruturado_residual = 0` (a bromélia não deve mais aparecer como resíduo estruturado bloqueante, pois volta a `pipe_indeterminado`).
- `n_colunas_indeterminadas` volta a incluir a coluna da bromélia (esperado: 2 — bromélia + `__dup2` —, revertendo o ganho de 1 obtido pelo 03.5G para essa coluna especificamente; a coluna liana permanece resolvida, portanto não conta mais como indeterminada).
- PNB golden preservada (mesma baseline já validada em hotfixes anteriores da série 03.5).

## Git

```
$ git status --short
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035g2_restringe_aliases_pipe/

$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | (ver abaixo)
```

Nenhum CSV/ZIP/XLSX/XLS/ODS/RDS/RDA/SQLITE/DB/GPKG/SHP/GEOJSON/KML/KMZ real staged ou versionado. Nenhum dado real foi lido, copiado ou executado nesta etapa — apenas o relato de auditoria já fornecido pelo usuário e testes sintéticos isolados. Staging previsto apenas para:
- `monitora_campsav_alvo_global_v2.6.0.R`
- `diagnostics/hotfix_035g2_restringe_aliases_pipe/HOTFIX_035G2_RESTRINGE_ALIASES_PIPE.md`
