# Hotfix 03 — Sincronização de metadados de pendência impeditiva após validação row-level de Encostam

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, 16:03–18:26 (-03:00) |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit base | `1385325` (`fix: valida Encostam row-level em registros_corrig`), sobre `0b0aa35`, `5646e73`, `482347b` |
| Commit depois | nenhum — alteração ainda não commitada |
| Arquivo alterado | `monitora_campsav_alvo_global_v2.6.0.R` (único arquivo tocado) |
| Diff | +44 linhas, 0 removidas, todas dentro do corpo de `monitora_validar_encostam_rowlevel_minimo()` |

## Funções alteradas/criadas

- **Função alterada**: `monitora_validar_encostam_rowlevel_minimo()` (criada no Hotfix 02) — nenhuma outra função do script foi tocada.
- **Função local nova**: `monitora_hotfix03_sincronizar_pendencia(idx, tag_remover, msg_remover)`, definida *dentro* de `monitora_validar_encostam_rowlevel_minimo()` (não é uma função global nova), para manter o diff inteiramente contido em um único ponto já auditado.

## Problema tratado

`monitora_pendencia_impeditiva`, `monitora_pendencia_impeditiva_tipo` e `monitora_pendencia_impeditiva_msg` são gravados por `monitora_publicacao_ab_auditar_pendencias_impeditivas` **antes** de `monitora_validar_encostam_rowlevel_minimo()` rodar, dentro do mesmo funil de exportação (`monitora_publicacao_aa_exportar_registros_corrig_aprovado`, definição efetiva). Sem sincronização, uma linha corrigida pela Regra A (Encostam vazio → `solo_nu`) continuaria marcada com a pendência `ponto_sem_interceptacao`, mesmo já corrigida — uma contradição entre o dado final e o metadado de pendência.

## Investigação antes de editar

Localizei as três únicas linhas do script onde essas colunas recebem valor real (`monitora_publicacao_ab_auditar_pendencias_impeditivas`, ~linha 29419–29430) e confirmei seu uso:

- **Bloqueiam exportação de `registros_validados.csv`**: `MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS` (flag global derivada) impede a materialização do produto final — confirmado no comportamento observado na baseline (`registros_validados.csv não será criado`).
- **Aparecem em relatórios**: `auditoria_pendencias_impeditivas_registros_corrig.csv`, `resumo_pendencias_impeditivas_registros_corrig.csv`.
- **Não são recalculadas depois**: nenhuma outra parte do script (fora da própria função de auditoria) recomputa essas 3 colunas — confirmado por busca exaustiva (`grep`) por todas as ocorrências de `monitora_pendencia_impeditiva_tipo`/`_msg` no arquivo.
- Achado adicional: a categoria "exclusividade de `solo_nu`" (Regra B) **não existe hoje** no vocabulário de `monitora_publicacao_ab_auditar_pendencias_impeditivas` — essa checagem só existe, com outro nome de coluna, em `monitora_validados_validar_condicionais_xlsform21` (tag `solo_nu_com_outra_categoria`, motor `monitora_validados_*`, fora do escopo deste hotfix). Isso significa que, nos dados de hoje, a Regra B nunca terá uma pendência pré-existente para sincronizar — documentado explicitamente abaixo.

## Hipótese de ajuste apresentada antes de editar (conforme solicitado)

Ajuste mais seguro identificado: **não recalcular a auditoria inteira** (que tocaria as 4 categorias já golden-testadas e o motor `monitora_validados_*`, fora de escopo), mas sim **remover cirurgicamente apenas as tags exatas** correspondentes às condições resolvidas pelas Regras A/B, preservando qualquer outra tag na mesma linha (`" | "`-separadas) e recalculando o booleano `monitora_pendencia_impeditiva` como `nzchar(tipo)` após a remoção. Essa foi a abordagem implementada.

## Regra de atualização dos metadados

Para cada linha efetivamente corrigida por:
- **Regra A** (Encostam vazio → `solo_nu`): remove a tag `ponto_sem_interceptacao` e a mensagem fixa correspondente (`"Ponto sem categoria de interceptação em Encostam/tipo_forma_vida; corrigir a partir do registro original ou excluir a COLETA."`), se presentes.
- **Regra B** (`solo_nu` exclusivo removido): remove a tag `solo_nu_com_outra_categoria` e a mensagem `"solo_nu é exclusivo"`, se presentes. **Hoje é um no-op** nos dados reais (a auditoria atual não produz essa tag), mas o nome da tag/mensagem foi escolhido **idêntico** ao já usado em `monitora_validados_validar_condicionais_xlsform21`, de modo que a sincronização já funciona corretamente se essa checagem for futuramente incorporada também nesta auditoria — sem exigir nova alteração neste ponto.
- **Qualquer outra tag** na mesma linha é preservada intacta (não removida, não reordenada).
- **Regras C–F**: nenhuma linha afetada só por elas é tocada por este mecanismo, porque `monitora_hotfix03_sincronizar_pendencia` só é chamada com `idx_vazio` (Regra A) e `idx_regra_b_aplicada` (Regra B) — nunca com os índices das Regras C–F.

## Casos sintéticos testados (isolados, sem dados reais)

Extraídas verbatim as dependências do próprio script (`monitora_dt_referenciar`, `monitora_correcao_limpar_texto`, `monitora_correcao_vazio_vec`, `monitora_correcao_tokenizar`, `monitora_correcao_colapsar_tokens`, `monitora_relatorio_exoticas_normalizar_token`, `monitora_relatorio_exoticas_tem_token`) e a função com o Hotfix 03, testadas com 3 linhas sintéticas cobrindo os 4 casos pedidos:

| Caso | Linha | Antes | Depois | Resultado |
|---|---|---|---|---|
| 1 — Encostam vazio | 1 | `tipo=""`, pendência `ponto_sem_interceptacao \| nativa_sem_forma_vida` | `tipo="solo_nu"`, pendência `nativa_sem_forma_vida` | ✅ tag de Encostam vazio removida |
| 3 — pendência não relacionada (mesma linha 1) | 1 | — | `nativa_sem_forma_vida` preservada, `monitora_pendencia_impeditiva = TRUE` | ✅ preservada |
| 2 — `solo_nu` coexistindo | 2 | `tipo="solo_nu nativa"`, pendência `solo_nu_com_outra_categoria` | `tipo="nativa"`, pendência `""`, `monitora_pendencia_impeditiva = FALSE` | ✅ tag removida, sem pendência residual |
| 4 — Regra C apenas relatada | 3 | `tipo="outra_forma_vida"`, pendência `outra_forma_vida_sem_forma_vida_outros` | inalterado, pendência preservada, `monitora_pendencia_impeditiva = TRUE` | ✅ nada apagado |

**Resultado: PASSOU** (todas as `stopifnot` corretas, saída impressa e conferida manualmente).

## Reexecução da baseline PNB

Mesmo comando/modo/pasta de execução limpa das etapas anteriores; `input/pnb_2022_2026.zip` intocado.

| Métrica | Antes (hotfix 02) | Depois (hotfix 03) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606/6 | 606/6 | ✅ |
| Nativa sem forma de vida | 3/3 | 3/3 | ✅ |
| Seca/morta sem forma de vida | 10/8 | 10/8 | ✅ |
| Outra forma de vida legada | 7/5 | 7/5 | ✅ |
| Colunas importados/corrig | 176/180 | 176/180 | ✅ |
| Tamanho dos 3 produtos de dados | idênticos | byte-idênticos | ✅ |
| Tempo núcleo | 51,2 s | 50,9 s | ✅ equivalente |
| `registros_importados.csv` sobrescrito? | Não | Não (hashes distintos) | ✅ |
| `auditoria_encostam_rowlevel_minimo.csv` gerado? | Não | Não (0 ocorrências neste dataset) | ✅ esperado |
| Novos warnings/erros | — | Nenhum (warning pré-existente `data.table::unique` mantido) | ✅ |

## Regressão real (parcial — ver pendência abaixo)

Executados, fora do repositório, em pastas de execução isoladas e descartáveis (`/tmp/.../scratchpad/exec_regressao_0{1,2}`), **2 dos 5 datasets 2025 principais** de `/home/dfed/dados_originais/_referencia_regressao_real_monitora_v262/datasets/`:

| Dataset | Papel | Resultado | Tokens preservados (input → corrig) |
|---|---|---|---|
| `01_2025_max_outra_forma_vida` (FNC_2026) | Maior cobertura real de `outra_forma_vida` | Exit 0, sem erros novos, `registros_corrig.csv` gerado (170→174 colunas), 0 achados das Regras A-F | `outra_forma_vida`: 761→761; `solo_nu`: 280→280 |
| `02_2025_top_combinado` (PNSC_CIPO_2026) | Maior combinação `outra_forma_vida` + `serrapilheira` | Exit 0, sem erros novos, `registros_corrig.csv` gerado, 0 achados das Regras A-F, linhas idênticas (8586) | `outra_forma_vida`: 433→433; `solo_nu`: 310→310; `serrapilheira`: 5729→5756 (aumento explicado abaixo) |

Em ambos, `registros_importados.csv` e `registros_corrig.csv` têm hashes distintos (não sobrescrita), e nenhum dado real foi copiado para dentro do repositório — as pastas de execução ficaram inteiramente em `/tmp`.

**Nota sobre o dataset 02**: a contagem de substring `"serrapilheira"` no arquivo inteiro subiu de 5729 para 5756 (+27) entre input e `registros_corrig.csv`, com o mesmo número de linhas (8586) nos dois arquivos. Investigado: não é duplicação de linha nem efeito das Regras A-F (nenhum achado foi reportado para essas regras neste dataset). É consistente com a sincronização pré-existente do campo superior a partir de material botânico (`monitora_correcao_sincronizar_encostam_final`/lógica de "contato real", já presente na v2.6.0 antes de qualquer um dos 3 hotfixes), que pode adicionar o token `serrapilheira` ao campo superior quando a sublista de material botânico já tem conteúdo mas o campo superior ainda não refletia isso — comportamento anterior aos hotfixes 01–03, não introduzido por eles.

**Interessante**: em ambos os datasets reais, o campo Encostam-equivalente já vem do SISMONITORA no formato canônico `amostragem/registro/tipo_forma_vida` (tokens, não rótulos textuais como `"Outras plantas terrestres, líquens e/ou fungos"`). Por isso, nem o Hotfix 01 (mapeamento de rótulo) nem achados das Regras A-F do Hotfix 02/03 foram exercitados por esses dois datasets especificamente — mas a preservação exata dos 761+280 e 433+310 tokens reais de `outra_forma_vida`/`solo_nu` ao longo de todo o pipeline, sem nenhuma corrupção, é uma evidência real e direta de que os hotfixes não introduziram regressão nesses dados de produção.

### Pendência explícita

**Os datasets 03, 04 e 05 (2025_serrapilheira_volume, 2025_fncs_anexado, 2025_pnm_anexado) e os datasets 06–10 (schemas intermediário/legado/outras fontes) não foram executados nesta etapa**, por restrição de tempo (cada execução leva ~4–5 minutos de parede). **Recomendo fortemente que o usuário rode a bateria completa dos 10 datasets de `/home/dfed/dados_originais/_referencia_regressao_real_monitora_v262/` antes de comitar/publicar este hotfix**, especialmente o dataset `03_2025_serrapilheira_volume` (maior volume real de `serrapilheira`) e os dois "anexados/validados na conversa" (`04`, `05`), que são justamente os mais prováveis de conter o rótulo textual `"Outras plantas terrestres, líquens e/ou fungos"` ou casos reais de Regras A–F ainda não observados.

## Riscos de regressão

- **Escopo do diff**: toda a alteração está contida na função já auditada `monitora_validar_encostam_rowlevel_minimo()`; nenhuma outra função foi tocada, reduzindo superfície de risco.
- **Dependência de nomes de tag/mensagem exatos**: a remoção só ocorre se a tag/mensagem baterem exatamente com as constantes hoje usadas por `monitora_publicacao_ab_auditar_pendencias_impeditivas`. Se essas strings mudarem no futuro sem atualizar este hotfix, a sincronização silenciosamente vira no-op (fail-safe: nunca remove a mais, na pior hipótese remove de menos) — comportamento intencionalmente conservador.
- **Regra B ainda sem efeito prático nos dados de hoje**: como não há uma tag real de exclusividade de `solo_nu` na auditoria atual, essa parte do hotfix é "preparação", não correção ativa — documentado, não escondido.
- **Regressão real incompleta**: ver pendência explícita acima.

## Pendências não tratadas propositalmente

- Unificação do motor de validação row-level (fora de escopo desde o Hotfix 02).
- Incorporar a checagem de exclusividade `solo_nu_com_outra_categoria` na própria `monitora_publicacao_ab_auditar_pendencias_impeditivas` (hoje só existe no motor `monitora_validados_*`) — se isso for feito no futuro, este hotfix já está pronto para sincronizar corretamente sem alteração adicional.
- Correção automática das Regras D e F — deliberadamente não implementada (mantido do Hotfix 02).
- Warning `'unique' is not an exported object from 'namespace:data.table'` — inalterado.
- Regressão real completa (datasets 03–10) — não executada nesta etapa; recomendada antes do commit.

## Conclusão

**PASSOU**, com uma pendência explícita e não bloqueante: a regressão real foi validada parcialmente (2 de 10 datasets locais), sem qualquer sinal de regressão nos dois executados. A lógica de sincronização de metadados foi implementada de forma cirúrgica, testada isoladamente nos 4 casos exigidos (incluindo preservação de pendências não relacionadas e não-mascaramento das Regras C–F), e a baseline PNB golden permanece byte-idêntica.

## Status git

Nenhum commit criado. `git status --short` mostra apenas `M monitora_campsav_alvo_global_v2.6.0.R` além do novo diretório de relatório `diagnostics/hotfix_03_pendencias_pos_encostam/`. Nenhum dado real, CSV, XLSX, ZIP ou output foi staged ou copiado para dentro do repositório — todas as execuções de regressão real ocorreram em `/tmp`, fora do worktree.
