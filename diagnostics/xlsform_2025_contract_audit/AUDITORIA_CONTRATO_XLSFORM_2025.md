# Auditoria estática — Contrato XLSForm 2025 vs. script v2.6.0

## Metadados

| Campo | Valor |
|---|---|
| Data/hora da auditoria | 2026-07-02 |
| Worktree auditado | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit | `482347b68f935e9938da957997d14103dabf7cf0` |
| Tag de origem | `v2.6.0` (anotada) |
| Status do worktree no início/fim | Limpo de commits; apenas arquivos não rastreados (`diagnostics/`, `docs/manual_usuario_v2.6.0.*`, `referencias_contrato_2025/`). Nenhum código editado, nenhum commit criado. |
| Relatório baseline PNB | `diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md` — presente, PASSOU |
| Handoff | `docs/desenvolvimento/HANDOFF_DEV_V262_GOLDEN_V260.md` — presente e usado como referência de premissas |
| Script auditado | `monitora_campsav_alvo_global_v2.6.0.R` (45.247 linhas) |

## Arquivos contratuais auditados

Ambos os arquivos já estavam disponíveis no Fedora (não foi necessário solicitar upload):

| Arquivo | Localização usada | SHA-256 |
|---|---|---|
| XLSForm 2025 | `referencias_contrato_2025/PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25_nSrC9X3.xlsx` | (idêntico ao anexo do usuário; também presente em `~/Desktop/02jul/`) |
| Template CSV 2025 | `referencias_contrato_2025/PNB_2026_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv` | (idêntico ao anexo do usuário) |

Versão do XLSForm detectada (aba `settings`): `form_id = PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25`, `version = 2025022101`, `form_title = "Terrestre - Campestre e Savânico - Plantas Herbáceas e Lenhosas, Nativas e Exóticas - Básico e Avançado"`.

Template CSV: **129 colunas**, UTF-8, separador vírgula — confirmado por leitura direta. Colunas críticas confirmadas nas posições 38–40: `amostragem/registro/tipo_forma_vida`, `amostragem/registro/forma_serrapilheira`, `amostragem/registro/forma_vida_outros`.

Campos críticos da aba `survey`/`choices` do XLSForm, lidos diretamente (não inferidos):

- `tipo_forma_vida` — `select_multiple tipo_forma_vida`, required=yes, constraint `if(selected(.,'solo_nu'), count-selected(.)=1, count-selected(.)>=1)`. Domínio: `solo_nu, serrapilheira, nativa, exotica, seca_morta, outra_forma_vida`.
- `forma_serrapilheira` (lista `forma_vida_serrapilheira`) — relevant `selected(${tipo_forma_vida},'serrapilheira')`, required=yes. Domínio: `serrapilheira, fragmentos_botanicos, material_inundado`.
- `forma_vida_outros` — relevant `selected(${tipo_forma_vida},'outra_forma_vida')`, required=yes. Domínio: `musgos, hepaticas, antoceros, liquens, fungos`.

Tudo confirmado bate exatamente com o contrato já relatado pelo usuário a partir dos anexos.

---

## Classificação por regra contratual

### 1. Campo Encostam / `tipo_forma_vida` — nome, tipo, required

**Classificação: OK.**
O script reconhece `tipo_forma_vida`/Encostam como campo canônico central em todo o pipeline (`chaves$tipo_forma_vida`, dezenas de funções `monitora_correcao_*`/`monitora_painel_*`/`monitora_validados_*`). O "required" (Encostam não pode ficar vazio) é ativamente checado como pendência impeditiva:

- `monitora_publicacao_...` (bloco de pendências impeditivas, ~linha 29320): `add_issue("ponto_sem_interceptacao", "Ponto amostral sem interceptação", monitora_correcao_vazio_vec(tipo), ...)`.

### 2. Constraint de exclusividade de `solo_nu` (`count-selected`)

**Classificação: PARCIAL.**
A regra de exclusividade (`solo_nu` só pode aparecer sozinho) está corretamente implementada em `monitora_validados_validar_condicionais_xlsform21` (linha 28328–28330: `add("solo_nu_com_outra_categoria", tipo_col, tem_solo & tem_outro, "solo_nu é exclusivo")`), e também é preventivamente respeitada por `monitora_validados_aplicar_regras_xlsform21` (linha 28189–28191: remove `solo_nu` do token reconstruído se qualquer outra categoria estiver presente).

Porém essa checagem **só roda dentro do motor opcional de `registros_validados.csv`**, que só é acionado quando `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = "S"` e sem pendências impeditivas em `registros_corrig.csv`. O motor de "pendências impeditivas" que decide se `registros_corrig.csv` fica marcado como checkpoint (linha ~29300–29400) **não verifica exclusividade de `solo_nu`**. Ou seja, `registros_corrig.csv` pode conter uma linha com `solo_nu` concomitante a outra categoria sem que isso seja sinalizado como pendência impeditiva no produto operacional principal.

### 3. Mapeamento "Outras plantas terrestres, líquens e/ou fungos" → `outra_forma_vida` (nunca `solo_nu`)

**Classificação: CONTRADIÇÃO (crítica).**
Encontrado em `monitora_campsav_alvo_global_v2.6.0.R`, linhas **32925–32943**, no bloco de tokenização da coluna rotulada `**Encostam** na vareta: (amostragem/registro)`:

```r
registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Plantas nativas" = "nativa",
      ...
      "Solo nu / rochas \\(sem plantas tocando a vareta\\)" = "solo_nu",
      "Material botânico em decomposição no solo" = "serrapilheira",
      "Outras plantas terrestres, líquens e/ou fungos" = "solo_nu",   # <-- linha 32940, ERRADO
      "Outras plantas terrestres, líquens e/ou fungos," = "solo_nu"  # <-- linha 32941, ERRADO
    )
  )
```

Isto é **exatamente** o erro descrito na seção "Erro de solo_nu" do handoff (`docs/desenvolvimento/HANDOFF_DEV_V262_GOLDEN_V260.md`), mas o handoff o apresenta implicitamente como problema a corrigir "daqui para frente" — a auditoria confirma que **ele já existe no próprio código da golden baseline v2.6.0**, não apenas na v2.6.2_teste.

Isso contradiz diretamente:
- a Regra 3 do contrato confirmado pelo usuário ("nunca mapear para solo_nu");
- a lógica correta e mais sofisticada que existe em **outras duas partes do mesmo script**: `monitora_correcao_calcular_tipo_forma_vida_esperado`/`monitora_correcao_sincronizar_encostam_final` (linhas 8733–8790, preserva `outra_forma_vida` corretamente como "contato real") e `monitora_validados_aplicar_regras_xlsform21` (linha 28186: `tipo_novo <- ifelse(outros | outra_original, monitora_validados_adicionar_token(tipo_novo, "outra_forma_vida", tipo_choices), tipo_novo)`), que reconstroem o campo superior corretamente. O script portanto contém **duas implementações corretas e uma incorreta da mesma regra**, o que é uma contradição interna, não apenas um bug isolado.

**Verificação empírica no dado real (baseline PNB já executada):** a string literal `"Outras plantas terrestres, líquens e/ou fungos"` tem **0 ocorrências** em `registros_importados_bruto.csv` e em `registros_importados.csv` do dataset comparativo PNB 2022–2026 usado na baseline. Ou seja, **o bug está presente e ativo no código, mas está dormente** — não foi exercitado pelo dataset PNB atual (a opção só existe no XLSForm 21FEV25/2025, e nenhum registro do dataset amostrado selecionou essa resposta especificamente no campo Encostam). O golden PNB (606/6, 3/3, 10/8, 7/5) **não detecta este bug** porque o indicador "Outra forma de vida legada" (item 4 abaixo) mede uma coisa diferente.

**Risco concreto:** assim que uma coleta real de 2025/2026 tiver essa resposta selecionada em Encostam, `registros_corrig.csv` gravará `solo_nu` em vez de `outra_forma_vida` na coluna rotulada, e — se a coleta também tiver outra categoria selecionada (ex.: "Plantas nativas,Outras plantas terrestres, líquens e/ou fungos") — o valor final ficará como `"nativa,solo_nu"`, violando a exclusividade de `solo_nu` do próprio XLSForm, sem que a checagem do item 2 (ausente no motor de `registros_corrig`) capture isso.

### 4. Tokens válidos de `tipo_forma_vida`

**Classificação: OK.**
`solo_nu`, `serrapilheira`, `nativa`, `exotica`, `seca_morta`, `outra_forma_vida` são usados de forma consistente como vocabulário canônico em dezenas de pontos do script (`MONITORA_TRIAGEM_CATEGORIAS_FORMA <- c("nativa","exotica","seca_morta")` + tratamento explícito de `serrapilheira`/`solo_nu`/`outra_forma_vida` em paralelo). Nenhum sétimo token espúrio foi encontrado no vocabulário canônico.

### 5. Campo filho `forma_vida_outros` (relevant/required/domínio)

**Domínio: OK.** Embutido corretamente em múltiplos pontos, ex. linha 26726: `c("musgos", "hepaticas", "antoceros", "liquens", "fungos")` e no dump do XLSForm 21FEV25 embutido (linhas 5079–5081), idêntico ao confirmado no arquivo `.xlsx`.

**Relevance/Required: PARCIAL/RISCO.**
`relevant = selected(${tipo_forma_vida},'outra_forma_vida')` está corretamente implementado em `monitora_validados_aplicar_regras_xlsform21` (linha 28203–28208, `relevance_forma_vida_outros`), e o `required` correspondente (campo relevante mas vazio) está em `monitora_validados_validar_condicionais_xlsform21` (linha 28341, `campo_obrigatorio_relevante_vazio`).

Porém — assim como o item 2 — essas checagens **só existem dentro do motor opcional `monitora_validados_*`**, usado apenas quando `registros_validados.csv` é gerado. O motor de pendências impeditivas de `registros_corrig.csv` (linha ~29300–29400) tem checagens equivalentes específicas para `nativa`/`exotica`/`seca_morta` sem forma de vida (`nativa_sem_forma_vida`, `exotica_sem_forma_vida`, `seca_morta_sem_forma_vida`), **mas não tem um `outra_forma_vida_sem_forma_vida_outros` equivalente**. Uma linha com `Encostam` contendo `outra_forma_vida` mas `forma_vida_outros` vazio **não é sinalizada como pendência impeditiva em `registros_corrig.csv`**, apenas — tarde demais — se e quando alguém tentar gerar `registros_validados.csv`.

### 6. Campo filho `forma_serrapilheira` (relevant/required/domínio)

**Mesma classificação do item 5: domínio OK, relevance/required PARCIAL/RISCO** pelo mesmo motivo (checagem existe só em `monitora_validados_aplicar_regras_xlsform21`/`validar_condicionais_xlsform21`, linhas 28196–28201 e 28338–28343; ausente do motor de pendências impeditivas de `registros_corrig.csv`).

### 7. Template CSV 2025 (129 colunas, UTF-8, vírgula, colunas críticas)

**Classificação: OK.** Confirmado por leitura direta do arquivo (ver seção de metadados acima). O script reconhece as três colunas críticas nos caminhos esperados e usa o mesmo layout `amostragem/registro/...` internamente.

### 8. `registros_importados.csv` não sobrescrito por `registros_corrig.csv`

**Classificação: OK** — confirmado tanto por desenho do código quanto empiricamente na baseline já executada.
Comentário explícito no próprio script (linhas ~1714, ~7666–7672): *"registros_importados.csv é produto bruto obrigatório e deve ser materializado logo após a concatenação dos CSVs lidos, antes de validações, harmonizações, deduplicações finais, painel ou registros_corrig."* A chamada real de gravação (`monitora_publicacao_h_fwrite_registros_importados`, invocada por volta da linha 31494) ocorre **antes** do bloco de tokenização arriscado do Encostam (linha 32925). Na execução real da baseline, os hashes SHA-256 e timestamps de `registros_importados.csv` e `registros_corrig.csv` são distintos e gravados em ordem correta (ver `BASELINE_V260_PNB.md`).

### 9. `registros_corrig.csv` criado a partir de `registros_importados.csv` já contaminado

**Classificação: OK (não observado).** Como o item 8 confirma que `registros_importados.csv` é gravado antes da tokenização do Encostam, ele não herda o bug do item 3. Isso foi confirmado empiricamente: 0 ocorrências do texto problemático em `registros_importados.csv` e em `registros_importados_bruto.csv` no dataset PNB testado.

### 10. Comparação de `select_multiple` por conjunto de tokens (não string ordenada)

**Classificação: OK.** `monitora_correcao_tokenizar` (linha 2418–2422) faz `strsplit(x, "\\s+")` e as operações de token usam `setdiff`/`unique`/`%in%` (`monitora_correcao_append_token_valor`, `monitora_correcao_remove_token_valor`, linhas 2424–2443), ou seja, comparação por conjunto, não por string literal ordenada. Nenhuma comparação `Encostam == "x y"` ordenada foi encontrada no núcleo do v2.6.0 (esse é um risco listado no handoff como regressão específica da v2.6.2_teste, não presente aqui).

### 11. Validação 2025 aplicada a dados legados sem migração

**Classificação: RISCO (latente, mitigado por padrão desligado).**
O sistema de metadados `meta_xls$opcoes` guarda múltiplas versões do XLSForm etiquetadas por `arquivo_xlsform` e prefere explicitamente `21FEV25|2025` com fallback para versões históricas (linha ~17698–17701) — desenho correto para a maior parte do pipeline operacional (`registros_corrig`).

Entretanto, `monitora_validados_validar_dominios_xlsform21` (linha 28266–28311) valida **todas** as colunas contra o domínio do XLSForm 21FEV25 sem filtrar por ano/versão de origem do registro — ou seja, se `registros_validados.csv` for gerado sobre uma base com registros legados (2022–2024) cujos tokens de sublista ainda não tiverem sido migrados para o vocabulário 2025, o domínio 2025 será aplicado "cegamente" a eles, gerando bloqueios (`token_fora_dominio_xlsform21`) que podem ser falsos positivos de migração, não erros reais de dado. Como `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = "N"` por padrão e a geração é bloqueada quando há pendências impeditivas (confirmado na baseline: `registros_validados.csv não será criado`), esse risco está **atualmente inerte**, mas é exatamente o item que o handoff already lista como trabalho pendente ("migração semântica legado -> canônico").

### 12. Separação `n_linhas_ocorrencia` / `n_coletas_afetadas` / `n_linhas_contexto`

**Classificação: AUSENTE.** Os três termos têm **0 ocorrências** em todo o script v2.6.0 (`grep -c` confirma). O motor de pendências impeditivas já separa corretamente `n_linhas` (`.N`) de `n_coletas` (`uniqueN(COLETA)`) no resumo de cada tipo de ocorrência (linha ~29390), o que cobre parcialmente o espírito da regra (linhas de ocorrência vs. coletas afetadas), mas **não existe** o terceiro conceito (`n_linhas_contexto` = total de linhas das coletas afetadas) em lugar nenhum. Isso é esperado — o handoff lista esse motor único como item de trabalho futuro (item 6 do "Caminho de desenvolvimento recomendado") — não é uma regressão da v2.6.0, é simplesmente ainda não implementado.

### 13. `registros_corrig.csv` não deve herdar as 129 colunas do template como expansão indevida

**Classificação: OK.** `registros_corrig.csv` tem 180 colunas (confirmado na baseline), não 129 nem 256. As colunas além das legadas/rotuladas trazidas do import são claramente prefixadas com `MONITORA_`/`monitora_` (`MONITORA_ARQUIVO_ORIGEM_EXECUCAO`, `MONITORA_CAMINHO_ARQUIVO_ENTRADA`, `MONITORA_ARQUIVO_ENTRADA`, `MONITORA_TIPO_ENTRADA`, `monitora_status_registros_corrig`, `monitora_pendencia_impeditiva*`), consistente com a premissa 5 do handoff.

### 14. `registros_validados.csv` criado sem validação row-level anterior

**Classificação: OK.** Confirmado tanto por código quanto empiricamente: a baseline real gerou o log `"registros_validados.csv não será criado: registros_corrig.csv ainda contém pendências impeditivas sinalizadas"` e nenhum arquivo `registros_validados.csv` foi produzido. Além do gate de pendências impeditivas de `registros_corrig.csv`, existe um segundo gate específico (`monitora_registros_validados_gravar_bloqueios`, linha 28643) que grava bloqueios de domínio/condicionais antes de liberar o produto final.

### 15. Motor único de validação row-level pré/pós-painel (premissa 12 do handoff)

**Classificação: AUSENTE/PARCIAL — achado estrutural relevante.**
A auditoria identificou **pelo menos quatro implementações distintas e não unificadas** que tratam da mesma família de regras (estado de `tipo_forma_vida`/Encostam e seus campos dependentes):

1. Tokenização crua por `str_replace_all` sobre o rótulo bruto do Encostam (linhas 32925–32956) — **contém o bug do item 3**;
2. `monitora_correcao_calcular_tipo_forma_vida_esperado` / `monitora_correcao_sincronizar_encostam_final` (linhas 8733–8909+) — recomputa o campo superior a partir do estado final das sublistas, corretamente;
3. Motor de "pendências impeditivas" de `registros_corrig.csv` (linhas ~29300–29440) — ad hoc, cobre nativa/exótica/seca_morta/duplicidade/interceptação vazia, mas **não** cobre exclusividade de `solo_nu` nem `outra_forma_vida` sem `forma_vida_outros`;
4. `monitora_validados_aplicar_regras_xlsform21` / `validar_dominios_xlsform21` / `validar_condicionais_xlsform21` (linhas 28132–28345+) — o mais completo e mais fiel ao contrato XLSForm 21FEV25, mas **só roda no caminho opcional de `registros_validados.csv`**.

Isso é exatamente o problema que a premissa 12 do handoff pede para eliminar (*"Deve haver um único motor de validação row-level usado em dois momentos"*). Hoje, regras corretamente implementadas no motor 4 simplesmente não protegem o produto operacional principal (`registros_corrig.csv`), o que já produziu ao menos um gap concreto (item 5/6) e um bug ativo não capturado (item 3).

---

## Resumo de classificação

| # | Regra | Classificação |
|---|---|---|
| 1 | Encostam/tipo_forma_vida: nome, tipo, required | OK |
| 2 | Exclusividade `solo_nu` (constraint) | PARCIAL |
| 3 | "Outras plantas..." → outra_forma_vida (nunca solo_nu) | **CONTRADIÇÃO** |
| 4 | Tokens válidos de tipo_forma_vida | OK |
| 5 | forma_vida_outros (relevant/required/domínio) | Domínio OK / Relevance-Required PARCIAL |
| 6 | forma_serrapilheira (relevant/required/domínio) | Domínio OK / Relevance-Required PARCIAL |
| 7 | Template CSV 2025 (129 col., UTF-8, vírgula) | OK |
| 8 | registros_importados.csv não sobrescrito | OK |
| 9 | registros_corrig não herda importados contaminado | OK |
| 10 | select_multiple por conjunto de tokens | OK |
| 11 | Domínio 2025 aplicado a dados legados sem migração | RISCO (latente, hoje inerte) |
| 12 | n_linhas_ocorrencia / n_coletas_afetadas / n_linhas_contexto | AUSENTE (esperado nesta versão) |
| 13 | registros_corrig não incha para 129 colunas do template | OK |
| 14 | registros_validados só se corrig validado | OK |
| 15 | Motor único de validação row-level | AUSENTE/PARCIAL |

---

## Recomendações de hotfix mínimo (ordem sugerida)

1. **[CRÍTICO — hotfix cirúrgico, baixo risco]** Corrigir linhas 32940–32941 de `monitora_campsav_alvo_global_v2.6.0.R`: trocar o destino do mapeamento de `"Outras plantas terrestres, líquens e/ou fungos"` (e sua variante com vírgula final) de `"solo_nu"` para `"outra_forma_vida"`. É uma alteração de duas linhas, sem impacto estrutural, e resolve a contradição mais grave encontrada. Não deve alterar o golden PNB atual (o dataset não contém essa resposta), mas remove uma bomba-relógio para dados 2025/2026 reais.
2. **[ALTO]** Adicionar ao motor de pendências impeditivas de `registros_corrig.csv` (bloco ~linha 29300–29400) duas checagens novas, espelhando o padrão já usado para nativa/exótica/seca_morta:
   - `outra_forma_vida_sem_forma_vida_outros` (Encostam contém `outra_forma_vida` e `forma_vida_outros` vazio);
   - `serrapilheira_sem_forma_serrapilheira` (Encostam contém `serrapilheira` e `forma_serrapilheira` vazio).
3. **[ALTO]** Adicionar ao mesmo motor a checagem de exclusividade `solo_nu_com_outra_categoria` (hoje só existe em `monitora_validados_validar_condicionais_xlsform21`), para que uma violação de constraint do XLSForm bloqueie `registros_corrig.csv` e não apenas `registros_validados.csv`.
4. **[MÉDIO — trabalho estrutural, não um hotfix pontual]** Planejar a consolidação dos quatro mecanismos do item 15 em um único motor de validação row-level reutilizado pré-painel e pós-painel, conforme a premissa 12 do handoff e o item 6 do seu roteiro de desenvolvimento recomendado. Não fazer isso como parte dos hotfixes 1–3; tratar como item de arquitetura separado, com teste-golden próprio.
5. **[MÉDIO]** Antes de habilitar `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS` em produção sobre dataset com mistura de campanhas legadas e 2025+, implementar/roteirizar a tabela de migração semântica legado→canônico citada no handoff, para que `monitora_validados_validar_dominios_xlsform21` não sinalize como erro tokens legados ainda não migrados.
6. **[BAIXO]** Investigar o warning já registrado na baseline (`'unique' is not an exported object from namespace:data.table`) no gerador do relatório consolidado de validação, antes de reativar esse gerador como parte de qualquer hotfix futuro.

## Riscos de regressão dos hotfixes propostos

- Hotfix 1 é uma correção de valor-alvo em uma tabela de substituição estática; risco de regressão é mínimo, mas deve ser confirmado que nenhuma lógica downstream dependia (incorretamente) do valor `solo_nu` para essa label específica antes de aplicar.
- Hotfixes 2–3 adicionam novas linhas ao relatório de pendências impeditivas; risco principal é **aumentar** o número de coletas bloqueadas em `registros_corrig.csv` para dados reais que hoje passam silenciosamente (esperado e desejável, mas deve ser comunicado à equipe operacional, pois pode represar coletas que hoje geram checkpoint "sem pendências").
- Hotfix 4 é o de maior risco de regressão por escopo (toca 4 blocos de código diferentes); deve ser feito por último e isoladamente, com testes-golden dedicados antes/depois.

## Testes-golden necessários

1. **Golden PNB atual** (`diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md`) deve continuar batendo exatamente (606/6, 3/3, 10/8, 7/5, 176/180 colunas, ~52–60 s) após os hotfixes 1–3, já que o dataset atual não contém os cenários corrigidos.
2. **Novo teste-golden dedicado ao contrato 2025** (a criar): dataset sintético mínimo contendo:
   - 1 linha com Encostam = somente "Outras plantas terrestres, líquens e/ou fungos" → esperado: token final `outra_forma_vida`, não `solo_nu`;
   - 1 linha com Encostam = "Plantas nativas" + "Outras plantas terrestres, líquens e/ou fungos" combinadas → esperado: token final contém `nativa` e `outra_forma_vida`, sem `solo_nu`;
   - 1 linha com `outra_forma_vida` selecionado e `forma_vida_outros` vazio → esperado: nova pendência impeditiva `outra_forma_vida_sem_forma_vida_outros`;
   - 1 linha com `solo_nu` + outra categoria simultaneamente → esperado: nova pendência impeditiva `solo_nu_com_outra_categoria`.
3. Dataset `TESTE_2.6.2` já previsto no handoff permanece necessário para as regressões específicas da v2.6.2_teste (fora do escopo desta auditoria, que trata apenas da v2.6.0).

## Conclusão

**O contrato XLSForm 2025 está incorporado PARCIALMENTE.**

Os elementos estruturais do contrato (nomes de campo, tipos, domínios, template de 129 colunas, arquitetura de produtos `importados`→`corrig`→`validados`) estão corretamente reconhecidos e, na maior parte, corretamente implementados — inclusive em um motor bastante completo (`monitora_validados_*`) que já resolve corretamente a maioria das regras de relevância, obrigatoriedade e exclusividade do XLSForm 21FEV25.

Porém a auditoria encontrou uma **contradição crítica e ativa** (item 3: mapeamento de "Outras plantas..." para `solo_nu` em vez de `outra_forma_vida`, na golden baseline v2.6.0, atualmente dormente apenas porque o dataset PNB de teste não contém essa resposta) e um **gap estrutural real** (itens 2, 5, 6, 15: as regras de relevância/obrigatoriedade/exclusividade do contrato 2025 só protegem o produto opcional `registros_validados.csv`, não o produto operacional principal `registros_corrig.csv`, porque existem múltiplos motores de validação não unificados).

Nenhuma dessas descobertas invalida a baseline PNB já aprovada (nenhuma delas é exercitada pelo dataset atual), mas ambas devem ser tratadas **antes** de confiar em `registros_corrig.csv`/`registros_validados.csv` gerados a partir de dados reais de campanhas 2025/2026, que devem conter a resposta "Outras plantas terrestres, líquens e/ou fungos" com maior frequência.
