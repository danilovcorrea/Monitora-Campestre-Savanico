# Dev 03.5M-D0 — Caso positivo controlado para pipes condicionais/esparsos

## 1. Resumo executivo

Criado um caso positivo **100% sintético e reproduzível**, fiel ao padrão real já documentado no próprio histórico deste repositório (COLETA 11168, Hotfix 03.5G2), e usado para testar quantitativamente o protótipo de resolução de pipe por contrato único (03.5M-A) contra o comportamento do resolvedor posicional absoluto legado. Resultado: **resolução por contrato acerta 101/101 pontos; a simulação do resolvedor posicional absoluto acerta só 97/101**, errando exatamente da mesma forma documentada no Hotfix 03.5G2 (coloca valor nos pontos 1/2 em vez de deixá-los vazios, e deixa os pontos 90/91 — os realmente elegíveis — sem resolução). Isso é evidência quantitativa, controlada e reproduzível de que a resolução por elegibilidade/relevance resolve exatamente o problema que motivou o Hotfix 03.5G/03.5G2, sem tocar em nenhum mecanismo operacional.

## 2. Status da busca por FNCS/COLETA 11168

Busca conservadora, só leitura, sem alterar nada:

- **Nomes de arquivo/diretório**: `find` em `/home/dfed/Desktop` e `/home/dfed/Projetos` por `*fncs*`/`*11168*` — encontrados vários diretórios/ZIPs de runs anteriores com dados FNCS (`/home/dfed/Desktop/02jul/FNCS/`, `/home/dfed/Desktop/03jul_dev/*/input/FNCS_23_24_25_26.zip`, extrações em `extracted/exec_*`), mas **nenhum arquivo com "11168" no nome** (esperado — é um valor de COLETA dentro do CSV, não um nome de arquivo).
- **Conteúdo de logs/texto** (`console_*.txt`, `.md`, `.log`, sem tocar CSV): `grep -rl "11168\|bromel"` em todos os `console_*.txt` de `/home/dfed/Desktop/03jul_dev/*` e `/home/dfed/Desktop/*` — **nenhuma ocorrência**. Os consoles das runs recentes (35g2, 35lc, 35mc, 35mc2 etc.) não mencionam essa COLETA especificamente porque nenhuma delas reproduziu o cenário (o alias que causava a resolução da bromélia já foi revertido desde o 03.5G2 — sem o alias, a coluna nunca chega a ser processada pelo resolvedor posicional, então nenhum log recente teria motivo para citar essa COLETA).
- **Evidência textual encontrada**: `diagnostics/hotfix_035g2_restringe_aliases_pipe/HOTFIX_035G2_RESTRINGE_ALIASES_PIPE.md`, já presente neste mesmo repositório (seção 2), documenta com precisão o caso real original: *"COLETA 11168 tem bromelioide apenas nos pontos 90 e 91, com valor bruto 'terrestre|terrestre'; o resolvedor tenta aplicar os 2 tokens aos pontos 1 e 2 (...), deixando os demais 99 pontos sem resolução e gerando resíduo estruturado bloqueante."*

**Decisão**: não abrir nenhum CSV/ZIP real para procurar ou confirmar a COLETA 11168 diretamente — o relatório já existente no repositório já documenta o padrão estrutural com precisão suficiente (101 pontos, elegibilidade em 90/91, valor bruto exato) para construir um fixture fiel, e evita qualquer risco de leitura/transcrição de dado real (mesmo que só para "confirmar existência"). Nenhum ZIP/CSV de `dados_originais`/`Desktop` foi aberto nesta tarefa.

## 3. Caso real vs. fixture sintético

**Não foi usado nenhum dado real.** Criado fixture sintético, função nova isolada:

```r
monitora_pipe_contrato_fixture_condicional_esparso_035m_d0(
  n_pontos = 101L, pontos_elegiveis = c(90L, 91L), tokens = c("terrestre", "terrestre")
)
```

Gera, em memória, sem ler nenhum arquivo:
- Um vetor `forma_vida_nativa` de 101 posições, com `"bromelioide"` só nos pontos 90/91 e `"graminoide"` no resto (reproduz o parent da relevance `selected(${forma_vida_nativa}, 'bromelioide')`).
- O valor bruto de pipe `"terrestre|terrestre"` (exatamente o valor documentado no caso real).
- Um vetor `esperado_por_ponto` (verdade-terreno): `"terrestre"` nos pontos 90/91, `NA` nos outros 99.

Função puramente geradora de dado artificial — não lê arquivo, não é chamada por nenhum consumidor operacional, não altera nenhum objeto global.

## 4. Matriz de cenários testados

| # | Cenário | Esperado | Obtido |
|---|---|---|---|
| 1 | Texto livre com pipe (liana) | Preservado intacto | `preservado_texto_livre`, valor idêntico ao original |
| 2 | `select_multiple` com pipe (Encostam/`tipo_forma_vida`) | Tratado como conjunto de tokens | `conjunto_tokens` |
| 3 | `estruturado_completo_por_ponto` (`form_veg`) | Resolvido por posição absoluta | `resolvido_por_ponto_absoluto`, valor correto no ponto pedido |
| 4 | **Fixture COLETA 11168** — bromélia condicional/esparsa, 101 pontos, elegível só em 90/91, valor `"terrestre\|terrestre"`, resolvido **por contrato** (elegibilidade/relevance) para todos os 101 pontos | 101/101 pontos corretos | **101/101 corretos** — pontos 90/91 = `"terrestre"`, demais 99 pontos = `NA` |
| 4 (comparativo) | Mesmo fixture, simulando o resolvedor **posicional absoluto** (token *i* → ponto *i*, mesma lógica de `monitora_produtos_resolver_pipes_por_ponto`, calculado à parte, sem tocar a função real) | — | **97/101 corretos** — pontos 1/2 recebem `"terrestre"` (errado, deveriam ser `NA`); pontos 90/91 ficam `NA` (errado, deveriam ser `"terrestre"`) — reproduz exatamente o defeito documentado no Hotfix 03.5G2 |
| 5 | Bromélia sem contexto de relevance | Não resolvida por ponto absoluto | `depende_de_parent_relevance_contexto_insuficiente` |
| 6 | Ambíguo/fora do contrato (`uuid`, `protocolo`) | Diagnosticado, não forçado | `diagnosticar_fora_do_contrato` para ambos |
| 7 | Token insuficiente (2 tokens, 3 pontos elegíveis) | Pendência/diagnóstico seguro, não resolve às cegas | `contagem_elegiveis_diverge_tokens_nao_resolvido` |
| — | Dataset sintético agregado (5 colunas, incl. fixture) via `monitora_pipe_contrato_diagnosticar_dataset`/`resumir` | Classificação e risco corretos por coluna | Confirmado — `alto_exige_relevance`=1 coluna (bromélia), `medio_select_multiple_...`=1, `baixo_ou_sem_pipe`=3 |

## 5. Resultado do caso condicional/esparso por relevance

**101/101 pontos resolvidos corretamente** pela abordagem de elegibilidade/relevance do protótipo 03.5M-A, contra **97/101** (91.1%) da simulação do resolvedor posicional absoluto no mesmo fixture — os 4 erros do posicional são exatamente os 4 pontos que causaram o resíduo bloqueante real (2 pontos com valor indevido + 2 pontos elegíveis sem resolução). Este é o primeiro teste desta linhagem 03.5M a demonstrar, com números, o ganho concreto da abordagem por contrato sobre o mecanismo atual — sem tocar o mecanismo atual.

## 6. Confirmação: nenhuma saída contém valor real

- Todos os valores usados (`"Cipó A"`, `"terrestre"`, `"graminoide"`, `"bromelioide"`, `"sintetico-1"` etc.) são literais de teste ou gerados pela função-fixture — nenhum arquivo real foi lido em nenhum momento desta tarefa.
- Nenhum CSV foi criado nesta tarefa (só o relatório `.md` — não foi necessário gravar CSV separado, já que os testes rodaram inteiramente em memória e os resultados agregados estão neste relatório).
- Nenhuma COLETA/UC/EA/UA/UUID/coordenada/espécie real aparece em nada do que foi produzido.

## 7. Conclusão sobre base para 03.5M-D

Há agora evidência **sintética, controlada e quantitativa** de que a abordagem por contrato resolve corretamente o padrão documentado da COLETA 11168 — mas ainda **não há evidência empírica contra dado real** (a 03.5M-B já havia confirmado que o único dataset real disponível, `registros_corrig.csv` da run 35lc, tem zero pipe residual). O fixture sintético é fiel ao padrão documentado, mas uma migração operacional real ainda se beneficiaria de validação contra uma run FNCS real que reproduza a condição (ou pelo menos confirme que ela não existe mais nos dados atuais, o que também seria informação útil).

## 8. Recomendação objetiva

**`exigir_run_FNCS_real`** — antes de planejar 03.5M-D (migração operacional parcial), é necessário confirmar, com uma run FNCS real (dado de produção, não hipotético), se o padrão condicional/esparso da COLETA 11168 (ou equivalente) ainda existe nos dados atuais e, se existir, se a resolução por contrato produz o resultado esperado sobre o dado real — não só sobre o fixture sintético. Essa run não foi executada nesta tarefa (fora de escopo/autorização). Até lá, a recomendação seguinte mais segura é: manter o relatório opt-in (03.5M-C/C2) ativo em execuções já planejadas, e usar este fixture sintético como suíte de regressão de baixo custo para qualquer evolução futura do protótipo 03.5M-A, sem precisar de dado real para isso.

## 9. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 36 ++++++++++++++++++++++++++++++++++
 1 file changed, 36 insertions(+)
```

36 inserções, **0 remoções** — confirmado puramente aditivo (só a função-fixture nova). `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna`, `monitora_pipe_contrato_diagnosticar_dataset`, `monitora_pipe_contrato_relatorio_optin` e `monitora_pipe_contrato_relatorio_optin_seguro` não aparecem no diff (exceto como contexto de hunk não modificado, no ponto de inserção).

## 10. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035m_d0_caso_positivo_pipes_condicionais/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
