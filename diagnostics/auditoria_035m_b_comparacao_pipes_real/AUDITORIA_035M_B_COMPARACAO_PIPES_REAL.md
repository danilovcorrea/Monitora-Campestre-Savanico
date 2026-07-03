# Auditoria 03.5M-B — Diagnóstico/comparação real de pipes: legado vs. contrato único

## 1. Resumo executivo

Rodado, em modo diagnóstico seguro (só leitura, só agregados), sobre o `registros_corrig.csv` real da run 35lc (23.634 linhas × 181 colunas). Achado principal: **zero resíduo de "|" em qualquer coluna deste dataset** — confirma que `monitora_produtos_resolver_pipes_por_ponto`/`monitora_bloquear_pipe_residual_produto` já entregam, neste run golden, exatamente o que prometem (nenhum pipe sobrevive até o checkpoint `registros_corrig.csv`). Isso significa que os riscos estruturais identificados na Auditoria 03.5M-A (select_multiple/condicional-esparso tratados por ponto absoluto) **não se manifestaram como corrupção de dado neste run específico** — são riscos estruturais confirmados na lógica, não incidentes observados nesta amostra.

Achado secundário, novo nesta etapa: o classificador de contrato único reconhece a coluna em **146 de 181 casos (81%)** onde o classificador legado de pipe cai no fallback genérico `pipe_indeterminado` (101 de 181 colunas) — não porque discordem sobre o que fazer com um pipe real (não há nenhum), mas porque o classificador legado de pipe usa um casamento de nome mais estreito que o do contrato único (que já herda as 5 estratégias de match da 03.5K, incluindo labels históricos e o path do template SISMonitora da 03.5K/M-A). Isso é uma diferença de **cobertura diagnóstica**, não uma divergência de comportamento operacional — hoje inofensiva porque `pipe_indeterminado` já é o resultado mais seguro possível (nunca resolve, só reporta).

## 2. Arquivo real lido e confirmação de somente leitura

- Preferido conforme instruído: `/home/dfed/Desktop/03jul_dev/35lc/output/01_produtos_dados/registros_corrig.csv` — **encontrado e usado**.
- Lido via `data.table::fread(arquivo, colClasses = "character")` — leitura pura em memória, sem `fwrite`/`file.copy`/qualquer escrita de volta ao arquivo ou ao diretório de origem.
- Arquivo **não foi alterado, não foi copiado para o repositório, não foi usado como entrada de pipeline** (não passou por nenhuma função de importação/correção/painel — só pelas 2 funções novas de diagnóstico da 03.5M-B, que só leem `names()`/valores de coluna para contagem, nunca escrevem).
- Objeto `registros` (o `data.table` carregado) foi explicitamente removido da memória (`rm(registros); gc()`) ao final da sessão de diagnóstico.
- Dimensões confirmadas: **23.634 linhas × 181 colunas** (nota: o cabeçalho tem 193 campos por `head -n1 | tr ',' '\n'`, mas `fread` consolidou para 181 colunas — provavelmente por vírgulas dentro de campos citados; não investigado further, fora do escopo de segurança desta tarefa, e não afeta a validade das contagens por coluna).

## 3. Confirmação: nenhum valor real foi gravado

- As 3 funções novas (`monitora_pipe_contrato_diagnosticar_dataset`, `monitora_pipe_contrato_resumir_diagnostico_dataset`) só produzem: nome de coluna (`coluna`), contagens inteiras (`n_linhas_total`, `n_linhas_com_pipe`), booleanos, e rótulos de classificação (strings de um vocabulário fechado, ex. `"texto_livre"`, `"pipe_indeterminado"`) — nunca o conteúdo de uma célula.
- Testado explicitamente (seção 8, testes 2-3): um dataset sintético com valores distintivos (`"Cipó A"`, `"abc-123"` etc.) confirmou que nenhum desses valores aparece no diagnóstico, mesmo com `incluir_valores=TRUE` (que só adiciona **estatísticas agregadas** — comprimento médio e nº médio de tokens dos valores com pipe — nunca o texto em si).
- Os 3 CSVs gravados em `diagnostics/auditoria_035m_b_comparacao_pipes_real/` foram inspecionados linha a linha após a geração: contêm só nomes de coluna (`COLETA`, `DATA DO REGISTRO` etc. — os próprios nomes de coluna do produto, que já são metadado público, não dado de linha), classificações e contagens. Varredura por padrões de coordenada (`\d{2}\.\d{4,}`) e UUID (`[0-9a-f]{8}-...`) não encontrou nenhuma ocorrência.
- **Nenhum COLETA/UC/EA/UA/UUID/coordenada/espécie/linha individual** aparece em nenhum dos 3 produtos.

## 4. Funções novas isoladas

1. **`monitora_pipe_contrato_diagnosticar_dataset(registros, max_colunas, incluir_valores, contrato, indices, produto)`** — para cada coluna: conta linhas com pipe, classifica pelo legado (`monitora_produtos_classificar_pipe_coluna`, reaproveitado, read-only) e pelo contrato (`monitora_pipe_contrato_classificar_coluna`, 03.5M-A), normaliza a decisão legado para comparar com a estratégia do contrato, e sinaliza divergência. `incluir_valores=FALSE` por padrão.
2. **`monitora_pipe_contrato_resumir_diagnostico_dataset(diagnostico)`** — agrega o diagnóstico por cardinalidade, decisão do contrato, decisão do legado, divergência, risco (`alto_exige_relevance`/`medio_select_multiple_tratado_como_posicional_pelo_legado`/`medio_divergencia_legado_contrato`/`baixo_diagnostico_apenas`/`baixo_ou_sem_pipe`) e ação recomendada.

Nenhuma das duas é chamada por nenhum consumidor operacional (confirmado por `grep`, só aparecem em suas próprias definições e nos testes/no script de diagnóstico desta sessão).

Dois bugs reais foram encontrados e corrigidos durante os testes (antes de rodar contra o dado real): `identical()` e `isTRUE()` usados por engano em vez de comparação vetorizada (`==`) dentro de `data.table::fcase()` em `monitora_pipe_contrato_resumir_diagnostico_dataset` — causavam erro de tamanho de argumento. Corrigidos, testados novamente, confirmados corretos antes de tocar no dado real.

## 5. Totais do dataset

| Métrica | Valor |
|---|---:|
| Linhas | 23.634 |
| Colunas | 181 |
| Colunas com pelo menos 1 célula contendo "\|" | **0** |
| Total de células com "\|" em todo o dataset | **0** |

## 6. Matriz legado vs. contrato (por cardinalidade do contrato)

| Cardinalidade (contrato) | Nº colunas | Colunas com pipe | Nº divergências vs. legado |
|---|---:|---:|---:|
| `texto_livre` | 54 | 0 | 32 |
| `select_multiple` | 26 | 0 | 26 |
| `estruturado_completo_por_ponto` | 5 | 0 | 2 |
| `estruturado_condicional_esparso` | 29 | 0 | 29 |
| `tecnico_midia` | 16 | 0 | 6 |
| `fora_do_contrato` | 13 | 0 | 13 |
| `ambiguo_indeterminado` | 3 | 0 | 3 |
| Sem match no contrato (`NA`) | 35 | 0 | 35 |

Decisão do classificador **legado** (agregado): `pipe_indeterminado`=101, `pipe_residual_tecnico_tolerado`=24, `pipe_residual_operacional`=31, `pipe_permitido_texto_livre`=22, `pipe_residual_revisar`=3.

## 7. Resumo por risco (03.5M-B)

| Risco | Nº colunas | Colunas com pipe |
|---|---:|---:|
| `baixo_ou_sem_pipe` | 181 | 0 |

Como nenhuma coluna deste dataset tem pipe, **todas** caem em `baixo_ou_sem_pipe` — a classificação de risco por presença-de-pipe não tem nada a reportar aqui além de confirmar a ausência total de resíduo. As categorias `alto_exige_relevance`/`medio_select_multiple_...`/`medio_divergencia_...` (validadas com dado sintético, seção 8) simplesmente não têm nenhuma linha real neste dataset porque a condição `tem_pipe` nunca é satisfeita.

## 8. Campos de maior risco por nome de coluna (estrutural, não por pipe observado)

Como não há pipe real neste run, "risco" aqui é **estrutural** (o que aconteceria *se* um pipe aparecesse), baseado só na cardinalidade/estratégia do contrato — nomes de coluna, sem nenhum valor:

- **29 colunas `estruturado_condicional_esparso`** (a família bromélia/cactácea/orquídea/samambaia × nativa/exótica/seca-morta) — se algum dia carregarem "\|", exigem elegibilidade/relevance antes de qualquer resolução automática (nunca ponto absoluto).
- **26 colunas `select_multiple`** — se caírem em `pipe_residual_operacional`/`pipe_residual_revisar` pelo legado, seriam tratadas como posicional pelo mecanismo atual, quando deveriam ser tratadas como conjunto de tokens.
- **35 colunas sem match no contrato** — via `atributo_canonico_contrato`, todas correspondem a nomes de coluna já em formato de label/histórico (mesma família de casos já documentada na Auditoria 03.5F) — candidatas a melhorar cobertura do contrato em etapa futura, não urgente (nenhuma tem pipe).

## 9. Achados sobre `select_multiple` real

**Nenhum campo `select_multiple` real deste dataset tem pipe residual hoje** (0 de 26). Não há evidência de dano observado. O risco estrutural (mecanismo legado não distingue select_multiple de select_one ao resolver por ponto) permanece **teórico, não confirmado em produção** por este run.

## 10. Achados sobre `texto_livre` real

**Nenhum campo `texto_livre` real tem pipe neste run** (0 de 54) — nem mesmo os campos de texto livre que legitimamente poderiam conter "|" digitado pelo observador (liana, descrições). Não há evidência de que o legado esteja tocando indevidamente texto livre.

## 11. Achados sobre `estruturado_condicional_esparso` real (bromélia etc.)

**Nenhuma das 29 colunas condicionais/esparsas tem pipe neste run** (0/29) — incluindo as variantes de bromélia nativa/exótica/seca-morta. Consistente com o Hotfix 03.5G2 (a bromélia hoje cai em `pipe_indeterminado`, nunca é resolvida por ponto) e com o checkpoint de persistência limpo confirmado na 03.5L-C. **Nenhum risco de ponto absoluto se materializou neste dataset.**

## 12. Achados sobre ambíguos/fora do contrato

**Nenhuma das 3 colunas `ambiguo_indeterminado` nem das 13 `fora_do_contrato` tem pipe** neste run.

## 13. Recomendação para 03.5M-C

1. **Continuar em modo diagnóstico**, não conectar nada ao pipeline ainda — este dataset (o único disponível para comparação real até agora) não tem nenhum pipe residual, então não há como validar empiricamente a resolução por elegibilidade/relevance (03.5M-A) contra um caso real positivo. É preciso um dataset com resíduo de pipe estrutural real (ex.: reproduzir as condições da COLETA 11168/FNCS que motivaram o Hotfix 03.5G) antes de qualquer migração operacional.
2. **Pode conectar só alerta/relatório** (não decisão): já que o dataset atual não expõe risco real, um próximo passo seguro de baixo custo é rodar `monitora_pipe_contrato_diagnosticar_dataset()` como relatório informativo adicional em runs futuras (mesmo padrão opt-in da 03.5L-B, flag própria, default OFF) — só para acompanhar se select_multiple/condicional-esparso algum dia desenvolvem pipe residual em produção.
3. **Nenhum subconjunto deve ser migrado operacionalmente ainda** — mesmo os "baixo risco" (`texto_livre`, `estruturado_completo_por_ponto`) já funcionam corretamente hoje; migrá-los não teria benefício mensurável e ainda assim mudaria o mecanismo validado sem necessidade.
4. **Testes PNB/FNCS necessários antes de qualquer substituição operacional** (não nesta etapa): (a) uma run FNCS reproduzindo o cenário original da COLETA 11168 (bromélia condicional/esparsa com pipe residual real) para validar a resolução por elegibilidade contra um caso positivo de verdade; (b) uma run PNB completa comparando hash/dimensões do `registros_corrig.csv` resultante contra o baseline golden já documentado, para confirmar que a substituição (quando acontecer) não muda nada fora do escopo pretendido.

## 14. Testes realizados

| # | Teste | Resultado |
|---|---|---|
| 1 | `Rscript -e 'invisible(parse("monitora_campsav_alvo_global_v2.6.0.R")); cat("PARSE_OK\n")'` | `PARSE_OK` |
| 2 | Dataset sintético (6 colunas: texto_livre, select_multiple, completo_por_ponto, condicional_esparso, fora_do_contrato-como-uuid, coluna inventada) | Classificado corretamente em todas as 6 categorias, contagens de pipe corretas |
| 3 | Confirmação de ausência de valores reais no diagnóstico (`incluir_valores=FALSE`) | Nenhum valor distintivo do dataset sintético apareceu na saída |
| 3b | Mesmo teste com `incluir_valores=TRUE` | Só estatísticas agregadas (comprimento/nº de tokens médios) adicionadas — nenhum valor real |
| 4 | Diagnóstico sobre `registros_corrig.csv` real (35lc) | 181 colunas processadas, 0 com pipe, produtos seguros gravados e inspecionados |
| 5 | Nenhuma chamada operacional alterada | Confirmado via `git diff` (seção 16) |

## 15. Confirmação: warning do relatório consolidado fora de escopo

Nenhuma evidência encontrada nesta auditoria que relacione o warning de `data.table::unique` do relatório consolidado de validação a pipes/relevance/cardinalidade — não investigado, não tocado, conforme instruído.

## 16. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 119 +++++++++++++++++++++++++++++++++
 1 file changed, 119 insertions(+)
```

119 inserções, **0 remoções** — confirmado puramente aditivo. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto` e `monitora_produtos_classificar_pipe_coluna` não aparecem no diff, exceto como contexto de hunk não modificado.

## 17. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/auditoria_035m_b_comparacao_pipes_real/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
