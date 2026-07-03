# Hotfix 03.5C-C — Resolvedor robusto de campo por contrato para classificação de pipe

## 1. Resumo executivo

O Hotfix 03.5C-B introduziu um fallback "por último segmento do path" para resolver o caso em que o path real de um campo no template diverge do path registrado no dump XLSForm embutido (`impact_manejo_uso/tipos_impacto_manejo_uso_outro` vs. `amostragem/registro/tipos_impacto_manejo_uso_outro`). Esse fallback funcionava, mas era uma heurística única e não auditável, sem tratamento de ambiguidade.

Investigando o contrato embutido nesta etapa, encontrei uma confirmação de que a preocupação do usuário era procedente e, na verdade, **mais séria do que o caso conhecido**: existem pelo menos dois campos (`forma_vida_nativa_cactacea`, `forma_vida_nativa_samambaia`) em que o **mesmo path canônico completo** é usado, em versões diferentes do XLSForm embutido, para representar perguntas genuinamente diferentes — ora uma pergunta de hábito (`select_one`), ora uma pergunta de espécie em texto livre (`text`). Ou seja, a ambiguidade não é só de nome curto; existe mesmo por path exato, o nível de correspondência mais específico disponível.

Implementei um resolvedor em 4 etapas priorizadas e explícitas (path exato → alias auditado → nome curto exato → label exato), cada uma com checagem de ambiguidade isolada: se uma etapa encontra tipos conflitantes (texto E estruturado) para o mesmo candidato, o resultado é `pipe_indeterminado` imediatamente — nenhuma etapa "força" uma decisão e nenhuma heurística de nome (o padrão regex antigo, mantido por compatibilidade) tem permissão de sobrepor uma ambiguidade já detectada pelo contrato.

## 2. Gap encontrado no 03.5C-B

1. O fallback por nome curto era uma heurística única, sem alternativa de alias explícito nem detecção de ambiguidade.
2. Ao auditar o contrato completo (416 linhas de `meta$campos`), encontrei que a classificação por `tipo_base` pode ser genuinamente ambígua mesmo por **path canônico completo**, não apenas por nome curto — um risco mais sério do que o hotfix anterior havia mapeado.
3. A lógica antiga, se aplicada ingenuamente a um campo ambíguo, teria um viés perigoso: como o código verificava `if (all(tipos == "text"))` e só depois `if (any(tipos %in% c("select_multiple","select_one")))`, um campo com tipos mistos (`"text"` e `"select_one"` simultaneamente) cairia no segundo `if` e seria classificado como estruturado — halterando/bloqueando um campo que, em pelo menos uma versão do formulário, é texto livre.

## 3. Campo(s) afetado(s)

| Campo | Divergência | Path(s) no dump | Tipo(s) encontrado(s) |
|---|---|---|---|
| `impact_manejo_uso/tipos_impacto_manejo_uso_outro` | Path real do template ≠ path no dump embutido | Real: `impact_manejo_uso/tipos_impacto_manejo_uso_outro`; dump: `amostragem/registro/tipos_impacto_manejo_uso_outro` | `text` (4/4 versões, sem conflito) — resolvido por **alias explícito auditado**, não mais por heurística genérica |
| `amostragem/registro/forma_vida_nativa_cactacea` | **Ambiguidade real por path completo** | Mesmo path usado para "O cacto nativo observado é:" (`select_one`) e "Espécie ou nome popular (Cactácea)" (`text`), em versões diferentes do XLSForm embutido | `select_one` e `text` — classificado como `pipe_indeterminado` |
| `amostragem/registro/forma_vida_nativa_samambaia` | Mesma natureza do caso acima | "Selecione se a samambaia observada é:" (`select_one`) vs. "Espécie ou nome popular (Samambaia)" (`text`) | `select_one` e `text` — classificado como `pipe_indeterminado` |

## 4. Estratégia de resolução implementada

Nova função `monitora_pipe_coluna_classificar_natureza`, com 4 estratégias em ordem de especificidade, cada uma passando pelo mesmo verificador de ambiguidade (`monitora_pipe_decidir_natureza_candidatos`):

1. **Path canônico completo** (`caminho_norm_publicacao_ae == alvo`).
2. **Alias explícito auditado** — nova tabela `monitora_pipe_aliases_campos_conhecidos()`, pequena e documentada (não uma lista genérica), contendo hoje apenas o caso comprovado: `impact_manejo_uso/tipos_impacto_manejo_uso_outro` → nome canônico `tipos_impacto_manejo_uso_outro`, com o motivo registrado em comentário no próprio código.
3. **Nome curto exato** (último segmento do path).
4. **Label exato**, quando presente no contrato.

Todas as comparações são `==` exatas após normalização (sem fuzzy matching, sem substring, sem regex amplo). Se uma etapa encontra candidatos com `tipo_base` conflitante (`"text"` e `select_one`/`select_multiple` ao mesmo tempo), a função retorna imediatamente `"pipe_ambiguo"` — não tenta outra estratégia "para desempatar".

Em `monitora_produtos_classificar_pipe_coluna`, a checagem de `"pipe_ambiguo"` foi posicionada **antes** do padrão de nome legado (`grepl("ponto|encostam|forma.*vida|...")`), justamente para impedir que esse regex, mantido por compatibilidade com Encostam/forma_vida_*, sobreponha uma ambiguidade já detectada pelo contrato — esse era exatamente o bug que um teste sintético revelou durante a implementação (ver seção 5).

Nenhuma coluna é classificada como texto livre por conter "outro" (ou qualquer substring) no nome — a decisão vem exclusivamente do `tipo_base` do contrato.

## 5. Testes sintéticos e resultados

Todos executados isoladamente em `/tmp`, sem dados reais, com o dump completo do XLSForm embutido extraído verbatim do script:

| # | Cenário | Resultado |
|---|---|---|
| D.1 | Campo com path exato `text` (`observacoes_gerais`) | ✅ Preserva `\|` |
| D.2 | Campo com nome curto `text` e path divergente conhecido (`impact_manejo_uso/tipos_impacto_manejo_uso_outro`, via alias) | ✅ Preserva `\|` |
| D.3 | `select_multiple` com `\|` resolvível | ✅ Resolve |
| D.4 | Campo estruturado com `\|` irresolúvel | ✅ Bloqueia |
| D.5 | Campo totalmente desconhecido com `\|` | ✅ Reporta como indeterminado, não altera, não bloqueia |
| D.6 | **Ambiguidade real** (`forma_vida_nativa_cactacea`, path exato com tipos conflitantes) | ✅ Reportado como indeterminado; **confirmado que não foi alterado pelo resolvedor nem bloqueado** — a classificação não foi forçada nem para texto livre nem para estruturado |
| — | Controle: produto bruto nunca bloqueia | ✅ |
| — | Regressão: `monitora_pendencia_impeditiva_tipo` com `\|` interno legítimo | ✅ Preservado, sem bloqueio |

**Achado durante a implementação**: a primeira versão desta correção usava `"pipe_indeterminado"` tanto para "não encontrado" quanto para "ambíguo", e o teste D.6 revelou que, nesse caso, o `grepl("forma.*vida", ...)` do padrão de nome legado ainda classificava `forma_vida_nativa_cactacea` como `pipe_residual_operacional` (estruturado), ignorando a ambiguidade! Corrigido introduzindo um valor de retorno distinto (`"pipe_ambiguo"`) verificado **antes** do regex legado. Documentado aqui por transparência.

## 6. Baseline PNB e resultados

Mesma pasta de execução limpa, mesmo `input/pnb_2022_2026.zip` (hash confirmado idêntico):

| Métrica | Antes (Hotfix 03.5C-B) | Depois (Hotfix 03.5C-C) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606/6 | 606/6 | ✅ |
| Nativa sem forma de vida | 3/3 | 3/3 | ✅ |
| Seca/morta sem forma de vida | 10/8 | 10/8 | ✅ |
| Outra forma de vida legada | 7/5 | 7/5 | ✅ |
| Colunas importados/corrig | 176/180 | 176/180 | ✅ |
| Tamanho dos 3 produtos de dados | byte-idênticos | byte-idênticos | ✅ |
| Tempo núcleo | ~53 s | 55,5 s | ✅ equivalente |
| Bloqueios/erros | Nenhum | Nenhum | ✅ |
| Relatórios `pipe_indeterminado`/`bloqueio_pipe_residual` | Não gerados | Não gerados (dataset PNB não usa Encostam com path canônico nem os campos ambíguos identificados) | ✅ esperado |

## 7. Regressão real local e resultados

Executados fora do Git, nas mesmas 4 pastas de execução isoladas já usadas nos Hotfixes 03.5B/03.5C-B:

| Dataset | Exit | Bloqueios | `pipe_indeterminado` | Colunas `registros_corrig.csv` |
|---|---|---|---|---|
| 01_2025_max_outra_forma_vida | 0 | Nenhum | Não | 174 |
| 03_2025_serrapilheira_volume | 0 | Nenhum | Sim — mesma coluna já conhecida (`"Outros tipos de manejo ou uso: (impact_manejo_uso)"`, label com sufixo de grupo divergente, fora do escopo desta correção) | 174 |
| 04_2025_fncs_anexado | 0 | Nenhum | Não | 174 |
| 05_2025_pnm_anexado | 0 | Nenhum | Não | 174 |

Todos os 4 datasets passaram sem bloqueio. No dataset 01, confirmei novamente que `observacoes_gerais` manteve seus 505 `|` intactos.

## 8. Evidência de que pipes legítimos em texto livre continuam preservados

Teste D.1 (path exato) e D.2 (alias explícito) confirmam preservação exata do valor original, sem nenhuma alteração de caractere. Regressão real do dataset 01 confirma a mesma contagem de `|` em `registros_importados.csv` antes e depois da correção (505 ocorrências).

## 9. Evidência de que pipes estruturados indevidos continuam bloqueando

Teste D.4 confirma bloqueio via `stop()` para `amostragem/registro/tipo_forma_vida` com descompasso de índice, com a mesma mensagem/relatório já validados no Hotfix 03.5C-B. Nenhuma regressão nesse comportamento.

## 10. Arquivos alterados

- `monitora_campsav_alvo_global_v2.6.0.R` — único arquivo de código alterado. Diff: +94/-16 linhas.
  - Nova função `monitora_pipe_aliases_campos_conhecidos()` (tabela de aliases explícitos e auditáveis).
  - Nova função `monitora_pipe_decidir_natureza_candidatos()` (detecção de ambiguidade isolada, reaproveitada pelas 4 etapas).
  - `monitora_pipe_coluna_classificar_natureza()` reescrita: 4 estratégias priorizadas, cada uma checando ambiguidade antes de decidir.
  - `monitora_produtos_classificar_pipe_coluna()` ajustada: nova checagem de `"pipe_ambiguo"` com prioridade sobre o regex de nome legado.
- `diagnostics/hotfix_035c_c_resolvedor_contrato_campos/HOTFIX_035C_C_RESOLVEDOR_CONTRATO_CAMPOS.md` — este relatório.
- Scripts de teste sintético em `/tmp/.../scratchpad/` — não versionados, sem dados reais.

Nenhuma alteração na regra de bloqueio/preservação definida no Hotfix 03.5C-B (`monitora_bloquear_pipe_residual_produto` não foi tocada nesta etapa); apenas a resolução de **qual categoria** uma coluna recebe foi tornada mais robusta.

## 11. Fora do escopo

- Regra de bloqueio/preservação de pipe em si (Hotfix 03.5C-B) — inalterada.
- Proteção de texto livre — mantida, não removida.
- Outra forma de vida legada.
- Bug de vírgula interna na tokenização.
- Escopo do painel 101/84.
- Centralização completa de todos os contratos do script.
- Redesenho da cadeia de importação.
- Correção do caso do dataset 03 (`"Outros tipos de manejo ou uso: (impact_manejo_uso)"`, label com sufixo de grupo) — continua classificado como `pipe_indeterminado` (seguro: não bloqueia, não altera, apenas reporta). Não avaliado como comprovado o suficiente para virar um novo alias explícito nesta etapa; candidato a hotfix futuro pontual se o usuário considerar relevante.

## 12. Próximo passo recomendado

Nenhum hotfix adicional urgente. Se o usuário quiser eliminar também o `pipe_indeterminado` residual do dataset 03 (label com sufixo de grupo), o próximo passo seria confirmar por inspeção adicional se esse padrão de sufixo (`": (nome_do_grupo)"`) é sistemático o suficiente para virar uma quinta estratégia de normalização de label (removendo o sufixo antes de comparar) — hoje deliberadamente fora de escopo por já ser seguro (relata, não corrompe, não bloqueia).

## Segurança Git

```
$ git status --short
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035c_c_resolvedor_contrato_campos/

$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | 110 +++++++++++++++++++++++++++++-----
 1 file changed, 94 insertions(+), 16 deletions(-)

$ git diff --name-only
monitora_campsav_alvo_global_v2.6.0.R
```

Nenhum CSV/ZIP/XLSX/RDS/DB real staged ou criado dentro do repositório. Staging previsto apenas para:
- `monitora_campsav_alvo_global_v2.6.0.R`
- `diagnostics/hotfix_035c_c_resolvedor_contrato_campos/HOTFIX_035C_C_RESOLVEDOR_CONTRATO_CAMPOS.md`

Commit ainda não realizado — aguardando as checagens de segurança finais abaixo.
