# Hotfix 03.5C-B — Bloqueio de resíduo de pipe em campos estruturados, com proteção de texto livre

## 1. Resumo executivo

Este hotfix teve duas fases. Na primeira, implementei exatamente o que foi pedido originalmente: uma guarda pós-normalização que bloqueia a exportação de `registros_importados.csv`/`registros_corrig.csv` quando sobra resíduo de `|` após `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)`. Os testes sintéticos passaram e a baseline PNB ficou intacta — mas a regressão real revelou um problema sério: **2 dos 4 datasets 2025 reais bloquearam por resíduo de `|` em campos de texto livre** (`observacoes_gerais`, `tipos_impacto_manejo_uso_outro`), onde o `|` era pontuação legítima do observador de campo (ex.: `"Declividade leve | rochas amontoadas de forma não natural"`), não um artefato de achatamento de múltiplos pontos.

O usuário então definiu a regra correta: campos de texto livre/descritivo nunca devem ser submetidos à resolução posicional nem ao bloqueio por `|`. Implementei essa distinção **consultando o contrato XLSForm já embutido no script** (campo `tipo_base`), não por heurística de nome — evitando exatamente a "varredura indiscriminada de campos textuais" que o usuário pediu para não fazer. Após essa correção, os mesmos 4 datasets reais passaram sem nenhum bloqueio, com o `|` de texto livre devidamente preservado.

## 2. Decisão do usuário

Fase 1 (autorização original): bloquear a exportação de `registros_importados.csv`/`registros_corrig.csv` quando houver resíduo de `|` não resolvido, mantendo `corrigir=TRUE` em ambas as chamadas.

Fase 2 (ajuste de escopo, após regressão real revelar falso-positivo sistêmico): campos de texto livre/descritivo nunca devem ser tratados como resíduo de achatamento de múltiplos pontos, independentemente do nome da coluna — a identificação deve vir do contrato XLSForm incorporado (`tipo_base`), não de uma lista de nomes.

## 3. Explicação da especificação final

| Produto | Pode ter `\|` em campo estruturado? | Pode ter `\|` em campo de texto livre? |
|---|---|---|
| `registros_importados_bruto.csv` | Sim (nunca é tocado por esta guarda) | Sim |
| `registros_importados.csv` | **Não** — bloqueia se sobrar | **Sim** — preservado, nunca alterado nem bloqueado |
| `registros_corrig.csv` | **Não** — bloqueia se sobrar | **Sim** — preservado, nunca alterado nem bloqueado |

Três categorias de classificação (além das já existentes `pipe_esperado_em_bruto` e `pipe_residual_tecnico_tolerado`, para foto/colunas técnicas, inalteradas):

- **`pipe_permitido_texto_livre`**: coluna com `tipo_base == "text"` confirmado no contrato XLSForm embutido. Nunca alterada pelo resolvedor, nunca bloqueada, `|` preservado como digitado.
- **`pipe_residual_operacional` / `pipe_residual_revisar`** (== estruturado elegível): coluna confirmada `select_multiple`/`select_one` no contrato, ou reconhecida pelo padrão de nome já existente (Encostam, forma_vida_*, espécie, hábito, ponto, uuid). Resíduo de `|` aqui é falha real de resolução posicional — bloqueia.
- **`pipe_indeterminado`** (nova): coluna não encontrada no contrato XLSForm embutido (nem por path, nem por nome curto, nem por label). Não é alterada pelo resolvedor (para não arriscar corromper um possível texto livre não mapeado), e o resíduo de `|`, se houver, é **reportado à parte, sem bloquear** — bloquear indiscriminadamente colunas desconhecidas repetiria o mesmo erro que motivou este ajuste.

## 4. Função/guarda implementada

### `monitora_pipe_coluna_classificar_natureza(coluna)` (nova)

Consulta `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` — a mesma função, já existente e memoizada, que o restante do script usa para ler o contrato XLSForm embutido (múltiplas versões, sem depender de arquivo externo). Localiza a coluna por:
1. Path canônico completo (`caminho_registro`, normalizado);
2. Nome curto (`name`, normalizado);
3. Label (`label`, normalizado);
4. **Fallback pelo último segmento do path** (ex.: `impact_manejo_uso/tipos_impacto_manejo_uso_outro` → `tipos_impacto_manejo_uso_outro`) — necessário porque o grupo pai registrado no dump embutido pode divergir do path real usado no template/export para o mesmo campo folha (confirmado empiricamente com `tipos_impacto_manejo_uso_outro`, ver seção 6).

Retorna `pipe_permitido_texto_livre` se todo `tipo_base` encontrado for `"text"`; `pipe_residual_estruturado_elegivel` se houver `select_multiple`/`select_one`; `pipe_indeterminado` caso contrário (não encontrado, ou tipo não reconhecido).

### `monitora_produtos_classificar_pipe_coluna` (modificada)

Ordem de checagem: bruto → foto/técnico (inalterado) → **natureza via contrato** (nova, intercepta texto livre antes de qualquer regex) → padrão de nome já existente (Encostam/forma_vida/espécie/hábito/ponto/uuid/observação, preservado por compatibilidade) → estruturado confirmado via contrato → `pipe_indeterminado` (novo default, substituindo o antigo `pipe_residual_revisar` incondicional).

### `monitora_bloquear_pipe_residual_produto` (modificada)

Separa achados em dois relatórios: `bloqueio_pipe_residual_<produto>.csv` (estruturado, bloqueia com `stop()`) e `pipe_indeterminado_<produto>.csv` (reporta, não bloqueia). Nunca considera `pipe_permitido_texto_livre`/`pipe_residual_tecnico_tolerado`/`pipe_esperado_em_bruto`.

## 5. Pontos de chamada

Inalterados desde a implementação original: logo após `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)` e antes da escrita final, em `monitora_registros_importados_saneado_exportar` (2ª definição, efetiva) e em `monitora_publicacao_aa_exportar_registros_corrig_aprovado` (2ª definição, efetiva). `corrigir=TRUE` mantido em ambas — não desligado, conforme proibição explícita.

## 6. Testes sintéticos e resultados

Todos executados isoladamente em `/tmp`, com as dependências (incluindo o dump completo do XLSForm embutido, ~2260 linhas, extraído verbatim do script) sem dados reais:

| # | Cenário | Resultado |
|---|---|---|
| 1 | Estruturado resolvível (3 pontos, 3 tokens) | ✅ Resolve, não bloqueia |
| 2 | Estruturado não resolvível (descompasso 3 pontos/2 tokens) | ✅ Bloqueia, relatório gerado |
| 3 | Texto livre com `\|` legítimo (`observacoes_gerais`, valor `"Declividade leve \| rochas amontoadas..."`) | ✅ Preservado intacto, sem alteração, sem bloqueio |
| 3b | Texto livre com path divergente do dump (`impact_manejo_uso/tipos_impacto_manejo_uso_outro`, fallback por nome curto) | ✅ Reconhecido via fallback, preservado |
| 4 | `forma_serrapilheira` (estruturado via metadado, não via regex de nome) resolvível | ✅ Resolve corretamente |
| 4b | `forma_serrapilheira` não resolvível | ✅ Bloqueia |
| 5 | Coluna totalmente desconhecida (fora do contrato) com `\|` | ✅ Reportada como `pipe_indeterminado`, não bloqueia |
| 6 | Sem `\|` algum | ✅ Sem alteração, sem bloqueio |
| 7 | Controle: produto bruto nunca bloqueia | ✅ Confirmado |
| 8 | Regressão: `monitora_pendencia_impeditiva_tipo/_msg` com `\|` interno legítimo | ✅ Preservado intacto, sem bloqueio |

**Resultado: todos os cenários PASSARAM**, incluindo a checagem explícita de que o fallback por nome curto resolve a divergência real encontrada entre o path registrado no dump embutido e o path do template/export.

## 7. Baseline PNB e resultados

Executada duas vezes (antes e depois do ajuste de escopo 2), mesma pasta de execução limpa, mesmo `input/pnb_2022_2026.zip` (hash confirmado idêntico nas duas rodadas):

| Métrica | Fase 1 (guarda simples) | Fase 2 (com proteção de texto livre) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606/6 | 606/6 | ✅ |
| Nativa sem forma de vida | 3/3 | 3/3 | ✅ |
| Seca/morta sem forma de vida | 10/8 | 10/8 | ✅ |
| Outra forma de vida legada | 7/5 | 7/5 | ✅ |
| Colunas importados/corrig | 176/180 | 176/180 | ✅ |
| Tamanho dos 3 produtos de dados | byte-idênticos | byte-idênticos | ✅ |
| Tempo núcleo | 51,2 s | 53,4 s | ✅ equivalente |
| Bloqueios/erros | Nenhum | Nenhum | ✅ |
| Relatório `pipe_indeterminado`/`bloqueio_pipe_residual` gerado? | Não | Não (dataset PNB não tem nenhum dos dois cenários) | ✅ esperado |

Importante: na **primeira** execução completa da Fase 1 (antes de eu corrigir um falso-positivo próprio, ver seção 9), a baseline PNB **bloqueou indevidamente** por resíduo de `|` nas colunas `monitora_pendencia_impeditiva_tipo`/`_msg` (que usam `|` como separador interno de lista de tags, não relacionado a ponto amostral). Esse falso-positivo foi corrigido antes de qualquer regressão real ser rodada, adicionando a mesma exclusão de colunas técnicas (`pipe_residual_tecnico_tolerado`) já usada pelo resolvedor original à minha guarda. As contagens na tabela acima já refletem a versão corrigida.

## 8. Regressão real local e resultados

Executados fora do Git, em pastas de execução isoladas e descartáveis, os 4 datasets 2025 reais já usados no Hotfix 03.5B, com o script final (ajuste de escopo 2 incluído):

| Dataset | Exit | Bloqueios | `pipe_indeterminado` gerado? | Colunas `registros_corrig.csv` |
|---|---|---|---|---|
| 01_2025_max_outra_forma_vida (FNC_2026) | 0 | Nenhum | Não | 174 |
| 03_2025_serrapilheira_volume (PNCV_2026) | 0 | Nenhum | **Sim** — `"Outros tipos de manejo ou uso: (impact_manejo_uso)"` (label com sufixo de grupo divergente do dump; reportado, não bloqueado — ver seção 9) | 174 |
| 04_2025_fncs_anexado (FNCS_2026) | 0 | Nenhum | Não | 174 |
| 05_2025_pnm_anexado (PNM_2025) | 0 | Nenhum | Não | 174 |

Todos os 4 passaram sem bloqueio. No dataset 01, especificamente, confirmei que `observacoes_gerais` manteve seu conteúdo com `|` intacto (505 ocorrências de `|` presentes em `registros_importados.csv`, o mesmo texto observado no bloqueio da Fase 1) — ou seja, o dado que antes causava bloqueio agora é corretamente preservado sem alteração.

## 9. Evidência de que `corrigir=TRUE` foi mantido

Nenhuma das duas chamadas efetivas de `monitora_produtos_resolver_pipes_por_ponto` teve o argumento `corrigir` alterado — ambas continuam `corrigir = TRUE`. A proteção de texto livre atua **antes**, na classificação (`monitora_produtos_classificar_pipe_coluna`), impedindo que colunas de texto livre sejam sequer candidatas à alteração posicional (`alterar <- corrigir && classe %in% c("pipe_residual_operacional", "pipe_residual_revisar")` — inalterado; `pipe_permitido_texto_livre` e `pipe_indeterminado` simplesmente nunca aparecem nessa lista).

## 10. Evidência de que pipe residual estruturado bloqueia exportação

Testes sintéticos #2 e #4b (seção 6) demonstram bloqueio real via `stop()` com mensagem contendo produto, número de linhas, colunas afetadas e caminho do relatório. Nenhum dos 4 datasets reais tinha resíduo estruturado genuíno, então o bloqueio real não foi exercitado em dado de produção nesta rodada — apenas em ensaio sintético controlado, exatamente como pedido ("Criar testes sintéticos sem dados reais").

## 11. Achado corrigido durante a implementação (falso-positivo próprio)

Na primeira versão da guarda (antes de qualquer ajuste de escopo), testei a baseline PNB e ela **bloqueou indevidamente** `registros_corrig.csv` por `|` em `monitora_pendencia_impeditiva_tipo`/`_msg` — colunas técnicas internas do próprio script que usam `" | "` para juntar múltiplas tags de pendência na mesma linha (ex.: `"ponto_sem_interceptacao | nativa_sem_forma_vida"`), sem qualquer relação com ponto amostral. Corrigi isso fazendo minha guarda respeitar a mesma classificação `pipe_residual_tecnico_tolerado` que o resolvedor original já usa para essas colunas (`grepl("^(monitora_|...)", cc)`), evitando duplicar uma decisão que já existia. Teste de regressão dedicado (#8, seção 6) cobre esse caso especificamente.

## 12. Arquivos alterados

- `monitora_campsav_alvo_global_v2.6.0.R` — único arquivo de código alterado. Diff final: +177/-1 linhas.
  - Nova função `monitora_pipe_coluna_classificar_natureza` (consulta o contrato XLSForm embutido).
  - `monitora_produtos_classificar_pipe_coluna` modificada (adiciona checagem de texto livre antes das demais regras; novo default `pipe_indeterminado`).
  - Nova função `monitora_bloquear_pipe_residual_produto` (guarda pós-resolução, 3 categorias de tratamento).
  - Duas chamadas novas da guarda, logo após as chamadas existentes de `monitora_produtos_resolver_pipes_por_ponto` para `registros_importados.csv` e `registros_corrig.csv`.
- `diagnostics/hotfix_035c_bloqueia_pipe_residual/HOTFIX_035C_BLOQUEIA_PIPE_RESIDUAL.md` — este relatório.
- Scripts de teste sintético em `/tmp/.../scratchpad/` — não versionados, sem dados reais.

Nota: a função `monitora_produtos_classificar_pipe_coluna` tem uma 1ª definição (morta, nunca executa) mais cedo no arquivo, que **não foi alterada** — apenas a 2ª definição (efetiva) foi modificada, seguindo o mesmo padrão de cautela já usado nos hotfixes anteriores diante desse tipo de duplicação pré-existente no script.

## 13. Fora do escopo

- Vazamento de sanitização semântica em `registros_importados.csv` via seleção posicional em si (Hotfix 03.5C-A/A2) — este hotfix não alterou a lógica de escolha posicional para campos estruturados, apenas impediu que ela fosse aplicada a campos de texto livre.
- Outra forma de vida legada.
- Bug de vírgula interna na tokenização (documentado no Hotfix 03.5B).
- Escopo do painel 101/84.
- Centralização completa de contratos/motores de validação.
- Aperfeiçoamento adicional do casamento de `label` com sufixos de grupo divergentes (caso do dataset 03, seção 8) — hoje cai em `pipe_indeterminado`, comportamento seguro mas não perfeitamente preciso; deixado como está por ser uma lacuna de baixo risco (não bloqueia, não altera) e por evitar expandir o escopo cirúrgico desta correção.

## 14. Próximo passo recomendado

Nenhum hotfix adicional urgente identificado nesta rodada. Se o usuário quiser fechar a lacuna residual da seção 8 (labels com sufixo de grupo não reconhecidos), isso pode ser um pequeno ajuste pontual na normalização de `label` dentro de `monitora_pipe_coluna_classificar_natureza` (ex.: remover sufixos `" (grupo)"` antes de comparar) — proponho tratar como um item separado, menor, apenas se o usuário considerar relevante, já que hoje já é seguro (reporta, não corrompe, não bloqueia).

## Segurança Git

```
$ git status --short
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035c_bloqueia_pipe_residual/

$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | 178 +++++++++++++++++++++++++++++++++-
 1 file changed, 177 insertions(+), 1 deletion(-)

$ git diff --name-only
monitora_campsav_alvo_global_v2.6.0.R
```

Nenhum CSV/ZIP/XLSX/RDS/DB real staged ou criado dentro do repositório. Staging previsto apenas para:
- `monitora_campsav_alvo_global_v2.6.0.R`
- `diagnostics/hotfix_035c_bloqueia_pipe_residual/HOTFIX_035C_BLOQUEIA_PIPE_RESIDUAL.md`

Commit ainda não realizado — aguardando as checagens de segurança finais abaixo.
