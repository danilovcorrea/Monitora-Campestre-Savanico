# Hotfix 03.5B — Resolvedor flexível de Encostam (rotulada vs. canônica)

## 1. Resumo executivo

A Auditoria 03.5A apontou que o bloco de tokenização de Encostam referenciava de forma hardcoded a coluna rotulada `**Encostam** na vareta: (amostragem/registro)`, ao contrário do resto do motor (painel, pendências impeditivas, Hotfixes 02/03), que usa um resolvedor flexível. A auditoria hipotetizou dois riscos: (a) criação de "coluna fantasma" `NA` quando só existe a coluna canônica `amostragem/registro/tipo_forma_vida`; (b) perda silenciosa de dado em schema misto.

**Investigação mais aprofundada nesta etapa revelou que o risco (a) não se concretiza na prática**: existe, ~360 linhas antes do bloco de tokenização, um bloco de renomeação/consolidação de nomes de coluna (`monitora_renomear_ou_consolidar_coluna`) que já roda incondicionalmente para dezenas de campos do protocolo, incluindo Encostam (linha ~32742 antes desta correção). Quando só a coluna canônica existe, esse bloco a **renomeia** para o nome rotulado antes da tokenização — não há coluna fantasma nesse caso isolado. Isso foi confirmado tanto por leitura de código quanto por teste isolado.

**O risco real e verificado** está em outro lugar: quando **ambas** as colunas existem simultaneamente com valores diferentes e não vazios (schema misto — plausível quando se concatenam exportações de diferentes convenções de cabeçalho), a função genérica `monitora_renomear_ou_consolidar_coluna` **preserva apenas o valor da coluna-alvo (rotulada) e descarta silenciosamente o valor da coluna de origem (canônica)**, registrando o conflito apenas em um log técnico (`monitora_log_registrar_evento`), não em um diagnóstico visível. Isso pode perder um token ecológico real (e.g., perder `solo_nu` verdadeiro, ou perder `serrapilheira`/`nativa` genuíno) sem qualquer sinal no produto de dados.

Este hotfix substitui a chamada genérica, exclusivamente para Encostam, por um resolvedor dedicado que nunca descarta conteúdo real silenciosamente: consolida por linha, une os dois lados quando complementares, e respeita a exclusividade contratual de `solo_nu` (removendo-o quando coexiste com qualquer outro conteúdo real, com diagnóstico explícito) — sem tocar em nenhum outro campo do protocolo, no vazamento de camada de `registros_importados.csv`, no escopo do painel ou em "outra forma de vida legada".

## 2. Bloco hardcoded encontrado

Antes da correção, em `monitora_campsav_alvo_global_v2.6.0.R`:

```r
if ("amostragem/registro/tipo_forma_vida" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/tipo_forma_vida", "**Encostam** na vareta: (amostragem/registro)")
}
```

(linha ~32742 antes da edição, dentro do grande bloco top-level, incondicional em relação ao modo de execução, de renomeação de ~300 campos do protocolo para os nomes finais usados por `registros_corrig.csv`.)

Logo depois (linha ~33103, `## Padronização de categorias gerais`), o Hotfix 01 já atua no bloco de tokenização real:

```r
registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(., c(
    ...
    "Outras plantas terrestres, líquens e/ou fungos" = "outra_forma_vida",   # Hotfix 01
    "Outras plantas terrestres, líquens e/ou fungos," = "outra_forma_vida"  # Hotfix 01
  ))
```

Este bloco de tokenização referencia a coluna rotulada diretamente, por nome literal — daí a dependência de que a renomeação anterior já tenha unificado tudo sob esse nome.

## 3. Risco antes do hotfix (confirmado empiricamente, refinado em relação à Auditoria 03.5A)

Teste isolado com a sequência real completa (renomeação + tokenização), sem dados reais:

| Cenário | Resultado ANTES do hotfix |
|---|---|
| Só coluna rotulada | Correto — `outra_forma_vida` |
| Só coluna canônica | Correto — renomeada para rotulada antes da tokenização, sem coluna fantasma (**corrige a hipótese da Auditoria 03.5A**) |
| Ambas, mesmo valor | Correto — coalesce sem perda |
| Ambas, uma vazia e outra preenchida | Correto — coalesce preenche o vazio |
| **Ambas preenchidas, valores diferentes e reais** (ex.: canônica="Plantas nativas", rotulada="Material botânico em decomposição no solo") | **Perde um dos dois lados silenciosamente** — o valor da coluna canônica é descartado, só um "AVISO" em log técnico, nada no dado final nem em relatório visível |

## 4. Estratégia de resolução flexível aplicada

Nova função `monitora_correcao_resolver_encostam_flexivel(dt, output_dir, log_dir, exec_id)`, inserida no lugar exato da chamada genérica anterior (mesmo ponto do fluxo, mesmo guard de existência da coluna canônica):

1. Nenhuma das duas colunas existe → não faz nada, não cria coluna.
2. Só a canônica existe → renomeia para o nome rotulado (mesmo comportamento já existente).
3. Só a rotulada existe → não faz nada.
4. Ambas existem → consolida linha a linha:
   - se um lado é vazio, usa o outro;
   - se os dois lados representam o mesmo conjunto de tokens (comparando por conjunto, ignorando ordem/separador — reaproveita a mesma filosofia de comparação por conjunto já usada em `monitora_correcao_tokenizar`), mantém um dos dois, sem duplicar;
   - se os dois lados têm conteúdo diferente e **nenhum é exclusivamente "Solo nu / rochas..."**, une os dois lados com espaço (não cria dado novo, apenas concatena os dois blobs brutos — o bloco de tokenização seguinte já reconhece cada rótulo embutido por correspondência de substring);
   - se um lado é exclusivamente "Solo nu / rochas..." e o outro tem qualquer conteúdo diferente, remove o lado `solo_nu` e preserva o outro — mesma regra de exclusividade contratual já aplicada pelo Hotfix 02 (Regra B) — e registra o caso em `output/auditoria_encostam_resolvedor_flexivel.csv` (espelhado em `log/`), com `tipo_resolucao`, valores originais dos dois lados e valor final.
5. Ao final, a coluna canônica é removida (o mesmo já acontecia na função genérica, e é consistente com o padrão de "consolidar tudo em um nome final" usado pelas ~300 outras chamadas de renomeação do mesmo bloco). Nenhum token é perdido: quando havia conflito real, o token descartado (`solo_nu`) é removido por regra contratual explícita e auditável, não por acaso; quando era complementar, nada é descartado.

**Por que não usar vírgula como separador na união**: o próprio bloco de tokenização original já tem uma segunda passada de limpeza (`"token," -> "token"`) que, se aplicada a um blob genuinamente multi-token separado por vírgula, apaga a vírgula e cola os dois tokens sem separador (bug pré-existente, não introduzido por este hotfix — ver seção 9). Usar espaço como separador de união evita acionar esse comportamento.

## 5. Testes sintéticos e resultados

Todos executados isoladamente (dependências extraídas verbatim do script, sem dados reais), com a função já corrigida no arquivo:

| # | Cenário | Resultado |
|---|---|---|
| 1 | `schema_rotulado` — só rotulada, valor "Outras plantas..." | ✅ `outra_forma_vida`; nenhuma coluna canônica residual |
| 2 | `schema_canonico` — só canônica, valor "Outras plantas..." | ✅ `outra_forma_vida`; **nenhuma coluna fantasma `NA`**; apenas 1 coluna final |
| 3 | `schema_misto_mesmo_valor` — ambas com "Plantas nativas" | ✅ `nativa`; sem duplicar, sem conflito |
| 4 | `schema_misto_complementar` — canônica="Plantas nativas", rotulada="Material botânico..." | ✅ `serrapilheira nativa`; nenhum token perdido |
| 4b | Equivalentes por conjunto, ordem trocada (`"A,B"` vs `"B,A"`) | ✅ Resolvedor reconhece equivalência e mantém um lado, sem concatenar duplicando — testado isoladamente na saída do resolvedor (ver nota na seção 9 sobre limite pré-existente do tokenizador para blobs com vírgula interna) |
| 5 | `schema_misto_conflitante` — canônica="Solo nu...", rotulada="Plantas nativas" | ✅ `nativa`, sem `solo_nu`; diagnóstico `conflito_solo_nu_removido` gravado em CSV com os dois valores originais |

**Resultado: todos os cenários PASSARAM.**

## 6. Baseline PNB e resultados

Mesmo comando/modo/pasta de execução limpa das etapas anteriores; `input/pnb_2022_2026.zip` intocado (hash confirmado idêntico).

| Métrica | Antes (Hotfix 03) | Depois (Hotfix 03.5B) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606/6 | 606/6 | ✅ |
| Nativa sem forma de vida | 3/3 | 3/3 | ✅ |
| Seca/morta sem forma de vida | 10/8 | 10/8 | ✅ |
| Outra forma de vida legada | 7/5 | 7/5 | ✅ |
| Colunas importados/corrig | 176/180 | 176/180 | ✅ |
| Tamanho dos 3 produtos de dados | idênticos | byte-idênticos | ✅ |
| `registros_importados.csv`/`registros_corrig.csv` distintos | Sim | Sim (hashes distintos) | ✅ |
| Tempo núcleo | ~51 s | 52,3 s | ✅ equivalente |
| Novos warnings/erros | — | Nenhum (warning pré-existente `data.table::unique` mantido) | ✅ |
| `auditoria_encostam_resolvedor_flexivel.csv` gerado? | — | Não (dataset PNB só tem a coluna rotulada; nunca as duas simultaneamente) | ✅ esperado |

## 7. Regressão real local e resultados

Executados fora do Git, em pastas de execução isoladas e descartáveis (`/tmp/.../scratchpad/exec_hotfix035b_regressao*`), os 4 datasets 2025 de schema canônico solicitados:

| Dataset | Exit | Erros | Colunas import./corrig | `outra_forma_vida` in→out | `solo_nu` in→out | `serrapilheira` in→out | Diagnóstico do resolvedor | Coluna Encostam final |
|---|---|---|---|---|---|---|---|---|
| 01_2025_max_outra_forma_vida (FNC_2026) | 0 | nenhum | 170/174 | 761→761 | 280→280 | — | não gerado (schema puro) | única, sem fantasma |
| 03_2025_serrapilheira_volume (PNCV_2026) | 0 | nenhum | 170/174 | 17→17 | 802→802 | 27776→28044 | não gerado (schema puro) | única, sem fantasma |
| 04_2025_fncs_anexado (FNCS_2026) | 0 | nenhum | 170/174 | 295→295 | 44→44 | 9994→10082 | não gerado (schema puro) | única, sem fantasma |
| 05_2025_pnm_anexado (PNM_2025) | 0 | nenhum | 170/174 | 6→6 | 116→116 | 3253→3878 | não gerado (schema puro) | única, sem fantasma |

Em todos os 4 datasets: `outra_forma_vida` e `solo_nu` preservados **exatamente** entre input bruto e `registros_corrig.csv`; nenhuma coluna fantasma; nenhum erro novo; nenhum dado real copiado para dentro do repositório (execuções inteiramente em `/tmp`).

**Nota sobre a variação de `serrapilheira`** (mesma observada e já documentada no relatório do Hotfix 03): o total de ocorrências da substring `serrapilheira` no arquivo inteiro varia entre input e `registros_corrig.csv` (ex.: 27776→28044 no dataset 03) com o mesmo número de linhas nos dois arquivos. Isso é consistente com a sincronização pré-existente do campo superior a partir de material botânico (`monitora_correcao_sincronizar_encostam_final`), presente desde antes deste hotfix, e não está relacionado ao resolvedor de Encostam — nenhum desses datasets teve `outra_forma_vida`/`solo_nu` alterado, que são os tokens sob responsabilidade direta deste hotfix e do Hotfix 01.

Nenhum desses 4 datasets exercitou o "schema misto" (ambas as colunas presentes simultaneamente) — todos usam exclusivamente o schema canônico. Isso é esperado: os datasets de referência foram selecionados por cobertura de token, não por mistura de convenção de cabeçalho. O cenário de schema misto permanece validado apenas por teste sintético (seção 5, casos 3–5).

## 8. Arquivos alterados

- `monitora_campsav_alvo_global_v2.6.0.R` — único arquivo de código alterado. Diff: +121/-1 linhas, todas dentro de um único bloco (nova função `monitora_correcao_resolver_encostam_flexivel` + substituição da chamada genérica anterior por uma chamada a ela). Nenhuma outra função, nenhum outro campo do protocolo, nenhum default público alterado.
- `diagnostics/hotfix_035b_encostam_resolvedor_flexivel/HOTFIX_035B_ENCOSTAM_RESOLVEDOR_FLEXIVEL.md` — este relatório.
- Scripts de teste sintético usados nesta etapa ficaram em `/tmp/.../scratchpad/` (não versionados, conforme escopo; sem dados reais).

## 9. Fora do escopo (propositalmente não tratado)

- **Vazamento de sanitização semântica para `registros_importados.csv`** (achado da Auditoria 03.5A envolvendo `monitora_produtos_resolver_pipes_por_ponto`) — não tocado; é o Hotfix 03.5C.
- **"Outra forma de vida" legada** (resíduos de sublista `outra` em nativa/exótica/seca-morta) — não tocado.
- **Escopo do painel** (101/84 pontos, qualquer lógica de UI Shiny) — não tocado.
- **Centralização completa dos contratos/motores de validação** (item já apontado nas auditorias anteriores) — não tocado; continua sendo um projeto separado, maior.
- **Bug pré-existente do bloco de tokenização original para blobs com vírgula interna** (`"token," -> "token"` apaga a vírgula separadora entre dois tokens reais quando o blob inteiro é comma-joined, colando-os em uma palavra só, ex. `"nativa,exotica"` → `"nativaexotica"`) — confirmado durante os testes desta etapa (caso 4b), mas é um comportamento do bloco de tokenização original, independente deste hotfix e fora do seu escopo cirúrgico. Registrado aqui para conhecimento; candidato a tratamento em hotfix futuro dedicado ao bloco de tokenização em si, não ao resolvedor de coluna.

## 10. Próximo passo recomendado

**Hotfix 03.5C** — tratar o vazamento de sanitização semântica identificado na Auditoria 03.5A para `registros_importados.csv` (a função `monitora_produtos_resolver_pipes_por_ponto`, chamada com `corrigir=TRUE`, altera valores de colunas ecológicas — Encostam, forma_vida_*, espécie, hábito — já nessa camada, antes de `registros_corrig.csv`). Antes disso, também vale registrar como candidato de baixa prioridade o bug pré-existente do bloco de tokenização com blobs comma-joined citado na seção 9.

## Segurança Git

```
$ git status --short
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035b_encostam_resolvedor_flexivel/

$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | 121 +++++++++++++++++++++++++++++++++-
 1 file changed, 121 insertions(+), 1 deletion(-)

$ git diff --name-only
monitora_campsav_alvo_global_v2.6.0.R
```

Nenhum CSV/ZIP/XLSX/RDS/DB real staged ou criado dentro do repositório. Staging previsto apenas para:
- `monitora_campsav_alvo_global_v2.6.0.R`
- `diagnostics/hotfix_035b_encostam_resolvedor_flexivel/HOTFIX_035B_ENCOSTAM_RESOLVEDOR_FLEXIVEL.md`

Commit ainda não realizado nesta etapa — aguardando autorização explícita do usuário, conforme critério de sucesso.
