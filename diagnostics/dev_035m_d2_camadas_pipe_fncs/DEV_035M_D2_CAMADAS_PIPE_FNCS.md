# Dev 03.5M-D2 — Camada de desaparecimento do pipe FNCS e instrumentação pré-normalização

## 1. Resumo executivo

**Causa raiz identificada com alta confiança, por leitura de código e teste isolado reproduzível — não é `monitora_produtos_resolver_pipes_por_ponto`.** O mecanismo que efetivamente processa as colunas condicionais de forma de vida (bromelioide/cactacea/orquidea/samambaia × nativa/exótica/seca-morta) é um bloco de código **inline, anterior e estruturalmente diferente** de tudo que as etapas 03.5M-A a D1 investigaram: uma sequência `fifelse`/`fcase` + `word(x, sep = fixed("|"), -1)` ("extração do último token"), executada bem antes de qualquer checkpoint 03.5M existente. Esse bloco:

1. Explica **por completo** por que nenhum diagnóstico 03.5M (nem o novo, nem o legado `monitora_bloquear_pipe_residual_produto`) via pipe residual em `registros_corrig`/`registros_importados` na run real 35md1: o pipe já foi removido (por `word(...,-1)`) muito antes desses pontos rodarem.
2. Também explica, separadamente, por que `registros_importados_bruto.csv` mostrava a coluna como `MONITORA_COL_00049` em vez de seu nome real: **bug real e reproduzível**, confirmado por teste isolado, em `monitora_publicacao_h_fwrite_registros_importados()` — o nome técnico interno "vaza" para o cabeçalho final por compartilhamento de referência entre `nomes_originais` e `names(out)`, exatamente como um comentário já existente no próprio código antecipava como risco.
3. Levanta uma preocupação **mais séria do que resíduo bloqueante**: a extração `word(..., -1)` toma sempre o **último** token e aplica esse mesmo valor a **toda linha que satisfaça o teste `str_detect()` sobre a lista-mãe**, sem checar qual token pertence a qual ponto elegível. Se dois pontos elegíveis tiverem valores **diferentes** entre si (não apenas repetições do mesmo valor, como em todos os casos reais observados até agora), esse mecanismo perderia silenciosamente o(s) valor(es) não-último(s) — uma falha potencialmente pior que o bloqueio, porque não gera nenhum erro nem resíduo visível.

Como resposta, foi criada a instrumentação **mais simples possível**: reutilizar 100% da função e do wrapper opt-in já existentes (`monitora_pipe_contrato_relatorio_optin_seguro`, 03.5M-C/C2), adicionando apenas **um novo ponto de chamada** — com um novo `contexto` — imediatamente **antes** do bloco `word(...,-1)`, onde o pipe ainda está intacto. Nenhuma flag nova, nenhuma função nova, nenhuma lógica nova.

## 2. Evidência da run FNCS 35md1

Run localizada em `/home/dfed/Desktop/03jul_dev/35md1_fncs/35md1_fncs.zip` (2026-07-03 22:36–22:40), inspecionada **só leitura**, extraindo **apenas** arquivos pequenos de auditoria/diagnóstico (nunca os CSVs de dados brutos de 23–29MB) para um diretório temporário fora do repositório, **apagado ao final**.

| Produto | Linhas × Colunas |
|---|---|
| `registros_importados_bruto.csv` | 16.968 × 142 |
| `registros_importados.csv` | 16.968 × 164 |
| `registros_corrig.csv` | 16.463 × 181 |

Auditorias de pipe já geradas pelo próprio script, lidas do ZIP:

| Arquivo | Resultado |
|---|---|
| `auditoria_pipe_residual_resumo_registros_importados_csv` | `n_colunas_preservadas=0, n_linhas_pipe_preservadas=0, n_colunas_indeterminadas=0, n_linhas_pipe_indeterminadas=0, n_colunas_estruturado_residual=0` — **zero pipe em qualquer categoria**, já no primeiro checkpoint pós-tokenização (`pre_painel_pos_extracao_tokens_operacionais`) |
| `auditoria_pipe_residual_resumo_registros_corrig_csv` | Idêntico — zero em todas as categorias |
| `relatorio_pipes_contrato_checkpoint_parcial_registros_corrig` (03.5M-C2, opt-in) | `Total de colunas: 177`, `Colunas com pipe: 0`, recomendação `sem_acao` |

## 3. Confirmação: pipe presente no bruto, ausente no importado/corrigido

Confirmado — o cabeçalho completo de `registros_importados_bruto.csv` (lido só a 1ª linha do CSV dentro do ZIP, sem tocar nenhuma linha de dado) mostra que **as 142 colunas**, sem exceção, aparecem como `MONITORA_COL_00001` … `MONITORA_COL_00142` — não apenas a coluna 49. Isso confirma que o "achado" da run (9 células com pipe em `MONITORA_COL_00049`) foi obtido por uma inspeção ad hoc do arquivo bruto (provavelmente `fread()` + busca de "|"), e que a **coluna 49 é simplesmente a única, entre as 142, que tinha pipe** — seu nome semântico real não pôde ser recuperado a partir do próprio arquivo (ver seção 5).

## 4. Mapeamento de `MONITORA_COL_00049`

**Não foi possível recuperar o nome original a partir do arquivo `registros_importados_bruto.csv` desta run** — porque a corrupção afeta as 142 colunas por igual (não é uma perda seletiva de 1 coluna). A causa raiz dessa corrupção **foi identificada e comprovada por teste isolado** (não é específica desta run; afeta qualquer execução que passe por este caminho de código):

`monitora_campsav_alvo_global_v2.6.0.R`, função `monitora_publicacao_h_fwrite_registros_importados()` (usada exclusivamente para escrever `registros_importados_bruto.csv`):

```r
nomes_originais <- enc2utf8(as.character(base::names(out)))
...
nomes_internos <- sprintf("MONITORA_COL_%05d", seq_along(nomes_originais))
...
data.table::setnames(out, nomes_internos)   # <- muta por referência
...
header <- paste(monitora_publicacao_i_csv_quote(nomes_originais), collapse = ",")  # <- deveria usar o nome ORIGINAL
```

O próprio comentário do código, já presente antes desta tarefa, previa exatamente este risco: *"Em data.table, names(out) pode compartilhar referência interna; se o vetor for mantido por referência, o setnames(out, nomes_internos) pode vazar para o cabeçalho manual e produzir MONITORA_COL_00001... no CSV final."* — **o risco descrito se confirmou na prática.**

**Teste isolado reproduziu o bug de forma determinística e trivial** (função extraída verbatim do script para um harness isolado, `Rscript` puro):

| Cenário testado | Nome da coluna 49 no CSV final |
|---|---|
| 60 colunas, nomes normais, sem duplicata, sem valor vazio | `"MONITORA_COL_00049"` (esperado: nome original) |
| Nome original vazio/ausente | `"MONITORA_COL_00049"` |
| Duas colunas com nome real idêntico (caso de duplicata, o cenário que a função foi desenhada para tratar) | `"MONITORA_COL_00049"` / `"MONITORA_COL_00050"` |
| Releitura do CSV final via `fread()` | Mesmo resultado — `MONITORA_COL_00049` |

**Ou seja: o bug ocorre mesmo no caso mais simples possível (nomes únicos, bem formados), não só nos casos de duplicata que a função foi originalmente escrita para tratar.** `enc2utf8(as.character(names(out)))` não força necessariamente uma cópia independente do vetor de nomes em R/data.table; quando `setnames()` (que modifica por referência, é essa a razão de sua existência) sobrescreve `names(out)`, o vetor capturado em `nomes_originais` é corrompido junto, porque aponta para o mesmo objeto subjacente.

**Nenhuma alteração foi feita a esta função nesta tarefa** — o escopo desta etapa permite só diagnóstico opt-in/aditivo; a correção deste bug é candidata a uma tarefa de hotfix futura e específica (ver seção 12).

## 5. Camada provável onde o pipe desaparece — achado principal

Localizada por auditoria de código (`grep`/leitura), **não é `monitora_produtos_resolver_pipes_por_ponto`.** É um bloco de código totalmente diferente, mais antigo (comentário `## erva bromelioide seca ou morta` etc.), no trecho identificado pelo próprio script como `padronizacao_formas_vida_condicionais_basicas` (checkpoint em `monitora_campsav_alvo_global_v2.6.0.R:36364` após a correção, ~linha 36341 antes):

```r
### extração do último token em colunas específicas (o SISMONITORA exporta a lista concatenada por "|")

## ponto amostral
registros_corrig$`ponto_amostral (amostragem/registro)` <-
  word(registros_corrig$`ponto_amostral (amostragem/registro)`, sep = fixed("|"), -1)
...
## erva bromelioide seca ou morta
registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2` <-
  fcase(
    registros_corrig$PROTOCOLO == "...",
    fcase(
      str_detect(registros_corrig$`Formas de vida de plantas ...secas ou mortas:...`, "bromelioide", negate = FALSE),
      word(registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2`, sep = fixed("|"), -1)
    ),
    ...
  )
```

Este padrão (`fifelse`/`fcase` gated em `str_detect(coluna_mãe, token)`, seguido de `word(coluna_filha, sep="|", -1)`) se repete para **todas as 12 combinações** categoria×forma (nativa/exótica/seca-morta × bromelioide/cactacea/orquidea/samambaia) e também para `ponto_amostral`, `ponto_metro` e `**Encostam** na vareta:`.

## 6. Causa provável (hipótese técnica, não migrada nem corrigida nesta etapa)

O comentário do próprio bloco documenta a premissa de design: *"extração do último token em colunas específicas (o SISMONITORA exporta a lista concatenada por '|')"* — ou seja, o desenho assume que, quando essas colunas aparecem com pipe, é porque o SISMONITORA reexportou/duplicou a mesma resposta (histórico de edição/reenvio), e que **tomar sempre o último token é seguro**.

Essa premissa parece válida para `ponto_amostral`/`ponto_metro`/`Encostam` (colunas técnicas, sem variação semântica esperada). **Para os 12 campos condicionais (bromelioide/cactacea/orquidea/samambaia), a mesma premissa é a exata causa raiz documentada no Hotfix 03.5G2**: nesses campos, o pipe pode legitimamente representar **respostas distintas em pontos elegíveis diferentes** dentro da mesma COLETA (ex.: ponto 90 = `terrestre`, ponto 91 = `epifita`) — não uma duplicata redundante. `word(..., -1)` aplicado uniformemente a toda linha que satisfaça `str_detect(mãe, token)` tomaria **sempre o último valor observado na COLETA inteira**, aplicando-o a **todos os pontos elegíveis daquela COLETA**, inclusive os que originalmente tinham valor diferente. Em todos os casos reais inspecionados até agora (Hotfix 03.5G2, D1, este achado), os tokens repetidos observados eram **idênticos entre si** (ex.: `terrestre|terrestre`), o que mascara esse efeito — o resultado final coincide com o valor correto por não haver, nos casos observados, dois valores realmente diferentes. **Isso não garante que a mesma coincidência se sustente em todas as COLETAs futuras.**

Esta é uma hipótese fundamentada em leitura de código, não confirmada com um caso real onde os tokens divergem — nenhuma run real disponível localmente teve token divergente nas colunas condicionais (seção 3 do relatório D1 e evidência desta etapa mostraram sempre `"terrestre"` repetido).

## 7. Trechos/funções auditadas

- `monitora_publicacao_h_fwrite_registros_importados()` (bug de nome, seção 4).
- `monitora_registros_importados_exportar()` (chamador do writer acima).
- Bloco inline `## ponto amostral` / `## erva bromelioide seca ou morta` / `## cactacea seca ou morta` / `## orquídea` / `## samambaia` (não são funções nomeadas — código de nível superior entre os checkpoints `padronizacao_categorias_e_material_botanico` e `padronizacao_formas_vida_condicionais_basicas`).
- `monitora_pre_painel_quarentenar_coletas_incompletas` — descartado como explicação principal: o corte de 505 linhas (16.968→16.463) acontece **depois** de `registros_importados.csv` já auditar zero pipe (que tem as mesmas 16.968 linhas de `registros_importados_bruto.csv`), então a quarentena de completude não é a causa do desaparecimento observado nesta run — só reduz linhas depois que o pipe já havia sumido.
- Nenhuma das funções restritas (`monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna`) participa deste caminho — **confirma que a resolução real de pipe condicional/esparso em produção nunca passou por essas funções.**

## 8. Instrumentação criada

**Nenhuma flag nova.** Reaproveitado 100% o wrapper e a função já existentes e testados (`monitora_pipe_contrato_relatorio_optin_seguro`, 03.5M-C/C2), com um **novo ponto de chamada** e um **novo valor de `contexto`**:

```r
if (exists("monitora_pipe_contrato_relatorio_optin_seguro", mode = "function")) {
  try(monitora_pipe_contrato_relatorio_optin_seguro(
    registros_corrig, contexto = "pre_normalizacao_formas_vida_condicionais",
    output_dir = MONITORA_OUTPUT_DIR, log_dir = MONITORA_LOG_DIR, exec_id = MONITORA_EXEC_ID
  ), silent = TRUE)
}
```

Inserido imediatamente após `monitora_perf_registrar_checkpoint("padronizacao_categorias_e_material_botanico", ...)` e **antes** do bloco `word(...,-1)` (seção 5) — o único ponto de integração que pode capturar o pipe condicional/esparso ainda intacto, de acordo com a auditoria de código desta etapa.

Continua controlado por `MONITORA_DIAGNOSTICO_PIPES_CONTRATO` (default `"N"`, sem alteração da flag em si) e pela marca global `MONITORA_DIAGNOSTICO_PIPES_CONTRATO_GERADO` (03.5M-C2): como esta nova chamada roda **primeiro** na ordem de execução, se encontrar pipe real, ela é a que grava os produtos; as duas chamadas já existentes (`checkpoint_parcial_registros_corrig`, `pos_export_registros_corrig`) ficam automaticamente suprimidas na mesma execução — o que é desejável, já que sabemos (por esta e pela run 35md1) que essas duas sempre relatam zero de qualquer forma, uma vez que rodam depois do bloco `word(...,-1)`.

## 9. Pontos de integração

Único novo ponto: logo após o checkpoint `padronizacao_categorias_e_material_botanico`, antes do comentário `### extração do último token em colunas específicas`. Os dois pontos já existentes (03.5M-C/C2) permanecem sem alteração de código, só passam a ficar, na prática, redundantes quando o novo ponto encontra algo (mas continuam funcionando normalmente quando o novo ponto não encontra pipe).

## 10. Testes realizados

| # | Teste | Resultado |
|---|---|---|
| 1 | `Rscript -e 'invisible(parse(...)); cat("PARSE_OK\n")'` | `PARSE_OK` |
| 2 | Teste isolado do bug de `monitora_publicacao_h_fwrite_registros_importados()` (3 cenários: nome normal, nome vazio, nomes duplicados) | Bug reproduzido nos 3 cenários — `MONITORA_COL_00049` aparece no header final mesmo com nome original bem formado |
| 3 | Releitura do CSV corrompido via `fread()` | Confirma a mesma corrupção — não é um artefato de exibição |
| 4 | Sintético, flag `"N"` (reuso do wrapper já testado) | Retorno `NULL`, 0 arquivos |
| 5 | Sintético, flag `"S"`, novo contexto `pre_normalizacao_formas_vida_condicionais` | 3 arquivos gerados, conteúdo correto, marca global setada |
| 6 | Sequência dos 3 pontos de chamada na ordem real do script, mesma execução, flag `"S"` | Só a 1ª chamada (`pre_normalizacao_formas_vida_condicionais`) gera arquivo; as 2 seguintes retornam `NULL` sem duplicar nem sobrescrever |
| 7 | Segurança: varredura de valores distintivos nos produtos gerados | Nenhum valor real encontrado |
| 8 | Leitura estática | 3 ocorrências de `monitora_pipe_contrato_relatorio_optin_seguro(` no script (1 nova + 2 existentes); `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna` não aparecem no diff |

## 11. Riscos de dados reais e mitigação

- ZIP da run 35md1_fncs: só arquivos pequenos de auditoria/diagnóstico e o console log foram extraídos (nunca os 3 CSVs de dados brutos de 23–29MB) para diretório temporário fora do repositório, **apagado ao final**.
- Header de `registros_importados_bruto.csv`: lida só a 1ª linha (nomes de coluna, não é dado sensível) via `unzip -p ... | head -1`, nunca o corpo do arquivo.
- Nenhum valor de célula, COLETA, UC, EA, UA, UUID, coordenada ou espécie foi transcrito neste relatório — só nomes de coluna, contagens agregadas e o token contratual genérico `"terrestre"` (já usado nas etapas anteriores).

## 12. Confirmação: resolvedor operacional não alterado

`git diff --stat` mostra só 23 inserções, 0 remoções, todas no novo bloco de instrumentação. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto` e `monitora_produtos_classificar_pipe_coluna` não aparecem no diff. O bloco `word(...,-1)`/`fifelse`/`fcase` identificado como causa provável (seção 5–6) **não foi tocado** — só documentado e usado como referência para posicionar a nova chamada de diagnóstico.

## 13. Confirmação: warning `data.table::unique`

Não investigado, não tocado nesta etapa.

## 14. Recomendação objetiva

**`instrumentacao_multicamada_necessaria`** — a run real 35md1 confirma que o mecanismo de resolução condicional/esparso em produção não é o previsto pelas etapas anteriores (03.5M-A a D1 assumiam `monitora_produtos_resolver_pipes_por_ponto` como o mecanismo relevante); é o bloco `word(...,-1)` identificado nesta etapa. A instrumentação criada (seção 8) deve ser exercitada em uma próxima run FNCS real para capturar, pela primeira vez, o estado do pipe **antes** desse bloco rodar — isso permitirá (a) confirmar quantitativamente quantas colunas/linhas realmente tinham pipe nesse ponto (a run 35md1 mostrou 1 coluna/9 células em bruto, mas o nome real ficou irrecuperável por causa do bug da seção 4) e (b) obter, pela primeira vez, um diagnóstico com nome de coluna real associado a pipe condicional/esparso ainda não resolvido, permitindo avaliar concretamente se `word(...,-1)` está de fato descartando valores divergentes (seção 6) em vez de só "coincidir" com o valor correto.

Adicionalmente, recomienda-se abrir uma tarefa de hotfix específica e separada para corrigir o bug de `monitora_publicacao_h_fwrite_registros_importados()` (seção 4) — é um bug real, reproduzível, e compromete a fidelidade do "snapshot técnico" que `registros_importados_bruto.csv` promete entregar (nenhuma coluna preserva seu nome real hoje). Essa correção está fora do escopo desta etapa (só diagnóstico/instrumentação aditiva foi autorizado).

## 15. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 23 +++++++++++++++++++++++
 1 file changed, 23 insertions(+)
```

23 inserções, **0 remoções** — confirmado puramente aditivo.

## 16. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035m_d2_camadas_pipe_fncs/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
