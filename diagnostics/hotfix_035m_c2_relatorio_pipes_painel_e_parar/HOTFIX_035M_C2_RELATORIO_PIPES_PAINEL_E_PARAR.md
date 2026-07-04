# Hotfix 03.5M-C2 — Relatório opt-in de pipes agora também roda em `painel_e_parar`

## 1. Achado da run 35mc

A run PNB `35mc` (script com `MONITORA_DIAGNOSTICO_PIPES_CONTRATO <- "S"`) confirmou que o fluxo operacional continua correto (bruto/importados/corrig materializados, validados corretamente bloqueado, encerramento controlado, zero pipe residual), **mas** `output/diagnosticos_pipes_contrato/` não foi criado, apesar da flag estar ligada.

## 2. Causa do não acionamento

A única chamada de `monitora_pipe_contrato_relatorio_optin()` (03.5M-C) estava dentro do bloco:

```r
if (!isTRUE(MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE)) {
  ...
  try(monitora_pipe_contrato_relatorio_optin(...), silent = TRUE)
  ...
}
```

Em modo `painel_e_parar` (e também `ate_registros_corrig`/`abrir_painel_cache`/`painel_incremental_registros_corrig`, via `MONITORA_PARAR_APOS_REGISTROS_CORRIG`), o script executa **antes** desse bloco:

```r
if (isTRUE(MONITORA_PARAR_APOS_REGISTROS_CORRIG)) {
  monitora_execucao_gravar_checkpoint_parcial(registros_corrig, produto = "registros_corrig.csv", motivo = MONITORA_MODO_EXECUCAO)
  MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE <- TRUE
}
```

— ou seja, `MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE` já vira `TRUE` antes da checagem `!isTRUE(...)`, então o bloco inteiro (e a chamada do relatório opt-in dentro dele) é pulado. Isso não é um bug do bloco em si — é o comportamento **intencional** de `painel_e_parar` (parar antes da exportação completa/`registros_validados`) — só que a 03.5M-C tinha sido conectada exclusivamente no lado "não parou", perdendo justamente o caminho mais usado para teste manual.

## 3. Patch aplicado

**Helper novo** `monitora_pipe_contrato_relatorio_optin_seguro(registros, contexto, output_dir, log_dir, exec_id)`, inserido logo após `monitora_pipe_contrato_relatorio_optin()`:

- Retorna `invisible(NULL)` imediatamente se a função-base não existir, ou se a marca global `MONITORA_DIAGNOSTICO_PIPES_CONTRATO_GERADO` já estiver `TRUE` (mesma execução).
- Chama `monitora_pipe_contrato_relatorio_optin()` dentro de `tryCatch` (erro vira `warning`, nunca propaga).
- Só marca `MONITORA_DIAGNOSTICO_PIPES_CONTRATO_GERADO <- TRUE` (via `assign(..., envir = .GlobalEnv)`) quando a chamada retorna resultado não-nulo — o que só acontece com a flag `"S"` e sucesso na geração. Com a flag `"N"`, a marca nunca é setada (cada chamada continua um no-op barato, sem custo cumulativo).
- Nunca altera `registros`.

**Dois pontos de chamada**, ambos agora usando o wrapper seguro:

1. **Novo**, dentro do bloco `if (isTRUE(MONITORA_PARAR_APOS_REGISTROS_CORRIG))`, logo após `monitora_execucao_gravar_checkpoint_parcial(...)` e antes de `MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE <- TRUE` — `contexto = "checkpoint_parcial_registros_corrig"`.
2. **Existente** (03.5M-C), no caminho completo, dentro de `if (!isTRUE(MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE))` — só trocou a função chamada (`monitora_pipe_contrato_relatorio_optin` → `monitora_pipe_contrato_relatorio_optin_seguro`), `contexto = "pos_export_registros_corrig"` mantido.

Ambas as chamadas continuam envolvidas em `try(..., silent = TRUE)`, além do `tryCatch` interno do próprio wrapper — dupla camada de segurança, mesmo padrão já usado em toda a linhagem 03.5L-B/03.5M-C.

## 4. Como evita duplicação

Os dois pontos de chamada são, na prática, **mutuamente exclusivos por modo de execução**: `painel_e_parar` (e afins) sempre encerra antes de alcançar o segundo ponto; o modo `completo` nunca passa pelo primeiro bloco com essa configuração de flags. Mesmo assim, a marca global `MONITORA_DIAGNOSTICO_PIPES_CONTRATO_GERADO` garante que, **se algum modo futuro ou alguma mudança de código viesse a alcançar os dois pontos na mesma execução**, só a primeira chamada bem-sucedida geraria arquivo — a segunda retorna `NULL` sem tocar em nada (nem sobrescreve, nem duplica; simplesmente não executa). Testado explicitamente (seção 5, teste de duplicação): a 2ª chamada no mesmo processo R devolve `NULL`, não cria arquivo novo, e os arquivos da 1ª chamada permanecem com o mesmo `mtime` (não foram reescritos).

## 5. Testes realizados

Mesmo método das etapas anteriores (extração literal das funções reais para fora do repositório, `Rscript` puro, sem PNB/FNCS/pipeline/painel).

| # | Teste | Resultado |
|---|---|---|
| 1 | `Rscript -e 'invisible(parse(...)); cat("PARSE_OK\n")'` | `PARSE_OK` |
| 2 | Sintético, flag `"N"`, via wrapper | Retorno `NULL`, 0 arquivos, marca global permanece `FALSE` |
| 3 | Sintético, flag `"S"`, via wrapper, `contexto="checkpoint_parcial_registros_corrig"` | Retorno lista com `diagnostico`/`resumo`; 3 arquivos gerados (CSV diagnóstico, CSV resumo, TXT); marca global passa a `TRUE` |
| 3b | Conteúdo dos 3 arquivos varrido por valores distintivos do dataset sintético | Nenhum valor real encontrado |
| 4 | Imutabilidade do objeto de entrada (`names`/conteúdo), antes/depois, flag OFF e ON | Idênticos em ambos os casos |
| 4 (duplicação) | 2ª chamada do wrapper na mesma "execução" (mesma marca global já `TRUE`) | Retorno `NULL`; nenhum arquivo novo; `mtime` dos arquivos originais inalterado |
| 5 | Leitura estática: confirmar chamada em `painel_e_parar` e no caminho completo | Confirmado — 2 ocorrências de `monitora_pipe_contrato_relatorio_optin_seguro(` no script, uma logo após `monitora_execucao_gravar_checkpoint_parcial` dentro do bloco `if (isTRUE(MONITORA_PARAR_APOS_REGISTROS_CORRIG))`, outra no bloco completo existente |
| — | Confirmação de que nenhuma função operacional de resolução de pipe foi alterada | `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna` não aparecem no diff |

## 6. Confirmação: não altera produtos reais

O wrapper e as duas chamadas só leem `registros_corrig` (já em memória) para diagnóstico — nunca escrevem nele, nunca alteram `registros_importados.csv`/`registros_corrig.csv`/painel/`registros_validados.csv`/estatísticas. Confirmado por desenho (nenhuma das funções em jogo tem `data.table::set()`/`setnames()`/`setcolorder()` sobre o objeto de entrada) e por teste (seção 5, testes 4 e 4b).

## 7. Confirmação: warning `data.table::unique` fora de escopo

Não investigado, não tocado — nenhuma evidência encontrada nesta etapa que o relacione a pipes/relevance/cardinalidade ou ao ponto de integração corrigido.

## 8. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 73 +++++++++++++++++++++++++++++++----
 1 file changed, 66 insertions(+), 7 deletions(-)
```

As 7 remoções são exclusivamente a troca do nome de função chamada no ponto de integração existente (`monitora_pipe_contrato_relatorio_optin` → `monitora_pipe_contrato_relatorio_optin_seguro`, mais o comentário atualizado) — nenhuma lógica de pipe/relevance/cardinalidade ou de exportação de produto foi tocada. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna`, `monitora_pipe_contrato_diagnosticar_dataset` e `monitora_pipe_contrato_resumir_diagnostico_dataset` não aparecem no diff.

## 9. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035m_c2_relatorio_pipes_painel_e_parar/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
