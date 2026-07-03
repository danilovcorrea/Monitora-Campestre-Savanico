# Dev 03.5M-C — Relatório/alerta opt-in de pipes por contrato (conectado, flag OFF por padrão)

## 1. Resumo executivo

Conectado ao pipeline real (pela primeira vez nesta linhagem 03.5M) um relatório/alerta de pipes por contrato único, **inteiramente opt-in**: com a flag nova desligada (padrão), zero código novo executa. Com a flag ligada, só gera relatório à parte — nunca resolve, bloqueia, reordena ou altera `registros_corrig`, nem substitui `monitora_produtos_resolver_pipes_por_ponto`/`monitora_bloquear_pipe_residual_produto`. A integração acontece no ponto sugerido pela própria tarefa: logo após `registros_corrig.csv` ser exportado e ter sua persistência confirmada (mesmo checkpoint tocado pelo Hotfix 03.5L-C), antes da exportação de `registros_validados.csv`.

## 2. Flag criada e default

```r
MONITORA_DIAGNOSTICO_PIPES_CONTRATO <- "N"
```

- Default: **`"N"` (OFF)**.
- Mesmo padrão de `Sys.getenv(..., unset=...)` das demais opções, configurável por variável de ambiente com o mesmo nome.
- Valor inválido (fora de `"S"`/`"N"`) degrada silenciosamente para `"N"` — mesma escolha já usada na flag irmã da 03.5L-B, coerente com "não bloquear fluxo por causa de um diagnóstico".

## 3. Função criada

**`monitora_pipe_contrato_relatorio_optin(registros, contexto, output_dir, log_dir, exec_id)`**:

1. Checa a flag primeiro — retorna `invisible(NULL)` se não for `"S"` (sem ler `registros`, sem qualquer custo).
2. Chama `monitora_pipe_contrato_diagnosticar_dataset(registros, incluir_valores = FALSE, ...)` (03.5M-B) e `monitora_pipe_contrato_resumir_diagnostico_dataset()` (03.5M-B), ambas em `tryCatch` — qualquer erro vira `warning()`, nunca propaga.
3. Se a flag estiver ON e o diagnóstico tiver sucesso, grava (também em `tryCatch`) 3 arquivos em `output/diagnosticos_pipes_contrato/`:
   - `diagnostico_pipes_contrato_<contexto>_<exec_id>.csv` — 1 linha por coluna, só nomes/contagens/classificações.
   - `resumo_pipes_contrato_<contexto>_<exec_id>.csv` — mesma granularidade + `risco`/`acao_recomendada`.
   - `relatorio_pipes_contrato_<contexto>_<exec_id>.txt` — alerta legível com as 6 contagens pedidas (total de colunas, colunas com pipe, colunas com pipe por `texto_livre`/`select_multiple`/`estruturado_condicional_esparso`, colunas ambíguas/fora do contrato com pipe) + indicador booleano de "exigiria relevance/elegibilidade" + recomendação operacional (`sem_acao`/`manter_diagnostico`/`nao_migrar_resolvedor`/`avaliar_03_5M_D`).
4. Registra 1 evento informativo via `monitora_log_registrar_evento()` só quando ON (nada quando OFF).
5. Nunca altera `registros` — confirmado por desenho (só leitura de `names()`/valores de coluna para contagem) e por teste (seção 8).

Lógica de recomendação: `sem_acao` se zero pipe; `nao_migrar_resolvedor` se há pipe em `estruturado_condicional_esparso` ou em coluna ambígua/fora do contrato; `manter_diagnostico` se há pipe só em `texto_livre`/`select_multiple` (sem os casos de risco alto); `avaliar_03_5M_D` no caso residual (pipe presente, mas em nenhuma das categorias de risco reconhecidas — situação hoje não observada em nenhum teste).

## 4. Ponto de integração

Dentro do fluxo principal do script, logo após o bloco que já existia:

```r
if (!is.null(registros_corrig_pre_lido)) {
  monitora_correcao_auditar_persistencia_operacoes(
    registros_corrig_pre_lido, MONITORA_AUDITORIA_CORRECOES_CAMPOS_ULTIMA,
    ..., contexto = "pos_export_pre_analises_registros_corrig", abortar = TRUE
  )
}
```

— ou seja, **depois** que `registros_corrig.csv` já foi exportado (`monitora_publicacao_aa_exportar_registros_corrig_aprovado`/`monitora_fwrite`) e sua persistência já foi auditada com sucesso (mesmo checkpoint do Hotfix 03.5L-C), e **antes** do bloco que gera `registros_validados.csv`. A chamada nova:

```r
if (exists("monitora_pipe_contrato_relatorio_optin", mode = "function")) {
  try(monitora_pipe_contrato_relatorio_optin(
    registros_corrig, contexto = "pos_export_registros_corrig",
    output_dir = MONITORA_OUTPUT_DIR, log_dir = MONITORA_LOG_DIR, exec_id = MONITORA_EXEC_ID
  ), silent = TRUE)
}
```

está envolvida em `try(..., silent = TRUE)` (camada extra de segurança, além do `tryCatch` interno da função) e usa `registros_corrig` (o objeto em memória, não recarrega do disco) — não lê `output/`/`log/` como entrada, só escreve neles quando ON.

## 5. Confirmação: o relatório é opt-in

Confirmado por leitura estática (a chamada operacional só existe atrás de `exists(...)` + a própria função checa a flag na primeira linha) e por teste (seção 8, teste 2): com `MONITORA_DIAGNOSTICO_PIPES_CONTRATO = "N"`, a função retorna `NULL` imediatamente e **zero arquivos** são criados.

## 6. Confirmação: OFF não gera arquivos

Testado explicitamente — `list.files()` no diretório de saída após a chamada com flag OFF retorna 0 arquivos.

## 7. Confirmação: ON gera apenas produtos seguros

Testado com dataset sintético cobrindo 6 categorias (texto_livre, select_multiple, completo_por_ponto, condicional_esparso, fora_do_contrato/ambíguo) — os 3 arquivos gerados (CSV de diagnóstico, CSV de resumo, TXT de alerta) foram lidos de volta e verificados: nenhum valor distintivo do dataset sintético (`"Cipó A"`, `"abc-123"` etc.) aparece em nenhum dos 3, mesmo varrendo o conteúdo bruto de todos os arquivos gerados.

## 8. Testes realizados

Mesmo método das etapas anteriores (extração literal das funções reais para fora do repositório, `Rscript` puro, sem PNB/FNCS/pipeline).

| # | Teste | Resultado |
|---|---|---|
| 1 | `Rscript -e 'invisible(parse(...)); cat("PARSE_OK\n")'` | `PARSE_OK` |
| 2 | Sintético, flag `"N"` | Retorno `NULL`, 0 arquivos gerados |
| 3 | Sintético, flag `"S"` (6 colunas: texto_livre, select_multiple, completo_por_ponto, condicional_esparso/bromélia, ambíguo `uuid`, fora do contrato) | Retorno lista com `diagnostico`/`resumo`; 3 arquivos gerados; conteúdo do CSV e do TXT conferido — contagens corretas (5 colunas com pipe de 6, 1 em cada categoria de risco) |
| 3b | Conteúdo dos 3 arquivos varrido por valores distintivos do dataset sintético | Nenhum valor real encontrado em nenhum arquivo |
| 4 | Imutabilidade: `names()`/conteúdo do objeto antes/depois, com flag OFF e ON | Idênticos em ambos os casos |
| 5 | Leitura estática: chamada operacional atrás da flag | Confirmado — só 1 ponto de chamada no script inteiro, dentro de `try()`, com `exists()` guard, e a função checa a flag internamente antes de qualquer leitura |
| 6 | Confirmação de que não rodou PNB/FNCS/pipeline completo | Confirmado por construção — só dataset sintético em toda a sessão de teste |

Relatório TXT de exemplo (dataset sintético, não é dado real):
```
Total de colunas: 6
Colunas com pipe: 5
Colunas com pipe em texto_livre: 1
Colunas com pipe em select_multiple: 1
Colunas com pipe em estruturado_condicional_esparso: 1
Colunas com pipe ambíguas/fora do contrato: 1
Há caso que exigiria relevance/elegibilidade: SIM
Recomendação operacional: nao_migrar_resolvedor
```

## 9. Por que ainda não migrar o resolvedor operacional

Confirmado pela 03.5M-B: o único dataset real disponível para comparação (`registros_corrig.csv` da run 35lc) tem **zero pipe residual** em todas as 181 colunas — não há caso positivo real para validar a resolução por elegibilidade/relevance contra dado de produção. Migrar o resolvedor agora significaria substituir um mecanismo validado (confirmado limpo pelo Hotfix 03.5L-C) por um novo caminho ainda sem nenhuma evidência empírica positiva. O relatório opt-in agora conectado é exatamente o instrumento que, em runs futuras (especialmente uma reprodução do cenário FNCS/COLETA 11168 que motivou o Hotfix 03.5G), poderia revelar esse caso positivo sem nenhum risco — é só leitura e relatório.

## 10. Recomendação para a próxima etapa

**03.5M-D (ou 03.5M-D0) só deve ser considerada depois de uma run real com o relatório opt-in ligado (`MONITORA_DIAGNOSTICO_PIPES_CONTRATO="S"`) capturar um caso positivo de pipe residual** — idealmente reproduzindo o cenário original da COLETA 11168/FNCS. Até lá, a etapa seguinte razoável é: (a) rodar o pipeline com a flag ligada em uma próxima execução PNB/FNCS já planejada por outros motivos (não rodar uma run só para isso), e (b) se a recomendação automática do relatório indicar `nao_migrar_resolvedor` ou `manter_diagnostico`, permanecer nesse modo; só evoluir para uma etapa de migração real se e quando o relatório mostrar pipe residual isolado em `select_multiple`/`estruturado_completo_por_ponto` sem nenhum caso de `estruturado_condicional_esparso`/ambíguo — o que ainda não aconteceu em nenhuma run observada.

## 11. Confirmação: warning do relatório consolidado fora de escopo

Nenhuma evidência encontrada nesta etapa que relacione o warning de `data.table::unique` do relatório consolidado de validação a pipes/relevance/cardinalidade — não investigado, não tocado.

## 12. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 146 ++++++++++++++++++++++++++++++++++
 1 file changed, 146 insertions(+)
```

146 inserções, **0 remoções** — confirmado puramente aditivo. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_produtos_classificar_pipe_coluna`, `monitora_pipe_contrato_diagnosticar_dataset` e `monitora_pipe_contrato_resumir_diagnostico_dataset` não aparecem no diff (exceto como contexto de hunk não modificado, no ponto de inserção da função nova).

## 13. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/dev_035m_c_relatorio_pipes_contrato/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
