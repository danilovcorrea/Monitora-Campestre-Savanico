# Hotfix 03.5L-C — Falsa trava de persistência bloqueava o checkpoint `registros_corrig.csv`

## 1. Resumo executivo

A run PNB pós-03.5L-B (`/home/dfed/Desktop/03jul_dev/35lb`) abortou antes de materializar `registros_corrig.csv` com `Error: Correções auditadas não persistiram em registros_corrig (pos_aplicacao_objeto)`. A causa **não é a 03.5L-B** (o diagnóstico opt-in não participa desse caminho de código) — é um critério **global demais** dentro do reclassificador de falsos-positivos de persistência já existente (`monitora_persist_reclassificar_por_efeito_diagnostico_final`), que só confirma o efeito de uma operação semântica quando a **triagem diagnóstica de todo o objeto** não tem nenhuma ocorrência impeditiva, em vez de olhar o efeito da própria operação. A operação `TRIOUT_ATOM_20260703184835_1180` (limpeza de outras formas de vida) já tinha, pela sua própria auditoria, `residuos_depois=0` e `pendencias_manuais=0` — mas 12 linhas impeditivas remanescentes de **outras categorias não relacionadas** (nativa/seca-morta sem forma de vida) impediram a reclassificação, deixando 5 checagens como falha real e abortando antes do checkpoint.

Corrigido com uma nova camada de reclassificação **específica para a família `TRIOUT`**, que usa só o efeito da própria operação (via a linha-sentinela que `monitora_correcao_aplicar_limpeza_outras_formas_atomica()` já grava no `audit`), rodando antes do reclassificador global existente — que continua intocado e continua servindo de segunda tentativa para as demais famílias (`PENDFV`/`MVLOTE`/`TRIDESC`/`MOVFV`/etc.) e como rede de segurança se a sentinela específica não existir.

## 2. Causa da falha na run 35lb

| Evidência | Valor |
|---|---|
| Operação | `TRIOUT_ATOM_20260703184835_1180` (`limpeza_outras_formas_atomica`) |
| Aplicação da operação | `aplicada`, mensagem própria: `residuos_antes=6; residuos_depois=0; pendencias_manuais=0` |
| Checagens de persistência da operação | 21 total, **5 falhas** (`falha_valor_nao_persistiu`) |
| Atributo das 5 falhas | `"Formas de vida de plantas nativas: (amostragem/registro)"` (campo superior recalculado) |
| `modo_comparacao` das 5 falhas | `falha_estado_final_contratual_normalizado` |
| Mensagem literal | "...Triagem diagnóstica final ainda indica 12 linha(s) impeditiva(s)." |
| Total da run | 381 checagens: 369 `ok_aplicacao_persistiu`, 6 `ok_exclusao_persistiu`, 1 `ok_estado_final_contratual_consistente`, **5 `falha_valor_nao_persistiu`** |

A mensagem confirma exatamente o mecanismo: o reclassificador (`monitora_persist_reclassificar_por_efeito_diagnostico_final`, L6929 antes do hotfix) roda `monitora_diag_rel_gerar_ocorrencias()` sobre o **objeto inteiro** e só reclassifica quando `n_linhas_imp == 0L` **globalmente**. As 12 linhas impeditivas eram de categorias fora do escopo da limpeza de outras formas (confirmadas no log do painel: `nativa_sem_forma_vida=3`, `seca_morta_sem_forma_vida=7-9` — nada relacionado a `outra_forma_vida`), então a condição global nunca foi satisfeita, mesmo com o efeito próprio da operação já comprovadamente zerado.

## 3. Evidências dos arquivos de auditoria

Todos os 3 arquivos pedidos existiam em `/home/dfed/Desktop/03jul_dev/35lb/output/correcoes_campos/` e foram lidos (só estrutura/contagens, sem reproduzir valores de célula/COLETA sensíveis neste relatório):

- `auditoria_persistencia_correcoes_pos_aplicacao_objeto_ultima_execucao.csv` — 381 linhas; distribuição de `status_persistencia` confirmada acima; as 5 falhas têm `id_correcao == "TRIOUT_ATOM_20260703184835_1180"`, `atributo` sempre o campo superior de forma de vida nativa, `modo_comparacao == "falha_estado_final_contratual_normalizado"`.
- `auditoria_operacoes_atomicas_resumo_pos_aplicacao_objeto_ultima_execucao.csv` — 4 linhas (uma por operação atômica da sessão: `EXCCOL`, `RECALC_SUPERIORES_XLSFORM`, `SYNC_ENCOSTAM_FINAL`, `TRIOUT_ATOM`); só a linha `TRIOUT_ATOM` tem `status_persistencia_operacao == "falha"` (`n_checagens_persistencia=21`, `n_falhas_persistencia=5`); a mensagem consolidada confirma `residuos_antes=6; residuos_depois=0; pendencias_manuais=0` — o próprio resumo já mostrava a operação como estruturalmente bem-sucedida.
- `correcoes_semanticas_ultima_execucao.csv` — 7 itens auditáveis (confirma a sessão do painel: exclusão de 6 COLETAS + limpeza de outras formas), consistente com o console log (`2 operação(ões) semântica(s), 7 item(ns) auditável(is)`).
- `console_35lb_error.txt` — confirma a sequência completa: painel fechado com pendências reconhecidas explicitamente pelo usuário → aplicação das 2 operações semânticas → `TRIOUT` aplicado com sucesso (`[PERF] painel_correcoes_triout_1_de_1`) → abort no passo seguinte de persistência, antes de qualquer materialização de `registros_corrig.csv`.

Também observado (não bloqueante, não corrigido nesta etapa por estar fora do escopo do hotfix pedido): o console mostra um `AVISO: Organização do output deixou itens não permitidos na raiz: ... diretorios=diagnosticos_contrato_unico_registros_importados` — é só um aviso cosmético da organização de output sobre a pasta nova da 03.5L-B, não impede nada e não está relacionado à causa do abort. Registrado aqui para not ser confundido com a causa real.

## 4. Função/trecho corrigido

Arquivo: `monitora_campsav_alvo_global_v2.6.0.R`, dentro de `monitora_correcao_auditar_persistencia_operacoes()` (função com a trava, L6523), imediatamente antes de `monitora_persist_reclassificar_por_efeito_diagnostico_final` (L6929 antes do hotfix).

**Nova função local** `monitora_persist_reclassificar_triout_por_efeito_proprio(res, audit, contexto_estado)`:

- Filtra só falhas (`falha_valor_nao_persistiu`/`falha_linha_nao_localizada`) cujo `id_correcao` bate `^TRIOUT`.
- Busca, dentro do `audit` já passado para a função-mãe (mesmo objeto, sem nenhuma leitura nova), a linha-sentinela que `monitora_correcao_aplicar_limpeza_outras_formas_atomica()` já grava (`atributo == "__limpar_outras_formas_vida__"`), para o mesmo `id_correcao`.
- **Só reclassifica se TODAS as linhas-sentinela dessa operação tiverem `status == "auditoria_ok"`** (residuos_depois=0 e pendencias_manuais=0, calculados pela própria função aplicadora, não recalculado aqui).
- Reclassifica para `status_persistencia = "ok_persistiu_por_efeito_especifico_outras_formas"`, com a mensagem exigida: *"Limpeza de outras formas persistiu por efeito específico; pendências impeditivas globais remanescentes não relacionadas permanecem para checkpoint de registros_corrig."* (mais o texto da sentinela própria, para rastreabilidade).
- Se não houver sentinela, ou a sentinela indicar resíduo/pendência real (`status != "auditoria_ok"`), **não reclassifica nada** — a falha segue para o reclassificador global existente como segunda tentativa e, se este também não resolver, continua bloqueando normalmente (comportamento inalterado).

**Chamada adicionada**: `res <- monitora_persist_reclassificar_triout_por_efeito_proprio(res, audit, contexto_estado = contexto)`, logo antes da chamada já existente a `monitora_persist_reclassificar_por_efeito_diagnostico_final`.

Nenhuma outra função foi alterada.

## 5. Justificativa de segurança

- **Critério não depende de zerar pendências globais**: usa exclusivamente o resultado já calculado pela própria aplicação da operação (`residuos_depois`, `pendencias_manuais`), a mesma fonte que já aparece em `auditoria_operacoes_atomicas_resumo_*.csv` — nenhum recálculo novo, nenhuma heurística nova.
- **Exige unanimidade, não maioria**: `all(status == "auditoria_ok")` sobre todas as linhas-sentinela da operação — uma única sentinela com resíduo real impede a reclassificação inteira.
- **Sem sentinela = sem reclassificação**: se o `audit` não tiver a linha-sentinela (ex.: replay de sessão antiga sem essa auditoria própria), a função não tenta adivinhar — a falha permanece e cai no caminho já existente (reclassificador global, depois bloqueio).
- **Escopo estritamente por `id_correcao`**: só afeta as linhas de `res` cujo `id_correcao` já falhou E começa com `TRIOUT` — nenhuma outra família, nenhuma outra operação, nenhuma linha de outro tipo é tocada (confirmado no teste 5, cenário misto).
- **Não mexe na auditoria de exclusão de COLETAS**: `ok_exclusao_persistiu`/`falha_exclusao_nao_persistiu` é gerado por um bloco totalmente separado (L6568) e nunca examinado por esta função (confirmado no teste 4).
- **Roda antes do reclassificador global, não o substitui**: `monitora_persist_reclassificar_por_efeito_diagnostico_final` continua exatamente como estava, disponível como segunda tentativa para as famílias que legitimamente precisam do critério global (PENDFV/MVLOTE/TRIDESC/MOVFV/RECALC_SUPERIORES_XLSFORM/SYNC_ENCOSTAM_FINAL, cujo efeito depende de recálculo cruzado entre categorias, ao contrário de TRIOUT).

## 6. Por que isso não libera `registros_validados.csv`

Este hotfix altera só a trava de **persistência de correções em `registros_corrig` (checkpoint)** — a função `monitora_correcao_auditar_persistencia_operacoes()`. A geração de `registros_validados.csv` é controlada por um mecanismo completamente separado (flag `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS`/`MONITORA_GERAR_REGISTROS_VALIDADOS`, subsistema `monitora_validados_*`), que continua consultando as pendências impeditivas do estado final normalmente — nenhum código desse caminho foi tocado. As 12 linhas impeditivas não relacionadas (nativa/seca-morta sem forma de vida) continuam existindo no objeto e continuam sendo reportadas e bloqueando o que já bloqueavam antes; só deixaram de bloquear indevidamente a materialização de `registros_corrig.csv` por causa de uma operação que já tinha, ela mesma, zerado seu próprio resíduo.

## 7. Por que isso não mexe em pipes/relevance/cardinalidade

O hotfix está inteiramente dentro do subsistema de auditoria de persistência de correções do painel (`monitora_correcao_auditar_persistencia_operacoes` e vizinhos, arquivo original em torno da L6500-7050). `monitora_produtos_resolver_pipes_por_ponto` e `monitora_bloquear_pipe_residual_produto` (os pontos explicitamente proibidos de tocar) pertencem a um subsistema diferente (resolução/bloqueio de resíduo de pipe em `registros_importados.csv`/produtos finais) e não aparecem em nenhum lugar do código novo — confirmado por `git diff` (seção 9) e pelos testes (nenhum deles carrega ou invoca essas funções).

## 8. Testes realizados e resultado

Isolamento: a nova função é *self-contained* (só usa `res`/`audit`/`contexto_estado`, sem depender de nenhuma variável do escopo da função-mãe), extraída literalmente do script real (mesmas linhas, sem reescrever nada) para teste fora do repositório via `Rscript` puro. Nenhum dado real foi usado — só sintético construído a partir da estrutura já observada nos CSVs da run 35lb (nomes de coluna e valores de exemplo genéricos, não os dados reais da run).

| # | Teste | Resultado |
|---|---|---|
| 1 | `Rscript -e 'invisible(parse("monitora_campsav_alvo_global_v2.6.0.R")); cat("PARSE_OK\n")'` | `PARSE_OK` |
| 2 | Sintético positivo: TRIOUT com 2 falhas `falha_valor_nao_persistiu` + sentinela própria `auditoria_ok` (residuos_depois=0, pendencias_manuais=0) | Ambas reclassificadas para `ok_persistiu_por_efeito_especifico_outras_formas`; **0 falhas remanescentes** — não abortaria |
| 3 | Negativo: mesma estrutura, mas sentinela própria com `falha_residuo_pos_limpeza` (resíduo/pendência reais) | **Continua bloqueando** — 2 falhas remanescentes, nenhuma reclassificação |
| 3b | Negativo: TRIOUT sem nenhuma linha-sentinela no `audit` | **Continua bloqueando** — não reclassifica às cegas |
| 4 | Exclusão de COLETA (`EXCCOL`, `falha_exclusao_nao_persistiu`) | **Inalterado** — a função nem examina, pois o id não começa com `TRIOUT` |
| 5 | Cenário misto: 2 falhas TRIOUT (efeito próprio OK) + 1 falha de outra família (`PENDFV`, não relacionada) | TRIOUT reclassificado; **a falha do `PENDFV` permanece bloqueando** — confirma que o escopo é estritamente por operação, não libera tudo |
| 6 | Flag 03.5L-B default `"N"` no script principal versionado | Confirmado: `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "N"` (L205) |

## 9. Confirmação da flag 03.5L-B

- **Arquivo principal versionado** (`monitora_campsav_alvo_global_v2.6.0.R`, tracked): `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "N"` — já estava correto, **nenhuma correção foi necessária**.
- **Arquivo de run usado pelo usuário** (`/home/dfed/Desktop/03jul_dev/35lb/monitora_campsav_alvo_global_v2.6.0_03.5L-B_PNB_FLAG_OFF.R`): estava com `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS <- "S"` — **deliberado** para aquela run de teste (junto com `MONITORA_MODO_EXECUCAO="painel_e_parar"`, `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS="S"`, `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS="S"`, `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS="S"` — um conjunto de flags de teste amplo, não um erro isolado). Esta cópia não faz parte do repositório e não foi alterada. Uma nova run de validação deve partir do script versionado (flag `"N"`) e, se o objetivo for validar o diagnóstico opt-in especificamente, ligar só essa flag.
- Também existe uma cópia idêntica ao script versionado (`monitora_campsav_alvo_global_v2.6.0_03.5L-B_PNB_FLAG_OFF.R`, untracked) na raiz do repositório — já com `"N"`, sem diferenças em relação ao script principal; não foi tocada.

## 10. Necessidade de nova run PNB

**Sim, necessária** — mas só depois deste hotfix ser revisado (e, quando o usuário decidir, commitado). A nova run deve:
1. Usar o script versionado com a flag da 03.5L-B em `"N"` (padrão) para validar que o hotfix por si só resolve o abort sem nenhum outro efeito colateral.
2. Reproduzir a mesma sessão de painel (exclusão das 6 COLETAS + limpeza de outras formas) para confirmar que `registros_corrig.csv` agora é materializado como checkpoint (com as pendências impeditivas não relacionadas ainda relatadas, mas não bloqueando o checkpoint).
3. Confirmar que `registros_validados.csv` continua **não** sendo gerado enquanto as pendências impeditivas (nativa/seca-morta sem forma de vida) não forem resolvidas — validando a seção 6 acima.
4. Não é uma run que este agente deva executar — é do usuário, no RStudio, conforme já estabelecido nas etapas anteriores.

## 11. `git diff --stat`

```
$ git diff --stat -- monitora_campsav_alvo_global_v2.6.0.R
 monitora_campsav_alvo_global_v2.6.0.R | 73 +++++++++++++++++++++++++++++++++++
 1 file changed, 73 insertions(+)
```

73 inserções, **0 remoções** — confirmado puramente aditivo. `monitora_produtos_resolver_pipes_por_ponto`, `monitora_bloquear_pipe_residual_produto`, `monitora_persist_reclassificar_estado_contratual` e `monitora_persist_reclassificar_por_efeito_diagnostico_final` não aparecem no diff (só como contexto de hunk não modificado).

## 12. `git status --short`

```
?? monitora_campsav_alvo_global_v2.6.0_03.5L-B_PNB_FLAG_OFF.R
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035l_c_registros_corrig_checkpoint/
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-B_PNB_FLAG_OFF.R` já estava presente (untracked) na raiz do repositório antes desta tarefa — não foi criado nem alterado por este hotfix.

Nesta tarefa, nenhum `git add`, `commit` ou `push` foi executado.
