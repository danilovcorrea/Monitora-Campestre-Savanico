# Hotfix 01 — Correção do mapeamento "Outras plantas..." → outra_forma_vida

## Metadados

| Campo | Valor |
|---|---|
| Data/hora | 2026-07-02, 16:03–16:08 (-03:00) |
| Worktree | `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260` |
| Branch | `dev-v2.6.2-rollforward-golden-v260` |
| Commit antes da alteração | `482347b68f935e9938da957997d14103dabf7cf0` (tag `v2.6.0`) |
| Commit depois | nenhum — **alteração ainda não commitada**, apenas working tree |
| Arquivo alterado | `monitora_campsav_alvo_global_v2.6.0.R` |
| Linhas alteradas | 32940–32941 |

## Diff aplicado

```diff
-      "Outras plantas terrestres, líquens e/ou fungos" = "solo_nu",
-      "Outras plantas terrestres, líquens e/ou fungos," = "solo_nu"
+      "Outras plantas terrestres, líquens e/ou fungos" = "outra_forma_vida",
+      "Outras plantas terrestres, líquens e/ou fungos," = "outra_forma_vida"
```

Duas linhas alteradas, nenhuma outra linha tocada. Nenhuma refatoração, nenhuma unificação de motor, nenhuma alteração de default, README ou CHANGELOG.

## Justificativa contratual (XLSForm 2025)

- Campo Encostam (`amostragem/registro/tipo_forma_vida`), tokens válidos: `solo_nu, serrapilheira, nativa, exotica, seca_morta, outra_forma_vida`.
- A label `"Outras plantas terrestres, líquens e/ou fungos"` corresponde, na aba `choices` do XLSForm 21FEV25, exclusivamente ao token `outra_forma_vida` — nunca a `solo_nu` (`solo_nu` = "Solo nu / rochas (sem plantas tocando a vareta)", uma resposta semanticamente distinta e mutuamente exclusiva pela constraint `if(selected(.,'solo_nu'), count-selected(.)=1, count-selected(.)>=1)`).
- O mapeamento anterior violava essa constraint: uma coleta com "Plantas nativas" + "Outras plantas..." selecionadas simultaneamente seria corrompida para `"nativa,solo_nu"`, quebrando a exclusividade de `solo_nu` exigida pelo próprio XLSForm.
- A correção alinha esta linha às outras duas implementações corretas já existentes no mesmo script (`monitora_correcao_calcular_tipo_forma_vida_esperado`/`sincronizar_encostam_final`, linhas 8733–8790; `monitora_validados_aplicar_regras_xlsform21`, linha 28186), que já tratavam esse mapeamento corretamente — eliminando a contradição interna identificada na auditoria (`diagnostics/xlsform_2025_contract_audit/AUDITORIA_CONTRATO_XLSFORM_2025.md`, item 3).

## Checagem mínima do mapeamento literal

Extraído verbatim o bloco de mapeamento das linhas 32929–32942 do arquivo já corrigido (sem transcrição manual) e aplicado com `stringr::str_replace_all` a um vetor de teste isolado, sem depender do pipeline completo:

| Entrada | Saída | Esperado |
|---|---|---|
| `Outras plantas terrestres, líquens e/ou fungos` | `outra_forma_vida` | ✅ |
| `Outras plantas terrestres, líquens e/ou fungos,` | `outra_forma_vida,` | ✅ |
| `Plantas nativas,Outras plantas terrestres, líquens e/ou fungos` | `nativa,outra_forma_vida` | ✅ (sem `solo_nu` residual) |
| `Solo nu / rochas (sem plantas tocando a vareta)` | `solo_nu` | ✅ (regra de solo_nu isolado preservada) |

Asserções (`stopifnot`) confirmaram: resultado da resposta literal == `outra_forma_vida`; nenhuma ocorrência de `solo_nu` no resultado; `solo_nu` isolado continua mapeando corretamente para `solo_nu`. **CHECAGEM MÍNIMA: PASSOU.**

## Reexecução da baseline PNB

Comando (idêntico ao usado na baseline original, pasta de execução limpa fora do worktree, dados brutos intocados):

```bash
MONITORA_MODO_EXECUCAO=ate_registros_corrig \
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS=S \
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S \
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=S \
/usr/bin/Rscript monitora_campsav_alvo_global_v2.6.0.R
```

Executado em `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260_execucao_pnb` (mesma pasta de execução limpa da baseline original; script substituído pela versão com o hotfix, hash verificado idêntico ao worktree; `input/pnb_2022_2026.zip` **intocado**, hash SHA-256 confirmado igual ao original).

## Contagens golden — antes vs. depois do hotfix

| Métrica | Antes (baseline original) | Depois (com hotfix) | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606 linhas / 6 coletas | 606 linhas / 6 coletas | ✅ inalterado |
| Nativa sem forma de vida | 3 linhas / 3 coletas | 3 linhas / 3 coletas | ✅ inalterado |
| Seca/morta sem forma de vida | 10 linhas / 8 coletas | 10 linhas / 8 coletas | ✅ inalterado |
| Outra forma de vida legada | 7 linhas / 5 coletas | 7 linhas / 5 coletas | ✅ inalterado |
| Colunas `registros_importados.csv` | 176 | 176 | ✅ inalterado |
| Colunas `registros_corrig.csv` | 180 | 180 | ✅ inalterado (não inchou para 256) |
| Tamanho `registros_importados_bruto.csv` | 31.864.856 bytes | 31.864.856 bytes | ✅ idêntico |
| Tamanho `registros_importados.csv` | 39.469.973 bytes | 39.469.973 bytes | ✅ idêntico |
| Tamanho `registros_corrig.csv` | 40.805.162 bytes | 40.805.162 bytes | ✅ idêntico |
| Tempo núcleo (`dec:` em "fim execucao") | 51,2 s | 51,1 s | ✅ equivalente |
| `registros_importados.csv` sobrescrito por `registros_corrig.csv`? | Não (hashes/timestamps distintos) | Não (hashes/timestamps distintos, ordem bruto→importados→corrig preservada) | ✅ |
| Warning pré-existente (`data.table::unique`) | presente | presente (inalterado, não relacionado a este hotfix) | — |

Nenhuma regressão detectada. O dataset PNB não contém a resposta literal corrigida, portanto o resultado byte-idêntico dos três produtos de dados era o esperado — o hotfix é comprovadamente neutro para este dataset e correto para o caso antes ausente da baseline (validado pela checagem mínima isolada).

## Conclusão

**PASSOU.**

A correção cirúrgica (2 linhas) elimina a contradição contratual crítica identificada na auditoria XLSForm 2025 sem alterar nenhum resultado da baseline PNB golden já aprovada. A checagem mínima isolada confirma que a resposta "Outras plantas terrestres, líquens e/ou fungos" agora mapeia corretamente para `outra_forma_vida`, inclusive quando combinada com outra categoria (`nativa`), sem introduzir `solo_nu` espúrio.

## Pendências não tratadas nesta etapa (propositalmente fora de escopo)

- Unificação dos múltiplos motores de validação row-level (item 15 da auditoria) — não tocado.
- Proteção de `registros_corrig.csv` por validação row-level equivalente à do motor `monitora_validados_*` (exclusividade de `solo_nu`, obrigatoriedade de `forma_vida_outros`/`forma_serrapilheira`) — não tocado.
- Relatório específico de introdução automática de `solo_nu` quando Encostam fica vazio após correções — não tocado, ainda ausente.
- Warning `'unique' is not an exported object from 'namespace:data.table'` no gerador de relatório consolidado de validação — não tocado, continua presente e não relacionado a este hotfix.

## Status git

Nenhum commit criado. `git status --short` mostra apenas a modificação do arquivo do script (`M monitora_campsav_alvo_global_v2.6.0.R`) mais os artefatos não rastreados já existentes de etapas anteriores (`diagnostics/`, `docs/manual_usuario_v2.6.0.*`, `referencias_contrato_2025/`).
