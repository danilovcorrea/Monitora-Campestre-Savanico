# HOTFIX 035N-B - auditoria acumulativa de registros importados

Data: 2026-07-04
Modo: hotfix observacional/estatico, sem commit, sem push, sem PNB/FNCS/pipeline pesado.

## Motivacao

A auditoria arquitetural `44d0c93ae14d` apontou que `auditoria_registros_importados_resumo.csv` era gravada por multiplas camadas/produtos usando o mesmo nome. As escritas diretas sobrescreviam contexto anterior e permitiam perda de historico entre:

- `registros_importados_bruto.csv`
- `registros_importados.csv`
- `registros_importados_operacional_pre_painel.csv`
- contexto de retomada por `registros_corrig.csv`, no qual `registros_importados.csv` e produto nao aplicavel

## Intervencao

Foi adicionado o helper unico `monitora_registros_importados_resumo_auditoria_gravar()`.

Comportamento do helper:

- recebe `resumo`, `produto`, `contexto` e `camada`;
- adiciona `produto`, `contexto` e `camada` quando ausentes;
- le o resumo existente, quando houver;
- concatena historico anterior e nova linha com `data.table::rbindlist(..., fill = TRUE, use.names = TRUE)`;
- grava `auditoria_registros_importados_resumo.csv` de forma acumulativa;
- aplica a mesma politica acumulativa ao arquivo por execucao em `log_dir`.

## Pontos Afetados

As escritas diretas em `monitora_campsav_alvo_global_v2.6.0.R` foram substituidas por chamadas ao helper nos pontos que constroem resumo de:

- exportacao bruta de `registros_importados_bruto.csv`;
- exportacao operacional de `registros_importados.csv`;
- auditoria de nao aplicavel em modo retomada por `registros_corrig.csv`;
- redefinicao viva posterior de `monitora_registros_importados_exportar()`;
- redefinicao viva posterior de `monitora_registros_importados_saneado_exportar()`, preservando `produto_nome` para `registros_importados_operacional_pre_painel.csv`.

Nao houve alteracao nos produtos de dados nem na logica dos exportadores.

## Validacoes

Validacoes estaticas executadas:

- `rg "auditoria_registros_importados_resumo\\.csv" monitora_campsav_alvo_global_v2.6.0.R`
- `rg "produto_linha\\(\"registros_importados_operacional_pre_painel\\.csv\"" monitora_campsav_alvo_global_v2.6.0.R`
- `rg "produto_nome\\s*=\\s*\"registros_importados_operacional_pre_painel\\.csv\"" monitora_campsav_alvo_global_v2.6.0.R`
- `rg "monitora_auditar_produtos_finais|reparar_csv_objeto\\(\"registros_corrig|reparar_csv_objeto\\(\"registros_corrig_stat" monitora_campsav_alvo_global_v2.6.0.R`

Resultados observados antes das validacoes finais:

- somente o helper grava fisicamente `auditoria_registros_importados_resumo.csv`;
- chamadas restantes ao helper sao esperadas;
- H2R-C segue com uma ocorrencia de `produto_linha("registros_importados_operacional_pre_painel.csv"` e com `produto_nome = "registros_importados_operacional_pre_painel.csv"`;
- 035N-A segue sem chamadas ativas de reparo tardio para `registros_corrig` ou `registros_corrig_stat` em `monitora_auditar_produtos_finais()`.

## Riscos Remanescentes

- O CSV consolidado segue compartilhado por camadas; a protecao agora e acumulativa por schema/contexto, mas ainda depende de todos os novos pontos futuros usarem o helper.
- O historico pode acumular entradas repetidas se a mesma execucao for reprocessada com o mesmo `exec_id`.
- O contrato ideal continua sendo motor unico/contrato unico de publicacao e auditoria de produtos, reduzindo redefinicoes vivas e duplicidade historica de exportadores.
