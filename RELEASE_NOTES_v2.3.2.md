# v2.3.2 - Exclusão de COLETAS sem vestígios e painel legível

Publicação pública `v2.3.2` do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Corrige a exclusão auditável de COLETAS filtradas/selecionadas no painel.
- A exclusão de COLETAS passa a remover integralmente as linhas das COLETAS alvo, sem tentar editar a coluna estrutural `COLETA`.
- Inclui auditoria pós-exclusão para impedir publicação de `registros_corrig.csv` com vestígios das COLETAS excluídas.
- Oculta `uuid`/amostragem-registro apenas nas tabelas do painel para melhorar a leitura, preservando o campo em dados, operações, logs, auditorias e arquivos finais.
- Mantém `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` como padrão inicial.
- Revisa os comentários do script público para remover menções residuais a versões anteriores como referência corrente, incluindo a menção indevida à `v2.3.0` observada no script anterior.

## Validação operacional esperada

- Execução sem painel deve concluir normalmente.
- Painel deve abrir quando `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"`.
- Exclusão de COLETAS em lote deve gerar operações auditáveis por COLETA.
- Após salvar e continuar, `output/registros_corrig.csv` não deve conter linhas das COLETAS excluídas, nem linhas residuais com número da COLETA e demais campos vazios.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.3.2.R`
- `monitora_campsav_alvo_global.R`
- `MONITORA_CAMPSAV_Alvo_Global.R`
- `R/monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
