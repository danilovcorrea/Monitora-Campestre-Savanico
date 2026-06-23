# v2.4.2 - Integridade de entrada, datas e correções assistidas

Publicação pública v2.4.2 do script `monitora_campsav_alvo_global.R`.

## Principais alterações

- Corrige o recálculo de `DATA_MONITORA_PARSEADA` e `ANO` após alteração auditável de `Data (data_hora)`.
- Autoriza, na auditoria de colunas protegidas, apenas recálculos coerentes de derivados temporais.
- Corrige o mapeamento de aliases de data: `data_do_registro`, `DATA DO REGISTRO` e equivalentes deixam de ser usados como `Data (data_hora)`.
- Mantém `data_do_registro`, `data_do_recebimento` e `ultima_edicao` como metadados protegidos.
- Fortalece o controle de entrada no painel de correções assistidas com base nos metadados XLSForm/SISMONITORA.
- Restringe operações de token a atributos compatíveis com listas.
- Bloqueia edição direta de atributos derivados/protegidos, como `ANO`, `DATA_MONITORA_PARSEADA` e `num_placa_formatado`.
- Permite exclusão auditável de uma única COLETA no painel, além de lotes de COLETAS.
- Atualiza automaticamente o valor original esperado ao mudar COLETA, filtros superiores, atributo, escopo ou ponto.
- Preserva a lógica de operações atômicas e o uso de `data.table` para manter performance.

## Observações operacionais

A opção padrão de publicação mantém:

    MONITORA_MODO_EXECUCAO <- "completo"
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"

Para abrir o painel de correções assistidas, alterar manualmente `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` em cópia local de execução.
