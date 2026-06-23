# v2.4.2 - Integridade de entrada, datas e correções assistidas

Publicação pública corrigida v2.4.2 do script `monitora_campsav_alvo_global.R`.

## Natureza desta publicação

Esta publicação substitui a primeira tentativa de publicação de v2.4.2, removida por sanitização incompleta dos arquivos públicos. A versão funcional validada é mantida como v2.4.2, agora com todas as cópias públicas, documentação, SHA256 e pacote ZIP sincronizados.

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
- Sincroniza `monitora_campsav_alvo_global.R`, `monitora_campsav_alvo_global_v2.4.2.R`, `MONITORA_CAMPSAV_Alvo_Global.R`, `R/monitora_campsav_alvo_global.R` e `R_monitora_campsav_alvo_global.R`.
- Preserva a lógica de operações atômicas e o uso de `data.table` para manter performance.

## Padrão público

    MONITORA_MODO_EXECUCAO <- "completo"
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
    MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
