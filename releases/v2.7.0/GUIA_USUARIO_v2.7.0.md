# Guia do usuário — v2.7.0

## Preparação

1. Crie uma pasta de execução vazia.
2. Copie `monitora_campsav_alvo_global_v2.7.0.R` para essa pasta.
3. Crie o subdiretório `input/`.
4. Coloque em `input/` somente os arquivos que devem integrar a execução.
5. Preserve uma cópia independente dos arquivos originais.

O script aceita ZIPs de download direto do SISMONITORA, planilhas e arquivos tabulares reconhecidos. Não é necessário extrair ZIPs manualmente.

## Execução completa sem painel

Mantenha os padrões publicados:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
```

Execute o script. Se houver pendências impeditivas, `registros_validados.csv` não será criado; use os relatórios detalhados para localizar cada ocorrência.

## Execução com correções assistidas

Altere:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

No painel:

1. Examine as ocorrências impeditivas e as outras ocorrências para revisão.
2. Selecione o escopo exato da operação: ponto, coleta ou lote de coletas.
3. Adicione a correção à fila.
4. Atualize a prévia integral quando desejar conferir o estado reconciliado.
5. Use **Fechar, checkpoint e salvar** para aplicar as operações e prosseguir.

Uma atualização integral já concluída não precisa ser repetida no fechamento quando nenhuma operação nova tiver sido adicionada.

## Produtos que devem ser conferidos

- `output/01_produtos_dados/registros_corrig.csv`
- `output/01_produtos_dados/registros_validados.csv`, quando aprovado pelo gate
- `output/02_painel_correcoes/ocorrencias_diagnosticas/pos_painel/`
- `output/02_painel_correcoes/linhagem/`
- `output/03_auditorias/`
- `output/03_auditorias/relatorios_validacao/`
- `log/`

O relatório detalhado pós-painel informa coleta, ponto amostral, atributo, valor, regra e motivo das ocorrências que permanecem.

## Continuidade incremental

Para continuar uma curadoria anterior:

1. Crie uma nova pasta limpa.
2. Coloque em `input/` um único `registros_corrig*.csv` produzido pelo script.
3. Copie a pasta de linhagem correspondente para `input/linhagem/`.
4. Use um modo `painel_incremental_*`.
5. Mantenha `MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"`.

Não separe o produto corrigido de sua linhagem e não misture arquivos provenientes de execuções diferentes.

## Replay semântico

O replay reconstrói as decisões sobre os mesmos arquivos brutos. Ele não é continuidade incremental.

1. Coloque em `input/` os arquivos brutos originais.
2. Copie o ledger para `input/linhagem/correcoes_semanticas_consolidada.csv`.
3. Não inclua `registros_corrig.csv`.
4. Ative `MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"`.

O script bloqueia replay sem ledger ou replay combinado a um checkpoint já corrigido.

## Dados sensíveis

Não publique pastas de execução. `input/`, `output/`, `log/` e `extracted/` podem conter dados pessoais, coordenadas, fotos, UUIDs e observações de campo.

## Em caso de bloqueio

Não edite silenciosamente o produto final para contornar o gate. Consulte as ocorrências diagnósticas, o relatório detalhado de rejeições, as auditorias do contrato e o log da execução; corrija a causa no input ou por operação auditável.
