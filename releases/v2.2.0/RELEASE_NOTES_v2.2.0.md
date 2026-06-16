# Release v2.2.0

Versão pública posterior à `v2.1.3`, com inclusão do **Painel de validação - correções assistidas de `registros_corrig`** e manutenção dos produtos analíticos consolidados na série `v2.1.x`.

## Arquivo principal

```text
monitora_campsav_alvo_global_v2.2.0.R
```

## Cópias sincronizadas

```text
monitora_campsav_alvo_global_v2.2.0.R
monitora_campsav_alvo_global.R
MONITORA_CAMPSAV_Alvo_Global.R
R/monitora_campsav_alvo_global.R
releases/v2.2.0/monitora_campsav_alvo_global_v2.2.0.R
```

## Destaques

- Painel Shiny opcional para validação assistida de `registros_corrig`.
- Correções auditáveis em `input/correcoes_campos.csv`.
- Triagem e correção assistida de formas de vida exóticas.
- Movimento assistido de forma de vida entre categorias.
- Metadados de XLSForms históricos e atual embutidos no script para controle de entrada e campos condicionais.
- Preservação do fluxo analítico padrão quando o painel está desativado.

## Uso do painel

No início do script, altere:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

para:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

## Validação recomendada

1. Executar sem painel (`"N"`).
2. Executar com painel (`"S"`).
3. Testar correção simples/lote.
4. Testar movimento exótica → nativa.
5. Conferir `output/registros_corrig.csv` e `output/correcoes_campos/auditoria_correcoes_campos_ultima_execucao.csv`.
6. Conferir SHA256 das cinco cópias públicas do script.
