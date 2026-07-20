# v2.7.1 — Auditorias versionadas e cobertura integral de CPF

A versão `v2.7.1` é uma atualização corretiva localizada sobre a `v2.7.0`. Ela preserva o painel, os atributos editáveis, a fila, os resolvedores operacionais, as operações semânticas e os 13 modos públicos, alterando somente duas auditorias que podiam produzir resultados incompletos ou incompatíveis com a versão de origem dos dados.

## Correções

- A auditoria de dependências condicionais passa a usar a regra de hábito aplicável à versão do XLSForm de cada linha. Para samambaia, a ausência de hábito permanece válida nos registros históricos de 2022–2024 e é impeditiva a partir do XLSForm 2025.
- O relatório de normalização de CPF passa a conservar todas as ocorrências inválidas detectadas antes das operações semânticas. Quando uma linha é removida por exclusão auditável da coleta, a ocorrência permanece relatada como subsumida pela exclusão, sem reaparecer nos produtos de dados.

## Comportamento preservado

- Nenhuma alteração na interface do painel, na lista de atributos a corrigir, na apresentação da fila ou nas rotinas semânticas.
- Correções específicas continuam protegidas contra sanitizações amplas e operações em lote.
- SANHAB, SANEORF, TRIOUT, TRIDESC, EXCCOL e correções simples/lote mantêm aplicação atômica e auditável.
- Os modos completos, parciais, de replay e de continuidade incremental mantêm os mesmos contratos públicos.
- `registros_validados.csv` continua condicionado à inexistência de pendências impeditivas em `registros_corrig.csv`.

## Validação

A candidata foi executada e auditada sem intervenção manual em três runs sucessivas com o dataset PNCV:

1. `painel_e_parar`, com correções específicas, exclusões e sanitização ampla;
2. `painel_incremental_registros_corrig`, herdando checkpoint e linhagem da primeira run;
3. `painel_incremental_completo`, herdando novamente checkpoint e linhagem e resolvendo todas as pendências impeditivas.

Resultados principais:

- 76.255 linhas preservadas nos checkpoints incrementais e em `registros_validados.csv`;
- 3 → 6 → 7 eventos de linhagem, com identidade e hashes semânticos herdados intactos;
- 56 ocorrências de CPF inválido relatadas: 55 valores removidos e 1 ocorrência subsumida por exclusão de coleta;
- nenhum CPF inválido preenchido nos produtos;
- 24 samambaias históricas sem hábito corretamente não bloqueantes e nenhuma samambaia 2025 sem hábito;
- correções específicas, TRIDESC e SANHAB confirmados nos atributos físicos canônicos;
- zero pendência impeditiva no fechamento e materialização correta de `registros_validados.csv`.

Também foi executada regressão real com o dataset FNCS e as 12 operações semânticas validadas da versão anterior. O produto corrigido manteve 16.766 linhas e equivalência semântica célula a célula, além de zero pendência condicional após as operações.

## Padrões públicos

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
```

## Privacidade

A release não inclui dados reais de entrada, produtos locais, logs, imagens, coordenadas, dados cadastrais ou outros artefatos sensíveis das execuções de validação.
