# v2.7.2 — Reconciliação multiversão e desempenho contratual

A versão `v2.7.2` corrige a reconciliação de formas de vida e hábitos provenientes de estruturas históricas dos XLSForms e elimina uma regressão de desempenho observada na materialização das regras contratuais. O painel, os atributos a corrigir, a apresentação da fila, as operações semânticas e os 13 modos públicos permanecem inalterados.

## Correções

- As listas de formas de vida passam a reconhecer `name`, rótulos e variantes unívocas derivados dos XLSForms 2022, 2023, 2024 e 2025 antes da projeção para o contrato 2025.
- Hábitos históricos são resolvidos por versão e por linha. Um valor canônico já preenchido permanece soberano; quando ele está vazio, somente aliases físicos seguros são consultados.
- Valores repetidos que representam um único hábito, como `terrestre|terrestre`, são reduzidos ao token contratual único. Combinações realmente distintas continuam sujeitas à validação, sem escolha arbitrária.
- A proteção entre `nativa`, `exotica` e `seca_morta` impede que um hábito compartilhado fisicamente seja propagado para outra categoria que reivindique a mesma forma de vida.
- A validação de domínios passa a identificar tokens inválidos por correspondência exata, sem interpretar o conteúdo como expressão regular.

## Desempenho

A resolução histórica deixou de reconstruir os mesmos mapas linha a linha. Os mapas de aliases são cacheados por atributo e versão e aplicados de forma vetorizada.

- multi-UC: `15,987 s`;
- Mapinguari: `2,619 s`;
- FNCS: `2,382 s`.

Esses resultados eliminam a regressão de aproximadamente 83 segundos observada durante o desenvolvimento.

## Comportamento preservado

- Nenhuma alteração na interface, nos atributos a corrigir ou na forma de apresentação das operações na fila.
- SANHAB, SANEORF, TRIOUT, TRIDESC, EXCCOL, movimentos de formas de vida e correções simples/lote mantêm seus contratos operacionais.
- Correções específicas continuam protegidas contra sanitizações amplas e movimentos em lote sobrepostos.
- `registros_validados.csv` continua condicionado à inexistência de pendências impeditivas em `registros_corrig.csv`.
- Linhagem, replay semântico, persistência pós-exportação e organização dos produtos permanecem compatíveis com a v2.7.1.

## Validação independente

- Multi-UC em três runs incrementais: 37/37 verificações aprovadas; 135.037 registros de entrada, 10.403 exclusões explícitas e 124.634 registros finais, sem bloqueios.
- Mapinguari em três runs incrementais: 50/50 verificações aprovadas; 22.119 registros de entrada, 1.818 exclusões explícitas e 20.301 registros finais, sem bloqueios.
- FNCS: 34/34 verificações aprovadas, 16.766 registros finais e equivalência semântica célula a célula com a referência validada.
- Hábitos SANHAB, correções específicas, sanitizações, movimentos, exclusões, hashes de linhagem, cardinalidade e herança para `registros_validados.csv` foram conferidos diretamente nos produtos exportados.
- Os scripts públicos finais diferem da candidata executada somente pela identificação mecânica da versão e do build.

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

A release não inclui entradas reais, produtos de execução, logs, imagens, coordenadas, dados cadastrais ou outros artefatos sensíveis usados nos testes.
