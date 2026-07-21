# v2.7.3 — Persistência contratual e validação estratificada

A versão `v2.7.3` consolida a projeção dos hábitos reconciliados nos produtos finais e amplia a robustez da harmonização histórica. O painel, os atributos a corrigir, a apresentação da fila, as operações semânticas e os 13 modos públicos permanecem inalterados.

## Correções

- `registros_validados.csv` passa a herdar cada hábito do atributo físico canônico já reconciliado em `registros_corrig.csv`, pela mesma regra contratual usada no diagnóstico e no SANHAB.
- O resolvedor de aliases considera também a associação explícita ao token contratual, reforçando a compatibilidade entre `name`, rótulos e caminhos físicos dos XLSForms históricos.
- Repetições escalares equivalentes em campos históricos separados por `|`, como `terrestre|terrestre`, são comparadas como um único token. Combinações distintas permanecem diferentes e continuam sujeitas ao gate.
- A limpeza de dependências condicionais exclui atributos protegidos do conjunto de campos de origem elegíveis para remoção.

## Comportamento preservado

- Nenhuma alteração na interface, no painel, nos atributos a corrigir, na fila ou na denominação das operações.
- SANHAB, SANEORF, TRIOUT, TRIDESC, EXCCOL, movimentos de formas de vida e correções simples ou em lote mantêm seus contratos operacionais.
- Correções específicas continuam protegidas contra sanitizações amplas e movimentos em lote sobrepostos.
- `registros_validados.csv` continua condicionado à inexistência de pendências impeditivas em `registros_corrig.csv`.
- Linhagem, replay semântico, persistência pós-exportação, organização dos produtos e escrita portátil dos CSVs permanecem preservados.

## Validação independente

- 13/13 modos públicos executados no artefato funcional: 102 gates aprovados, nenhuma falha.
- 22/22 UCs reexecutadas no artefato funcional: 242 gates aprovados, nenhuma falha.
- 14 replays e oito campanhas de três runs incrementais aprovados, além da auditoria consolidada.
- Total acumulado: 626 gates operacionais aprovados e nenhuma falha.
- A cobertura incluiu SANHAB, exclusões impeditivas, correções específicas, sanitizações, movimentos em lote, preservação de identidade e cardinalidade, linhagem, contrato único, produtos obrigatórios, herança de hábitos e codificação UTF-8 com BOM.
- A preparação editorial alterou apenas duas linhas de comentário. A comparação sintática confirmou que todos os tokens executáveis do arquivo publicado são idênticos aos do artefato funcional testado.

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
