# v2.7.4 — Replay semântico auditável e gate de convergência

A versão `v2.7.4` torna o replay semântico verificável contra uma run-oráculo sem alterar o painel, a fila, os atributos a corrigir, as operações semânticas ou seus resolvedores. O objetivo é demonstrar que o input bruto somado à trilha semântica reproduz o mesmo `registros_corrig` final.

## Correções

- A comparação com o oráculo ocorre sobre o estado final reconciliado de `registros_corrig`, imediatamente antes da exportação.
- O gate exige identidade canônica preenchida e única no produto atual e na referência.
- Rótulos físicos diferentes são reconciliados somente quando o mapa contratual permite associação unívoca.
- Oráculo ausente, inválido, ambíguo ou divergente bloqueia a materialização quando o modo estrito está ativo.
- O padrão do diagnóstico de replay passa a ser seguro: operações incompatíveis ou inaplicáveis interrompem a execução.

## Auditoria produzida

Os relatórios ficam em `output/03_auditorias/replay_semantico/` e registram:

- identidade adotada e sua unicidade;
- aliases de colunas reconciliados e associações ambíguas;
- chaves presentes apenas em uma das bases;
- diferenças de schema;
- número de colunas e células divergentes;
- amostra detalhada das células diferentes;
- selo final `replay_equivalente_ao_oraculo`.

O oráculo nunca é usado como entrada operacional e não fornece valores ao produto.

## Comportamento preservado

- Nenhuma alteração na interface, no painel, nos atributos a corrigir, na apresentação da fila ou na denominação das operações.
- SANHAB, SANEORF, TRIOUT, TRIDESC, EXCCOL, movimentos de formas de vida e correções simples ou em lote mantêm seus contratos.
- Correções específicas continuam protegidas contra sanitizações amplas e operações em lote sobrepostas.
- Os 13 modos públicos permanecem disponíveis; o replay é aceito somente nos cinco modos compatíveis com input bruto.
- `registros_validados.csv` continua condicionado à aprovação integral do contrato sobre `registros_corrig.csv`.

## Validação independente

- PNCA: 7.070 registros, dois movimentos reaplicados e convergência exata com a referência.
- APAI: 7.878 registros, com SANHAB, movimento e exclusão reaplicados atomicamente e convergência exata.
- Nos dois casos, `registros_corrig.csv` e `registros_validados.csv` foram materializados e a linhagem foi verificada por hashes.
- Uma divergência deliberada de uma célula impediu a exportação.
- Modo incompatível, oráculo ausente e identidade não única também foram bloqueados.
- O custo medido do gate foi de aproximadamente 0,5 s em PNCA e 1 s em APAI.

## Padrões públicos

O replay e o oráculo permanecem desligados por padrão:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO <- "N"
```

## Privacidade

A release não inclui inputs, produtos de execução, logs, imagens, coordenadas, dados cadastrais ou qualquer dataset empregado na validação.
