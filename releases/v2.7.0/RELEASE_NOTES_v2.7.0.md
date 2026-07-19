# v2.7.0 — Contrato consolidado, operações atômicas e linhagem auditável

A versão `v2.7.0` consolida mudanças estruturais no tratamento e na validação dos dados do Alvo Global do Componente Campestre Savânico. A publicação sucede a série `v2.6.x` e preserva os modos de execução, o painel e os produtos públicos, com reforço de contrato, persistência e auditoria.

## Principais entregas

- Reconciliação de nomes, rótulos, aliases e caminhos dos XLSForms 2022–2025.
- Validação de `registros_corrig.csv` pelo contrato consolidado e projeção final conforme XLSForm 2025/template SISMONITORA.
- Fonte reconciliada de ocorrências diagnósticas, com uma identidade por caso real e relatórios detalhados.
- Operações semânticas atômicas, com escopo efetivo por linha e proteção de correções específicas contra operações amplas.
- Prévia, modal, aplicação e fechamento baseados na mesma reconciliação.
- Gate impeditivo para `registros_validados.csv`: o produto não é materializado enquanto houver pendência contratual impeditiva.
- Replay semântico e continuidade incremental pelo contrato `replay_semantico_v2`, com manifesto e hashes de linhagem.
- Persistência auditada após aplicação, exportação e releitura dos produtos.
- Reparo determinístico de mojibake e escrita portátil de CSVs.
- Organização canônica de produtos, relatórios, auditorias, estatísticas e gráficos.

## Operações assistidas

A versão preserva correções simples e em lote, exclusão de coletas, movimento individual e em lote de formas de vida, sanitização de hábito obrigatório, saneamento de outras formas de vida e demais operações semânticas do painel.

Quando uma correção específica e uma operação ampla alcançam o mesmo escopo, a correção específica é protegida. Movimentos de múltiplas formas são representados e aplicados como os itens semânticos solicitados, sem perda silenciosa de itens da fila.

## Produtos e linhagem

A cadeia auditável inclui:

```text
input
  → registros_importados_bruto.csv
  → registros_importados.csv
  → registros_importados_operacional_pre_painel.csv
  → registros_corrig.csv
  → registros_validados.csv
```

Exclusões deliberadas e alterações de valores são registradas nos relatórios e na trilha semântica. Em continuidade incremental, o `registros_corrig.csv` e a pasta de linhagem da mesma execução devem permanecer juntos.

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

O painel pode ser ativado deliberadamente no bloco operacional. A geração de `registros_validados.csv` continua condicionada à aprovação do gate, mesmo com a opção de geração ligada.

## Compatibilidade

- Os 13 modos de execução públicos permanecem disponíveis.
- O contrato corrente de replay é `replay_semantico_v2`; ledgers anteriores somente podem ser usados pelas rotinas de validação ou migração explícita do próprio script.
- `registros_validados.csv`, quando apto, mantém 129 atributos na ordem do template de destino.
- `coletor/cpf` e `coletor/nome` são preservados na forma física esperada pelo template SISMONITORA.
- O script e os produtos CSV foram preparados para Windows, macOS e Linux.

## Validação da versão

A candidata funcional aprovada passou por matriz automatizada de 80 verificações, sem falhas. Foram cobertos:

- contagens independentes do input e dos relatórios;
- preservação da linhagem;
- operações específicas, sanitizações amplas e movimentos em lote;
- persistência em `registros_corrig.csv` e `registros_validados.csv`;
- continuidade incremental e reconstrução por replay total;
- proteção de correções específicas;
- auditorias de persistência pós-exportação;
- schema, chaves, codificação e ausência de mojibake nos produtos finais.

Os bytes públicos finais são novamente submetidos a parse, testes de regressão, comparação estrutural e checksums antes da criação da tag.

## Privacidade

A release não inclui arquivos reais de `input/`, `output/`, `log/` ou `extracted/`, bases CSV/XLSX de execução, coordenadas, fotos, UUIDs, dados cadastrais ou outros produtos locais sensíveis.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.7.0.R`
- `monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `R/monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `GUIA_USUARIO_v2.7.0.md`
- `VERSION`
- `SHA256SUMS.txt`
