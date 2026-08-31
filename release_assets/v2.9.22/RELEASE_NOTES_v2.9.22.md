# Monitora Campestre-Savânico v2.9.22

Publicada em 31 de agosto de 2026 sobre a v2.9.21.

## Validação contratual antecipada

- O dataset passa a ser auditado antes da abertura do painel para identificar
  valores `select_one` e tokens `select_multiple` que não pertencem às
  `choices` vigentes do XLSForm 2025.
- A validação deriva diretamente do contrato único embutido. Não foi criado
  mapa, alias ou domínio local concorrente.
- Operações destinadas a campos de hábito são aceitas somente quando a
  categoria, a forma e o campo físico satisfazem a `relevance` contratual da
  linha-alvo. Uma operação dirigida ao ramo errado falha antes da mutação.
- Textos literais `NA` e `NULL` deixam de ser serializados como identificadores
  estáveis válidos.

## Composição segura de operações por token

- A fila pode combinar `replace_token` e `append_token` na mesma célula quando
  a origem, o destino e o token acrescentado são distintos.
- A combinação é aplicada como uma única transação atômica. Inclusões
  sobrepostas continuam classificadas como conflito e não produzem alteração
  parcial.
- A revisão permite recuperar de forma auditável valores concatenados cuja
  decomposição em choices vigentes seja única, sem inferir táxon ou decisão
  biológica.

## Homologação

- Testes focais cobriram domínio válido e inválido, relevância de hábito,
  composição distinta e sobreposição conflitante.
- Casos reais de duas UCs confirmaram detecção antecipada, bloqueio fechado,
  aplicação atômica e ausência de falso positivo em registros válidos.
- A recuperação real no RStudio para Windows usou R 4.6.0, preservou
  cardinalidade e esquema e só foi salva após a prévia integral chegar a zero
  pendências impeditivas.
- Contrato embutido, integridade do arquivo único, testes de regressão e
  desempenho no Windows integram os gates da publicação.

## Preservação

- O XLSForm 2025 e o contrato único embutido permanecem integralmente
  inalterados.
- A v2.9.21 pública e todas as releases históricas permanecem byte a byte
  intactas.
- Inicialização rápida, módulos não alcançados, produtos, relatórios,
  estatísticas e cartografia conservam o comportamento publicado.
- A revisão é geral e não contém regra por UC, COLETA, ponto, pessoa ou táxon.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.22.R`. O build exibido no console é
`v2.9.22-20260831-r02`.
