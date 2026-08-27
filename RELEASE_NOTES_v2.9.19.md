# Monitora Campestre-Savânico v2.9.19

Publicada em 27 de agosto de 2026 sobre a v2.9.18.

## Correção

- A sanitização assistida de hábitos obrigatórios passa a utilizar a mesma
  elegibilidade contratual no diagnóstico, na prévia, na aplicação e no replay.
- Valores inválidos não vazios, como `?`, deixam de ser excluídos do alvo
  físico da operação quando a ocorrência `SANHAB` foi confirmada pelo contrato
  XLSForm.
- Valores vazios continuam corrigíveis; valores já válidos permanecem
  protegidos contra sobrescrita. A operação continua atômica e idempotente.

## Homologação

- Parse integral e teste focal aprovados para valores vazios, inválidos não
  vazios, válidos e reaplicação.
- Execução real no R 4.6/Windows, no modo
  `painel_incremental_completo`, aprovada com zero pendência impeditiva.
- A correção foi confirmada em `registros_corrig.csv`,
  `registros_validados.csv` e na planilha XLSX de importação SISMONITORA.
- A linhagem acumulada foi preservada: 92 eventos, 13 sessões, nenhuma perda
  ou alteração de evento herdado e nenhuma falha de aplicação.
- Todos os produtos obrigatórios solicitados foram materializados, incluindo
  seis planilhas SISMONITORA, 190 figuras, relatórios analíticos em todos os
  formatos e mapa Sentinel.

## Preservação

- O início do script anterior às variáveis manuais permanece byte a byte
  equivalente à v2.9.18, descontados somente versão e build.
- Leitura, painel fora da ocorrência focal, contrato único, linhagem, demais
  correções, produtos, estatísticas, relatórios e cartografia não foram
  alterados.
- O arquivo único conserva margem segura sob o limite de 5 MiB do editor do
  RStudio para Windows, inclusive após conversão integral para CRLF.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.19.R`. O build exibido no console é
`v2.9.19-20260827-r01`.
