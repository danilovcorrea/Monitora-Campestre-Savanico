# Monitora Campestre-Savânico v2.9.20

Publicada em 27 de agosto de 2026 sobre a v2.9.19.

## Cartografia Sentinel-2

- Corrige a regressão espacial causada pela reatribuição manual de extensão e
  CRS a visões internas de COGs Sentinel-2.
- O GDAL pode utilizar uma visão interna somente quando bandas, CRS, extensão
  e resolução preservam o contrato espacial do COG oficial. Caso contrário, a
  rotina usa o raster principal.
- Caches antigos sem garantia de origem espacial são rejeitados. Caches
  comprovadamente derivados de bandas espectrais diretas e rasters legados de
  até 12,5 m permanecem compatíveis.
- A rotina continua priorizando cobertura, baixa presença de nuvens e sombras,
  recência e resolução adequada, sem abandonar a geração do mapa quando a
  aquisição preferencial não está disponível.

## Recuperação da fila no painel

- Excluir uma correção pendente passa a ser uma ação atômica de recuperação.
- A operação escolhida é removida mesmo quando outras intenções da fila ainda
  apresentam conflitos.
- Conflitos remanescentes continuam visíveis, auditados e bloqueados antes da
  prévia integral ou do salvamento; nenhuma proteção semântica foi relaxada.

## Homologação

- A implementação anterior defeituosa apresentou correlações de apenas 0,02 a
  0,05 com os COGs oficiais, confirmando a causa da regressão.
- A leitura corrigida foi comparada ao raster principal em tiles, mosaico e 12
  janelas independentes. As janelas alcançaram correlação de 0,9342 a 0,9727 e
  erro absoluto médio de 1,34 a 5,02.
- Execução real da APAI sem cache concluiu catálogo, composição de quatro
  tiles e mapa em 50,7 s, com 100% de cobertura e todas as UAs sobre pixels
  válidos.
- Teste funcional no Windows preservou a geometria do Linux, com correlação de
  0,99972 a 0,99993.
- A correção do painel foi exercitada com conflitos remanescentes: a intenção
  selecionada foi excluída, as demais foram preservadas e o bloqueio continuou
  ativo.

## Preservação

- Os blocos de inicialização rápida/RStudio permanecem byte a byte idênticos à
  v2.9.19, descontadas somente versão e build.
- A simulação do Source com echo produziu os mesmos 174 bytes da v2.9.1.
- O arquivo público conserva uma única expressão externa e permanece abaixo do
  limite de 5 MB do editor do RStudio no Windows.
- Produtos de dados, contrato único, linhagem, demais operações do painel,
  estatísticas e relatórios não foram alterados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.20.R`. O build exibido no console é
`v2.9.20-20260827-r01`.
