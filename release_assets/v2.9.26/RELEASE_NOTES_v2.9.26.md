# Monitora Campestre-Savânico v2.9.26

Publicada em 16 de setembro de 2026 sobre a v2.9.25 `r04`.

## Evidências temporais e relatório analítico

- A população principal de cada contraste temporal passa a usar todas as UAs
  comuns ao respectivo par de campanhas. Painéis por ano de entrada permanecem
  como análise de sensibilidade e não substituem a evidência principal.
- Ausência amostrada é zero nas métricas de cobertura; proporção relativa sem
  denominador permanece indefinida. Isso evita tanto perder UAs sem ocorrência
  quanto inventar composição.
- A seção de época das campanhas volta a apresentar uma tabela compacta dos
  períodos observados e explicita os anos que formam cada cenário de referência,
  antes da figura de sensibilidade e da recomendação sobre linha de base.
- Formas de vida com zero toque em toda a UC deixam de ser exibidas nas tabelas
  do relatório; os CSVs completos continuam preservados para auditoria.

## Entrada simples e auditável do projeto QField

- Em execução com uma UC, MBTiles e camadas adicionais podem ser colocados
  diretamente em `qfield_input/`, sem subpasta da UC e sem nome obrigatório.
- O papel de cada MBTiles é determinado pelos níveis de zoom materializados:
  detalhe em zoom máximo 18 ou superior, regional em 14 ou inferior e
  operacional entre 15 e 17. O nome do arquivo não participa da classificação.
- Para imagem detalhada, o script deriva um centro por UA dos consensos aceitos
  pela validação espacial, cria os círculos de 500 m e realiza internamente o
  recorte físico para WebP transparente. A fonte não é sobrescrita.
- A auditoria registra hashes, zooms, tiles preservados, tiles de borda, tiles
  descartados, cobertura dos extremos anuais e redução do arquivo.
- KML, KMZ, GPKG e ZIP de shapefile podem compartilhar `qfield_input/`. Papéis
  especiais continuam podendo ser declarados em `camadas_qfield.csv`.
- Permanecem as camadas anuais `UC_verg_ini_YYYY` e `UC_verg_fin_YYYY`, pontos
  amostrais previstos, apoio editável, acessos, Sentinel regional, coordenadas
  WGS 84 em graus decimais e `Google Satellite` opcional exclusivamente online.

## Manual do usuário

- O manual gerado pelo script passa a documentar integralmente configuração,
  onze rotinas de ponta a ponta, diretórios de saída, continuidade incremental,
  relatórios, QField, situações operacionais e diagnóstico de falhas.
- A orientação inclui a seleção segura da aba biológica em XLSX com abas
  adicionais, tratamentos herdados e atuais, época/linha de base, material
  botânico anterior a 2025 e limites de interpretação.
- O texto reforça que o contrato único e o XLSForm 2025 não podem ser
  flexibilizados para acomodar inconsistências dos registros.

## Homologação e preservação

- PNB com todos os anos confirmou pareamentos, populações analíticas, tabela de
  calendário, mapa Sentinel e ausência de formas zeradas nas tabelas.
- A simulação de uma usuária na APA Ibirapuitã incorporou um MBTiles de nome
  arbitrário e um KML fictício na pasta plana. O projeto cobriu 156 extremos
  anuais; 1.396 tiles foram preservados, 678 recortados na borda e 1 descartado,
  com redução de 28,1% e fontes de entrada intactas.
- O manual foi renderizado no R 4.6.0 do Windows com o Pandoc do RStudio. O
  script permanece abaixo de 5 MiB mesmo com terminações CRLF.
- A inicialização congelada do RStudio e o contrato XLSForm materializado são
  idênticos à baseline pública; nenhuma rodada de UC integra esta publicação.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.26.R`. O build exibido no console é
`v2.9.26-20260916-r01`.
