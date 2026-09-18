# Monitora Campestre-Savânico v2.9.26

Publicada em 16 de setembro de 2026 sobre a v2.9.25 `r04`. A revisão
substitutiva `r03`, de 17 de setembro de 2026, mantém o mesmo serial e corrige
a geração antecipada do relatório de validação nos modos de checkpoint parcial.

## Revisão substitutiva r03 — relatório do checkpoint parcial

- A função de equivalência dos nomes de UC passa a ser carregada antes do
  primeiro consumidor documental. Isso elimina o erro
  `could not find function "monitora_stat_uc_chave_equivalencia"` ao finalizar
  `painel_incremental_registros_corrig`, `painel_e_parar` e caminhos parciais
  equivalentes.
- A função foi apenas reposicionada: sua expressão, a inicialização congelada do
  RStudio, o contrato XLSForm e todas as demais funções permanecem idênticos à
  revisão pública `r02`.
- A homologação reproduziu o defeito na `r02` e aprovou a correção sobre os
  21.311 registros e 211 coletas da FNB. O relatório `.Rmd`/`.md` foi gerado no
  R 4.6.0 do Windows com o Pandoc instalado pelo RStudio.
- A inconsistência nome/CPF da coleta 10898 continua corretamente fora de
  automação: ela bloqueia somente a planilha XLSX opcional e exige confirmação
  dos dados da equipe, sem afetar os CSVs principais.

## Revisão substitutiva r02 — robustez no Windows

- Leitura de CSV, relatórios analíticos e projeto QField passam a preservar e
  validar UTF-8 explicitamente, inclusive sob locale restritivo.
- A escrita do QGS valida o XML antes da promoção e evita metadados GDAL que
  produziam conteúdo incompatível no fluxo MBTiles.
- Ausências representadas por travessões Unicode deixam de depender de classes
  de expressão regular sensíveis ao locale.
- A tentativa que havia falhado foi repetida integralmente no RStudio 2026.04
  com R 4.6.0 no Windows: 7.878 registros, relatórios nos formatos solicitados,
  mapa, projeto QField e finalização foram aprovados; inputs permaneceram
  byte a byte intactos.
- Inicialização do RStudio, contrato XLSForm, regras semânticas e módulos fora
  do escopo permanecem preservados.

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
`v2.9.26-20260917-r03`.
