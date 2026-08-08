# v2.9.4 — Relatórios compactos, Sentinel padrão e inicialização direta

A versão `v2.9.4` parte integralmente da última versão pública aprovada,
`v2.9.3`, e torna os relatórios analíticos mais executivos sem reduzir a
cobertura estatística, ecológica ou auditável dos produtos técnicos. Também
ativa o Sentinel-2 público como fundo padrão quando o módulo de relatórios for
solicitado e elimina o atraso de inicialização observado no botão **Source** do
RStudio para Windows.

## Relatórios analíticos compactos

- O relatório sintético prioriza resumo executivo, achados prioritários,
  esforço amostral, estado da cobertura, indicadores ecológicos, estrutura
  herbácea/lenhosa, resultados temporais, recomendações e limites.
- O relatório detalhado mantém método, robustez inferencial, composição,
  hipóteses, contexto de manejo, rastreabilidade e referências, com seleção
  visual mais estrita.
- Na homologação FNCS 2023–2026, o sintético passou de 22 para 9 páginas e o
  detalhado de 36 para 16 páginas.
- Seis figuras executivas são incorporadas ao sintético e dez figuras
  analíticas ao detalhado.
- Os 104 PNGs técnicos, CSVs, testes pareados, IC95%, FDR-BH, equivalência,
  comparações com linha de base e análises de composição continuam completos,
  materializados e auditados fora da seleção documental.
- O mapa vetorial equivalente deixa de ser repetido quando o mapa Sentinel é
  gerado com sucesso.

## Sentinel-2 público

- `MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE` passa a ter `"S"` como
  padrão declarado.
- A opção somente é efetiva quando
  `MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"`.
- Com os relatórios desligados, não ocorre validação da opção, carregamento de
  dependências, consulta STAC, leitura de imagem ou processamento cartográfico
  específico.
- A fonte `SENTINEL2_PUBLICO` continua sem conta, chave, token ou faturamento e
  usa a extensão do dataset, sem hardcode de UC.
- O mapa vetorial local permanece disponível mediante configuração explícita
  da opção como `"N"`.

## Inicialização e renderização multiplataforma

- O programa deixa de encapsular seus aproximadamente 4,5 MB em uma única
  expressão externa `evalq` e volta a ser avaliado diretamente no ambiente
  global.
- A mudança elimina a travessia interna que podia manter o RStudio para Windows
  sem resposta por 4 a 10 minutos antes da primeira mensagem.
- A correção permanece integralmente no arquivo `.R`, sem iniciador, projeto,
  `.Rprofile` ou arquivo auxiliar.
- O script continua corrigindo `Source with Echo` pela API oficial do RStudio
  quando necessário.
- Foi adicionado resolvedor multiplataforma do Pandoc distribuído pelo RStudio
  ou pelo Quarto, acionado somente quando formatos documentais o exigem.

## Desempenho

- Em ensaio controlado no Windows, com o mesmo `registros_corrig.csv`, R 4.6.0
  e configuração idêntica, os tempos reais foram 73,448 s na v2.9.1,
  73,959 s na v2.9.2, 76,218 s na v2.9.3 e 76,731 s na v2.9.4.
- A diferença real da v2.9.4 para a v2.9.3 foi de 0,7%, sem regressão material.
- O caminho com relatórios desligados retorna antes de qualquer trabalho do
  Sentinel e preserva a política de custo zero para opções inativas.

## Validação

- Parse integral aprovado nos bytes finais da candidata.
- Teste real pelo botão **Source** no RStudio para Windows aprovado, sem a
  espera de vários minutos observada anteriormente.
- Homologação integral FNCS 2023–2026 aprovada com 16.766 pontos, 58 UAs,
  quatro contextos SISMONITORA, 104 PNGs e todos os gates finais em estado
  `ok`.
- Foram gerados Rmd, Markdown, HTML, DOCX e PDF para as versões sintética e
  detalhada, sem falhas na auditoria de renderização.
- O mapa usou Sentinel-2 L2A adquirido em 5 de agosto de 2026, com 100% de
  cobertura da extensão e estimativa de 6,8% de nuvens e sombras.
- As 25 páginas finais em PDF foram renderizadas e inspecionadas integralmente,
  sem cortes ou sobreposições observadas.

## Preservado

- contrato único e projeção SISMONITORA;
- 13 modos públicos, painel e operações atômicas;
- CPF opcional e opção de remoção ou preservação de UUID;
- sanitização de coletores, impactos condicionais e justificativas em lote;
- diagnósticos de formação vegetacional e vegetação seca ou morta;
- linhagem, replay, oráculo, precedência e gates impeditivos;
- bases, planilhas SISMONITORA, estatísticas, gráficos e KML/KMZ;
- cautela causal, robustez inferencial e rastreabilidade dos relatórios.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.4.R`. O build declarado no console é
`v2.9.4-20260808.1`.
