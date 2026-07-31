# v2.9.0 — Relatórios analíticos, importação multicontexto e desempenho

A versão `v2.9.0` amplia os produtos opcionais do Monitora sem alterar o
contrato único, o painel ou os produtos centrais publicados na `v2.8.2`.
Relatórios analíticos, mapa orbital e planilhas de importação continuam
desligados por padrão e somente são materializados quando solicitados.

## Relatórios analíticos opcionais

Para cada execução com uma única UC, o script pode produzir relatórios
sintético e detalhado em Rmd, Markdown, HTML e PDF, acompanhados das tabelas e
figuras editáveis que sustentam o texto. Os documentos apresentam:

- esforço amostral por UC, formação vegetacional e ano;
- número efetivamente observado de UAs e pontos amostrais;
- continuidade temporal e distribuição espacial das UAs;
- situação dos dados como `Validado`, `Em validação` ou `Não validado`;
- evidências e pendências priorizadas, sem expor códigos internos na prosa;
- rótulos gramaticalmente adequados em tabelas, legendas e texto.

Ativação:

```r
MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"
MONITORA_FORMATOS_RELATORIOS_ANALITICOS <- c("rmd", "md", "html", "pdf")
```

Os relatórios são aceitos somente nos modos completos documentados e exigem
uma única UC, evitando apresentar agregações entre unidades como se fossem um
relatório institucional específico.

## Imagem Sentinel-2 pública

O mapa de continuidade pode usar imagens Sentinel-2 L2A recentes. A seleção é
derivada da extensão espacial do dataset e consulta um catálogo STAC público,
sem conta, chave, token, faturamento ou hardcode de UC, campanha, cena ou tile.
O módulo avalia cobertura, nuvens e sombras na área efetivamente exibida e lê
somente as janelas necessárias dos COGs públicos.

```r
MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"
MONITORA_FONTE_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "SENTINEL2_PUBLICO"
```

O Google Maps permanece disponível como alternativa explícita e requer a
variável de ambiente `MONITORA_GOOGLE_MAPS_API_KEY`. Credenciais nunca são
gravadas pelo script.

## Planilhas SISMONITORA multicontexto

Quando `registros_validados.csv` contém mais de uma combinação de UC, ciclo ou
campanha, a geração opcional não bloqueia mais a execução. O script particiona
a fonte deterministicamente e cria uma planilha independente para cada
contexto UC + ciclo + campanha.

- Um contexto preserva o nome
  `registros_validados_importacao_sismonitora.xlsx`.
- Múltiplos contextos recebem nomes inequívocos com índice, hash e rótulos
  seguros de UC, ciclo e campanha.
- Cada arquivo mantém 115 colunas e as três abas do modelo 21FEV25.
- UUIDs de transporte permanecem vazios para inclusão de registros novos; os
  identificadores da fonte ficam preservados em `registros_validados.csv` e na
  auditoria.
- Todos os contextos são validados antes da publicação dos arquivos, impedindo
  conjunto parcial quando uma partição viola o contrato.

## Finalização e compatibilidade

- Chrome, Chromium ou Edge são resolvidos no Windows, Linux e macOS por
  configuração explícita, variáveis de ambiente, executáveis no `PATH` e
  instalações usuais.
- Um favicon local elimina o erro HTTP 404 que interrompia a criação dos PDFs.
- Formatos concluídos e auditorias são preservados quando um consumidor externo
  falha; o bloqueio permanece explícito e ocorre somente depois da organização
  final do output.
- A raiz de `output/` mantém apenas `README_OUTPUT.txt` e
  `indice_produtos.csv`; os demais arquivos são organizados em seus diretórios
  canônicos.

## Desempenho contratual

Aliases históricos e a migração de canela-de-ema usam gates por token, evitando
varreduras e auditorias caras quando o caso não existe. A reconciliação de
formas de vida reutiliza estado idêntico, invalida somente linhas focalmente
alteradas e mantém fallback global para mudanças estruturais ou ambíguas.

Checkpoints granulares separam normalizações, reconciliação, migração,
auditoria impeditiva, produtos opcionais e finalização. Dispositivos gráficos
residuais criados pela run são encerrados sem tocar o `RStudioGD` ou outros
dispositivos anteriores da sessão.

## Inicialização no RStudio

O código público permanece legível e autossuficiente, mas constitui uma única
expressão externa avaliada no ambiente global. Isso impede que `Source with
Echo` enfileire dezenas de milhares de expressões antes do início. A preferência
do RStudio é corrigida persistentemente pela API oficial quando necessário;
Rscript não é alterado e não existe arquivo iniciador auxiliar.

No teste real com RStudio 2026.07.1+147 e R 4.6.0 no Windows, a primeira
mensagem apareceu em 0,317 s com `Source with Echo` e em 1,805 s com o botão
Source normal.

## Preservação e validação

- Baseline pública anterior: `v2.8.2`.
- Contrato único, painel, operações, replay, linhagem, 13 modos, estatísticas,
  produtos centrais e KML/KMZ permanecem preservados.
- Execução integral real aprovada no Windows, incluindo XLSX multicontexto e
  relatórios em todos os formatos solicitados.
- QA funcional e testes dos módulos aditivos aprovados no Linux.
- macOS foi auditado estaticamente; não há alegação de execução real nesse
  sistema.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.0.R`. O build declarado no console é
`v2.9.0-20260731.1`.
