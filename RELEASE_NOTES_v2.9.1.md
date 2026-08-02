# v2.9.1 — UUID configurável, DOCX e cartografia institucional

A versão `v2.9.1` parte integralmente da última versão pública aprovada,
`v2.9.0`, e consolida as revisões desenvolvidas depois de sua publicação.
As mudanças ficam restritas aos dois produtos opcionais já existentes:
planilhas de importação no SISMONITORA e relatórios analíticos. O contrato
único, o painel e os produtos centrais permanecem congelados.

## Planilha SISMONITORA e UUID

A variável abaixo foi acrescentada imediatamente depois da opção que ativa a
planilha:

```r
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- "N"
MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "S"
```

- `"S"` preserva o comportamento já homologado para inclusão: mantém as
  colunas `uuid` e `amostragem/registro/uuid` no schema, mas esvazia seus
  valores no XLSX.
- `"N"` preserva os valores dos dois UUIDs na planilha, preparando o produto
  para eventual atualização de registros existentes quando essa operação for
  homologada pela equipe do SISMONITORA.
- A variável só é lida e validada se a criação do XLSX estiver ativa.
- `registros_validados.csv` permanece byte a byte intocado em ambos os modos.
- O gate que exige a geração aprovada de `registros_validados.csv` permanece.
- Fontes multicontexto continuam produzindo um XLSX independente para cada
  combinação UC + ciclo + campanha, sem bloquear a execução.

## Relatórios analíticos

- Os relatórios sintético e detalhado passam a ser gerados também em DOCX.
- O conjunto padrão passa a ser Rmd, Markdown, HTML, DOCX e PDF.
- Os títulos das seções do relatório sintético foram harmonizados com os do
  detalhado quando pertinentes, incluindo `Resumo executivo`, `Achados
  prioritários` e `Esforço amostral por UC, formação e ano`.
- Tabelas, prosa e legendas usam rótulos gramaticalmente adequados; códigos
  internos e o termo `QA` não são expostos na situação dos dados.
- O esforço informa UAs efetivamente amostradas e número de pontos amostrais;
  a duplicação conceitual como “transecções” e a expressão “posições
  planejadas” não são usadas.

## Padrão cartográfico aprovado

O mapa Sentinel-2 dos relatórios adota um leiaute quadrado com aproximadamente
76% da altura para a área cartográfica e 24% para uma faixa institucional
inferior. Na área principal permanecem somente as feições amostrais, a escala
gráfica e o norte geográfico. A faixa inferior reúne:

- localizador no canto esquerdo, com bioma, estados, limite oficial da UC e
  extensão da grade amostral;
- quadro único de legenda, com `Continuidade do esforço amostral nas UAs` e
  `Formação vegetacional`;
- quadro `Informações do mapa`, com identificação do protocolo, UC, período,
  projeção, código EPSG, escala numérica, imagem, aquisição, processamento,
  fontes, limitações e `Elaboração: CBC/ICMBio`;
- marcas institucionais Monitora, CBC e ICMBio incorporadas ao próprio script.

Também foram consolidados:

- título completo com preposição gramatical adequada ao nome da UC;
- moldura com coordenadas geográficas e latitude rotacionada 90° no sentido
  anti-horário;
- grade branca fina, suave e semitransparente;
- símbolo de norte em branco, com proporção preservada;
- escala gráfica branca, mais espessa e posicionada no canto inferior esquerdo
  da área cartográfica;
- preenchimento integral da moldura após reprojeção da imagem;
- fontes ampliadas e quadros dimensionados pelo conteúdo;
- margens uniformes e ausência de quadros sobre as feições do mapa principal;
- harmonização dos mesmos termos no mapa vetorial complementar.

## Dados geoespaciais e privacidade

- A extensão, a projeção UTM, a zona, a busca Sentinel-2, a escala e o
  localizador são derivados do dataset de entrada; não há hardcode de FNCS,
  PNM ou qualquer outra UC.
- O fundo usa Sentinel-2 L2A público pelo Earth Search, sem conta, chave,
  token ou faturamento.
- O limite oficial da UC é consultado no ICMBio e os estados e biomas no IBGE.
  Os arquivos vetoriais são baixados para diretório temporário, materializados
  em memória e removidos ao fim da função. Nenhuma referência espacial de UC é
  incorporada ao script ou persistida como produto.
- A auditoria cartográfica registra fonte, versão, processamento, EPSG,
  aquisição, cobertura de nuvens/sombras e limitações de uso em CSV e JSON
  alinhados ao conjunto mínimo aplicável do MGB 2.0.

## Desempenho e opções desligadas

- Relatórios, mapa orbital e XLSX continuam desligados por padrão.
- Quando uma opção está em `"N"`, seu módulo retorna antes de carregar
  dependências, ler produtos, consultar serviços ou materializar arquivos.
- No gate real de 16.766 linhas e quatro contextos, os quatro XLSX foram
  criados em 16,690 s; o contexto único foi criado em 4,860 s.
- A homologação focal final dos dez documentos, incluindo imagem Sentinel,
  levou 94,872 s. Esse custo existe somente quando o módulo é solicitado.
- As otimizações contratuais da v2.9.0 foram preservadas. Na execução integral
  final, a reconciliação pré-auditoria levou 0,278 s e o gate de canela-de-ema
  0,124 s quando o token estava ausente; nenhuma linha foi migrada.

## Validação

- Parse integral do script final aprovado.
- Geração XLSX aprovada com 16.766 registros reais da FNCS, 166 coletas e
  quatro contextos, nos modos de remoção e preservação de UUID.
- Execução integral real da FNCS aprovada com produtos centrais, quatro XLSX,
  KML/KMZ, 104 PNGs principais e dez documentos analíticos. A execução
  materializou 305 arquivos, concluiu o pipeline em 505,552 s e terminou em
  534,982 s reais, com 1,53 GiB de memória residente máxima.
- Gate final dos relatórios aprovado com 16.766 pontos, 58 UAs, dez documentos
  e zero falhas na auditoria de renderização.
- PDFs e DOCXs foram renderizados e inspecionados visualmente; não foram
  observados cortes, sobreposições ou perda das figuras.
- Cartografia genérica aprovada também com 201 geometrias reais do Parque
  Nacional Mapinguari, correspondentes a 49 UAs em 2021–2025. O teste resolveu
  dinamicamente UTM 20S, imagem Sentinel e o limite oficial do PNM.
- Compatibilidade funcional executada em Linux; DOCX também renderizado pelo
  Microsoft Word no Windows. Caminhos e consumidores externos foram auditados
  para Windows, Linux e macOS. Não se declara execução real em macOS.

## Preservado

- Baseline pública `v2.9.0`.
- Contrato único dos XLSForms e projeção SISMONITORA.
- Interface, painel, atributos, filtros, navegação, persistência e operações.
- Regras semânticas, precedência, reconciliação e sanitizações homologadas.
- Linhagem, replay, oráculo e auditorias impeditivas.
- Treze modos públicos, produtos centrais, estatísticas, gráficos e KML/KMZ.
- Encapsulamento autossuficiente e solução interna de inicialização do RStudio.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.1.R`. O build declarado no console é
`v2.9.1-20260801.2`.
