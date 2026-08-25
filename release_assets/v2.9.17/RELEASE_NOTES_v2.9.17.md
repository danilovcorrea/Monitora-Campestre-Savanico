# Monitora Campestre-Savânico v2.9.17

Publicada em 25 de agosto de 2026 sobre a v2.9.16.

## Relatório de validação editável

- O Rmd técnico permanece disponível e o relatório consolidado pode gerar
  Markdown realmente renderizado e DOCX editável.
- O DOCX usa modelo editorial A4 embutido no arquivo R, capa, títulos
  hierárquicos, paleta institucional, tabelas com larguras controladas,
  hyperlinks e verificação estrutural OpenXML.
- A renderização usa diretório temporário curto e publicação verificada por
  hash, reduzindo o risco de caminhos longos no Word para Windows.
- Falhas documentais permanecem isoladas dos produtos de dados. Em checkpoints
  anteriores ao carregamento do modelo, o DOCX é adiado para o fechamento da
  execução e os demais formatos seguem normalmente.

## Transparência dos painéis analíticos

- Achados prioritários passam a informar o ano inicial do painel fixo e o
  número de UAs da população efetivamente comparada.
- A direção observada é confrontada individualmente com os painéis iniciados
  após ampliações do esforço, permitindo distinguir corroboração e
  sensibilidade ao desenho amostral.
- `indice_evidencias_relatorio.csv` passa a registrar
  `ano_inicial_painel` e `populacao_analitica`.
- A redação de valores de p inferiores a 0,001 foi corrigida. Nenhum cálculo,
  denominador ou critério estatístico foi alterado.

## Contrato único e fechamento hierárquico

- Um resolvedor físico central projeta paths, names, labels e aliases
  contratuais exatos sobre as colunas do conjunto de dados.
- Representações compartilhadas por mais de um atributo e colunas físicas
  duplicadas falham de modo fechado, sem seleção por aproximação textual.
- O fechamento hierárquico de formas de vida usa as dependências do XLSForm
  21FEV25, inclusive relações transitivas. Texto livre não é interpretado como
  token e não pode reinjetar a categoria `outra`.
- A triagem de formas secas ou mortas e a síntese analítica compartilham a
  mesma resolução contratual dos quatro atributos de impactos.
- Campo vazio, atributo ausente e resolução ambígua são estados distintos;
  falha de resolução nunca é apresentada como ausência declarada de contexto.

## Homologação e desempenho

- Regressão real PNSC aprovada em 13.130 linhas, inclusive a coleta 42512,
  ponto 11, e cinco coletas com contexto de fogo reconhecido.
- Esquemas reais de APAI, FNCS e PNCV foram resolvidos sem ambiguidade; o caso
  histórico APAI 17626, pontos 11 e 23, permaneceu preservado.
- A matriz sintética cobriu paths, names, labels, aliases, conflitos,
  idempotência, ordem aleatória e estados contextuais.
- No PNSC real, o fechamento focal passou de 0,437 s na candidata-base para
  0,078 s na versão final do módulo.
- O bloco congelado de inicialização rápida do RStudio permanece equivalente à
  v2.9.16, descontados versão e build.

## Preservação

- Leitura, painel, replay, linhagem, produtos de dados, estatísticas,
  cartografia Sentinel-2, modos de execução e demais módulos não relacionados
  ao escopo permanecem preservados.
- O script continua autossuficiente e não depende de arquivos locais.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.17.R`. O build exibido no console é
`v2.9.17-20260825-r01`.
