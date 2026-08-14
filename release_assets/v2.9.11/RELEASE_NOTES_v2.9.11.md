# Monitora Campestre-Savânico v2.9.11

Publicada em 14 de agosto de 2026 sobre a v2.9.10.

## Relatórios analíticos

Bases com uma única campanha agora geram uma linha de base transversal entre
UAs. O relatório não atribui aumento, redução, estabilidade ou tendência
temporal quando não existe série suficiente. O estado dessa limitação é
registrado em auditoria própria.

O rótulo de `amostragem/especie` foi restabelecido conforme o XLSForm, e a
geração de DOCX em caminhos longos no Windows passou a usar temporários curtos
sem modificar o nome final do documento.

## Sentinel-2 e localizador

A busca por imagem Sentinel-2 começa na janela de 60 dias e é ampliada somente
quando nenhuma aquisição atende aos critérios de cobertura e qualidade. As
janelas acumuladas são 60, 120, 180, 365, 730, 1.460 e 2.920 dias, seguidas,
se necessário, pelo catálogo desde o início da missão Sentinel-2.

O mapa informa a data da aquisição, a defasagem, a janela efetivamente
consultada, a cobertura e a estimativa local de nuvens e sombras. O localizador
obtém rede amostral, UC, estados e biomas por consultas independentes e mantém
as camadas disponíveis quando uma fonte remota falha.

## Integridade e desempenho

A publicação de justificativas, checkpoints e arquivos associados é
transacional: candidatos são validados antes da substituição e qualquer falha
aciona rollback. Bloqueios transitórios de arquivo recebem retentativas curtas.

A prévia integral reutiliza o contrato de edição calculado uma única vez. Em
uma fila real de 150 operações da PNE, a repetição final caiu de 283,375 s na
v2.9.10 para 18,760 s. O caminho sem Sentinel continua retornando antes de
qualquer consulta remota.

## Homologação

- O botão **Source** iniciou imediatamente no RStudio/Windows.
- A PNE concluiu uma execução integral com produtos de dados, planilha de
  importação SISMONITORA, relatório de validação e relatórios analíticos.
- APAI, EEC, EET, FNCS, PNCA, PNCV, PNE, PNGSV, PNM e RBG geraram os relatórios
  sintético e detalhado em Rmd, Markdown, HTML, DOCX e PDF.
- As dez UCs geraram mapa Sentinel e localizador completo com rede, UC, estados
  e biomas. A APAI precisou da janela de 120 dias; as demais foram atendidas na
  janela inicial de 60 dias.
- O PNGSV, com uma única campanha, foi aprovado como relatório transversal sem
  inferência temporal indevida.

O contrato único, os itens congelados, os 13 modos públicos, a linhagem e os
produtos de dados permanecem preservados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.11.R`. O build exibido no console é
`v2.9.11-20260814`.
