# v3.0.5 — relatórios Word/PDF e comparabilidade temporal

Build: `v3.0.5-20260929-r01`.

## Revisões

- Preserva todas as seções, tabelas, figuras e textos no Word; corrige índice, separação dos tópicos do resumo, bordas, larguras e centralização dos valores e respectivos cabeçalhos. Mantém títulos junto ao primeiro elemento após marcadores de tabelas/figuras no Word.
- Mantém todas as páginas em retrato. Figuras de evidência temporal ficam completas em uma página, com fonte ajustada às células; a trajetória usa eixos comuns e conserva a proporção da imagem.
- Explicita métrica e campanha nas tabelas e textos, elimina motivos repetidos e resolve referências a tabelas e figuras por identidade. Numeração continua sequencial quando faltam elementos. A presença de exóticas não é tratada automaticamente como invasão.
- Explica siglas na primeira menção e reúne siglas, unidades e símbolos em glossário temático com ordenação alfabética interna.
- Corrige o uso do nome histórico do formulário nas categorias gerais harmonizadas: calendário, pares/modelos climáticos, avaliação preditiva, integração multivariada, trajetória e desenho condicional de fogo. Preserva formação, posição, continuidade, desenho e suporte amostral. O detalhamento do material botânico continua condicionado ao instrumento de origem.
- Torna reprodutível a comparação editorial por período e conserva a quantidade solicitada de reamostragens em todos os perfis de recursos. A linhagem permanece em registros_corrig aprovado e derivados; não houve migração geral do motor para registros_validados.

## Homologação

| UC | Produtos | Sintético PDF / Word (páginas) | Detalhado PDF / Word (páginas) |
|---|---:|---:|---:|
| FNB | 22/22 | 19 / 17 | 94 / 90 |
| PNI | 22/22 | 17 / 16 | 66 / 62 |
| PNM | 22/22 | 19 / 18 | 88 / 82 |

As três UCs tiveram execução integral da candidata r11, com reutilização dos dados corrigidos, linhagem, imagens e caches disponíveis, sem nova curadoria. A revisão r12 altera exclusivamente a apresentação Word, reaplicada aos seis DOCX e conferida após salvar/reabrir no Microsoft Word. Os PDFs e resultados científicos não foram recalculados por essa revisão de paginação. A promoção pública altera somente os três identificadores de versão/build, com reversibilidade byte a byte verificada.

Gates: contrato de 129 campos, dados/UUIDs/linhagem, produtos e QField, 256 combinações de omissões editoriais, reprodutibilidade, comparabilidade, todas as células e linhas de tabelas, numeração, paridade de conteúdo/imagens/links, alinhamento e bordas, retrato, proporções e limites das imagens, páginas vazias e índices do Word após salvar/reabrir. A revisão visual dirigida complementa a cobertura automática; não se declara leitura visual individual de todas as páginas.

Script: **4,896,594 bytes LF / 4,980,820 bytes CRLF**, abaixo de 5.000.000 bytes.

## Reprodutibilidade em PNM

A comparação com a rodada antiga identificou diferenças exclusivamente nas reamostragens e suas classificações derivadas. As configurações antiga (999 bootstrap / 1.999 permutações no modo econômico) e atual (1.999 / 4.999 solicitadas) foram reproduzidas com as funções reais. Dois contrastes de composição passaram de estabilidade para inconclusivo; 12 comparações editoriais por período mudaram de classe com a correção das sementes. Dados, médias, efeitos centrais e pares permanecem iguais. O relatório atual utiliza os resultados corrigidos; a entrega inclui o inventário completo. FNB e PNI reproduziram todos os arquivos estatísticos de origem.

## Limites preservados

Resultados não estimáveis permanecem explícitos quando faltam independência, datas, suporte ou desenho. Não se presume ausência de fogo a partir da ausência de registro. A homologação desta revisão não repete a interação completa no painel RStudio, o uso de QField em aparelho/offline ou a importação por UUID no SISMONITORA. A inspeção nativa dos relatórios Word foi realizada. A verificação local das entregas OneDrive não certifica a sincronização na nuvem.

Bases e relatórios institucionais não integram o pacote público.
