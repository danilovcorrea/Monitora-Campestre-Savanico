# v3.0.6 — interpretação e apresentação dos relatórios

Build: `v3.0.6-20260929-r01`.

## Revisões

- Amplia a explicação das análises multivariadas: pergunta, população, mudanças observadas, associação isolada, contribuição adicional dos blocos época/clima/fogo/contexto, sensibilidade à escala e limites de atribuição. Distingue a ordenação ajustada das diferenças entre extremos das trajetórias de cobertura ao longo dos anos.
- Usa os resultados e as formações da própria UC nas sínteses; trata lacunas, ano único, valores constantes e análises não estimáveis. O resumo de fogo explicita as interseções cartográficas e seus limites. O resumo climático descreve condições antecedentes de 90 dias, sem apresentá-las como tendência climática da UC.
- Adota “Época de amostragem” e “reamostragens”; esclarece primeira amostragem, manutenção, não reamostragem e retomada de UAs. Primeira ocorrência na série não comprova implantação da UA.
- Revê a sequência entre contexto, figuras, resultados e interpretação; apresenta a descrição das trajetórias depois da figura; mantém referências e numeração dinâmicas. Retira as duas enumerações adicionais da seção Estado da cobertura vegetal.
- Elimina notas automáticas duplicadas, preservando notas substantivas. Padroniza Campestre e Savânica nas colunas de formação e centraliza verticalmente os cabeçalhos HTML/PDF, acompanhando o Word. Impede cabeçalhos de tabela isolados da primeira linha no Word.

## Homologação

| UC | Produtos | Sintético PDF / Word (páginas) | Detalhado PDF / Word (páginas) |
|---|---:|---:|---:|
| FNB | 22/22 | 21 / 19 | 103 / 97 |
| PNI | 22/22 | 19 / 17 | 72 / 68 |
| PNM | 22/22 | 21 / 20 | 100 / 94 |

FNB, PNI e PNM tiveram execução integral da candidata, utilizando dados já validados, linhagem, imagens QField e caches existentes. Os resultados científicos foram comparados à homologação anterior. Os documentos passaram por verificações de conteúdo, células, figuras, numeração, paridade, paginação, retrato, ausência de páginas vazias, margens e índices salvos e reabertos no Microsoft Word. A revisão visual dirigida complementa a verificação automática integral.

A versão pública difere da candidata homologada somente nos três identificadores de versão/build; a reversibilidade byte a byte é verificada na promoção. Não houve migração da linhagem dos cálculos: permanecem o registro corrigido com selo validado e seus produtos derivados.

Script: **4895302 bytes LF / 4979536 bytes CRLF**, abaixo de 5.000.000 bytes. O módulo interno utiliza compressão XZ suportada pelo R base; leitura conferida no R nativo Windows.

## Limites da conferência

Não se repetiram nesta revisão a interação completa no painel RStudio, o uso de QField em aparelho/offline ou a importação no SISMONITORA, cujos fluxos não foram modificados. Resultados sem suporte continuam não estimáveis. Os documentos não atribuem causalidade aos fatores a partir de associações descritivas. As entregas foram verificadas localmente; a sincronização na nuvem depende do OneDrive. Bases e relatórios institucionais não integram o pacote público.
