# Monitora Campestre-Savânico v2.9.13

Publicada em 19 de agosto de 2026 sobre a v2.9.12.

## Relatórios analíticos

- A seção de hipóteses, evidências e gestão passa a ser condicionada aos
  resultados efetivamente observados em cada UC.
- As tabelas de estado apresentam, nessa ordem, número de UAs, cobertura e
  proporção relativa.
- O contexto de fogo é sintetizado pela união de COLETAs com ao menos um
  registro pertinente, sem contagem duplicada, e acompanhado dos subcontextos
  declarados, do total anual de COLETAs e do percentual correspondente.
- Menções inequívocas a fogo no campo Outros são classificadas somente para a
  síntese analítica e permanecem auditadas, sem alterar a base.
- Referências seguem a ABNT NBR 6023:2025; a seção final adota o título
  **Limitações e especificidades**.

## Esforço incremental e estatística

- Todas as UAs observadas, as UAs comuns pareadas e os grupos por ano de
  entrada passam a ser apresentados como leituras complementares.
- Cada tema do relatório detalhado contém a série iniciada no primeiro ano e
  os painéis iniciados nos anos de ampliação do esforço.
- Figuras próprias dos relatórios são produzidas a partir das séries e dos
  resultados estatísticos já materializados, sem copiar PNGs técnicos nem
  recalcular testes.
- A chave canônica de formação passa a associar corretamente os testes aos
  painéis de proporção.
- Gráficos técnicos e dos relatórios compartilham paleta semântica, símbolos,
  significados, posição e legenda auxiliar.
- Eixos e áreas de anotação são adaptativos. Rótulos que não cabem na barra
  usam corredor externo e linha de ligação; pares insuficientes usam travessão;
  o símbolo da composição conjunta é omitido quando existe uma única categoria.
- Auditorias registram corte, congestionamento, símbolos e treinamento das
  escalas. As cinco categorias gerais, suas cores e a legenda são validadas
  antes da gravação.

## Sentinel-2 e apresentação institucional

- A seleção orbital prioriza menor cobertura local de nuvens e sombras e usa a
  data mais recente como desempate.
- A janela de busca e o mosaico são ampliados progressivamente, com limites de
  aquisições e tempo; indisponibilidade orbital não bloqueia os demais
  relatórios.
- A extensão de exibição permanece separada da extensão técnica da consulta e
  lacunas após a reprojeção acionam nova tentativa.
- As marcas institucionais são incorporadas ao script. Monitora e CBC aparecem
  lado a lado com separação física de 7,5 mm em HTML, DOCX e PDF.
- Títulos e subtítulos são ajustados à largura física disponível para evitar
  cortes nos formatos finais.

## Curadoria e continuidade

- A nova opção `MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS`, com padrão `N`,
  permite acrescentar arquivos brutos omitidos a uma continuidade
  `painel_incremental_*`.
- A incorporação é atômica, idempotente e assinada na linhagem. Duplicatas
  idênticas são ignoradas; incompletudes e conflitos bloqueiam o lote inteiro.
- Operações assistidas de hábito persistem no atributo XLSForm físico correto
  para bromélia, cactácea, orquídea e samambaia, inclusive em bases legadas ou
  multiversão.

## Autossuficiência e desempenho

- As dependências dos produtos opcionais são declaradas no próprio arquivo R e
  verificadas somente quando o módulo correspondente é ativado.
- Com a incorporação de novas COLETAs desligada, o script não lista nem lê
  `input/novas_coletas/`.
- O bloco de inicialização rápida do RStudio, o contrato único, os itens
  congelados, os 13 modos, a linhagem e os produtos de dados permanecem
  preservados.
- Na homologação integral do PNCV no R 4.6 para Windows, foram processadas
  77.164 linhas e produzidos 208 PNGs, planilhas SISMONITORA e relatórios
  sintético e detalhado em Rmd, Markdown, HTML, DOCX e PDF. Os 15 produtos
  esperados passaram pelos respectivos gates.
- O tempo total foi 614,496 s; a etapa analítica, 66,841 s. A preparação
  gráfica levou 154,896 s, contra 157,951 s na candidata anterior.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.13.R`. O build exibido no console é
`v2.9.13-20260819`.
