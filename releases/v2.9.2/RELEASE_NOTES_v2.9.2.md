# v2.9.2 — Integridade das correções, revisão espacial e rastreabilidade

A versão `v2.9.2` parte integralmente da última versão pública aprovada,
`v2.9.1`, e corrige comportamentos relatados durante a validação operacional do
painel. O contrato único, os defaults, os 13 modos, a linhagem, o replay e os
produtos centrais permanecem preservados.

## Atributos superiores e ações contratuais

- Campos de nível COLETA são aplicados a todas as linhas efetivamente
  observadas da COLETA, sem depender de uma contagem fixa.
- Campos `select_multiple` oferecem adicionar, remover ou substituir token.
- Campos textuais oferecem substituir, acrescentar texto ou limpar.
- O tipo, o escopo e as ações disponíveis derivam do contrato único.
- A prévia e a auditoria registram a abrangência real antes da aplicação.

## Movimento atômico entre formas de vida

- O painel valida toda a subárvore dependente antes de alterar qualquer célula.
- Lista de origem, lista de destino, hábito, espécies, descritores e campos
  exclusivos são transferidos como uma única operação.
- Ausência, ambiguidade ou conflito bloqueiam antes da escrita.
- A origem é limpa somente depois da transferência integral.

## Validação espacial

- As antigas seções por COLETA e por ANO foram reunidas em `Correção espacial:
  origem → destino → operação`.
- O usuário escolhe entre uma ou mais COLETAS ou um lote entre ANOS.
- O destino é preenchido quando os filtros resolvem uma única COLETA.
- A abrangência usa o número real de linhas; o formulário não solicita número
  esperado.
- A prévia mostra apenas as coordenadas que serão alteradas.
- `Limpar filtros` apaga filtros gerais e espaciais, COLETA e coordenadas sem
  remover operações já adicionadas à fila.

## Formação vegetacional

Foi criado diagnóstico não impeditivo para três situações:

- formação não uniforme dentro da COLETA;
- formação divergente entre coletas da mesma UA e ano;
- mudança de formação da mesma UA entre anos.

O diagnóstico gera ocorrência e relatório para revisão, mas não corrige nem
bloqueia automaticamente uma mudança ecológica legítima.

## Justificativas de pendências

A aba `Justificar pendências` registra eventos append-only com:

- ID estável da ocorrência;
- responsável e timestamp;
- classificação controlada;
- justificativa textual;
- estados vigente, encerrada por resolução e órfã.

Justificar não altera dados, não encerra a ocorrência por decisão textual e não
libera gates impeditivos.

## Relatórios analíticos

- Quando o mapa Sentinel é concluído, o mapa vetorial equivalente de
  continuidade não é gerado; o mapa de UAs por ano permanece.
- Título, texto, mapa completo e faixa institucional usam a mesma largura
  editorial em Rmd, Markdown, HTML, DOCX e PDF.
- A conversão DOCX remove somente as tags controladas pelo módulo e preserva o
  símbolo literal `<` dos resultados estatísticos.
- O status textual permanece restrito a `Validado`, `Em validação` e `Não
  validado`, sem `QA`.

## Desempenho e opções desligadas

- Relatórios analíticos, Sentinel e planilhas SISMONITORA permanecem
  desligados por padrão.
- Os caminhos em `N` retornam antes de dependências, I/O, rede ou
  materialização própria.
- A v2.9.2 preserva os gates por token e as otimizações contratuais da v2.9.0.

## Validação

- Testes seletivos aprovaram acréscimo de texto, ações por token, escopo real
  de COLETA, diagnóstico de formação, justificativas e DOCX.
- Casos reais de edição superior e movimento exótica para nativa foram
  reproduzidos e aprovados.
- O painel foi testado em navegador real com 16.766 linhas, incluindo reset,
  fluxo espacial unificado, justificativas e rótulos limpos.
- A execução integral com Sentinel terminou em 304,986 s, com 24 produtos
  auditados e nenhuma falha obrigatória.
- Foram aprovados quatro XLSX SISMONITORA, dez documentos analíticos e 47
  páginas renderizadas de PDF/DOCX.
- O botão Source foi testado no RStudio 4.6.0 para Windows: a primeira expressão
  começou em menos de um segundo. Houve execução real também em Linux/WSL2.
- A compatibilidade macOS foi auditada estaticamente; não se declara execução
  real sem host.

## Preservado

- contrato único e projeção SISMONITORA;
- defaults e 13 modos públicos;
- semântica ecológica e precedência;
- linhagem, replay, oráculo e gates impeditivos;
- produtos centrais, estatísticas, gráficos e KML/KMZ;
- opção de remoção/preservação de UUID;
- cartografia institucional e consulta temporária de limites oficiais;
- solução interna e autocontida de inicialização no RStudio.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.2.R`. O build declarado no console é
`v2.9.2-20260807.1`.
