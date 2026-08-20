# Monitora Campestre-Savânico v2.9.14

Publicada em 19 de agosto de 2026 sobre a v2.9.13.

## Incorporação de novas COLETAs

- A preparação isolada de novos arquivos em modos `painel_incremental_*`
  passa a funcionar no R para Windows sem depender da passagem incompatível de
  variáveis de ambiente ao processo auxiliar.
- O script cria internamente um arquivo R temporário, configura o ambiente do
  processo filho e o remove ao final. Nenhum launcher ou arquivo externo passa
  a ser necessário.
- A leitura recursiva de `input/novas_coletas/` aceita CSV, XLSX, XLS e ZIP,
  inclusive subpastas e ZIPs aninhados.
- Planilhas XLS binárias passam pelo mesmo pipeline padrão de leitura e
  conversão das demais entradas reconhecidas.
- Console, inventário, auditoria de completude e diagnóstico de deduplicação do
  processo isolado são preservados antes da limpeza dos temporários.
- COLETAs quarentenadas ou incompletas bloqueiam atomicamente apenas a
  incorporação. Nenhuma linha parcial é anexada e o checkpoint anterior não é
  substituído.

## Edição contratual de UA

- O campo de UA deixa de ser uma lista limitada aos valores observados na base.
- O usuário pode informar qualquer unidade no intervalo contratual
  `UA-001_VgCS` a `UA-999_VgCS`, inclusive uma UA ainda ausente do conjunto de
  dados.
- A validação exata é aplicada na interface e repetida no servidor antes de a
  operação entrar na fila.
- O escopo superior da UA permanece inalterado: a correção alcança todas as
  linhas da COLETA selecionada.

## Preservação e desempenho

- Com `MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS <- "N"`, o retorno ocorre antes
  de listagem de arquivos, leitura, subprocesso ou escrita.
- A seção funcional de inicialização rápida do RStudio permanece idêntica à
  v2.9.13, exceto pelos identificadores de versão e build.
- No ensaio controlado no R para Windows, a v2.9.13 iniciou em 8,330 s e a
  v2.9.14 em 8,480 s até o mesmo ponto de parada, diferença de 0,150 s sem
  regressão material.
- Teste real no RStudio para Windows confirmou início imediato pelo comando
  **Source** e processamento completo da entrada sintética usada na
  homologação.
- Contrato único, demais controles do painel, modos, produtos, relatórios,
  linhagem e itens congelados permanecem preservados.

## Homologação

- A falha original da run PNCV foi reproduzida no Windows. O erro de abertura
  do processo foi eliminado; o novo diagnóstico identificou corretamente uma
  duplicidade do ponto 101 e a ausência do ponto 89 na COLETA 8190, sem anexar
  linhas à base anterior.
- Uma COLETA sintética completa com 101 pontos foi incorporada com sucesso no
  Windows.
- Entradas CSV, XLSX, XLS binário e ZIP aninhado foram processadas até
  `registros_corrig.csv`, com 101 linhas e 101 pontos.
- Repetição idêntica, idempotência, conflito factual, incompletude e validação
  dos limites de UA passaram pelos testes automatizados.
- Os testes de inicialização, regressão seletiva, precondição bruta e
  integridade sintática também foram aprovados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.14.R`. O build exibido no console é
`v2.9.14-20260819`.
