# Monitora Campestre-Savânico v2.9.24

Publicada em 9 de setembro de 2026 sobre a v2.9.23.

## Entradas do SISMONITORA e CSV incremental

- Exportações XLSX com abas novas deixam de depender da posição da aba. A aba
  biológica é selecionada de forma unívoca pelo esquema esperado; ambiguidade
  ou ausência do esquema permanece impeditiva.
- A leitura preserva texto e codificação sem conversão silenciosa. Conversões
  aceitas exigem round-trip exato e ficam registradas em auditoria.
- O leitor de `registros_corrig.csv` testa em tempo de execução se a versão
  instalada de `data.table::fread` preserva aspas escapadas. Quando o teste
  falha, somente esse leitor incremental usa `utils::read.csv`; ambientes que
  passam no teste mantêm o caminho rápido.

## Painel e representações contratuais

- Representações físicas concorrentes do mesmo campo contratual são
  reconciliadas antes do painel apenas quando uma coluna está vazia ou quando
  os valores coincidem. Divergências bloqueiam a rodada sem mutação parcial.
- A intenção de correção somente entra na fila depois do preflight semântico
  completo. A revisão permite editar ou excluir material botânico inundável
  sem relaxar o domínio ou alterar o contrato.

## Relatório de validação

- O resumo informa separadamente arquivos da rodada atual e arquivos herdados
  de rodadas anteriores.
- Tratamentos são divididos em modificações do bolsista, modificações
  automatizadas e registros técnicos de auditoria, recuperação e conciliação.
  Auditorias deixam de aparentar correções de inconsistências.

## Relatórios analíticos

- Tabelas passam a explicitar `Nº de registros`, `Nº de pontos com presença` e
  os denominadores de UAs para cobertura e composição, com ordem padronizada
  antes de cobertura e proporção relativa.
- O esforço amostral de relatórios de uma única UC deixa de repetir “por UC”.
  Campanhas únicas recebem texto e escala próprios, sem faixa temporal
  artificial nem barras de erro ocupando toda a área do gráfico.
- Legendas de testes, explicações sobre seca/morta e textos de contexto de fogo
  são condicionados aos resultados efetivamente presentes e dirigidos ao
  leitor.
- A evidência de solo/rochas usa a categoria exata e o ano mais recente, sem
  incorporar o máximo de outra categoria ou de campanha anterior.

## Grafias históricas de UC

- Diferenças formadas exclusivamente por `de`, `da`, `do`, `das` ou `dos` são
  reconciliadas apenas nas cópias derivadas usadas por estatísticas e
  relatórios, prevalecendo a grafia da campanha mais recente.
- Dados-fonte, `registros_corrig`, linhagem e contrato permanecem inalterados.
  UCs materialmente distintas continuam impeditivas.

## Homologação e recuperações

- Gates focais e de contrato foram aprovados no Linux e no R 4.6.0 do Windows.
  A inicialização congelada do RStudio e o XLSForm 2025 são idênticos aos da
  v2.9.23; o arquivo permanece abaixo de 5 MiB mesmo estimado em CRLF.
- O caso real de RVSVOB preservou literalmente 2.048 aspas em cada uma das 101
  linhas da coleta afetada. A leitura no Windows concluiu em 2,87 segundos.
- O inventário atualizado foi recuperado em 22 produtos. Todos passaram por
  auditoria externa; as execuções aplicáveis materializaram mapa Sentinel antes
  da cópia para o OneDrive.

## Preservação

- O contrato único, o XLSForm 2025 e a inicialização do RStudio não foram
  alterados.
- Não foi criada regra por UC, coleta, pessoa, ponto, táxon ou dataset.
- A v2.9.23 e todas as releases históricas permanecem byte a byte intactas.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.24.R`. O build exibido no console é
`v2.9.24-20260909-r01`.
