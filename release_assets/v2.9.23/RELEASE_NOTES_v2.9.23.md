# Monitora Campestre-Savânico v2.9.23

Publicada em 1º de setembro de 2026 sobre a v2.9.22.

## Representação contratual de `select_one`

- A validação pré-painel passa a reconhecer valores já materializados como o
  label exato de uma choice `select_one`, resolvendo-os para o respectivo name
  quando a relação é unívoca no próprio contrato único/XLSForm 2025.
- Essa tradução existe somente na cópia usada pela validação. O dataset não é
  regravado e o valor originalmente observado permanece nos relatórios.
- Campos `select_multiple` continuam validados token a token, sem tradução de
  labels, alias local, normalização por caixa ou acento e sem flexibilização do
  domínio vigente.

## Identidade das ocorrências contratuais

- Ocorrências `token_fora_dominio_contrato` na mesma linha passam a incorporar
  caminho contratual, lista e token inválido em sua identidade semântica.
- A mesma composição discrimina as identidades persistentes usadas por
  justificativas. Ocorrências de campos diferentes deixam de colidir ou se
  duplicar durante a materialização dos relatórios.
- Nenhuma regra por UC, coleta, pessoa, ponto, táxon ou dataset foi criada.

## Homologação

- O gate RED reproduziu na v2.9.22 os falsos tokens de formação vegetacional e
  as colisões de identidade; a v2.9.23 passou nos mesmos cenários.
- No PNI, a candidata preservou somente 51 ocorrências reais de `outra` em 12
  coletas e eliminou 6.868 falsos rótulos de formação vegetacional.
- No RVSVOB, preservou 206 ocorrências reais — 202 `pastejo`, duas `outra` em
  nativas e duas `outra` em secas ou mortas — e eliminou 24.038 falsos rótulos
  de formação vegetacional.
- As auditorias materializaram 1.209/1.209 identidades no PNI e 2.170/2.170 no
  RVSVOB, sem duplicata semântica. A linhagem do RVSVOB manteve o hash previsto
  no manifesto.
- Testes focais, contrato integral, limite CRLF, regressões e desempenho foram
  aprovados no Linux e no R 4.6.0 do Windows. As duas runs chegaram ao painel
  e foram encerradas de forma controlada no RStudio do Windows.

## Preservação

- O XLSForm 2025 e o contrato único embutido permanecem integralmente
  inalterados.
- A inicialização congelada do RStudio permanece idêntica à da v2.9.22, exceto
  pelos identificadores de versão e build.
- A v2.9.22 pública e todas as releases históricas permanecem byte a byte
  intactas.
- Painel, semântica de correções, linhagem, produtos, relatórios, estatísticas,
  cartografia e módulos não alcançados conservam o comportamento publicado.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.23.R`. O build exibido no console é
`v2.9.23-20260901-r01`.
