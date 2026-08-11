# v2.9.5 — Integridade operacional e inicialização rápida no RStudio

A versão `v2.9.5` parte integralmente da última versão pública aprovada,
`v2.9.4`, e preserva o contrato único, os 13 modos públicos, a linhagem, o
replay, os produtos de dados, as planilhas SISMONITORA, as estatísticas, a
cartografia e a cautela causal. A versão concentra-se na operação cotidiana do
painel, na triagem de vegetação seca ou morta e na restauração definitiva do
início imediato pelo botão **Source** do RStudio para Windows.

## Inicialização no RStudio

- Foi restaurada integralmente a arquitetura de inicialização homologada na
  `v2.9.1`: o arquivo possui uma única expressão externa `base::evalq`, sem
  carregar, reler ou reinterpretar o próprio script.
- Foram removidas as tentativas intermediárias que consultavam o editor do
  RStudio ou faziam um segundo parse do arquivo de aproximadamente 4,6 MB.
- A proteção contra **Source with Echo** usa apenas a API oficial de preferências
  do RStudio e não depende de launcher, projeto, perfil ou arquivo auxiliar.
- O teste real no RStudio para Windows, pelo botão **Source**, iniciou a execução
  imediatamente após o clique. O gate automatizado confirmou a mesma estrutura
  externa e o mesmo volume mínimo de eco da `v2.9.1`.

## Triagem operacional de vegetação seca ou morta

- O relatório passa a orientar a busca de possíveis falsos positivos, com
  rastreabilidade por COLETA, UA, ano, forma de vida e grupo herbáceo/lenhoso.
- São integrados o percentual observado, a trajetória temporal e os atributos de
  impactos e manejo, inclusive menções compatíveis com fogo ou queima, sempre
  como contexto e nunca como prova causal.
- A classificação separa: suspeita de falso positivo; contexto ausente; padrão
  recorrente a acompanhar; ocorrência biologicamente plausível a revisar; e
  revisão rotineira.
- A COLETA é exibida nos resumos, trajetórias e demais produtos em que a
  identificação operacional é cabível.

Os critérios são ordenadores de triagem. Eles não corrigem automaticamente o
dado, não confirmam processos ecológicos e não substituem a conferência do
registro original.

## Justificativas de pendências

- O filtro por rótulo agora restringe efetivamente as linhas visíveis da tabela.
- **Selecionar todas as pendências filtradas** marca todo o conjunto exibido,
  permitindo aplicar uma justificativa comum a centenas de ocorrências.
- Justificativas adicionadas na sessão podem ser selecionadas individualmente,
  em conjunto ou integralmente e excluídas antes do salvamento.
- Inclusão e exclusão são atômicas: o lote inteiro é publicado somente após
  validação completa, sem estado parcial em caso de falha.
- Pendências e alertas espaciais remanescentes continuam incluídos no catálogo
  justificável e preservam identidade, responsável, classificação e timestamp.

## Painel, correções e cartografia

- A ordem operacional das abas é: **Correções de registros**, **Equipe da
  COLETA**, **Validação espacial** e **Justificar pendências**.
- O movimento assistido de forma de vida exótica para nativa preserva hábito e
  descritores compatíveis e limpa a ramificação exótica somente após validar a
  operação completa. O caso de samambaia da COLETA 42644 do PNCV foi verificado
  nos pontos 39, 40, 43, 45, 47, 49 e 50.
- O localizador cartográfico consulta limites oficiais de UC por serviço
  temporário do ICMBio/INDE e estados e biomas do IBGE, sem incorporar shapefile
  de UC, hardcode de unidade ou artefato espacial persistente no script.

## Relatórios analíticos e estatística

- Médias temporais mantêm símbolos derivados dos testes pareados materializados:
  aumento, redução, estabilidade demonstrada, resultado inconclusivo ou pares
  insuficientes.
- A auditoria liga cada símbolo ao resultado estatístico de origem e impede que
  ausência de significância seja apresentada como estabilidade.
- Os módulos analíticos e orbitais continuam opcionais e retornam antes de
  dependências, leitura ou processamento próprios quando desligados.

## Validação e desempenho

- Parse integral e contrato de release aprovados sobre os bytes finais.
- Estrutura de inicialização: uma expressão externa e 174 bytes de eco simulado,
  idênticos à referência `v2.9.1`.
- Homologação real PNCV concluída com todos os nove produtos obrigatórios, 174 de
  174 PNGs e duração total de 627,808 s.
- A triagem real de vegetação seca ou morta processou 404 COLETAS e 2.408
  ocorrências linha × forma de vida no PNCV; a mediana do núcleo caiu de 0,647 s
  para 0,518 s entre as revisões comparadas.
- Testes também cobriram PNM, integridade atômica das justificativas, localizador
  on-line, sanitização de coletores, relatórios estatísticos e planilhas de
  importação no SISMONITORA.
- A implementação usa caminhos compatíveis com Windows, Linux e macOS. O teste
  físico do RStudio foi realizado no Windows; os gates de código e execução
  também foram executados em Linux.

## Uso

Use `monitora_campsav_alvo_global_v2.9.5.R`. O build declarado no console é
`v2.9.5-20260811.1`.

Ao abrir o arquivo no RStudio, confirme **Sim** no aviso de tamanho e ajuste as
variáveis operacionais. Depois, use **Source** normalmente. Não é necessário
nenhum arquivo auxiliar para a inicialização rápida.
