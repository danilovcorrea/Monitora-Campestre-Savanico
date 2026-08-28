# Monitora Campestre-Savânico v2.9.21

Publicada em 28 de agosto de 2026 sobre a v2.9.20.

## Correção contratual

- O fechamento hierárquico agora cruza cada dependência `relevance` com as
  `choices` vigentes do campo pai. Uma dependência órfã não pode mais criar um
  token fora do domínio.
- O token histórico `outros` não é choice válida de `forma_vida_exotica` no
  XLSForm 21FEV25. Ele permanece distinto de “outra forma de vida”, do token
  histórico `outra` e de `outros` no contexto de impactos/manejo.
- O preenchimento de uma folha textual de outra espécie exótica fecha somente
  os ancestrais semânticos válidos do contrato: espécie presente, categoria
  exótica, forma específica e token de outra espécie na lista correspondente.
  O módulo histórico não é reclassificado.

## Compatibilidade com checkpoints anteriores

- Checkpoints que conservem o ramo órfão `outros` podem ser reconciliados
  automaticamente quando existe exatamente uma forma exótica válida já
  marcada, o texto de espécie é não vazio, o destino está vazio ou contém o
  mesmo texto e não há espécie nomeada ou ancestral conflitante.
- A transação preserva literalmente o texto, remove o token órfão, preenche o
  ramo contratual correto e registra cada célula alterada.
- Mais de uma forma válida, texto divergente, espécie incompatível ou qualquer
  ambiguidade bloqueiam a migração inteira sem alteração parcial.

## Homologação

- A v2.9.20 reproduziu o defeito ao reinjetar `outros`; a candidata eliminou o
  comportamento no mesmo teste.
- O contrato XLSForm embutido foi comparado por corpo de função, objeto
  materializado e serialização: igualdade integral com a v2.9.20.
- Três esquemas reais com 184, 187 e 190 colunas resolveram o contrato sem
  ambiguidade.
- No caso real de 20.705 registros, a execução alterou exatamente 20 células
  nos quatro registros autorizados, preservou linhas, colunas e todas as demais
  células, e produziu `registros_validados.csv` com zero bloqueio contratual.
- O fluxo completo gerou 448 artefatos, incluindo três planilhas SISMONITORA,
  108 gráficos, relatórios analíticos nos cinco formatos, KML/KMZ e relatório
  consolidado.
- No Windows R 4.6.0, contrato, caso real e desempenho passaram. O fechamento
  levou 1,09 s na candidata contra 0,59 s na baseline, dentro do gate, e o
  arquivo CRLF conserva 662.986 bytes de margem sob o limite de 5 MiB.

## Preservação

- O XLSForm mais recente e o contrato único embutido não foram alterados nem
  flexibilizados.
- A v2.9.20 pública permanece byte a byte intacta.
- Inicialização rápida, painel fora do fechamento alcançado, estatísticas,
  relatórios, cartografia e demais produtos permanecem preservados.
- A migração é geral, derivada do contrato e sem hardcode por UC, COLETA,
  ponto, táxon ou pessoa.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.21.R`. O build exibido no console é
`v2.9.21-20260828-r01`.
