# Monitora Campestre-Savânico v2.9.8

Publicação de 12 de agosto de 2026, baseada integralmente na v2.9.7 aprovada.

## Falha recuperável no painel

A v2.9.8 corrige o encerramento de uma sessão quando a prévia do motor único
encontra uma precondição incompatível. A primeira falha agora interrompe o plano
antes de qualquer finalização subsequente, informa operação, COLETA, atributo,
status e causa e preserva a auditoria detalhada. Uma fila idêntica que já falhou
não é recalculada repetidamente.

O checkpoint recuperável passa a conservar, em uma única transação:

- correções de campos e o histórico de intenções;
- operações espaciais;
- justificativas ativas e encerradas;
- auditorias e diagnóstico exato da falha.

O checkpoint é restaurado automaticamente somente quando sua impressão digital
corresponde à mesma base. Ele é removido apenas depois de salvamento concluído
ou descarte explícito. O formato legado que continha somente justificativas
continua reconhecido com os gates de compatibilidade aplicáveis.

## Precondição bruta em lotes de atributos superiores

Nas operações futuras de campo superior em lote, o texto amigável exibido no
painel deixa de ser reutilizado como trava transacional. Cada item auditável
preserva o valor bruto exato da respectiva COLETA, inclusive espaços nas
extremidades. A solução é geral para os atributos superiores processados por esse
fluxo e não contém regra específica para UC, COLETA ou atributo.

Checkpoints integrais produzidos pela primeira candidata da v2.9.8 podem ser
reidratados uma única vez quando a base é a mesma e a divergência se limita,
comprovadamente, a espaços nas extremidades. Divergências materiais permanecem
bloqueadas. A migração produz auditoria própria e não altera a base.

## Relatório de validação

Foi corrigida a chamada que tentava acessar `data.table::unique`, função não
exportada pelo pacote. O relatório usa o genérico `unique` do R, com despacho
adequado para `data.table`, sem mudar o conteúdo esperado.

## Compatibilidade, desempenho e preservação

- A arquitetura externa de uma única expressão `base::evalq`, homologada para
  o botão **Source** do RStudio no Windows, permanece inalterada.
- A migração real do checkpoint da run19 levou 0,016 s.
- A prévia do lote real de 26 COLETAS levou aproximadamente 6,96 ms por
  avaliação, incremento de cerca de 0,78 ms em relação à candidata anterior.
- Módulos opcionais permanecem sem custo próprio quando desligados.
- Contrato único, itens congelados, 13 modos, linhagem, replay, produtos de
  dados, SISMONITORA, estatísticas, relatórios analíticos, Sentinel-2,
  cartografia, KML/KMZ e cautela causal foram preservados.

## Homologação

Na base real do PNCV usada na run19, com 77.164 linhas, foram recuperadas 28
operações semânticas e 53 itens auditáveis. O lote afetado continha 26 COLETAS
e 2.626 linhas-alvo. Oito precondições foram reidratadas nas três filas do
checkpoint; depois da migração, 53 de 53 precondições coincidiram exatamente
com a base. Uma divergência material simulada foi bloqueada.

Também foram aprovados parse em R no Linux e no Windows, preservação de uma
única expressão externa, motor único com 30 operações reais, checkpoint na
escala de 3.583 justificativas e restauração após reabertura.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.8.R`. O build exibido no console é
`v2.9.8-20260812.1`.
