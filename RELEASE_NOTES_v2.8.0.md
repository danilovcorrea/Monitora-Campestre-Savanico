# v2.8.0 — Validação espacial, produtos KML/KMZ e precedência operacional

A versão `v2.8.0` amplia os produtos espaciais e fortalece a aplicação atômica
das correções, preservando o contrato consolidado, a linhagem, o painel e os 13
modos públicos.

## Produtos espaciais

- `UAs_verg_ini_verg_fin.kml/.kmz` passa a ser um produto operacional sem dados
  primários da amostragem, mantendo `form_veg`, metadados das feições, ícones,
  cores e rótulos das UAs.
- `UAs_registros_corrig_stat.kml/.kmz` reúne os atributos estatísticos derivados
  de `registros_corrig_stat.csv` para validação institucional.
- `UAs_areas_operacionais_protecao_100m.kml/.kmz` representa uma área de
  proteção de 100 m em torno do ponto médio do transecto, sem preenchimento e
  com contorno amarelo.
- O rótulo da área operacional contém somente a denominação da UA; coleta, ano
  de referência espacial e demais metadados ficam na tabela de atributos.
- A geração de KMZ verifica `doc.kml`, o ícone incorporado e a integridade do
  arquivo compactado antes de concluir a exportação.

## Validação espacial

- Filtros de origem e destino passam a aceitar UC, UA e ano, preenchendo as
  coletas compatíveis sem exigir consulta prévia aos produtos de dados.
- Recomendações determinísticas são separadas dos alertas que exigem
  interpretação, evitando recomendações generalizadas de descarte de campanhas.
- Operações espaciais registram fonte, destino, abrangência, justificativa,
  linhas esperadas, linhas afetadas e resultado de aplicação.
- A sanitização espacial atua somente nos casos inequívocos e não substitui uma
  correção específica no mesmo escopo.

## Integridade das operações

- A precedência entre correções específicas e operações amplas é bidirecional:
  o resultado independe da ordem de inclusão na fila.
- A proteção cobre SANHAB, SANEORF, TRIOUT, correções simples ou em lote,
  movimentos de formas de vida e sanitizações espaciais.
- Movimentos individuais resolvem e limpam o hábito obrigatório na categoria de
  origem por meio do contrato canônico antes da reconciliação do estado.
- Triagem, relatórios, aplicação e gate final compartilham as mesmas identidades
  estáveis e preservam a linhagem em execuções incrementais.

## Organização dos produtos

- Produtos KML/KMZ ficam organizados no módulo espacial segundo sua finalidade.
- Nenhum dado de execução, input, output, log ou mídia integra a release pública.

## Compatibilidade preservada

- Nenhuma alteração na lista “Atributos a corrigir” ou na denominação das
  operações semânticas.
- Os 13 modos de execução, o replay semântico, a continuidade incremental e os
  gates contratuais permanecem disponíveis.
- `registros_validados.csv` continua condicionado à aprovação integral do
  contrato sobre `registros_corrig.csv`.

## Validação

- Duas runs encadeadas foram executadas para RBC, FNCS, PNM, PNB e PNCV:
  `painel_e_parar` seguido de `painel_incremental_completo`.
- 159.883 registros e identificadores foram preservados e materializados em
  `registros_validados.csv`.
- 74 eventos da primeira run foram herdados; 45 correções específicas e 3.860
  movimentos exótica → nativa foram acompanhados.
- Correções específicas foram verificadas antes e depois de SANHAB, SANEORF,
  TRIOUT, movimentos em lote e sanitização espacial.
- Foram produzidos e validados 15 KML e 15 KMZ.
- As dez execuções utilizaram o mesmo script, com SHA-256
  `22b6a186de53b0c66f075bf2c26f4001dadb182cae8ee9778c4ee1faf24ade52`.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.8.0.R`. O build declarado no console é
`v2.8.0-20260722.2`.
