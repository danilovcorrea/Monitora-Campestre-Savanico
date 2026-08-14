# Monitora Campestre-Savânico v2.9.12

Publicada em 14 de agosto de 2026 sobre a v2.9.11.

## Mapa Sentinel-2

O mapa principal passa a representar o limite oficial da UC quando o contorno
intercepta a moldura cartográfica na escala das UAs. O limite usa a mesma cor
do localizador e aparece na legenda somente quando está efetivamente visível no
mapa principal. Quando a rede está integralmente no interior da UC, o limite
permanece no localizador sem criar um item de legenda sem correspondência
visual.

A geometria temporária já obtida para o localizador é reutilizada. A revisão
não incorpora referência espacial de UCs, não cria hardcodes e não realiza uma
consulta remota adicional.

## Composição cartográfica

A moldura e os rótulos de coordenadas passam a ocupar a mesma largura editorial
da faixa inferior que contém localizador, legenda, informações do mapa e
marcas. A prancha usa 2.800 x 3.200 pixels, faixa inferior de 21% e margem
externa comum próxima de 3 mm.

A proporção espacial da imagem orbital é preservada, sem estiramento do raster,
deslocamento das UAs ou alteração de escala por hardcode.

## Limpeza integral do painel

O botão geral **Limpar filtros** agora reinicia todos os filtros, campos
transitórios, buscas e seleções das abas Correções de registros, Equipe da
COLETA, Validação espacial, Justificar pendências e Auditoria opt-in.

O responsável pela sessão e as filas auditáveis de correções, operações
espaciais e justificativas permanecem preservados. Históricos e auditorias
também não são modificados. A limpeza atua somente sobre controles, estados
leves de seleção e proxies das tabelas; não recalcula a prévia integral, não lê
arquivos e não percorre os registros.

O botão **Limpar filtros espaciais** permanece restrito à aba espacial e não
altera as demais abas.

## Homologação

- Inventário automatizado: 74 de 74 controles editáveis classificados no
  contrato de limpeza.
- Teste real no RStudio/Windows confirmou início praticamente imediato após o
  clique em **Source**.
- O botão geral limpou todas as abas e preservou responsável e filas.
- O botão espacial limpou somente o módulo espacial.
- Duas execuções reais com o snapshot APAI foram concluídas de forma
  controlada, com 7.878 linhas, 184 colunas e nenhum bloqueio contratual
  XLSForm21.
- PNCV, PNCA e APAI preservaram mapas de 2.800 x 3.200 pixels, com moldura e
  coordenadas ocupando 97,82% da largura externa.
- O parse mediano da candidata foi 0,700 s, contra 0,736 s na revisão anterior,
  sem regressão mensurável de inicialização.

O contrato único, os itens congelados, os 13 modos públicos, a linhagem, os
produtos de dados e as revisões consolidadas na v2.9.11 permanecem preservados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.12.R`. O build exibido no console é
`v2.9.12-20260814`.
