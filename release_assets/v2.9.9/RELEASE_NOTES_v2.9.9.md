# Monitora Campestre-Savânico v2.9.9

Publicação de 13 de agosto de 2026, baseada integralmente na v2.9.8 aprovada e
no build homologado em 12 de agosto.

## Prévia atômica e recuperação do painel

A versão corrige uma falha observada ao combinar movimentos de formas de vida
com alterações de atributos superiores e listas de impactos em várias COLETAS.
Cada COLETA passa a conservar sua própria precondição bruta. Antes de executar
operações volumosas, o motor confirma toda a fila; se uma condição não coincidir,
nada é alterado.

A exceção deixa de escapar do cálculo reativo. O painel permanece aberto,
apresenta uma auditoria compacta e permite ao usuário atualizar a prévia,
revisar as operações ou salvar de forma controlada. A solução é geral e não
contém regra específica para UC, COLETA ou atributo.

## Relatório de validação

O relatório consolidado passa a ocupar `output/07_relatorio_validacao/`. Sua
estrutura foi reorganizada em resumo executivo, escopo, tratamentos,
ocorrências remanescentes, histórico, produtos, validação espacial, evidências
e continuidade. O texto evita terminologia desnecessariamente especializada;
inventários técnicos completos permanecem em CSV e são referenciados por
hiperlinks.

O documento ganhou capa, hierarquia visual, cartões, tabelas e estilos de
impressão. `README_OUTPUT.txt` e `indice_produtos.csv` apontam para o novo
caminho. Um produto antigo que colida com o destino não é apagado: é preservado
na área de legado.

## Manual do usuário

O manual passa a ser gerado em `manual_usuario/`, com fontes e dados de apoio.
Ele documenta os 13 modos, primeira validação, continuidade, geração de
produtos, painel, listas de conferência e glossário.

A nova variável abaixo controla exclusivamente o PDF:

```r
MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF <- "N"
```

Com o padrão `N`, nenhum renderizador de PDF do manual é consultado. Quando a
opção é `S`, o PDF é tentado somente após o fluxo principal e uma falha
documental não invalida os produtos de dados.

## Compatibilidade e desempenho

- O trecho anterior às variáveis manuais é idêntico ao da v2.9.8, excetuando
  versão e build.
- A execução integral real de RBG levou 799,578 s, contra 789,690 s da
  referência: variação de 1,25%, sem regressão significativa.
- A etapa analítica levou 72,081 s e o relatório de validação terminal 8,995 s.
- Contrato único, itens congelados, 13 modos, linhagem, SISMONITORA,
  estatísticas, mapas Sentinel-2, KML/KMZ e compatibilidade Windows, Linux e
  macOS permanecem preservados.

## Homologação

No checkpoint real recuperado de RBG, 27 ações auditáveis foram aplicadas. O
teste integral gerou `registros_corrig.csv`, `registros_validados.csv`, cinco
planilhas SISMONITORA, 144 PNGs, mapa Sentinel e relatórios sintético e
detalhado em Rmd, Markdown, HTML, DOCX e PDF. Todos os produtos obrigatórios
terminaram com status `ok`.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.9.R`. O build exibido no console é
`v2.9.9-20260812`.
