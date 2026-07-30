# v2.8.1 — Fechamento hierárquico e persistência do TRIOUT

A versão `v2.8.1` corrige um caso residual de sanitização de “outra forma de
vida” observado em uma continuidade incremental da APAI. A correção é focal e
preserva o contrato consolidado, a interface, a fila semântica, os produtos e
os 13 modos públicos da v2.8.0.

## Causa corrigida

- O TRIOUT limpava corretamente o estado intermediário, mas o fechamento
  hierárquico posterior podia recalcular os campos superiores a partir de um
  descritor legado que ainda participava do estado reconciliado.
- Em filas com operações concorrentes, a pós-condição era confirmada cedo
  demais e não reconfirmava o objeto final depois de todas as operações,
  materialização e releitura.
- O token contratual atual `outros` precisava permanecer distinguível dos
  resíduos históricos `outra` e `outra_forma_vida`.

## Correção

- O fechamento do TRIOUT usa o contrato único para localizar somente
  dependências históricas válidas e recalcula a hierarquia sobre o estado
  efetivo final.
- A pós-condição é verificada depois da aplicação integral da fila e novamente
  depois da exportação/releitura do checkpoint.
- A precedência das correções específicas permanece protegida; campo inventado,
  alias genérico ou falha de localização continuam bloqueados.

## Compatibilidade preservada

- Nenhuma alteração na interface, nos observers do painel, nos IDs reativos, na
  lista “Atributos a corrigir”, na apresentação da fila ou nos códigos das
  operações semânticas.
- Nenhuma alteração nos 13 modos públicos, no schema final, na cardinalidade,
  no contrato XLSForms 2022–2025, no replay, nos produtos KML/KMZ ou na
  validação espacial.
- A comparação congelada da APAI exige somente as duas diferenças causais
  esperadas e reprova qualquer outra alteração de célula de negócio.

## Validação

- RED confirmado na v2.8.0 para as filas das rodadas 02 e 03 da APAI.
- GREEN focal na v2.8.1: `residuos=3->0` e `residuos=2->0`.
- Fluxo integrado de backend: 7.878 linhas por rodada, exportação, releitura e
  gate pós-exportação sem falhas.
- Fluxo real do painel em navegador: filtro de pendência, COLETA 17626,
  escopo de coleta inteira, botão de sanitização, prévia, confirmação de
  checkpoint, exportação e releitura; os pontos 11 e 23 ficaram sem resíduo.
- Regressão histórica real da COLETA 11140 aprovada nos pontos 18, 22 e 40,
  sem alteração de cardinalidade.
- Parse e testes focais aprovados no Linux e no Windows; o mesmo artefato foi
  carregado no RStudio para Windows com R 4.6.0 e hash idêntico.
- Carga integral comparativa APAI/PNB/PNM: 64.337 linhas × 267 colunas
  idênticas à v2.8.0, exceto pelo caminho temporal de extração; variação de
  +0,7% no tempo computacional e +0,8% no pico de memória, sem swap.
- Parse e regressões TRIOUT/APAI, espacial e KML/KMZ aprovados sobre o script
  funcional.
- Comparação estrutural: seis funções alteradas; nenhuma função de UI, KML,
  validação espacial ou relatório foi modificada.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.8.1.R`. O build declarado no console é
`v2.8.1-20260730.1`.
