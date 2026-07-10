# v2.6.4 — Hotfix do contrato do painel, persistência e importação robusta

Publicação pública da versão v2.6.4 do Monitora Campestre-Savânico, hotfix da v2.6.3.

## Destaques

- Painel de correções com 98 atributos editáveis de fato: dropdown, auditoria e log ficam reconciliados, sem divergência silenciosa e sem atributo exposto que termine bloqueado tardiamente pelo botão "Adicionar correção".
- Domínios XLSForm normalizados: os 14 atributos que apareciam como bloqueados por "domínio ausente" foram resolvidos — a maioria por correção na correspondência entre nome de coluna e rótulo do XLSForm (nomes que carregavam aspas HTML herdadas do formulário de origem), e o caso restante (`UC`) por um fallback seguro de domínio observado, já que sua lista XLSForm é dinâmica/externa e nunca teve opções embutidas.
- `UA`/`EA`/`CICLO`/`CAMPANHA`, `Data`/`Horário` e coordenadas inicial/final seguem editáveis e validados (geopoint) no painel.
- `forma_vida_nativa_samambaia` como hábito obrigatório (`select_one`); `forma_vida_nativa_samambaia_sp` como texto livre de espécie.
- Persistência de correções pós-exclusão de coletas duplicadas (EXCCOL) por chave estável (`uuid_registro`/chave composta), sem usar o atributo corrigido como filtro de contexto de si mesmo.
- Importação de colunas estruturadas mais robusta: reparo determinístico de mojibake (UTF-8 decodificado como Latin-1) antes da consolidação de aliases e da tokenização, e resolução segura por deduplicação quando todos os tokens de uma célula residual são idênticos — nunca inventa dado.
- Diagnóstico específico (por COLETA/UA/coluna/faixa de pontos) no lugar do erro genérico anterior, quando uma lista de valores separada por `|` não tem correspondência segura com os pontos amostrais de uma coleta.
- Gráficos consolidados em `output/06_graficos`.

## Padrões públicos seguros

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
```

Os modos orientados a painel, como `painel_e_parar`, continuam acionando o Shiny de forma deliberada.

## Uso recomendado para bolsistas

1. Criar pasta limpa de execução.
2. Copiar `monitora_campsav_alvo_global_v2.6.4.R` para a raiz.
3. Criar `input/` e colocar ali somente os arquivos brutos.
4. Para curadoria assistida, usar `MONITORA_MODO_EXECUCAO <- "painel_e_parar"`.
5. Salvar e fechar o painel para materializar `output/01_produtos_dados/registros_corrig.csv`.
6. Preservar `output/02_painel_correcoes/correcoes_semanticas_consolidada.csv` como prova/replay da curadoria.

## Notas de compatibilidade

- Replay semântico `replay_semantico_v1` preservado, sem mudança de contrato.
- `registros_validados.csv` continua com 129 colunas no template validado, quando gerado sem pendências impeditivas.
- Dados de origem com listas de valores separadas por `|` sem correspondência segura com os pontos amostrais continuam bloqueados até correção manual — comportamento intencional, para proteger a integridade dos dados, não uma regressão.

## Privacidade

A release pública não inclui `input/`, `output/`, `log/`, `extracted/`, CSVs reais, XLSX reais, ZIPs de dados, KMLs reais, shapefiles, bancos ou produtos de execução local.

## Limitações conhecidas

- Um subconjunto de coletas de uma execução de validação apresentou, em um campo estruturado específico, listas de valores separadas por `|` sem correspondência segura com os 101 pontos amostrais esperados a partir de um certo ponto da coleta — assinatura consistente com instabilidade do lado do aplicativo/dispositivo de campo, não do script. Esses casos permanecem bloqueados até que os dados de origem sejam revisados e corrigidos manualmente pela equipe de coleta; isso é uma pendência de dados, não de código.
- O preview/modal de `ponto_sem_interceptacao` auto-resolvido por `solo_nu` pode aparecer de forma transitória; o gate final pós-painel é a fonte de verdade.
- A frente de contrato único experimental fica fora desta publicação.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.6.4.R`
- `monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `R/monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
- `INSTRUCOES_BOLSISTAS_v2.6.4.md`
