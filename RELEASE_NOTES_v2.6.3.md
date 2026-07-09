# v2.6.3 — v2.6.3 com painel, replay e produtos finais

Publicação pública da versão v2.6.3 validada do Monitora Campestre-Savânico.

## Destaques

- Painel operacional para curadoria assistida de bolsistas, com operações semânticas auditáveis.
- Operações validadas: EXCCOL, PENDHAB em lote e individual, SANEORF, TRIDESC/TRIOUT e correções simples/lote quando aplicáveis.
- Preservação do replay semântico por `correcoes_semanticas_consolidada.csv`, compatível com `replay_semantico_v1`.
- Geração de `registros_corrig.csv`, `registros_validados.csv` com 129 colunas quando não houver bloqueios, `registros_corrig_stat.csv`, estatísticas, gráficos, KMLs, manual e relatório consolidado.
- Correção documental pós-correção documental incorporada: seção 5.2 do relatório consolidado com leitura CSV robusta, tabela compacta e fallback contra cabeçalhos `V1..Vn`.

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
2. Copiar `monitora_campsav_alvo_global_v2.6.3.R` para a raiz.
3. Criar `input/` e colocar ali somente os arquivos brutos.
4. Para curadoria assistida, usar `MONITORA_MODO_EXECUCAO <- "painel_e_parar"`.
5. Salvar e fechar o painel para materializar `output/01_produtos_dados/registros_corrig.csv`.
6. Preservar `output/02_painel_correcoes/correcoes_semanticas_consolidada.csv` como prova/replay da curadoria.

## Privacidade

A release pública não inclui `input/`, `output/`, `log/`, `extracted/`, CSVs reais, XLSX reais, ZIPs de dados, KMLs reais, shapefiles, bancos ou produtos de execução local.

## Limitações conhecidas

- O preview/modal de `ponto_sem_interceptacao` auto-resolvido por `solo_nu` pode aparecer de forma transitória; o gate final pós-painel é a fonte de verdade.
- A auditoria de persistência por chave estável/UUID permanece como melhoria futura.
- A frente H2R de contrato único experimental fica fora desta publicação.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.6.3.R`
- `monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `R/monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
- `INSTRUCOES_BOLSISTAS_v2.6.3.md`
