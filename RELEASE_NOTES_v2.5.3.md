# v2.5.3 - Performance, barra de progresso e reprodutibilidade estatística

Publicação pública `v2.5.3` do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Otimizações de performance com `data.table` em preparação de dados, pareamento estatístico e exportações.
- Barra de progresso informativa com backend `cli`, percentual, etapa, detalhe e ETA.
- Atualização controlada da barra para manter acompanhamento visual sem penalização relevante de performance.
- Reprodutibilidade estatística para bootstrap e permutação Monte Carlo por semente base registrada.
- Checkpoints mais detalhados para diagnóstico de performance em correção de ponto metro e auditorias de COLETAS duplicadas.
- Preservação dos produtos finais públicos: `registros_corrig.csv`, `registros_corrig_stat.csv`, relatórios, gráficos PNG, KMLs e auditorias.

## Padrões públicos

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
```

## Arquivos principais

- `monitora_campsav_alvo_global_v2.5.3.R`
- `monitora_campsav_alvo_global.R`
- `R/monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`

## Observação de performance

A versão preserva a rotina de correção de ponto metro e registra seu tempo em checkpoint próprio, facilitando otimizações futuras sem alterar os produtos públicos desta publicação.
