# v2.5.2 - Painel espacial guiado, controle XLSForm e governança de privacidade

Esta versão consolida a revisão funcional validada do painel espacial e adiciona produtos/auditorias para tornar o fluxo de curadoria mais claro e rastreável.

## Destaques

- Painel espacial redesenhado como origem → destino → operação.
- Filtros dependentes por Status espacial, UA, ANO e COLETA.
- Origem espacial independente do destino, permitindo copiar coordenadas de outro ano da mesma UA.
- Lote de destino filtrado, com botão para usar todas as COLETAS filtradas.
- `output/registros_importados.csv` passa a ser opcional e desligado por padrão público seguro; use somente em execução local deliberada.
- Reordenação técnica de `registros_corrig.csv`.
- Controle de entrada baseado no contrato XLSForm, com bloqueio imediato de tokens, datas e coordenadas inválidos.
- Operação composta para forma de vida com hábito obrigatório em correção individual/ponto.

## Configurações públicas seguras

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
```

Nos modos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_registros_corrig`, o script força internamente a abertura do painel.

## Privacidade

`registros_importados.csv` fica desligado por padrão público seguro e deve ser gerado apenas em execução local deliberada. O release público não deve conter `input/`, `output/`, `log/`, `extracted/` nem produtos de dados.

## Observação

Lotes com forma de vida que exige hábito permanecem bloqueados por segurança até validação específica futura. A operação composta com hábito foi validada para correção individual/ponto.
