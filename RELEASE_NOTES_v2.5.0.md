# v2.5.0 - Validação espacial, cache do painel e curadoria incremental

## Destaques

- Validação espacial de COLETAS por coordenadas de início/fim de transecto, com consenso robusto por UC/EA/UA.
- Painel de correções assistidas com aba espacial, mapa, correções atômicas e auditorias.
- Modo `painel_e_parar` para curadoria assistida e gravação controlada de `registros_corrig.csv`.
- Modo `abrir_painel_cache` para reabrir o painel rapidamente sem reconstruir leitura, padronização e relatórios pré-painel.
- Modo `painel_incremental_registros_corrig` para continuar curadoria a partir de um `registros_corrig.csv` produzido pelo script.
- Regra obrigatória: arquivos de entrada somente em `input/`.
- Suporte a coordenadas manuais com altitude e acurácia opcionais.
- Relatórios de apoio do painel otimizados com `data.table`.

## Execução padrão

A versão pública permanece segura por padrão:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
```

Para curadoria assistida:

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"
```

## Arquivos principais

- `monitora_campsav_alvo_global_v2.5.0.R`
- `monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
