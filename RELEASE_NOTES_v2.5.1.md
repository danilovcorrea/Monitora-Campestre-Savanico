# v2.5.1 - Hotfix da abertura do painel de curadoria

Esta versão corrige um erro funcional introduzido na publicação `v2.5.0`.

## Corrigido

- Nos modos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_registros_corrig`, o script volta a forçar internamente `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"`.
- Com isso, o painel abre antes da trava de duplicatas `UC+UA+ANO`, permitindo exclusão/correção auditável de COLETAS.

## Mantido

- Defaults públicos seguros:
  - `MONITORA_MODO_EXECUCAO <- "completo"`
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`
  - `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"`
- Comentários revisados da série `v2.5.0`.
- Validação espacial, cache pré-painel e modo incremental a partir de `registros_corrig.csv`.

## Uso recomendado para curadoria espacial

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"
```
