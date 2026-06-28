# v2.5.6 - Auditoria cadastral não bloqueante e fechamento validado

## Principais mudanças

- `registros_corrig.csv` e `registros_validados.csv` refletem o fechamento validado do painel e do contrato XLSForm21.
- O aviso falso de bloqueio de `registros_validados.csv` foi removido.
- A auditoria de persistência aceita estados finais compatíveis por efeito diagnóstico.
- A limpeza de resíduos legados de outras formas de vida permanece vetorizada.
- `outra_forma_vida` permanece válida quando usada conforme o XLSForm 2025 com `forma_vida_outros`.
- O campo `amostragem/registro/forma_vida_seca_mortaarvore_abaixo` é preservado conforme contrato XLSForm/SISMONITORA 2025.
- Sanitização cadastral automática não bloqueante para casos inequívocos entre `CICLO`, `CAMPANHA`, `EA` e `UA`.
- Casos cadastrais suspeitos, mas não inequívocos, são registrados em relatório e não bloqueiam produtos.

## Defaults públicos

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
```
