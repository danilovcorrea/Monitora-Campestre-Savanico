# v2.3.1 - Painel de correções: limpeza atômica e escopo de lote

## Destaques

- Limpeza atômica auditável de outra forma de vida.
- Remoção de tokens históricos de outra(s) forma(s) de vida em listas de forma de vida nativa, exótica e seca/morta.
- Limpeza dos descritores históricos dependentes de outra forma de vida.
- Persistência validada no `output/registros_corrig.csv` final.
- Checkpoints de auditoria pós-aplicação e pré-exportação.
- Escopo do painel simplificado para:
  - coleta individual;
  - coletas do lote.
- `COLETAS do lote` passa a ser a lista explícita usada para operações em lote.
- Botão `Limpar filtros` no painel lateral.
- Labels de formas de vida baseados no XLSForm mais recente, mantendo names históricos vinculados internamente.
- Hábito restrito às formas condicionais previstas no XLSForm: bromelioide, cactacea, orquidea e samambaia.
- Painel preservado como desativado por padrão: `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.

## Validação operacional

- Limpeza de outra forma de vida validada com resíduos finais iguais a zero.
- Checkpoint pós-aplicação: resíduos iguais a zero.
- Checkpoint pré-exportação: resíduos iguais a zero.
- Correção assistida de falsa duplicata de UA preservada e auditada.

## Arquivo principal

```
monitora_campsav_alvo_global_v2.3.1.R
```
