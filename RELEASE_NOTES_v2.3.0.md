# Release v2.3.0

Versão pública posterior à `v2.2.2`, com ampliação do Painel de validação - correções assistidas de `registros_corrig`, reforço da curadoria de COLETAS e novas travas de integridade antes da etapa estatística.

## Destaques

- Correção em lote por múltiplas COLETAS, com filtros superiores hierárquicos e multisseleção.
- Seleção automática de todas as COLETAS resultantes dos filtros superiores.
- Exclusão auditável de COLETAS filtradas/selecionadas.
- Triagem operacional de UAs duplicadas no mesmo ano.
- Preservação de COLETAS distintas com mesma UC + UA + ANO para correção manual, evitando deduplicação automática indevida.
- Trava pós-correções: a execução é interrompida quando conflitos UC + UA + ANO com múltiplas COLETAS permanecem não resolvidos.
- Auditorias pré e pós-correções para COLETAS com UAs duplicadas.
- Inclusão de `COLETA` em `registros_corrig_stat`, antes de `UC`, para maior rastreabilidade.
- Manutenção dos recursos da `v2.2.2`: integridade transacional, auditoria semântica, triagem de formas de vida exóticas, deduplicação defensiva e melhorias de performance.

## Arquivo principal

`monitora_campsav_alvo_global_v2.3.0.R`

## Cópias sincronizadas

- `monitora_campsav_alvo_global_v2.3.0.R`
- `monitora_campsav_alvo_global.R`
- `MONITORA_CAMPSAV_Alvo_Global.R`
- `R/monitora_campsav_alvo_global.R`
- `releases/v2.3.0/monitora_campsav_alvo_global_v2.3.0.R`

## Validação recomendada

1. Executar sem painel.
2. Executar com painel.
3. Testar correção em lote por múltiplas COLETAS.
4. Testar seleção automática de COLETAS pelos filtros superiores.
5. Testar exclusão auditável de COLETAS.
6. Testar conflito UC + UA + ANO com múltiplas COLETAS e confirmar interrupção quando não resolvido.
7. Conferir `registros_corrig.csv`, `registros_corrig_stat.csv` e auditorias de COLETAS duplicadas por UA/ano.
8. Conferir SHA256 idêntico nas cinco cópias públicas do script.
