# Validacao da matriz 2x2 - fechamento do painel Shiny (H2R-C)

Data/hora de execucao: 2026-07-05 10:24:35
Script de producao testado: `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260/monitora_campsav_alvo_global_v2.6.0.R`

## Contexto

Esta rodada valida a correcao aplicada em `monitora_painel_encerrar_sem_materializar`
(funcao que estava ausente, causando erro real de producao no fluxo
"Fechar sem salvar" -> "Cancelar execucao sem materializar", conforme
`diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/VALIDACAO_RUNAPP_SINTETICO.md`).

## Matriz executada

| Caso | Acao | Operacoes no painel | Passou |
|------|------|----------------------|--------|
| A | Fechar sem salvar | Sem operacoes | SIM |
| B | Fechar sem salvar | Com operacoes | NAO |
| C | Salvar checkpoint com pendencias | Sem operacoes | SIM |
| D | Salvar checkpoint com pendencias | Com operacoes | NAO |

## Detalhes por caso

### Caso A
- passou: TRUE
- precondicao_operacao_ok: TRUE
- contem_erro_funcao_ausente: FALSE
- encerrou_sozinho: TRUE
- log app: `log_caso_A_app.log`
- log chromote: `log_caso_A_chromote.log`

### Caso B
- passou: FALSE
- precondicao_operacao_ok: FALSE
- contem_erro_funcao_ausente: FALSE
- encerrou_sozinho: TRUE
- log app: `log_caso_B_app.log`
- log chromote: `log_caso_B_chromote.log`

### Caso C
- passou: TRUE
- precondicao_operacao_ok: TRUE
- contem_erro_funcao_ausente: FALSE
- encerrou_sozinho: TRUE
- log app: `log_caso_C_app.log`
- log chromote: `log_caso_C_chromote.log`

### Caso D
- passou: FALSE
- precondicao_operacao_ok: FALSE
- contem_erro_funcao_ausente: FALSE
- encerrou_sozinho: TRUE
- log app: `log_caso_D_app.log`
- log chromote: `log_caso_D_chromote.log`

## Isolamento

- Cada caso roda em `tmp_isolado_<CASO>/` proprio, com `MONITORA_CORRECOES_DIR`,
  `MONITORA_LOG_DIR`, `MONITORA_OUTPUT_DIR` e `MONITORA_ARQUIVO_CORRECOES_ESPACIAIS`
  isolados sob este diretorio de diagnostico.
- Nenhum dado real foi lido; dataset 100% sintetico (6 linhas fabricadas).
- Nao houve escrita em `input/`, `output/` ou `log/` reais do projeto.

## Resultado final: FALHA
