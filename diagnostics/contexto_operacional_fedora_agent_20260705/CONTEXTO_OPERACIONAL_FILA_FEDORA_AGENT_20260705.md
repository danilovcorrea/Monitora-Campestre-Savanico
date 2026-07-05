# Contexto operacional obrigatório — fila Fedora Agent — 2026-07-05

Este arquivo registra o contexto normativo que deve ser considerado nas próximas filas/jobs do Fedora Agent para o desenvolvimento do projeto Monitora-Campestre-Savanico.

## Repositório e branch

Repositório local:

`/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260`

Branch esperada:

`dev-v2.6.2-h2r-cadeia-produtos`

## Auditoria somente leitura antes de qualquer alteração

Antes de qualquer alteração, o agente deve fazer auditoria somente leitura:

1. consultar a última tarefa real disponível pelo mecanismo de orquestração;
2. se houver tarefa recente, consultar status e summary antes de log completo;
3. confirmar branch, HEAD, upstream, ahead/behind;
4. confirmar `git status -sb`;
5. confirmar `git diff --stat` e `git diff --cached --stat`;
6. confirmar que os dois itens não rastreados abaixo continuam preservados:
   - `diagnostics/backup_pre_commit_h2r_c_20260704_124238/`;
   - `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`.

## Proibições operacionais

Não fazer:

- `reset --hard`;
- `clean` destrutivo;
- `sudo`;
- rebase destrutivo;
- tag;
- release formal;
- alteração fora do repositório;
- versionamento de dados reais;
- uso de `output/` ou `log/` como fonte normativa;
- alteração de comportamento padrão do script sem teste proporcional;
- piora de performance;
- toque nos dois não rastreados preservados sem decisão explícita.

## Fontes normativas principais

Contrato canônico integral:

`diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`

Índice de governança:

`diagnostics/contrato_governanca_dev_consolidado/README.md`

Arquitetura real materializada:

`diagnostics/arquitetura_real_materializada_20260704/ARQUITETURA_REAL_MATERIALIZADA_20260704.md`

Plano executivo do motor único:

`diagnostics/plano_executivo_motor_unico_20260704/PLANO_EXECUTIVO_MOTOR_UNICO_20260704.md`

Relatório de fechamento do motor e contrato únicos:

`diagnostics/fechamento_motor_contrato_unico_20260705/FECHAMENTO_MOTOR_CONTRATO_UNICO_20260705.md`

## Cadeia normativa de produtos

A cadeia normativa de produtos é:

`registros_importados_bruto.csv -> registros_importados.csv -> registros_importados_operacional_pre_painel.csv -> registros_corrig.csv -> registros_validados.csv`

Proibições críticas associadas:

- não reconstruir `registros_importados.csv` nem `registros_importados_bruto.csv` a partir de `registros_corrig.csv` ou `registros_validados.csv`;
- não dar duas semânticas ao mesmo arquivo;
- não usar camadas posteriores para reconstruir camadas anteriores;
- não usar `output/` ou `log/` como fonte normativa.

## Estado do trabalho no momento do registro

- O contrato único foi consolidado como fonte canônica.
- A arquitetura real foi materializada.
- O plano executivo do motor único foi criado.
- O fechamento do motor/contrato únicos foi documentado.
- Importação, pré-validação, pós-validação, exportação, estatísticas/gráficos e painel receberam incrementos seguros/opt-in.
- A aba opt-in do painel por contrato único foi adicionada.
- `shiny::testServer()` validou a aba.
- `shiny::runApp()` real controlado validou app mínimo da aba opt-in.
- Estava em andamento a tentativa de avançar testes Shiny/navegador/painel completo sem dados reais.

## Autonomia e remoto

Autonomia operacional autorizada pelo usuário para este escopo:

- `AUTORIZO_AUTONOMIA_ETAPA`
- `AUTORIZO_COMMIT`
- `AUTORIZO_PUBLICAR_REMOTO`

Interpretação segura neste estágio:

- commit local está autorizado dentro do escopo;
- push normal para a branch dev atual está autorizado dentro do escopo;
- tag e release formal não estão autorizadas sem autorização explícita futura.

## Motor e economia de tokens

Motor padrão: Claude.

Codex só deve ser usado quando houver autorização explícita do usuário ou fallback já autorizado. Quando Codex for usado, a tarefa deve incluir literalmente:

`USAR_CODEX_EXPLICITAMENTE`

Economia de tokens:

- para tarefas Fedora, usar summary antes de log completo;
- abrir log completo só se o summary estiver vazio, inconsistente, indicar erro, ou se o usuário pedir explicitamente;
- não consultar status interativo de cota/uso do Claude/Codex em modo não interativo;
- testes R determinísticos devem rodar localmente pelo Fedora/orquestrador;
- Claude/Codex devem consumir artefatos resumidos como `MANIFEST.json`, `.md` e `.json`, não executar R pesado.

## Próximo passo seguro

Continuar a partir do relatório de fechamento e do plano executivo, priorizando validação Shiny/navegador/painel completo com dataset sintético, sem dados reais e sem pipeline pesado.
