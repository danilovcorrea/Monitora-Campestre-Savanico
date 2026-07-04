# Índice de governança — contrato de desenvolvimento

Este diretório reúne as versões do contrato de governança e desenvolvimento
do Monitora-Campestre-Savanico. Este README existe para eliminar qualquer
ambiguidade sobre qual arquivo é normativo.

## Fonte canônica principal

**`CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`**

- É a base de desenvolvimento vigente.
- Incorpora as atualizações H2R/H2R-C e a linhagem de produtos sem
  supressão de premissas do contrato-base.
- Deve ser usado para orientar auditorias, hotfixes documentais, evolução
  incremental e revisões futuras do motor único.
- Em caso de divergência com qualquer outro documento deste repositório
  (resumo, handoff, prompt de orquestração ou espelho DOCX), **prevalece
  este arquivo**.

## Documentos derivados/auxiliares (subordinados ao INTEGRAL.md)

- `CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO.md` — versão consolidada
  resumida anterior. Mantida apenas como **registro histórico**. Não deve
  ser usada como contrato-base de desenvolvimento porque reduziu
  granularidade normativa em relação à versão integral.
- `CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL_04jul26.docx` —
  espelho DOCX do INTEGRAL.md, gerado para leitura/distribuição fora de
  Markdown. Não é fonte superior ao `.md`; em caso de divergência entre o
  DOCX e o `.md`, prevalece o `.md`.
- `CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_04jul26.docx` — espelho
  DOCX da versão resumida histórica, mesma relação de subordinação.

## Documentos relacionados fora deste diretório

- `diagnostics/handoff_20260704_contrato_governanca_dev/` — handoff e
  prompt de orquestração daquela etapa; são artefatos de transição de
  conversa, não normativos.
- `diagnostics/autonomia_agente_20260704/` — validação de autonomia
  operacional do agente (commit/push); não normativo sobre o produto.
- `diagnostics/arquitetura_real_materializada_20260704/` — leitura
  descritiva da arquitetura real do repositório, subordinada a este
  contrato quando houver conflito.
- `diagnostics/plano_executivo_motor_unico_20260704/` — plano executivo e
  status de implementação do motor único, referenciando as seções 27–28
  do INTEGRAL.md.

## Regra de não supressão

Nenhum documento derivado, hotfix documental ou resumo pode reduzir,
apagar, rebaixar ou reinterpretar por omissão as premissas do
`CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`. Ver seção 0
do próprio contrato integral.
