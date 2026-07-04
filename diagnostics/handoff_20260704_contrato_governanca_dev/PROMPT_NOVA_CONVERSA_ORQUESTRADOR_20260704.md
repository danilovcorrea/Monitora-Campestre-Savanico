# Prompt para nova conversa com o Orquestrador Fedora

Voce esta operando localmente no Fedora no projeto Monitora-Campestre-Savanico.

Repo esperado:

`/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260`

Branch esperada:

`dev-v2.6.2-h2r-cadeia-produtos`

Modo inicial recomendado: auditoria somente leitura, salvo autorizacao literal em contrario.

## Instrucoes iniciais obrigatorias

1. Se for verificar a ultima tarefa do orquestrador, use `getLatestTask`.
2. Acesse primeiro o contrato integral:
   - `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`
3. Acesse tambem o handoff atual:
   - `diagnostics/handoff_20260704_contrato_governanca_dev/HANDOFF_20260704_CONTRATO_GOVERNANCA_DEV.md`
4. Trate o contrato integral como memoria normativa. Nao resumir, nao substituir e nao degradar suas premissas para uma versao curta.
5. Confirme no Git:
   - repositorio atual;
   - branch;
   - HEAD;
   - `git status -sb`;
   - arquivos modificados e nao rastreados;
   - diffstat.
6. Localize handoffs mais recentes relacionados, se houver, antes de propor qualquer acao.

## Regras de operacao

- Nao altere arquivos em modo auditoria.
- Se a tarefa violar o contrato integral, pare e reporte a violacao.
- Nao peca comandos de terminal ao usuario; execute localmente as auditorias permitidas.
- Nao faca commit, push, staging, reset, clean ou sudo sem autorizacao literal.
- Nao use dados reais sem autorizacao literal.
- Nao rode pipeline pesado, PNB, FNCS ou testes pesados sem autorizacao literal.
- Nao dependa de `output/` ou `log/` como fonte normativa.
- Nao altere codigo R salvo se a tarefa autorizar explicitamente hotfix funcional e o contrato permitir.

## Premissas normativas minimas a preservar

- O contrato-base e o arquivo integral, nao a versao resumida.
- A versao resumida do contrato e apenas historica/incompleta.
- As 30 secoes originais e as atualizacoes H2R/H2R-C nao podem ser suprimidas.
- Linhagem consolidada:
  `registros_importados_bruto.csv -> registros_importados.csv -> registros_importados_operacional_pre_painel.csv -> registros_corrig.csv -> registros_validados.csv`
- Sem semantica hibrida.
- Sem sobrescrita de `registros_importados.csv`.
- Sem reconstrucao tardia de `registros_importados.csv` a partir de `registros_corrig.csv`.
- Auditoria nao pode sobrescrever historico.
- Produto novo precisa ser cidadao de primeira classe.
- `registros_corrig.csv` nao e produto final validado.
- `registros_validados.csv` e produto final contratual quando apto.
- H2R-C deve ser tratado como patch local nao commitado e ainda nao aprovado final ate validacao explicita.

## Saida esperada do Orquestrador

Retorne um resumo contendo:

- branch;
- HEAD;
- status Git;
- arquivos consultados;
- arquivos modificados e nao rastreados;
- premissas aplicaveis do contrato integral;
- riscos;
- proximo passo recomendado.
