# HANDOFF 2026-07-04 — Contrato de governanca, linhagem de produtos e proximos passos

## Clausula de leitura obrigatoria

Este handoff deve ser lido junto com `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md` e nao substitui o contrato-base. O contrato-base consolidado integral e a memoria normativa deste ciclo; este documento e apenas um handoff operacional para nova conversa do Orquestrador.

## Estado do repositorio observado

- Repositorio: `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260`
- Branch: `dev-v2.6.2-h2r-cadeia-produtos`
- HEAD: `cc5378e docs: audita linhagem da cadeia de produtos`
- Upstream observado: `origin/dev-v2.6.2-h2r-cadeia-produtos`
- Status Git observado antes deste handoff:
  - ` M monitora_campsav_alvo_global_v2.6.0.R`
  - `?? diagnostics/contrato_governanca_dev_consolidado/`
  - `?? diagnostics/hotfix_035m_d2_h2r_c_desambigua_registros_importados/`
  - `?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`
- Diffstat observado antes deste handoff:
  - `monitora_campsav_alvo_global_v2.6.0.R | 53 ++++++++++++++++++++++-------------`
  - `1 file changed, 34 insertions(+), 19 deletions(-)`

Observacao: o arquivo R modificado e os demais nao rastreados acima ja existiam antes deste handoff e nao foram alterados por esta tarefa.

## Jobs relevantes conhecidos

- `74b0a08e0d7c`: auditoria H2R-C.
- `fb23526b5827`: auditoria integracao produto operacional.
- `8f6f19bdddbc`: auditoria memoria/premissas.
- `1f0a907886f5`: versao resumida inicial incompleta.
- `2f0e4946ef42`: gerou DOCX resumido.
- `2e5ba12e28bf`: gerou/esta gerando versao integral corretiva, se finalizado.

## Decisao critica

A versao resumida do contrato nao deve ser usada como contrato-base. A versao normativa e a versao INTEGRAL.

## Caminhos normativos

- `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`
- `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.docx`

Observacao operacional: no estado observado, ha tambem caminho DOCX datado `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL_04jul26.docx`. O Orquestrador deve confirmar qual DOCX integral final existe antes de tratar qualquer DOCX como normativo.

## Caminhos de versao anterior resumida, apenas historica/incompleta

- `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO.md`
- `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_04jul26.docx`

Esses caminhos nao devem substituir o contrato integral.

## Premissas que nao podem ser suprimidas

- Todas as 30 secoes originais do contrato consolidado.
- Todas as atualizacoes H2R/H2R-C incorporadas ao contrato integral.
- Nenhuma premissa normativa pode ser reduzida a resumo operacional quando houver risco de perda de obrigacoes, excecoes, criterios de parada, trilhas de auditoria ou regras de governanca.

## Linhagem consolidada de produtos

Linhagem contratual consolidada:

`registros_importados_bruto.csv -> registros_importados.csv -> registros_importados_operacional_pre_painel.csv -> registros_corrig.csv -> registros_validados.csv`

## Regras criticas

- Sem semantica hibrida.
- Sem sobrescrita de `registros_importados.csv`.
- Sem reconstrucao tardia de `registros_importados.csv` a partir de `registros_corrig.csv`.
- Auditoria nao pode sobrescrever historico.
- Produto novo precisa ser cidadao de primeira classe.
- `registros_corrig.csv` nao e produto final validado.
- `registros_validados.csv` e produto final contratual quando apto.

## Estado H2R-C

H2R-C deve ser tratado como patch local nao commitado e ainda nao aprovado final. Permanecem riscos sobre:

- Integracao do produto operacional em catalogo.
- Integracao do produto operacional em indice.
- Auditoria final da cadeia de produtos.
- Resumo de auditoria ambiguo quanto a produtos intermediarios, produtos finais e produto operacional pre-painel.

## Proximo passo recomendado

Antes de qualquer hotfix funcional:

1. Validar a versao integral do contrato.
2. Fazer auditoria somente leitura para confirmar cobertura de secoes, incluindo as 30 secoes originais e atualizacoes H2R/H2R-C.
3. Somente depois, se aprovado, executar hotfix minimo de governanca H2R-C.

## Restricoes permanentes

- Nao fazer commit, push, staging, reset, clean ou sudo sem autorizacao literal.
- Nao usar dados reais.
- Nao rodar pipeline pesado sem autorizacao.
- Nao alterar codigo R sem autorizacao especifica e escopo funcional aprovado.
- Nao depender de `output/` ou `log/` como fonte normativa.
- Em modo auditoria, nao alterar arquivos.
