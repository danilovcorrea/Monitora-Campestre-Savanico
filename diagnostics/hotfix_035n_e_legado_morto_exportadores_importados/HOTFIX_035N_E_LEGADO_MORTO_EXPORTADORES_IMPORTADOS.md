# HOTFIX 035N-E - Legado morto dos exportadores de registros importados

Data: 2026-07-04

## Escopo

Hotfix observacional/minimo para marcar explicitamente que as primeiras
definicoes dos exportadores de registros importados sao legado sobrescrito
lexicalmente por definicoes vivas posteriores.

Nao houve remocao destrutiva, renomeacao de funcoes vivas, stage, commit, push,
tag ou release.

## Mapa 035N-D

- `monitora_registros_importados_exportar`
  - definicoes: linhas aproximadas 24584 e 33596
  - status: definicao 24584 morta por sobrescrita lexical; definicao 33596 viva
  - chamada viva: linha aproximada 34636
- `monitora_registros_importados_saneado_exportar`
  - definicoes: linhas aproximadas 24695 e 33645
  - status: definicao 24695 morta por sobrescrita lexical; definicao 33645 viva
  - chamadas vivas: linhas aproximadas 34691 e 38946

## Alteracao aplicada

Arquivo alterado: `monitora_campsav_alvo_global_v2.6.0.R`.

- Adicionado helper somente leitura
  `monitora_motor_unico_importados_implementacoes_vivas_035n_e()`, nao chamado
  pelo fluxo principal, retornando `data.table` com funcao, linha legada
  aproximada, linha viva aproximada, status e observacao.
- Adicionados comentarios normativos imediatamente antes das definicoes antigas:
  `LEGADO_SOBRESCRITO_035N_E`.
- Adicionados comentarios normativos imediatamente antes das definicoes vivas:
  `IMPLEMENTACAO_VIVA_035N_E`.

## Por que nao remover ou renomear nesta etapa

As definicoes antigas ja nao sao a implementacao operacional alcancada depois da
avaliacao sequencial do script, porque os mesmos nomes sao redefinidos
posteriormente. Remover ou renomear agora aumentaria o risco de alterar
linhagem, diffs de auditoria e dependencias indiretas em um hotfix que deve ser
minimo. A marcacao explicita reduz ambiguidade sem alterar fluxo operacional.

## Validacoes

- Branch auditada: `dev-v2.6.2-h2r-cadeia-produtos`.
- HEAD auditado: `496440f fix: consolida linhagem H2R-C de registros importados`.
- Upstream auditado sem fetch: `origin/dev-v2.6.2-h2r-cadeia-produtos`.
- Divergencia auditada sem fetch: `0 0`.
- H2R-C intacto:
  - uma ocorrencia de `produto_linha("registros_importados_operacional_pre_painel.csv"`.
  - uma ocorrencia de `produto_nome = "registros_importados_operacional_pre_painel.csv"`.
- 035N-A intacto:
  - zero ocorrencias de `reparar_csv_objeto("registros_corrig"`.
  - zero ocorrencias de `reparar_csv_objeto("registros_corrig_stat"`.
- 035N-B intacto:
  - `auditoria_registros_importados_resumo.csv` aparece como path fisico apenas
    dentro de `monitora_registros_importados_resumo_auditoria_gravar()`.
  - demais pontos localizados chamam
    `monitora_registros_importados_resumo_auditoria_gravar()`.
- 035N-C intacto:
  - candidato documental de
    `registros_importados_operacional_pre_painel.csv` existe em
    `monitora_doc_produtos_dados_descricoes()`.
- Definicoes preservadas:
  - `monitora_registros_importados_exportar`: 2 definicoes.
  - `monitora_registros_importados_saneado_exportar`: 2 definicoes.
  - ultimas definicoes continuam nas regioes vivas aproximadas.

- `git diff --check -- monitora_campsav_alvo_global_v2.6.0.R`: sem erros.
- `git diff --no-index --check -- /dev/null diagnostics/hotfix_035n_e_legado_morto_exportadores_importados/HOTFIX_035N_E_LEGADO_MORTO_EXPORTADORES_IMPORTADOS.md`:
  sem erros de whitespace reportados; exit 1 esperado por comparar arquivo novo
  contra `/dev/null`.
- Parse leve com `Rscript`: nao confirmado. O comando falhou antes do parse com
  `Failed to connect to system scope bus via local transport: Operation not permitted`.

## Riscos remanescentes

- As definicoes legadas seguem presentes no arquivo para preservar historico; a
  reducao de ambiguidade depende da marcacao normativa e da ordem lexical do
  script.
- A consolidacao definitiva ainda depende de migrar aliases/importacao para um
  contrato unico, evitando semantica hibrida entre produtos historicos e
  derivados.
- Este hotfix nao executou PNB, FNCS, pipeline pesado nem dados reais.

## Proximo passo recomendado

Migrar aliases/importacao para perfis derivados de
`monitora_contrato_unico_indices()`, sem semantica hibrida, como caminho para o
contrato unico dos produtos de registros importados.
