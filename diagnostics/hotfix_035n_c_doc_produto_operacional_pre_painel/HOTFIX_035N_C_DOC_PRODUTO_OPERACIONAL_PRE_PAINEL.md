# HOTFIX 035N-C - cidadania documental do produto operacional pre-painel

Data: 2026-07-04

## Escopo

Intervencao minima em `monitora_doc_produtos_dados_descricoes()` para completar a deteccao documental de `registros_importados_operacional_pre_painel.csv`.

O produto ja constava na tabela descritiva da funcao como camada operacional pos-tokenizacao/pre-painel, sem substituir `registros_importados.csv`, `registros_corrig.csv` nem `registros_validados.csv`, mas nao constava em `candidatos`. Isso podia marcar o produto como nao materializado nos metadados documentais mesmo quando existisse fisicamente.

## Alteracao aplicada

- Incluido `registros_importados_operacional_pre_painel.csv` em `candidatos`, com caminhos:
  - `output/registros_importados_operacional_pre_painel.csv`
  - `output/01_produtos_dados/registros_importados_operacional_pre_painel.csv`

## Nao alterado

- Nenhum README publicado.
- Nenhum produto de dados.
- Nenhum exportador.
- Nenhum mecanismo H2R-C, 035N-A ou 035N-B.

## Validacoes

- Git inicial: branch `dev-v2.6.2-h2r-cadeia-produtos`, HEAD `496440f fix: consolida linhagem H2R-C de registros importados`, upstream `origin/dev-v2.6.2-h2r-cadeia-produtos`, divergencia sem fetch `0 0`.
- `git diff --check -- monitora_campsav_alvo_global_v2.6.0.R diagnostics/hotfix_035n_c_doc_produto_operacional_pre_painel/HOTFIX_035N_C_DOC_PRODUTO_OPERACIONAL_PRE_PAINEL.md`: sem apontamentos.
- Entrada/candidato documental confirmado em `monitora_doc_produtos_dados_descricoes()`.
- H2R-C intacto: `produto_linha("registros_importados_operacional_pre_painel.csv"` ocorre 1 vez; `produto_nome = "registros_importados_operacional_pre_painel.csv"` ocorre 1 vez.
- 035N-A intacto: nenhuma chamada ativa de `reparar_csv_objeto("registros_corrig"` ou `reparar_csv_objeto("registros_corrig_stat"` dentro de `monitora_auditar_produtos_finais()`.
- 035N-B intacto: `auditoria_registros_importados_resumo.csv` e gravado fisicamente no helper `monitora_registros_importados_resumo_auditoria_gravar()`; demais pontos localizados chamam o helper.
- Parse leve com `Rscript -e 'parse(file="monitora_campsav_alvo_global_v2.6.0.R")'`: nao confirmado por bloqueio do ambiente, com erro `Failed to connect to system scope bus via local transport: Operation not permitted`.
