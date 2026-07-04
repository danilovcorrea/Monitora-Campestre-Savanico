# Arquitetura real materializada — Monitora-Campestre-Savanico

Data: 2026-07-04
Branch auditada: `dev-v2.6.2-h2r-cadeia-produtos`
HEAD auditado no início desta etapa: `5780af270c3653f097b89f51ff29b7b1010316d1`

Este documento descreve o repositório **como ele realmente está**, não como
seria idealizado. É subordinado ao contrato canônico em
`diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`
(ver índice em `diagnostics/contrato_governanca_dev_consolidado/README.md`).
Em qualquer divergência, prevalece o contrato integral.

## 1. Script oficial

- **Script oficial real:** `monitora_campsav_alvo_global_v2.6.0.R` (raiz do
  repositório), referenciado pelo `README.md` público como script principal
  da versão `v2.6.0`.
- Contém a cadeia de 5 produtos pós-H2R-C (ver seção 3) e a função
  `monitora_contrato_unico_indices()` (linha aproximada 32496), que já
  deriva índices operacionais a partir do contrato único embutido
  (`monitora_contrato_unico_embutido()`).
- Também contém `monitora_motor_unico_importados_implementacoes_vivas_035n_e()`
  (linha aproximada 24584), helper somente leitura, não chamado pelo fluxo
  principal, que documenta em runtime quais definições de exportadores de
  registros importados são a implementação viva vs. legado sobrescrito
  lexicalmente.

## 2. Arquivos `.R` no repositório — o que é o quê

- **Oficial vigente:** `monitora_campsav_alvo_global_v2.6.0.R` (raiz).
- **Aliases de nome de release, desatualizados em relação ao v2.6.0
  vigente:** `monitora_campsav_alvo_global.R`, `MONITORA_CAMPSAV_Alvo_Global.R`,
  `R_monitora_campsav_alvo_global.R`, `R/monitora_campsav_alvo_global.R`.
  Comparação por hash SHA-256 mostra que esses quatro arquivos **não** são
  cópias do script oficial atual: são artefatos de preparações de release
  anteriores (tamanhos e hashes batem com versões `v2.5.x`, não `v2.6.0`).
  Presumivelmente gerados por `tools/preparar_release_publica_v2_4_0.py` (ou
  script equivalente) em execuções passadas e não atualizados desde então.
  Não foram alterados nesta etapa — fora do escopo desta tarefa, que é
  auditoria/documentação, não limpeza de artefatos de release.
- **Versões numeradas na raiz** (`monitora_campsav_alvo_global_v2.0.1.R` até
  `v2.5.6.R`): histórico de releases públicas anteriores, cada uma também
  presente em `releases/vX.Y.Z/` e/ou `release_assets/vX.Y.Z/`.
- **`archive/versoes_historicas/`**: snapshots datados de versões internas
  anteriores à numeração pública atual (2024–2026).
- **`releases/` e `release_assets/`**: cópias de release por versão,
  produzidas por `tools/preparar_release_publica_*.py`, incluindo variantes
  de nome de arquivo (`MONITORA_CAMPSAV_Alvo_Global.R`,
  `R_monitora_campsav_alvo_global.R`, etc.) para compatibilidade de
  distribuição.

### 2.1. Arquivo não rastreado: `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`

- **Classificação com evidência: backup/checkpoint histórico pré-H2R-C,
  obsoleto para uso corrente.**
- Evidência: `diff` linha a linha contra o script oficial mostra que este
  arquivo **não tem** a camada `registros_importados_operacional_pre_painel.csv`
  (a tabela de produtos nele lista 4 produtos, não 5) nem o bloco
  `MONITORA_DIAGNOSTICO_PIPES_CONTRATO` (03.5M-C). Ou seja, é uma foto do
  script **antes** dos commits H2R-C (`496440f`) e 035N (`3d85944`).
  Timestamp do arquivo (2026-07-03 19:11) é anterior a ambos os commits
  (2026-07-04).
- Não é uma variante "PNB flag off" ativa em uso — o nome sugere uma
  configuração pontual de teste (flag PNB desligada) capturada num momento
  específico do desenvolvimento 03.5L-C, e ficou solto na raiz sem commit.
- **Decisão desta etapa: preservar sem alterar, sem commitar, sem apagar.**
  Não é necessário para o funcionamento atual (o script oficial já superou
  este estado). Mantido como pendência deliberada de organização, conforme
  proibição explícita de mexer nele sem decisão documentada — esta seção É
  a decisão documentada: **classificação = backup obsoleto pré-H2R-C**,
  ação recomendada futura = mover para um diretório de backups (não
  deletar) numa etapa própria de limpeza, fora do escopo desta tarefa.

## 3. Cadeia de produtos (linhagem pós-H2R/H2R-C)

Definida no contrato integral (seções 6–17) e refletida em
`README.md` e nas tabelas de documentação embutidas no script oficial
(`monitora_doc_produtos_dados_descricoes()`, `arq_produtos`, linhas
aproximadas 1126 e 1367):

1. `output/01_produtos_dados/registros_importados_bruto.csv` — snapshot
   técnico e fiel da leitura/montagem da entrada bruta.
2. `output/01_produtos_dados/registros_importados.csv` — camada canônica de
   importação saneada, pré-transformação operacional pós-tokenização.
3. `output/01_produtos_dados/registros_importados_operacional_pre_painel.csv`
   — camada operacional pós-tokenização/pré-painel (H2R-C); **não
   substitui** as camadas 2, 4 ou 5.
4. `output/01_produtos_dados/registros_corrig.csv` — base corrigida,
   auditável, checkpoint operacional.
5. `output/01_produtos_dados/registros_validados.csv` — produto final
   contratual (opcional, quando `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = S`).

**Nenhum desses arquivos existe versionado no Git.** `output/`, `log/`,
`extracted/` e `input/` estão no `.gitignore`; os CSVs de linhagem são
produtos de execução, gerados em runtime a partir de dados reais de campo,
e nunca devem ser tratados como fonte normativa nem versionados. Verificado
nesta etapa: `git ls-files | grep registros_importados` só retorna
documentos `.md` de auditoria, nunca os `.csv` em si; não há alteração
rastreada pendente sobre esses arquivos.

**Regra de não reconstrução tardia (vinculante):** `registros_importados.csv`
e `registros_importados_bruto.csv` nunca devem ser reconstruídos a partir de
`registros_corrig.csv`, `registros_validados.csv` ou qualquer produto
posterior da cadeia. Cada camada é produzida uma única vez, na sua etapa
própria, a partir da entrada disponível naquele ponto do pipeline — nunca
projetada retroativamente a partir de uma camada mais avançada.

## 4. `output/` e `log/` como diagnóstico, nunca como fonte normativa

- `output/` contém produtos de dados, caches (`90_cache/`), correções
  (`correcoes_campos/`) e relatórios de execução — todos derivados de uma
  execução específica, com dados potencialmente sensíveis, nunca
  versionados.
- `log/` contém rastreabilidade de execução, performance e auditoria.
- Nenhum dos dois é fonte normativa. A fonte normativa é o contrato único
  embutido no script (`monitora_contrato_unico_embutido()`), conforme
  contrato integral, seção 4. Ausência de artefatos de execuções anteriores
  não pode mudar a interpretação contratual.

## 5. `diagnostics/` — natureza dos subdiretórios

`diagnostics/` concentra auditorias, hotfixes documentais, handoffs e
backups de segurança, nomeados por etapa (`03.5x`, `035x`). Não é um
produto de execução do script; é trilha de desenvolvimento e governança.
Destaques relevantes a esta etapa:

- `diagnostics/contrato_governanca_dev_consolidado/` — contrato canônico
  (ver README próprio, criado nesta etapa).
- `diagnostics/handoff_20260704_contrato_governanca_dev/` — handoff/prompt
  de orquestração de conversa anterior; não normativo.
- `diagnostics/autonomia_agente_20260704/` — validação incremental de
  autonomia operacional (etapa → commit → push), com smoke test e handoff
  final; contexto operacional, não normativo sobre o produto de dados.
- `diagnostics/hotfix_035n_e_legado_morto_exportadores_importados/` —
  mapeia definições duplicadas de exportadores no script oficial (viva vs.
  morta por sobrescrita lexical); relevante para qualquer futura limpeza de
  código morto, mas não altera comportamento.
- `diagnostics/backup_pre_commit_h2r_c_20260704_124238/` — ver seção 6.

### 5.1. `diagnostics/backup_pre_commit_h2r_c_20260704_124238/` (não rastreado)

- **Função:** snapshot de segurança tirado manualmente **antes** de um
  commit relacionado a H2R-C, em 2026-07-04 12:42. Contém:
  `arquivos_nao_rastreados_relevantes.txt` (lista dos arquivos não
  rastreados relevantes naquele instante), `git_diff_atual.patch`,
  `git_diff_stat.txt`, `git_log_1_oneline.txt`, `git_status_sb.txt`
  (estado do Git congelado naquele momento) e uma cópia de
  `monitora_campsav_alvo_global.R` e `README.md` como estavam antes das
  mudanças subsequentes.
- **Risco:** nenhum risco ativo — é um backup read-only, não é consumido
  por nenhum fluxo de execução nem por nenhuma função do script. O único
  risco é organizacional (permanecer não rastreado indefinidamente).
- **Decisão desta etapa: preservar sem alterar.** Não deletar, não mover,
  não versionar. É uma rede de segurança manual do usuário; versioná-lo
  misturaria artefato de diagnóstico pontual com histórico de código, sem
  ganho de rastreabilidade real (o próprio Git já é a rede de segurança
  formal a partir do momento em que os commits H2R-C existem, como de
  fato existem: `496440f`, `3d85944`).

## 6. Fontes normativas vs. derivadas — resumo

| Categoria | Exemplos | Papel |
|---|---|---|
| Fonte normativa | `CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`; `monitora_contrato_unico_embutido()` no script | Vinculante, única fonte de verdade |
| Derivado rastreável | `monitora_contrato_unico_indices()`; perfis/índices/caches do contrato único | Deve ser recalculável a partir da fonte normativa |
| Checkpoint auditável | `registros_corrig.csv` | Estado intermediário, não fonte, não produto final |
| Produto final | `registros_validados.csv` | Saída contratual, quando habilitada |
| Diagnóstico/execução | `output/`, `log/` | Nunca fonte normativa |
| Backup/segurança | `diagnostics/backup_pre_commit_h2r_c_20260704_124238/` | Rede de segurança manual, não normativo, não consumido em runtime |
| Obsoleto/histórico | `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`; aliases de release desatualizados (seção 2) | Preservado, não usado, não fonte |

## 7. Pendências para o motor único

Ver documento próprio:
`diagnostics/plano_executivo_motor_unico_20260704/PLANO_EXECUTIVO_MOTOR_UNICO_20260704.md`.

## 8. Riscos remanescentes observados nesta etapa

- Os aliases de release desatualizados na raiz (seção 2) podem confundir um
  desenvolvedor novo sobre qual é o script oficial; risco baixo (README já
  aponta o nome correto), mas vale uma limpeza dedicada futura.
- `diagnostics/hotfix_035n_e_.../` já identificou código morto por
  sobrescrita lexical no script oficial (duas definições de dois
  exportadores); ainda não removido, por decisão deliberada de manter
  histórico e reduzir risco de diff amplo.
- Os dois itens não rastreados (arquivo solto e backup) permanecem como
  pendência deliberada de organização, não como bloqueio funcional.
