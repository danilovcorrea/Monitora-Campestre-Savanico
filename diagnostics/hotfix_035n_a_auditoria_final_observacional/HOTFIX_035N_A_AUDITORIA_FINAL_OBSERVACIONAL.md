# HOTFIX 035N-A - auditoria final observacional

## Motivacao normativa

H2R-C e uma transicao compativel rumo ao motor unico/contrato unico, nao a conclusao desse contrato. Nessa transicao, produtos centrais precisam manter linhagem clara por produto, camada e contexto. A auditoria final deve observar e registrar ausencia ou vazio, mas nao deve reconstruir nem sobrescrever produto central tardiamente.

O reparo tardio de `registros_corrig.csv` e `registros_corrig_stat.csv` dentro de `monitora_auditar_produtos_finais()` misturava responsabilidades: a etapa de auditoria podia gerar produto final a partir de objeto em memoria antes de declarar falha. Isso enfraquecia a rastreabilidade e mantinha semantica hibrida entre exportacao legitima e auditoria.

## Funcao afetada

- Arquivo: `monitora_campsav_alvo_global_v2.6.0.R`
- Funcao: `monitora_auditar_produtos_finais()`
- Alteracao: removidas as chamadas ativas a `reparar_csv_objeto("registros_corrig", ...)` e `reparar_csv_objeto("registros_corrig_stat", ...)`, junto com o helper local usado apenas para essa regravacao tardia.

## Contrato operacional

A auditoria final continua verificando presenca e tamanho por `produto_linha()`. Se `registros_corrig.csv` ou `registros_corrig_stat.csv` estiver ausente ou vazio, a auditoria deve reportar falha/presenca conforme o estado do arquivo, sem tentar reparar a partir de objetos globais.

O fluxo legitimo anterior de exportacao desses produtos nao foi alterado. Tambem nao houve alteracao nos exportadores de `registros_importados.csv` nem de `registros_importados_operacional_pre_painel.csv`.

## Validacoes estaticas executadas

- `git status -sb`
- `git branch --show-current`
- `git log -1 --oneline`
- `git rev-list --left-right --count HEAD...@{u}`
- `git diff --stat`
- `git diff --cached --stat`
- `rg` para chamadas ativas de `reparar_csv_objeto("registros_corrig"` e `reparar_csv_objeto("registros_corrig_stat"` na funcao auditada
- `rg` para ocorrencia de `produto_linha("registros_importados_operacional_pre_painel.csv"`
- `rg` para presenca de `produto_nome = "registros_importados_operacional_pre_painel.csv"`
- `rg`/leitura estatica do bloco pos-tokenizacao para confirmar que ele escreve o produto operacional proprio, nao `registros_importados.csv`
- `git diff --check`
- `Rscript` encontrado no ambiente, mas parse leve nao confirmado: a execucao foi bloqueada antes do parse por `Failed to connect to system scope bus via local transport: Operation not permitted`

## Riscos remanescentes

- Como a auditoria final deixou de reparar `registros_corrig.csv` e `registros_corrig_stat.csv`, execucoes que antes mascaravam falha de exportacao agora devem aparecer como falha de produto ausente ou vazio. Esse e o comportamento esperado para auditoria observacional.
- A mudanca nao conclui motor unico/contrato unico. Ainda e necessario auditar outros pontos de escrita tardia, fallback por objeto global e semantica hibrida na cadeia de produtos.
