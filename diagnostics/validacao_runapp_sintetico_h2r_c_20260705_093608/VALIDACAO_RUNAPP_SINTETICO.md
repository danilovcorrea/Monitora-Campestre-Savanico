# Rodada dedicada: `shiny::runApp()` real do painel com dataset sintético e stubs sintéticos das 45 constantes MONITORA_*

Data/hora: 2026-07-05. Branch: `dev-v2.6.2-h2r-cadeia-produtos`. HEAD no início: `91ab4f4`.

## Objetivo

Fechar a pendência documentada em
`diagnostics/validacao_fecho_painel_codetools_20260705_015121/VALIDACAO_FECHO_PAINEL_CODETOOLS.md`:
exercitar `shiny::runApp()` real de `monitora_correcao_painel()` (não um mock),
usando **somente** dataset sintético e stubs sintéticos das 45 constantes
`MONITORA_*`/`.MONITORA_*` já catalogadas, sem tocar `registros_importados*.csv`
nem qualquer dado real.

## Método (seguro por construção)

Script: `harness_runapp_sintetico.R` (neste diretório).

1. `parse()` do script inteiro (`monitora_campsav_alvo_global_v2.6.0.R`) — **não**
   `source()`.
2. Filtro AST **recursivo** (extensão do método usado em
   `fecho_painel_codetools.R`): extrai atribuições `nome <- function(...) {...}`
   mesmo quando aninhadas em blocos `if(){}`/`{}` de nível 0 (padrão de
   "dobra de seção" usado no script original — ex. `monitora_validados_schema_embutido`
   estava dentro de um `if (TRUE) { ... }` de nível 0). Nenhuma condição de
   `if` é avaliada; a navegação é puramente sobre a AST. Isso resolveu o gap
   dos "25 nomes `monitora_*` não capturados" da rodada anterior: capturamos
   **910/910** atribuições de função no arquivo (vs. 508 na rodada anterior).
3. Cada função é avaliada num `fn_env` (`parent = globalenv()`, após
   `library(shiny)`, `library(DT)`, `library(data.table)`) — define a função,
   não executa seu corpo nem argumentos default.
4. As 45 constantes do fecho são definidas em `globalenv()` **antes** da
   chamada, com valores sintéticos (diretórios/IDs isolados sob
   `diagnostics/.../tmp_isolado*/`) ou idênticos ao default literal do
   próprio script quando o default é config/estrutura, não dado real (ex.
   lista de colunas protegidas, esquema vazio de `MONITORA_LOG_EXECUCAO`/
   `MONITORA_PERF_EXECUCAO`).
5. `dt` e `meta_xls` são **100% sintéticos/fabricados**: 6 linhas, nomes de
   coluna emprestados da estrutura pública do XLSForm SISMONITORA embutido no
   próprio script (`monitora_validados_schema_embutido()`, 129 atributos —
   isso é *schema*, não dado real), mas todos os **valores** são fabricados
   (`SINTETICO-001`, `uuid-sintetico-001`, `UC_SINTETICA`, etc.).
6. `MONITORA_CORRECOES_DIR`/`MONITORA_LOG_DIR`/`MONITORA_OUTPUT_DIR` apontam
   para `diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/tmp_isolado*/`
   — nunca para `output/`/`log/` reais do projeto.
7. `shiny::runApp()` é chamado de verdade (é a função real do script, não um
   mock), com `Sys.setenv(MONITORA_PAINEL_LAUNCH_BROWSER="false")` e
   `options(shiny.port=38765, shiny.host="127.0.0.1")`.
8. Interação real com a UI via **Chrome headless** (`chromote`, já instalado —
   `/usr/bin/google-chrome`), clicando nos botões reais renderizados pelo
   Shiny, sem simulação de eventos R.

Nenhum arquivo de código de produção foi alterado. Nenhum dado real foi lido.

## Execuções realizadas

### Tentativa 1 — `timeout 20s`, sem interação (`tentativa1.log`)
`monitora_correcao_painel()` executa toda a fase de setup (schema, dicionário,
contrato, triagem, relatórios de ocorrências — todos gravados em
`tmp_isolado/`, nunca em `output/`/`log/` reais) e chega em
`runApp() será chamado agora; launch.browser=FALSE.` seguido de
`Listening on http://127.0.0.1:38765`. Processo encerrado pelo `timeout`
(exit 124), como esperado (não há navegador real para fechar a sessão).

### Tentativa 2 — `timeout 25s` + verificação HTTP real (`tentativa2_runapp_com_curl.log`, `curl_root_response.html`)
Com o servidor Shiny real no ar, `curl http://127.0.0.1:38765/` retornou
**HTTP 200**, 58.653 bytes de HTML. O HTML contém os textos reais da UI do
painel: `"Salvar correções e fechar"` e `"Fechar painel sem salvar e continuar
script"` — confirmação objetiva de que a UI real do painel de correções foi
servida por um `runApp()` genuíno.

### Tentativa 3 — clique real via Chrome headless em "Fechar painel sem salvar" (`tentativa3_fechamento_gracioso.log`, `chromote_fechar_gracioso.R/.log`)
`chromote` navegou até a página (título confirmado:
*"Monitora Campestre-Savânico - Alvo Global - Painel de correções assistidas"*),
clicou no botão real `id="cancelar"` ("Fechar painel sem salvar e continuar
script"). Como o dataset sintético tem 6 coletas com ocorrência impeditiva
("nativa sem forma de vida"), o painel abriu — corretamente — o modal de
confirmação. Clicamos no botão real `id="confirmar_encerrar_sem_materializar"`
("Cancelar execução sem materializar").

**Resultado: bug real de produção encontrado.** O observer correspondente
(linha 23291-23295 de `monitora_campsav_alvo_global_v2.6.0.R`) chama
`monitora_painel_encerrar_sem_materializar(...)` — **função que não existe em
nenhum lugar do arquivo** (confirmado por grep: única ocorrência do nome no
arquivo é a própria chamada, linha 23294; não há definição). O erro
`could not find function "monitora_painel_encerrar_sem_materializar"`
propagou pelo observer, pelo `tryCatch` interno de `monitora_correcao_painel`
(linha 23313-23319), até nosso harness, que capturou:
`"Painel encerrado sem ação explícita de salvar ou fechar. Nenhuma correção
foi aplicada. Reabra o painel e repita a operação desejada."`
O processo R terminou sozinho (não via `timeout`/SIGTERM) — encerramento
"gracioso" no sentido de que não travou nem exigiu kill externo, mas expôs um
caminho de crash real: **qualquer usuário que abrir o painel com pendências
impeditivas, clicar em "Fechar painel sem salvar" e depois confirmar
"Cancelar execução sem materializar" vai bater nesse erro.**

Este achado não apareceu na auditoria estática anterior
(`fecho_painel_codetools`) porque `monitora_painel_encerrar_sem_materializar`
caiu no grupo dos "25 nomes `monitora_*` não capturados pelo filtro" daquele
relatório — lá ficou registrado como gap a investigar; aqui, com execução
real, confirmamos que **não é um problema de captura, é ausência real de
definição no código de produção**.

### Tentativa 4 — clique real via Chrome headless em "Salvar correções e fechar" (`tentativa4_salvar.log`, `chromote_salvar.R/.log`)
Nova instância limpa (`tmp_isolado_salvar/`). Clicamos no botão real
`id="salvar"`. O modal de confirmação de pendências apareceu; clicamos no
botão real `id="confirmar_salvar_checkpoint_pendente"` ("Salvar checkpoint com
pendências").

**Resultado: sucesso completo, ciclo de vida real e gracioso do `runApp()`.**
O log mostra a sequência real de negócio: confirmação de pendências
registrada, "Nenhuma correção de campos pendente na sessão", "Nenhuma
correção espacial aplicável", `"Painel fechado sem correções novas; o script
continuará a partir deste ponto."`. `shiny::runApp()` retornou normalmente
(sem exceção), e `monitora_correcao_painel()` devolveu um `data.table`/
`data.frame` ao chamador — exatamente o comportamento esperado em produção
quando um usuário fecha o painel via o botão principal. Todos os arquivos de
auditoria/relatório foram gravados unicamente em `tmp_isolado_salvar/`
(verificado — lista completa abaixo).

## Verificação de isolamento (sem tocar dado real)

- `git status -sb` e `git diff --stat` no repositório: sem alterações em
  arquivos rastreados, em nenhum momento da rodada.
- Nenhum arquivo foi escrito em `output/`, `log/` ou `correcoes_campos/` reais
  do projeto — confirmado via `git status --porcelain -uall -- output log
  correcoes_campos` (saída vazia).
- Todos os artefatos gerados pela chamada real de `monitora_correcao_painel()`
  ficaram exclusivamente sob
  `diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/tmp_isolado/`
  e `.../tmp_isolado_salvar/` (30 arquivos CSV/TXT de auditoria/relatório —
  ver `find` no log da sessão).
- Nenhum processo R/Chrome/porta 38765 ficou residente ao final (verificado
  via `ps aux` e `ss -ltnp`).
- Os artefatos não rastreados sensíveis conhecidos
  (`diagnostics/backup_pre_commit_h2r_c_20260704_124238/`,
  `diagnostics/validacao_r_local_h2r_c_20260705_003935/`,
  `diagnostics/validacao_r_local_h2r_c_20260705_004113/`,
  `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`) permaneceram
  intactos durante toda a rodada.
- Nenhum commit, push, tag ou publicação foi realizado nesta rodada (fora do
  escopo autorizado).

## Limitações remanescentes

1. O harness usa um filtro sintático (AST) para capturar funções — mais
   completo que a rodada anterior (910/910 vs. 508/910), mas ainda depende de
   o padrão de definição ser `nome <- function(...)`, direto ou aninhado em
   `if`/`{}` de nível 0. Não testamos definições via `assign()` dinâmico (se
   houver alguma, não identificada nesta rodada).
2. Os stubs das constantes de progresso/performance (`MONITORA_PROGRESSO_*`,
   `MONITORA_PERF_*`) foram definidos como "desabilitado" para reduzir
   superfície de estado a replicar; isso significa que o comportamento da
   barra de progresso em si **não** foi exercitado nesta rodada (não é
   necessário para o objetivo, que era validar o ciclo de vida do `runApp()`
   do painel).
3. Datasets sintéticos cobrem 1 coluna editável (`forma_vida_nativa_arvore_abaixo`)
   e 2 fluxos de pendência (UAs duplicadas, nativa sem forma de vida). Outras
   combinações de estado (ex. sem nenhuma pendência, ou com correções de
   campo/espaciais efetivamente aplicadas) não foram testadas nesta rodada.

## Conclusão

`shiny::runApp()` completo **foi executado de verdade**, por múltiplas vezes,
com dataset e stubs 100% sintéticos, incluindo dois ciclos de vida completos
via clique real em Chrome headless: um que expôs um bug real de produção
(função ausente no caminho "cancelar execução sem materializar") e outro que
completou com sucesso do início ao fim (abrir → confirmar pendências →
fechar sem correções → retorno normal ao chamador). A pendência de "testar
`runApp()` completo do painel com dataset sintético" está **fechada** com
evidência objetiva. Fica como **novo achado, não como pendência desta
tarefa**, o bug real de `monitora_painel_encerrar_sem_materializar` ausente,
recomendado para correção em rodada própria (fora do escopo desta tarefa de
teste/validação, que não teve autorização para alterar código de produção).
