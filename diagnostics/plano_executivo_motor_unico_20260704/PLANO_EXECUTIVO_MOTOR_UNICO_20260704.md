# Plano executivo do motor único — status e trilha (2026-07-04)

Este documento não substitui o plano normativo, que está nas seções 27
("Plano incremental do motor único") e 28 ("Critérios de aceitação do
motor único") de
`diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`.
Este documento é o **rastreamento operacional** de onde o script oficial
(`monitora_campsav_alvo_global_v2.6.0.R`) está, hoje, em relação a esse
plano — para que a próxima etapa não precise reauditar do zero.

## 1. O que já está implementado e em uso (verificado nesta etapa)

Etapas do plano normativo (seção 27) já cumpridas, com evidência de código:

1. **Contrato embutido** — `monitora_contrato_unico_embutido()` (linha
   aproximada 32120). Fonte única, sem dependência de XLSX/CSV/ZIP externo
   em runtime, conforme seção 4 do contrato.
2. **Perfis derivados / índices e caches** — `monitora_contrato_unico_indices()`
   (linha aproximada 32496). Recebe o contrato embutido e deriva
   `severidade`, `estágio aplicável` e demais anotações de orquestração
   **sem introduzir fatos novos** sobre o atributo (comentário normativo no
   próprio código confirma essa garantia).
3. **Diagnóstico opt-in conectado** — `monitora_contrato_unico_indices()` já
   é consumida por 6 pontos vivos do script, todos de natureza diagnóstica/
   comparativa, não de substituição do fluxo primário:
   - `monitora_contrato_unico_diagnosticar_observado_canonico()`
   - `monitora_registros_importados_diagnostico_contrato_unico()`
   - `monitora_registros_importados_comparar_ordem_legado_vs_contrato()`
   - `monitora_pipe_contrato_classificar_coluna()`
   - `monitora_pipe_contrato_resolver_tokens_isolado()`
   - `monitora_pipe_contrato_diagnosticar_dataset()`
4. **Camada de transparência sobre legado morto** —
   `monitora_motor_unico_importados_implementacoes_vivas_035n_e()` (linha
   aproximada 24584), helper somente leitura, não chamado pelo fluxo
   principal, que expõe qual definição de cada exportador de registros
   importados é viva vs. sobrescrita lexicalmente. Reduz ambiguidade sem
   alterar comportamento (ver
   `diagnostics/hotfix_035n_e_legado_morto_exportadores_importados/`).

## 2. O que ainda não está conectado (fontes paralelas continuam sendo o caminho primário)

No início deste plano, os seguintes itens da seção 27 **não tinham
evidência de conclusão**. As seções 6 a 11 registram os incrementos
posteriores executados em 2026-07-04; esta lista deve ser lida como
baseline inicial da auditoria, não como estado final do arquivo:

- conectar importação (uso determinante, não apenas diagnóstico);
- conectar pré-validação;
- conectar painel;
- conectar pós-validação;
- conectar exportação;
- conectar estatísticas e gráficos;
- remoção de fontes paralelas (só pode ocorrer após equivalência
  comprovada — nenhuma removida ainda, corretamente, pois nenhuma conexão
  determinante foi feita ainda).

Esta auditoria não teve orçamento de tokens/tempo para inspecionar cada um
desses seis pontos em profundidade (o script tem ~48 mil linhas); a
lista acima é o **próximo alvo de auditoria**, não uma auditoria concluída
de ausência.

## 3. Decisão desta etapa: não introduzir novo código

Havia espaço, em tese, para dar mais um passo incremental (ex.: conectar
diagnóstico de pré-validação a `monitora_contrato_unico_indices()`).
**Decisão: não fazer isso nesta etapa.** Motivos:

- O contrato normativo (seção 2) exige evitar mudanças simultâneas em
  importação, painel, validação e exportação, e exige teste proporcional a
  cada alteração de risco de schema/cardinalidade/performance.
- Esta etapa já entrega três documentos de governança/arquitetura novos;
  somar uma alteração funcional no script de ~48 mil linhas sem uma rodada
  de teste dedicada (que esta etapa não orçou, por instrução explícita de
  economizar tokens e evitar pipeline pesado) elevaria o risco de regressão
  sem necessidade.
- Os dois incrementos mais recentes de código (`3d85944` e o helper
  035N-E) já foram entregues em commits anteriores, cada um com sua própria
  auditoria de impacto zero. Empilhar mais um incremento de código na mesma
  etapa que também mexe em documentação ampla não é proporcional.

## 4. Próximo incremento recomendado (não executado agora)

Auditar **um único ponto de conexão determinante** por vez, começando pelo
mais isolado e de menor blast radius: `monitora_registros_importados_diagnostico_contrato_unico()`
já compara o observado com o canônico — o próximo passo natural é avaliar
se esse diagnóstico pode virar bloqueio/alerta determinante (não apenas
relatório) para casos de alta severidade, com teste antes/depois sobre uma
massa de dados sintética (nunca dados reais) e comparação de contagem de
linhas/colunas do produto gerado. Critério de aceite: zero mudança de
schema ou cardinalidade em `registros_importados.csv` para o mesmo input,
antes e depois da conexão.

## 5. Critérios de aceitação (seção 28) — status resumido

| Critério | Status |
|---|---|
| Perfis derivados da mesma fonte | Parcial — índices existem, mas fluxo primário ainda não depende deles em todos os pontos |
| Importação sem fontes paralelas | Parcial — 03.5L-C elevou divergências de alta severidade a alerta opt-in; fluxo primário ainda não é substituído pelo contrato |
| Pré-validação com diagnósticos suficientes | Parcial — 03.5M-C/03.5M-C2 conectaram diagnóstico/relatório opt-in de pipes por contrato único; sem novo bloqueio primário |
| Painel exibindo regras/severidade/origem/bloqueio | Auditado documentalmente em 03.5Q-C; não conectado ao contrato único nesta etapa por maior blast radius |
| Pós-validação determinando aptidão de `registros_corrig.csv` | Parcial — 03.5N-C declarou severidade contratual em resumo/alerta; aptidão final segue pelo fluxo existente |
| Exportação com 129 colunas quando apto | Parcial — 03.5O-C propagou severidade ao resumo de exportação; schema de exportação segue pelo fluxo existente |
| Índices/caches deriváveis | Sim — `monitora_contrato_unico_indices()` |
| Produtos com identidade/linhagem próprias | Sim — ver arquitetura real materializada, seção 3 |
| Testes demonstrando equivalência | Parcial — auditorias documentais existem; sem suíte automatizada dedicada |
| Performance aceitável | Não medido nesta etapa (sem pipeline pesado, por decisão de economia) |
| Documentação refletindo a cadeia de produtos | Sim — README, contrato integral e este conjunto de documentos |

O motor único **ainda não** atende integralmente à seção 28; está em
progresso real e verificável, não apenas planejado.

## 6. Incremento 03.5L-C executado (2026-07-04)

Executado exatamente o próximo incremento recomendado na seção 4, na sua
forma segura (alerta, não bloqueio — ver justificativa abaixo).

**Ponto auditado/conectado:**
`monitora_registros_importados_diagnostico_contrato_unico()` (e o merge de
metadados em `monitora_contrato_unico_diagnosticar_observado_canonico()`
do qual ela depende).

**Status antes:** diagnóstico 100% opt-in (flag
`MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS`, padrão "N"),
gerava só arquivos de relatório (mapa + resumo + txt) e um evento de log
sempre "INFO" — nenhuma coluna de alta severidade era destacada como tal;
`severidade` (já calculada em `monitora_contrato_unico_indices()`) não
chegava ao mapa observado→canônico.

**Status depois:**
- `meta_atributo` em `monitora_contrato_unico_diagnosticar_observado_canonico()`
  passa a incluir `severidade` (coluna já existente em
  `indices$por_atributo_canonico`; nenhum fato novo introduzido — mesma
  garantia normativa do restante da função).
- `monitora_registros_importados_diagnostico_contrato_unico()` calcula
  `colunas_alta_severidade_bloqueantes` (severidade "alta" E
  `bloqueia_migracao_automatica` = TRUE) e, quando > 0, eleva o evento de
  log de "INFO" para "AVISO" e emite `warning()` visível no console —
  citando o arquivo de diagnóstico a revisar. Continua estritamente
  opt-in (mesma flag, mesmo padrão "N"), continua sem tocar
  `registros_importados.csv` ou qualquer produto do pipeline: só altera o
  que é escrito em `output/diagnosticos_contrato_unico_registros_importados/`
  e no log de execução.
- **"Bloqueio" (interromper migração automática) foi deliberadamente NÃO
  implementado nesta etapa** — exigiria decidir o que "bloquear" significa
  no fluxo real de importação (que ainda usa fonte paralela, seção 2), o
  que é mudança de comportamento do pipeline primário e está fora do
  escopo de um incremento isolado e de baixo risco. Only "alerta
  determinante" foi entregue, que é o subconjunto seguro da recomendação
  da seção 4 e satisfaz o critério de aceite (zero mudança de schema ou
  cardinalidade).

**Fontes paralelas:** nenhuma removida (não havia equivalência a
substituir neste ponto — é diagnóstico, não substituição).

**Testes executados** (proporcionais, sem dados reais nem pipeline
pesado, ver `diagnostics/` para os scripts descartáveis usados em `/tmp`):
1. `Rscript -e 'parse(...)'` sobre o script completo — sintaxe OK.
2. Teste sintético isolando a função modificada com stubs das 3 dependências
   de contrato único (evita depender do contrato embutido real, que requer
   ~milhares de linhas adicionais de dump XLSForm fora do escopo deste
   incremento): 4 cenários — (a) sem colunas de alta severidade → evento
   INFO, sem warning; (b) 2 colunas de alta severidade bloqueantes →
   evento AVISO + warning com contagem e caminho do arquivo corretos; (c)
   coluna `severidade` ausente do diagnóstico (compatibilidade com
   chamadores/versões antigas) → degrada para INFO sem erro; (d) flag
   desligada → retorno `NULL`, no-op confirmado (custo zero preservado).
   Em todos os 4 cenários, confirmado que o `data.table` de entrada
   (`registros_importados`) permanece com as mesmas linhas/colunas antes e
   depois da chamada — critério de aceite da seção 4 cumprido.
3. Teste isolado do merge `meta_atributo` (novo `severidade` incluído)
   contra uma tabela `atributos` sintética — confirma join por nome
   correto, sem perda de linhas, sem colisão de coluna.
4. Grep estático: nenhum consumidor de `mapa`/`diagnostico` no script
   acessa colunas por posição (`[[n]]`); os dois consumidores existentes
   (`monitora_registros_importados_comparar_ordem_legado_vs_contrato` e
   `monitora_pipe_contrato_classificar_coluna`) selecionam por nome — a
   nova coluna `severidade` não quebra nenhum dos dois. Grep também
   confirma que a função alterada nunca escreve em
   `registros_importados.csv`/`registros_importados_bruto.csv`.

**Riscos remanescentes:**
- Sem teste dinâmico end-to-end contra o contrato embutido real (custo
  proibitivo para este incremento; mitigado pelos stubs fiéis ao schema
  real e pelo teste de merge isolado).
- A escalada para "AVISO" é nova visibilidade, não nova ação — usuários
  que hoje ignoram logs "INFO" começarão a ver "AVISO" quando houver
  colunas de alta severidade bloqueantes; isso é o comportamento
  pretendido (é "alerta determinante"), mas é uma mudança perceptível de
  saída de log que só se manifesta com a flag opt-in ligada.

**Próximo ponto determinante recomendado (executado a seguir, seção 7):**
dos seis pontos ainda não conectados (seção 2), o próximo de menor blast
radius após este é a pré-validação (`monitora_pipe_contrato_diagnosticar_dataset()`
já existe como diagnóstico) — avaliar o mesmo padrão de escalada
relatório→alerta para severidade alta ali, antes de considerar o painel
(maior blast radius por interação humana direta).

## 7. Incremento 03.5M-C3 executado (2026-07-04)

Executado exatamente o próximo incremento recomendado na seção 6, na sua
forma segura (alerta, não bloqueio — mesma justificativa da seção 6).

**Ponto auditado/conectado:** pré-validação de pipes por contrato único
sobre um dataset já em memória (tipicamente `registros_corrig`) —
`monitora_pipe_contrato_diagnosticar_dataset()` (per-coluna) e
`monitora_pipe_contrato_relatorio_optin()` (relatório/log opt-in,
consumida por `monitora_pipe_contrato_relatorio_optin_seguro()` em 3
pontos de integração já vivos do script).

**Status antes:** `monitora_pipe_contrato_classificar_coluna()` já
retornava `severidade`/`bloqueia_migracao_automatica` (herdados de
`monitora_contrato_unico_diagnosticar_observado_canonico()` desde a seção
6), mas `monitora_pipe_contrato_diagnosticar_dataset()` não os extraía de
`class_contrato_row` para o diagnóstico por coluna, e
`monitora_pipe_contrato_relatorio_optin()` só emitia evento "INFO",
sem distinguir pipes de alta severidade bloqueante.

**Status depois:**
- `monitora_pipe_contrato_diagnosticar_dataset()` passa a incluir as
  colunas `severidade` e `bloqueia_migracao_automatica` no diagnóstico por
  coluna (mesmo fato já calculado pelo contrato único; nenhum dado novo).
  Extração degrada com segurança (`NA`) quando o classificador não expõe
  essas colunas (compatibilidade com chamadores antigos).
- `monitora_pipe_contrato_relatorio_optin()` calcula
  `n_pipe_alta_severidade_bloqueante` (colunas com pipe, severidade "alta"
  e `bloqueia_migracao_automatica` = TRUE) e, quando > 0, eleva o evento
  de log de "INFO" para "AVISO" e emite `warning()` visível — citando o
  relatório a revisar. Continua estritamente opt-in (mesma flag
  `MONITORA_DIAGNOSTICO_PIPES_CONTRATO`, mesmo padrão "N"), continua sem
  alterar `registros` (nenhum produto real, incluindo
  `registros_corrig.csv`/`registros_validados.csv`): só o relatório/log
  em `output/diagnosticos_pipes_contrato/` ganham visibilidade.
- **"Bloqueio" real da pré-validação foi deliberadamente NÃO
  implementado** — mesma razão da seção 6: exigiria decidir semântica de
  bloqueio no fluxo primário, fora do escopo de um incremento isolado.

**Fontes paralelas:** nenhuma removida (diagnóstico aditivo, não
substituição).

**Testes executados** (proporcionais, sem dados reais nem pipeline
pesado, script descartável em `/tmp/teste_035m_c3/teste.R`):
1. `Rscript -e 'parse(...)'` sobre o script completo — sintaxe OK.
2. Teste sintético isolando as 3 funções alteradas/relacionadas
   (`monitora_pipe_contrato_diagnosticar_dataset`,
   `monitora_pipe_contrato_resumir_diagnostico_dataset`,
   `monitora_pipe_contrato_relatorio_optin`) com stub do classificador
   legado, do classificador de contrato e de `monitora_log_registrar_evento`:
   (a) flag desligada → retorno `NULL`, no-op confirmado; (b) 1 coluna com
   pipe de alta severidade bloqueante → evento AVISO + `warning()` com
   contagem e caminho do relatório corretos; (c) `data.table` de entrada
   (`registros`) com as mesmas linhas/colunas/nomes antes e depois da
   chamada — critério de aceite de zero mudança de schema cumprido; (d)
   classificador que não expõe `severidade`/`bloqueia_migracao_automatica`
   (compatibilidade com versão anterior) → diagnóstico degrada para `NA`
   sem erro, sem perder a coluna (bug de `data.table()` descartando coluna
   `NULL` identificado e corrigido durante o próprio incremento, antes do
   commit).
3. Grep estático: nenhum consumidor de `diagnostico`/`diag`/`resumo` no
   script acessa colunas por posição (`[[n]]`); todos os `fwrite`/
   `writeLines` das funções alteradas continuam gravando só em
   `dir_diag`/`output/diagnosticos_pipes_contrato/`; nenhuma referência
   cruzada a `registros_importados.csv`/`registros_importados_bruto.csv`
   dentro das funções de pré-validação de pipes.

**Riscos remanescentes:**
- Mesmo risco estrutural da seção 6: sem teste dinâmico end-to-end contra
  o contrato embutido real (mitigado por stubs fiéis ao schema já
  confirmado em `monitora_pipe_contrato_classificar_coluna()`).
- Mesma mudança perceptível de saída de log (INFO→AVISO) só se manifesta
  com a flag opt-in ligada — comportamento pretendido, não regressão.

**Próximo ponto determinante recomendado:** dos pontos ainda não
conectados (seção 2), restam painel, pós-validação, exportação e
estatísticas/gráficos. O painel tem maior blast radius por interação
humana direta; pós-validação (aptidão de `registros_corrig.csv`) é o
próximo candidato de menor blast radius, seguindo o mesmo padrão
relatório→alerta antes de considerar qualquer bloqueio real.

## 8. Incremento 03.5N-C executado (2026-07-04)

Executado o próximo incremento recomendado na seção 7, na sua forma segura
(enriquecimento de auditoria + alerta complementar, não bloqueio — mesma
justificativa das seções 6 e 7).

**Ponto auditado:** a governança real de bloqueios de
`registros_validados.csv` vive em duas funções, ambas já existentes e já
determinantes do fluxo primário (não diagnóstico opcional):
`monitora_registros_corrig_gravar_auditoria_contrato_xlsform21()` (calcula
`bloqueante`/`status_contrato` por atributo, a partir do schema XLSForm21
embutido em `monitora_validados_schema_embutido()`) e
`monitora_registros_validados_exportar()` (decide se
`registros_validados.csv` é gerado, bloqueado ou abortado, usando esse
mesmo cálculo). Essas duas funções usam uma fonte de schema/regras própria
(XLSForm21 embutido), paralela ao contrato único
(`monitora_contrato_unico_embutido()`/`monitora_contrato_unico_indices()`)
— nenhuma delas consultava severidade do contrato único antes deste
incremento. Importante: essa auditoria **nunca bloqueia
`registros_corrig.csv`** (confirmado lendo o call site em
`monitora_publicacao_aa_preparar_validar_registros_corrig()`, linha
~30223, que chama com `abortar = FALSE`) — só marca flags globais e
bloqueia `registros_validados.csv`.

**Ponto conectado (incremento desta etapa):**
`monitora_registros_corrig_gravar_auditoria_contrato_xlsform21()`.

**Status antes:** a auditoria por atributo calculava `bloqueante` a partir
de 6 critérios próprios (formato/domínio/condicional/sanitização/
cardinalidade/chave única), gravava CSVs de auditoria e resumo, e emitia
log "ERRO"/"INFO" conforme houvesse bloqueio — sem nenhuma referência a
severidade do contrato único.

**Status depois:**
- Novo helper `monitora_registros_corrig_severidade_contrato_unico()`
  (definido logo antes da função principal): reaproveita
  `monitora_contrato_unico_diagnosticar_observado_canonico()` — o mesmo
  mapeador observado→canônico já testado nos incrementos 03.5L-C e
  03.5M-C3 — para obter `severidade`/`bloqueia_migracao_automatica` por
  atributo (nenhum fato novo, nenhuma regra de match duplicada). É opt-in
  pela nova flag `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS`
  (padrão "N"): com a flag desligada, função ausente ou qualquer erro
  interno, retorna `data.table` vazio — degradação sempre segura.
- `monitora_registros_corrig_gravar_auditoria_contrato_xlsform21()` faz um
  left-join aditivo (por `atributo`, sem remover/duplicar linha) das
  colunas `severidade_contrato_unico` e
  `bloqueia_migracao_automatica_contrato_unico` na tabela de auditoria já
  gravada em CSV. O `resumo` ganha
  `n_bloqueios_alta_severidade_contrato_unico` (contagem de atributos que
  já eram `bloqueante` pela regra própria E têm severidade "alta" no
  contrato único). Quando essa contagem é > 0, emite um `warning()`
  complementar citando o arquivo de auditoria a revisar — visibilidade
  extra, não uma nova condição de bloqueio.
- **`bloqueante`/`status_contrato` (a regra que efetivamente decide se
  `registros_validados.csv` é bloqueado) permanecem calculados
  exclusivamente pelos 6 critérios próprios já existentes — o contrato
  único não participa dessa decisão nesta etapa.** Isso é deliberado: o
  critério de aceite da seção 4/6/7 (zero mudança de schema/cardinalidade)
  exige que nenhuma condição de bloqueio nova seja introduzida; conectar
  o contrato único à própria decisão de `bloqueante` — deixando de ser
  paralelo — é o próximo incremento, não este.

**Fontes paralelas:** nenhuma removida (enriquecimento aditivo, não
substituição; XLSForm21 embutido continua sendo a única fonte da decisão
`bloqueante`).

**Testes executados** (proporcionais, sem dados reais nem pipeline
pesado, script descartável em `/tmp/teste_035n_c/teste.R`):
1. `Rscript -e 'invisible(parse("monitora_campsav_alvo_global_v2.6.0.R"))'`
   sobre o script completo — sintaxe OK.
2. Teste sintético isolando as duas funções alteradas/novas com stub de
   `monitora_contrato_unico_diagnosticar_observado_canonico()` e demais
   dependências mínimas extraídas do próprio script-fonte (evita depender
   do contrato embutido real): 4 cenários — (a) flag desligada → colunas
   novas todas `NA`, contagem 0, custo da chamada ao contrato único
   evitado; (b) flag ligada com 1 atributo bloqueante de severidade alta
   → colunas enriquecidas corretamente, contagem 1, `warning()` emitido
   citando o arquivo certo; (c) flag ligada mas função do contrato único
   ausente → degrada para `NA` sem erro; (d) flag ligada mas função do
   contrato único lança erro internamente → degrada para `NA` sem
   propagar o erro. Em todos os 4 cenários, confirmado que a
   `data.table` de entrada (`auditoria_stub`) permanece com as mesmas
   linhas/colunas antes e depois das 4 chamadas — critério de aceite
   cumprido.
3. Grep estático: nenhuma referência a
   `registros_importados.csv`/`registros_importados_bruto.csv` dentro do
   helper ou da função principal; todos os `fwrite` da função principal
   continuam gravando exclusivamente em `arq_log`/`arq_out`/
   `arq_resumo_log`/`arq_resumo_out` (mesmos caminhos de antes, nenhum
   caminho novo introduzido); nenhum consumidor de `contrato`/
   `contrato_logico` no script acessa coluna por posição (`[[n]]`).

**Riscos remanescentes:**
- Mesmo risco estrutural das seções 6/7: sem teste dinâmico end-to-end
  contra o contrato embutido real (mitigado por stub fiel ao formato já
  confirmado de `monitora_contrato_unico_diagnosticar_observado_canonico()`,
  que já tem 03.5L-C/03.5M-C3 em produção usando o mesmo contrato).
- A decisão de bloqueio de `registros_validados.csv` continua 100% sob a
  regra própria XLSForm21 — o contrato único aqui é só anotação; se algum
  dia a intenção for usar severidade do contrato único para *decidir*
  bloqueio, isso é mudança de comportamento do fluxo primário e exige novo
  incremento dedicado com sua própria auditoria de risco, não uma
  extensão silenciosa deste.
- Mudança de saída perceptível (novas colunas nos CSVs de auditoria,
  `warning()` extra) só se manifesta com a flag opt-in ligada —
  comportamento pretendido, não regressão.

**Próximo ponto determinante recomendado:** dos pontos ainda não
conectados (seção 2), restam painel, exportação e estatísticas/gráficos.
Exportação (`monitora_registros_validados_exportar()`, a função que decide
se o produto é de fato escrito) é o próximo candidato natural — já
consome o cálculo de `bloqueante` enriquecido nesta etapa, mas ainda não
propaga a severidade do contrato único para sua própria mensagem de
bloqueio/log; o painel continua tendo maior blast radius por interação
humana direta e deve vir depois.

## 9. Incremento 03.5O-C executado (2026-07-04)

Executado o próximo incremento recomendado na seção 8, na sua forma mais
conservadora ainda: só propagação de metadado já calculado, sem novo
`warning()` e sem alterar nenhuma condição de bloqueio.

**Ponto auditado:** `monitora_registros_validados_exportar()` — a função
que de fato decide se `registros_validados.csv` é escrito, bloqueado ou
abortado. Ela já chama
`monitora_registros_corrig_gravar_auditoria_contrato_xlsform21()` (linha
~29511) tanto no modo `somente_auditar_contrato_corrig` (contexto
`pre_materializacao_registros_corrig`) quanto no modo de exportação real
(contexto `assert_final_export_registros_validados`), herdando desde
03.5N-C (seção 8) o enriquecimento de severidade do contrato único e o
`warning()` complementar já emitidos ali. Confirmado, porém, que a
contagem `n_bloqueios_alta_severidade_contrato_unico` calculada nessa
chamada nunca chegava ao **próprio resumo da exportação**
(`auditoria_registros_validados_resumo.csv`/`caminhos$resumo_out`, gravado
mais adiante na mesma função) — quem só olha esse relatório específico não
via a correlação com severidade do contrato único.

**Status antes:** `resumo` (a tabela gravada em
`auditoria_registros_validados_resumo.csv` e em
`caminhos$resumo_log`) tinha `n_bloqueios` (soma dos 6 critérios próprios
do XLSForm21) mas nenhuma coluna correlacionando esses bloqueios com
severidade do contrato único.

**Status depois:**
- Novo bloco logo após a chamada a
  `monitora_registros_corrig_gravar_auditoria_contrato_xlsform21()` extrai
  `auditoria_contrato_corrig$resumo$n_bloqueios_alta_severidade_contrato_unico`
  (dado já calculado em 03.5N-C; nenhum cálculo novo) com `NA_integer_`
  como fallback seguro (auditoria ausente, não é lista, `resumo` nulo ou
  coluna inexistente).
- `resumo` ganha a coluna
  `n_bloqueios_alta_severidade_contrato_unico_registros_corrig`, propagando
  esse valor. Esta é a única mudança: uma coluna a mais em um CSV de
  auditoria (`auditoria_registros_validados_resumo.csv`), nunca em
  `registros_validados.csv` (`out`/`cols`/`caminho_saida`/`header_saida`
  não foram tocados).
- `n_bloq` (a variável que efetivamente decide bloqueio/abortagem de
  `registros_validados.csv`, calculada separadamente a partir de
  `vazias_obrig`/`problemas_fmt`/`problemas_dom`/`problemas_cond`/
  `problemas_card`/`problemas_chave`/`problemas_sanitizacao_*`) **não foi
  alterada nem lida pelo novo bloco** — zero participação na decisão real.
  Nenhum `warning()` novo foi adicionado (o já existente, de 03.5N-C, já
  cobre a visibilidade no momento em que a auditoria do contrato de
  `registros_corrig` roda).

**Fontes paralelas:** nenhuma removida (propagação aditiva de metadado já
calculado; XLSForm21 embutido continua sendo a única fonte da decisão de
bloqueio de `registros_validados.csv`).

**Testes executados** (proporcionais, sem dados reais nem pipeline
pesado):
1. `Rscript -e 'parse("monitora_campsav_alvo_global_v2.6.0.R")'` sobre o
   script completo — sintaxe OK.
2. Teste sintético isolando a lógica de extração do novo bloco (sem
   depender do contrato embutido real nem de `registros_corrig`): 4
   cenários — (a) `auditoria_contrato_corrig` `NULL` (função ausente ou
   flag geral desligada) → `NA` seguro; (b) flag da 03.5N-C desligada
   (resumo tem a coluna, valor 0, comportamento real herdado) → propaga
   `0L`; (c) flag ligada com bloqueios de alta severidade → propaga o
   valor exato (`3L` no teste); (d) `resumo` sem a coluna (compatibilidade
   com versão anterior do helper) → `NA` seguro, sem erro. Todos os 4
   cenários confirmados.
3. `git diff --stat` sobre o script: 22 inserções, 0 remoções — mudança
   puramente aditiva, nenhuma linha de código pré-existente alterada.
4. Grep estático: nenhuma referência nova a
   `registros_importados.csv`/`registros_importados_bruto.csv` no diff;
   consumidores existentes de `auditoria_registros_validados_resumo.csv`
   (linhas ~30394-30412, no relatório consolidado) leem `n_bloqueios` por
   nome de coluna (`"n_bloqueios" %in% names(rr)`), não por posição —
   tolerantes à coluna nova, confirmando ausência de quebra.

**Riscos remanescentes:**
- Mesmo risco estrutural das seções 6/7/8: sem teste dinâmico end-to-end
  contra o contrato embutido real e `registros_corrig` real (mitigado por
  reaproveitar exatamente o dado já testado em 03.5N-C, sem novo cálculo).
- A decisão de bloqueio/abortagem de `registros_validados.csv` continua
  100% sob os 6 critérios próprios do XLSForm21 — o contrato único
  continua sendo só anotação também neste ponto de exportação.
- Mudança de saída limitada a uma coluna a mais em um CSV de auditoria;
  só é não-`NA` quando a flag opt-in `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS`
  já existente está ligada — comportamento pretendido, não regressão.

**Próximo ponto determinante recomendado:** dos pontos ainda não
conectados (seção 2), restam painel e estatísticas/gráficos. Painel tem
maior blast radius por interação humana direta (edição ao vivo de
`registros_corrig` no Shiny); estatísticas/gráficos (consumo somente
leitura de produtos já materializados) é o próximo candidato de menor
blast radius e deve ser avaliado antes do painel.

## 10. Incremento 03.5P-C executado (2026-07-04)

Executado o próximo incremento recomendado na seção 9, na forma mais
conservadora do padrão já usado nas seções 6-9: só declaração/log,
zero cálculo novo, zero coluna nova em produto central.

**Ponto auditado:** construção das tabelas estatísticas/gráficos, a
partir de `### Construção das tabelas estatísticas` (materialização de
`registros_corrig_stat`, consumida por todas as funções `monitora_plot_*`
e `monitora_stat_*` mais abaixo no script). Confirmado, por leitura
completa da seção e do comentário normativo já existente logo acima dela
("O ponto de integração fica depois da padronização/deduplicação e antes
da criação de `registros_corrig_stat`. Assim, qualquer correção validada
afeta as estatísticas, gráficos e arquivos finais da mesma execução."),
que:
- `registros_corrig_stat` é sempre derivado de `registros_corrig` **em
  memória** (nunca por releitura de CSV) — estatísticas/gráficos não
  dependem de `registros_validados.csv` existir, ter sido gerado ou
  aprovado nesta execução;
- `monitora_registros_validados_exportar()` já é chamada antes deste
  ponto (linha ~39573), mas seu retorno não é capturado nesse call site
  específico — quando a exportação é bloqueada pela regra própria
  XLSForm21 (schema embutido, seção 8), a execução **continua** e
  constrói estatísticas normalmente, sem qualquer declaração de que o
  produto contratual `registros_validados.csv` não foi de fato gerado
  nesta rodada;
- a única interrupção antes das estatísticas é por
  `MONITORA_REGISTROS_CORRIG_PENDENCIAS_IMPEDITIVAS` (pendências de
  `registros_corrig`, não relacionadas ao schema XLSForm21 de
  `registros_validados.csv`);
- não havia hard-code nem fonte paralela concorrente disputando a
  materialização de `registros_corrig_stat` — um único caminho de
  construção; o risco identificado é de **ausência de declaração**, não
  de ambiguidade de fonte.

**Status antes:** nenhuma declaração, em log ou metadado, de que
estatísticas/gráficos partem de `registros_corrig_stat`/`registros_corrig`
e não de `registros_validados.csv`, nem de se `registros_validados.csv`
foi de fato gerado nesta execução, nem de quantas colunas de
`registros_corrig` (a fonte real das estatísticas) carregam severidade
alta bloqueante do contrato único.

**Status depois:**
- Novo helper `monitora_estatisticas_declarar_status_fonte()`, chamado
  uma única vez logo antes de `### Construção das tabelas estatísticas`
  (dentro de `try(..., silent = TRUE)`, sem propagar erro). Opt-in pela
  nova flag `MONITORA_DECLARAR_STATUS_FONTE_ESTATISTICAS` (padrão `"S"`,
  diferente das flags anteriores que defaultam `"N"` — decisão
  deliberada: esta função não grava nenhum arquivo novo, não lê dado
  sensível, e o custo de log é desprezível mesmo com o flag ligado por
  padrão; o consumo do contrato único dentro dela continua condicionado
  à flag própria já existente e já `"N"` por padrão,
  `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS`).
- Declara em um único evento de log (`declaracao_fonte_estatisticas`):
  (a) que a fonte é sempre `registros_corrig_stat`/`registros_corrig`,
  nunca `registros_validados.csv`; (b) se `registros_validados.csv` foi
  solicitado nesta execução e, se sim, se foi de fato gerado ou ficou
  bloqueado/abortado; (c) reaproveitando o helper já testado em 03.5N-C
  (`monitora_registros_corrig_severidade_contrato_unico()`, mesma flag
  opt-in própria), quantas colunas de `registros_corrig` têm severidade
  alta bloqueante do contrato único — nenhum fato novo, nenhum cálculo
  duplicado.
- Severidade do evento: `"AVISO"` somente quando `registros_validados.csv`
  foi solicitado e não foi gerado, ou quando há colunas de severidade
  alta bloqueante do contrato único; `"INFO"` nos demais casos (inclusive
  quando `registros_validados.csv` simplesmente não foi solicitado nesta
  execução — situação normal, não deve gerar aviso).
- **Nenhuma coluna nova em `registros_corrig_stat`, nenhum arquivo novo em
  `output/`, nenhuma mudança em qualquer função `monitora_plot_*`/
  `monitora_stat_*`, nenhuma participação em decisão de bloqueio.** Só
  log de execução.

**Fontes paralelas:** nenhuma removida (não havia; a mudança é
puramente de visibilidade sobre uma fonte já única).

**Testes executados** (proporcionais, sem dados reais nem pipeline
pesado, script descartável em `/tmp/teste_035p_c/teste.R`):
1. `Rscript -e 'parse("monitora_campsav_alvo_global_v2.6.0.R")'` sobre o
   script completo — sintaxe OK.
2. Teste sintético isolando uma cópia fiel da função inserida, com stubs
   de `monitora_log_registrar_evento` e de
   `monitora_registros_corrig_severidade_contrato_unico()`: 5 cenários —
   (a) flag geral desligada → `NULL`, nenhum evento de log, custo zero;
   (b) `registros_validados.csv` não solicitado → evento `INFO`,
   `n_colunas_alta_severidade` = `NA` (contrato único não avaliado); (c)
   solicitado mas não gerado (bloqueado) → evento `AVISO`, detalhe cita
   explicitamente que não foi gerado; (d) gerado, com 1 coluna de
   severidade alta bloqueante do contrato único → evento `AVISO`,
   contagem exata (`1L`); (e) helper do contrato único removido do
   ambiente (simula ausência/erro) → degrada para `NA` sem erro. Em
   todos os 5 cenários, confirmado que o `data.table` de entrada
   permanece com as mesmas linhas/colunas antes e depois da chamada —
   critério de aceite de zero mudança de schema/cardinalidade cumprido
   (bug de severidade encontrado e corrigido durante o próprio teste:
   primeira versão emitia `AVISO` sempre que `registros_validados.csv`
   não fosse gerado, mesmo quando simplesmente não solicitado — corrigido
   antes do commit para só escalar quando *solicitado e não gerado*).
3. `git diff --stat`: 76 inserções, 0 remoções — mudança puramente
   aditiva.
4. Grep estático: dentro do bloco novo, nenhuma leitura/escrita de
   `registros_importados*.csv`/`registros_corrig.csv`/
   `registros_validados.csv` (só menção em comentário/string de log);
   nenhuma atribuição a `registros_corrig_stat`/`registros_validados`
   dentro do bloco novo.

**Riscos remanescentes:**
- O "motivo exato" do bloqueio de `registros_validados.csv` não é
  capturado neste ponto porque o call site de
  `monitora_registros_validados_exportar()` na linha ~39573 não guarda o
  valor de retorno (diferente do call site da linha ~30324, que usa
  `val <- ...`); a declaração informa que houve bloqueio/abortagem, mas
  remete o motivo detalhado ao relatório já existente
  (`auditoria_registros_validados_resumo.csv`). Capturar o motivo exato
  exigiria alterar o call site de exportação (fora do escopo deste
  incremento isolado, que não deve tocar a função de exportação já
  auditada em 03.5N-C/03.5O-C).
- Mesmo risco estrutural das seções 6-9: sem teste dinâmico end-to-end
  contra o contrato embutido real e `registros_corrig` real (mitigado
  por reaproveitar exatamente o helper já testado em 03.5N-C, sem novo
  cálculo).
- Mudança de saída limitada a um evento de log por execução; passa a
  aparecer por padrão (flag própria `"S"`), diferente do padrão `"N"`
  das seções anteriores — decisão deliberada e justificada acima, não
  regressão (nenhum arquivo/coluna de produto é afetado).

**Próximo ponto determinante recomendado:** dos pontos ainda não
conectados (seção 2), resta apenas o painel — maior blast radius por
interação humana direta (edição ao vivo de `registros_corrig` no Shiny).
Recomenda-se, antes de qualquer alteração de código no painel, uma
auditoria documental dedicada (sem incremento de código na mesma etapa),
seguindo a mesma decisão da seção 3 deste plano.

## 11. Incremento 03.5Q-C executado (2026-07-04, Codex temporário)

Executada a auditoria documental dedicada recomendada na seção 10, sem
alteração de código. Esta etapa usou Codex explicitamente e
temporariamente porque a sessão do Claude atingiu limite e só retornaria
às 20h America/Sao_Paulo; para as próximas tarefas após o reset, o motor
recomendado volta a ser Claude, conforme política operacional carregada.

**Ponto auditado:** painel de correções assistidas e aplicação das
correções no fluxo principal, especialmente:
- decisão de abertura por `MONITORA_ABRIR_PAINEL_CORRECOES` e modos
  orientados a painel;
- materialização prévia de
  `registros_importados_operacional_pre_painel.csv`, sem sobrescrever
  `registros_importados.csv`;
- preparação de dicionários e metadados XLSForm do painel
  (`MONITORA_META_XLSFORMS_CORRECOES`,
  `MONITORA_DICIONARIO_ATRIBUTOS_CORRECOES`);
- geração de relatórios de apoio pré/pós-painel;
- chamada interativa `monitora_correcao_painel(registros_corrig, ...)`;
- aplicação posterior de `correcoes_campos` sobre `registros_corrig`;
- checkpoint parcial/final de `registros_corrig.csv` por
  `monitora_execucao_gravar_checkpoint_parcial()` e
  `monitora_publicacao_aa_exportar_registros_corrig_aprovado()`.

**Status antes:** o painel era o único ponto restante sem auditoria
incremental recente no plano. O contrato integral exige que o painel
exiba regra, severidade, origem e bloqueio, sem ocultar pendências
impeditivas nem converter diagnóstico em validação. O script já preserva
a separação de produtos H2R-C antes do painel e mantém a validação de
`registros_corrig.csv` depois do painel, mas a interface em si ainda é
orientada por metadados XLSForm/dicionários próprios e relatórios de
apoio, não por um `perfil_painel_edicao` derivado exclusivamente do
contrato único.

**Status depois:** nenhuma conexão funcional nova foi introduzida. A
conclusão auditável desta etapa é que **não é seguro alterar código do
painel no mesmo incremento**, porque o painel tem o maior blast radius do
motor único: interação humana direta, cache/reabertura incremental,
replay de correções, validação espacial opcional, relatórios pré/pós e
aplicação atômica posterior das operações. Um alerta opt-in ou mudança de
UI mal posicionada poderia alterar fluxo de curadoria ou interpretação de
pendências sem teste end-to-end do Shiny.

**Fontes paralelas identificadas no painel:** metadados XLSForm
embutidos, dicionário de atributos, dependências condicionais e
relatórios de apoio continuam sendo fontes operacionais próprias do
painel. Elas não devem ser removidas nem substituídas até que um
`perfil_painel_edicao` derivado de `monitora_contrato_unico_indices()`
demonstre equivalência de exibição, severidade, bloqueio e escopo de
edição.

**Critérios de aceite para uma futura conexão de código do painel:**
1. Flag opt-in default OFF para qualquer declaração de contrato único na
   interface.
2. Nenhuma alteração em schema, linhas, cardinalidade ou conteúdo de
   `registros_corrig` quando a flag estiver OFF.
3. Nenhuma releitura normativa de `output/` ou `log/`; o painel deve usar
   somente objetos em memória e perfis derivados do contrato embutido.
4. Nenhuma reconstrução de `registros_importados.csv` ou
   `registros_importados_bruto.csv` a partir de `registros_corrig.csv` ou
   `registros_validados.csv`.
5. Severidade, origem, status de bloqueio e tipo de correção exibidos
   como anotação de contexto, sem bloquear novas operações até haver teste
   end-to-end específico.
6. Teste sintético isolado para o helper de perfil do painel e teste
   Shiny mínimo/stub verificando que a abertura e o fechamento do painel
   não mudam `registros_corrig` sem correções.

**Testes/auditorias executados nesta etapa documental:**
1. `git status -sb`, branch, HEAD, upstream e ahead/behind — branch
   `dev-v2.6.2-h2r-cadeia-produtos`, HEAD `23cf163`, upstream alinhado,
   sem diff rastreado; apenas os dois não rastreados esperados.
2. `git log --oneline 75e298c..HEAD` e `git log --name-status
   75e298c..HEAD` — confirmados os incrementos recentes de
   pós-validação, exportação e estatísticas/gráficos.
3. Presença do contrato integral confirmada em
   `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`.
4. Grep/leitura estática do plano e do script nos pontos de painel,
   checkpoint parcial, exportação de `registros_corrig` e cadeia
   `registros_importados_bruto.csv -> registros_importados.csv ->
   registros_importados_operacional_pre_painel.csv -> registros_corrig.csv
   -> registros_validados.csv`.
5. Revisão de diff documental: nenhuma alteração no script e nenhuma
   alteração em produto central.

**Riscos remanescentes:**
- O painel ainda não exibe, como fonte determinante derivada do contrato
  único, regra/severidade/origem/bloqueio para cada ocorrência conforme o
  critério de aceitação da seção 28.
- As fontes operacionais do painel permanecem legítimas, mas paralelas ao
  contrato único; removê-las sem prova de equivalência quebraria a
  curadoria.
- A validação de comportamento do painel exige teste Shiny controlado;
  esta etapa evitou esse risco deliberadamente e não declara conclusão
  funcional do motor único para painel.

**Próximo ponto recomendado:** implementar, em tarefa separada e com teste
Shiny/stub dedicado, um helper somente leitura
`perfil_painel_edicao` derivado de `monitora_contrato_unico_indices()`,
default OFF, inicialmente apenas para produzir uma tabela de auditoria da
interface. Só depois avaliar exposição visual no painel.
