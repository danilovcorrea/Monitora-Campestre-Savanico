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

Os seguintes itens da seção 27 **não têm evidência de conclusão** nesta
auditoria — os pontos de importação, pré-validação, painel, pós-validação,
exportação e estatísticas/gráficos continuam operando por lógica própria
(regras locais/hard-codes históricos), com o contrato único atuando apenas
como camada de diagnóstico/comparação em paralelo, não como fonte
determinante do comportamento:

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
| Importação sem fontes paralelas | Não — importação ainda não é determinantemente conectada |
| Pré-validação com diagnósticos suficientes | Parcial — diagnóstico de pipes e comparação legado-vs-contrato já existem |
| Painel exibindo regras/severidade/origem/bloqueio | Não auditado nesta etapa |
| Pós-validação determinando aptidão de `registros_corrig.csv` | Não auditado nesta etapa |
| Exportação com 129 colunas quando apto | Não auditado nesta etapa |
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
