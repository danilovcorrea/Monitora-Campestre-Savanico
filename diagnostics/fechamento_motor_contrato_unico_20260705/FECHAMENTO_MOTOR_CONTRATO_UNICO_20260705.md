# Fechamento final integrado — motor único e contrato único

Data: 2026-07-05
Branch auditada: `dev-v2.6.2-h2r-cadeia-produtos`
HEAD no início desta etapa: `669d555a9dc88454fa5592cb4013c409643e4a82`
(upstream `origin/dev-v2.6.2-h2r-cadeia-produtos`, ahead/behind 0/0 confirmado)

Este documento é subordinado ao contrato canônico em
`diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`
(seções 27–28) e ao rastreamento operacional em
`diagnostics/plano_executivo_motor_unico_20260704/PLANO_EXECUTIVO_MOTOR_UNICO_20260704.md`
(seções 1–15). Em qualquer divergência, prevalece o contrato integral.

**Objetivo deste documento:** responder, sem fingir conclusão onde há
pendência real, o que está entregue integralmente, o que está entregue de
forma incremental/opt-in segura, o status de cada critério da seção 28, os
testes executados nesta rodada, os riscos remanescentes que impedem
declarar validação produtiva plena, e qual o próximo passo H2R-C seguro.

## 1. Branch/HEAD base e jornada de commits

Estado confirmado nesta etapa (Etapa 0):

- Branch: `dev-v2.6.2-h2r-cadeia-produtos`.
- HEAD: `669d555` — "docs: valida shiny controlado da aba opt-in do painel
  por contrato unico".
- Upstream: `origin/dev-v2.6.2-h2r-cadeia-produtos`, 0 ahead / 0 behind.
- `git diff`/`git diff --cached`: vazios (working tree limpo antes desta
  etapa).
- Os dois artefatos não rastreados esperados estão presentes e intactos:
  `diagnostics/backup_pre_commit_h2r_c_20260704_124238/` e
  `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`.
- Contrato integral presente em
  `diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`.

Jornada principal de commits desde a baseline v2.6.0/auditoria de linhagem
(`cc5378e` em diante, ordem cronológica, do mais antigo ao HEAD):

| Commit | Resumo |
|---|---|
| `cc5378e` | audita linhagem da cadeia de produtos |
| `496440f` | consolida linhagem H2R-C de registros importados |
| `3d85944` | consolida governança 035N rumo ao motor único |
| `5780af2`/`e61d47b` | autonomia operacional do agente (commit local) |
| `5b38224` | consolida contrato único como fonte canônica |
| `9405547` | materializa arquitetura real do projeto |
| `04e9eff` | estrutura plano executivo do motor único |
| `4844c86`/`7a71c73` | 03.5L-C — diagnóstico de importação eleva a alerta de severidade |
| `75ebe80`/`75e298c` | pré-validação por contrato único eleva a alerta |
| `ddcd04c`/`bdc4d6d` | pós-validação por contrato único eleva a alerta |
| `c91f037`/`a483a98` | exportação propaga severidade do contrato único |
| `8417d9e`/`23cf163` | estatísticas/gráficos declaram status da fonte |
| `c5d74e4`/`a2d4143` | perfil de painel derivado do contrato único (helper) |
| `cf720a6` | teste do helper de painel contra contrato real |
| `3f928b8`/`2e40ab2` | exposição visual opt-in do painel (aba "Auditoria contrato único") |
| `669d555` | validação Shiny controlada (`testServer`) da aba opt-in do painel |
| `c381a49` | `shiny::runApp()` real (`httpuv`/loopback) da aba opt-in isolada |
| *(esta rodada)* | 03.5V-C — dimensiona fecho de dependências do `runApp()` do painel completo; bloqueio documentado, execução não realizada |

## 2. Entregas do contrato único

- Fonte única embutida no script (`monitora_contrato_unico_embutido()`,
  ~linha 32120), sem dependência de XLSX/CSV/ZIP externo em runtime —
  cumpre seção 4 do contrato.
- Índices/perfis derivados (`monitora_contrato_unico_indices()`, ~linha
  32496), com 16 índices e 6 perfis (`perfil_importacao`,
  `perfil_pre_painel`, `perfil_painel_edicao`, `perfil_pos_painel_corrig`,
  `perfil_export_registros_validados`, `perfil_estatisticas_graficos`),
  sem introduzir fatos novos sobre o atributo (garantia documentada no
  próprio código).
- Diagnóstico opt-in conectado em 6 pontos vivos do script (todos
  diagnósticos/comparativos, nenhum substituindo o fluxo primário — ver
  seção 1 do plano executivo).

## 3. Entregas da arquitetura real

Documentadas em `diagnostics/arquitetura_real_materializada_20260704/`:
identificação do script oficial, distinção entre script vigente e aliases
de release desatualizados, classificação do arquivo não rastreado
`..._03.5L-C_PNB_FLAG_OFF.R` como backup obsoleto pré-H2R-C (preservado,
não usado), e mapa de fontes normativas vs. derivadas vs. diagnóstico.
Nenhuma mudança de comportamento; documento puramente descritivo.

## 4. Entregas do motor único por ponto runtime

| Ponto runtime | Incremento | Natureza | Flag (default) |
|---|---|---|---|
| Importação (observado→canônico) | 03.5L-C | alerta opt-in (INFO→AVISO) | `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS` (`N`) |
| Pré-validação (pipes) | 03.5M-C3 | alerta opt-in (INFO→AVISO) | `MONITORA_DIAGNOSTICO_PIPES_CONTRATO` (`N`) |
| Pós-validação (`registros_corrig`) | 03.5N-C | enriquecimento de auditoria + alerta complementar | `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS` (`N`) |
| Exportação (`registros_validados.csv`) | 03.5O-C | propagação de metadado já calculado ao resumo | mesma flag acima |
| Estatísticas/gráficos | 03.5P-C | declaração de status da fonte (log) | `MONITORA_DECLARAR_STATUS_FONTE_ESTATISTICAS` (`S` — decisão deliberada, custo desprezível) |
| Painel (auditoria) | 03.5R-C/R-C2 | helper `monitora_perfil_painel_edicao_contrato_unico()`, testado com contrato real | `MONITORA_AUDITORIA_PERFIL_PAINEL_CONTRATO_UNICO` (`FALSE`) |
| Painel (exposição visual) | 03.5S-C | aba opt-in "Auditoria contrato único (opt-in)" | mesma flag acima |
| Painel (validação dinâmica) | 03.5T-C | `shiny::testServer()` real sobre a aba, sem `runApp()` | — (só verificação) |
| Painel (`runApp()` real, isolado) | 03.5U-C | `shiny::runApp()` real (`httpuv`/loopback) sobre a aba isolada, HTTP 200, HTML inicial correto nos dois estados da flag | — (só verificação) |
| Painel (dimensionamento `runApp()` completo) | 03.5V-C | fecho de dependências medido (123+ definições, 68% do script) e classificado como não seguro de extrair/executar nesta rodada; nenhuma execução realizada | — (só auditoria, ver seção 17 do plano executivo) |

Em todos os pontos: **nenhuma fonte paralela foi removida** (a remoção só
é autorizada após equivalência comprovada, seção 27 do contrato — ainda
não demonstrada); a decisão real de bloqueio de `registros_validados.csv`
continua 100% sob os 6 critérios próprios do XLSForm21 embutido — o
contrato único participa como anotação/alerta, não como decisor, em todos
os pontos pós-importação.

## 5. Matriz de critérios da seção 28

| Critério (seção 28) | Status | Evidência objetiva |
|---|---|---|
| Todos os perfis derivados da mesma fonte | **PARCIAL/OPT-IN** | Índices/perfis existem e são usados por 6+ pontos diagnósticos; fluxo primário de decisão ainda não depende deles em todos os pontos |
| Importação reconhece observado→canônico sem fontes paralelas | **PARCIAL/OPT-IN** | 03.5L-C eleva divergências de alta severidade a alerta opt-in; fonte paralela de importação continua sendo o caminho determinante |
| Pré-validação produz diagnósticos suficientes | **PARCIAL/OPT-IN** | 03.5M-C3 conecta diagnóstico/alerta opt-in de pipes; sem bloqueio primário novo |
| Painel exibe regras/severidade/origem/bloqueio | **PARCIAL/OPT-IN** | Helper + aba opt-in implementados e validados estaticamente (03.5R-C2), dinamicamente via `testServer` (03.5T-C) e agora via `shiny::runApp()` real em `httpuv`/loopback (03.5U-C, 2026-07-05); **sem sessão reativa real (WebSocket) nem validação do painel completo com navegador** |
| Pós-validação determina aptidão de `registros_corrig.csv` | **PARCIAL/OPT-IN** | 03.5N-C anota severidade contratual; decisão de aptidão continua pelos 6 critérios próprios do XLSForm21 |
| Exportação gera `registros_validados.csv` com 129 colunas quando apto | **PARCIAL/OPT-IN** | 03.5O-C propaga severidade ao resumo; schema/decisão de exportação seguem pelo fluxo existente, não alterados |
| Índices/caches deriváveis | **CUMPRIDO** | `monitora_contrato_unico_indices()` recalculável a partir do contrato embutido, sem I/O de disco (confirmado por fecho `codetools::findGlobals`, seções 13/15 do plano) |
| Produtos com identidade/linhagem próprias | **CUMPRIDO** | 5 camadas nomeadas e documentadas (contrato seção 31, README, arquitetura real seção 3); nenhum arquivo com duas semânticas |
| Testes demonstram equivalência ou mudança aprovada | **PARCIAL** | Testes sintéticos/isolados por incremento (stubs + contrato real) cobrem zero-regressão local; **sem suíte automatizada dedicada nem teste de equivalência ponta a ponta do pipeline completo** |
| Performance aceitável | **NÃO MEDIDO** (fora do escopo autorizado desta rodada e das anteriores — decisão deliberada de economia, sem pipeline pesado/dados reais) |
| Documentação/README/manual/índice/auditoria final refletem a cadeia | **CUMPRIDO** | README (produtos principais + governança), índice de governança, plano executivo, arquitetura real e este fechamento |

**Conclusão da matriz:** o motor único está em **progresso incremental
real e verificável**, não apenas planejado, mas **não atende
integralmente** à seção 28. Nenhum item foi declarado "cumprido" sem
evidência de código/teste correspondente nesta auditoria.

## 6. Cadeia de produtos e salvaguardas

Linhagem vigente (contrato seção 31, reafirmada em README e arquitetura
real):

```
registros_importados_bruto.csv -> registros_importados.csv ->
registros_importados_operacional_pre_painel.csv -> registros_corrig.csv ->
registros_validados.csv
```

Salvaguardas verificadas nesta etapa (Etapa 2, greps sobre o script
completo de 48235 linhas):

- Nenhuma ocorrência de atribuição ou `fwrite` que reconstrua
  `registros_importados.csv`/`registros_importados_bruto.csv` a partir de
  `registros_corrig`/`registros_validados` (grep vazio, conforme esperado).
- `git ls-files | grep registros_importados` só retorna documentos `.md`
  de auditoria — nenhum CSV de linhagem versionado (confirmado
  previamente na arquitetura real, seção 3; não há motivo para divergir
  nesta etapa, pois não houve alteração de `.gitignore` nem de política de
  versionamento).

## 7. Testes/auditorias executados nesta rodada (Etapa 2)

1. `git status -sb`, `git log -1 --stat`, `git diff --stat`,
   `git diff --cached --stat`, verificação de upstream e ahead/behind —
   estado confirmado idêntico ao esperado, working tree limpo.
2. `Rscript -e 'invisible(parse("monitora_campsav_alvo_global_v2.6.0.R", keep.source = FALSE))'`
   sobre o script completo (48235 linhas) — sintaxe OK.
3. Grep contra reconstrução tardia de `registros_importados*.csv` a partir
   de `registros_corrig`/`registros_validados` — nenhuma ocorrência.
4. Grep das flags opt-in do motor único (`MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS`,
   `MONITORA_DIAGNOSTICO_PIPES_CONTRATO`,
   `MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS`,
   `MONITORA_AUDITORIA_PERFIL_PAINEL_CONTRATO_UNICO`,
   `MONITORA_DECLARAR_STATUS_FONTE_ESTATISTICAS`) — todas com default `"N"`/`FALSE`,
   exceto a declaração de status de estatísticas (`TRUE`, decisão
   deliberada e já documentada na seção 10 do plano executivo, sem custo
   de I/O).
5. Confirmação de presença dos documentos canônicos: contrato integral,
   plano executivo, arquitetura real, README raiz e índice de governança.
6. Confirmação de que os dois artefatos não rastreados esperados
   permanecem intocados (tamanho/listagem inalterados frente à auditoria
   anterior).
7. Não foi executado teste de `runApp()`/headless adicional nesta rodada:
   a validação Shiny dinâmica mais recente (`shiny::testServer()`) já foi
   executada e documentada na etapa anterior (03.5T-C, commit `669d555`);
   repeti-la sem alteração de código não agregaria evidência nova, e a
   avaliação desta rodada é de que um teste `runApp()` real (navegador,
   sessão completa do painel com `dt` real, `drawCallback` JS) tem
   complexidade/risco que excede o escopo de fechamento documental —
   **registrado como pendência (seção 8), não executado**.

Nenhum dado real foi usado; nenhum pipeline pesado foi executado; nenhuma
linha do script foi alterada por esta rodada até este ponto.

## 8. Riscos remanescentes que impedem validação produtiva plena

1. **Sem `shiny::runApp()` real end-to-end do painel completo.** Em
   2026-07-05 (03.5U-C), foi executado `shiny::runApp()` real (servidor
   `httpuv` de fato, loopback, HTTP 200) sobre a aba isolada nos dois
   estados da flag — confirma que a UI/servidor real (não só mock) sobe
   sem erro e serve o HTML/output binding correto. Isso **não cobre**:
   sessão reativa real via WebSocket (o `curl` usado só busca o HTML
   estático inicial, sem exercitar o `renderDT` em si — essa parte segue
   coberta apenas por `testServer`, seção 15 do plano executivo),
   navegador, `drawCallback` JS, paginação AJAX real do `DT`, nem
   interação com as demais abas/filtros/`dt` real de `registros_corrig`
   (fora do escopo desta rodada — fecho de dependências do painel
   completo é ordens de grandeza maior que o da aba isolada).
   **Atualização de 2026-07-05 (03.5V-C):** esse "ordens de grandeza
   maior" foi medido, não só estimado: 123+ definições top-level
   necessárias (vs. 23 para a aba isolada), espalhadas por 68% da
   extensão do script de 48235 linhas, num script que é pipeline linear
   (não biblioteca) e cujo `source()` completo dispararia o bloco de
   pipeline pesado real protegido por
   `MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE` (linhas 39602–48233).
   Pacotes (`shiny`/`DT`/`data.table`/`leaflet`/`httpuv`) já estão todos
   instalados — não é isso que bloqueia. Conclusão desta rodada: não
   seguro extrair/executar sem uma rodada dedicada com orçamento e método
   próprios (ver seção 17 do plano executivo). Não executado.
2. **Contrato único ainda não decide bloqueio em nenhum ponto
   pós-importação.** Em pós-validação e exportação, ele é anotação/alerta;
   a decisão real permanece 100% sob os 6 critérios próprios do XLSForm21
   embutido. Convergir isso é mudança de comportamento do fluxo primário,
   fora do escopo de qualquer incremento já executado.
3. **`ocorrencias_painel` (parâmetro do helper de painel) segue sem
   conexão a contagens reais de uso** — capacidade preparada, não ativa.
4. **Sem suíte de testes automatizada dedicada ao motor único** — os
   testes existentes são scripts descartáveis por incremento (não
   versionados), com stubs e, em alguns casos, contrato real; não há
   execução única e repetível de regressão para todos os pontos
   simultaneamente.
5. **Performance não medida** em nenhuma etapa do motor único até aqui
   (decisão deliberada de economia, sem pipeline pesado/dados reais);
   permanece item da seção 28 sem evidência.
6. **Dois artefatos não rastreados seguem como pendência organizacional
   deliberada** (não funcional): `diagnostics/backup_pre_commit_h2r_c_20260704_124238/`
   e `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`. Ambos
   classificados e preservados intencionalmente (arquitetura real, seções
   2.1 e 5.1); esta rodada não os moveu, versionou nem apagou.

## 9. Limites explícitos desta rodada

- Nenhum dado real foi usado.
- Nenhum pipeline pesado foi executado.
- **Atualização de 2026-07-05 (03.5U-C):** foi executado `shiny::runApp()`
  real (não mock) sobre a aba isolada, em loopback, com timeout e sem
  processo órfão — ver seção 16 do plano executivo. Continua **não**
  executado: `runApp()` do painel completo (`monitora_correcao_painel()`
  com `dt` real/sintético e demais abas) e qualquer sessão com navegador
  real/`shinytest2`/`chromote` (não instalados neste ambiente).
- Nenhuma refatoração ampla ou em lote foi feita.
- Nenhuma reconstrução de `registros_importados.csv`/`registros_importados_bruto.csv`
  a partir de camadas posteriores foi feita ou proposta.
- **Atualização de 2026-07-05 (03.5V-C):** dimensionado (não executado) o
  `runApp()` do painel completo — fecho de dependências medido em 123+
  definições/68% do script; classificado como não seguro de extrair ou de
  fazer `source()` completo nesta rodada (ver seção 17 do plano
  executivo). Nenhuma linha de código alterada.

## 10. Decisão sobre os dois artefatos não rastreados

Preservados como pendência organizacional deliberada, sem apagar,
versionar ou adicionar a `.gitignore` nesta etapa. Classificação já
registrada em `diagnostics/arquitetura_real_materializada_20260704/ARQUITETURA_REAL_MATERIALIZADA_20260704.md`
(seções 2.1 e 5.1): backup manual de segurança pré-commit H2R-C e backup
obsoleto pré-H2R-C do script, respectivamente. Nenhum dos dois é
consumido por qualquer fluxo de execução.

## 11. Próximo passo H2R-C seguro

**Atualização de 2026-07-05 (03.5U-C):** o item 1 abaixo foi parcialmente
executado — `shiny::runApp()` real da aba isolada, sem navegador, sem
sessão WebSocket (ver seção 16 do plano executivo). O que falta para
fechamento integral do critério "painel" está detalhado abaixo.

**Atualização de 2026-07-05 (03.5V-C):** o item 2 abaixo foi dimensionado
(não executado) — fecho de dependências medido em 123+ definições
top-level espalhadas por 68% do script (vs. 23 para a aba isolada), num
script que é pipeline linear cujo `source()` completo dispararia o
pipeline pesado real. Classificado como não seguro para esta rodada;
requer método programático de extração do fecho (`codetools::findGlobals`
recursivo) e isolamento dos globais com I/O real, ambos fora do escopo
autorizado aqui (ver seção 17 do plano executivo).

Na ordem de menor para maior blast radius, não executado nesta rodada:

1. **Sessão Shiny real via navegador/`shinytest2`/`chromote` (não
   instalados neste ambiente) sobre a aba isolada**, para exercitar
   WebSocket, `drawCallback` JS e paginação AJAX real do `DT` — requer
   decisão explícita de instalar pacotes novos (rede/tempo adicional),
   fora do escopo desta rodada.
2. **`shiny::runApp()` real do painel completo**
   (`monitora_correcao_painel()` com `dt` sintético mínimo e demais abas),
   em ambiente de teste dedicado — fecho de dependências medido em 123+
   definições/68% do script (03.5V-C), ordens de grandeza maior que o da
   aba isolada; único jeito de fechar integralmente o critério "painel" da
   seção 28 em conjunto com o item 1.
3. Avaliar se `ocorrencias_painel` deve ser alimentado com contagens reais
   de uso por atributo, e só depois disso decidir sobre qualquer
   participação do contrato único em decisões de bloqueio real
   (pós-validação/exportação) — mudança de comportamento do fluxo
   primário, exige incremento dedicado com auditoria de risco própria.
4. Nenhum dos três itens acima deve ser feito na mesma etapa que qualquer
   outra alteração funcional, conforme seção 2 do contrato (evitar
   mudanças simultâneas em importação/painel/validação/exportação).

## 12. Conclusão

A entrega autônoma segura desta rodada está **concluída**: auditoria
integrada da seção 28, testes econômicos executados, relatório final
versionado produzido, e nenhuma contradição documental de risco
encontrada que exigisse correção (Etapa 4 não teve achados). Persistem
pendências reais e conhecidas — validação `runApp()` real, participação
determinante do contrato único em bloqueio, suíte automatizada e medição
de performance — que **exigem dados sintéticos/reais e/ou uma rodada
dedicada** e não devem ser declaradas concluídas por este documento.

**Atualização de 2026-07-05 (03.5U-C):** em continuação autônoma desta
mesma rodada, foi executado `shiny::runApp()` real (servidor `httpuv` de
fato em loopback, sem mock) sobre a aba isolada "Auditoria contrato único
(opt-in)", nos dois estados da flag, sem dados reais, sem pipeline
pesado, sem processo órfão (ver seção 16 do plano executivo e seções 4,
8, 9 e 11 acima, já atualizadas). Isso reduz — sem eliminar — a pendência
"sem `runApp()` real"; permanece pendente a sessão reativa via WebSocket
(não exercitada por este teste, que só buscou o HTML estático inicial via
`curl`) e o `runApp()` do painel completo com as demais abas e `dt`
real/sintético, ambos fora do escopo de proporcionalidade desta rodada.
Nenhuma alteração de código foi feita nem necessária; nenhum bug foi
encontrado na aba opt-in durante este teste.

**Atualização de 2026-07-05 (03.5V-C):** em nova continuação autônoma,
avaliou-se se o `runApp()` do painel completo (pendência acima) podia ser
executado sem instalar pacotes. Pacotes não são o bloqueio (todos
presentes). O bloqueio é estrutural e foi medido: 123+ definições
top-level necessárias, espalhadas por 68% do script de 48235 linhas, num
script que é pipeline linear (não biblioteca de funções) cujo `source()`
completo executaria o bloco de pipeline pesado real protegido por
`MONITORA_EXECUCAO_ENCERRADA_CONTROLADAMENTE`. Extrair esse fecho à mão,
como feito para a aba isolada (23 funções), equivaleria a reconstruir mais
de dois terços do script — refatoração em lote disfarçada, com risco de
divergência silenciosa. Por isso, **não foi executado** nesta rodada;
documentado como pendência real (seção 17 do plano executivo), não fingida
como concluída. Nenhuma alteração de código foi feita.

## 13. Atualização Etapa 05 — consolidação H2R-C com evidência R local

Data/hora: 2026-07-05, execução por Codex em modo `commit_autorizado`.

Estado confirmado no início da Etapa 05:

- Branch: `dev-v2.6.2-h2r-cadeia-produtos`.
- HEAD inicial: `89920d6` — "docs: dimensiona runapp completo do painel
  com dt sintetico".
- Upstream: `origin/dev-v2.6.2-h2r-cadeia-produtos`, 0 ahead / 0 behind.
- `git diff --stat`: vazio antes da edição documental desta etapa.
- Artefatos não rastreados preservados e não versionados nesta etapa:
  `diagnostics/backup_pre_commit_h2r_c_20260704_124238/`,
  `diagnostics/validacao_r_local_h2r_c_20260705_003935/`,
  `diagnostics/validacao_r_local_h2r_c_20260705_004113/` e
  `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R`.

Evidência R local versionada nesta etapa:

- Diretório:
  `diagnostics/validacao_r_local_h2r_c_20260705_004208/`.
- Manifest:
  `diagnostics/validacao_r_local_h2r_c_20260705_004208/MANIFEST.json`.
- Resumo:
  `diagnostics/validacao_r_local_h2r_c_20260705_004208/VALIDACAO_R_LOCAL_H2R_C.md`.
- Resultado estruturado:
  `diagnostics/validacao_r_local_h2r_c_20260705_004208/validacao_r_local_h2r_c.json`.
- Resultado geral: `OK`; `r_returncode = 0`; `parse(file=script_path) OK`.

Esta etapa **não executou Rscript dentro do motor de IA**. A evidência foi
gerada localmente no Fedora/orquestrador em 2026-07-05 00:42:08 -0300
contra o script oficial rastreado
`monitora_campsav_alvo_global_v2.6.0.R`, com R 4.6.1. O consumo por Codex
ficou restrito a leitura/auditoria dos artefatos já gerados.

Checks H2R-C confirmados pela evidência `004208`:

- Script principal faz parse com sucesso.
- `registros_importados_operacional_pre_painel.csv` aparece como produto
  próprio e esperado.
- A função viva `monitora_registros_importados_saneado_exportar()` aceita
  `produto_nome`, preservando o default `registros_importados.csv` para
  chamadas existentes.
- A escrita pós-tokenização usa
  `produto_nome = "registros_importados_operacional_pre_painel.csv"`,
  sem sobrescrever `registros_importados.csv`.
- A ordem validada permanece:
  `registros_importados_bruto.csv` -> `registros_importados.csv` ->
  `registros_corrig` -> `registros_importados_operacional_pre_painel.csv`.
- Após `rm(registros)`, a verificação confirma que não há reexecução
  operacional de saneamento para reconstruir `registros_importados.csv` a
  partir de camada posterior.

Auditorias Codex adicionais, sem R e sem pipeline pesado:

- Busca exata no script oficial confirmou o parâmetro `produto_nome`, a
  chamada pós-tokenização com nome próprio, as listas internas de produtos
  centrais e a linha de auditoria final para
  `registros_importados_operacional_pre_painel.csv`.
- `git diff --no-index --stat` entre o backup
  `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` e o script
  oficial confirmou divergência ampla esperada (backup obsoleto pré-H2R-C),
  sem necessidade de versionar, apagar ou usar esse arquivo.
- Nenhum dado real, output pesado, backup ou diagnóstico superseded foi
  alterado.

Conclusão da Etapa 05: não houve necessidade de nova alteração funcional
no script principal. A intervenção suficiente e segura foi consolidar o
handoff documental e versionar a evidência R local OK mais recente,
mantendo fora do commit os diagnósticos `003935`/`004113` por estarem
superseded e os backups já classificados como artefatos organizacionais
preservados.
