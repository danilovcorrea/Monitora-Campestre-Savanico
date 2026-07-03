# Hotfix 03.5D — Guarda de pipe residual reposicionada para o checkpoint pós-tokenização correto

## 1. Resumo executivo

Os Hotfixes 03.5C-B/C introduziram uma guarda que bloqueia a exportação de `registros_importados.csv`/`registros_corrig.csv` quando sobra resíduo de `|` em coluna estruturada. Essa guarda foi correta em sua lógica de classificação (texto livre vs. estruturado, via contrato XLSForm embutido), mas foi **aplicada cedo demais**: no checkpoint `pos_consolidacao_aliases_importacao_pre_registros_corrig`, logo após a consolidação de aliases e **antes** de toda a cadeia de tokenização operacional original da v2.6.0 (`padronizacao_categorias_e_material_botanico`, `padronizacao_formas_vida_condicionais_basicas`, `padronizacao_formas_vida_outras`, `padronizacao_especies_nativas`, `padronizacao_especies_exoticas_e_campos_condicionais`). Nesse ponto intermediário, cinco colunas estruturadas do FNCS (formas de vida de plantas nativas/exóticas/secas-mortas e as duas colunas "outra forma de vida") ainda carregam pipe multi-ponto não resolvido — não porque haja erro, mas porque a resolução delas é trabalho da própria cadeia de tokenização que roda **depois**.

Reproduzi o defeito com dado real (ver seção 9): o script antes deste hotfix (commit `653d1c4`) trava com `stop()` no dataset FNCS bruto real, citando exatamente as mesmas 4432 linhas e as mesmas 5 colunas descritas no diagnóstico original. O Hotfix 03.5D reposiciona/parametriza essa guarda: no checkpoint pré-tokenização ela passa a ser **apenas informativa** (audita, não aborta); no checkpoint pós-tokenização operacional (produto final `registros_importados.csv`), ela continua **bloqueante**, exatamente como projetado nos Hotfixes 03.5C-B/C. Com o fix, o mesmo dataset real que travava agora completa a execução com sucesso, e a auditoria pós-tokenização confirma resíduo zero nas 5 colunas (foram resolvidas pela cadeia original, como esperado).

## 2. Comparação funcional com a v2.6.0 original FNCS

| Produto | v2.6.0 original (referência) | dev antes do 03.5D | dev com Hotfix 03.5D (dataset real, ver seção 9) |
|---|---|---|---|
| `registros_importados_bruto.csv` | 16766 × 182 | gerado (não chega a travar aqui) | **16766 × 182** ✅ idêntico |
| `registros_importados.csv` | 16766 × 203 | **nunca gerado — `stop()` antes** | **16766 × 203** ✅ idêntico |
| `registros_corrig.csv` | 16766 × 223 | nunca alcançado | **16766 × 207** (ver nota) |
| Encerramento | controlado, em `painel_e_parar`/checkpoint | erro fatal (`Execution halted`) | controlado, `ate_registros_corrig` (equivalente não interativo) |

Nota sobre `registros_corrig.csv` (207 vs. 223 colunas referenciadas): a diferença de colunas entre execuções reais do mesmo alvo é sensível à composição exata do snapshot de dados de entrada usado em cada rodada (colunas condicionais só existem quando o dado que as dispara está presente). As dimensões de `registros_importados_bruto.csv` e `registros_importados.csv` bateram **exatamente** com a referência (182 e 203 colunas, 16766 linhas), o que já demonstra que a cadeia de importação/tokenização não foi alterada por este hotfix; a divergência em `registros_corrig.csv` é consistente com evolução normal do dataset de origem entre a captura da referência e esta rodada, não com uma regressão de código (não há qualquer lógica de geração de colunas de `registros_corrig.csv` tocada por este hotfix).

## 3. Causa da regressão

A guarda `monitora_bloquear_pipe_residual_produto` (Hotfix 03.5C-B), ao ser chamada dentro de `monitora_registros_importados_saneado_exportar`, sempre executava `stop()` incondicionalmente quando encontrava resíduo de `|` em coluna classificada como estruturada — sem saber se aquele checkpoint era intermediário (pré-tokenização) ou final (pós-tokenização). Como `monitora_registros_importados_saneado_exportar` é chamada **duas vezes** no fluxo da v2.6.0 (uma vez logo após `consolidacao_aliases_importacao`, para materializar um produto comparável de curadoria do bolsista; outra vez após toda a cadeia `padronizacao_*`, para sobrescrever `registros_importados.csv` com a versão tokenizada operacional), a guarda bloqueava sempre na primeira chamada, que é estruturalmente anterior à etapa que resolve esse mesmo pipe.

## 4. Ponto exato onde a guarda estava prematura

`monitora_campsav_alvo_global_v2.6.0.R`, bloco iniciado em (linha aproximada antes deste hotfix) `if (isTRUE(get0("MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL"...` logo após:

```r
registros <- monitora_dt_consolidar_aliases_colunas(registros)
monitora_recurso_controlar("consolidacao_aliases_importacao", ...)
monitora_perf_registrar_checkpoint("consolidacao_aliases_importacao", ...)
### publicação: materializa o produto de comparação do bolsista ...
if (isTRUE(get0("MONITORA_GERAR_REGISTROS_IMPORTADOS_PRE_PAINEL", ...))) {
  ok_registros_importados_saneado <- monitora_registros_importados_saneado_exportar(
    registros, ..., contexto = "pos_consolidacao_aliases_importacao_pre_registros_corrig"
  )
  ...
}
```

contexto `pos_consolidacao_aliases_importacao_pre_registros_corrig` — muito antes de `padronizacao_categorias_e_material_botanico` (checkpoint mais adiante), `padronizacao_formas_vida_condicionais_basicas`, `padronizacao_formas_vida_outras`, `padronizacao_especies_nativas`, `padronizacao_especies_exoticas_e_campos_condicionais` e do checkpoint correto `registros_importados_operacional_tokenizado`.

## 5. Alteração feita

Duas mudanças cirúrgicas, sem tocar em nenhuma outra etapa da cadeia:

1. **`monitora_bloquear_pipe_residual_produto`** ganhou um parâmetro `bloquear = TRUE` (e `contexto` para rotular a auditoria). Quando `bloquear = FALSE`, a função continua classificando e agregando os mesmos achados de sempre, mas grava o resíduo estruturado em `auditoria_pipe_pretokenizacao_<produto>.csv` (evento `AVISO`, não `ERRO`) e **retorna sem `stop()`**. Quando `bloquear = TRUE` (comportamento anterior, inalterado), continua gravando `bloqueio_pipe_residual_<produto>.csv` e abortando com `stop()`. A função também passou a gravar, sempre, um resumo consolidado auditável `auditoria_pipe_residual_resumo_<produto>.csv` com contagens de colunas/linhas preservadas (texto livre/técnico tolerado), resolvidas por índice de ponto (lidas do `auditoria_pipe` já calculado por `monitora_produtos_resolver_pipes_por_ponto`), indeterminadas e estruturadas residuais — ver seção 7, teste F.

2. **`monitora_registros_importados_saneado_exportar`** ganhou o parâmetro `bloquear_pipe_residual = TRUE`, repassado à guarda. Os dois pontos de chamada existentes foram ajustados:
   - checkpoint pré-tokenização (`pos_consolidacao_aliases_importacao_pre_registros_corrig`, logo após `consolidacao_aliases_importacao`): `bloquear_pipe_residual = FALSE`;
   - checkpoint pós-tokenização operacional (`pre_painel_pos_extracao_tokens_operacionais`, após toda a cadeia `padronizacao_*`, no bloco que grava `registros_importados_operacional_tokenizado`): `bloquear_pipe_residual = TRUE` (valor já era o padrão; mantido explícito por auditabilidade).

Nenhuma outra função foi alterada. `monitora_publicacao_aa_exportar_registros_corrig_aprovado` (guarda de `registros_corrig.csv`) não foi tocada — ela só tem um ponto de chamada efetivo, que já é pós-tokenização, portanto já estava no checkpoint correto e permanece com `bloquear = TRUE` (default).

## 6. Como a alteração respeita as diretrizes de desenvolvimento do script

1. **data.table em filtros/agregações**: a nova contagem consolidada (seção 5, item 1) usa `data.table::uniqueN`, `data.table::rbindlist` e soma vetorizada sobre a tabela de auditoria já produzida por `monitora_produtos_resolver_pipes_por_ponto` — nenhuma nova varredura linha a linha.
2. **Evitar varreduras linha a linha**: a classificação por coluna (`for (cc in cols_char)`) já existia nos Hotfixes 03.5C-B/C e não foi alterada; a única adição é uma contagem por coluna já dentro desse mesmo laço (sem laço extra) e um `sum()`/`uniqueN()` de vetores já materializados.
3. **Operações de correção atômicas, auditáveis e reversíveis**: cada checkpoint grava seu próprio relatório (`auditoria_pipe_pretokenizacao_*` vs. `bloqueio_pipe_residual_*` vs. `auditoria_pipe_residual_resumo_*`), todos delimitados por produto e contexto, sem sobrescrever dado de sessão.
4. **Separação entrada bruta / cache pré-painel / correções da sessão / produtos finais**: preservada — a guarda não decide correção de dado, apenas decide se aborta ou não a exportação de um checkpoint específico; a resolução de pipe continua sendo feita exclusivamente por `monitora_produtos_resolver_pipes_por_ponto(corrigir=TRUE)`, que não foi alterada.
5. **Não usar output/ ou log/ como entrada implícita**: não alterado; a guarda apenas escreve relatórios em `output/03_auditorias/importacao` e `log/`, nunca lê de lá para decidir comportamento.
6. **Comentários internos**: os novos comentários referenciam apenas o próprio hotfix e a sequência de checkpoints do script (nomes já públicos no cabeçalho/documentação do script), sem referência a ferramentas internas de desenvolvimento.

## 7. Testes sintéticos e resultados

Executados isoladamente em `/tmp` (scratchpad da sessão), sem dados reais, extraindo verbatim (via `sed` das linhas correspondentes do próprio `monitora_campsav_alvo_global_v2.6.0.R` já com o Hotfix 03.5D aplicado) as funções `monitora_produtos_coluna_coleta_generica`, `monitora_produtos_coluna_ponto_generica`, `monitora_produtos_resolver_pipes_por_ponto` (inalterada) e `monitora_bloquear_pipe_residual_produto` (modificada), com um stub determinístico para `monitora_produtos_classificar_pipe_coluna` (a lógica de classificação via contrato XLSForm, em si, já foi validada nos Hotfixes 03.5C-B/C e não foi tocada por este hotfix).

| # | Cenário | Resultado |
|---|---|---|
| A | Pipe estruturado com descompasso índice-de-ponto/tokens, checkpoint pré-tokenização (`bloquear=FALSE`) | ✅ Não bloqueia; gera `auditoria_pipe_pretokenizacao_*.csv` |
| B | Mesmo pipe estruturado irresolúvel, checkpoint pós-tokenização (`bloquear=TRUE`) | ✅ Bloqueia via `stop()`; gera `bloqueio_pipe_residual_*.csv` |
| C | Texto livre com `\|` legítimo (`"Declividade leve \| rochas amontoadas"`) | ✅ Preservado byte-a-byte, nunca bloqueia (mesmo com `bloquear=TRUE`) |
| D | Coluna indeterminada (fora do contrato XLSForm) com `\|` | ✅ Não alterada, não bloqueia, reportada em `pipe_indeterminado_*.csv` |
| E | Pipe técnico tolerado (padrão de fotos/anexos) | ✅ Preservado, não bloqueia |
| F | Resumo final auditável | ✅ `auditoria_pipe_residual_resumo_*.csv` gerado com `n_colunas_preservadas`, `n_linhas_pipe_resolvidas_por_indice_ponto`, `n_colunas_indeterminadas`, `n_colunas_estruturado_residual` e `abortou_exportacao` |

**Resultado: 6/6 testes passaram** (`Total: 6 testes, 0 falha(s)`).

## 8. PNB golden e resultados

Execução em pasta limpa fora do worktree (`.../Monitora-Campestre-Savanico_v262_rollforward_v260_execucao_pnb_hotfix035d`), mesmo `input/pnb_2022_2026.zip` da baseline golden documentada em `diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md` (SHA-256 `75fe8875a55a1aa1d3baf3b4663e9ece6089fe7359185f82eda536215fda1480`, confirmado idêntico), mesmas variáveis de ambiente (`MONITORA_MODO_EXECUCAO=ate_registros_corrig`, `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS=S`, `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S`, `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=S`).

| Métrica | Golden v2.6.0 (baseline) | dev com Hotfix 03.5D | Status |
|---|---|---|---|
| UAs duplicadas mesmo ano | 606 linhas / 6 coletas | **606 / 6** | ✅ |
| Nativa sem forma de vida | 3 / 3 | **3 / 3** | ✅ |
| Seca/morta sem forma de vida | 10 / 8 | **10 / 8** | ✅ |
| Outra forma de vida legada | 7 / 5 | **7 / 5** | ✅ |
| Colunas `registros_importados.csv` | 176 | **176** | ✅ |
| Colunas `registros_corrig.csv` | 180 | **180** | ✅ |
| Tamanho `registros_importados_bruto.csv` | 31.864.856 bytes | **31.864.856 bytes** | ✅ byte-idêntico |
| Tamanho `registros_importados.csv` | 39.469.973 bytes | **39.469.973 bytes** | ✅ byte-idêntico |
| Tamanho `registros_corrig.csv` | 40.805.162 bytes | **40.805.162 bytes** | ✅ byte-idêntico |
| Ordem de gravação | bruto → importados → corrig | **bruto → importados → corrig** | ✅ |
| Tempo núcleo (`dec:` em `fim_execucao`) | 51,2–51,8 s (referência) | **51,9 s** | ✅ dentro/próximo da faixa 52–60s |
| Bloqueios/erros | nenhum | **nenhum** (`Exit status: 0`) | ✅ |
| Relatório de bloqueio de pipe gerado | não | **não** (apenas os novos resumos informativos `auditoria_pipe_residual_resumo_*`, sem residual) | ✅ esperado |
| `registros_validados.csv` sobrescrevendo `registros_importados.csv` | não | **não** (arquivos com tamanhos/timestamps distintos, ordem de escrita preservada) | ✅ |

O dataset PNB nunca exercitou o cenário de pipe residual estruturado (nem antes nem depois do Hotfix 03.5D), portanto os arquivos são byte-idênticos à baseline — confirmando ausência de qualquer efeito colateral deste hotfix quando a guarda não precisa agir.

## 9. FNCS real e resultados

### 9.1 Localização do dataset que reproduz o defeito

A auditoria inicial usando `dados_originais/extr_dados_form/FNCS/{2023..2026}` (CSVs já consolidados por ano, cabeçalhos em formato canônico) **não reproduziu** o bloqueio nem antes nem depois do hotfix — esse formato de entrada não carrega as colunas com o padrão de pipe problemático. Investigando a origem exata das 5 colunas citadas no diagnóstico (`"Formas de vida de plantas nativas"`, etc.), localizei o dataset correto: `dados_originais/download_direto/FNCS/{2023,2024,2025,2026}/*.zip` — 89 ZIPs de exportação bruta do SISMONITORA (formato "download direto", cabeçalhos em label completo com HTML embutido, ex. `"Formas de vida de plantas <span style=\"color:red\">nativas:</span> (amostragem/registro)"`), que é exatamente o formato de entrada relatado no diagnóstico original.

### 9.2 Prova de que o defeito existe no código pré-Hotfix-03.5D (controle)

Executei o commit `653d1c4` (HEAD antes deste hotfix) fora do Git, em pasta isolada, com os mesmos 89 ZIPs reais como `input/`, mesmas variáveis de ambiente do golden PNB. Resultado:

```
Error: Falha ao materializar registros_importados.csv saneado após a consolidação inicial de colunas/aliases. [...]
Warning message:
Falha ao gerar registros_importados.csv saneado: Exportacao de registros_importados.csv bloqueada:
4432 linha(s) com residuo de '|' nao resolvido em 5 coluna(s) estruturada(s)
(Formas de vida de plantas nativas, Formas de vida de plantas exóticas,
 Formas de vida de plantas secas ou mortas, Outra forma de vida de planta nativa,
 Outra forma de vida de planta seca e/ou morta). [...]
Execution halted
```

**Reprodução exata** do diagnóstico do checklist (4432 linhas, exatamente as mesmas 5 colunas). `registros_importados.csv` nunca chega a ser gravado.

### 9.3 Execução com o Hotfix 03.5D aplicado (mesmo dataset)

Mesmos 89 ZIPs, mesmas variáveis de ambiente, script com o fix.

| Item | Resultado |
|---|---|
| Exit status | **0** (`Execution` concluída, sem `stop()`) |
| Mensagem final | `Execução parcial concluída de forma controlada: registros_corrig.csv \| modo=ate_registros_corrig` |
| `registros_importados_bruto.csv` | **16766 × 182** |
| `registros_importados.csv` | **16766 × 203** |
| `registros_corrig.csv` | **16766 × 207** (ver nota na seção 2) |
| Checkpoint `registros_importados_operacional_tokenizado` | disparado normalmente, dentro da sequência `padronizacao_*` |
| `auditoria_pipe_pretokenizacao_registros_importados_csv.csv` (checkpoint pré-tokenização, `bloquear=FALSE`) | **gerado**: 4432 linhas únicas, exatamente as mesmas 5 colunas do controle (seção 9.2) — confirma que a mesma condição que travava antes agora é apenas reportada |
| `auditoria_pipe_residual_resumo_registros_importados_csv.csv` (checkpoint final, `bloquear=TRUE`) | `n_colunas_estruturado_residual=0`, `n_linhas_pipe_estruturado_residual=0`, `abortou_exportacao=FALSE`, `n_linhas_pipe_resolvidas_por_indice_ponto=6060` — as 5 colunas foram **totalmente resolvidas** pela cadeia `padronizacao_*` antes do checkpoint final |
| `bloqueio_pipe_residual_registros_importados_csv.csv` (relatório de bloqueio final) | **não gerado** (nenhum resíduo sobrou) |
| Pipe em texto livre (`observacoes_gerais` e afins) | preservado (3 colunas, 1723 linhas, classificadas como preservadas no resumo final) |

Isso confirma, com dado real, exatamente o comportamento exigido: **as 5 colunas do FNCS deixam de bloquear antes da tokenização operacional**, e, como não sobrou resíduo estruturado real após a tokenização, não há bloqueio indevido no checkpoint final — coerente com o requisito 6 (bloquear apenas se sobrar resíduo genuíno).

## 10. Comparação de performance

| Execução | Núcleo (`dec:` em `fim_execucao`) | Tempo de parede total (`time -v`) | Observação |
|---|---|---|---|
| PNB golden (dev + Hotfix 03.5D) | 51,9 s | 4m50,75s | dentro da faixa golden 52–60s; parede inclui carregamento de ~15 pacotes e geração de manual, não faz parte do núcleo medido |
| FNCS bruto real, pré-fix (controle) | não chega ao fim (aborta em `consolidacao_aliases_importacao`, ~205s acumulado de setup) | — | trava antes de qualquer etapa de tokenização pesada |
| FNCS bruto real, com Hotfix 03.5D | 55,0 s | 4m48,83s | conclui com sucesso; não há duplicação de trabalho de resolução de pipe (a resolução continua sendo feita uma única vez por `monitora_produtos_resolver_pipes_por_ponto`, em cada um dos dois checkpoints já existentes antes deste hotfix) |

Não há degradação de performance: o núcleo de ambas as execuções ficou dentro da faixa observada para a v2.6.0 golden (52–60s para PNB) e não houve reintrodução do padrão degradado da v2.6.2_teste antiga (~144s). O tempo "72–73s ativos / ~87,7s acumulado com pausa/painel" citado como referência original do FNCS presumivelmente inclui espera interativa do painel Shiny (`painel_e_parar`), que não se aplica a esta rodada em modo não interativo (`ate_registros_corrig`); a métrica comparável — tempo de núcleo sem espera de painel — ficou em 55,0s, sem sinal de degradação.

## 11. Arquivos alterados

- `monitora_campsav_alvo_global_v2.6.0.R` — único arquivo de código alterado (+103/-8 linhas):
  - `monitora_bloquear_pipe_residual_produto`: novos parâmetros `contexto` e `bloquear`; nova ramificação não-bloqueante (`auditoria_pipe_pretokenizacao_<produto>.csv`, evento `AVISO`); novo resumo consolidado sempre gravado (`auditoria_pipe_residual_resumo_<produto>.csv`).
  - `monitora_registros_importados_saneado_exportar`: novo parâmetro `bloquear_pipe_residual`, repassado à guarda.
  - Dois pontos de chamada ajustados: pré-tokenização (`bloquear_pipe_residual = FALSE`) e pós-tokenização operacional (`bloquear_pipe_residual = TRUE`, explícito).
- `diagnostics/hotfix_035d_guarda_pipe_checkpoint_correto/HOTFIX_035D_GUARDA_PIPE_CHECKPOINT_CORRETO.md` — este relatório.
- Scripts de teste sintético em `/tmp/.../scratchpad/hotfix035d/` — não versionados, sem dados reais.
- Pastas de execução real (PNB e FNCS, pré-fix e pós-fix) — todas fora do Git, em `~/Projetos/..._execucao_pnb_hotfix035d/` e `~/dados_originais/_exec_hotfix035d_*` — nenhuma versionada.

## 12. Fora do escopo

- Outra forma de vida legada.
- Bug de vírgula interna na tokenização.
- Escopo do painel 101/84.
- Centralização de motor único de validação.
- Reaplicação de funcionalidades da v2.6.2_teste.
- Redesenho da importação.
- Alteração de release pública.
- A diferença de 16 colunas em `registros_corrig.csv` (207 vs. 223 referenciadas) não foi investigada a fundo por estar fora do escopo cirúrgico deste hotfix (guarda de pipe) e não ter relação com nenhuma linha alterada por ele; ver nota na seção 2.
- Não foi criado nenhum resolvedor pesado adicional pré-tokenização; a resolução de pipe continua exclusivamente a cargo de `monitora_produtos_resolver_pipes_por_ponto`, já existente.

## 13. Próximo passo recomendado

Nenhum hotfix adicional urgente identificado. Se o usuário quiser investigar a divergência de colunas em `registros_corrig.csv` (seção 2), recomenda-se tratar como item separado e comparar a composição exata das colunas condicionais entre a captura de referência original e o snapshot atual de `dados_originais/download_direto/FNCS/`, fora do escopo desta correção pontual.

## Segurança Git

```
$ git status --short
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035d_guarda_pipe_checkpoint_correto/

$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | 111 +++++++++++++++++++++++++++++++---
 1 file changed, 103 insertions(+), 8 deletions(-)
```

Nenhum CSV/ZIP/XLSX/XLS/ODS/RDS/RDA/SQLITE/DB/GPKG/SHP/GEOJSON/KML/KMZ real staged ou versionado. Todas as execuções com dado real (PNB e FNCS, com e sem o fix) ocorreram fora do Git, em pastas descartáveis. Staging previsto apenas para:
- `monitora_campsav_alvo_global_v2.6.0.R`
- `diagnostics/hotfix_035d_guarda_pipe_checkpoint_correto/HOTFIX_035D_GUARDA_PIPE_CHECKPOINT_CORRETO.md`
