# Auditoria 03.5H4 — Estratégia de testes automatizados para 03.5I–03.5N

Baseado no inventário local já existente (`/home/dfed/dados_originais/_inventario_monitora_20260702_165610`, gerado 2026-07-02, metadados/contagens agregadas apenas — sem linha bruta, UUID, coordenada, nome, e-mail, CPF, telefone ou valor de célula identificável). Nenhum dado real foi lido linha a linha nesta auditoria; só os CSVs de metadados/contagens do próprio inventário e o relatório já existente `diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md`. Nenhuma execução (PNB/FNCS/pipeline pesado) foi realizada.

## 1. Diretório principal dos datasets representativos

`/home/dfed/dados_originais/_inventario_monitora_20260702_165610` (inventário/metadados) apontando para dados reais em `/home/dfed/dados_originais/extr_dados_form/`, `/home/dfed/dados_originais/download_direto/`, `/home/dfed/dados_originais/planilha/` — **nenhum desses caminhos deve ser copiado para dentro do repositório**. Os testes devem rodar em pasta de execução limpa fora do worktree (mesmo padrão já usado no baseline PNB: `..._execucao_pnb/`), nunca commitando `input/` nem `output/`.

Schemas identificados no inventário (relevantes para cobertura de teste):

| Schema | Versão XLSForm | Arquivos | Linhas totais | Papel |
|---|---|---:|---:|---|
| `xlsform_2025_129` | `21FEV25` (129 colunas — mesmo schema auditado em 03.5H2) | 37 | 195.132 | Cobertura da versão atual/foco da fonte única |
| `intermediario_98` | `05MAI23` | 15 | 57.671 | Cobertura histórica intermediária |
| `intermediario_100` | `03MAI24` | 10 | 56.762 | Cobertura histórica intermediária |
| `legado_80` | `11AGO22` | 10 | 25.654 | Cobertura histórica legada (80 colunas) |
| `outro_*` (`download_direto`/`planilha`) | produtos já processados (`registros_corrig.csv` etc.) | ~1900 | ~304.000 | Produtos pós-pipeline já existentes — úteis para teste de camada painel/consolidação sem reprocessar bruto |

## 2. Datasets candidatos

| Papel | Dataset recomendado | Dimensões | Justificativa |
|---|---|---|---|
| **PNB golden** | `input/pnb_2022_2026.zip` (SHA-256 `75fe8875...fda1480`), já usado e documentado em `diagnostics/golden_v260_baseline/BASELINE_V260_PNB.md` | Bundle 2022–2026, produto de referência `registros_corrig.csv`: **180 colunas × 24.241 linhas**, núcleo do pipeline em ~51–52s | Já é o baseline golden estabelecido do projeto (schemas `legado_80`+`intermediario_98`/`100`+`xlsform_2025_129` no mesmo bundle) — reusar, não recriar |
| **FNCS regressão pipes/bromélia** | `extr_dados_form/FNCS/2025/FNCS_2025_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv` (5050 linhas, 50 coletas) e/ou `extr_dados_form/FNCS/2026/FNCS_2026_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv` (5858 linhas, 58 coletas) | Schema `xlsform_2025_129`; `outra_forma_vida`=99/295, `serrapilheira`=4932/5041 | É literalmente o dataset onde a regressão do Hotfix 03.5G foi encontrada em produção (COLETA 11168, citado no commit `c5355a3`) — mantém o caso real de referência para qualquer mudança em pipe/bromélia |
| **Dataset médio representativo (linha histórica)** | `extr_dados_form/PNCV/2023/PNCV_2023_PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23.csv` | 12.221 linhas, 121 coletas, schema `intermediario_98` | Maior volume da linha 05MAI23 fora do PNB, bom para checar `serrapilheira_sem_filho` (diferença estrutural esperada em schema não-2025, não é erro) |
| **Dataset médio representativo (linha 2025)** | `extr_dados_form/FNC/2025/FNC_2025_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv` | 6060 linhas, 60 coletas, schema `xlsform_2025_129`, `outra_forma_vida`=372, `serrapilheira`=2856 | Volume médio da linha 2025 com boa cobertura de ambos os diagnósticos |
| **Dataset painel representativo** | `extr_dados_form/PNSC_CIPO/2026/PNSC_CIPO_2026_PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.csv` | 8585 linhas, 85 coletas, schema `xlsform_2025_129`, maior `outra_forma_vida`=433 e alta `serrapilheira`=2878 do inventário | Melhor cobertura combinada de diagnósticos entre os candidatos 2025 — bom para exercitar painel/`registros_validados`/relatórios com `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S` |
| **Testes sintéticos pequenos** | Não vêm do inventário — construir a partir dos padrões já documentados (Hotfix 03.5G2: bromélia condicional/esparsa nos pontos 90/91; Auditoria 03.5F: rótulos com sufixo `(amostragem/registro)` e `__dup2`) | Dezenas de linhas, não centenas | Alinhado à recomendação do próprio inventário: "quando uma falha real aparecer, converter o padrão para caso sintético/minimizado antes de versionar qualquer teste" — nunca versionar amostra real |

## 3–4. Estratégia de teste por etapa

### 03.5I — Fonte única embutida (contrato isolado)

- **Rodar**: só testes de contrato em R isolado (extrair as funções da fonte única para script separado e chamar diretamente, mesmo método usado nas auditorias 03.5H2/03.5H3) — comparar linha a linha, atributo a atributo, contra `monitora_validados_schema_embutido()` e `monitora_correcao_xlsforms_embutidos()` atuais.
- **Não rodar**: nenhum dataset real, nenhum PNB, nenhum FNCS, nenhum pipeline.
- **Custo esperado**: segundos (só carregamento de tabelas embutidas em memória).
- **Critérios de aprovação**: 100% dos 113 atributos que já batiam em 03.5H2 continuam batendo pela mesma estratégia; `ea`/`ua` resolvem para `estacao_amostral`/`unidade_amostral`; nenhuma tabela nova referenciada por código de produção ainda.
- **Parar e pedir análise** se: qualquer atributo que batia deixar de bater, ou se a fonte única exigir mudar `monitora_correcao_colunas_chave`/`monitora_esp_colunas_chave`/`monitora_validados_aliases` nesta etapa (fora de escopo de 03.5I).

### 03.5J — Índices/caches derivados (contrato + índices)

- **Rodar**: mesmo teste de contrato isolado de 03.5I + verificação de que os índices (`name_norm`, `caminho_norm`, `label_norm`) são determinísticos (mesma entrada → mesma saída, sem estado global vazando entre chamadas) e que o cache invalida corretamente entre execuções simuladas.
- **Não rodar**: dataset real, PNB, FNCS.
- **Custo esperado**: segundos a poucos minutos (микро-benchmark de tempo de construção do índice, sem I/O de produto).
- **Critérios de aprovação**: tempo de construção do índice não regressivo vs. baseline atual de `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()`; nenhuma mudança de contrato herdada da etapa 03.5I.
- **Parar e pedir análise** se: cache não invalidar entre chamadas (risco já sinalizado na Auditoria 03.5H, `.GlobalEnv` de `MONITORA_PUBLICACAO_AE_XLSFORM21_*`).

### 03.5K — Mapa observado→canônico (1 dataset representativo, diagnóstico apenas)

- **Rodar**: 1 dataset — recomendado `PNSC_CIPO/2026` (painel representativo, seção 2) — só para gerar o relatório diagnóstico comparando nomes de coluna reais contra a fonte única; **sem aplicar nenhuma decisão automática**, sem escrever produto final.
- **Não rodar**: PNB completo, FNCS completo, nenhum outro dataset além do escolhido, `registros_validados` (ainda não é objetivo desta etapa).
- **Custo esperado**: 1 leitura de CSV (~9 MB) + comparação de nomes — segundos a poucos minutos, sem rodar o pipeline de correção completo.
- **Critérios de aprovação**: relatório gerado lista 100% das colunas do dataset escolhido, classificadas (bate exato / bate por alias / sem match), sem colunas "esquecidas" silenciosamente.
- **Parar e pedir análise** se: aparecer alguma coluna real não classificável por nenhuma estratégia já mapeada nas auditorias 03.5H/H2/H3.

### 03.5L — Consolidar `registros_importados` (PNB + FNCS + 1 médio)

- **Rodar**: PNB golden (bundle 2022–2026, seção 2) **até `registros_importados`** (não precisa rodar até `registros_corrig`/`registros_validados` nesta etapa) + FNCS 2025 ou 2026 (regressão pipe/bromélia) + 1 dataset médio (`PNCV_2023` ou `FNC_2025`, seção 2).
- **Não rodar**: `registros_validados` (fora de escopo desta etapa — só `registros_importados_bruto`/`registros_importados`), validação espacial completa, os ~1900 arquivos `outro_*` do inventário (fora do escopo desta migração).
- **Custo esperado**: minutos por dataset (ordem de grandeza do baseline PNB documentado: núcleo ~51s para o bundle completo até `registros_corrig`; até `registros_importados` deve ser mais rápido).
- **Critérios de aprovação**: dimensões (linhas × colunas) de `registros_importados_bruto.csv`/`registros_importados.csv` idênticas às do baseline PNB já documentado (176 colunas × 24.241 linhas para `registros_importados.csv`); nenhuma coluna nova/removida sem explicação; `n_colunas_indeterminadas`/`n_linhas_pipe_indeterminadas` (auditoria de pipe) não pioram para FNCS.
- **Parar e pedir análise** se: qualquer critério de parada da seção 6 disparar.

### 03.5M — Migrar pipes por relevance/cardinalidade (FNCS obrigatório + sintético pequeno)

- **Rodar**: FNCS 2025 e/ou 2026 (**obrigatório** — é o dataset da regressão original) até `registros_corrig` + o(s) caso(s) sintético(s) pequeno(s) da seção 2 (bromélia condicional/esparsa, rótulos com sufixo/`__dup`).
- **Não rodar**: PNB completo (só se FNCS + sintético passarem primeiro); os ~1900 arquivos `outro_*`; `registros_validados` ainda não é o foco (entra em 03.5N).
- **Custo esperado**: minutos para FNCS (dataset pequeno-médio, 5050–5858 linhas) + segundos para os sintéticos.
- **Critérios de aprovação**: zero resíduo estruturado bloqueante para os campos condicionais já conhecidos (bromelioide/cactácea/orquídea/samambaia); nova categoria de cardinalidade "estruturado condicional/esparso" (03.5I seção 4) classifica corretamente o caso que causou a regressão 03.5G sem reintroduzi-la; `pipe_indeterminado` não aumenta para FNCS em relação ao estado atual (pós-03.5G2).
- **Parar e pedir análise** se: qualquer resíduo estruturado bloqueante reaparecer em FNCS, ou se o caso sintético da bromélia esparsa não for classificado como "condicional/esparso".

### 03.5N — Migrar painel/validados/relatórios (PNB + FNCS + painel representativo)

- **Rodar**: PNB golden completo (até `registros_corrig`, replicando exatamente o baseline já documentado) + FNCS + dataset painel representativo (`PNSC_CIPO/2026`, seção 2) com `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S` e `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=S` ativados (é a etapa que introduz `registros_validados.csv` e o módulo espacial na cadeia de teste, únicos consumidores de `ea`/`ua` completo).
- **Não rodar**: os ~1900 arquivos `outro_*` de `download_direto`/`planilha` (volume não justifica nesta etapa; usar como amostra adicional só se algo específico deles for citado como regressão).
- **Custo esperado**: maior desta cascata — ordem de grandeza do baseline PNB completo (~4-5 min de parede) × 3 datasets; é a única etapa em que rodar PNB completo é obrigatório e esperado.
- **Critérios de aprovação**: `registros_validados.csv` com 129 colunas (quando ativado) batendo com o schema já auditado (03.5H2); `chaves$ea`/`chaves$ua` resolvendo sem `NA` nos 73+4 pontos de consumo mapeados (03.5H3); nenhuma divergência de schema/ordem de coluna não explicada em nenhum dos 3 produtos.
- **Parar e pedir análise** se: `ea`/`ua` resolverem para `NA` em qualquer um dos 3 datasets, ou se `registros_validados.csv` divergir de 129 colunas sem explicação documentada.

## 5. Métricas de assinatura (a coletar em toda etapa que rodar dataset real)

- Dimensões (linhas × colunas) de cada produto gerado (`registros_importados_bruto`, `registros_importados`, `registros_corrig`, `registros_validados` quando ativo) — mesmo padrão já usado no baseline PNB.
- Schema/ordem de colunas (hash ou lista ordenada) — detectar renomeação/reordenação não intencional.
- Contagens diagnósticas do próprio inventário como referência prévia: `n_outra_forma_vida`, `n_serrapilheira`, `n_serrapilheira_sem_filho`, `n_solo_nu_coexistindo`, `n_encostam_vazio` — comparar valor pré-mudança vs. pós-mudança no mesmo dataset, não só valor absoluto.
- Auditoria de pipe (`n_colunas_indeterminadas`, `n_linhas_pipe_indeterminadas`, contagem por categoria de `monitora_pipe_coluna_classificar_natureza`) — mesmo relatório já usado nas auditorias 03.5F/03.5G2.
- Encostam: vazio, vírgula residual, `nativa`/`exotica` colada, `solo_nu` coexistindo com outra categoria — os 4 indicadores já usados no Hotfix 03.5E/Auditoria 03.5F.
- `registros_validados.csv`: 129 colunas exatas quando `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS=S`.
- Tempo total e por etapa (marcador interno `dec:` do próprio script, não só tempo de parede) — comparar contra os ~51–52s já documentados como golden para o bundle PNB completo.
- Confirmação de ausência de dado real staged (`git status --short` limpo de `input`/`output`/CSV de produto) antes de qualquer commit futuro.

## 6. Critérios de parada (qualquer um interrompe a cascata e pede análise do usuário/ChatGPT antes de continuar)

- Alteração não explicada em schema (coluna nova, removida ou reordenada sem justificativa documentada).
- Alteração não explicada em contagens diagnósticas (seção 5) entre execução de referência e execução da etapa.
- Novo pipe residual estruturado bloqueante (qualquer categoria além de `pipe_indeterminado`/`pipe_permitido_texto_livre` inesperada).
- Regressão em qualquer um dos 4 indicadores de Encostam (vazio, vírgula, colada, `solo_nu` coexistindo).
- Divergência em `registros_validados.csv` (contagem de colunas ≠ 129, ou `ea`/`ua` resolvendo para `NA`).
- Piora relevante de performance (tempo interno do núcleo do pipeline acima da faixa golden documentada, ~52–60s para o bundle PNB completo, sem explicação de escopo adicional).
- Qualquer dado real staged (`git status --short` mostrando CSV/ZIP/XLSX de produto ou de `dados_originais`).
- Necessidade de rodar teste pesado fora do combinado nesta matriz (qualquer dataset `outro_*`/`download_direto`/`planilha` fora dos já listados, ou execução completa de todos os 1975 CSVs do inventário) — só com aprovação explícita antes de rodar.

## Nenhuma pendência escondida

Este relatório não decide qual dataset usar em cada rodada real futura além da recomendação acima — a escolha final de execução (quando, com quais variáveis de ambiente, em qual pasta limpa) fica para quando cada etapa (03.5I–03.5N) for efetivamente executada, não nesta auditoria.
