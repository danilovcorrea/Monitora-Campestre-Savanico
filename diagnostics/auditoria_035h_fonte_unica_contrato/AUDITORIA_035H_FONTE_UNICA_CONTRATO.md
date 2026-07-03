# Auditoria 03.5H — Inventário de fontes de contrato antes de fonte única

## 1. Resumo executivo

Auditoria puramente estática (grep/leitura de `monitora_campsav_alvo_global_v2.6.0.R`, sem editar código, sem rodar PNB/FNCS, sem ler dado real). Objetivo: mapear **todas** as fontes atuais que hoje representam "contrato" (schema/aliases/template/relevance/choices/cardinalidade) antes de qualquer criação de fonte única embutida.

Achado central: não existe hoje uma fonte única — existem **pelo menos 3 fontes de schema independentes** e **5 subsistemas de alias isolados**, que não se referenciam entre si. A mais completa e já embutida sem dependência externa é o dump XLSForm de `monitora_correcao_xlsforms_embutidos()` (4 versões, 3 tabelas: `campos`/`opcoes`/`dependencias`, cobrindo type, tipo_base, list_name, label, relevant, required). As demais fontes (schema `validados`, aliases `validados`, aliases `pipe`, aliases `dt_consolidar`, aliases `publicacao_p`) são recortes/hardcodes paralelos, parcialmente sobrepostos, mantidos manualmente e sem elo de derivação com o dump XLSForm.

Também foram encontradas **duas duplicações reais de função** (não apenas nomenclatura) diretamente na área de contrato: `monitora_publicacao_c_normalizar_contratos_pre_exportacao` (L7653 vs L8682) e `monitora_produtos_classificar_pipe_coluna` (L24587 vs L31827). Em R, a segunda definição sobrescreve silenciosamente a primeira — a versão antiga fica morta, mas continua no arquivo e pode confundir quem for consolidar. Isso não é um bug hoje (a versão viva é a mais recente e mais correta em ambos os casos), mas é um risco direto para a fonte única.

Nenhuma pendência foi escondida: a lacuna que causou a regressão do Hotfix 03.5G (campo condicional/esparso tratado como se fosse completo) **continua sem solução estrutural** — é discutida na seção 9/11 como o item de maior risco para a fonte única.

## 2. Metodologia

Leitura estática via `grep -n` e inspeção de trechos do arquivo único (46001 linhas). Nenhuma tabela/função nova foi criada, nenhum dado real foi lido, nenhuma execução (PNB/FNCS/sintética) foi realizada. Referências de linha abaixo são do estado atual do branch `dev-v2.6.2-rollforward-golden-v260` (HEAD local após Hotfix 03.5G2).

## 3. Q1 — Funções/tabelas que hoje representam contrato

| Fonte | Local | O que cobre | Classificação |
|---|---|---|---|
| `monitora_correcao_xlsforms_embutidos()` | L3883 | Dump embutido (sem XLSX/CSV externo) das 4 versões de formulário (`03MAI24`, `05MAI23`, `21FEV25_nSrC9X3`, `ago2022`): tabela `campos` (name/caminho/type/tipo_base/list_name/label/relevant/required), `opcoes` (choices por list_name) e `dependencias` (relevance explícito) | **fonte a consolidar** (núcleo da fonte única) |
| `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` | L6143 | Memoização + colunas normalizadas (`*_norm_publicacao_ae`) + flag `arquivo_21fev25_publicacao_ae` sobre a fonte acima | **adaptador/consumidor** |
| `monitora_correcao_dependencias_padrao()` | L6206 | Regras de relevance **hardcoded** (não extraídas do XLSForm) para 4 formas condicionais (bromelioide/cactacea/orquidea/samambaia × nativa/exotica/seca_morta) | **fonte a consolidar** (é regra real, mas paralela) |
| `monitora_validados_schema_embutido()` | L27280 | Tabela hardcoded de 129 atributos (posicao/atributo/formato/largura/obrigatorio_valor/nivel) — o "template SISMonitora" para `registros_validados.csv`. **Sem nenhum elo com o dump XLSForm.** | **fonte a consolidar** (ou substituível se o dump XLSForm já cobrir os mesmos 129 atributos — não verificado nesta auditoria) |

## 4. Q2 — Duplicadas ou concorrentes

- **Duplicação real de função** (mesmo nome, corpo diferente, R usa a última): `monitora_publicacao_c_normalizar_contratos_pre_exportacao` definida em L7653 (assinatura simples) e novamente em L8682 (assinatura com `linhas`/`forcar_global`, escopo incremental via `monitora_publicacao_g_definir_linhas_contrato`). A de L7653 é morta em runtime. — **legado a remover depois**.
- **Duplicação real de função**: `monitora_produtos_classificar_pipe_coluna` definida em L24587 (heurística por regex de nome, sem consultar contrato) e novamente em L31827 (consulta `monitora_pipe_coluna_classificar_natureza()`, trata `pipe_ambiguo`, prioriza contrato sobre regex). A de L24587 é morta em runtime. — **legado a remover depois**.
- Padrão mais amplo: o arquivo tem ~50 outros nomes de função top-level redefinidos (`grep` de duplicatas), majoritariamente fora do escopo de contrato (relatórios de exóticas, cache, produtos). Não auditado item a item aqui — fica registrado como padrão de risco do arquivo, a revisar separadamente antes da fonte única (evitar que a consolidação herde uma versão morta por engano).
- **Concorrência funcional (não duplicação literal) entre 3 caminhos de resolução de coluna**: (a) `monitora_correcao_candidatos_coluna_xlsform`/`resolver_coluna_xlsform_registro` (L3491/L3556, score por linha via `monitora_correcao_xlsform_registro`, L3426), (b) `monitora_publicacao_p_resolver_coluna_por_alias_e_perfil` (L2721, alias+regex+perfil, por campo), (c) `monitora_pipe_coluna_classificar_natureza` (L31780, 4 estratégias exatas sobre `campos`). As três resolvem "a que campo do contrato esta coluna corresponde", com estratégias e tabelas de alias diferentes e não compartilhadas.

## 5. Q3 — Hard-codes de atributos

- `monitora_validados_schema_embutido()` (L27280): 129 atributos, formatos e níveis inteiramente hardcoded (não gerado do XLSForm).
- `monitora_correcao_dependencias_padrao()` (L6206): lista fixa de tokens condicionais (`bromelioide, cactacea, orquidea, samambaia` × `nativa, exotica, seca_morta`).
- `monitora_dt_consolidar_aliases_colunas()` (L26349): `alias_map` com 15 entradas hardcoded (COLETA/UC/UA/CICLO/... e colunas de registro).
- `monitora_publicacao_p_resolver_coluna_ponto_amostral`/`_ponto_metro` (L2747/L2764): listas de alias + regex hardcoded por campo, um par de funções por atributo (padrão não escalável para os ~223 atributos de `registros_corrig`).
- Regex de classificação técnica/mídia hardcoded em `monitora_produtos_classificar_pipe_coluna` (`foto|imagem|image|media|arquivo_midias|anexo`, L31831) e prefixos técnicos (`^(monitora_|arquivo_|origem_|hash_|md5|tipo_entrada|\.id$)`, L31832) — nenhum dos dois vem do contrato XLSForm (que não distingue "mídia" como categoria própria).

## 6. Q4 — Aliases isolados por subsistema

| Subsistema | Função(ões) | Escopo | Isolamento |
|---|---|---|---|
| Pipe (importados) | `monitora_pipe_aliases_campos_conhecidos()` L31735 | 3 entradas (impact_manejo_uso, liana) | Não consultado por nenhum outro subsistema |
| Validados | `monitora_validados_aliases_xlsform_historico()` L27381 + `monitora_validados_aliases_adicionais()` L27498 + `monitora_validados_unir_aliases()`/`monitora_validados_aliases()` L27592/27600 | ~60+ entradas, cobre rótulos históricos completos (inclui variantes com `<span>`, sufixos `.1`/`.2`/`__dup1`) | Não consultado pelo resolvedor de pipe nem por `dt_consolidar_aliases_colunas` |
| Consolidação de colunas | `monitora_dt_consolidar_aliases_colunas()` L26349 | 15 entradas, roda antes de dedupe | Própria tabela, não reaproveita nenhuma das outras |
| Publicação/perfil | `monitora_publicacao_p_resolver_coluna_por_alias_e_perfil()` L2721 + wrappers | 2 campos cobertos hoje (ponto_amostral, ponto_metro) | Padrão próprio (alias+regex+perfil), não usa `campos`/`opcoes` do dump |
| Correção (fuzzy) | `monitora_correcao_candidatos_coluna_xlsform()` L3491 | Não é uma tabela de alias, é scoring sobre `name`/`caminho_registro`/`label` do dump | Consulta o dump diretamente, mas com heurística própria de score, não compartilhada |

Confirma achado já registrado na Auditoria 03.5F: o rótulo "A bromélia observada é: (amostragem/registro)" **já está mapeado** em `monitora_validados_aliases_adicionais()` (L27504) desde antes do Hotfix 03.5G — só não era consultado pelo resolvedor de pipe. Isso não muda com o Hotfix 03.5G2 (que restringiu o alias de pipe, não tocou em `validados`).

## 7. Q5 — Regras de relevance/dependência

- Fonte primária: coluna `relevant` dentro de `campos` (dump embutido, ex.: `selected(${tipos_impacto_manejo_uso}, 'outros')`) e a tabela `dependencias` (mesma função, `arquivo_xlsform/parent_name/token/dependent_name/.../relevant/fonte`), com `fonte` distinguindo `xlsform_embutido` de `regra_padrao_monitora`.
- Suplemento hardcoded: `monitora_correcao_dependencias_padrao()` (L6206) gera `relevant` sintético (`selected(${forma_vida_<categoria>}, '<token>')`) para os 4 formas condicionais, mesclado via `monitora_correcao_unificar_dependencias()` (L6224).
- Reimplementação independente (não lê a coluna `relevant`): `monitora_validados_validar_condicionais_xlsform21()` (L28496) — regras de exclusividade/pareamento (`solo_nu` exclusivo, pares categoria→forma_vida) escritas diretamente em R, sem derivar da string `relevant` do contrato.
- **Gap estrutural**: nenhuma das três fontes acima expressa "este campo é condicional/esparso na prática de campo" (proporção real de linhas onde a condição é satisfeita). O `relevant` é uma expressão XLSForm válida, mas o resolvedor posicional (`monitora_correcao_xlsform_registro`, por linha) não a interpreta — foi exatamente essa lacuna que causou a regressão de 275 linhas no Hotfix 03.5G (ver `diagnostics/hotfix_035g2_restringe_aliases_pipe/`).

## 8. Q6 — Regras de choices/domínios

- Fonte primária: tabela `opcoes` do dump embutido (mesma função L3883), consultada por `monitora_correcao_choices_xlsform()` (L10139, filtra por `list_name`, prefere a versão `21FEV25` quando `preferir_ultima=TRUE`).
- Adaptador especializado: `monitora_validados_xlsform21_meta/_campo/_mapa_lista/_choices` (L28090–28182) — **não** é fonte paralela; reusa `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae()` e apenas filtra/cacheia o subconjunto `21FEV25` em variáveis globais (`MONITORA_PUBLICACAO_AE_XLSFORM21_*_CACHE`, `.GlobalEnv`) — **adaptador/consumidor**, mas com efeito colateral de estado global a documentar antes de mexer.
- Validação de domínio: `monitora_validados_validar_dominios_xlsform21()` (L28449) — consumidor de `opcoes` via o adaptador acima.

## 9. Q7/Q8/Q9 — O que consolidar, o que vira adaptador, o que remover depois

**Fonte a consolidar (núcleo da fonte única):**
- `campos`/`opcoes`/`dependencias` de `monitora_correcao_xlsforms_embutidos()` — já embutido, já cobre 4 versões, já tem `relevant`/`required`/`tipo_base`/`list_name`.
- `monitora_correcao_dependencias_padrao()` — regra real, precisa entrar na fonte única (hoje é hardcode paralelo).
- `monitora_validados_schema_embutido()` — 129 atributos; decidir se é subconjunto derivável de `campos` (por posição do produto validado) ou se carrega informação (formato/largura/nível) que o dump XLSForm não tem — **não verificado nesta auditoria**, é o primeiro item a investigar antes de qualquer merge.

**Adaptador/consumidor (manter como camada fina sobre a fonte única):**
- `monitora_correcao_xlsforms_embutidos_cache_publicacao_ae`, `monitora_validados_xlsform21_*`, `monitora_correcao_choices_xlsform`, `monitora_pipe_coluna_classificar_natureza`, `monitora_correcao_colunas_template_sismonitora`, `monitora_correcao_attrs_template_sismonitora_editaveis`.

**Substituível (a lógica pode ser gerada a partir da fonte única em vez de mantida à mão):**
- `monitora_pipe_aliases_campos_conhecidos`, `monitora_dt_consolidar_aliases_colunas` (alias_map), `monitora_publicacao_p_resolver_coluna_*` (aliases hardcoded) — todos poderiam ler de um índice de aliases único derivado do dump + `validados_aliases_adicionais` unificado.

**Legado a remover depois (dead code confirmado):**
- `monitora_publicacao_c_normalizar_contratos_pre_exportacao` (definição de L7653).
- `monitora_produtos_classificar_pipe_coluna` (definição de L24587).

**Risco de regressão (não remover/alterar sem novo hotfix dedicado):**
- `monitora_correcao_xlsform_registro` + `monitora_correcao_resolver_coluna_xlsform_registro` — resolvedor **por linha** (score sobre `.id`/`PROTOCOLO`/`controle_versao`), é o padrão que já causou a regressão 03.5G quando aplicado a campo condicional/esparso sem checar esparsidade real.
- `monitora_validados_validar_condicionais_xlsform21` — reimplementa relevance à mão; migrar para leitura de `relevant` sem replicar exatamente as mesmas regras pode mudar resultado de validação de `registros_validados.csv`.
- Estado global (`.GlobalEnv`) nos caches `MONITORA_PUBLICACAO_AE_XLSFORM21_*` — qualquer consolidação que mude a chave de cache precisa garantir invalidação, senão risco de meta "presa" entre runs/testes.

## 10. Q10 — Como preservar performance

- O dump embutido já é `data.table` gerado uma vez via `fread` de TSV embutido (`monitora_correcao_fread_tsv_embutido`) + cache global (`monitora_correcao_xlsforms_embutidos_cache_publicacao_ae`) — o padrão de cache/índice já existe e deve ser o modelo para a fonte única (não recriar por linha).
- Ponto de atenção real: `monitora_correcao_xlsform_registro()` é chamada **por linha** (`linha` é escalar) dentro de `monitora_correcao_resolver_coluna_xlsform_registro`/fluxos de correção de forma de vida — qualquer fonte única que substitua isso deve vetorizar (join por chave em vez de loop de score por linha) para não piorar o custo atual.
- `monitora_correcao_candidatos_coluna_xlsform` já é vetorizado sobre colunas (data.table), não sobre linhas — manter esse padrão como referência para os resolvedores substituíveis da seção 9.
- `monitora_dt_consolidar_aliases_colunas` já documenta explicitamente cuidado de memória ("evita materializar várias colunas grandes ao mesmo tempo") e usa `monitora_recurso_controlar(..., risco = "alto", ...)` — a fonte única deve manter esse mesmo controle de recurso ao expor os novos índices derivados.
- Índices a criar na fonte única (evitar recomputar por chamada): `name_norm`, `caminho_norm`, `label_norm` já existem como padrão (`*_norm_publicacao_ae`) — generalizar para todas as tabelas (schema validados, alias maps) em vez de normalizar ad-hoc em cada função consumidora.

## 11. Q11 — Como migrar sem quebrar PNB/FNCS

1. Não criar fonte paralela agora (conforme escopo desta tarefa) — esta auditoria é só o inventário.
2. Antes de mesclar `monitora_validados_schema_embutido` no dump XLSForm, confirmar item a item (script à parte, sem rodar pipeline pesado) se os 129 atributos batem 1:1 com `caminho_registro`/`name` do dump — só então decidir "fonte a consolidar" vs "camada adicional que o dump não cobre".
3. Migrar primeiro os itens "substituíveis" (seção 9) para ler da fonte única mantendo a mesma saída (mesmos aliases, mesma ordem de prioridade) — validar por comparação de saída (dry-run comparando classificação antiga vs nova coluna a coluna, sem rodar FNCS completo).
4. Só depois de (3) estabilizado, atacar os itens "risco de regressão": resolvedor por linha e `validar_condicionais_xlsform21`. Cada um deve virar hotfix isolado com relatório próprio (mesmo padrão dos hotfixes 03.5D–03.5G2), não uma migração única.
5. Remover o dead code confirmado (`monitora_publicacao_c_normalizar_contratos_pre_exportacao` L7653, `monitora_produtos_classificar_pipe_coluna` L24587) só depois de confirmar por `grep` que nenhuma chamada no arquivo referencia a versão antiga por acidente (elas são shadowed, não chamadas diretamente por posição, mas confirmar antes de apagar).
6. Preservar em todas as etapas a separação de produtos `registros_importados_bruto` → `registros_importados` → `registros_corrig` → `registros_validados` (já reforçada em código, ex.: guarda `permitir_apenas_bruto` em `monitora_registros_importados_exportar`, L31846) — a fonte única deve ser consultada por cada estágio, não colapsar os estágios.
7. Cardinalidade de pipe: o vocabulário atual (`pipe_permitido_texto_livre`, `pipe_residual_estruturado_elegivel`, `pipe_ambiguo`, `pipe_indeterminado`, ver `monitora_pipe_coluna_classificar_natureza`, L31780) ainda não distingue "estruturado completo" de "estruturado condicional/esparso" nem isola "select_multiple" de "select_one", nem tem categoria própria para "técnico/mídia" ou "fora do contrato" — a fonte única deveria introduzir essas categorias explicitamente para que o caso do Hotfix 03.5G (campo select_one correto no contrato, mas esparso na prática) fique representável e detectável estaticamente, em vez de só aparecer via resíduo bloqueante em run real como aconteceu em COLETA 11168.

## 12. Nenhuma pendência escondida

- O gap da seção 7 (relevance sem noção de esparsidade real) e da seção 11 item 7 (cardinalidade sem "condicional/esparso" como categoria) são as duas pendências mais relevantes para a fonte única e ficam registradas aqui, não resolvidas.
- Se os 129 atributos de `monitora_validados_schema_embutido` não baterem 1:1 com o dump XLSForm (não verificado — ver seção 11 item 2), a fonte única precisará de um segundo nível (schema de exportação) além do schema de formulário, não um único achatamento.
