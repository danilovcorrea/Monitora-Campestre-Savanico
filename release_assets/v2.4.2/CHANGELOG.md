## v2.4.2 - 2026-06-23

### Corrigido
- Recálculo de `DATA_MONITORA_PARSEADA` e `ANO` após alteração de `Data (data_hora)`.
- Autorização controlada de recálculos derivados de data na auditoria de colunas protegidas.
- Remoção de aliases indevidos de `Data (data_hora)`, especialmente `data_do_registro`.
- Preenchimento automático do valor original esperado para atributos de data e atributos de COLETA.
- Exclusão auditável de uma única COLETA no painel.
- Sincronização das cópias públicas do script e sanitização completa da publicação.

### Alterado
- Controle de entrada no painel com validação por tipo de atributo e domínio XLSForm/SISMONITORA.
- Restrições de operação por atributo, preservando operações atômicas e performance com `data.table`.

## v2.4.1 - registros_validados e contrato XLSForm/SISMONITORA

- Mantém `registros_corrig.csv` como versão canônica corrigida e auditável.
- Adiciona `registros_validados.csv` opcional como versão pública compatível com o contrato do XLSForm vigente e a estrutura de exportação do SISMONITORA.
- Acrescenta `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` como variável pública, desligada por padrão.
- Consolida auditorias de schema, formatos, domínios XLSForm, condicionais, chaves, UUIDs e sanitizações antes da exportação validada.
- Consolida sanitização de outras formas de vida históricas, fotos e descritores correlatos e dependentes de forma de vida desconhecida.
- Corrige resumos por unidade vazios para evitar arquivos residuais em `output/`.

## [v2.4.0] - 2026-06-21

### Adicionado
- `MONITORA_MODO_EXECUCAO` com modos `completo`, `sem_png`, `estatisticas_sem_graficos`, `ate_registros_corrig` e `painel_e_parar`.
- Operações semânticas atômicas para exclusão de COLETAS, movimento de formas de vida, substituição de desconhecida, limpeza de outras formas e movimento em lote de formas de vida.
- Movimento em lote de formas de vida por COLETAS com migração segura e relatório de ambiguidades.
- Notificações de início/conclusão e trava contra duplo clique no painel.
- Auditoria de persistência pós-aplicação e pós-exportação.
- Sincronização final de `Encostam`/`tipo_forma_vida` a partir dos campos inferiores finais.

### Alterado
- Fila do painel passa a exibir operações semânticas coerentes com as ações do usuário.
- Relatórios de comparação pré/pós-correções normalizam tipos auxiliares antes de `rbindlist()`.
- Comentários internos do script foram revisados para remover marcas de desenvolvimento, referências transitórias e justificativas interlocutórias.

### Corrigido
- Persistência de operações atômicas em `registros_corrig.csv`.
- Divergências reais de `Encostam` após correções sobrepostas.
- Falhas de `rbindlist()` por classes divergentes em relatórios auxiliares.
- Continuação silenciosa quando o painel encerra sem ação explícita.


## [v2.3.2] - 2026-06-19

### Destaques

- Publica a versão `v2.3.2` após a `v2.3.1`.
- Corrige a exclusão de COLETAS filtradas/selecionadas no Painel de validação - correções assistidas de `registros_corrig`.
- Garante que a exclusão de COLETAS seja aplicada como remoção integral de linhas, sem deixar vestígios no `registros_corrig.csv`.
- Melhora a visualização das tabelas do painel ao ocultar o campo `uuid` apenas na interface.

### Alterado

- `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES` permanece com padrão inicial `"N"`.
- O `uuid`/amostragem-registro deixa de ser exibido nas tabelas do painel, mas permanece preservado nos dados, operações, logs, auditorias e arquivos finais.
- Comentários do script público foram revisados para remover menções residuais a versões anteriores como referência corrente, incluindo a menção indevida à `v2.3.0` observada no script público anterior.

### Corrigido

- O botão de exclusão de COLETAS em lote deixa de ser bloqueado pela regra de coluna estrutural/protegida, pois a operação não edita `COLETA`; ela remove integralmente as linhas das COLETAS alvo.
- Auditoria pós-exclusão passa a bloquear a execução caso alguma linha de COLETA excluída permaneça na base corrigida.

# Changelog

## v2.3.1 - 2026-06-19

### Painel de correções assistidas

- Implementa limpeza atômica auditável de outra forma de vida.
- Remove tokens históricos de outra(s) forma(s) de vida em listas de forma de vida nativa, exótica e seca/morta.
- Limpa descritores históricos dependentes de outra forma de vida.
- Adiciona checkpoints de persistência pós-aplicação e pré-exportação.
- Simplifica o escopo do painel para coleta individual ou coletas do lote.
- Define COLETAS do lote como fonte explícita para operações em lote.
- Adiciona botão Limpar filtros.
- Usa labels do XLSForm mais recente para formas de vida, mantendo names históricos vinculados internamente.
- Mantém hábito restrito às formas condicionais previstas no XLSForm: bromelioide, cactacea, orquidea e samambaia.
- Preserva o painel desativado por padrão.

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.3.0] - 2026-06-18

### Destaques

- Publica a versão `v2.3.0` após a `v2.2.2`.
- Amplia o Painel de validação - correções assistidas de `registros_corrig`.
- Introduz correção em lote por múltiplas COLETAS com filtros superiores hierárquicos.
- Introduz exclusão auditável de COLETAS filtradas ou selecionadas.
- Altera a deduplicação para preservar COLETAS distintas com mesma UC + UA + ANO para curadoria manual.
- Adiciona trava pós-correções para impedir análise estatística quando restarem conflitos de UAs duplicadas no mesmo ano.
- Inclui `COLETA` em `registros_corrig_stat`, antes de `UC`.

### Adicionado

- Filtros superiores hierárquicos e multisseleção no painel: UC(s), EA(s), ano(s), ciclo(s), campanha(s), UA(s) e COLETAS.
- Checkbox para usar todas as COLETAS resultantes dos filtros superiores.
- Prévia auditável por COLETA antes de gerar operações em lote.
- Operações de correção em lote com uma operação por COLETA por atributo.
- Botão para adicionar exclusão auditável de COLETAS filtradas/selecionadas.
- Filtro de triagem para UAs duplicadas no mesmo ano.
- Auditorias pré e pós-correções de COLETAS com UAs duplicadas por ano.
- Cache de localização de linhas por COLETA, coleta_uuid, uuid_registro e linha_indice.
- Coluna `COLETA` em `registros_corrig_stat`, posicionada antes de `UC`.

### Alterado

- A deduplicação automática foi ajustada para manter registros genuinamente idênticos, mas preservar COLETAS distintas envolvidas em conflitos UC + UA + ANO.
- Casos de múltiplas COLETAS para a mesma UC + UA + ANO deixam de ser resolvidos automaticamente e passam a ser encaminhados para triagem/correção no painel.
- A etapa pós-correções passa a bloquear a continuidade da execução quando conflitos de UAs duplicadas no mesmo ano permanecem não resolvidos.
- O README foi atualizado para refletir os novos fluxos de curadoria, mantendo as seções públicas anteriores, incluindo uso auxiliar de IA.
- Os links do README foram atualizados para as cinco cópias públicas da `v2.3.0`.

### Corrigido

- Redução de risco operacional em edições de atributos uniformes por COLETA.
- Evita que conflitos reais de campo sejam mascarados por deduplicação automática.
- Reforça a rastreabilidade entre `registros_corrig.csv` e `registros_corrig_stat.csv` por meio do atributo `COLETA`.

### Validação recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"`.
- Testar correção em lote por múltiplas COLETAS.
- Testar seleção automática de COLETAS pelos filtros superiores.
- Testar exclusão auditável de COLETAS.
- Testar caso com múltiplas COLETAS na mesma UC + UA + ANO e confirmar interrupção pós-correções quando o conflito não for resolvido.
- Conferir `output/registros_corrig.csv`, `output/registros_corrig_stat.csv` e auditorias de COLETAS com UAs duplicadas.
- Conferir que as cinco cópias públicas do script têm SHA256 idêntico.



## [v2.2.2] - 2026-06-18

### Destaques

- Publica a versão `v2.2.2` após a `v2.2.1`.
- Estabiliza o Painel de validação - correções assistidas de `registros_corrig`.
- Mantém o relatório de ocorrência de formas de vida exóticas, o painel assistido e o fluxo analítico, estatístico e gráfico consolidado.

### Adicionado

- Pré-validação transacional de grupos de correção.
- Auditoria semântica pré/pós-correção.
- Tabela unificada de triagem do painel.
- Localização acelerada por `linha_indice`.
- Deduplicação defensiva por assinatura semântica.

### Alterado

- Triagem de exóticas passa a exigir vínculo operacional estrito entre `Encostam`, forma de vida e espécie.
- Hábito passa a ser aceito apenas para formas condicionais.
- Mapa canônico estrutural de colunas passa a ser cacheado.
- Comentários do script foram revisados para remover menções a versões internas e comentários interlocutórios.

### Corrigido

- Bloqueio de correções parciais em movimentos assistidos.
- Redução de reintrodução de tokens residuais após movimentos exótica → nativa.
- Tratamento mais seguro de CSVs vazios, warnings de exportação e objetos temporários.
- Melhoria de checkpoints, progresso textual e controle de recursos.

## [v2.2.1] - 2026-06-16

### Adicionado
- Relatório de ocorrência de formas de vida exóticas em output/.
- Produtos CSV auxiliares com registros de formas de vida exóticas com e sem espécie vinculada.
- Resumos por unidade, forma de vida e campo de espécie exótica.

### Corrigido
- Triagem do relatório restrita a registros com token exotica em **Encostam** na vareta.
- Correção da contagem de espécies exóticas vinculadas.
- Exclusão de campos auxiliares indevidos, como .id, da detecção de espécies.
- Tratamento de NA, campos vazios e --- como ausência nos produtos exportados.
- Reconhecimento de campos textuais abertos do tipo Outra espécie ... exótica como espécie vinculada quando associados à forma de vida exótica correspondente.
# Changelog

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.2.0] - 2026-06-16

### Destaques

- Consolida a evolução posterior à última versão pública `v2.1.3`.
- Introduz o **Painel de validação - correções assistidas de `registros_corrig`**.
- Mantém o fluxo analítico principal da série `v2.1.x`, incluindo estatísticas temporais, gráficos publicáveis seriados, painéis amostrais por ano inicial e relatório textual estatístico.

### Adicionado

- Painel Shiny opcional para validação e correção assistida de registros consolidados.
- Variável explícita no início do script para abertura do painel:
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` abre o painel;
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` executa sem painel.
- CSV longo de correções assistidas em `input/correcoes_campos.csv`.
- Auditoria de correções em `log/auditoria_correcoes_campos_*.csv` e cópia da última execução em `output/correcoes_campos/`.
- Metadados embutidos dos XLSForms 2022, 2023, 2024 e 2025 para regras de validação do painel, sem dependência de leitura de XLSForms externos.
- Dicionários auxiliares e diagnóstico adaptável aos atributos reais presentes em `registros_corrig`.
- Triagem de formas de vida exóticas nos registros selecionados, com exibição de coleta, ponto amostral, ponto metro, forma exótica e UUID do registro.
- Movimento assistido de forma de vida entre `exotica`, `nativa` e `seca_morta`.
- Controle de campos condicionais para formas como `samambaia`, `orquidea`, `cactacea` e `bromelioide`, incluindo hábito `terrestre`, `epifita` ou `rupicola` quando aplicável.
- Harmonização auditável de campos superiores e inferiores vinculados pelo XLSForm.

### Alterado

- O painel de validação é opcional e não altera a execução analítica padrão quando desativado.
- O valor padrão da opção do painel foi mantido como `"N"` para preservar a execução normal em produção.
- A aplicação de correções passou a usar resolução defensiva de nomes de colunas, considerando variações de labels, HTML, aspas escapadas, acentos e pontuação.
- O recálculo de `**Encostam** na vareta: (amostragem/registro)` passou a ser feito a partir das categorias de forma de vida após as correções assistidas.
- A documentação pública foi atualizada para incluir o painel, mantendo as seções institucionais, técnicas, de versionamento e de uso auxiliar de IA.

### Corrigido

- Remoção de riscos associados à edição direta de `registros_corrig.csv` em planilhas, por meio de correções guiadas e auditadas.
- Tratamento de movimentos exótica → nativa para limpar/atualizar campos inferiores e superiores vinculados.
- Compatibilidade entre campos do XLSForm e colunas consolidadas com labels ou HTML representados de formas distintas.
- Proteção contra correções malformadas geradas por seleção de linhas pré-triadas.

### Validação recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` para validar o fluxo analítico padrão.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` para validar o painel.
- Testar ao menos:
  - uma correção simples/lote por coleta;
  - um movimento assistido exótica → nativa;
  - conferência de `output/registros_corrig.csv`;
  - conferência de `output/correcoes_campos/auditoria_correcoes_campos_ultima_execucao.csv`.
- Conferir que os cinco scripts públicos têm o mesmo SHA256.

## [v2.1.3] - 2026-06-12

### Destaques

- Última versão pública da série `v2.1.x` antes da publicação da `v2.2.0`.
- Mantém os produtos editoriais, estatísticos e gráficos consolidados nas versões `v2.1.0` a `v2.1.2`.

### Corrigido

- Tratamento defensivo para exportação de gráficos em subconjuntos vazios.
- Ajustes em parâmetros de rótulos e exportação para reduzir falhas em cenários de dados filtrados ou incompletos.
- Compatibilidade com arquivos manipulados em editores de planilha, preservando as correções já consolidadas na série `v2.1.x`.

## [v2.1.2] - 2026-06-12

### Alterado

- Refatoração nominal interna e revisão editorial da documentação.
- Padronização de nomes internos de funções utilitárias, objetos globais, rotinas de recursos, auditoria, gráficos, layout, estatística e relatório textual.
- Consolidação da nomenclatura pública baseada em **painéis amostrais por ano inicial**.
- Atualização editorial do `README.md`, com recuperação das informações sobre uso auxiliar de IA.

### Validação

- Preservação dos produtos analíticos principais em relação ao baseline de refatoração.
- Scripts principais sincronizados.

## [v2.1.1] - 2026-06-12

### Destaques

- Revisão editorial dos produtos públicos após a publicação da `v2.1.0`.
- Atualização da terminologia pública de “coortes” para **painéis amostrais por ano inicial**.
- Padronização da nomenclatura pública dos gráficos com serial global `fig_001_...png` a `fig_156_...png`.
- Ampliação do relatório textual estatístico.

### Adicionado

- Índice mestre de gráficos `output/indice_graficos.csv`.
- Gráficos publicáveis seriados em `output/plots_png/`.
- Tabelas estatísticas dos painéis amostrais por ano inicial.

### Corrigido

- Classificação pública da `fig_036`.
- Ausência da exportação de composição geral contra linha de base dos painéis amostrais por ano inicial.
- Defasagem conceitual do relatório textual estatístico.

## [v2.1.0] - 2026-06-11

### Destaques

- Consolida a evolução desde a última versão pública `v2.0.2`.
- Inclui novos produtos analíticos, gráficos editoriais, análises longitudinais por ano inicial e auditorias.

### Adicionado

- Gráficos temporais editoriais com escopo amostral explícito.
- Painéis editoriais para amostra total por ano, UAs presentes em todos os anos avaliados e comparações pareadas por período consecutivo.
- Estatística pareada específica para gráficos editoriais período a período.
- Relatório textual estatístico em `output/relatorio_textual_estatistico.txt`.
- Auditorias de layout de rótulos, símbolos estatísticos, esforço amostral, performance e memória.

### Corrigido

- Rótulos de ano e `n UA` embaralhados em facetas.
- Linhas ou rótulos duplicados em painéis temporais.
- Sobreposição de símbolos estatísticos e rótulos.
- Legendas inferiores que ultrapassavam os limites dos painéis.
- Compatibilidade entre fontes com sobreposição de exportações.

## [v2.0.2] - 2026-06-10

### Destaques

- Última versão pública antes da consolidação `v2.1.0`.
- Continha o núcleo de tratamento, padronização, deduplicação, estatística, auditoria e relatório textual.

### Alterado

- Ajustes de consistência entre cópias públicas do script.
- Ajustes editoriais e de documentação da série `v2.0.x`.

## [v2.0.1] - 2026-06-10

### Alterado

- Ajustes incrementais de publicação.
- Organização de arquivos.
- Consistência entre cópias do script.
- Preparação da linha pública para revisão documental e tagueamento estável.

## [v2.0.0] - 2026-06-10

### Destaques

- Primeira versão pública com adoção de versionamento semântico.
- Consolidação estatística, auditoria e relatório textual.
- Organização pública do repositório com cópia congelada em `releases/v2.0.0/`.
- Registro explícito do uso auxiliar de IA generativa a partir da fase de consolidação pública.

### Adicionado

- Importação de múltiplos tipos de entrada.
- Extração recursiva de ZIPs do SISMONITORA.
- Auditoria de arquivos candidatos à importação.
- Deduplicação semântica de registros equivalentes.
- Verificação de integridade dos dados.
- Tratamento defensivo de colunas, datas, coordenadas e aliases.
- Controle de performance, memória e recursos computacionais.
- Análise estatística inferencial pareada por unidade amostral.
- Comparações ano a ano e contra linha de base acumulada.
- Teste de permutação pareado.
- Intervalo de confiança por bootstrap.
- Correção de múltiplas comparações por FDR.
- Análise de mudança na composição geral com distância de Bray-Curtis.
