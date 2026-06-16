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
