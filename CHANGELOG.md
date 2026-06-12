# Changelog

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## v2.1.3 - Correção de rótulos obrigatórios em subconjuntos vazios

- Corrige erro em gráficos obrigatórios quando uma formação vegetacional está ausente no conjunto de dados analisado.
- Garante que `prop_num_rotulo_obrig` e demais colunas técnicas de rótulo sejam criadas mesmo em subconjuntos vazios.
- Substitui `label.size` por `linewidth` em `geom_label()`, removendo aviso de depreciação do ggplot2 3.5.0.
- Não altera cálculos estatísticos, CSVs analíticos, regras de permutação, bootstrap, FDR ou deduplicação.
## v2.1.2 - Refatoração nominal interna e revisão editorial da documentação

### Alterado
- Padronização de nomes internos de funções utilitárias, objetos globais, rotinas de recursos, auditoria, gráficos, layout, estatística e relatório textual.
- Consolidação da nomenclatura pública baseada em **painéis amostrais por ano inicial**, evitando a reintrodução de terminologia ambígua em contexto ecológico.
- Atualização editorial do `README.md`, com recuperação e revisão das informações sobre uso auxiliar de IA presentes em READMEs das versões públicas desde `v2.0.0`.
- Preservação dos nomes públicos de arquivos, colunas exportadas, variáveis de ambiente e produtos analíticos.

### Validação
- `output/registros_corrig.csv` preservado com hash idêntico ao baseline de refatoração.
- `output/registros_corrig_stat.csv` preservado com hash idêntico ao baseline de refatoração.
- Scripts principais sincronizados e validados por parse.
- Refatoração nominal sem alteração esperada nos produtos analíticos principais.

## [v2.1.1] - 2026-06-12

### Destaques

- Revisão editorial dos produtos públicos após a publicação da v2.1.0.
- Atualização da terminologia pública de “coortes” para **painéis amostrais por ano inicial**.
- Explicitação de que os painéis amostrais por ano inicial não representam coortes ecológicas de indivíduos.
- Padronização da nomenclatura pública dos gráficos com serial global `fig_001_...png` a `fig_156_...png`.
- Ampliação do relatório textual estatístico para refletir todos os principais produtos estatísticos e editoriais da versão atual.

### Adicionado

- Índice mestre de gráficos:
  - `output/indice_graficos.csv`.

- Gráficos publicáveis seriados em:
  - `output/plots_png/`.

- Saídas estatísticas dos painéis amostrais por ano inicial:
  - `estatisticas_mudanca_ano_a_ano_paineis_ano_inicial.csv`;
  - `estatisticas_mudanca_linha_base_paineis_ano_inicial.csv`;
  - `estatisticas_composicao_geral_ano_a_ano_paineis_ano_inicial.csv`;
  - `estatisticas_composicao_linha_base_paineis_ano_inicial.csv`.

- Seções no relatório textual estatístico para:
  - amostra total;
  - série pareada total;
  - comparações pareadas por período editorial;
  - painéis amostrais por ano inicial;
  - mudanças por categoria;
  - composição geral;
  - comparações contra medição anterior;
  - comparações contra linha de base acumulada.

### Alterado

- A geração do relatório textual estatístico passou a ocorrer ao final do fluxo, depois da geração dos produtos editoriais e dos painéis amostrais por ano inicial.
- A nomenclatura pública dos gráficos foi substituída por uma taxonomia seriada baseada em:
  - serial;
  - bloco analítico;
  - escopo amostral;
  - métrica;
  - tema;
  - formação vegetacional;
  - presença ou ausência de rótulos.
- Os nomes públicos dos produtos deixaram de usar “coorte” e passaram a usar “painel” ou `paineis_ano_inicial`.
- Os índices fragmentados anteriores foram substituídos por um índice mestre único.
- O README foi atualizado para a v2.1.1, preservando seções institucionais importantes da v2.0.0, incluindo:
  - script recomendado para uso;
  - estrutura do repositório;
  - scripts históricos;
  - uso auxiliar de IA generativa;
  - backup pré-revisão;
  - licença.

### Corrigido

- Classificação pública da `fig_036`, que passou de `nao_classificada` para `cobertura`.
- Ausência da exportação de composição geral contra linha de base dos painéis amostrais por ano inicial.
- Defasagem conceitual do relatório textual estatístico em relação aos produtos gráficos e estatísticos da v2.1.0.
- Ambiguidade terminológica causada pelo uso público de “coorte” em contexto ecológico.

### Observações de validação

- A versão v2.1.1 foi executada localmente sem erros.
- Foram gerados 156 gráficos PNG seriados em `output/plots_png/`.
- Os cinco scripts principais da versão foram conferidos com o mesmo hash SHA256.
- A versão foi publicada no commit `0b5ffde` e marcada com a tag `v2.1.1`.

## [v2.1.0] - 2026-06-11

### Destaques

- Consolida a evolução desde a última versão pública v2.0.2.
- Promove a linha interna validada para uma versão pública menor, em razão da inclusão de novos produtos analíticos, gráficos editoriais, análises longitudinais por ano inicial e auditorias.
- Mantém compatibilidade com entradas brutas do SISMONITORA, exportações em lote e arquivos `registros_corrig*.csv` produzidos por execuções anteriores.

### Adicionado

- Gráficos temporais editoriais com escopo amostral explícito.
- Painéis editoriais para:
  - amostra total por ano;
  - UAs presentes em todos os anos avaliados;
  - comparações pareadas por período consecutivo.
- Estatística pareada específica para os gráficos editoriais período a período.
- Análise longitudinal por ano inicial, posteriormente revisada na v2.1.1 para a nomenclatura pública “painéis amostrais por ano inicial”.
- Relatório textual estatístico em `output/relatorio_textual_estatistico.txt`, com síntese dos principais achados e descrições por UC, formação, ano, linha de base e categoria.
- Auditoria de layout de rótulos em `output/auditoria_layout_rotulos.csv`.
- Auditoria de símbolos estatísticos dos gráficos.
- Auditoria de esforço amostral temporal.
- Controle adaptativo de recursos, com registros de performance, memória, threads e tamanho de lote.
- Tratamento explícito para arquivos `registros_corrig*.csv` usados como nova entrada.

### Alterado

- Reestruturação dos rótulos de ano e esforço amostral (`n UA`) nos gráficos editoriais.
- Separação entre chaves internas de ordenação/posicionamento e rótulos editoriais exibidos.
- Refinamento das margens, conectores, faixas reservadas e limites de eixo X para gráficos com rótulos.
- Revisão das regras editoriais para gráficos densos, com diferenciação entre versão principal sem rótulos e versão com rótulos como apoio de diagnóstico e validação.
- Incorporação dos símbolos estatísticos aos rótulos de linha ou categoria quando apropriado.
- Ajuste da legenda inferior metodológica para respeitar a largura útil dos painéis.
- Padronização pública dos nomes de auditorias e logs, removendo sufixos de desenvolvimento interno.
- Revisão dos comentários do script para uso público.

### Corrigido

- Rótulos de ano e `n UA` embaralhados em facetas quando o mesmo ano aparecia com esforços amostrais diferentes.
- Linhas ou rótulos duplicados em painéis temporais.
- Sobreposição de símbolos estatísticos com rótulos de barras.
- Conectores desnecessários ou visualmente desalinhados em gráficos densos.
- Corte ou extravasamento de rótulos nas margens de painéis facetados.
- Legendas inferiores que ultrapassavam os limites laterais dos painéis.
- Problemas de compatibilidade entre fontes quando havia sobreposição entre exportações individuais, exportações em lote e bases pós-tratamento.
- Risco de junções muitos-para-muitos indevidas por uso de rótulos editoriais compostos como chaves internas.

### Observações de validação

- Os plots foram conferidos visualmente após a correção das legendas inferiores.
- As legendas inferiores passaram a quebrar linha antes da margem dos painéis.
- Ainda há espaço para refinamento editorial futuro, mas o comportamento atual foi considerado aceitável para publicação.

## [v2.0.2] - 2026-06-10

### Destaques

- Última versão pública antes da consolidação v2.1.0.
- Continha o núcleo de tratamento, padronização, deduplicação, estatística, auditoria e relatório textual.
- Incorporou revisão editorial incremental em relação à v2.0.1.

### Alterado

- Ajustes de consistência entre cópias públicas do script.
- Ajustes editoriais e de documentação da série v2.0.x.

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
- Preservação de versões históricas anteriores ao versionamento semântico.
- Registro explícito do uso auxiliar de IA generativa a partir da fase de consolidação pública.

### Adicionado

- Importação de múltiplos tipos de entrada:
  - ZIPs do SISMONITORA;
  - CSV/XLSX em lote;
  - arquivos pós-tratamento.
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
- Geração de gráficos revisados com rótulos, símbolos estatísticos e legendas explicativas.
- Geração de relatório textual estatístico e ecológico.
- Documentação auxiliar sobre versionamento e uso de IA.

### Organização do repositório

- Definição de `MONITORA_CAMPSAV_Alvo_Global.R` como script recomendado.
- Inclusão de cópia interna em `R/monitora_campsav_alvo_global.R`.
- Inclusão de cópia congelada em `releases/v2.0.0/`.
- Preservação de versões históricas em `archive/versoes_historicas/`.
- Registro do backup pré-revisão:
  - branch `backup/pre-revisao-editorial-20260610`;
  - tag `pre-revisao-editorial-20260610`.

### Uso auxiliar de IA

- Registro de que, a partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio auxiliar de ferramentas de IA generativa.
- Explicitação de que decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica permanecem sob responsabilidade humana.

### Licença

- Publicação sob licença GPL-3.0.
