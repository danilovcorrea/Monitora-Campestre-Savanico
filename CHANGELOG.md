# Changelog

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.1.0] - 2026-06-11

### Destaques

- Consolida a evolução desde a última versão pública v2.0.2.
- Promove a linha interna validada para uma versão pública menor, em razão da inclusão de novos produtos analíticos, gráficos editoriais, análise de coortes e auditorias.
- Mantém compatibilidade com entradas brutas do SISMONITORA, exportações em lote e arquivos `registros_corrig*.csv` produzidos por execuções anteriores.

### Adicionado

- Gráficos temporais editoriais com escopo amostral explícito.
- Painéis editoriais para:
  - amostra total por ano;
  - UAs presentes em todos os anos avaliados;
  - comparações pareadas por período consecutivo.
- Estatística pareada específica para os gráficos editoriais período a período.
- Análise de coortes por ano inicial.
- Saídas estatísticas de coorte:
  - `estatisticas_mudanca_ano_a_ano_coortes_iniciais.csv`;
  - `estatisticas_mudanca_linha_base_coortes_iniciais.csv`;
  - `estatisticas_composicao_geral_ano_a_ano_coortes_iniciais.csv`.
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

### Produção anterior

- Última versão pública antes da consolidação v2.1.0.
- Continha o núcleo de tratamento, padronização, deduplicação, estatística, auditoria e relatório textual.

## [v2.0.1] - 2026-06-10

### Alterado

- Ajustes incrementais de publicação, organização de arquivos e consistência entre cópias do script.

## [v2.0.0] - 2026-06-10

### Adicionado

- Consolidação pública do fluxo de tratamento e análise do Alvo Global.
- Auditorias estruturadas de entrada, duplicidade, completude e rastreabilidade.
- Exportação de tabelas analíticas e produtos gráficos.
