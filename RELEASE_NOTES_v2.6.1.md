# v2.6.1 - Painel administrativo, registros validados e relatório de validação

## Destaques

- Painel de correções assistidas atualizado para exibir atributos administrativos e contratuais do template SISMONITORA/XLSForm 2025, incluindo ciclo, campanha, EA, UA, validado, validador, data de validação e observação de validação.
- Edição de atributos administrativos passa a usar domínio controlado a partir dos valores observados na base, com resolução de aliases como EA/ea e UA/ua.
- `registros_corrig.csv` passa a ser o produto mestre organizado, saneado e auditado para os 129 atributos do template 2025.
- `registros_validados.csv` passa a ser gerado somente por seleção e reordenação dos 129 atributos do `registros_corrig.csv`, quando não houver pendências impeditivas nesses atributos.
- Sincronização contratual de aliases estruturais, administrativos, data/hora, formação vegetacional, ponto, metro e interceptação antes da auditoria final.
- Relatório consolidado de validação prioriza HTML por padrão, evitando dependência obrigatória de LaTeX/PDF em ambientes Fedora/RStudio.
- Auditorias de bloqueio de `registros_validados.csv` passam a separar problemas reais de linhas informativas ou status OK.

## Validação operacional da versão

A versão foi validada localmente em execução com painel, correções assistidas, geração de `registros_corrig.csv`, `registros_validados.csv` com 129 colunas e relatório consolidado de validação em HTML/Markdown/JSON.

## Privacidade

Esta release contém somente código, documentação, checksums e pacote público. Produtos locais de execução, `input/`, `output/`, `log/`, `extracted/` e dados do SISMONITORA não devem ser publicados.
