## v2.0.1 - Correções de integridade na consolidação de entradas mistas

Versão validada para produção.

Principais alterações:
- Corrige falha em `monitora_merge_duplicate_columns()` quando arquivos CSV chegam com nomes de colunas exatamente repetidos no cabeçalho.
- Adiciona trava final de deduplicação por `UC + CICLO + CAMPANHA + UA + ANO + ponto`, antes da geração de `registros_corrig_stat`.
- Mantém preferência por entradas pós-tratamento quando há sobreposição com arquivos brutos individuais.
- Gera auditoria da deduplicação final em `log/` e `output/`.
- Mantém auditorias de completude, datas ambíguas, fontes duplicadas e compatibilidade entre formatos de entrada.

Validação:
- Testada com `registros_corrig_PNCV_com2025.zip` combinado com `massa_teste.zip`.
- Execução concluída sem erro fatal.
- `registros_corrig.csv`: 82.314 linhas.
- `registros_corrig_stat.csv`: 815 linhas, uma por `UC + UA + ANO`.
- Nenhuma duplicata final por `UC + CICLO + CAMPANHA + UA + ANO + ponto`.
- Ressalva de dado real mantida: PNCV `UA-019_VgCS` em 2022 com 100 pontos, faltando o ponto 89.

# Changelog

Todas as mudanças relevantes deste projeto serão documentadas neste arquivo.

A partir de junho de 2026, o projeto passa a adotar versionamento público semântico.

## [v2.0.0] - 2026-06-10

### Adicionado

- Suporte a múltiplos tipos de entrada, incluindo ZIPs do SISMONITORA, CSV/XLSX em lote e arquivos pós-tratamento.
- Auditoria de arquivos candidatos à importação, incluindo tipo de entrada, duplicidades, compatibilidade e rastreabilidade.
- Deduplicação semântica entre registros equivalentes provenientes de múltiplas fontes.
- Verificações de integridade dos dados, incluindo pontos amostrais, anos plausíveis, colunas ausentes e completude.
- Controle de performance, memória e recursos computacionais.
- Estatística inferencial pareada por unidade amostral.
- Comparações ano a ano e contra linha de base acumulada.
- Teste de permutação pareado e intervalo de confiança por bootstrap.
- Correção de múltiplas comparações por FDR.
- Análise de mudança na composição geral com distância de Bray-Curtis.
- Geração de relatório textual estatístico e ecológico.
- Gráficos revisados com rótulos, símbolos estatísticos, legendas explicativas e exportação padronizada.
- Documentação sobre versionamento público semântico.
- Documentação sobre uso auxiliar de ferramentas de IA no desenvolvimento.

### Alterado

- Reestruturação geral do fluxo de execução do script.
- Organização de diretórios de entrada, extração, saída e logs.
- Revisão editorial dos comentários internos, com padronização majoritária em português.
- Padronização editorial de comentários para uso técnico, desenvolvimento e orientação de usuários.
- Melhoria do tratamento de datas, coordenadas, aliases de colunas e arquivos de entrada.
- Melhoria da rastreabilidade dos produtos gerados.

### Corrigido

- Tratamento de colunas duplicadas ou com sufixos artificiais.
- Risco de uso acidental de arquivos de saída como nova entrada.
- Inconsistências em rótulos e legendas de gráficos.
- Problemas de robustez em cenários com colunas ausentes, objetos vazios ou dados incompletos.

### Observações

- Esta é a primeira versão pública consolidada após a reorganização do projeto.
- Versões anteriores foram preservadas no histórico Git e no branch/tag de backup.
- Na fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio auxiliar de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração e organização do versionamento público.
- O uso de IA teve caráter auxiliar; a responsabilidade técnica, metodológica e interpretativa permanece com o autor.
