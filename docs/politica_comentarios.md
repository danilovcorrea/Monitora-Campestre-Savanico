# Política editorial para comentários dos scripts R

Esta política orienta a revisão dos comentários dos scripts do repositório Monitora-Campestre-Savanico.

## Objetivo

Os comentários devem auxiliar usuários, revisores e desenvolvimento futuro. Devem explicar finalidade, critérios, objetos relevantes, decisões metodológicas, validações, entradas, saídas e pontos de atenção operacional.

## Regras editoriais

1. Redigir comentários em português do Brasil, mantendo nomes próprios, nomes de pacotes, funções, arquivos, objetos e termos técnicos quando necessário.
2. Remover comentários que apenas comparem versões, como “corrigido na vX”, “ajuste da vY”, “teste anterior” ou “versão antiga”.
3. Evitar comentários redundantes que apenas repitam literalmente o código.
4. Manter comentários úteis sobre decisões metodológicas, critérios estatísticos, auditoria, deduplicação, padronização, gráficos, exportações e controle de performance.
5. Preservar nomes de objetos, nomes de colunas, pacotes e funções exatamente como usados no código.
6. Não alterar código ativo durante a revisão de comentários.
7. Quando houver dúvida, manter o comentário e registrá-lo no relatório de revisão para avaliação manual.

## Cabeçalho recomendado para o script atual

```r
### Programa Monitora - Componente Campestre Savânico
### Script: Alvo Global - tratamento, análise e visualização de dados
### Versão pública: v2.0.0
### Data da versão: AAAA-MM-DD
### Status: versão pública estável
```

## Cabeçalho recomendado para scripts históricos

```r
### Programa Monitora - Componente Campestre Savânico
### Script histórico preservado para rastreabilidade
### Versão pública: pré-versionamento semântico
### Status: histórico / não recomendado para novas análises
```
