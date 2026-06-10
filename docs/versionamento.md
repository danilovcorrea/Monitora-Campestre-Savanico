# Versionamento público do repositório

A partir da consolidação da versão pública `v2.0.0`, o repositório passa a adotar versionamento semântico para as versões públicas estáveis.

## Esquema adotado

O padrão é:

```text
vMAJOR.MINOR.PATCH
```

- `MAJOR`: mudanças estruturais, alterações relevantes de arquitetura, entradas, saídas, estatística ou compatibilidade.
- `MINOR`: novas funcionalidades compatíveis com a versão principal atual.
- `PATCH`: correções pontuais, documentação, comentários ou pequenos ajustes sem mudança metodológica.

## Histórico anterior

Antes da adoção do versionamento semântico, o desenvolvimento foi registrado por scripts datados, versões intermediárias e correções de desenvolvimento. Esses arquivos devem ser preservados para rastreabilidade, mas não recebem numeração pública retroativa individual.

## Arquivos recomendados

- `R/monitora_campsav_alvo_global.R`: script principal atual.
- `archive/`: scripts históricos preservados.
- `releases/vX.Y.Z/`: cópias congeladas de versões públicas.
- `CHANGELOG.md`: descrição das mudanças por versão pública.
- `VERSION`: versão pública atual.
