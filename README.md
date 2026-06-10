# Monitora-Campestre-Savanico

Scripts de tratamento, auditoria, análise estatística e visualização de dados do **Alvo Global do Componente Campestre Savânico** do Programa Monitora.

## Versão pública atual

**v2.0.1** — Consolidação estatística, auditoria e relatório textual.

A partir desta versão, o repositório passa a adotar versionamento público semântico.

Consulte:

- [`VERSION`](VERSION)
- [`CHANGELOG.md`](CHANGELOG.md)
- [`docs/versionamento.md`](docs/versionamento.md)

## Script recomendado para uso

O script atual recomendado está disponível diretamente na raiz do repositório:

[`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

A mesma versão também é mantida na estrutura interna do projeto:

[`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R)

Uma cópia congelada da versão pública `v2.0.1` está disponível em:

[`releases/v2.0.1/monitora_campsav_alvo_global_v2.0.1.R`](releases/v2.0.1/monitora_campsav_alvo_global_v2.0.1.R)

## Principais funcionalidades da v2.0.1

- Importação de múltiplos tipos de entrada: ZIPs do SISMONITORA, CSV/XLSX em lote e arquivos pós-tratamento.
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

## Estrutura do repositório

- `MONITORA_CAMPSAV_Alvo_Global.R`: script atual recomendado para uso.
- `R/monitora_campsav_alvo_global.R`: cópia do script atual na estrutura interna do projeto.
- `releases/v2.0.1/`: cópia congelada da versão pública `v2.0.1`.
- `archive/versoes_historicas/`: versões históricas anteriores ao versionamento semântico.
- `docs/`: documentação auxiliar.
- `tools/`: ferramentas auxiliares de auditoria e revisão.
- `.github/workflows/`: automações do GitHub Actions.

## Scripts históricos

Os scripts `.R` datados foram movidos para:

[`archive/versoes_historicas/`](archive/versoes_historicas/)

Esses arquivos representam versões históricas anteriores à adoção do versionamento público semântico. Foram preservados por rastreabilidade.

Na revisão associada à publicação da `v2.0.1`, os comentários dos scripts históricos foram revisados editorialmente e padronizados majoritariamente em português, sem alteração do código ativo.

A versão recomendada para uso atual é:

[`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

## Uso auxiliar de IA

Na fase de consolidação publicada como `v2.0.1`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração e organização do versionamento público.

O uso de IA teve caráter auxiliar. As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor.

Mais detalhes em:

[`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Backup pré-revisão

O estado do repositório antes da revisão editorial, adoção do versionamento semântico e publicação da `v2.0.1` foi preservado em:

- branch: `backup/pre-revisao-editorial-20260610`
- tag: `pre-revisao-editorial-20260610`

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

[`LICENSE`](LICENSE)
