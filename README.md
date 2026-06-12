# Monitora Campestre-Savânico — Alvo Global

Scripts de tratamento, auditoria, deduplicação, análise estatística e geração de produtos gráficos para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do **Programa Monitora**.

Versão pública atual: **v2.1.0** — gráficos editoriais, coortes e consolidação de layout.

A partir da série pública `v2.0.x`, o repositório adota versionamento público semântico.

Consulte também:

- [`VERSION`](VERSION)
- [`CHANGELOG.md`](CHANGELOG.md)
- [`docs/versionamento.md`](docs/versionamento.md)
- [`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Script recomendado para uso

O script atual recomendado está disponível diretamente na raiz do repositório:

- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

A mesma versão também é mantida nas demais cópias públicas do projeto para compatibilidade e rastreabilidade:

- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)
- [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R)
- [`monitora_campsav_alvo_global_v2.1.0.R`](monitora_campsav_alvo_global_v2.1.0.R)
- [`releases/v2.1.0/monitora_campsav_alvo_global_v2.1.0.R`](releases/v2.1.0/monitora_campsav_alvo_global_v2.1.0.R)

Na publicação da `v2.1.0`, essas cinco cópias públicas do script foram mantidas com conteúdo idêntico e hash SHA256 comum registrado em [`SHA256SUMS.txt`](SHA256SUMS.txt).

## Finalidade

O script consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis. O fluxo foi desenhado para aceitar tanto dados brutos quanto arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais.

## Entradas aceitas

Coloque os arquivos no subdiretório `input/` ou no mesmo diretório do script. O uso de `input/` é recomendado para evitar confusão com produtos antigos.

Entradas reconhecidas:

- arquivos ZIP exportados individualmente pelo SISMONITORA;
- arquivos ZIP, CSV ou XLSX de exportação em lote;
- arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados deliberadamente como nova entrada.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Saídas principais

Na raiz do projeto:

- `registros_corrig.csv`: registros padronizados, deduplicados e auditáveis;
- `registros_corrig_stat.csv`: base estatística por UC, UA, ano e métricas derivadas.

Em `output/`:

- tabelas de proporção relativa e cobertura vegetal;
- tabelas estatísticas de mudança ano a ano, linha de base e composição geral;
- relatório textual estatístico (`relatorio_textual_estatistico.txt`);
- gráficos publicáveis em `output/plots_png/`;
- índice de gráficos e auditorias de símbolos, esforço amostral e layout de rótulos;
- arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

- relatório de execução;
- auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
- relatórios de performance, memória e controle de recursos.

## Principais recursos da v2.1.0

- Consolidação de gráficos temporais editoriais com escopo amostral explícito.
- Gráficos para amostra total, UAs presentes em todos os anos e comparações pareadas por período.
- Análise de coortes por ano inicial.
- Relatório textual estatístico com síntese dos principais achados por UC, formação vegetacional, ano, linha de base e categoria.
- Auditoria ampliada de entrada, deduplicação, completude, coordenadas, performance, memória, símbolos estatísticos e layout de rótulos.
- Motor editorial de rótulos, conectores, símbolos estatísticos, margens e legendas inferiores dos gráficos.
- Aceitação defensiva de `registros_corrig*.csv` como entrada válida, sem reprocessar automaticamente produtos de `output/` e `log/`.

## Recursos consolidados desde a v2.0.0

A série pública `v2.0.x` consolidou o núcleo de tratamento, auditoria e análise estatística. A versão `v2.1.0` mantém esses recursos e amplia a camada editorial e temporal dos produtos.

Funcionalidades preservadas e ampliadas:

- importação de múltiplos tipos de entrada: ZIPs do SISMONITORA, CSV/XLSX em lote e arquivos pós-tratamento;
- auditoria de arquivos candidatos à importação;
- deduplicação semântica de registros equivalentes;
- verificação de integridade dos dados;
- tratamento defensivo de colunas, datas, coordenadas e aliases;
- controle de performance, memória e recursos computacionais;
- análise estatística inferencial pareada por unidade amostral;
- comparações ano a ano e contra linha de base acumulada;
- teste de permutação pareado;
- intervalo de confiança por bootstrap;
- correção de múltiplas comparações por FDR;
- análise de mudança na composição geral com distância de Bray-Curtis;
- geração de gráficos com rótulos, símbolos estatísticos e legendas explicativas;
- geração de relatório textual estatístico e ecológico.

## Estrutura do repositório

- `MONITORA_CAMPSAV_Alvo_Global.R`: script atual recomendado para uso.
- `monitora_campsav_alvo_global.R`: cópia pública equivalente do script atual.
- `R/monitora_campsav_alvo_global.R`: cópia do script atual na estrutura interna do projeto.
- `monitora_campsav_alvo_global_v2.1.0.R`: cópia versionada da versão pública atual.
- `releases/v2.1.0/`: cópia congelada da versão pública `v2.1.0`.
- `releases/v2.0.0/`, `releases/v2.0.1/`, `releases/v2.0.2/`: versões públicas anteriores preservadas por rastreabilidade.
- `archive/versoes_historicas/`: versões históricas anteriores ao versionamento público semântico.
- `docs/`: documentação auxiliar, incluindo versionamento e uso de IA.
- `tools/`: ferramentas auxiliares de auditoria e revisão, quando presentes.
- `.github/workflows/`: automações do GitHub Actions, quando presentes.

## Scripts históricos

Os scripts `.R` datados ou anteriores à adoção do versionamento público semântico foram movidos para:

- [`archive/versoes_historicas/`](archive/versoes_historicas/)

Esses arquivos representam versões históricas preservadas por rastreabilidade. Na revisão associada à publicação da `v2.0.0`, os comentários dos scripts históricos foram revisados editorialmente e padronizados majoritariamente em português, sem alteração do código ativo.

A versão recomendada para uso atual é:

- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

## Uso auxiliar de IA generativa

Este projeto teve uma fase inicial de desenvolvimento baseada em edição manual, consulta a documentação técnica, exemplos de código e fontes abertas de referência, incluindo fóruns técnicos e materiais de apoio.

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração e organização do versionamento público. O uso de IA teve caráter auxiliar.

As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor.

Versões anteriores à adoção do versionamento semântico foram preservadas por rastreabilidade histórica e podem refletir diferentes fases de desenvolvimento, inclusive fases sem evidência clara de apoio direto de IA.

Mais detalhes em:

- [`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Backup pré-revisão

O estado do repositório antes da revisão editorial, adoção do versionamento semântico e publicação da `v2.0.0` foi preservado em:

- branch: `backup/pre-revisao-editorial-20260610`
- tag: `pre-revisao-editorial-20260610`

## Requisitos de R

Pacotes usados pelo script:

- `rstudioapi`
- `dplyr`
- `data.table`
- `purrr`
- `stringr`
- `tidyverse`
- `ggplot2`
- `ggrepel`
- `readxl`
- `openxlsx`
- `sf`

O script tenta instalar pacotes ausentes durante a execução. Em ambientes institucionais ou sem permissão de instalação, instale os pacotes previamente.

## Uso básico

1. Clone ou baixe este repositório.
2. Coloque o script no diretório de trabalho do projeto.
3. Coloque as entradas em `input/`.
4. Execute o script completo no RStudio ou por `Rscript`.
5. Consulte os produtos em `output/` e as auditorias em `log/`.

Exemplo por terminal:

```bash
Rscript monitora_campsav_alvo_global.R
```

## Parâmetros por variáveis de ambiente

Alguns parâmetros podem ser definidos antes da execução:

| Variável | Valores esperados | Finalidade |
|---|---|---|
| `MONITORA_PERFIL_EXECUCAO` | `auto`, `rapido`, `economico` | Ajusta estratégia de performance e uso de memória. |
| `MONITORA_BATCH_SIZE_CSV` | inteiro | Define tamanho de lote para leitura/processamento de CSVs. |
| `MONITORA_DT_THREADS` | inteiro | Define número de threads do `data.table`. |
| `MONITORA_GC_MODO` | `auto`, `agressivo`, `desligado` | Controla chamadas de coleta de lixo. |
| `MONITORA_AUDITORIA_COORDENADAS_COMPLETA` | `true`/`false` | Habilita auditoria completa de coordenadas. |
| `MONITORA_EXPORTAR_GRAFICOS` | `true`/`false` | Habilita exportação de gráficos. |
| `MONITORA_EXPORTAR_KML` | `true`/`false` | Habilita exportação de KML. |
| `MONITORA_STAT_BOOT` | inteiro | Número de reamostragens bootstrap. |
| `MONITORA_STAT_PERM` | inteiro | Número de permutações estatísticas. |
| `MONITORA_GRAFICOS_PAREADOS_TODOS_PARES` | `true`/`false` | Controla geração ampliada de pares temporais nos gráficos editoriais. |

Exemplo no Linux/macOS:

```bash
MONITORA_PERFIL_EXECUCAO=economico MONITORA_EXPORTAR_KML=false Rscript monitora_campsav_alvo_global.R
```

## Interpretação dos gráficos

Os gráficos publicáveis foram desenhados para explicitar:

- formação vegetacional;
- ano;
- esforço amostral (`n UA`);
- escopo amostral da comparação;
- incerteza por IC95%, quando aplicável;
- símbolos estatísticos de mudança por categoria ou composição geral;
- legenda metodológica inferior, quando necessária.

Em gráficos com muitas categorias, a versão sem rótulos pode ser a principal para publicação, enquanto a versão com rótulos serve como apoio de diagnóstico e validação.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.1.0"
TAG="v${VERSAO}"

sha256sum \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

grep -Rni "Versão pública" \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

cat VERSION
```

Critério de liberação: os cinco scripts devem ter o mesmo hash, `VERSION` deve conter a versão pública atual e os gráficos de validação devem ter sido conferidos visualmente.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

- [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico
