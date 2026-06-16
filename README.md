# Monitora Campestre-Savânico — Alvo Global

Scripts de tratamento, auditoria, deduplicação, análise estatística, validação assistida e geração de produtos gráficos para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

## Versão pública atual

**v2.2.0**

A versão `v2.2.0` consolida a linha pública posterior à `v2.1.3` e acrescenta um **Painel de validação - correções assistidas de `registros_corrig`**, mantendo o fluxo analítico padrão do script. A execução analítica normal continua disponível sem abertura do painel; a validação assistida é acionada por uma variável explícita no início do script.

A terminologia pública vigente mantém **painéis amostrais por ano inicial** para os subconjuntos longitudinais de UAs definidos pelo primeiro ano de acompanhamento.

## Consulte também

- [`CHANGELOG.md`](CHANGELOG.md): histórico das versões públicas.
- [`VERSION`](VERSION): versão pública atual.
- [`LICENSE`](LICENSE): licença do projeto.
- [`docs/uso_de_ia.md`](docs/uso_de_ia.md): registro do uso auxiliar de IA, quando disponível.
- [`docs/versionamento.md`](docs/versionamento.md): notas de versionamento, quando disponível.
- [`releases/v2.2.0/`](releases/v2.2.0/): cópia congelada da versão pública atual.

## Script recomendado para uso

Use preferencialmente:

- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): cópia pública atual com nome padronizado em minúsculas.
- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R): nome público histórico equivalente.
- [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R): cópia mantida na estrutura interna do projeto.
- [`monitora_campsav_alvo_global_v2.2.0.R`](monitora_campsav_alvo_global_v2.2.0.R): cópia versionada da versão pública atual.
- [`releases/v2.2.0/monitora_campsav_alvo_global_v2.2.0.R`](releases/v2.2.0/monitora_campsav_alvo_global_v2.2.0.R): cópia congelada da versão pública `v2.2.0`.

Na publicação da `v2.2.0`, essas cinco cópias públicas do script são mantidas com conteúdo idêntico.
## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis. O script foi desenhado para aceitar tanto dados brutos quanto arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais.

A partir da `v2.2.0`, o script também incorpora uma etapa opcional de validação assistida de `registros_corrig`, voltada a correções controladas, auditáveis e menos sujeitas a problemas comuns de edição manual em planilhas.

## Entradas aceitas

Coloque os arquivos no subdiretório `input/` ou no mesmo diretório do script. O uso de `input/` é recomendado para evitar confusão com produtos antigos.

Entradas reconhecidas:

- arquivos ZIP exportados individualmente pelo SISMONITORA;
- arquivos ZIP, CSV ou XLSX de exportação em lote;
- arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados deliberadamente como nova entrada;
- arquivo `input/correcoes_campos.csv`, quando gerado pelo painel e usado para reaplicação auditável das correções.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Saídas principais

Na raiz do projeto:

- `registros_corrig.csv`: registros padronizados, deduplicados e auditáveis;
- `registros_corrig_stat.csv`: base estatística por UC, UA, ano e métricas derivadas.

Em `output/`:

- tabelas de proporção relativa e cobertura vegetal;
- tabelas estatísticas de mudança ano a ano, linha de base e composição geral;
- tabelas estatísticas dos painéis amostrais por ano inicial;
- relatório textual estatístico (`relatorio_textual_estatistico.txt`);
- índice mestre de gráficos (`indice_graficos.csv`);
- gráficos publicáveis seriados em `output/plots_png/`;
- auditorias de símbolos, esforço amostral, layout de rótulos e produtos gráficos;
- auditorias e dicionários auxiliares do painel em `output/correcoes_campos/`, quando a validação assistida for usada;
- arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

- relatório de execução;
- auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
- auditoria de correções assistidas (`auditoria_correcoes_campos_*.csv`), quando aplicável;
- relatórios de performance, memória e controle de recursos.

## Principais recursos da v2.2.0

- Inclusão do **Painel de validação - correções assistidas de `registros_corrig`**.
- Correções controladas em formato longo, com auditoria antes/depois.
- Edição assistida por escopo: coleta inteira, ponto específico ou registro por UUID.
- Fluxo facilitado para triagem de formas de vida exóticas e movimento assistido de exótica para nativa.
- Regras de campos condicionais baseadas em metadados embutidos dos XLSForms 2022, 2023, 2024 e 2025.
- Controle de dependências para formas como `samambaia`, `orquidea`, `cactacea` e `bromelioide`, incluindo hábito `terrestre`, `epifita` ou `rupicola` quando exigido.
- Harmonização de campos superiores e inferiores vinculados, com recálculo de `**Encostam** na vareta: (amostragem/registro)` a partir das categorias de forma de vida.
- Correção auditável de operações em lote por coleta, sem necessidade de edição direta do CSV consolidado em planilha.
- Preservação do fluxo estatístico e gráfico consolidado na série `v2.1.x`.

## Painel de validação - correções assistidas de `registros_corrig`

A abertura do painel é controlada por uma variável no início do script:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

Para abrir o painel, altere para:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

Com `"N"`, o script segue a execução analítica normal. Com `"S"`, o script consolida `registros_corrig`, abre o painel de validação e, após o fechamento do painel, aplica as correções salvas e continua o fluxo.

O painel foi desenhado para reduzir riscos de manipulação direta em Excel ou outros editores de planilha, como alteração de encoding, perda de zeros à esquerda, reinterpretação de datas, conversão indevida de valores decimais e edição acidental de múltiplas colunas.

## Arquivo de correções assistidas

O painel gera ou atualiza:

```text
input/correcoes_campos.csv
```

Esse arquivo registra a “receita” das alterações, em formato longo, e não substitui diretamente `registros_corrig.csv`. As correções são aplicadas pelo script com auditoria.

O arquivo final corrigido segue sendo:

```text
output/registros_corrig.csv
```

## Recursos consolidados desde a v2.0.0

Desde a primeira versão pública com versionamento semântico, o projeto consolidou:

- importação de múltiplos tipos de entrada;
- extração recursiva de ZIPs do SISMONITORA;
- conversão e leitura de CSV/XLSX;
- padronização de categorias e nomes;
- tratamento de arquivos pós-processados `registros_corrig*.csv`;
- deduplicação semântica;
- auditoria de entrada, completude, coordenadas e compatibilidade entre fontes;
- controle de performance, memória, threads e tamanho de lote;
- geração de bases corrigidas;
- cálculo de cobertura vegetal e proporção relativa;
- estatística inferencial pareada por UA;
- comparações ano a ano;
- comparações contra linha de base acumulada;
- análise de composição geral;
- geração de gráficos revisados;
- relatório textual estatístico;
- documentação pública e organização de versões congeladas em `releases/`.

## Estrutura do repositório

- `MONITORA_CAMPSAV_Alvo_Global.R`: script atual recomendado para uso.
- `monitora_campsav_alvo_global.R`: cópia pública equivalente do script atual.
- `R/monitora_campsav_alvo_global.R`: cópia do script atual na estrutura interna do projeto.
- `monitora_campsav_alvo_global_v2.2.0.R`: cópia versionada da versão pública atual.
- `releases/v2.2.0/`: cópia congelada da versão pública `v2.2.0`.
- `releases/v2.1.3/`, `releases/v2.1.2/`, `releases/v2.1.1/`, `releases/v2.1.0/`, `releases/v2.0.0/`, `releases/v2.0.1/`, `releases/v2.0.2/`: versões públicas anteriores preservadas por rastreabilidade, quando presentes no repositório.
- `archive/versoes_historicas/`: versões históricas anteriores ao versionamento público semântico.
- `docs/`: documentação auxiliar, incluindo versionamento e uso de IA, quando presente.
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

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração, organização do versionamento público, análise de logs, padronização de nomenclatura e proposição de testes. O uso de IA teve caráter auxiliar.

As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor. Ferramentas de IA não substituem a validação humana, a execução local do script, a inspeção dos produtos, a comparação de hashes, a auditoria dos logs e a responsabilidade técnica sobre a publicação.

Mais detalhes em:

- [`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Backup pré-revisão

O estado do repositório antes da revisão editorial, adoção do versionamento semântico e publicação da `v2.0.0` foi preservado em:

- branch: `backup/pre-revisao-editorial-20260610`;
- tag: `pre-revisao-editorial-20260610`.

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
- `shiny` e `DT`, apenas quando o painel de correções assistidas for ativado.

O script tenta instalar pacotes ausentes durante a execução. Em ambientes institucionais ou sem permissão de instalação, instale os pacotes previamente.

## Uso básico

1. Clone ou baixe este repositório.
2. Coloque os arquivos de entrada em `input/`.
3. Execute o script completo no RStudio ou por `Rscript`.
4. Consulte os produtos em `output/` e as auditorias em `log/`.

Exemplo por terminal:

```bash
Rscript MONITORA_CAMPSAV_Alvo_Global.R
```

Também é possível executar a versão pública específica:

```bash
Rscript monitora_campsav_alvo_global_v2.2.0.R
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
MONITORA_PERFIL_EXECUCAO=economico MONITORA_EXPORTAR_KML=false Rscript MONITORA_CAMPSAV_Alvo_Global.R
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

Os gráficos são exportados em `output/plots_png/` com nomenclatura pública seriada:

```text
fig_001_...png
fig_002_...png
...
fig_156_...png
```

O arquivo `output/indice_graficos.csv` deve ser usado para identificar rapidamente:

- serial;
- nome público do PNG;
- nome interno legado;
- bloco analítico;
- escopo amostral;
- ano inicial do painel, quando aplicável;
- métrica;
- tema;
- formação;
- presença ou ausência de rótulos;
- uso recomendado;
- arquivos de dados associados.

Em gráficos com muitas categorias, a versão sem rótulos pode ser a principal para publicação, enquanto a versão com rótulos serve como apoio de diagnóstico e validação.

## Painéis amostrais por ano inicial

A expressão **painéis amostrais por ano inicial** refere-se a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento. Esses painéis são subconjuntos amostrais de acompanhamento temporal.

Eles não representam grupos ecológicos de indivíduos acompanhados individualmente, mas sim conjuntos de unidades amostrais comparáveis dentro do desenho analítico adotado.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.2.0"
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

Critério de liberação:

- os cinco scripts devem ter o mesmo hash;
- `VERSION` deve conter a versão publicada;
- a execução sem painel deve ser validada;
- o painel deve ser testado em uma correção simples e em um movimento assistido exótica → nativa;
- os gráficos de validação devem ter sido conferidos visualmente;
- `output/indice_graficos.csv` deve estar consistente;
- `relatorio_textual_estatistico.txt` deve incorporar os produtos estatísticos atuais.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

- [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico


