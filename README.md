# Monitora Campestre-Savânico — Alvo Global

Scripts de tratamento, auditoria, deduplicação, análise estatística e geração de produtos gráficos para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

## Versão pública atual

**v2.1.1**

A versão `v2.1.1` é uma revisão editorial e documental da série `v2.1.x`, com padronização dos produtos gráficos, atualização do relatório textual estatístico e substituição pública do termo “coorte” por **painel amostral por ano inicial**.

## Consulte também

- [`CHANGELOG.md`](CHANGELOG.md): histórico das versões públicas.
- [`VERSION`](VERSION): versão pública atual.
- [`LICENSE`](LICENSE): licença do projeto.
- [`docs/uso_de_ia.md`](docs/uso_de_ia.md): registro do uso auxiliar de IA, quando disponível.
- [`docs/versionamento.md`](docs/versionamento.md): notas de versionamento, quando disponível.

## Script recomendado para uso

O script atual recomendado está disponível diretamente na raiz do repositório:

- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

A mesma versão também é mantida nas demais cópias públicas do projeto para compatibilidade e rastreabilidade:

- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)
- [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R)
- [`monitora_campsav_alvo_global_v2.1.1.R`](monitora_campsav_alvo_global_v2.1.1.R)
- [`releases/v2.1.1/monitora_campsav_alvo_global_v2.1.1.R`](releases/v2.1.1/monitora_campsav_alvo_global_v2.1.1.R)

Na publicação da `v2.1.1`, essas cinco cópias públicas do script foram mantidas com conteúdo idêntico.

## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis.

O script foi desenhado para aceitar tanto dados brutos quanto arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais.

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
- tabelas estatísticas dos painéis amostrais por ano inicial;
- relatório textual estatístico (`relatorio_textual_estatistico.txt`);
- índice mestre de gráficos (`indice_graficos.csv`);
- gráficos publicáveis seriados em `output/plots_png/`;
- auditorias de símbolos, esforço amostral, layout de rótulos e produtos gráficos;
- arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

- relatório de execução;
- auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
- relatórios de performance, memória e controle de recursos.

## Principais recursos da v2.1.1

- Revisão editorial da nomenclatura pública dos produtos.
- Substituição pública de “coortes” por **painéis amostrais por ano inicial**.
- Explicitação de que esses painéis são subconjuntos longitudinais de UAs definidos pelo primeiro ano de acompanhamento, e não coortes ecológicas de indivíduos.
- Geração de 156 gráficos PNG seriados como `fig_001_...png` a `fig_156_...png`.
- Consolidação de um índice mestre de gráficos em `output/indice_graficos.csv`.
- Relatório textual estatístico ampliado e gerado ao final da execução, incorporando:
  - amostra total;
  - série pareada total;
  - comparações pareadas por período editorial;
  - painéis amostrais por ano inicial;
  - mudanças por categoria;
  - composição geral;
  - comparações contra medição anterior e linha de base acumulada.
- Inclusão das saídas estatísticas:
  - `estatisticas_mudanca_ano_a_ano_paineis_ano_inicial.csv`;
  - `estatisticas_mudanca_linha_base_paineis_ano_inicial.csv`;
  - `estatisticas_composicao_geral_ano_a_ano_paineis_ano_inicial.csv`;
  - `estatisticas_composicao_linha_base_paineis_ano_inicial.csv`.
- Correção da classificação pública da `fig_036` como gráfico de cobertura.
- Manutenção da rastreabilidade entre nomes públicos seriados e nomes internos legados por meio do índice mestre.

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
- `monitora_campsav_alvo_global_v2.1.1.R`: cópia versionada da versão pública atual.
- `releases/v2.1.1/`: cópia congelada da versão pública `v2.1.1`.
- `releases/v2.1.0/`, `releases/v2.0.0/`, `releases/v2.0.1/`, `releases/v2.0.2/`: versões públicas anteriores preservadas por rastreabilidade.
- `archive/versoes_historicas/`: versões históricas anteriores ao versionamento público semântico.
- `docs/`: documentação auxiliar, incluindo versionamento e uso de IA, quando presente.
- `tools/`: ferramentas auxiliares de auditoria e revisão, quando presentes.
- `.github/workflows/`: automações do GitHub Actions, quando presentes.

## Scripts históricos

Os scripts `.R` datados ou anteriores à adoção do versionamento público semântico foram movidos para:

- [`archive/versoes_historicas/`](archive/versoes_historicas/)

Esses arquivos representam versões históricas preservadas por rastreabilidade.

Na revisão associada à publicação da `v2.0.0`, os comentários dos scripts históricos foram revisados editorialmente e padronizados majoritariamente em português, sem alteração do código ativo.

A versão recomendada para uso atual é:

- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R)

## Uso auxiliar de IA generativa

Este projeto teve uma fase inicial de desenvolvimento baseada em edição manual, consulta a documentação técnica, exemplos de código e fontes abertas de referência, incluindo fóruns técnicos e materiais de apoio.

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração, organização do versionamento público, análise de logs, padronização de nomenclatura e proposição de testes.

O uso de IA teve caráter auxiliar.

As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados, execução local e responsabilidade técnica pelo script permanecem sob responsabilidade humana do autor/mantenedor.

Versões anteriores à adoção do versionamento semântico foram preservadas por rastreabilidade histórica e podem refletir diferentes fases de desenvolvimento, inclusive fases sem evidência clara de apoio direto de IA.

Mais detalhes, quando disponível, em:

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
Rscript monitora_campsav_alvo_global_v2.1.1.R
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

A v2.1.1 usa a expressão **painéis amostrais por ano inicial** para se referir a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento.

Esses painéis **não representam coortes ecológicas de indivíduos**. O termo “coorte” foi preservado apenas em nomes internos legados quando necessário para rastreabilidade do código, mas foi substituído nos produtos públicos.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

Na v2.1.1, o relatório passou a incorporar os produtos editoriais e os painéis amostrais por ano inicial, incluindo mudanças por categoria e mudanças de composição geral.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.1.1"
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
- os gráficos de validação devem ter sido conferidos visualmente;
- `output/indice_graficos.csv` deve estar consistente;
- `relatorio_textual_estatistico.txt` deve incorporar os produtos estatísticos atuais.

## Licença

Este projeto está licenciado sob a licença GPL-3.0.

Consulte:

- [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico
