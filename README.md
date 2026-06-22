# Monitora Campestre-Savânico — Alvo Global

## Versão atual

**Versão pública atual:** `v2.4.1`

**Script principal:** `monitora_campsav_alvo_global_v2.4.1.R`

A versão `v2.4.1` mantém `registros_corrig.csv` como versão canônica corrigida, auditável e usada pelo pipeline, e acrescenta `registros_validados.csv` como produto público opcional compatível com o contrato do XLSForm vigente e com a estrutura de exportação do SISMONITORA.

## Principais recursos da versão `v2.4.1`

- Mantém `registros_corrig.csv` como versão canônica corrigida, auditável e usada pelo pipeline.
- Acrescenta `registros_validados.csv` como versão pública opcional compatível com o contrato do XLSForm vigente e com a estrutura de exportação do SISMONITORA.
- Mantém `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` como padrão público.
- Com `"S"`, gera `registros_validados.csv` a partir de `registros_corrig.csv` final, sem template externo.
- Amplia auditorias de schema, formatos, domínios XLSForm, condicionais, chaves, UUIDs e sanitizações.
- Consolida a sanitização de outras formas de vida em fontes históricas, com conversão apenas de descritores inequívocos e limpeza de resíduos legados sem conversão.
- Consolida a sanitização de dependentes de forma de vida desconhecida, preservando fotos e descritores apenas quando o token `desconhecida` ainda está presente.
- Corrige a sobrescrita de resumos por unidade vazios, evitando arquivos residuais em `output/`.

## Modos de execução

- `completo`: executa todo o pipeline.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` sem abrir o painel e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.

O padrão público permanece:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
```

## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

O script aceita dados brutos e arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais. O painel opcional de validação assistida reduz riscos associados à edição direta de CSVs consolidados em editores de planilha.

## Entradas aceitas

Coloque os arquivos no subdiretório `input/` ou no mesmo diretório do script. O uso de `input/` é recomendado para evitar confusão com produtos antigos.

Entradas reconhecidas:

- arquivos ZIP exportados individualmente pelo SISMONITORA;
- arquivos ZIP, CSV ou XLSX de exportação em lote;
- arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados deliberadamente como nova entrada;
- arquivo `input/correcoes_campos.csv`, quando gerado pelo painel e usado para reaplicação auditável das correções.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Saídas principais

Na raiz do projeto ou no diretório de execução:

- `registros_corrig.csv`: versão canônica corrigida, padronizada, deduplicada e auditável;
- `registros_corrig_stat.csv`: base estatística por UC, UA, ano e métricas derivadas.

Em `output/`:

- `registros_validados.csv`, quando `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"`;
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
- auditorias de `registros_validados.csv`, quando esse produto for gerado;
- relatórios de performance, memória e controle de recursos.

## `registros_corrig.csv` e `registros_validados.csv`

`registros_corrig.csv` é a base canônica do pipeline: contém os registros corrigidos, auditáveis e usados nas análises.

`registros_validados.csv` é um produto público opcional, criado a partir de `registros_corrig.csv` final, com compatibilidade estrutural com o contrato do XLSForm vigente e com a exportação esperada pelo SISMONITORA. Ele não substitui a base canônica do pipeline.

## Painel de validação — correções assistidas

A abertura do painel é controlada por variável no início do script:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

Para abrir o painel:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

Com `"N"`, o script segue a execução analítica normal. Com `"S"`, o script consolida `registros_corrig.csv`, abre o painel de validação e, após o fechamento do painel, aplica as correções salvas e continua o fluxo.

O painel gera ou atualiza:

```text
input/correcoes_campos.csv
```

Esse arquivo registra a receita das alterações, em formato longo, e não substitui diretamente `registros_corrig.csv`.

## Estrutura do repositório

- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R): script atual recomendado para uso.
- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): cópia pública equivalente do script atual.
- [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R): cópia do script atual na estrutura interna do projeto.
- [`R_monitora_campsav_alvo_global.R`](R_monitora_campsav_alvo_global.R): cópia pública equivalente.
- [`monitora_campsav_alvo_global_v2.4.1.R`](monitora_campsav_alvo_global_v2.4.1.R): cópia versionada da versão pública atual.
- [`releases/v2.4.1/`](releases/v2.4.1/): cópia congelada da versão pública `v2.4.1`.
- [`release_assets/v2.4.1/`](release_assets/v2.4.1/): artefatos auxiliares da publicação pública.
- [`archive/versoes_historicas/`](archive/versoes_historicas/): versões históricas anteriores ao versionamento público semântico.
- [`docs/`](docs/): documentação auxiliar, incluindo versionamento e uso de IA, quando presente.
- [`tools/`](tools/): ferramentas auxiliares de auditoria e revisão, quando presentes.
- [`.github/workflows/`](.github/workflows/): automações do GitHub Actions, quando presentes.

## Script recomendado para uso

Use preferencialmente:

```bash
Rscript monitora_campsav_alvo_global.R
```

Também é possível executar a versão pública específica:

```bash
Rscript monitora_campsav_alvo_global_v2.4.1.R
```

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

Os gráficos publicáveis foram desenhados para explicitar formação vegetacional, ano, esforço amostral (`n UA`), escopo amostral da comparação, incerteza por IC95%, símbolos estatísticos de mudança por categoria ou composição geral, e legenda metodológica inferior, quando necessária.

Os gráficos são exportados em `output/plots_png/` com nomenclatura pública seriada. O arquivo `output/indice_graficos.csv` deve ser usado para identificar serial, nome público do PNG, nome interno legado, bloco analítico, escopo amostral, ano inicial do painel, métrica, tema, formação, presença ou ausência de rótulos, uso recomendado e arquivos de dados associados.

## Painéis amostrais por ano inicial

A expressão **painéis amostrais por ano inicial** refere-se a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento. Esses painéis são subconjuntos amostrais de acompanhamento temporal. Eles não representam grupos ecológicos de indivíduos acompanhados individualmente, mas sim conjuntos de unidades amostrais comparáveis dentro do desenho analítico adotado.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.4.1"
TAG="v${VERSAO}"

sha256sum \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  R_monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

grep -Rni "Versão pública" \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  R_monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

cat VERSION
```

Critério de liberação:

- os scripts públicos devem ter o mesmo hash;
- `VERSION` deve conter a versão publicada;
- a execução sem painel deve ser validada;
- quando aplicável, o painel deve ser testado em correção simples, movimento assistido e exclusão de COLETAS em lote;
- a exclusão de COLETAS não deve deixar linhas residuais em `registros_corrig.csv`;
- `registros_validados.csv`, quando gerado, deve passar nas auditorias contratuais;
- os gráficos de validação devem ter sido conferidos visualmente;
- `output/indice_graficos.csv` deve estar consistente;
- `relatorio_textual_estatistico.txt` deve incorporar os produtos estatísticos atuais.

## Uso auxiliar de IA generativa

Este projeto teve uma fase inicial de desenvolvimento baseada em edição manual, consulta a documentação técnica, exemplos de código e fontes abertas de referência, incluindo fóruns técnicos e materiais de apoio.

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração, organização do versionamento público, análise de logs, padronização de nomenclatura e proposição de testes. O uso de IA teve caráter auxiliar.

As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor. Ferramentas de IA não substituem a validação humana, a execução local do script, a inspeção dos produtos, a comparação de hashes, a auditoria dos logs e a responsabilidade técnica sobre a publicação.

Mais detalhes em:

- [`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Consulte também

- [`CHANGELOG.md`](CHANGELOG.md): histórico das versões públicas.
- [`VERSION`](VERSION): versão pública atual.
- [`LICENSE`](LICENSE): licença do projeto.
- [`docs/uso_de_ia.md`](docs/uso_de_ia.md): registro do uso auxiliar de IA, quando disponível.
- [`docs/versionamento.md`](docs/versionamento.md): notas de versionamento, quando disponível.
- [`releases/v2.4.1/`](releases/v2.4.1/): cópia congelada da versão pública atual.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

- [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico
