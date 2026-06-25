# Monitora Campestre-Savânico — Alvo Global

## Versão pública atual

- Versão: `v2.5.3`
- Script principal versionado: [`monitora_campsav_alvo_global_v2.5.3.R`](monitora_campsav_alvo_global_v2.5.3.R)
- Script canônico: [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)
- Cópia congelada no repositório: [`releases/v2.5.3/monitora_campsav_alvo_global_v2.5.3.R`](releases/v2.5.3/monitora_campsav_alvo_global_v2.5.3.R)
- Assets auxiliares da publicação: [`release_assets/v2.5.3/`](release_assets/v2.5.3/)
- Modo padrão público seguro: `MONITORA_MODO_EXECUCAO <- "completo"`, `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`, `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"`, `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` e `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"`.

Para curadoria assistida, use `MONITORA_MODO_EXECUCAO <- "painel_e_parar"`. Para reabrir painel sem reconstruir tudo, use `MONITORA_MODO_EXECUCAO <- "abrir_painel_cache"` após uma execução que tenha gerado cache. Para continuar uma curadoria a partir de `registros_corrig.csv`, use `MONITORA_MODO_EXECUCAO <- "painel_incremental_registros_corrig"` com um único `registros_corrig*.csv` em `input/`.

## Principais recursos da versão atual

A versão `v2.5.3` consolida ganhos de performance com `data.table`, barra de progresso `cli` por padrão, estatística Monte Carlo reprodutível e instrumentação de performance, mantendo os defaults públicos seguros, a governança de privacidade e os modos de execução já publicados.

- Otimizações com `data.table` em preparação estatística, objetos gráficos, agregações, pareamentos e exportações intermediárias.
- Barra de progresso informativa com backend `cli`, percentual, etapa, detalhe e ETA, com atualização controlada para reduzir custo de console.
- Reprodutibilidade estatística para bootstrap e permutação Monte Carlo, com semente base e RNG registrados no log.
- Checkpoints mais granulares para diagnóstico de performance, incluindo `correcao_ponto_metro` e auditorias de COLETAS por UC+UA+ANO.
- Preservação dos produtos finais públicos da série 2.5: `registros_corrig.csv`, `registros_corrig_stat.csv`, relatórios, gráficos PNG, KMLs e auditorias.
- Manutenção do painel opcional de correções assistidas, validação espacial opcional, cache pré-painel, continuidade incremental e controles de privacidade.

## Versão anterior

A versão pública imediatamente anterior foi `v2.5.2`. O histórico detalhado das versões anteriores fica no [`CHANGELOG.md`](CHANGELOG.md) e nas páginas de release do GitHub.

## Modos de execução

Configure `MONITORA_MODO_EXECUCAO` no início do script conforme o objetivo da rodada.

- `completo`: executa todo o pipeline, com bases tratadas, auditorias, estatísticas, relatórios e gráficos.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` sem abrir o painel e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.
- `abrir_painel_cache`: reabre o painel a partir do cache pré-painel, sem reconstruir toda a leitura e sem reaplicar correções antigas em `input/`.
- `painel_incremental_registros_corrig`: abre o painel a partir de um `registros_corrig.csv` já produzido pelo script, colocado em `input/`, para continuar a curadoria.

O padrão público permanece seguro:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
```

Nos modos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_registros_corrig`, o script força internamente a abertura do painel, preservando o default público seguro fora desses modos.

## Privacidade e produtos locais sensíveis

Produtos locais gerados em `output/`, `log/` e `input/correcoes_*.csv` podem conter dados pessoais ou institucionais sensíveis, incluindo nomes, CPF, UC, UA, COLETA, coordenadas, fotos, UUIDs e observações de campo. Não publique nem compartilhe `input/`, `output/`, `log/`, `extracted/`, `registros_importados.csv`, `registros_corrig.csv`, `registros_validados.csv`, `correcoes_campos.csv` ou `correcoes_espaciais.csv` sem triagem.

`registros_importados.csv` fica desligado por padrão público seguro. Para gerá-lo em execução local deliberada, use `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "S"`.

## Entradas aceitas

Coloque os arquivos de entrada exclusivamente no subdiretório `input/`, no mesmo diretório do script. Essa regra evita confusão entre dados de entrada, produtos antigos e arquivos gerados em execuções anteriores. Arquivos de entrada colocados na raiz da pasta do script não devem ser usados.

Caso seja necessário reaproveitar um produto anterior, copie deliberadamente o arquivo adequado para `input/`.

Entradas reconhecidas:

- arquivos ZIP exportados individualmente pelo SISMONITORA;
- arquivos ZIP, CSV, XLSX ou XLS de exportação em lote;
- arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados deliberadamente no modo `painel_incremental_registros_corrig`;
- arquivo `input/correcoes_campos.csv`, quando usado deliberadamente para reaplicação auditável de correções.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora. O script aceita dados brutos e arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais.

O painel opcional de validação assistida reduz riscos associados à edição direta de CSVs consolidados em editores de planilha.

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

`registros_corrig.csv` é a base canônica do pipeline: contém os registros corrigidos, auditáveis e usados nas análises. `registros_validados.csv` é um produto público opcional, criado a partir de `registros_corrig.csv` final, com compatibilidade estrutural com o contrato do XLSForm vigente e com a exportação esperada pelo SISMONITORA. Ele não substitui a base canônica do pipeline.

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
- [`monitora_campsav_alvo_global_v2.5.3.R`](monitora_campsav_alvo_global_v2.5.3.R): cópia versionada da versão pública atual.
- [`releases/v2.5.3/`](releases/v2.5.3/): cópia congelada da versão pública `v2.5.3`.
- [`release_assets/v2.5.3/`](release_assets/v2.5.3/): artefatos auxiliares da publicação pública.
- [`archive/versoes_historicas/`](archive/versoes_historicas/): versões históricas anteriores ao versionamento público semântico.
- [`docs/`](docs/): documentação auxiliar, incluindo versionamento e uso de IA, quando presente.
- [`tools/`](tools/): ferramentas auxiliares de auditoria e revisão, quando presentes.
- [`.github/workflows/`](.github/workflows/): automações do GitHub Actions, quando presentes.

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
- `cli`
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

Exemplo em terminal:

```bash
MONITORA_PERFIL_EXECUCAO=economico MONITORA_EXPORTAR_KML=false Rscript monitora_campsav_alvo_global.R
```

## Interpretação dos gráficos

Os gráficos publicáveis foram desenhados para explicitar formação vegetacional, ano, esforço amostral (`n UA`), escopo amostral da comparação, incerteza por IC95%, símbolos estatísticos de mudança por categoria ou composição geral e legenda metodológica inferior, quando necessária.

Os gráficos são exportados em `output/plots_png/` com nomenclatura pública seriada. O arquivo `output/indice_graficos.csv` deve ser usado para identificar serial, nome público do PNG, nome interno legado, bloco analítico, escopo amostral, ano inicial do painel, métrica, tema, formação, presença ou ausência de rótulos, uso recomendado e arquivos de dados associados.

## Painéis amostrais por ano inicial

A expressão **painéis amostrais por ano inicial** refere-se a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento. Esses painéis são subconjuntos amostrais de acompanhamento temporal. Eles não representam grupos ecológicos de indivíduos acompanhados individualmente, mas sim conjuntos de unidades amostrais comparáveis dentro do desenho analítico adotado.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.5.3"
TAG="v${VERSAO}"
sha256sum   "monitora_campsav_alvo_global_${TAG}.R"   monitora_campsav_alvo_global.R   MONITORA_CAMPSAV_Alvo_Global.R   R/monitora_campsav_alvo_global.R   R_monitora_campsav_alvo_global.R   "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

grep -Rni "Versão pública"   "monitora_campsav_alvo_global_${TAG}.R"   monitora_campsav_alvo_global.R   MONITORA_CAMPSAV_Alvo_Global.R   R/monitora_campsav_alvo_global.R   R_monitora_campsav_alvo_global.R   "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

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
- [`releases/v2.5.3/`](releases/v2.5.3/): cópia congelada da versão pública atual.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

- [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico
