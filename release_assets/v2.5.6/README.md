# Monitora Campestre-Savânico — Alvo Global

### Destaques da versão v2.5.6

- Consolida o fechamento validado de `registros_corrig.csv` e `registros_validados.csv`.
- Mantém o painel de correções assistidas com preview coerente e operações vetorizadas.
- Preserva `outra_forma_vida` como choice válido do XLSForm 2025 quando acompanhado de `forma_vida_outros`.
- Preserva o campo contratual `amostragem/registro/forma_vida_seca_mortaarvore_abaixo`.
- Adiciona sanitização cadastral automática não bloqueante para deslocamentos inequívocos entre `CICLO`, `CAMPANHA`, `EA` e `UA`.
- Registra alertas cadastrais não bloqueantes em relatórios específicos.


## Versão pública atual

- Versão: `v2.5.6`
- Script principal versionado: [`monitora_campsav_alvo_global_v2.5.6.R`](monitora_campsav_alvo_global_v2.5.6.R)
- Script canônico: [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)
- Cópia congelada no repositório: [`releases/v2.5.6/monitora_campsav_alvo_global_v2.5.6.R`](releases/v2.5.6/monitora_campsav_alvo_global_v2.5.6.R)
- Assets auxiliares da publicação: [`release_assets/v2.5.6/`](release_assets/v2.5.6/)
- Pacote completo da release: [`releases/v2.5.6/Monitora-Campestre-Savanico_v2.5.5_release_completa.zip`](releases/v2.5.6/Monitora-Campestre-Savanico_v2.5.5_release_completa.zip)
- Release no GitHub: [https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.5.6](https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.5.6)

A versão `v2.5.6` consolida a curadoria assistida por painel com contrato SISMONITORA/XLSForm 2025, produtos de entrada saneados para comparação, controles de edição por tipo de atributo e auditorias adicionais de rastreabilidade.

A versão pública imediatamente anterior foi `v2.5.4`. O histórico detalhado fica no `CHANGELOG.md` e nas páginas de release do GitHub.

## Finalidade

Este repositório consolida rotinas em R para tratamento, auditoria e análise de dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

O script lê exportações do SISMONITORA, harmoniza colunas históricas, deduplica registros, aplica correções auditáveis, gera bases tratadas, estatísticas temporais, relatórios, gráficos e auditorias de consistência.

## Principais recursos da versão v2.5.6

A versão `v2.5.6` consolida o painel contratual de correções, a geração de registros importados saneados para comparação, os controles de edição por tipo de atributo e auditorias adicionais de rastreabilidade.

Principais entregas públicas:

- correção dos modos de painel, incluindo `painel_e_parar`, `abrir_painel_cache` e modos incrementais de painel;
- detalhamento adicional de progresso após o painel e durante etapas de fechamento contratual;
- geração de `registros_importados.csv` saneado e comparável com `registros_corrig.csv`;
- preservação do retrato técnico bruto em `registros_importados_bruto.csv`;
- auditorias automáticas entre `registros_importados.csv`, `registros_corrig.csv` e `registros_validados.csv`;
- seletor de atributos do painel baseado no template SISMONITORA/XLSForm 2025;
- ordenação do seletor conforme a ordem contratual do template;
- inclusão de atributos contratuais mesmo quando ausentes ou vazios nas coletas selecionadas;
- controles dinâmicos de edição por tipo de atributo;
- bloqueio de operações incompatíveis com listas de tokens;
- auditoria de cobertura dos controles de edição do painel;
- preservação dos defaults públicos seguros, com painel e produtos sensíveis desligados por padrão.

## Fluxo geral dos dados

O pipeline organiza os dados em três níveis principais.

### `registros_importados.csv`

Produto opcional que registra o retrato da importação consolidada antes das etapas finais de harmonização, deduplicação, curadoria e contrato.

Ele fica desligado por padrão público seguro:

```r
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
```

Quando habilitado, deve ser tratado como produto sensível, pois pode conter dados brutos, identificadores, metadados institucionais e campos ainda não saneados.

### `registros_corrig.csv`

Base canônica corrigida e auditável usada pelo pipeline analítico.

Na versão `v2.5.4`, este produto só deve ser materializado depois do fechamento contratual da base em memória. Ausências são gravadas fisicamente como o literal `NA`, para reduzir ambiguidade em editores de planilha e em reuso incremental.

### `registros_validados.csv`

Produto opcional derivado de `registros_corrig.csv`, com 129 atributos na ordem e formato esperados pelo contrato de validação.

Ele mantém vazios efetivos, não o literal `NA`, para preservar compatibilidade com fluxos de validação e importação que interpretam campos vazios como ausência real de preenchimento.

A geração é controlada por:

```r
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
```

## Modos de execução

Configure `MONITORA_MODO_EXECUCAO` no início do script conforme o objetivo da rodada.

- `completo`: executa o pipeline completo, incluindo bases tratadas, auditorias, estatísticas, relatórios e gráficos.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.
- `abrir_painel_cache`: reabre o painel a partir do cache pré-painel, sem reconstruir toda a leitura.
- `painel_incremental_registros_corrig`: abre o painel a partir de um `registros_corrig.csv` já produzido pelo próprio script e colocado deliberadamente em `input/`.

Nos modos de painel, o script pode forçar internamente a abertura da interface de curadoria, preservando o default público seguro fora desses modos.

## Painel de validação — correções assistidas

A abertura direta do painel também pode ser controlada por variável:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

Para abrir o painel deliberadamente:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

O painel registra operações em:

```text
input/correcoes_campos.csv
```

Esse arquivo representa a receita auditável de correções. Ele não substitui diretamente `registros_corrig.csv`.

## Entradas aceitas

Coloque os arquivos de entrada exclusivamente no subdiretório `input/`, no mesmo diretório do script. Essa regra evita mistura entre dados de entrada, produtos antigos e arquivos gerados.

Entradas reconhecidas:

- arquivos ZIP exportados individualmente pelo SISMONITORA;
- arquivos ZIP, CSV, XLSX ou XLS de exportação em lote;
- arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados no modo incremental adequado;
- arquivo `input/correcoes_campos.csv`, quando usado deliberadamente para reaplicação auditável de correções.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Saídas principais

Na raiz do projeto ou diretório de execução:

- `registros_corrig.csv`;
- `registros_corrig_stat.csv`.

Em `output/`:

- `registros_validados.csv`, quando habilitado;
- tabelas de proporção relativa e cobertura vegetal;
- tabelas estatísticas de mudança ano a ano, linha de base e composição geral;
- tabelas estatísticas dos painéis amostrais por ano inicial;
- relatório textual estatístico;
- índice mestre de gráficos;
- gráficos publicáveis seriados em `output/plots_png/`;
- auditorias de símbolos, esforço amostral, layout de rótulos e produtos gráficos;
- auditorias e dicionários auxiliares do painel, quando usado;
- arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

- relatório de execução;
- auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
- auditoria de correções assistidas, quando aplicável;
- auditorias de contrato e de `registros_validados.csv`, quando esse produto for gerado;
- relatórios de performance, memória e controle de recursos.

## Privacidade e produtos locais sensíveis

Produtos gerados em `input/`, `output/`, `log/` e `extracted/` podem conter dados pessoais ou institucionais sensíveis, incluindo nomes, CPF, UC, UA, COLETA, coordenadas, fotos, UUIDs, observações de campo e metadados de coleta.

Não publique nem compartilhe sem triagem:

- `input/`;
- `output/`;
- `log/`;
- `extracted/`;
- `registros_importados.csv`;
- `registros_corrig.csv`;
- `registros_corrig_stat.csv`;
- `registros_validados.csv`;
- `correcoes_campos.csv`;
- `correcoes_espaciais.csv`.

## Estrutura do repositório

- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): script público canônico atual.
- [`monitora_campsav_alvo_global_v2.5.6.R`](monitora_campsav_alvo_global_v2.5.6.R): script público versionado da versão atual.
- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): script canônico da versão atual.
- [`VERSION`](VERSION): número da versão pública atual.
- [`CHANGELOG.md`](CHANGELOG.md): histórico público de mudanças.
- [`RELEASE_NOTES_v2.5.5.md`](RELEASE_NOTES_v2.5.5.md): notas da release atual.
- [`release_assets/v2.5.6/`](release_assets/v2.5.6/): artefatos auxiliares da publicação.
- [`releases/v2.5.6/`](releases/v2.5.6/): cópia congelada da release no repositório.
- [`Monitora-Campestre-Savanico_v2.5.5_release_completa.zip`](releases/v2.5.6/Monitora-Campestre-Savanico_v2.5.5_release_completa.zip): pacote completo da release.
- [`archive/versoes_historicas/`](archive/versoes_historicas/): versões históricas anteriores ao versionamento público semântico.
- [`docs/`](docs/): documentação auxiliar, quando presente.
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
- `shiny` e `DT`, apenas quando o painel de correções assistidas for ativado

O script tenta instalar pacotes ausentes durante a execução. Em ambientes institucionais ou sem permissão de instalação, instale os pacotes previamente.

## Variáveis de ambiente úteis

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

Os gráficos publicáveis explicitam formação vegetacional, ano, esforço amostral, escopo de comparação, incerteza por IC95%, símbolos estatísticos de mudança por categoria ou composição geral e legenda metodológica inferior, quando necessária.

Os gráficos são exportados em `output/plots_png/`. O arquivo `output/indice_graficos.csv` deve ser usado para identificar nome público do PNG, bloco analítico, escopo amostral, ano inicial do painel, métrica, tema, formação, presença ou ausência de rótulos, uso recomendado e arquivos de dados associados.

## Painéis amostrais por ano inicial

A expressão **painéis amostrais por ano inicial** refere-se a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento. Esses painéis são subconjuntos amostrais de acompanhamento temporal. Eles não representam grupos ecológicos de indivíduos acompanhados individualmente, mas sim conjuntos de unidades amostrais comparáveis dentro do desenho analítico adotado.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

## Auditoria antes de publicar uma versão

Antes de publicar uma nova versão, confira:

- scripts públicos canônico e versionado com conteúdo completo;
- `VERSION` coerente com a versão publicada;
- README, CHANGELOG e release notes sem nomes de arquivos de desenvolvimento;
- ausência de rótulos operacionais de desenvolvimento, caminhos locais ou marcas de trabalho no script público;
- painel desligado por padrão público seguro;
- checksums atualizados;
- ZIP completo da release recriado a partir dos arquivos finais;
- tag e release apontando para o commit correto;
- assets mínimos da release publicados.

## Como citar este repositório

Ao citar este repositório, indique a versão pública utilizada, a data de acesso e, quando aplicável, o endereço da release específica.

Formato recomendado:

```text
CORRÊA, Danilo V. Monitora Campestre-Savânico — Alvo Global. Versão v2.5.6. GitHub, 2026. Disponível em: https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.5.6. Acesso em: DD mês AAAA.
```

Formato BibTeX sugerido:

```bibtex
@software{correa_monitora_campestre_savanico_2_5_5,
  author = {Corrêa, Danilo V.},
  title = {Monitora Campestre-Savânico — Alvo Global},
  version = {v2.5.6},
  year = {2026},
  url = {https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.5.6},
  note = {Script em R para tratamento, auditoria e análise de dados do Componente Campestre Savânico do Programa Monitora}
}
```

Se você usou uma release específica, prefira citar o link da release em vez do link genérico da página principal do repositório.

## Uso auxiliar de IA generativa

Este projeto teve apoio de IA generativa em etapas de refatoração, revisão, organização editorial, elaboração de comandos de auditoria e diagnóstico de erros. O uso de IA não substitui validação técnica, revisão humana, execução local, conferência de produtos, auditoria de privacidade e responsabilidade sobre publicação.

Recomenda-se usar IA apenas como apoio controlado, preservando dados sensíveis fora de prompts públicos e conferindo manualmente qualquer alteração antes de execução ou publicação.


Repositório público: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
