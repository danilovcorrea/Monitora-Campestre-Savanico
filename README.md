## Versão pública v2.4.1

A versão `v2.4.1` mantém `registros_corrig.csv` como versão canônica corrigida, auditável e usada pelo pipeline, e acrescenta `registros_validados.csv` como produto público opcional compatível com o contrato do XLSForm vigente e com a estrutura de exportação do SISMONITORA.

Principais pontos da versão:

- `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` permanece como padrão público.
- Com `"S"`, `registros_validados.csv` é gerado a partir de `registros_corrig.csv` final, sem template externo.
- O produto validado usa o XLSForm vigente como contrato semântico e o formato de exportação do SISMONITORA como contrato de estrutura, ordem e serialização.
- Foram ampliadas as auditorias de schema, formatos, domínios XLSForm, condicionais, chaves, UUIDs e sanitizações.
- A sanitização de outras formas de vida foi consolidada para fontes históricas, com conversão apenas de descritores inequívocos e limpeza auditada de resíduos legados.
- A sanitização de fotos e descritores de forma de vida desconhecida preserva esses campos apenas quando o token `desconhecida` permanece na categoria correspondente.
- Resumos por unidade vazios são sobrescritos com cabeçalho vazio, evitando arquivos residuais em `output/`.


# Monitora Campestre-Savânico — Alvo Global

<!-- MONITORA_RELEASE_ATUAL_START -->
## Versão atual

**Versão pública atual:** `v2.4.0`

**Script principal:** `monitora_campsav_alvo_global_v2.4.1.R`

A versão `v2.4.0` consolida os modos de execução para desenvolvimento, execução parcial e validação do painel, além de operações atômicas de curadoria com auditoria de persistência no objeto e no `registros_corrig.csv` exportado.

### Modos de execução

- `completo`: executa todo o pipeline.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` sem abrir o painel e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.

O padrão público permanece:

- `MONITORA_MODO_EXECUCAO <- "completo"`
- `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`

### Principais recursos da versão `v2.4.0`

- Painel com operações semânticas atômicas e fila coerente com as ações do usuário.
- Exclusão atômica de COLETAS em lote.
- Movimento atômico de formas de vida e substituição assistida de forma desconhecida.
- Movimento em lote de formas de vida por COLETAS, com migração segura e relatório de ambiguidades.
- Limpeza atômica de outras formas de vida.
- Notificações de início e conclusão para operações demoradas.
- Trava contra duplo clique e duplicidade semântica.
- Auditoria de persistência pós-aplicação e pós-exportação.
- Sincronização final de `Encostam`/`tipo_forma_vida` com os campos inferiores finais.
- Comparação pré/pós-correções robusta a diferenças de classe em campos contextuais.

<!-- MONITORA_RELEASE_ATUAL_END -->

Scripts de tratamento, auditoria, deduplicação, análise estatística, validação assistida e geração de produtos gráficos para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

## Histórico da versão pública v2.3.2
A versão `v2.3.2` sucede a `v2.3.1` e consolida ajustes operacionais no Painel de validação - correções assistidas de `registros_corrig`, com foco em exclusão auditável de COLETAS, ausência de vestígios após remoção de linhas e melhoria da visualização das tabelas do painel.

A versão mantém o fluxo analítico, estatístico, gráfico e de auditoria consolidado nas versões anteriores, preserva a taxonomia pública vigente de **painéis amostrais por ano inicial** e mantém o padrão inicial de execução sem abertura do painel:

```r
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

## Consulte também

* [CHANGELOG.md](CHANGELOG.md): histórico das versões públicas.
* [VERSION](VERSION): versão pública atual.
* [LICENSE](LICENSE): licença do projeto.
* [docs/uso_de_ia.md](docs/uso_de_ia.md): registro do uso auxiliar de IA, quando disponível.
* [docs/versionamento.md](docs/versionamento.md): notas de versionamento, quando disponível.
* [releases/v2.3.2/](releases/v2.3.2/): cópia congelada da versão pública atual.

## Script recomendado para uso

Use preferencialmente:

* [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): cópia pública atual com nome padronizado em minúsculas.
* [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R): nome público histórico equivalente.
* [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R): cópia mantida na estrutura interna do projeto.
* [`monitora_campsav_alvo_global_v2.3.2.R`](monitora_campsav_alvo_global_v2.3.2.R): cópia versionada da versão pública atual.
* [`releases/v2.3.2/monitora_campsav_alvo_global_v2.3.2.R`](releases/v2.3.2/monitora_campsav_alvo_global_v2.3.2.R): cópia congelada da versão pública `v2.3.2`.

Na publicação da `v2.3.2`, essas cópias públicas do script são mantidas com conteúdo idêntico.

## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis.

O script foi desenhado para aceitar tanto dados brutos quanto arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais. O painel opcional de validação assistida reduz riscos associados à edição direta de CSVs consolidados em editores de planilha.

## Entradas aceitas

Coloque os arquivos no subdiretório `input/` ou no mesmo diretório do script. O uso de `input/` é recomendado para evitar confusão com produtos antigos.

Entradas reconhecidas:

* arquivos ZIP exportados individualmente pelo SISMONITORA;
* arquivos ZIP, CSV ou XLSX de exportação em lote;
* arquivos `registros_corrig*.csv` gerados por execuções anteriores do próprio script, quando usados deliberadamente como nova entrada;
* arquivo `input/correcoes_campos.csv`, quando gerado pelo painel e usado para reaplicação auditável das correções.

O script não exige extração manual dos ZIPs. As extrações recursivas são feitas em `extracted/`.

## Saídas principais

Na raiz do projeto:

* `registros_corrig.csv`: registros padronizados, deduplicados e auditáveis;
* `registros_corrig_stat.csv`: base estatística por UC, UA, ano e métricas derivadas.

Em `output/`:

* tabelas de proporção relativa e cobertura vegetal;
* tabelas estatísticas de mudança ano a ano, linha de base e composição geral;
* tabelas estatísticas dos painéis amostrais por ano inicial;
* relatório textual estatístico (`relatorio_textual_estatistico.txt`);
* índice mestre de gráficos (`indice_graficos.csv`);
* gráficos publicáveis seriados em `output/plots_png/`;
* auditorias de símbolos, esforço amostral, layout de rótulos e produtos gráficos;
* auditorias e dicionários auxiliares do painel em `output/correcoes_campos/`, quando a validação assistida for usada;
* arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

* relatório de execução;
* auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
* auditoria de correções assistidas (`auditoria_correcoes_campos_*.csv`), quando aplicável;
* relatórios de performance, memória e controle de recursos.

## Principais recursos da v2.3.2

* Correção do botão de exclusão de COLETAS filtradas/selecionadas no painel.
* A exclusão de COLETAS passa a ser tratada como remoção integral de linhas, sem tentativa de edição da coluna estrutural `COLETA`.
* Auditoria pós-exclusão bloqueia a execução caso qualquer linha das COLETAS excluídas permaneça em `registros_corrig`.
* O painel oculta `uuid`/amostragem-registro somente na visualização das tabelas, preservando o campo nos dados, operações, logs, auditorias e arquivos finais.
* O padrão inicial da variável `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES` permanece `"N"`.
* Comentários do script público foram revisados para remover menções residuais a versões públicas anteriores como referência de desenvolvimento corrente, incluindo a menção indevida à `v2.3.0` observada na versão anterior.

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

Esse arquivo registra a receita das alterações, em formato longo, e não substitui diretamente `registros_corrig.csv`. As correções são aplicadas pelo script com auditoria. O arquivo final corrigido segue sendo:

```text
output/registros_corrig.csv
```

## Recursos consolidados desde a v2.0.0

Desde a primeira versão pública com versionamento semântico, o projeto consolidou importação de múltiplos tipos de entrada, extração recursiva de ZIPs do SISMONITORA, padronização de categorias e nomes, tratamento de arquivos pós-processados, deduplicação semântica, auditorias, controle de performance, cálculo de cobertura vegetal e proporção relativa, estatística inferencial pareada por UA, comparações temporais, análise de composição geral, geração de gráficos, relatório textual estatístico e organização de versões congeladas em `releases/`.

## Estrutura do repositório

* [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R): script atual recomendado para uso.
* [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): cópia pública equivalente do script atual.
* [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R): cópia do script atual na estrutura interna do projeto.
* [`monitora_campsav_alvo_global_v2.3.2.R`](monitora_campsav_alvo_global_v2.3.2.R): cópia versionada da versão pública atual.
* [`releases/v2.3.2/`](releases/v2.3.2/): cópia congelada da versão pública `v2.3.2`.
* [`releases/v2.3.1/`](releases/v2.3.1/), [`releases/v2.3.0/`](releases/v2.3.0/), [`releases/v2.2.2/`](releases/v2.2.2/), [`releases/v2.2.1/`](releases/v2.2.1/), [`releases/v2.2.0/`](releases/v2.2.0/), [`releases/v2.1.3/`](releases/v2.1.3/), [`releases/v2.1.2/`](releases/v2.1.2/), [`releases/v2.1.1/`](releases/v2.1.1/), [`releases/v2.1.0/`](releases/v2.1.0/), [`releases/v2.0.2/`](releases/v2.0.2/), [`releases/v2.0.1/`](releases/v2.0.1/) e [`releases/v2.0.0/`](releases/v2.0.0/): versões públicas anteriores preservadas por rastreabilidade, quando presentes no repositório.
* [`archive/versoes_historicas/`](archive/versoes_historicas/): versões históricas anteriores ao versionamento público semântico.
* [`docs/`](docs/): documentação auxiliar, incluindo versionamento e uso de IA, quando presente.
* [`tools/`](tools/): ferramentas auxiliares de auditoria e revisão, quando presentes.
* [`.github/workflows/`](.github/workflows/): automações do GitHub Actions, quando presentes.

## Scripts históricos

Os scripts `.R` datados ou anteriores à adoção do versionamento público semântico foram movidos para:

* [`archive/versoes_historicas/`](archive/versoes_historicas/)

Esses arquivos representam versões históricas preservadas por rastreabilidade. A versão recomendada para uso atual é:

* [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)

## Uso auxiliar de IA generativa

Este projeto teve uma fase inicial de desenvolvimento baseada em edição manual, consulta a documentação técnica, exemplos de código e fontes abertas de referência, incluindo fóruns técnicos e materiais de apoio.

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração, organização do versionamento público, análise de logs, padronização de nomenclatura e proposição de testes. O uso de IA teve caráter auxiliar.

As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor. Ferramentas de IA não substituem a validação humana, a execução local do script, a inspeção dos produtos, a comparação de hashes, a auditoria dos logs e a responsabilidade técnica sobre a publicação.

Mais detalhes em:

* [`docs/uso_de_ia.md`](docs/uso_de_ia.md)

## Backup pré-revisão

O estado do repositório antes da revisão editorial, adoção do versionamento semântico e publicação da `v2.0.0` foi preservado em:

* branch: `backup/pre-revisao-editorial-20260610`;
* tag: `pre-revisao-editorial-20260610`.

## Requisitos de R

Pacotes usados pelo script:

* `rstudioapi`
* `dplyr`
* `data.table`
* `purrr`
* `stringr`
* `tidyverse`
* `ggplot2`
* `ggrepel`
* `readxl`
* `openxlsx`
* `sf`
* `shiny` e `DT`, apenas quando o painel de correções assistidas for ativado.

O script tenta instalar pacotes ausentes durante a execução. Em ambientes institucionais ou sem permissão de instalação, instale os pacotes previamente.

## Uso básico

1. Clone ou baixe este repositório.
2. Coloque os arquivos de entrada em `input/`.
3. Execute o script completo no RStudio ou por `Rscript`.
4. Consulte os produtos em `output/` e as auditorias em `log/`.

Exemplo:

```bash
Rscript monitora_campsav_alvo_global.R
```

Também é possível executar a versão pública específica:

```bash
Rscript monitora_campsav_alvo_global_v2.3.2.R
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

Os gráficos publicáveis foram desenhados para explicitar formação vegetacional, ano, esforço amostral (`n UA`), escopo amostral da comparação, incerteza por IC95%, símbolos estatísticos de mudança por categoria ou composição geral, e legenda metodológica inferior, quando necessária.

Os gráficos são exportados em `output/plots_png/` com nomenclatura pública seriada. O arquivo `output/indice_graficos.csv` deve ser usado para identificar rapidamente serial, nome público do PNG, nome interno legado, bloco analítico, escopo amostral, ano inicial do painel, métrica, tema, formação, presença ou ausência de rótulos, uso recomendado e arquivos de dados associados.

## Painéis amostrais por ano inicial

A expressão **painéis amostrais por ano inicial** refere-se a subconjuntos longitudinais de UAs agrupadas pelo primeiro ano em que passam a compor uma série de acompanhamento. Esses painéis são subconjuntos amostrais de acompanhamento temporal.

Eles não representam grupos ecológicos de indivíduos acompanhados individualmente, mas sim conjuntos de unidades amostrais comparáveis dentro do desenho analítico adotado.

## Relatório textual estatístico

O relatório `output/relatorio_textual_estatistico.txt` sintetiza os principais achados estatísticos e descreve resultados por UC, formação vegetacional, ano, linha de base, grupo, categoria e escopo amostral.

## Auditoria antes de publicar uma versão

Antes de subir uma nova versão, conferir:

```bash
VERSAO="2.3.2"
TAG="v${VERSAO}"
sha256sum   "monitora_campsav_alvo_global_${TAG}.R"   monitora_campsav_alvo_global.R   MONITORA_CAMPSAV_Alvo_Global.R   R/monitora_campsav_alvo_global.R   R_monitora_campsav_alvo_global.R   "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

grep -Rni "Versão pública"   "monitora_campsav_alvo_global_${TAG}.R"   monitora_campsav_alvo_global.R   MONITORA_CAMPSAV_Alvo_Global.R   R/monitora_campsav_alvo_global.R   R_monitora_campsav_alvo_global.R   "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

cat VERSION
```

Critério de liberação:

* os scripts públicos devem ter o mesmo hash;
* `VERSION` deve conter a versão publicada;
* a execução sem painel deve ser validada;
* o painel deve ser testado em correção simples, movimento assistido e exclusão de COLETAS em lote;
* a exclusão de COLETAS não deve deixar linhas residuais em `registros_corrig.csv`;
* os gráficos de validação devem ter sido conferidos visualmente;
* `output/indice_graficos.csv` deve estar consistente;
* `relatorio_textual_estatistico.txt` deve incorporar os produtos estatísticos atuais.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte:

* [`LICENSE`](LICENSE)

## Citação

CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico
