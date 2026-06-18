# Monitora Campestre-Savânico — Alvo Global

Scripts de tratamento, auditoria, deduplicação, análise estatística, validação assistida e geração de produtos gráficos para dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

## Versão pública atual

**v2.3.0**

A versão `v2.3.0` consolida a linha pública posterior à `v2.2.2` e amplia o Painel de validação - correções assistidas de `registros_corrig`, com foco em correções operacionais em lote por múltiplas COLETAS, exclusão auditável de COLETAS, triagem de UAs duplicadas no mesmo ano, preservação de conflitos reais de campo para correção manual e reforço das travas de integridade antes da etapa estatística.

A versão mantém os recursos da `v2.2.2`, incluindo integridade transacional do painel, auditoria semântica pré/pós-correção, triagem operacional de formas de vida exóticas, relatório de ocorrência de formas de vida exóticas, deduplicação defensiva e melhorias de performance.

A terminologia pública vigente utiliza **painéis amostrais por ano inicial** para os subconjuntos longitudinais de UAs definidos pelo primeiro ano de acompanhamento.

## Consulte também

- [CHANGELOG.md](CHANGELOG.md): histórico das versões públicas.
- [VERSION](VERSION): versão pública atual.
- [LICENSE](LICENSE): licença do projeto.
- [docs/uso_de_ia.md](docs/uso_de_ia.md): registro do uso auxiliar de IA.
- [releases/v2.3.0/](releases/v2.3.0/): cópia congelada da versão pública atual.

## Script recomendado para uso

Use preferencialmente:

- [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R): cópia pública atual com nome padronizado em minúsculas.
- [`MONITORA_CAMPSAV_Alvo_Global.R`](MONITORA_CAMPSAV_Alvo_Global.R): nome público histórico equivalente.
- [`R/monitora_campsav_alvo_global.R`](R/monitora_campsav_alvo_global.R): cópia mantida na estrutura interna do projeto.
- [`monitora_campsav_alvo_global_v2.3.0.R`](monitora_campsav_alvo_global_v2.3.0.R): cópia versionada da versão pública atual.
- [`releases/v2.3.0/monitora_campsav_alvo_global_v2.3.0.R`](releases/v2.3.0/monitora_campsav_alvo_global_v2.3.0.R): cópia congelada da versão pública `v2.3.0`.

Na publicação da `v2.3.0`, essas cinco cópias públicas do script são mantidas com conteúdo idêntico.

## Finalidade

O fluxo consolida registros exportados do SISMONITORA e produz bases padronizadas, tabelas analíticas, auditorias, estatísticas temporais, relatório textual e gráficos publicáveis.

O script foi desenhado para aceitar tanto dados brutos quanto arquivos já tratados em execuções anteriores, permitindo novas rodadas de tratamento com dados incrementais.

A partir da `v2.2.0`, o script incorpora etapa opcional de validação assistida de `registros_corrig`, voltada a correções controladas, auditáveis e menos sujeitas a problemas comuns de edição manual em planilhas.

A partir da `v2.3.0`, o painel passa a apoiar rotinas de curadoria de COLETAS em lote, incluindo seleção hierárquica por filtros superiores, correção de atributos uniformes por coleta, exclusão auditável de COLETAS e triagem de UAs duplicadas no mesmo ano.

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
- `registros_corrig_stat.csv`: base estatística por COLETA, UC, UA, ano e métricas derivadas.

Em `output/`:

- tabelas de proporção relativa e cobertura vegetal;
- tabelas estatísticas de mudança ano a ano, linha de base e composição geral;
- tabelas estatísticas dos painéis amostrais por ano inicial;
- relatório textual estatístico (`relatorio_textual_estatistico.txt`);
- índice mestre de gráficos (`indice_graficos.csv`);
- gráficos publicáveis seriados em `output/plots_png/`;
- auditorias de símbolos, esforço amostral, layout de rótulos e produtos gráficos;
- relatório de ocorrência de formas de vida exóticas;
- auditorias e dicionários auxiliares do painel em `output/correcoes_campos/`, quando a validação assistida for usada;
- auditorias de COLETAS com UAs duplicadas no mesmo ano;
- arquivos KML, quando habilitados e aplicáveis.

Em `log/`:

- relatório de execução;
- auditorias de arquivos, tipos de entrada, duplicidades, compatibilidade entre fontes, completude e coordenadas;
- auditoria de correções assistidas (`auditoria_correcoes_campos_*.csv`), quando aplicável;
- auditorias pré e pós-correções de COLETAS com UAs duplicadas no mesmo ano;
- relatórios de performance, memória e controle de recursos.

## Principais recursos da v2.3.0

- Correção em lote por múltiplas COLETAS no painel, sem expandir para 101 operações por COLETA.
- Filtros superiores hierárquicos e multisseleção: UC(s) → EA(s) → ano(s) → ciclo(s) → campanha(s) → UA(s) → COLETAS.
- Opção para usar automaticamente todas as COLETAS resultantes dos filtros superiores.
- Prévia auditável por COLETA antes de adicionar operações.
- Bloqueio quando o atributo não é uniforme nas linhas da COLETA ou quando o número de linhas-alvo diverge do esperado.
- Exclusão auditável de COLETAS filtradas/selecionadas, removendo todos os registros da COLETA por operação registrada em `input/correcoes_campos.csv`.
- Filtro de triagem para UAs duplicadas no mesmo ano.
- Alteração da deduplicação para preservar COLETAS distintas com mesma UC + UA + ANO para curadoria manual.
- Trava pós-correções que interrompe a execução quando restarem conflitos UC + UA + ANO com múltiplas COLETAS não resolvidos.
- Auditorias específicas pré e pós-correções de COLETAS com UAs duplicadas no mesmo ano.
- Inclusão do atributo `COLETA` em `registros_corrig_stat`, antes de `UC`, para rastreabilidade da unidade estatística.
- Cache de localização de linhas por COLETA, coleta_uuid, uuid_registro e linha_indice na aplicação das correções.

## Painel de validação - correções assistidas de `registros_corrig`

A abertura do painel é controlada por uma variável no início do script:

    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"

Para abrir o painel, altere para:

    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"

Com `"N"`, o script segue a execução analítica normal. Com `"S"`, o script consolida `registros_corrig`, abre o painel de validação e, após o fechamento do painel, aplica as correções salvas e continua o fluxo.

O painel foi desenhado para reduzir riscos de manipulação direta em Excel ou outros editores de planilha, como alteração de encoding, perda de zeros à esquerda, reinterpretação de datas, conversão indevida de valores decimais e edição acidental de múltiplas colunas.

## Correções e exclusões em lote por COLETA

Na `v2.3.0`, o painel permite selecionar múltiplas COLETAS a partir dos filtros superiores e gerar operações auditáveis de correção ou exclusão.

Para correções de atributos de nível superior, o painel gera uma operação por COLETA por atributo, preservando performance e evitando a criação de 101 operações individuais por COLETA.

Para exclusão de COLETAS, o painel registra operação específica de exclusão em `input/correcoes_campos.csv`. A exclusão é aplicada pelo script, com auditoria linha a linha, e não por remoção manual direta do CSV consolidado.

## Triagem de UAs duplicadas no mesmo ano

A `v2.3.0` preserva para triagem os casos em que COLETAS distintas aparecem com a mesma combinação UC + UA + ANO. Esses casos podem representar erro de seleção da UA real no campo, reamostragem indevida da mesma UA ou outra inconsistência que deve ser resolvida por curadoria.

Quando esses conflitos permanecem após a etapa de correções assistidas, o script interrompe a execução antes da análise estatística e emite mensagem específica no console. O detalhamento também é registrado nos relatórios de auditoria.

## Arquivo de correções assistidas

O painel gera ou atualiza:

    input/correcoes_campos.csv

Esse arquivo registra a receita das alterações, em formato longo, e não substitui diretamente `registros_corrig.csv`. As correções são aplicadas pelo script com auditoria.

O arquivo final corrigido segue sendo:

    output/registros_corrig.csv

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

## Estatística e interpretação

A unidade analítica central é a UA. As análises temporais usam UAs pareadas sempre que a comparação exige controle por unidade amostral.

O script calcula proporções, cobertura vegetal, mudanças entre medições, comparações com linha de base acumulada, intervalos de confiança e testes pareados por permutação, com ajuste de múltiplas comparações quando aplicável.

Resultados com esforço amostral reduzido devem ser interpretados com cautela. O script sinaliza cenários com baixa quantidade de UAs pareadas e preserva auditorias para conferência.

## Estrutura do repositório

- `MONITORA_CAMPSAV_Alvo_Global.R`: script atual recomendado para uso.
- `monitora_campsav_alvo_global.R`: cópia pública equivalente do script atual.
- `R/monitora_campsav_alvo_global.R`: cópia do script atual na estrutura interna do projeto.
- `monitora_campsav_alvo_global_v2.3.0.R`: cópia versionada da versão pública atual.
- `releases/v2.3.0/`: cópia congelada da versão pública `v2.3.0`.
- `CHANGELOG.md`: histórico das versões públicas.
- `docs/uso_de_ia.md`: registro do uso auxiliar de IA.

## Uso auxiliar de IA

O desenvolvimento, revisão e documentação deste projeto podem usar ferramentas de IA como apoio auxiliar para refatoração, revisão textual, geração de comandos, inspeção de logs e proposta de testes.

As decisões metodológicas, validações operacionais, curadoria dos dados, interpretação ecológica e publicação das versões permanecem sob responsabilidade humana do responsável pelo projeto.

## Licença

Consulte [LICENSE](LICENSE).
