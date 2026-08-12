# Monitora Campestre-Savânico — Alvo Global

Rotinas em R para tratamento, auditoria, validação e análise de dados do **Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas** do **Componente Campestre Savânico** do Programa Monitora.

## Versão pública atual

- Versão: `v2.9.7`
- Script principal: [`monitora_campsav_alvo_global_v2.9.7.R`](monitora_campsav_alvo_global_v2.9.7.R)
- Script canônico: [`monitora_campsav_alvo_global.R`](monitora_campsav_alvo_global.R)
- Cópia congelada: [`releases/v2.9.7/`](releases/v2.9.7/)
- Notas da versão: [`RELEASE_NOTES_v2.9.7.md`](RELEASE_NOTES_v2.9.7.md)
- Release no GitHub: [v2.9.7](https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.9.7)

## Finalidade

O script lê diferentes exportações do SISMONITORA, reconhece estruturas históricas de XLSForm, harmoniza atributos, audita a importação, deduplica registros, aplica sanitizações e correções auditáveis, valida o produto corrigido e gera bases, relatórios, estatísticas, gráficos e produtos espaciais.

A validação operacional de `registros_corrig.csv` usa o contrato consolidado dos XLSForms 2022, 2023, 2024 e 2025, com projeção final conforme o XLSForm 2025 e o template SISMONITORA. `registros_validados.csv` somente é materializado quando não restam pendências impeditivas.

## Destaques da v2.9.7

- A linhagem passa a preservar cumulativamente os metadados de todas as sessões
  em `metadados_sessoes_painel_consolidado.csv`, assinado no manifesto.
- A seção 8.1 do relatório de validação apresenta a cronologia completa, com
  responsável, instituição, modo, encerramento e contagens de cada execução.
- Cadeias anteriores à v2.9.7 admitem recuperação técnica única a partir dos
  sidecars canônicos; runs de erro e ramificações paralelas são excluídas e
  auditadas pela ancestralidade entre revisões.
- Após a primeira continuidade, a recuperação transitória deixa de ser
  necessária: basta copiar `registros_corrig.csv` e toda a pasta de linhagem do
  output para o novo input.
- Hash, cardinalidade, conflitos e ausências são verificados de forma
  fail-closed, sem percorrer o dataset completo.
- A inicialização rápida homologada na v2.9.1 permanece intocada e foi novamente
  validada no R nativo do Windows.
- Contrato único, 13 modos, replay, produtos centrais, relatórios compactos,
  Sentinel-2 público, símbolos estatísticos, KML/KMZ e cautela causal das versões
  anteriores permanecem preservados.

## Revise as opções antes de cada execução

Os padrões da v2.9.7 representam a continuidade incremental integral usada pela
equipe de curadoria. Eles ativam painel, validação espacial, produtos de dados,
planilhas SISMONITORA, manual e relatórios analíticos. **Antes de cada execução,
revise o modo e todas as variáveis `MONITORA_OPCAO_*` no bloco inicial.** Desative
os módulos que não correspondem à ação pretendida. Desative produtos que não sejam necessários.
O modo `painel_incremental_completo` exige exatamente um `registros_corrig*.csv`
e a linhagem da mesma execução anterior.

O relatório consolidado materializa `inventario_sessoes_linhagem.csv` e a seção
8.1. O inventário distingue execuções registradas, sessões com decisões,
eventos herdados, reaplicados, novos e acumulados. Na v2.9.7, o manifesto também
assina `metadados_sessoes_painel_consolidado.csv`, que acompanha a linhagem nas
continuações seguintes.

```r
MONITORA_MODO_EXECUCAO <- "painel_incremental_completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- "S"
MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "N"
MONITORA_OPCAO_GERAR_MANUAL_USUARIO <- "S"
MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO <- "S"
MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"
MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "S"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"
MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "S"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
```

Para processar uma entrada bruta sem continuidade anterior, altere pelo menos o
modo para `completo` e decida se o painel deve abrir:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N" # ou "S"
```

Replay, continuidade incremental e execução bruta são fluxos diferentes. Não
combine entradas ou sidecars de linhagem provenientes de cadeias distintas.

## Fluxo e linhagem dos dados

A cadeia auditável é:

```text
input
  → registros_importados_bruto.csv
  → registros_importados.csv
  → registros_importados_operacional_pre_painel.csv
  → registros_corrig.csv
  → registros_validados.csv
```

- `registros_importados_bruto.csv`: retrato técnico da leitura e montagem dos arquivos.
- `registros_importados.csv`: entrada saneada após normalização inicial de cabeçalhos e aliases.
- `registros_importados_operacional_pre_painel.csv`: camada pós-tokenização e pré-painel.
- `registros_corrig.csv`: base canônica corrigida, reconciliada e auditável.
- `registros_validados.csv`: projeção final no schema e na ordem do contrato de destino; só existe após aprovação do gate.

As exclusões deliberadas de coletas e todas as mutações semânticas ficam documentadas. A trilha de linhagem deve acompanhar o produto corrigido em qualquer continuidade incremental.

## Entradas aceitas

Coloque os arquivos exclusivamente em `input/`, na mesma pasta de execução do script. O script reconhece:

- ZIPs de download direto do SISMONITORA, inclusive estruturas internas aninhadas;
- ZIP, CSV, XLSX e XLS de exportações em lote ou planilhas;
- combinações de dados oriundos dos XLSForms 2022, 2023, 2024 e 2025;
- `registros_corrig*.csv` gerado pelo próprio script, somente nos modos incrementais adequados;
- sidecars de linhagem e ledgers semânticos, nos fluxos de replay ou continuidade documentados.

Não extraia ZIPs manualmente e não misture produtos antigos com a entrada bruta. A extração recursiva ocorre em `extracted/`.

## Uso básico

1. Crie uma pasta limpa de execução.
2. Copie `monitora_campsav_alvo_global_v2.9.7.R` para a raiz.
3. Crie `input/` e coloque apenas os arquivos de entrada pertinentes.
4. Ajuste o bloco operacional do início do script, se necessário.
5. Execute no RStudio ou com `Rscript monitora_campsav_alvo_global_v2.9.7.R`.
6. Confira os produtos em `output/`, a documentação em `docs/` e a trilha técnica em `log/`.

## Modos de execução

A v2.9.7 preserva os 13 modos públicos:

- `completo`
- `sem_png`
- `estatisticas_sem_graficos`
- `ate_registros_corrig`
- `painel_e_parar`
- `abrir_painel_cache`
- `painel_incremental_registros_corrig`
- `registros_corrig_estatisticas_sem_graficos`
- `registros_corrig_sem_png`
- `registros_corrig_completo`
- `painel_incremental_estatisticas_sem_graficos`
- `painel_incremental_sem_png`
- `painel_incremental_completo`

Os modos `registros_corrig_*` e `painel_incremental_*` são retomadas controladas. Use somente produtos e linhagem provenientes da mesma execução anterior.

## Painel de correções assistidas

O painel apresenta ocorrências impeditivas e outras ocorrências para revisão, permite correções específicas, sanitizações amplas, movimentos individuais ou em lote e mantém uma fila semântica auditável.

O salvamento gera `registros_corrig.csv`, relatórios pós-painel, trilha de operações e auditorias de persistência. Uma atualização integral já concluída não é repetida no fechamento se nenhuma operação tiver sido adicionada desde então.

## Replay semântico e continuidade incremental

São fluxos diferentes e não devem ser combinados.

### Replay

Parte novamente dos arquivos brutos e reaplica um ledger semântico:

```r
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
MONITORA_REPLAY_CONTRATO_SEMANTICO_VERSAO <- "replay_semantico_v2"
```

Coloque somente uma cópia idêntica dos arquivos brutos e o ledger em `input/linhagem/correcoes_semanticas_consolidada.csv`. Não inclua `registros_corrig.csv` nem sidecars de continuidade incremental. Os modos compatíveis são `completo`, `sem_png`, `estatisticas_sem_graficos`, `ate_registros_corrig` e `painel_e_parar`.

Para confrontar o replay com uma run de referência, copie a pasta dessa execução para `input/oraculo_replay/` e ative:

```r
MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO <- "S"
MONITORA_OPCAO_REPLAY_ORACULO_ABORTAR_DIVERGENCIA <- "S"
```

O oráculo é usado somente na auditoria. Ele nunca substitui o input nem fornece valores ao produto. Oráculo ausente, identidade não única ou qualquer divergência impedem a exportação quando o gate estrito está ativo. Os resultados ficam em `output/03_auditorias/replay_semantico/`.

### Continuidade incremental

Coloque em `input/` um único `registros_corrig*.csv` e copie integralmente a pasta `output/02_painel_correcoes/linhagem/` da mesma execução para `input/linhagem/`. Use um modo `painel_incremental_*` e mantenha o replay em `N`.

Copie a linhagem do **output** da última rodada, não a antiga linhagem que ela
recebeu em `input/`. Não edite manualmente ledger, manifesto ou arquivos de
aplicação; isso rompe a garantia de proveniência.

Em cadeias anteriores à v2.9.7, a recuperação dos metadados históricos é uma
migração técnica única. Depois que o consolidado estiver assinado no manifesto,
as próximas rodadas não repetem a migração e seguem o fluxo incremental normal.

## Produtos principais

- `output/01_produtos_dados/`: produtos de dados, inclusive `registros_corrig.csv`, `registros_validados.csv` e `registros_corrig_stat.csv`.
- `output/02_painel_correcoes/`: operações da sessão, ocorrências diagnósticas, relatórios temáticos e linhagem.
- `output/03_auditorias/`: auditorias de importação, contrato, completude, persistência, replay e relatório consolidado.
- `output/04_validacao_espacial/`: consensos, alertas, correções, mapas e KMLs, quando habilitados.
- `output/05_estatisticas/`: tabelas e relatórios estatísticos.
- `output/06_graficos/`: produtos gráficos.
- `output/08_relatorios_analiticos/`: relatórios sintético e detalhado, tabelas,
  figuras, mapas e auditorias, quando o módulo for habilitado.
- `output/04_validacao_espacial/`: também recebe os KML/KMZ de UAs, estatísticas e áreas operacionais de proteção.
- `docs/`: manual gerado pelo script, quando habilitado.
- `log/`: eventos, performance, memória e rastreabilidade da execução.

## Requisitos de R

O script usa, conforme o modo selecionado, pacotes como `data.table`, `dplyr`,
`purrr`, `stringr`, `readxl`, `openxlsx`, `sf`, `cli`, `ggplot2`, `ggrepel`,
`shiny`, `DT`, `rmarkdown`, `knitr`, `kableExtra`, `pagedown`, `jsonlite`,
`digest` e os consumidores geoespaciais ativados pelo mapa orbital.

O próprio script tenta instalar dependências ausentes. Em ambientes institucionais, sem internet ou sem permissão de instalação, instale previamente os pacotes necessários.

## Variáveis de ambiente úteis

| Variável | Exemplo | Finalidade |
|---|---|---|
| `MONITORA_PERFIL_EXECUCAO` | `economico` | Ajusta estratégia de performance e memória. |
| `MONITORA_BATCH_SIZE_CSV` | `100000` | Define o tamanho dos lotes de leitura. |
| `MONITORA_DT_THREADS` | `4` | Limita threads do `data.table`. |
| `MONITORA_GC_MODO` | `auto` | Controla a coleta de memória. |
| `MONITORA_EXPORTAR_GRAFICOS` | `false` | Controla a exportação de gráficos. |
| `MONITORA_EXPORTAR_KML` | `false` | Controla a exportação de KML. |
| `MONITORA_STAT_BOOT` | `999` | Define reamostragens bootstrap. |
| `MONITORA_STAT_PERM` | `999` | Define permutações estatísticas. |

## Privacidade

Arquivos em `input/`, `output/`, `log/` e `extracted/` podem conter nomes, CPF, coordenadas, fotos, UUIDs, observações de campo, identificadores institucionais e outros dados sensíveis.

Esses diretórios e quaisquer produtos reais de execução não integram a release pública. Antes de compartilhar arquivos gerados, faça triagem de conteúdo e metadados.

## Auditoria antes de publicar

Uma publicação deve confirmar:

- equivalência funcional entre a candidata validada e os scripts públicos, exceto versão, build e defaults declarados;
- parse integral e testes automatizados dos bytes finais;
- identidade byte a byte entre cópias canônicas do script;
- ausência de dados reais, caminhos locais e marcas editoriais internas nos artefatos da versão;
- coerência entre `VERSION`, README, CHANGELOG, release notes, tag e release;
- checksums recriados depois de todas as alterações;
- ZIP montado somente a partir dos artefatos finais permitidos.

## Estrutura do repositório

- `monitora_campsav_alvo_global_v2.9.7.R`: script versionado atual.
- `monitora_campsav_alvo_global.R`: script canônico atual.
- `R_monitora_campsav_alvo_global.R` e `R/monitora_campsav_alvo_global.R`: espelhos canônicos.
- `VERSION`: versão pública atual.
- `CHANGELOG.md`: histórico público de mudanças.
- `RELEASE_NOTES_v2.9.7.md`: notas da versão atual.
- `GUIA_USUARIO_v2.9.7.md`: roteiro operacional resumido.
- `release_assets/v2.9.7/`: conjunto mínimo de artefatos da release.
- `releases/v2.9.7/`: cópia congelada da versão.
- `docs/`: políticas e documentação auxiliar.

## Uso auxiliar de IA generativa

Este projeto teve uma fase inicial de desenvolvimento baseada em edição manual, consulta a documentação técnica, exemplos de código e fontes abertas de referência, incluindo fóruns técnicos e materiais de apoio.

A partir da fase de consolidação publicada como `v2.0.0`, o desenvolvimento passou a contar com apoio de ferramentas de IA generativa para revisão editorial, refatoração, documentação, apoio à depuração, organização do versionamento público, análise de logs, padronização de nomenclatura e proposição de testes.

O uso de IA teve caráter auxiliar. As decisões metodológicas, critérios ecológicos, validações, testes, interpretação dos resultados e responsabilidade técnica pelo script permanecem sob responsabilidade do autor.

Ferramentas de IA não substituem a validação humana, a execução local do script, a inspeção dos produtos, a comparação de hashes, a auditoria dos logs e a responsabilidade técnica sobre a publicação.

## Como citar

```text
CORRÊA, Danilo V. Monitora Campestre-Savânico — Alvo Global. Versão v2.9.7. GitHub, 2026. Disponível em: https://github.com/danilovcorrea/Monitora-Campestre-Savanico/releases/tag/v2.9.7. Acesso em: DD mês AAAA.
```

Repositório público: https://github.com/danilovcorrea/Monitora-Campestre-Savanico

## Licença

Consulte [`LICENSE`](LICENSE).
