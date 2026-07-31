# Guia do usuário — v2.9.0

## Preparação

1. Crie uma pasta de execução vazia.
2. Copie `monitora_campsav_alvo_global_v2.9.0.R` para essa pasta.
3. Crie o subdiretório `input/`.
4. Coloque em `input/` somente os arquivos que devem integrar a execução.
5. Preserve uma cópia independente dos arquivos originais.

O script aceita ZIPs de download direto do SISMONITORA, planilhas e arquivos tabulares reconhecidos. Não é necessário extrair ZIPs manualmente.

No RStudio, confirme a abertura quando o IDE alertar que o script tem cerca de
4 MB. Essa confirmação ocorre ao abrir o documento. Depois disso, use o botão
**Source** normalmente. A v2.9.0 evita o atraso causado por `Source with Echo`
e desativa persistentemente essa preferência quando necessário.

## Execução completa sem painel

Mantenha os padrões publicados:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
```

Se houver pendências impeditivas, `registros_validados.csv` não será criado. Consulte os relatórios detalhados antes de corrigir o input ou executar operações auditáveis.

## Planilha opcional de inclusão no SISMONITORA

O produto `registros_validados_importacao_sismonitora.xlsx` destina-se à
inclusão de registros novos, originalmente coletados em formulário de papel.
Ele só pode ser criado quando `registros_validados.csv` também for gerado e
aprovado na mesma execução.

Para solicitar os dois produtos:

```r
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA <- "S"
```

Cada planilha contém um único contexto de UC, ciclo e campanha e exige
exatamente 101 pontos por coleta. Quando a fonte validada possui mais de um
contexto, o script gera automaticamente um arquivo independente para cada
combinação UC + ciclo + campanha. As colunas `uuid` e
`amostragem/registro/uuid` continuam presentes, mas todas as células de dados
ficam vazias para que o SISMONITORA gere identidades novas. Não preencha esses
UUIDs e não use esse produto para editar registros já existentes.

Os identificadores presentes em `registros_validados.csv` não são alterados e
permanecem rastreados na auditoria. `observacoes_gerais` também permanece na
fonte e na auditoria, mas é omitido da planilha enquanto o importador não
avaliar a função XPath `regex` do XLSForm 21FEV25.

Quando a opção está em `N`, que é o padrão público, o módulo não é
materializado, não lê dados e não carrega dependências adicionais.

## Relatórios analíticos opcionais

Os relatórios analíticos são desligados por padrão. Para uma execução contendo
uma única UC, ative:

```r
MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "S"
MONITORA_FORMATOS_RELATORIOS_ANALITICOS <- c("rmd", "md", "html", "pdf")
```

O script cria versões sintética e detalhada, tabelas e figuras editáveis de
esforço amostral, continuidade, situação dos dados e evidências priorizadas.
Os produtos ficam em `output/08_relatorios_analiticos/`.

Para acrescentar imagem de satélite pública ao mapa de continuidade:

```r
MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"
MONITORA_FONTE_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "SENTINEL2_PUBLICO"
```

Essa fonte consulta imagens Sentinel-2 L2A recentes a partir da extensão do
dataset e não exige conta, chave, token ou faturamento. Depende de internet e
pode criar cache local das janelas orbitais necessárias. O provedor
`GOOGLE_MAPS` continua disponível como alternativa e exige
`MONITORA_GOOGLE_MAPS_API_KEY` configurada no ambiente; a chave não é gravada.

Para gerar PDF, o script procura Chrome, Chromium ou Edge no Windows, Linux e
macOS. Se a autodetecção não localizar um navegador, informe o caminho em
`MONITORA_CAMINHO_NAVEGADOR_PDF` ou pelas variáveis `PAGEDOWN_CHROME`,
`CHROME_BIN` ou `CHROMOTE_CHROME`.

## Execução com correções assistidas

Altere:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
```

No painel:

1. Examine as ocorrências impeditivas e as outras ocorrências para revisão.
2. Selecione o escopo exato da operação: ponto, coleta ou lote de coletas.
3. Adicione a correção à fila.
4. Atualize a prévia integral quando desejar conferir o estado reconciliado.
5. Use **Salvar correções e fechar** para aplicar as operações e prosseguir.

Uma atualização integral já concluída não é repetida no fechamento quando nenhuma nova operação foi adicionada.

## Continuidade incremental

Para continuar uma curadoria anterior:

1. Crie uma nova pasta limpa.
2. Coloque em `input/` um único `registros_corrig*.csv` produzido pelo script.
3. Copie integralmente a pasta de linhagem correspondente para `input/linhagem/`.
4. Use um modo `painel_incremental_*`.
5. Mantenha `MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"`.

Não separe o produto corrigido de sua linhagem e não misture arquivos provenientes de execuções diferentes.

## Replay semântico

O replay reconstrói decisões anteriores sobre uma cópia idêntica do input bruto. Ele não é continuidade incremental.

1. Crie uma pasta limpa.
2. Coloque em `input/` somente os mesmos arquivos brutos usados na run anterior.
3. Copie apenas o ledger para `input/linhagem/correcoes_semanticas_consolidada.csv`.
4. Não inclua `registros_corrig.csv` nem sidecars de continuidade incremental.
5. Escolha `completo`, `sem_png`, `estatisticas_sem_graficos`, `ate_registros_corrig` ou `painel_e_parar`.
6. Ative:

```r
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
MONITORA_OPCAO_REPLAY_DIAGNOSTICO_NAO_ABORTAR <- "N"
```

O script bloqueia replay sem ledger, ledger vazio e modos incompatíveis.

## Comparação com uma run-oráculo

Para verificar se uma nova versão reproduz uma execução já validada:

1. Copie a pasta da run de referência para `input/oraculo_replay/`.
2. Mantenha o replay configurado como descrito acima.
3. Ative:

```r
MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO <- "S"
MONITORA_OPCAO_REPLAY_ORACULO_ABORTAR_DIVERGENCIA <- "S"
```

No console, confirme:

```text
Replay semântico solicitado: SIM
Replay concluído
Gate final do oráculo de replay: convergente_com_oraculo
```

Confira `output/03_auditorias/replay_semantico/oraculo_replay_selo_convergencia_pos_replay_final_reconciliado.csv`. Para aprovação estrita, `replay_equivalente_ao_oraculo` deve ser `SIM`.

O oráculo é somente referência de auditoria: seus valores não são importados para o pipeline.

## Validação espacial guiada

Ative o módulo somente quando a conferência espacial for necessária:

```r
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"
MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "S"
```

Na aba espacial, selecione UC, UA e ano da origem e do destino. As coletas
compatíveis são preenchidas a partir desses filtros, permanecendo disponíveis
para conferência antes de adicionar a operação.

Recomendações determinísticas podem ser sanitizadas em lote. Casos que admitem
mais de uma interpretação permanecem como alertas para decisão do usuário.
Correções espaciais específicas têm precedência sobre sanitizações amplas, seja
qual for a ordem em que forem adicionadas na sessão.

## Produtos KML e KMZ

Quando a exportação espacial estiver habilitada e houver coordenadas válidas, o
script gera três conjuntos:

- `UAs_verg_ini_verg_fin`: feições e metadados espaciais para trabalho de campo,
  sem dados primários da amostragem e com `form_veg`;
- `UAs_registros_corrig_stat`: resultados estatísticos derivados de
  `registros_corrig_stat.csv`, para uso institucional restrito;
- `UAs_areas_operacionais_protecao_100m`: áreas sem preenchimento, com contorno
  amarelo e raio de 100 m a partir do ponto médio do transecto.

O rótulo da área operacional contém somente a UA. A tabela de atributos registra
a coleta, o ano de referência espacial e os demais metadados disponíveis.

## Produtos que devem ser conferidos

- `output/01_produtos_dados/registros_corrig.csv`
- `output/01_produtos_dados/registros_validados.csv`, quando aprovado pelo contrato
- `output/02_painel_correcoes/ocorrencias_diagnosticas/`
- `output/02_painel_correcoes/linhagem/`
- `output/03_auditorias/`
- `output/03_auditorias/replay_semantico/`, quando o oráculo estiver ativo
- `output/04_validacao_espacial/`
- `output/08_relatorios_analiticos/`, quando os relatórios forem habilitados
- `log/`

## Dados sensíveis

Não publique pastas de execução. `input/`, `output/`, `log/` e `extracted/` podem conter dados pessoais, coordenadas, fotos, UUIDs e observações de campo.

## Em caso de bloqueio

Não edite o produto final para contornar gates. Consulte ocorrências diagnósticas, relatórios detalhados, auditorias do contrato, relatórios do replay e o log; corrija a causa no input ou por operação auditável.
