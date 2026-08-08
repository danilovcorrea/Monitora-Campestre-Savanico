# Guia do usuário — v2.9.4

## Preparação

1. Crie uma pasta de execução vazia.
2. Copie `monitora_campsav_alvo_global_v2.9.4.R` para essa pasta.
3. Crie o subdiretório `input/`.
4. Coloque em `input/` somente os arquivos que devem integrar a execução.
5. Preserve uma cópia independente dos arquivos originais.

O script aceita ZIPs de download direto do SISMONITORA, planilhas e arquivos tabulares reconhecidos. Não é necessário extrair ZIPs manualmente.

No RStudio, confirme a abertura quando o IDE alertar que o script tem cerca de
4 MB. Essa confirmação ocorre ao abrir o documento. Depois disso, use o botão
**Source** normalmente. A v2.9.4 é avaliada diretamente no ambiente global e
não encapsula o arquivo inteiro em uma expressão `evalq`; isso elimina a
travessia sintática que podia manter o RStudio sem resposta por vários minutos
antes da primeira mensagem. A preferência `Source with Echo` continua sendo
desativada pela API oficial do RStudio quando necessário. No teste real no
RStudio para Windows, a execução iniciou logo após a confirmação do arquivo
grande, sem o atraso anterior de 4 a 10 minutos.

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
MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "S"
```

Cada planilha contém um único contexto de UC, ciclo e campanha e exige
exatamente 101 pontos por coleta. Quando a fonte validada possui mais de um
contexto, o script gera automaticamente um arquivo independente para cada
combinação UC + ciclo + campanha. As colunas `uuid` e
`amostragem/registro/uuid` continuam sempre presentes no schema.

- Use `MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "S"` para
  esvaziar os UUIDs na planilha e permitir que o SISMONITORA crie identidades
  novas. Este é o padrão público já homologado para inclusão.
- Use `MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA <- "N"` somente
  quando a equipe responsável homologar a atualização de registros existentes
  por UUID. Nesse modo, os valores são preservados na planilha.

A opção de UUID só é lida quando a geração do XLSX está ativa. Em qualquer
modo, `registros_validados.csv` permanece intocado.

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
MONITORA_FORMATOS_RELATORIOS_ANALITICOS <- c("rmd", "md", "html", "docx", "pdf")
```

O script cria versões sintética e detalhada em Rmd, Markdown, HTML, DOCX e PDF,
além de tabelas e figuras editáveis de esforço amostral, continuidade, situação
dos dados, estrutura herbácea/lenhosa, categorias gerais, formas nativas,
exóticas, secas ou mortas, material botânico e evidências priorizadas. A
seleção incorporada aos documentos foi condensada: o sintético prioriza seis
figuras executivas e o detalhado dez figuras analíticas. Todos os gráficos,
testes e CSVs técnicos continuam materializados e auditados nos respectivos
subdiretórios. Os produtos ficam em `output/08_relatorios_analiticos/`.

As séries anuais apresentam média, IC95% e número de UAs. Os símbolos próximos
às médias resumem somente o teste entre UAs pareadas da campanha atual e da
campanha imediatamente anterior:

- `↑`: aumento demonstrado;
- `↓`: redução demonstrada;
- `≈`: estabilidade/equivalência demonstrada dentro da margem configurada;
- `?`: resultado inconclusivo;
- `—`: pares insuficientes;
- primeira campanha: sem símbolo comparativo.

Ausência de significância não equivale a estabilidade. Consulte
`auditoria_simbolos_medias_anuais_relatorio.csv`,
`auditoria_integracao_estatistica_graficos_relatorio.csv` e
`auditoria_robustez_inferencial_relatorio.csv` antes de interpretar mudanças.
Os relatórios separam resultado observado, hipótese compatível, explicações
alternativas e evidência necessária; não atribua causalmente fogo, invasão,
adensamento lenhoso ou desertificação somente a partir da série apresentada.

Quando o módulo de relatórios é ativado, a imagem Sentinel-2 pública já é o
padrão:

```r
MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "S"
MONITORA_FONTE_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "SENTINEL2_PUBLICO"
```

Essa opção somente é avaliada quando os relatórios estão ativos. Portanto,
`MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS <- "N"` não carrega dependências,
não consulta o catálogo e não executa processamento cartográfico específico.
Use `MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE <- "N"` para solicitar
explicitamente o mapa vetorial local. Quando o Sentinel é gerado, o mapa
vetorial equivalente não é repetido no relatório.

Essa fonte consulta imagens Sentinel-2 L2A recentes a partir da extensão do
dataset e não exige conta, chave, token ou faturamento. O mapa usa o leiaute
quadrado aprovado, com a área cartográfica na porção superior e uma faixa
inferior contendo localizador, legenda, informações do mapa e marcas Monitora,
CBC e ICMBio. Moldura, coordenadas, grade, norte geográfico, escalas gráfica e
numérica, projeção, período, fontes, processamento e limitações são registrados.

O limite oficial da UC, os estados e o bioma são obtidos dinamicamente para o
localizador. Os arquivos baixados são temporários, materializados em memória e
removidos ao fim da consulta; o script não incorpora referência espacial de
nenhuma UC. O cache persistente contém somente as janelas orbitais necessárias.
O provedor
`GOOGLE_MAPS` continua disponível como alternativa e exige
`MONITORA_GOOGLE_MAPS_API_KEY` configurada no ambiente; a chave não é gravada.

Para gerar os documentos, o script também resolve o Pandoc distribuído pelo
RStudio ou pelo Quarto no Windows, Linux e macOS. Para PDF, procura Chrome,
Chromium ou Edge. Se a autodetecção não localizar um navegador, informe o caminho em
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
2. Para atributos superiores, selecione a COLETA. O contrato propaga a ação a
   todas as linhas efetivamente observadas dessa COLETA.
3. Em listas de seleção múltipla, escolha adicionar, remover ou substituir um
   token. Em campos textuais, escolha substituir, acrescentar ou limpar.
4. Selecione o escopo exato das demais operações: ponto, coleta ou lote.
5. Adicione a correção à fila e atualize a prévia integral quando desejar
   conferir o estado reconciliado.
6. Use **Salvar correções e fechar** para aplicar as operações e prosseguir.

### Equipe da COLETA

Coletores são um repeat, não um atributo a ser replicado nas 101 linhas. Use o
controle **Equipe da COLETA**: cada integrante ocupa uma linha e o CPF
correspondente, quando informado, fica na mesma posição. Nome é obrigatório;
CPF é opcional. Para excluir uma pessoa, selecione somente a linha desse
integrante. Formatos históricos reconhecidos são saneados automaticamente antes
do painel; CPF ambíguo, parcial ou sem associação inequívoca é removido sem
inferência. Confira `auditoria_sanitizacao_coletores.csv`.

### Impactos de manejo e uso

`impact_manejo_uso` é a pergunta superior Sim/Não.
`tipos_impacto_manejo_uso` é a lista condicional **Quais?**. O painel exibe
`label — name`, mas grava os names do contrato único. Com o pai em Sim, adicione,
remova ou substitua quantos tokens forem necessários. Ao mudar para Não, os
tipos e descritores filhos incompatíveis são limpos de forma auditável.

Movimentos entre formas de vida incluem os descritores dependentes. A operação
só é aplicada se a lista, o hábito, as espécies e os campos exclusivos puderem
ser transferidos integralmente; conflitos bloqueiam antes de qualquer escrita.

A aba **Justificar pendências** permite registrar uma explicação para
ocorrências que permaneçam ao fim da revisão. Filtre e selecione várias linhas
para aplicar a mesma classificação e justificativa em lote; cada ocorrência
mantém ID próprio e o conjunto recebe um identificador de lote. Pendências e
alertas espaciais remanescentes também são listados. O catálogo é recalculado
após correções espaciais para retirar ocorrências efetivamente resolvidas. Cada
evento recebe responsável, classificação e timestamp. A justificativa é
auditável, mas não corrige o dado, não altera coordenadas, não encerra a
ocorrência por decisão textual e não libera uma pendência impeditiva.

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

Na aba espacial, use a seção única **Correção espacial: origem → destino →
operação** e escolha entre uma ou mais COLETAS ou um lote entre ANOS. Se os
filtros de destino resolverem uma única COLETA, ela será preenchida
automaticamente. A prévia exibe somente as coordenadas que a operação realmente
altera. O botão **Limpar filtros** apaga filtros gerais e espaciais, COLETA e
coordenadas preenchidas, sem remover operações já adicionadas à fila.

O número esperado de linhas não é solicitado nesse módulo: a abrangência usa
todas as linhas efetivamente observadas de cada COLETA.

Recomendações determinísticas podem ser sanitizadas em lote. Casos que admitem
mais de uma interpretação permanecem como alertas para decisão do usuário.
Correções espaciais específicas têm precedência sobre sanitizações amplas, seja
qual for a ordem em que forem adicionadas na sessão.

## Mudança de formação vegetacional

O script relata como revisão não impeditiva quando a formação vegetacional da
mesma UA varia dentro da COLETA, entre coletas do mesmo ano ou entre anos. Esse
diagnóstico não altera automaticamente a classificação: mudanças ecológicas
legítimas devem ser preservadas, enquanto erros de determinação podem ser
corrigidos no painel. Consulte o relatório específico em
`output/02_painel_correcoes/ocorrencias_diagnosticas/`.

## Vegetação seca ou morta

Ocorrências de vegetação seca ou morta são registradas como revisão não
impeditiva por linha e forma de vida, com resumo por UC, esforço amostral, UA,
ano e COLETA. O diagnóstico pode subsidiar hipóteses de fenologia, seca, fogo,
herbivoria ou outros processos, mas não identifica causalidade isoladamente.
Confira os relatórios próprios em
`output/02_painel_correcoes/ocorrencias_diagnosticas/` e a integração nos
relatórios analíticos.

## Manual detalhado gerado pelo script

Quando a geração do manual está ativa, o script materializa antes do painel um
manual HTML e a fonte Rmd em `docs/`, além do PDF quando as dependências estão
disponíveis. Esse manual descreve todas as variáveis iniciais, os 13 modos, os
produtos, a continuidade, o replay, os controles do painel, sanitizações,
relatórios e o roteiro operacional. Consulte-o durante a curadoria; não edite as
tabelas auxiliares geradas como se fossem dados de entrada.

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
