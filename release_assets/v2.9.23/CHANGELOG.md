# Changelog

## v2.9.23 - 2026-09-01

### Representação contratual de seleções únicas

- Labels exatos e unívocos de `select_one` já materializados no dataset são
  resolvidos para o name correspondente somente na cópia usada pela validação.
- O valor original permanece preservado e `select_multiple` continua estrito,
  sem alias, mapa local ou flexibilização de domínio.

### Identidade das ocorrências

- Tokens fora do domínio passam a ser discriminados por caminho contratual,
  lista e token inválido na identidade diagnóstica e persistente.
- Ocorrências diferentes da mesma linha deixam de colidir ou se duplicar na
  materialização dos relatórios.
- XLSForm 2025, contrato único, inicialização do RStudio e módulos fora do
  escopo permanecem inalterados.

## v2.9.22 - 2026-08-31

### Validação contratual antes da mutação

- Os campos `select_one` e `select_multiple` presentes no dataset são
  verificados contra as `choices` do XLSForm 2025 obtidas do contrato único
  antes da abertura do painel.
- Operações sobre hábitos são bloqueadas quando categoria, forma e campo
  físico não satisfazem a `relevance` contratual da linha-alvo.
- Identificadores estáveis serializados como os textos `NA` ou `NULL` são
  normalizados como ausentes.

### Composição atômica de tokens

- `replace_token` e `append_token` podem compor a mesma célula quando origem,
  destino e token acrescentado são distintos.
- Inclusões sobrepostas continuam classificadas como conflito e bloqueiam a
  transação sem alteração parcial.
- O XLSForm 2025 e o contrato único embutido permanecem inalterados.

## v2.9.21 - 2026-08-28

### Fechamento contratual de espécies exóticas

- Relações `relevance` somente podem acrescentar um token ao campo superior
  quando esse token também pertence às `choices` vigentes do pai.
- O ramo histórico órfão `outros` é separado das 14 formas exóticas válidas e
  nunca é tratado como alias de `outra forma de vida`, `outra` ou do token de
  impactos/manejo com a mesma grafia.
- O preenchimento explícito de uma folha textual de outra espécie exótica
  materializa somente os ancestrais semânticos válidos do contrato 2025 e não
  altera o módulo histórico.

### Roll-forward de checkpoints legados

- `outros` em `forma_vida_exotica` é migrado automaticamente somente quando há
  uma única forma exótica válida já informada, texto não vazio preservável,
  destino sem espécie conflitante e ausência de conflito nos ancestrais.
- A migração é atômica, idempotente e auditada célula a célula; qualquer
  ambiguidade mantém o registro intacto e deixa o gate contratual bloqueá-lo.
- O contrato XLSForm embutido e o arquivo público v2.9.20 permanecem
  inalterados.

### Homologação e recuperação

- RED reproduzido na v2.9.20 e GREEN focal aprovado na candidata.
- Contrato materializado e serializado integralmente idêntico à v2.9.20.
- Execução real em 20.705 registros preservou linhas, colunas e todas as
  células fora da transação; somente 20 células em quatro registros foram
  alteradas e `registros_validados.csv` concluiu com zero bloqueio.
- Gates no Windows R 4.6.0 aprovaram o caso real, CRLF/limite do RStudio e o
  desempenho. A candidata ficou com 4.579.894 bytes em CRLF e margem de
  662.986 bytes sob 5 MiB.

## v2.9.20 - 2026-08-27

### Cartografia Sentinel-2

- Visões internas de COGs deixam de receber extensão ou CRS reatribuídos.
- A seleção via GDAL exige igualdade de bandas, CRS e extensão com o COG
  oficial, além de resolução compatível; qualquer falha retorna ao raster
  principal.
- Caches sem garantia espacial são rejeitados e a auditoria registra a versão
  do pipeline e a validação da fonte.

### Recuperação da fila no painel

- A exclusão de correções pendentes deixa de ser bloqueada por conflitos de
  outras operações da fila.
- A intenção selecionada é removida atomicamente; conflitos restantes continuam
  auditados e impedem a prévia integral ou o salvamento.

### Homologação e preservação

- Tiles, mosaico, janelas independentes, execução real sem cache e teste no
  Windows foram comparados com os COGs oficiais.
- Cinco mapas operacionais foram atualizados sem reprocessar produtos de dados.
- Inicialização rápida preservada byte a byte e Source simulado equivalente à
  v2.9.1; arquivo único permanece abaixo de 5 MB.

## v2.9.19 - 2026-08-27

### Sanitização atômica de hábitos obrigatórios

- Diagnóstico, prévia, aplicação e replay passam a compartilhar a mesma
  elegibilidade definida pelo contrato XLSForm para a ocorrência `SANHAB`.
- Valores inválidos não vazios, inclusive `?`, deixam de ser descartados do
  alvo físico da correção; valores vazios continuam contemplados.
- Valores já válidos permanecem protegidos e a reaplicação da mesma operação
  é idempotente.

### Homologação e preservação

- Teste focal aprovado para valor vazio, valor inválido não vazio, valor
  válido e replay idempotente.
- Execução real em R 4.6 no Windows, no modo `painel_incremental_completo`,
  aprovada com os produtos corrigido, validado e XLSX SISMONITORA coerentes,
  zero pendência impeditiva e linhagem integral preservada.
- O início do script anterior às variáveis manuais e os módulos alheios à
  sanitização de hábitos não foram alterados.

## v2.9.18 - 2026-08-26

### Arquivo único e RStudio no Windows

- Contrato XLSForm embutido convertido para serialização R versão 2, XDR,
  compactada com gzip e restaurada somente em memória, sem dependência local.
- Arquivo principal reduzido de 5.212.182 para 4.465.796 bytes em LF e para
  4.549.339 bytes na simulação CRLF integral, com margem de 693.541 bytes sob
  o limite rígido de 5 MiB do editor do RStudio.
- `.gitattributes` fixa LF nos scripts R e documentos textuais públicos,
  prevenindo o crescimento automático do arquivo em clones no Windows.
- Comentários internos redundantes foram removidos; cabeçalho, instruções
  operacionais, variáveis manuais e divisores de seção permanecem legíveis.

### Homologação e preservação

- Igualdade integral das 416 linhas de campos, 1.349 opções, 465 dependências
  e quatro arquivos do contrato único em relação à v2.9.17.
- Execução real no R 4.6/Windows aprovada em 47.773 linhas: 94,8 s contra
  97,9 s na v2.9.17 e zero divergência em dados semânticos.
- Parse, restauração contratual, fechamento de contexto, relatórios editáveis
  e seis esquemas reais aprovados sem regressão.
- Nenhuma regra de leitura, correção, painel, replay, linhagem, produto,
  estatística, relatório ou cartografia foi alterada.

## v2.9.17 - 2026-08-25

### Relatórios e transparência analítica

- Relatório consolidado de validação também em Markdown renderizado e DOCX
  editável, com layout A4 embutido, hyperlinks, gates OpenXML e proteção contra
  caminhos longos no Word para Windows.
- Achados prioritários passam a identificar ano inicial, número de UAs e
  população do painel comparado, além da direção observada nos painéis
  iniciados após ampliação do esforço.
- Índice de evidências ampliado com `ano_inicial_painel` e
  `populacao_analitica`; redação dos valores de p inferiores a 0,001 corrigida
  sem alteração dos cálculos estatísticos.

### Contrato único e contexto

- Resolvedor físico central para paths, names, labels e aliases contratuais,
  com falha fechada em ambiguidades e cache pela assinatura das colunas.
- Fechamento hierárquico governado pelas dependências transitivas do XLSForm
  21FEV25, sem converter texto livre em token nem reinjetar falsas ocorrências
  de `outra`.
- Triagem de formas secas ou mortas e relatório analítico passam a compartilhar
  a resolução contratual dos atributos de impactos, distinguindo valor vazio,
  atributo ausente e resolução ambígua.

### Homologação e preservação

- Casos reais PNSC, APAI, FNCS e PNCV, matriz sintética, idempotência,
  conflitos e desempenho aprovados.
- Fechamento focal no PNSC real reduzido de 0,437 s para 0,078 s.
- Inicialização rápida, leitura, painel, replay, linhagem, produtos de dados,
  estatísticas, cartografia, modos e módulos alheios ao escopo preservados.

## v2.9.16 - 2026-08-21

### Revisão pública r04

- Caminhos físicos de relatórios analíticos e planilhas SISMONITORA passam a
  ser compactados de modo determinístico somente quando o destino ultrapassa
  os orçamentos conservadores de abertura no Word ou Excel para Windows.
- Nomes editoriais/lógicos, UC, período e contexto permanecem registrados nos
  índices e manifestos; conteúdo, hashes, dados e linhagem não são alterados.
- `indice_produtos.csv` passa a informar comprimento, limite recomendado e
  situação de abertura de cada produto documental no Windows.
- A atualização das rodadas existentes é focal: move os mesmos arquivos,
  confirma seus hashes e atualiza referências, sem recalcular dados,
  estatísticas, mapas ou relatórios.
- Caminhos curtos mantêm exatamente os nomes anteriores. O custo adicional é
  apenas vetorial e restrito aos produtos opcionais ativos.

### Revisão pública r03

- Enquadramento mínimo para 150 ppi na largura final do mapa, impedindo a
  ampliação excessiva de redes amostrais compactas sem aumentar a resolução
  solicitada ao Sentinel-2.
- Gate radiométrico com métricas de brilho, contraste, nitidez e entropia;
  correção local moderada somente quando necessária, sem consultas ou downloads
  adicionais.
- Auditoria cartográfica ampliada com densidade, fator de ampliação, métricas
  pré/pós-ajuste e decisão de qualidade.
- Orçamento cartográfico de 180 segundos e estratégia de aquisição preservados;
  custo nulo quando o mapa está desativado.
- Mapas de EEC, FNC, PNCA, PNGSV e PNM atualizados focalmente; PNCG atualizada
  integralmente; PNCV e PNCF dispensadas de nova execução por já atenderem aos
  gates da revisão.

### Revisão pública r02

- Substituída a contingência final por prévia georreferenciada de baixa
  resolução: o produto cartográfico agora exige COG em cor natural, bandas RGB
  nativas ou cache persistente previamente validado em alta resolução.
- Fonte Sentinel-2 nativa de 10 m, renderização limitada a dois milhões de
  células úteis e leitura explícita do nível interno adequado do COG evitam
  tanto a degradação visual quanto o processamento de pixels descartados.
- Gate final exige 100% de cobertura visual, todas as UAs sobre pixels válidos
  e resolução compatível com a extensão. Dez UCs reais foram aprovadas com
  resolução efetiva de 10,0 m a 92,1 m e nuvens/sombras de 0% a 0,0058%.
- As rodadas completas foram atualizadas focalmente, sem recalcular produtos
  de dados, estatísticas ou linhagem; HTML e DOCX receberam a figura homologada
  e apenas os PDFs foram reimpressos.

### Sentinel-2

- Consulta STAC paginada, janela temporal progressiva e seleção por cobertura,
  nebulosidade local e recência, com limite explícito de tempo.
- Extensão técnica única e um mosaico por aquisição; contingências auditáveis
  por COG, bandas RGB nativas, composição temporal e último cache de alta
  resolução validado da mesma UC.
- O mapa solicitado deixa de ser omitido apenas porque não existe uma cena no
  alvo ideal de qualidade e só é aprovado após materialização do arquivo e da
  auditoria.

### Linhagem

- Inventário completo de sessões importado e verificado nas continuações
  incrementais, com hash e cardinalidade assinados no manifesto final.
- Execuções legadas sem decisões permanecem registradas sem criar eventos
  artificiais; a ordem herdada é preservada e a sessão atual fecha a cronologia.

### Homologação e preservação

- Doze UCs e 159.176 linhas auditadas em recuperações/atualizações reais, sem
  regressão de registros ou linhagem; dez execuções completas com mapas,
  localizadores e relatórios aprovados.
- Continuidade de uma linhagem já assinada comprovada com preservação da ordem,
  acréscimo de sessão e nova assinatura válida.
- Bloco congelado de inicialização rápida, contrato único e módulos alheios ao
  escopo preservados; custo novo nulo quando o mapa está desativado.

## v2.9.15 - 2026-08-20

### Dados, painel e replay

- Schema analítico estabilizado quando um recorte por formação vegetacional é
  vazio, eliminando a regressão observada em bases com uma única formação.
- Conflitos entre triagem, movimento e edição direta da ocorrência
  `desconhecida` passam a ser detectados pelo alvo semântico antes da mutação.
- Replay v2 passa a aceitar `append_text`, `acrescentar_texto` e
  `adicionar_texto` como acréscimo textual idempotente e repara em memória
  ledgers anteriores sem modificar o arquivo de entrada.

### Cartografia e relatórios

- Estados e biomas atualizados pelas fontes oficiais mais recentes do IBGE;
  limite de UC federal mantido no ICMBio/INDE, com fonte, edição, escala,
  atualização, SHA-256 e fallback registrados em auditoria.
- Conversão DOCX corrigida para figuras HTML com atributos opcionais; gate
  OOXML valida imagem, legenda, descritor, relacionamento e contagem.
- Painéis `evidencia_estatistica_*.png` disponíveis são inseridos uma única vez
  nas seções temáticas do relatório detalhado, sem recálculo estatístico.
- Capa do DOCX passa a reproduzir a identidade editorial do PDF/HTML, com
  hierarquia institucional, marcas separadas, metadados completos e primeira
  página sem cabeçalho ou rodapé internos.

### Homologação e preservação

- Matriz real de 13 UCs aprovada; execução completa FNCS com Sentinel e todos
  os formatos concluída; DOCX real com 7/7 figuras no sintético e 46/46 no
  detalhado.
- Capa Word validada por exportação nativa e inspeção visual; segunda página e
  corpo dos relatórios permaneceram inalterados.
- Bloco congelado de inicialização rápida, contrato único, linhagem, produtos
  de dados e custo zero dos módulos opcionais desligados foram preservados.

## v2.9.14 - 2026-08-19

### Incorporação incremental

- Corrigida no Windows a preparação isolada de novas COLETAs em modos
  `painel_incremental_*`, com wrapper R temporário autogerado e sem dependência
  de arquivos externos.
- Busca recursiva ampliada para CSV, XLSX, XLS e ZIP, inclusive subpastas e
  arquivos compactados aninhados.
- Diagnósticos do processo isolado preservados antes da limpeza e bloqueio
  atômico de lotes incompletos ou conflitantes.

### Painel e contrato

- UA passa a aceitar entrada textual validada no intervalo
  `UA-001_VgCS`–`UA-999_VgCS`, sem ficar limitada ao domínio observado.
- Validação equivalente na interface e no servidor; abrangência por todas as
  linhas da COLETA preservada.

### Desempenho e homologação

- Caminho rápido sem listagem, leitura ou subprocesso quando a incorporação
  está desativada.
- Bloco funcional de inicialização do RStudio preservado e início imediato
  confirmado em teste real no RStudio para Windows.
- CSV, XLSX, XLS binário e ZIP aninhado homologados; idempotência, conflitos,
  incompletude e limites de UA cobertos por testes automatizados.

## v2.9.13 - 2026-08-19

### Relatórios e interpretação

- Hipóteses ecológicas condicionadas às evidências da UC; tabelas de estado
  reordenadas; contexto de fogo por COLETAs únicas, subcontextos, total anual e
  percentual.
- Esforço incremental separado em todas as UAs, UAs comuns pareadas e grupos
  por ano de entrada, com painéis específicos dentro de cada tema.
- Figuras próprias dos relatórios produzidas a partir das séries e testes já
  materializados, sem recálculo estatístico.
- Capas com marcas embutidas, separação horizontal de 7,5 mm entre Monitora e
  CBC, títulos ajustados e referências conforme ABNT NBR 6023:2025.

### Estatística e gráficos

- Chave canônica de formação corrigida para os painéis de proporção.
- Paleta semântica e iconografia unificadas entre PNGs técnicos e relatórios.
- Escalas, corredores, rótulos, conectores e símbolos passam a ter disposição
  adaptativa, com auditoria de corte e congestionamento.
- Travessão padronizado para pares insuficientes e omissão do símbolo conjunto
  quando existe apenas uma categoria.
- Gate das categorias gerais confirma as cinco categorias, cores e legenda.

### Sentinel-2

- Cenas selecionadas pela nebulosidade local, com recência como desempate e
  ampliação progressiva limitada da janela e do mosaico.
- Extensão de exibição separada da consulta técnica; lacunas após reprojeção
  acionam nova tentativa sem bloquear os demais produtos.

### Curadoria e autossuficiência

- Nova incorporação opcional, transacional e idempotente de COLETAs omitidas em
  continuidades `painel_incremental_*`, com assinatura na linhagem.
- Persistência de hábitos corrigida no atributo XLSForm físico de cada forma de
  vida, inclusive em bases legadas e multiversão.
- Dependências dos produtos opcionais declaradas no próprio script e avaliadas
  somente quando o módulo correspondente é ativado.

### Homologação

- Execução integral PNCV no R 4.6/Windows: 77.164 linhas, 208 PNGs, planilhas
  SISMONITORA e relatórios sintético/detalhado nos cinco formatos.
- Quinze produtos esperados aprovados; tempo total de 614,496 s e etapa
  analítica de 66,841 s, sem regressão na preparação gráfica comparável.

## v2.9.12 - 2026-08-14

### Cartografia

- O mapa Sentinel passa a representar o limite oficial da UC no mapa principal
  somente quando o contorno intercepta a moldura exibida; limite e item da
  legenda permanecem condicionados à visibilidade efetiva.
- A geometria temporária já obtida para o localizador é reutilizada, sem nova
  consulta, hardcode ou custo quando o produto está desligado.
- Moldura e rótulos de coordenadas passam a usar a mesma largura editorial da
  faixa inferior com localizador, legenda, informações e marcas.
- A prancha passa a 2.800 x 3.200 pixels, com faixa inferior de 21% e margem
  externa comum próxima de 3 mm, sem deformar o raster ou deslocar as UAs.

### Painel

- O botão geral `Limpar filtros` passa a cobrir filtros, campos transitórios,
  buscas e seleções das abas Correções de registros, Equipe da COLETA,
  Validação espacial, Justificar pendências e Auditoria opt-in.
- O responsável, as filas auditáveis, os históricos e as auditorias permanecem
  preservados durante a limpeza.
- A busca global e eventuais buscas por coluna das tabelas são reiniciadas por
  proxy, sem reconstrução dos dados.
- O botão local de limpeza espacial permanece restrito ao módulo espacial.

### Desempenho e preservação

- O contrato declarativo inventaria 74 controles editáveis e bloqueia futuras
  omissões no teste automatizado.
- A limpeza não executa prévia integral, reconciliação, leitura ou escrita de
  arquivos nem varredura dos registros.
- O parse mediano permaneceu estável: 0,700 s na v2.9.12 contra 0,736 s na
  candidata cartográfica anterior.
- A arquitetura de uma expressão externa e o eco de 174 bytes permanecem
  idênticos ao padrão homologado da v2.9.1.
- Contrato único, itens congelados, 13 modos, linhagem e produtos de dados foram
  preservados.

### Homologação

- Teste real no RStudio/Windows confirmou início praticamente imediato pelo
  botão Source e limpeza integral das quatro abas operacionais.
- Duas execuções reais APAI concluíram de forma controlada com 7.878 linhas,
  184 colunas e nenhum bloqueio XLSForm21.
- PNCV, PNCA e APAI mantiveram mapas de 2.800 x 3.200 pixels e 97,82% de
  ocupação da largura externa.

## v2.9.11 - 2026-08-14

### Corrigido

- Relatórios analíticos passam a tratar uma única campanha como linha de base
  transversal, sem inferência temporal indevida.
- O atributo legado `amostragem/especie` recupera o rótulo editorial do
  XLSForm sem alterar seu nome técnico ou os dados.
- Temporários DOCX usam caminho curto no Windows; a publicação final permanece
  atômica e os nomes editoriais dos documentos são preservados.
- A persistência de justificativas e checkpoints recebe candidatos validados
  por esquema, tamanho e hash, retentativas curtas para bloqueios transitórios
  e rollback integral em falha.

### Cartografia e Sentinel-2

- As consultas de UC, estados e biomas são independentes, auditáveis e
  resilientes; o localizador preserva as camadas disponíveis diante de falha
  parcial.
- A associação do limite da UC utiliza a extensão da rede e confirmação nominal
  normalizada, sem hardcode de unidade de conservação.
- A busca Sentinel-2 amplia progressivamente a janela temporal de 60 para 120,
  180, 365, 730, 1.460 e 2.920 dias e, se necessário, até o início da missão.
- Data da aquisição, defasagem, janela consultada, cobertura e nuvens/sombras
  são registradas no mapa e nas auditorias.
- O gate analítico passa a exigir solicitação e geração efetivas do Sentinel,
  além de localizador completo, quando a opção está ativada.

### Desempenho e preservação

- A prévia integral reutiliza o contrato de edição pré-calculado durante a
  transação, sem mudar regras ou resultados.
- Em fila real de 150 operações da PNE, a repetição final levou 18,760 s,
  contra 283,375 s na v2.9.10.
- A ampliação Sentinel só ocorre quando a janela mais recente é insuficiente;
  com a opção desligada, o módulo retorna antes de qualquer consulta remota.
- O bloco de inicialização do Source permanece idêntico ao da v2.9.10, exceto
  versão e build. Contrato único, itens congelados, linhagem e produtos de dados
  foram preservados.

### Homologação

- Teste real pelo botão Source no RStudio/Windows confirmou início imediato.
- PNE concluiu execução integral no Windows com produtos de dados, XLSX do
  SISMONITORA, relatório de validação e relatórios analíticos completos.
- Dez UCs geraram relatórios sintético e detalhado em Rmd, Markdown, HTML, DOCX
  e PDF, com mapas Sentinel e localizadores completos.
- A APAI encontrou aquisição adequada ao ampliar a consulta para 120 dias; as
  outras nove UCs foram atendidas na janela inicial de 60 dias.

## v2.9.10 - 2026-08-13

### Corrigido

- Figuras dos relatórios analíticos passam a receber nomes compactos,
  determinísticos e sem colisões somente quando o caminho completo excede o
  limite seguro do dispositivo gráfico no Windows.
- Índices, fontes editáveis e renderizadores DOCX, HTML e PDF passam a usar o
  nome efetivamente materializado; referências fixas ao gráfico de esforço
  amostral foram removidas.

### Desempenho e preservação

- A resolução do nome ocorre uma vez por figura e não percorre os dados.
- Antes dos relatórios, a contraprova EEC variou de 55,322 s para 56,321 s
  (+1,81%), dentro da variação operacional; o tempo adicional posterior
  corresponde à geração efetiva dos PDFs que anteriormente falhava.
- O bloco de inicialização do Source é byte a byte idêntico ao da v2.9.9.
- Contrato único, itens congelados, painel, linhagem, produtos de dados e
  comportamento em caminhos normais foram preservados.

### Homologação

- Run real no R 4.6 do Windows em diretório ainda mais longo que o incidente:
  139,987 s, código 0 e todos os produtos obrigatórios com status `ok`.
- Foram incorporados 44 resultados estatísticos; DOCX, HTML e os dois PDFs
  foram concluídos e validados.

## v2.9.9 - 2026-08-13

### Corrigido

- A prévia atômica de lotes de atributos superiores preserva a precondição
  bruta específica de cada COLETA, inclusive em operações de listas de tokens.
- A verificação das precondições antecede movimentos volumosos e impede que uma
  fila parcialmente aplicável modifique a base.
- Exceções recuperáveis da prévia são contidas pelo estado reativo: o painel
  permanece aberto e oferece diagnóstico, atualização e salvamento controlado.
- Filas idênticas já rejeitadas não repetem o processamento custoso.

### Documentação e organização

- Relatório de validação reposicionado em `output/07_relatorio_validacao/`, com
  resumo executivo, encadeamento coeso, linguagem acessível, tabelas resumidas,
  hiperlinks e layout profissional.
- Manual reorganizado em `manual_usuario/`, com os 13 modos, percursos de uso,
  listas de conferência, glossário e layout profissional.
- Nova opção `MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF`, com padrão `N`; o PDF
  é renderizado somente após o fluxo principal e não bloqueia dados.
- Índice, README do output, classificador e organizador final reconhecem os
  novos diretórios e preservam produtos legados em caso de colisão.

### Desempenho e preservação

- Execução integral real de RBG no R 4.6 do Windows: 799,578 s, variação de
  1,25% em relação à referência de 789,690 s.
- Etapa analítica: 72,081 s; relatório de validação terminal: 8,995 s.
- A seção anterior às variáveis manuais, responsável pelo comportamento do
  botão Source, é idêntica à v2.9.8, exceto versão e build.
- Contrato único, itens congelados, 13 modos, linhagem, produtos de dados,
  SISMONITORA, estatísticas, Sentinel-2 e compatibilidade multiplataforma foram
  preservados.

### Homologação

- Checkpoint real de RBG recuperado: 27 ações auditáveis, 13 COLETAS e
  aplicação integral das decisões à base e à linhagem.
- Todos os produtos obrigatórios foram materializados; cinco XLSX do
  SISMONITORA, 144 PNGs e relatórios em Rmd, Markdown, HTML, DOCX e PDF.
- Gates de parse, expressão externa única, persistência, atomicidade,
  documentação, produtos finais e inicialização comparativa aprovados.

## v2.9.8 - 2026-08-12

### Corrigido

- A primeira falha de precondição interrompe imediatamente o plano atômico,
  evitando processamento ou finalização subsequente sobre uma prévia inválida.
- A falha recuperável informa e audita operação, COLETA, atributo, status e
  causa; tentativas idênticas deixam de recalcular a mesma fila.
- Lotes de atributos superiores passam a separar o valor amigável de exibição
  do valor bruto exato usado na precondição transacional.
- O relatório de validação deixa de chamar `data.table::unique`, que não é uma
  função exportada, e usa o genérico correto do R.

### Adicionado

- Checkpoint integral e atômico com correções de campos, operações espaciais,
  justificativas, histórico de intenções e auditoria da falha.
- Restauração automática somente sobre a mesma base, validada por impressão
  digital e hash das filas.
- Migração restrita de checkpoints afetados pela candidata r01 quando a
  divergência comprovada se limita a espaços nas extremidades, com auditoria.

### Desempenho

- Migração da run19: 0,016 s.
- Prévia real do lote de 26 COLETAS: cerca de 6,96 ms por avaliação; incremento
  de aproximadamente 0,78 ms em relação à candidata anterior.
- Fila repetidamente inválida é reconhecida por assinatura e não é reavaliada.
- Inicialização de uma única expressão externa permanece inalterada.

### Preservado

- Baseline v2.9.7, contrato único, itens congelados, 13 modos, painel, replay,
  linhagem, produtos de dados, SISMONITORA, estatísticas, relatórios analíticos,
  Sentinel-2, cartografia, KML/KMZ e cautela causal.

### Validação

- PNCV run19: 77.164 linhas, 28 operações semânticas, 53 itens auditáveis,
  oito precondições reidratadas e 53 de 53 precondições exatas.
- Divergência material simulada bloqueada; checkpoint, filas e dados originais
  preservados.
- Gates de parse em Linux e Windows, inicialização, motor com 30 operações e
  checkpoint na escala de 3.583 justificativas aprovados.

## v2.9.7 - 2026-08-12

### Corrigido

- A seção 8.1 do relatório de validação deixa de preencher somente a sessão
  atual: todas as sessões históricas com sidecar persistido passam a conservar
  responsável, instituição, modo, encerramento e contagens.
- Metadados de uma execução sem decisões novas deixam de desaparecer do
  inventário apenas porque ela não originou um evento semântico.

### Adicionado

- Ledger cumulativo `metadados_sessoes_painel_consolidado.csv`, com schema
  explícito, identidade por `exec_id`, hash e cardinalidade no manifesto.
- Migração histórica única baseada na cadeia inequívoca de `revision_id` e
  `parent_revision_id`, com exclusão auditada de runs não ancestrais.
- Controles de integridade para quantidade de sessões, execuções sem metadados
  persistidos e coincidência do hash do manifesto com o arquivo físico.
- Auditoria da recuperação e testes de segunda geração, conflito e adulteração.

### Desempenho

- Leitura, combinação e inventário são proporcionais ao número de sessões e
  não percorrem `registros_corrig.csv`.
- Homologação nativa no Windows: 0,03 s em FNCS e 0,02 s em APAI para o
  inventário de sessões.
- A estrutura externa de inicialização da v2.9.1 permanece byte a byte
  equivalente, excetuando versão e build.

### Preservado

- Baseline v2.9.6, contrato único, itens congelados, 13 modos, painel, replay,
  produtos de dados, SISMONITORA, estatísticas, relatórios analíticos,
  Sentinel-2, cartografia, KML/KMZ e cautela causal.

### Validação

- FNCS: cinco sessões canônicas recuperadas; três ramificações paralelas
  excluídas e auditadas.
- APAI: quatro sessões canônicas recuperadas; uma run de erro e uma ramificação
  paralela excluídas e auditadas.
- Segunda geração aprovada sem artefatos de migração; adulterações do arquivo
  transitório e do consolidado permanente foram bloqueadas.
- Gates de regressão da v2.9.6, parse em Linux e R nativo no Windows aprovados.

## v2.9.6 - 2026-08-12

### Corrigido

- Lotes sucessivos e disjuntos de justificativas permanecem selecionáveis e
  podem ser aplicados sem que a seleção anterior restrinja indevidamente o
  catálogo atual.
- A fila de justificativas é reconciliada com a fonte canônica de ocorrências
  após correções, preservando eventos ainda válidos e registrando resoluções.
- Inclusão, exclusão, prévia e fechamento das justificativas passam pelo mesmo
  gate transacional, sem materialização parcial em caso de falha recuperável.
- Uma falha do XLSX opcional de importação SISMONITORA não encerra a execução
  nem impede produtos independentes; o arquivo afetado continua auditado como
  não concluído.
- A renderização PDF dos relatórios analíticos ocorre em processo R isolado,
  evitando mensagens `handle_read_frame`/`asio.system` no console principal.

### Adicionado

- Inventário explícito de execuções e sessões da linhagem no relatório de
  validação e em CSV editável, com eventos herdados, reaplicados, novos e
  acumulados conciliados por identidade.
- Progresso no console e no log de performance para catálogo Sentinel,
  aquisição, mosaico RGB, localizador, composição cartográfica, conteúdo,
  DOCX, HTML e PDF.
- Auditorias específicas do renderizador PDF isolado e do resultado dos
  produtos opcionais.

### Alterado

- Os padrões operacionais passam ao fluxo `painel_incremental_completo`, com
  painel, validação espacial, produtos de dados, XLSX, manual e relatórios
  analíticos ativos; UUID é preservado por padrão no XLSX. O README alerta para
  revisar todas as opções antes de cada execução.

### Desempenho

- A arquitetura externa homologada na v2.9.1 permanece intocada. O teste real
  no botão **Source** do RStudio para Windows iniciou a primeira expressão em
  0,743 s.
- Reconciliação de justificativas atua apenas sobre índices e filas já
  materializados; não adiciona varredura do dataset completo.
- Subetapas analíticas são informativas e não são somadas duas vezes ao tempo
  acumulado da execução.

### Preservado

- Contrato único, itens congelados, 13 modos, precedência, replay, produtos
  centrais, relatórios compactos, estatística, Sentinel-2 público, cartografia,
  KML/KMZ e cautela causal da v2.9.5.

### Validação

- Homologação real PNCV aprovada no RStudio/Windows com todos os produtos
  obrigatórios, 174 PNGs, cinco XLSX SISMONITORA e dez documentos analíticos.
- Duração total de 866,881 s; etapa analítica de 291,047 s; PDFs isolados
  concluídos sem mensagens WebSocket no console.
- Gates aprovados em Windows e Linux; resolutores e caminhos auditados para
  macOS, sem disponibilidade de equipamento físico macOS nesta homologação.

## v2.9.5 - 2026-08-11

### Corrigido

- Hotfix editorial `v2.9.5-20260811.2`: o resultado estatístico inconclusivo
  passa de `?` para `·` nos gráficos e relatórios analíticos, padronizado com os
  PNGs técnicos, sem alteração de cálculos ou classes estatísticas.
- A inicialização pelo botão **Source** do RStudio para Windows volta à
  arquitetura homologada na v2.9.1: uma única expressão externa `base::evalq`,
  sem releitura, segundo parse, consulta ao editor ou arquivo auxiliar.
- A tabela de justificativas passa a exibir somente as pendências filtradas e a
  selecionar integralmente o conjunto visível quando solicitado.
- Justificativas adicionadas na sessão podem ser selecionadas e excluídas antes
  do salvamento; inclusão e exclusão em lote são atômicas.
- A movimentação assistida de exótica para nativa preserva hábito e descritores
  compatíveis e evita a permanência parcial da ramificação exótica.

### Adicionado

- Relatório operacional de vegetação seca ou morta orientado à identificação de
  possíveis falsos positivos, com COLETA, UA, ano, forma de vida, grupo
  herbáceo/lenhoso, trajetória temporal e contexto de impactos e manejo.
- Classificações explícitas de triagem, sem inferência causal nem correção
  automática do dado.
- Seleção individual, múltipla ou total das justificativas da sessão para
  exclusão auditável.

### Revisado

- Ordem das abas restaurada para Correções de registros, Equipe da COLETA,
  Validação espacial e Justificar pendências.
- Resumos e trajetórias de vegetação seca ou morta passam a exibir a COLETA
  sempre que operacionalmente cabível.
- Localizador oficial usa consulta temporária ICMBio/INDE e referências do IBGE,
  sem hardcode de UC ou persistência do arquivo oficial.
- Símbolos das médias temporais permanecem vinculados aos testes pareados e às
  auditorias de origem, distinguindo estabilidade demonstrada de ausência de
  evidência.

### Desempenho

- Estrutura externa e volume mínimo de eco equivalentes à v2.9.1; início real
  imediato confirmado pelo botão **Source** no RStudio para Windows.
- Operações de justificativas são vetorizadas e módulos opcionais continuam sem
  custo próprio quando desligados.
- A mediana do núcleo de seca/morta no corpus PNCV caiu de 0,647 s para 0,518 s
  entre as revisões controladas.

### Preservado

- Baseline v2.9.4, contrato único, 13 modos, defaults públicos, precedência,
  linhagem, replay, produtos centrais, XLSX SISMONITORA, estatísticas, gráficos,
  KML/KMZ, cartografia institucional, relatórios compactos e cautela causal.

### Validação

- Homologação real PNCV aprovada com nove produtos obrigatórios, 174 de 174 PNGs
  e duração total de 627,808 s.
- Gates aprovados para inicialização, contrato de release, integridade atômica,
  triagem real PNM/PNCV, localizador on-line, coletores, impactos, estatística e
  importação SISMONITORA.

## v2.9.4 - 2026-08-08

### Revisado

- Os relatórios analíticos passam a usar uma seleção executiva de figuras e
  tabelas: na homologação FNCS, o sintético foi reduzido de 22 para 9 páginas e
  o detalhado de 36 para 16 páginas.
- O sintético incorpora seis figuras prioritárias e o detalhado dez, enquanto
  os 104 PNGs, CSVs e resultados estatísticos permanecem completos nos produtos
  técnicos.
- Quando o mapa Sentinel é concluído, o mapa vetorial equivalente deixa de ser
  repetido no documento.

### Alterado

- O Sentinel-2 público passa a ser o fundo padrão quando o módulo opcional de
  relatórios analíticos for ativado.
- A opção orbital somente é avaliada com os relatórios ativos; o caminho em `N`
  não carrega dependências, consulta catálogos nem executa processamento
  cartográfico específico.
- O script volta a ser avaliado diretamente no ambiente global, eliminando o
  encapsulamento `evalq` que podia atrasar por vários minutos o início pelo botão
  **Source** do RStudio para Windows.
- A renderização documental passa a resolver também o Pandoc distribuído pelo
  RStudio ou pelo Quarto no Windows, Linux e macOS.

### Desempenho

- Em comparação controlada no Windows, a v2.9.4 ficou 0,7% acima da v2.9.3 no
  tempo real e 1,2% no tempo instrumentado, sem regressão material.
- O teste real do botão **Source** iniciou a execução após a confirmação do
  arquivo grande, sem a espera anterior de 4 a 10 minutos.

### Preservado

- Baseline v2.9.3, contrato único, 13 modos, painel, precedência, linhagem,
  replay, produtos de dados, planilhas SISMONITORA, estatísticas, gráficos,
  KML/KMZ, cartografia institucional e cautela causal.

### Validação

- Homologação integral FNCS 2023–2026 aprovada com 16.766 pontos, 58 UAs,
  quatro XLSX, 104 PNGs e relatórios Rmd, Markdown, HTML, DOCX e PDF.
- Mapa Sentinel-2 gerado com 100% de cobertura da extensão; auditoria final de
  produtos e auditoria de renderização sem falhas.
- As 25 páginas finais em PDF foram renderizadas e inspecionadas integralmente.

## v2.9.3 - 2026-08-07

### Corrigido

- A edição de coletores deixa de tratar o nome como atributo superior replicado:
  cada integrante é editado ou excluído individualmente no repeat da COLETA, com
  CPF opcional e associação posicional preservada.
- `impact_manejo_uso` e `tipos_impacto_manejo_uso` passam a apresentar labels e
  names do contrato, respeitar a dependência Sim/Não e oferecer operações por
  token para a lista condicional.
- Justificativas podem ser aplicadas em lote e incluem pendências e alertas
  espaciais remanescentes com identidade estável e atualização pós-correção.

### Adicionado

- Sanitização automática e auditável de formatos históricos de coletores; CPF
  ambíguo, parcial ou sem associação inequívoca é descartado sem inferência.
- Diagnóstico não impeditivo de vegetação seca ou morta, com ocorrência por
  linha e forma de vida, resumo temático e integração ao relatório de validação.
- Manual operacional detalhado gerado pelo próprio script antes do painel, com
  modos, produtos, controles, replay, continuidade, sanitizações e auditorias.
- Núcleo ecológico dos relatórios analíticos para estrutura herbácea/lenhosa,
  categorias gerais, formas nativas, exóticas, secas ou mortas e material
  botânico em decomposição.
- Integração dos testes pareados, IC95%, FDR-BH, equivalência, linha de base e
  mudança composicional aos gráficos e às auditorias dos relatórios.

### Revisado

- Médias anuais recebem símbolos `↑`, `↓`, `≈`, `?` e `—`, derivados somente
  dos testes já materializados e explicados nas legendas.
- Achados são agrupados por métrica e formação; hipóteses, explicações
  alternativas e evidência necessária permanecem separadas de nexo causal.
- Tabelas visuais omitem contexto constante e mesclam somente células
  contextuais elegíveis; formação vegetacional, resultados numéricos e CSVs
  editáveis permanecem intocados.
- Parágrafos, títulos, notas e figuras usam alinhamento editorial consistente em
  Rmd, Markdown, HTML, DOCX e PDF.

### Preservado

- Baseline v2.9.2, contrato único, 13 modos, defaults, precedência, linhagem,
  replay, produtos centrais, XLSX SISMONITORA, estatísticas preexistentes,
  gráficos, KML/KMZ e cartografia homologada.
- Relatórios analíticos continuam opcionais e retornam antes de dependências,
  I/O ou materialização própria quando configurados como `N`.

### Validação

- Parse e testes de justificativas espaciais, integração estatística, coletores,
  impactos, seca/morta, painel RBG e XLSX SISMONITORA aprovados.
- Corpus histórico aprovado sobre 232 CSVs, dos quais 217 continham campos de
  coletores e 29.595 linhas estavam em formatos legados.
- Homologação FNCS 2023–2026 aprovada com 16.766 pontos, 58 UAs, duas formações,
  mapa Sentinel-2 e dez documentos analíticos sem falha.
- Inspeção visual integral de 58 páginas PDF e 56 páginas DOCX sem cortes,
  sobreposições, títulos órfãos ou regressões cartográficas.

## v2.9.2 - 2026-08-07

### Corrigido

- Atributos superiores passam a usar o escopo real da COLETA em todas as
  linhas observadas; listas de seleção múltipla recebem ações por token e
  textos podem ser acrescentados sem apagar o conteúdo existente.
- Movimentos entre formas de vida validam toda a subárvore antes da escrita e
  transferem atomicamente listas, hábito, espécies e descritores, evitando
  estados parciais.
- A limpeza dos filtros remove também COLETA, filtros espaciais e coordenadas
  preenchidas; a prévia espacial não exibe o vergalhão que não será alterado.
- A conversão DOCX preserva o símbolo estatístico literal `<` e o texto
  subsequente.

### Adicionado

- Diagnóstico não impeditivo de mudança de formação vegetacional na mesma UA,
  com classes dentro da COLETA, no mesmo ano e entre anos.
- Aba para justificativas append-only de pendências remanescentes, com ID
  estável, responsável, classificação, timestamp e ciclo de vida auditável.

### Revisado

- Os fluxos espaciais por COLETA e por ANO foram reunidos em uma seção única;
  campos de número esperado de linhas foram removidos e destinos inequívocos
  são preenchidos automaticamente.
- Quando o mapa Sentinel é gerado, o mapa vetorial equivalente é suprimido; o
  mapa por ano permanece. As larguras editoriais foram harmonizadas nos cinco
  formatos dos relatórios.

### Preservado

- Baseline v2.9.1, contrato único, 13 modos, defaults, linhagem, replay,
  precedência, produtos centrais, estatísticas, gráficos, KML/KMZ, planilhas
  SISMONITORA e cartografia fora das revisões declaradas.

### Validação

- Testes focais aprovados com os casos reais de edição superior e movimento
  exótica para nativa, além de painel real sobre 16.766 linhas.
- Pipeline integral aprovado em 304,986 s, com 24 gates finais sem falha,
  quatro XLSX, dez relatórios e mapa Sentinel-2 L2A.
- PDFs e DOCX tiveram 47 páginas renderizadas e inspecionadas. O botão Source
  foi testado no RStudio para Windows; não se declara execução real em macOS.

## v2.9.1 - 2026-08-01

### Adicionado

- Opção `MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA`, avaliada somente
  quando a criação do XLSX está ativa, para esvaziar ou preservar os UUIDs na
  planilha sem alterar `registros_validados.csv`.
- Formato DOCX para os relatórios analíticos sintético e detalhado.
- Leiaute cartográfico quadrado com faixa institucional inferior, localizador,
  legenda consolidada, informações do mapa e marcas Monitora, CBC e ICMBio.
- Metadados cartográficos auxiliares em CSV e JSON, com fontes, processamento,
  projeção, imagem, aquisição e limitações de uso.

### Revisado

- Títulos das seções sintética e detalhada, terminologia cartográfica e rótulos
  gramaticais foram harmonizados.
- O mapa principal passa a incluir moldura coordenada, grade discreta, escalas
  gráfica e numérica, norte geográfico e título com preposição adequada à UC.
- Localizador passa a representar bioma, estados, limite oficial da UC e grade
  amostral sem incorporar referências espaciais específicas ao script.
- Arquivos vetoriais oficiais são usados somente em diretório temporário e
  removidos depois da materialização em memória.
- O mapa vetorial complementar passa a usar `Continuidade do esforço amostral
  nas UAs`, `Formação vegetacional` e o título institucional completo.

### Preservado

- Baseline v2.9.0, contrato único, painel, regras semânticas, linhagem, replay,
  13 modos, produtos centrais, estatísticas e KML/KMZ.
- Relatórios, mapa orbital e planilhas SISMONITORA permanecem desligados por
  padrão e retornam sem custo próprio quando configurados como `N`.

### Validação

- Gate XLSX aprovado com 16.766 registros reais, 166 coletas e quatro contextos
  nos modos de remoção e preservação de UUID.
- Dez documentos analíticos finais aprovados em Rmd, Markdown, HTML, DOCX e
  PDF, com zero falhas na auditoria de renderização.
- Cartografia dinâmica aprovada com dados reais da FNCS e, independentemente,
  com 201 geometrias do Parque Nacional Mapinguari.
- Compatibilidade funcional executada em Linux e DOCX renderizado no Microsoft
  Word no Windows; macOS auditado estaticamente, sem alegação de execução real.

## v2.9.0 - 2026-07-31

### Adicionado

- Relatórios analíticos opcionais por UC, em versões sintética e detalhada,
  nos formatos editáveis Rmd e Markdown e nos formatos HTML e PDF.
- Tabelas e figuras de esforço amostral por UC, formação vegetacional e ano,
  continuidade temporal, situação dos dados e evidências priorizadas.
- Mapa opcional de continuidade sobre imagens Sentinel-2 L2A recentes, obtidas
  de catálogo público sem conta, chave, token, faturamento ou hardcode de UC.
- Alternativa explicitamente isolada para Google Maps, condicionada a chave do
  usuário e sem persistência de credenciais.

### Corrigido

- A importação SISMONITORA com múltiplos contextos deixa de bloquear a execução
  e gera uma planilha independente para cada combinação UC + ciclo + campanha.
- Os relatórios PDF passam a resolver o navegador de forma multiplataforma e
  eliminam a falha de `favicon.ico` observada no servidor efêmero do Chromium.
- A organização final do `output/` é concluída antes de qualquer bloqueio de
  produto e mantém na raiz somente o README e o índice canônico.
- A situação dos dados nos relatórios usa `Validado`, `Em validação` ou
  `Não validado`, sem expor códigos internos de controle de qualidade.
- O botão Source do RStudio deixa de preparar dezenas de milhares de expressões:
  o programa público é uma única expressão externa, mantém o corpo legível e
  corrige persistentemente a preferência `Source with Echo` quando necessário.

### Desempenho e auditabilidade

- Canonicalização de aliases e migração contratual de canela-de-ema passam a
  usar gates por token e retorno imediato quando o caso não existe no dataset.
- A reconciliação de formas de vida reutiliza estado semanticamente idêntico,
  reprocessa somente linhas focalmente alteradas e preserva fallback global em
  mudanças estruturais ou ambíguas.
- Checkpoints granulares distinguem normalização, reconciliação, migração,
  auditoria impeditiva, finalização e geração dos produtos opcionais.
- A finalização libera somente dispositivos gráficos criados pela execução e
  remove `Rplots.pdf` residual sem tocar dispositivos anteriores da sessão.

### Preservado

- Baseline v2.8.2, contrato único embutido, painel, linhagem, replay semântico,
  sanitizações, 13 modos públicos, produtos centrais, estatísticas e KML/KMZ.
- Relatórios analíticos, mapa orbital e XLSX SISMONITORA continuam desligados
  por padrão; quando em `N`, não materializam pacotes, consultas, dados ou I/O
  próprios e não acrescentam custo material ao pipeline.

### Validação

- Execução integral real no Windows com materialização dos produtos de dados,
  planilhas XLSX multicontexto e relatórios Rmd, Markdown, HTML e PDF.
- QA contratual e módulos aditivos aprovados no Linux; compatibilidade macOS
  auditada estaticamente, sem alegação de execução real nesse sistema.
- Teste real do RStudio 2026.07.1+147: primeira mensagem em 0,317 s com
  `Source with Echo` e em 1,805 s com o botão Source normal.


## v2.8.2 - 2026-07-30

### Adicionado

- Produto opcional `registros_validados_importacao_sismonitora.xlsx`, gerado
  exclusivamente a partir de `registros_validados.csv` aprovado e
  materializado na mesma execução.
- Projeção para inclusão de registros novos no SISMONITORA, preservando as três
  abas do modelo 21FEV25, acrescentando `uc` à aba `Preenchimento` e mantendo
  campos comuns somente na primeira linha de cada bloco de 101 pontos.
- Auditoria e manifesto específicos com hashes da fonte, do produto e do
  modelo, contagens de coletas e registros e rastreabilidade dos UUIDs
  existentes na fonte.

### Corrigido

- As colunas `uuid` e `amostragem/registro/uuid` permanecem estruturalmente na
  planilha, mas suas células de dados ficam vazias no modo de inclusão, evitando
  a rejeição `UUID inválido` e permitindo ao SISMONITORA criar identidades novas.
- `observacoes_gerais` é preservado na fonte e na auditoria, mas omitido da
  planilha enquanto o importador não avaliar a função XPath `regex` usada pelo
  XLSForm 21FEV25.

### Preservado

- `registros_validados.csv` e seus UUIDs não são alterados pela geração do
  produto derivado.
- Com a opção em `N`, o módulo XLSX não é materializado, não lê dados e não
  carrega dependências adicionais.
- Interface, painel, atributos exibidos, operações semânticas, 13 modos
  públicos, contrato único, produtos centrais, replay, validação espacial e
  produtos KML/KMZ permanecem inalterados.

### Validação

- Campanha FNCS 2026 aprovada com 58 coletas, 5.858 linhas de dados, 115
  colunas e três abas.
- As 5.858 células de `amostragem/registro/uuid` e as células de `uuid`
  permaneceram vazias no produto de importação, com a linhagem preservada na
  fonte e na auditoria.
- Componentes congelados do modelo, ordem dos pontos 1–101, CPF textual,
  campos comuns, hashes e caminho desligado foram verificados.
- A geração focal dos bytes finais levou 3,625 s; a regressão histórica de CPF, UUID e
  fechamento hierárquico permaneceu aprovada.

## v2.8.1 - 2026-07-30

### Corrigido

- O fechamento hierárquico do TRIOUT passa a atuar sobre o estado efetivo
  reconciliado, removendo resíduos legados de “outra forma de vida” que podiam
  reaparecer após a aplicação de operações concorrentes da mesma fila.
- A auditoria pós-aplicação e o gate pós-exportação passam a confirmar a
  persistência da sanitização depois da materialização e releitura do
  checkpoint.
- Resíduos históricos são diferenciados do token contratual atual `outros`,
  evitando tratar uma resposta válida do XLSForm vigente como legado.

### Preservado

- Interface, painel, lista “Atributos a corrigir”, apresentação da fila,
  denominação e códigos das operações semânticas permanecem inalterados.
- Contrato consolidado dos XLSForms 2022–2025, precedência de correções
  específicas, linhagem, replay, 13 modos públicos, produtos KML/KMZ e
  validação espacial permanecem compatíveis com a v2.8.0.
- Nenhuma célula de negócio fora dos dois alvos causais do caso APAI foi
  alterada na comparação congelada.

### Validação

- O teste focal reproduziu o erro na v2.8.0 e aprovou a correção na v2.8.1.
- As filas reais das rodadas 02 e 03 da APAI foram reaplicadas pelo backend
  produtivo, exportadas e relidas com zero resíduo nos dois alvos.
- O fluxo real do painel em navegador foi executado sobre a rodada 02, desde o
  filtro e a seleção da COLETA 17626 até a exportação e releitura do
  checkpoint.
- A carga integral APAI/PNB/PNM permaneceu idêntica à v2.8.0 em 64.337 linhas
  × 267 colunas, desconsiderado apenas o caminho temporal de extração, sem
  regressão material de tempo ou memória.
- Parse, regressões TRIOUT/APAI, validação espacial e produtos KML/KMZ
  permaneceram aprovados.

## v2.8.0 - 2026-07-23

### Adicionado

- Produto KML/KMZ operacional de vértices e transectos, com `form_veg`,
  simbologia padronizada e sem dados primários da amostragem.
- Produto KML/KMZ estatístico derivado de `registros_corrig_stat.csv`.
- Áreas operacionais de proteção de 100 m, sem preenchimento, com contorno
  amarelo, rótulo da UA e metadados da referência espacial.
- Filtros espaciais por UC, UA e ano para identificação guiada das coletas de
  origem e destino.
- Recomendações espaciais determinísticas auditáveis e sanitização limitada aos
  casos inequívocos.

### Corrigido

- Preservação bidirecional das correções específicas frente a SANHAB, SANEORF,
  TRIOUT, movimentos em lote e sanitizações espaciais.
- Limpeza do hábito obrigatório na categoria de origem durante movimentos
  atômicos, inclusive quando a tabela de dependências contém associação parcial.
- Triagem espacial incremental sem perda de coletas elegíveis.

### Preservado

- Contrato consolidado dos XLSForms 2022–2025 e projeção final conforme o
  XLSForm 2025/template SISMONITORA.
- Painel, atributos a corrigir, apresentação da fila, denominação das operações
  semânticas e 13 modos públicos.
- Replay semântico, continuidade incremental, linhagem e gate estrito de
  materialização de `registros_validados.csv`.

### Validação

- RBC, FNCS, PNM, PNB e PNCV aprovadas em duas runs encadeadas, com o mesmo SHA
  do script nas dez execuções.
- 159.883 registros preservados, 45 correções específicas e 3.860 movimentos
  acompanhados, sem divergência causada por operação ampla.
- 15 KML e 15 KMZ materializados e estruturalmente verificados.

## v2.7.4 - 2026-07-21

### Corrigido

- O oráculo do replay passa a comparar o estado final reconciliado de `registros_corrig`, imediatamente antes da exportação, em vez de um estado intermediário anterior às reconciliações contratuais.
- O gate estrito deixa de aceitar identidade composta potencialmente ambígua: a comparação exige identificador canônico preenchido e único nas duas bases.
- Diferenças físicas de rótulos de colunas são reconciliadas somente quando o mapa contratual produz associação unívoca; associações ambíguas permanecem impeditivas.
- Divergências do oráculo não podem mais ser ocultadas por tratamento diagnóstico quando o bloqueio estrito estiver ativo.

### Adicionado

- Relatórios específicos de identidade, reconciliação de colunas, chaves exclusivas, colunas divergentes e amostras de células em `output/03_auditorias/replay_semantico/`.
- Gate fail-closed para oráculo ausente, inválido, sem identidade única ou divergente.
- Orientação operacional completa para separar replay semântico de continuidade incremental e validar uma transição de versão por run-oráculo.

### Preservado

- Interface, painel, atributos a corrigir, apresentação da fila, operações semânticas, resolvedores e 13 modos públicos permanecem inalterados.
- O oráculo continua estritamente auditivo: nenhum valor da run de referência é usado como entrada ou correção do pipeline.
- Com o replay e o oráculo desativados, o fluxo operacional permanece equivalente à v2.7.3.

### Validação

- Replay PNCA aprovado com 7.070 registros e dois movimentos sem qualquer diferença de chave, coluna ou célula frente à run-oráculo.
- Replay APAI aprovado com 7.878 registros, incluindo SANHAB, movimento de forma de vida e exclusão de coleta aplicados atomicamente.
- Divergência deliberada de uma célula bloqueou a exportação; modo incompatível, oráculo ausente e identidade não única também foram rejeitados.
- O gate de comparação acrescentou aproximadamente 0,5 s em PNCA e 1 s em APAI.

## v2.7.3 - 2026-07-21

### Corrigido

- Projeção de hábitos em `registros_validados.csv` passa a usar o atributo físico canônico já reconciliado em `registros_corrig.csv`, compartilhando a regra contratual aplicada pelo diagnóstico e pelo SANHAB.
- Resolução de aliases históricos ampliada pela associação ao token contratual, sem depender das ocorrências presentes no conjunto de entrada.
- Valores escalares históricos repetidos em campos separados por `|` são reconhecidos como equivalentes somente quando todos os tokens não vazios são idênticos; combinações heterogêneas continuam bloqueadas.
- Limpeza automática de dependências condicionais deixa de incluir atributos protegidos entre os campos de origem elegíveis para remoção.

### Preservado

- Interface, painel, atributos a corrigir, apresentação da fila, operações semânticas e 13 modos públicos permanecem inalterados.
- Operações específicas continuam protegidas contra sanitizações amplas e movimentos em lote.
- Contrato único, linhagem incremental, replay, gates de exportação, organização dos produtos e desempenho vetorizado permanecem compatíveis.

### Validação

- 13/13 modos executados no artefato funcional, com 102 gates aprovados e nenhuma falha.
- 22/22 UCs reexecutadas no artefato funcional, com 242 gates aprovados e nenhuma falha.
- O conjunto acumulado inclui 14 replays, oito campanhas de três runs incrementais e auditoria consolidada, totalizando 626 gates operacionais aprovados e nenhuma falha.
- Foram verificados contrato único, cardinalidade, identidade, hábitos, operações, linhagem, produtos obrigatórios, codificação UTF-8 com BOM e ausência de falhas fatais.
- A revisão editorial da release alterou somente duas linhas de comentário; o parser confirmou identidade integral de todos os tokens executáveis em relação ao artefato testado.

## v2.7.2 - 2026-07-20

### Corrigido

- Reconciliação de listas históricas de formas de vida por mapas unívocos derivados dos XLSForms 2022–2025, sem depender das colunas ou ocorrências presentes em um dataset específico.
- Resolução de hábitos históricos por versão e linha, com normalização de tokens repetidos, materialização no atributo físico correto e proteção contra propagação entre `nativa`, `exotica` e `seca_morta`.
- Validação de tokens de domínio por correspondência exata, evitando interpretação indevida como expressão regular.
- Regressão de desempenho na materialização das regras contratuais eliminada por cache e processamento vetorizado dos mapas de aliases.

### Preservado

- Interface, atributos a corrigir, apresentação da fila, operações semânticas e 13 modos públicos permanecem inalterados.
- Operações específicas continuam protegidas contra sanitizações amplas e movimentos em lote.
- Gates contratuais, persistência pós-exportação, linhagem incremental, replay e organização dos produtos permanecem compatíveis.

### Validação

- Auditoria multi-UC aprovada em 37/37 verificações, com três runs incrementais e 124.634 registros finais.
- Auditoria Mapinguari aprovada em 50/50 verificações, com três runs incrementais e 20.301 registros finais.
- Regressão FNCS aprovada em 34/34 verificações, com 16.766 registros finais e equivalência semântica célula a célula com a referência validada.
- Etapa contratual medida em 15,987 s no multi-UC, 2,619 s em Mapinguari e 2,382 s no FNCS, eliminando a regressão de aproximadamente 83 s.

## v2.7.1 - 2026-07-20

### Corrigido

- Auditoria de dependências condicionais passa a respeitar a versão do XLSForm de cada linha: hábito de samambaia é obrigatório a partir de 2025, sem falso bloqueio para registros históricos de 2022–2024.
- Relatório de normalização de CPF preserva todas as ocorrências inválidas detectadas antes das operações, inclusive quando uma linha é posteriormente removida por exclusão auditável de coleta.

### Preservado

- Interface, atributos a corrigir, fila e rotinas semânticas permanecem inalterados.
- Operações específicas continuam protegidas contra sanitizações amplas e movimentos em lote.
- Os 13 modos públicos, a linhagem incremental, o replay semântico, os gates contratuais e a organização dos produtos permanecem compatíveis com a v2.7.0.

### Validação

- Três runs PNCV consecutivas aprovadas nos modos `painel_e_parar`, `painel_incremental_registros_corrig` e `painel_incremental_completo`, com 76.255 linhas finais, linhagem 3 → 6 → 7 eventos e zero pendência impeditiva.
- Regressão FNCS aprovada com 16.766 linhas e equivalência semântica célula a célula com a execução validada anterior.
- Produtos finais legíveis em UTF-8 com BOM, sem CPF inválido preenchido e com `registros_validados.csv` materializado somente após aprovação contratual.

## v2.7.0 - 2026-07-19

### Adicionado

- Fonte reconciliada de ocorrências diagnósticas, com identidade estável por caso real, metadados contratuais e relatórios detalhados pré e pós-painel.
- Linhagem cumulativa e replay pelo contrato `replay_semantico_v2`, com manifesto, hashes e proteção contra combinação indevida entre replay e checkpoint já corrigido.
- Auditorias ponta a ponta entre input, registros importados bruto, registros importados, camada operacional pré-painel, registros corrigidos e registros validados.
- Relatório detalhado de rejeições contratuais com coleta, ponto amostral, atributo, valor, regra, severidade e motivo.
- Proteção contra eventos duplicados em operações e atualização integral da prévia.

### Alterado

- Harmonização de aliases, nomes, rótulos e caminhos físicos dos XLSForms 2022, 2023, 2024 e 2025 passa a ser orientada pelo contrato consolidado e pela projeção XLSForm 2025/template SISMONITORA.
- Prévia, modal, aplicação e fechamento passam a compartilhar o mesmo estado reconciliado de operações e ocorrências.
- Operações semânticas específicas, sanitizações amplas e movimentos em lote usam escopo efetivo por linha e transações atômicas, preservando correções específicas sobrepostas.
- SANHAB atua sobre todas as categorias elegíveis de hábito obrigatório, respeita correções específicas e não exige modal próprio nem atualização integral automática do preview.
- TRIOUT reconcilia outras formas de vida, descritores e `Encostam`; `solo_nu` só é aplicado como fallback exclusivo quando nenhuma interceptação válida resta no ponto.
- Materialização de `registros_validados.csv` passa a depender da aprovação integral do gate contratual pós-correções.
- Organização canônica de produtos em `output/01_produtos_dados/` a `output/06_graficos/` e documentação em `docs/`.
- Escrita dos produtos CSV ajustada para leitura portátil em Windows, macOS e Linux e para abertura direta em aplicativos de planilha.

### Corrigido

- Contagem de hábitos obrigatórios ausentes por ocorrência real e categoria, sem propagação indevida entre categorias.
- Persistência de hábitos e movimentos de formas de vida nos atributos canônicos de `registros_corrig.csv` e `registros_validados.csv`.
- Conflitos falsos entre múltiplos movimentos válidos no mesmo escopo e perda de itens não exibidos na fila.
- Divergências entre contagens pré e pós-preview e atualização redundante no fechamento.
- Reconciliação de valores históricos separados por `|`, aliases com mojibake e campos condicionais esparsos, com bloqueio seguro quando a correspondência não é determinável.
- Propagação de mojibake de `registros_corrig.csv` para `registros_validados.csv`.
- Preservação de `coletor/cpf` e `coletor/nome` conforme a estrutura física do template SISMONITORA.

### Validação

- Matriz automatizada com 80 verificações aprovadas e nenhuma falha na candidata funcional.
- Linhagem, operações específicas, sanitizações amplas, movimentos em lote, incrementalidade, replay total, persistência pós-exportação e gate de materialização verificados.
- As cópias públicas do script são geradas a partir dos mesmos bytes finais e conferidas por SHA-256.

## v2.6.4 - Hotfix do contrato do painel, persistência e importação robusta

- Reconcilia o seletor "Atributo a corrigir" entre dropdown, auditoria e log do painel: 98 atributos editáveis de fato, sem divergência silenciosa. Causa raiz: um alias mojibake de campo de espécie (texto livre) era tratado como alias histórico de hábito e ocultado indevidamente do seletor.
- Resolve os 14 atributos que apareciam como `bloqueado_sem_dominio_xlsform`: 13 por falha de correspondência entre o nome de coluna (com aspas HTML herdadas do formulário de origem) e o rótulo do XLSForm; 1 (`UC`) por depender de lista XLSForm dinâmica/externa sem opções embutidas, agora resolvido com fallback de domínio observado seguro.
- Garante que nenhum atributo exposto no seletor termine bloqueado tardiamente pelo botão "Adicionar correção".
- Preserva `UA`/`EA` editáveis e persistentes, `CICLO`/`CAMPANHA` editáveis, `Data`/`Horário` editáveis e validados, coordenadas inicial/final validadas como geopoint.
- Mantém `forma_vida_nativa_samambaia` como hábito obrigatório (`select_one`) e `forma_vida_nativa_samambaia_sp` como texto livre de espécie.
- Preserva a persistência de correções pós-exclusão de coletas duplicadas (EXCCOL) por chave estável (`uuid_registro`/chave composta), sem usar o atributo corrigido como filtro de contexto de si mesmo.
- Importação mais robusta para colunas estruturadas: reparo determinístico de mojibake (UTF-8 decodificado como Latin-1) antes da consolidação de aliases e da tokenização; resolução segura por deduplicação quando todos os tokens de uma célula residual são idênticos (nenhum token é inventado).
- Substitui a mensagem genérica de erro na materialização do produto operacional pré-painel por um diagnóstico específico (COLETA/UA/coluna/faixa de pontos) quando uma lista de valores separada por `|` não tem correspondência segura com os pontos amostrais da coleta — nesse caso, a importação continua bloqueada de propósito, para proteger a integridade dos dados, até correção manual dos dados de origem.
- Mantém gráficos em `output/06_graficos` e replay semântico (`replay_semantico_v1`) preservado.
- Release pública sem dados reais, sem `input/`, `output/`, `log/`, `extracted/` ou produtos locais de execução.

## v2.6.3 - v2.6.3 com painel, replay e produtos finais

- Publica a versão v2.6.3 validada funcionalmente, com painel operacional para curadoria assistida.
- Preserva operações semânticas auditáveis: EXCCOL, PENDHAB em lote e individual, SANEORF, TRIDESC/TRIOUT e correções simples/lote.
- Mantém `correcoes_semanticas_consolidada.csv` como trilha oficial de replay semântico compatível com `replay_semantico_v1`.
- Confirma geração dos produtos finais quando sem bloqueios: `registros_corrig.csv`, `registros_validados.csv`, `registros_corrig_stat.csv`, estatísticas, gráficos, KMLs, manual e relatório consolidado.
- Incorpora correção documental da seção 5.2 do relatório consolidado, com leitura CSV robusta e fallback contra cabeçalhos `V1..Vn`.
- Mantém defaults públicos seguros: painel, registros importados, registros validados, validação espacial e replay desligados por padrão.
- Release pública sem dados reais, sem `input/`, `output/`, `log/`, `extracted/` ou produtos locais de execução.

# CHANGELOG

## v2.6.0 - Roll-forward semântico, relatório consolidado e governança de validação

- Publica a linha v2.6.0 a partir da versão operacional validada.
- Acrescenta roll-forward semântico por `correcoes_semanticas.csv`, com contrato `replay_semantico_v1`.
- Mantém comparação com run-oráculo desligada por padrão e disponível para auditoria avançada.
- Amplia o manual com passo a passo dos modos e orientação de continuidade operacional.
- Amplia o relatório consolidado para registrar modificações, sanitizações automáticas, operações assistidas e produtos de dados.
- Mantém defaults públicos seguros para execução completa sem painel e sem geração automática de produtos sensíveis opcionais.

# Changelog

## v2.5.6 - 2026-06-28

### Adicionado
- Auditoria cadastral não bloqueante para valores aparentemente deslocados entre atributos cadastrais.
- Sanitização automática de deslocamentos inequívocos entre `CICLO`, `CAMPANHA`, `EA` e `UA`.
- Relatórios de auditoria cadastral automática e ocorrências cadastrais suspeitas não bloqueantes.

### Corrigido
- Remoção de falso aviso de bloqueio de `registros_validados.csv`.
- Consolidação da auditoria de persistência por efeito diagnóstico final.
- Estabilização da limpeza vetorizada de resíduos legados de outras formas de vida.

### Preservado
- `outra_forma_vida` como choice válido de `tipo_forma_vida` quando acompanhado de `forma_vida_outros`.
- Campo contratual `amostragem/registro/forma_vida_seca_mortaarvore_abaixo`.
- Defaults públicos seguros.


## v2.5.5 - Painel contratual, registros importados saneados e controles de edição

- Corrige os modos de painel para forçar abertura nos fluxos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_*`.
- Melhora a granularidade do progresso em etapas pós-painel e de fechamento contratual.
- Transforma `registros_importados.csv` em produto saneado/comparável e preserva `registros_importados_bruto.csv` como snapshot técnico.
- Adiciona auditorias de comparação `registros_importados` × `registros_corrig` × `registros_validados`.
- Restringe o seletor do painel aos atributos do template SISMONITORA/XLSForm 2025, em lista global e ordenada pelo template.
- Garante inclusão de atributos contratuais ausentes nos dados de entrada, incluindo `forma_serrapilheira` e `forma_vida_outros`.
- Implementa controles dinâmicos de edição por tipo de atributo e auditoria `auditoria_painel_controle_atributos.csv`.
- Bloqueia `Substituir valor` e `Limpar valor` em listas de tokens/select_multiple; listas passam a aceitar apenas adicionar, remover ou substituir tokens.
- Bloqueia edição de atributos com domínio XLSForm não carregado.

## v2.5.4 - 2026-06-26

- Publica versão estável com fechamento contratual de `registros_corrig.csv` antes da materialização.
- Consolida geração auditável de `registros_validados.csv` com 129 atributos no contrato/template.
- Padroniza ausências: `registros_corrig.csv` com `NA` físico e `registros_validados.csv` com vazio efetivo.
- Remove resíduos de traços em campos vazios e preserva auditorias de schema, formatos, domínios e condicionais.
- Corrige classificação histórica de `canela_de_ema` para forma de vida nativa.
- Mantém painel de correções assistidas opcional e modos operacionais sem gráficos.

## [v2.5.3] - 2026-06-25

### Adicionado
- Barra de progresso `cli` como backend público padrão, com atualização controlada para reduzir ruído e custo de console.
- Registro explícito da configuração reprodutível dos testes Monte Carlo, com semente base e RNG documentados no log.
- Checkpoints de performance mais granulares em torno de `correcao_ponto_metro` e auditorias de COLETAS por UC+UA+ANO.

### Alterado
- Preparação estatística e objetos gráficos mantêm as otimizações com `data.table` introduzidas na série 2.5.
- A execução pública permanece em modo seguro por padrão, sem abrir painel, sem validação espacial automática, sem `registros_importados.csv` e sem `registros_validados.csv`, salvo configuração explícita.
- Relatórios e produtos mantêm a estrutura pública da `v2.5.2`, com melhorias de rastreabilidade e acompanhamento de execução.

### Corrigido
- Correção editorial do README público para alinhar versão, links versionados e diretórios congelados com `v2.5.3`.
- Redução de falsos gargalos na auditoria de duplicidade por separação de checkpoints.
- Sanitização editorial para remover menções a versões internas, revisões versões de desenvolvimento, caminhos locais e sistemas operacionais específicos.
- Preservação de UTF-8 sem BOM e bloqueio de padrões comuns de mojibake antes da publicação.

## [v2.5.2] - 2026-06-24

### Adicionado
- Painel espacial em fluxo origem → destino → operação, com origem independente do destino, lote filtrado e botão para usar COLETAS filtradas.
- `output/registros_importados.csv` opcional, desligado por padrão público seguro por poder conter nomes, CPF, UC, coordenadas, fotos, UUIDs e observações de campo.
- Controle inicial de entrada no painel baseado no contrato XLSForm, com mensagens instrutivas, tokens válidos, sugestões e exemplos de preenchimento.
- Operação composta para forma de vida com hábito obrigatório em correção individual/ponto, incluindo atualização do campo superior `Encostam`.

### Alterado
- `registros_corrig.csv` passa a ser reordenado de forma técnica e padronizada, priorizando colunas do contrato XLSForm/SISMONITORA.
- Listas de origem espacial passam a considerar todas as COLETAS válidas da mesma UA, não apenas o status/ano do destino.
- README passa a priorizar a versão pública atual, mantendo histórico detalhado no CHANGELOG e nas releases.

### Corrigido
- Evitado loop reativo nos filtros do painel espacial.
- Corrigida a seleção de UA/ANO/COLETA após filtragem por Status espacial.
- Corrigida a aplicação de correção espacial para lote explícito de COLETAS destino.
- A COLETA fonte deixa de aparecer como destino quando o lote é preenchido por filtros.
- Entradas inválidas de tokens, datas e coordenadas passam a ser bloqueadas no painel antes de gerar falhas posteriores.

## [v2.5.0] - 2026-06-24

## [v2.5.1] - 2026-06-24

### Corrigido
- Corrige a abertura obrigatória do painel nos modos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_registros_corrig`.
- Mantém os defaults públicos seguros, mas garante que modos de painel acionem `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` internamente.
- Evita que datasets com duplicatas `UC+UA+ANO` sigam para a trava pós-correções sem permitir curadoria no painel.

### Mantido
- Mantidos os comentários revisados da publicação `v2.5.0`.
- Mantidas as funcionalidades de validação espacial, cache pré-painel e continuidade incremental.

### Adicionado
- Validação espacial de COLETAS com consenso robusto por UC/EA/UA, alertas, pendências, comparação pré/pós e correções auditáveis no painel.
- Modos de execução para curadoria assistida, reabertura por cache e continuidade incremental a partir de `registros_corrig.csv`.
- Regra obrigatória de uso exclusivo da pasta `input/` para arquivos de entrada.
- Painel com diagnóstico de escopo vazio, mapa espacial independente e operações de sessão isoladas.
- Suporte a coordenadas manuais com altitude e acurácia opcionais, com auditoria.

### Alterado
- Relatórios de apoio do painel passaram a usar triagem vetorizada por regex/data.table.
- Reabertura por cache passou a ignorar correções antigas em `input/` e aplicar somente correções criadas na sessão atual.
- Comentários e instruções operacionais foram consolidados para uso por usuários e manutenção humana/IA.

### Corrigido
- Evitado uso de metadados de registro como data de campo quando existem campos `data_hora`.
- Comparação espacial pré/pós agora materializa colunas derivadas antes da ordenação.
- Relatórios/auditorias pós-correções passam a ser gravados de forma defensiva em modos curtos.

## v2.4.2 - 2026-06-23

## v2.4.1 - registros_validados e contrato XLSForm/SISMONITORA

- Mantém `registros_corrig.csv` como versão canônica corrigida e auditável.
- Adiciona `registros_validados.csv` opcional como versão pública compatível com o contrato do XLSForm vigente e a estrutura de exportação do SISMONITORA.
- Acrescenta `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` como variável pública, desligada por padrão.
- Consolida auditorias de schema, formatos, domínios XLSForm, condicionais, chaves, UUIDs e sanitizações antes da exportação validada.
- Consolida sanitização de outras formas de vida históricas, fotos e descritores correlatos e dependentes de forma de vida desconhecida.
- Corrige resumos por unidade vazios para evitar arquivos residuais em `output/`.

## [v2.4.0] - 2026-06-21

### Adicionado
- `MONITORA_MODO_EXECUCAO` com modos `completo`, `sem_png`, `estatisticas_sem_graficos`, `ate_registros_corrig` e `painel_e_parar`.
- Operações semânticas atômicas para exclusão de COLETAS, movimento de formas de vida, substituição de desconhecida, limpeza de outras formas e movimento em lote de formas de vida.
- Movimento em lote de formas de vida por COLETAS com migração segura e relatório de ambiguidades.
- Notificações de início/conclusão e trava contra duplo clique no painel.
- Auditoria de persistência pós-aplicação e pós-exportação.
- Sincronização final de `Encostam`/`tipo_forma_vida` a partir dos campos inferiores finais.

### Alterado
- Fila do painel passa a exibir operações semânticas coerentes com as ações do usuário.
- Relatórios de comparação pré/pós-correções normalizam tipos auxiliares antes de `rbindlist()`.
- Comentários internos do script foram revisados para remover marcas de desenvolvimento, referências transitórias e justificativas interlocutórias.

### Corrigido
- Persistência de operações atômicas em `registros_corrig.csv`.
- Divergências reais de `Encostam` após correções sobrepostas.
- Falhas de `rbindlist()` por classes divergentes em relatórios auxiliares.
- Continuação silenciosa quando o painel encerra sem ação explícita.


## [v2.3.2] - 2026-06-19

### Destaques

- Publica a versão `v2.3.2` após a `v2.3.1`.
- Corrige a exclusão de COLETAS filtradas/selecionadas no Painel de validação - correções assistidas de `registros_corrig`.
- Garante que a exclusão de COLETAS seja aplicada como remoção integral de linhas, sem deixar vestígios no `registros_corrig.csv`.
- Melhora a visualização das tabelas do painel ao ocultar o campo `uuid` apenas na interface.

### Alterado

- `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES` permanece com padrão inicial `"N"`.
- O `uuid`/amostragem-registro deixa de ser exibido nas tabelas do painel, mas permanece preservado nos dados, operações, logs, auditorias e arquivos finais.
- Comentários do script público foram revisados para remover menções residuais a versões anteriores como referência corrente, incluindo a menção indevida à `v2.3.0` observada no script público anterior.

### Corrigido

- O botão de exclusão de COLETAS em lote deixa de ser bloqueado pela regra de coluna estrutural/protegida, pois a operação não edita `COLETA`; ela remove integralmente as linhas das COLETAS alvo.
- Auditoria pós-exclusão passa a bloquear a execução caso alguma linha de COLETA excluída permaneça na base corrigida.

# Changelog

## v2.3.1 - 2026-06-19

### Painel de correções assistidas

- Implementa limpeza atômica auditável de outra forma de vida.
- Remove tokens históricos de outra(s) forma(s) de vida em listas de forma de vida nativa, exótica e seca/morta.
- Limpa descritores históricos dependentes de outra forma de vida.
- Adiciona checkpoints de persistência pós-aplicação e pré-exportação.
- Simplifica o escopo do painel para coleta individual ou coletas do lote.
- Define COLETAS do lote como fonte explícita para operações em lote.
- Adiciona botão Limpar filtros.
- Usa labels do XLSForm mais recente para formas de vida, mantendo names históricos vinculados internamente.
- Mantém hábito restrito às formas condicionais previstas no XLSForm: bromelioide, cactacea, orquidea e samambaia.
- Preserva o painel desativado por padrão.

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.3.0] - 2026-06-18

### Destaques

- Publica a versão `v2.3.0` após a `v2.2.2`.
- Amplia o Painel de validação - correções assistidas de `registros_corrig`.
- Introduz correção em lote por múltiplas COLETAS com filtros superiores hierárquicos.
- Introduz exclusão auditável de COLETAS filtradas ou selecionadas.
- Altera a deduplicação para preservar COLETAS distintas com mesma UC + UA + ANO para curadoria manual.
- Adiciona trava pós-correções para impedir análise estatística quando restarem conflitos de UAs duplicadas no mesmo ano.
- Inclui `COLETA` em `registros_corrig_stat`, antes de `UC`.

### Adicionado

- Filtros superiores hierárquicos e multisseleção no painel: UC(s), EA(s), ano(s), ciclo(s), campanha(s), UA(s) e COLETAS.
- Checkbox para usar todas as COLETAS resultantes dos filtros superiores.
- Prévia auditável por COLETA antes de gerar operações em lote.
- Operações de correção em lote com uma operação por COLETA por atributo.
- Botão para adicionar exclusão auditável de COLETAS filtradas/selecionadas.
- Filtro de triagem para UAs duplicadas no mesmo ano.
- Auditorias pré e pós-correções de COLETAS com UAs duplicadas por ano.
- Cache de localização de linhas por COLETA, coleta_uuid, uuid_registro e linha_indice.
- Coluna `COLETA` em `registros_corrig_stat`, posicionada antes de `UC`.

### Alterado

- A deduplicação automática foi ajustada para manter registros genuinamente idênticos, mas preservar COLETAS distintas envolvidas em conflitos UC + UA + ANO.
- Casos de múltiplas COLETAS para a mesma UC + UA + ANO deixam de ser resolvidos automaticamente e passam a ser encaminhados para triagem/correção no painel.
- A etapa pós-correções passa a bloquear a continuidade da execução quando conflitos de UAs duplicadas no mesmo ano permanecem não resolvidos.
- O README foi atualizado para refletir os novos fluxos de curadoria, mantendo as seções públicas anteriores, incluindo uso auxiliar de IA.
- Os links do README foram atualizados para as cinco cópias públicas da `v2.3.0`.

### Corrigido

- Redução de risco operacional em edições de atributos uniformes por COLETA.
- Evita que conflitos reais de campo sejam mascarados por deduplicação automática.
- Reforça a rastreabilidade entre `registros_corrig.csv` e `registros_corrig_stat.csv` por meio do atributo `COLETA`.

### Validação recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"`.
- Testar correção em lote por múltiplas COLETAS.
- Testar seleção automática de COLETAS pelos filtros superiores.
- Testar exclusão auditável de COLETAS.
- Testar caso com múltiplas COLETAS na mesma UC + UA + ANO e confirmar interrupção pós-correções quando o conflito não for resolvido.
- Conferir `output/registros_corrig.csv`, `output/registros_corrig_stat.csv` e auditorias de COLETAS com UAs duplicadas.
- Conferir que as cinco cópias públicas do script têm SHA256 idêntico.



## [v2.2.2] - 2026-06-18

### Destaques

- Publica a versão `v2.2.2` após a `v2.2.1`.
- Estabiliza o Painel de validação - correções assistidas de `registros_corrig`.
- Mantém o relatório de ocorrência de formas de vida exóticas, o painel assistido e o fluxo analítico, estatístico e gráfico consolidado.

### Adicionado

- Pré-validação transacional de grupos de correção.
- Auditoria semântica pré/pós-correção.
- Tabela unificada de triagem do painel.
- Localização acelerada por `linha_indice`.
- Deduplicação defensiva por assinatura semântica.

### Alterado

- Triagem de exóticas passa a exigir vínculo operacional estrito entre `Encostam`, forma de vida e espécie.
- Hábito passa a ser aceito apenas para formas condicionais.
- Mapa canônico estrutural de colunas passa a ser cacheado.
- Comentários do script foram revisados para remover menções a versões internas e comentários interlocutórios.

### Corrigido

- Bloqueio de correções parciais em movimentos assistidos.
- Redução de reintrodução de tokens residuais após movimentos exótica → nativa.
- Tratamento mais seguro de CSVs vazios, warnings de exportação e objetos temporários.
- Melhoria de checkpoints, progresso textual e controle de recursos.

## [v2.2.1] - 2026-06-16

### Adicionado
- Relatório de ocorrência de formas de vida exóticas em output/.
- Produtos CSV auxiliares com registros de formas de vida exóticas com e sem espécie vinculada.
- Resumos por unidade, forma de vida e campo de espécie exótica.

### Corrigido
- Triagem do relatório restrita a registros com token exotica em **Encostam** na vareta.
- Correção da contagem de espécies exóticas vinculadas.
- Exclusão de campos auxiliares indevidos, como .id, da detecção de espécies.
- Tratamento de NA, campos vazios e --- como ausência nos produtos exportados.
- Reconhecimento de campos textuais abertos do tipo Outra espécie ... exótica como espécie vinculada quando associados à forma de vida exótica correspondente.
# Changelog

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.2.0] - 2026-06-16

### Destaques

- Consolida a evolução posterior à última versão pública `v2.1.3`.
- Introduz o **Painel de validação - correções assistidas de `registros_corrig`**.
- Mantém o fluxo analítico principal da série `v2.1.x`, incluindo estatísticas temporais, gráficos publicáveis seriados, painéis amostrais por ano inicial e relatório textual estatístico.

### Adicionado

- Painel Shiny opcional para validação e correção assistida de registros consolidados.
- Variável explícita no início do script para abertura do painel:
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` abre o painel;
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` executa sem painel.
- CSV longo de correções assistidas em `input/correcoes_campos.csv`.
- Auditoria de correções em `log/auditoria_correcoes_campos_*.csv` e cópia da última execução em `output/correcoes_campos/`.
- Metadados embutidos dos XLSForms 2022, 2023, 2024 e 2025 para regras de validação do painel, sem dependência de leitura de XLSForms externos.
- Dicionários auxiliares e diagnóstico adaptável aos atributos reais presentes em `registros_corrig`.
- Triagem de formas de vida exóticas nos registros selecionados, com exibição de coleta, ponto amostral, ponto metro, forma exótica e UUID do registro.
- Movimento assistido de forma de vida entre `exotica`, `nativa` e `seca_morta`.
- Controle de campos condicionais para formas como `samambaia`, `orquidea`, `cactacea` e `bromelioide`, incluindo hábito `terrestre`, `epifita` ou `rupicola` quando aplicável.
- Harmonização auditável de campos superiores e inferiores vinculados pelo XLSForm.

### Alterado

- O painel de validação é opcional e não altera a execução analítica padrão quando desativado.
- O valor padrão da opção do painel foi mantido como `"N"` para preservar a execução normal em produção.
- A aplicação de correções passou a usar resolução defensiva de nomes de colunas, considerando variações de labels, HTML, aspas escapadas, acentos e pontuação.
- O recálculo de `**Encostam** na vareta: (amostragem/registro)` passou a ser feito a partir das categorias de forma de vida após as correções assistidas.
- A documentação pública foi atualizada para incluir o painel, mantendo as seções institucionais, técnicas, de versionamento e de uso auxiliar de IA.

### Corrigido

- Remoção de riscos associados à edição direta de `registros_corrig.csv` em planilhas, por meio de correções guiadas e auditadas.
- Tratamento de movimentos exótica → nativa para limpar/atualizar campos inferiores e superiores vinculados.
- Compatibilidade entre campos do XLSForm e colunas consolidadas com labels ou HTML representados de formas distintas.
- Proteção contra correções malformadas geradas por seleção de linhas pré-triadas.

### Validação recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` para validar o fluxo analítico padrão.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` para validar o painel.
- Testar ao menos:
  - uma correção simples/lote por coleta;
  - um movimento assistido exótica → nativa;
  - conferência de `output/registros_corrig.csv`;
  - conferência de `output/correcoes_campos/auditoria_correcoes_campos_ultima_execucao.csv`.
- Conferir que os cinco scripts públicos têm o mesmo SHA256.

## [v2.1.3] - 2026-06-12

### Destaques

- Última versão pública da série `v2.1.x` antes da publicação da `v2.2.0`.
- Mantém os produtos editoriais, estatísticos e gráficos consolidados nas versões `v2.1.0` a `v2.1.2`.

### Corrigido

- Tratamento defensivo para exportação de gráficos em subconjuntos vazios.
- Ajustes em parâmetros de rótulos e exportação para reduzir falhas em cenários de dados filtrados ou incompletos.
- Compatibilidade com arquivos manipulados em editores de planilha, preservando as correções já consolidadas na série `v2.1.x`.

## [v2.1.2] - 2026-06-12

### Alterado

- Refatoração nominal interna e revisão editorial da documentação.
- Padronização de nomes internos de funções utilitárias, objetos globais, rotinas de recursos, auditoria, gráficos, layout, estatística e relatório textual.
- Consolidação da nomenclatura pública baseada em **painéis amostrais por ano inicial**.
- Atualização editorial do `README.md`, com recuperação das informações sobre uso auxiliar de IA.

### Validação

- Preservação dos produtos analíticos principais em relação ao baseline de refatoração.
- Scripts principais sincronizados.

## [v2.1.1] - 2026-06-12

### Destaques

- Revisão editorial dos produtos públicos após a publicação da `v2.1.0`.
- Atualização da terminologia pública de “coortes” para **painéis amostrais por ano inicial**.
- Padronização da nomenclatura pública dos gráficos com serial global `fig_001_...png` a `fig_156_...png`.
- Ampliação do relatório textual estatístico.

### Adicionado

- Índice mestre de gráficos `output/indice_graficos.csv`.
- Gráficos publicáveis seriados em `output/plots_png/`.
- Tabelas estatísticas dos painéis amostrais por ano inicial.

### Corrigido

- Classificação pública da `fig_036`.
- Ausência da exportação de composição geral contra linha de base dos painéis amostrais por ano inicial.
- Defasagem conceitual do relatório textual estatístico.

## [v2.1.0] - 2026-06-11

### Destaques

- Consolida a evolução desde a última versão pública `v2.0.2`.
- Inclui novos produtos analíticos, gráficos editoriais, análises longitudinais por ano inicial e auditorias.

### Adicionado

- Gráficos temporais editoriais com escopo amostral explícito.
- Painéis editoriais para amostra total por ano, UAs presentes em todos os anos avaliados e comparações pareadas por período consecutivo.
- Estatística pareada específica para gráficos editoriais período a período.
- Relatório textual estatístico em `output/relatorio_textual_estatistico.txt`.
- Auditorias de layout de rótulos, símbolos estatísticos, esforço amostral, performance e memória.

### Corrigido

- Rótulos de ano e `n UA` embaralhados em facetas.
- Linhas ou rótulos duplicados em painéis temporais.
- Sobreposição de símbolos estatísticos e rótulos.
- Legendas inferiores que ultrapassavam os limites dos painéis.
- Compatibilidade entre fontes com sobreposição de exportações.

## [v2.0.2] - 2026-06-10

### Destaques

- Última versão pública antes da consolidação `v2.1.0`.
- Continha o núcleo de tratamento, padronização, deduplicação, estatística, auditoria e relatório textual.

### Alterado

- Ajustes de consistência entre cópias públicas do script.
- Ajustes editoriais e de documentação da série `v2.0.x`.

## [v2.0.1] - 2026-06-10

### Alterado

- Ajustes incrementais de publicação.
- Organização de arquivos.
- Consistência entre cópias do script.
- Preparação da linha pública para revisão documental e tagueamento estável.

## [v2.0.0] - 2026-06-10

### Destaques

- Primeira versão pública com adoção de versionamento semântico.
- Consolidação estatística, auditoria e relatório textual.
- Organização pública do repositório com cópia congelada em `releases/v2.0.0/`.
- Registro explícito do uso auxiliar de IA generativa a partir da fase de consolidação pública.

### Adicionado

- Importação de múltiplos tipos de entrada.
- Extração recursiva de ZIPs do SISMONITORA.
- Auditoria de arquivos versões de desenvolvimento à importação.
- Deduplicação semântica de registros equivalentes.
- Verificação de integridade dos dados.
- Tratamento defensivo de colunas, datas, coordenadas e aliases.
- Controle de performance, memória e recursos computacionais.
- Análise estatística inferencial pareada por unidade amostral.
- Comparações ano a ano e contra linha de base acumulada.
- Teste de permutação pareado.
- Intervalo de confiança por bootstrap.
- Correção de múltiplas comparações por FDR.
- Análise de mudança na composição geral com distância de Bray-Curtis.

## v2.5.3 - Performance, progresso e reprodutibilidade

- Otimiza preparação de objetos gráficos e rotinas estatísticas com operações `data.table`.
- Adiciona barra de progresso `cli` como padrão público, mantendo informações de etapa, detalhe, percentual e ETA.
- Adiciona controle de atualização da barra para evitar custo excessivo de console em execuções longas.
- Torna bootstrap e permutação Monte Carlo reprodutíveis por semente base registrada.
- Separa checkpoints de correção de ponto metro e auditorias de COLETAS duplicadas para diagnóstico de performance.
- Mantém como padrão público `MONITORA_MODO_EXECUCAO <- "completo"` e opções auxiliares desligadas por segurança.
- Preserva compatibilidade com os produtos finais da versão pública anterior.
