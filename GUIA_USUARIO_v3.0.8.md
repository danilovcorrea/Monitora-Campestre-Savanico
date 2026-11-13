# Guia do usuário — v3.0.8

O arquivo `monitora_campsav_alvo_global_v3.0.8.R` executa a curadoria e gera os
produtos. Os módulos necessários estão incorporados; não execute arquivos de
apoio de desenvolvimento. Na primeira utilização, mantenha conexão à internet
para instalar dependências e consultar as fontes públicas habilitadas.

O [manual completo em PDF](manual_usuario/manual_usuario_v3.0.8.pdf) e a
[versão navegável em HTML](manual_usuario/manual_usuario_v3.0.8.html) explicam
as operações para usuários iniciantes, com percursos de execução, controles do
painel, arquivos de entrada e conferência dos resultados.

## Começar

1. Crie uma pasta local curta, exclusiva para a rodada, fora da sincronização
   ativa do OneDrive. Copie o `.R` para ela e crie `input/`.
2. Para uma primeira rodada, coloque as exportações brutas do SISMONITORA em
   `input/`. Para continuidade, use a base `registros_corrig.csv` da rodada
   conferida e sua linhagem correspondente. Siga o percurso específico do manual.
3. Abra o `.R` no RStudio em UTF-8. Edite somente as opções do bloco operacional.
   O arquivo fica abaixo de 5.000.000 bytes, inclusive com terminações CRLF.
4. Para curadoria, escolha o modo com painel. Revise as abas de registros,
   equipe da coleta, validação espacial e justificativas; registre responsável
   e justificativa nas operações. Não edite diretamente os CSVs gerados.
5. Para atualizar produtos de uma base conferida, escolha
   `registros_corrig_completo`, com abertura do painel em `N`.
6. Clique em **Source** e execute o arquivo inteiro. Ao final, confira a
   auditoria de produtos e o índice do `output/`. Depois copie a rodada
   concluída para o diretório institucional, preservando a rastreabilidade.

Uma justificativa documenta a decisão, mas não elimina uma inconsistência
impeditiva. O script preserva um checkpoint quando a base ainda exige curadoria;
não declare os produtos bloqueados como concluídos.

## Continuidade das justificativas

Para continuar uma base corrigida, copie juntos:

1. `output/01_produtos_dados/registros_corrig.csv` para `input/` da nova rodada;
2. toda a pasta `output/02_painel_correcoes/linhagem/` para `input/linhagem/`.

Não selecione somente os arquivos de linhagem conhecidos das versões anteriores.
A pasta também transporta os eventos de justificativa, seus aliases comprovados
e os manifestos que os vinculam aos dados. Preserve a rodada de origem.

Ocorrências com justificativa vigente ficam disponíveis para consulta e deixam
de ser oferecidas para novo registro. Isso se aplica à ocorrência reconhecida,
e não a todas as ocorrências do mesmo tipo. O diagnóstico continua visível;
justificar não altera a severidade nem resolve uma impeditiva.

As justificativas de não resolução têm histórico próprio. A justificativa de
uma correção semântica explica uma alteração do dado e não substitui esse
histórico. Arquivo declarado corrompido interrompe a recuperação; não equivale
a histórico vazio.

Uma rodada anterior à correção pode não ter transportado suas justificativas.
Sua recuperação exige as fontes correspondentes, o snapshot, os dados e a
revisão da mesma cadeia, em cópia preparada. Não crie novos registros nem
edite manifestos para simular essa recuperação. A ausência de histórico no
input não comprova que nunca tenham existido decisões na rodada anterior.

Use o cache somente com entrada e parâmetros compatíveis. Cache legado sem
prova de integridade ou cache de outra base deve ser regenerado. Se os dados
foram corrigidos depois da geração do cache, prefira o checkpoint corrigido e
sua linhagem; não empreste decisões posteriores a uma base anterior.

## Produtos e análises

| Produto | Configuração e conferência |
|---|---|
| Base validada | Ative a geração de registros validados; confira os impedimentos antes da entrega. |
| Importação SISMONITORA | Ative a opção específica. As planilhas são separadas por UC, ciclo e campanha. Cadastros de coletores podem bloquear o produto; não invente nomes/CPFs. A política de UUID depende da finalidade da importação. |
| Relatório de validação | Confira alterações herdadas/atuais, pendências, exclusões e linhagem. |
| Relatórios analíticos | Confira esforço, datas, comparabilidade, resultados, incerteza e discussão. Use o índice com páginas e links. |
| Fogo | Ative `MONITORA_OPCAO_ANALISE_FOGO`. O próprio R obtém/cacheia a fonte ICMBio. Leia período, modalidade, exposição espacial e condições dos testes. |
| Clima | Ative `MONITORA_OPCAO_ANALISE_CLIMA`. O próprio R consulta/cacheia NASA POWER. UAs na mesma célula compartilham os dados meteorológicos. |
| QField | Geração e importação de camadas têm padrão S. Coloque MBTiles e vetores em `qfield_input/`. Ausência de fontes válidas e bloqueios são informados no console, sem interromper os demais produtos. Confira auditorias e teste o ZIP no celular, inclusive em modo avião. |
| Manual | Ative sua geração; o PDF tem opção própria. O manual fica em `manual_usuario/`, ao lado de `input/` e `output/`. |

**NE** significa que o teste não pôde ser estimado sob o desenho disponível;
não demonstra ausência de efeito. No fogo, D indica diferença detectada;
E5/E10 indicam equivalência demonstrada nas margens exploratórias de ±5/±10
pontos percentuais. Ausência de significância não é equivalência. A ordenação
multivariada exploratória não identifica causas nem substitui o modelo
longitudinal conjunto. Consulte o status de cada análise antes de interpretar.

## Alertas temporais no QField

Referência temporal ambígua ou insuficiente é alerta quando as coordenadas são
utilizáveis. Preserve as observações e confirme os vergalhões em campo, sem
correção artificial nem justificativa obrigatória para a limitação temporal.
As camadas mantêm as alternativas por campanha e os rótulos indicam incerteza.
Divergências, possível troca/inversão, coordenadas inválidas e conflitos continuam
impedindo o projeto integral. Consulte as pendências e alertas do painel antes
da geração; aceitar uma posição para navegação não a torna automaticamente
elegível para análises científicas.

## Preservação e suporte

O contrato XLSForm e a execução por um único arquivo R permanecem preservados.
Guarde a rodada anterior, o arquivo executado, sua versão/build e os logs.
Quando houver falha, registre a etapa e a mensagem integral. Não contorne o
bloqueio alterando o contrato ou eliminando observações sem decisão documentada.

As planilhas, coordenadas, fotografias, nomes de coletores e relatórios locais
podem conter informações institucionais ou pessoais. O pacote público da versão
contém software e documentação; as rodadas das UCs têm entrega institucional
separada.


## Destinos espaciais e nomes QField

A UA de destino acompanha os filtros gerais e espaciais escolhidos. Selecionar
uma coleta para inspeção lateral não reduz a lista de destinos nem substitui o
destino escolhido. Limpar filtros restaura o domínio disponível; seleções que
ficam fora de um filtro deliberado são removidas. Confira coleta, UA e ano no
preview antes de aplicar a operação.

No QField, os extremos anuais aparecem como `verg_ini_YYYY` e `verg_fin_YYYY`.
Os nomes físicos das camadas e a identificação da UC nos dados são preservados.


## Numeração e elementos ausentes nos relatórios

Cada relatório reinicia as sequências de tabelas e figuras. A numeração é
atribuída depois de selecionar e ordenar o conteúdo: tabelas vazias, figuras
não selecionadas e análises inaplicáveis não reservam números. As seções têm
numeração hierárquica, refletida no índice. Consulte a auditoria de numeração
junto aos arquivos do relatório para conhecer os objetos incluídos e omitidos.

As tabelas do Word são editáveis. Nas tabelas com mais de seis colunas, os campos
são apresentados em identificação e resultados/contexto, preservando os valores.
No HTML/PDF, a legenda fica associada à tabela e é conferida antes do fechamento.
Não interprete a ausência de uma análise como evidência de ausência de efeito.


## Apresentação e índice no Word

Os relatórios analíticos em Word usam Arial e a paleta verde nos títulos, índice e links,
acompanhando a apresentação do PDF. Tabelas editáveis, figuras, referências,
código e símbolos são preservados; o ajuste gráfico não recalcula análises.

Depois de abrir o arquivo no Word, use **Ctrl+A, F9** para atualizar campos e
índice. Confira links, paginação e quebras de página antes da entrega: a mudança
de fonte pode alterar a paginação em relação ao PDF ou a uma rodada anterior.

## Figuras temporais em partes

Quando o conjunto não cabe com texto legível, o relatório detalhado apresenta
partes numeradas. Cada parte tem legenda e referência próprias; a contagem dos
resultados estatísticos continua pertencendo ao tema completo.

1. Leia a indicação “parte 1 de n” e siga todas as continuações citadas.
2. Confira períodos, formações, indicadores, legendas e referências em cada parte.
3. Preserve a pasta inteira do relatório ao transportar a rodada, incluindo
   figuras, continuações e CSVs de páginas, células e auditoria. Não selecione
   somente a imagem de nome antigo nem renomeie arquivos isoladamente.
4. Se faltar uma parte ou houver erro de integridade, preserve a mensagem e os
   arquivos para diagnóstico; não complete a figura manualmente com dados de
   outra rodada.

O Word e o PDF podem ter quebras de página diferentes. A paginação das figuras
não altera valores, inferências ou o histórico de justificativas. Consulte as
[notas da versão](RELEASE_NOTES_v3.0.8.md) para a cobertura e os limites dos testes.

## Caminhos, conexões e diagnóstico

Os relatórios analíticos ficam em `output/08_analises/u_<hash>/`, com nomes
`analitico_sintetico` e `analitico_detalhado`. Validação fica em `07_validacao`
e auditorias em `03_auditorias`; a identificação da UC permanece nos documentos e
metadados. Dados e linhagem preservam suas pastas canônicas. Use uma pasta de
execução curta e confira o índice antes de compartilhar. Para um destino maior
que a reserva padrão de 120 caracteres, informe o caminho completo em
`MONITORA_DESTINO_COMPARTILHAMENTO` antes de executar o script. O índice considera
210 caracteres para CSV/XLSX, 240 para documentos e 259 para os demais arquivos;
um destino arbitrariamente longo ainda pode ultrapassar esses limites.

Imagens e camadas adicionais entram em `qfield_input/`. A pasta invertida
`input_qfield/` gera aviso e não é importada automaticamente. O console informa
progresso dos recortes, tentativas de conexão, espera, duração e motivo das falhas.
Consulte `log/` para auditorias de consultas de fogo e avisos agrupados. Uma falha
na fonte não significa ausência de fogo; não descarte uma pendência com base nela.
A geração de PDF tem limite de tempo e preserva a causa do erro; um PDF antigo
não é aceito como resultado novo. Confira os produtos ao encerrar a execução.

## Registros históricos e relatórios

A exigência de hábito segue o protocolo do registro. No protocolo de 2023, a ausência de hábito da samambaia não impede a mudança de categoria. Não invente um hábito para contornar a validação. Em lotes mistos, preencha somente o que o protocolo exigir. Confira a prévia e a trilha da operação.

As tabelas de estado indicam a campanha utilizada; figuras temporais podem abranger períodos distintos. Evidências extensas são distribuídas em partes numeradas, em retrato, preservando fonte interna mínima de 7 pontos. Confira todas as partes: uma única imagem pode representar somente uma parcela dos resultados. Tabelas e figuras indisponíveis não reservam números na sequência final.


## Leitura dos relatórios analíticos

PDF e Word apresentam o mesmo conteúdo científico. O Word usa tabelas com bordas, valores e respectivos cabeçalhos centralizados; textos descritivos permanecem à esquerda. O índice contém referências de página e deve ser atualizado após alterações no documento. Todas as páginas permanecem em retrato.

As siglas são explicadas na primeira menção e reunidas no glossário final, por tema e em ordem alfabética dentro de cada bloco. Os títulos informam a métrica e o recorte temporal; presença de espécie exótica, isoladamente, não comprova invasão biológica.

As categorias gerais harmonizadas da base aprovada não são separadas pelo nome histórico do formulário. Formação, posição, continuidade temporal, pareamento e suporte amostral continuam condicionando as análises. O detalhamento de material botânico só é utilizado onde o instrumento de origem permite comprová-lo. A origem histórica é conservada na linhagem dos dados.

## Leitura dos relatórios

As figuras e tabelas recebem numeração conforme os elementos disponíveis para a UC. As sínteses informam o recorte, a formação e os limites dos achados; análise indisponível não demonstra ausência de efeito. Na seção multivariada, associações isoladas e incrementos exclusivos não são parcelas causais nem percentuais que possam ser somados. Consulte também as notas da versão.

## Alcance da validação desta versão

A candidata concluiu uma rodada manual PNB em `painel_incremental_completo`,
com R 4.6.0/data.table 1.18.4 e módulos de fogo, clima e Sentinel ativos.
Foram confirmados 10 documentos analíticos, 59 páginas de figuras e 1.238
resultados, além da conclusão da organização e do retorno ao prompt.
A publicação foi autorizada sem novas rodadas; não é homologação de todos os
13 modos, de todas as UCs, do QField em aparelho ou da importação no SISMONITORA.

Uma tentativa em outro ambiente, com R 4.6.2 na captura, terminou com alerta
fatal cuja causa permanece indeterminada. O sucesso em outra máquina não
comprova sua correção. Em caso de encerramento fatal, preserve a pasta, o
console completo, a captura, versão/build do script e as versões de R/RStudio
disponíveis para diagnóstico. Não confunda os logs de tentativas diferentes.

Na rodada conferida, oito caminhos CSV foram projetados entre 212 e 228
caracteres para limite de 210, embora os caminhos locais curtos tivessem de
122 a 138. Confira o destino institucional e o índice antes de compartilhar;
a publicação não certifica automaticamente a portabilidade da pasta.

A presença e a escala das figuras foram testadas automaticamente em Word/PDF.
Isso não substitui abrir o Word, atualizar campos e índice e conferir seus
links, páginas e continuações. Essa aprovação manual não foi inferida nesta
revisão. As notas da versão distinguem a rodada manual dos testes documentais.
