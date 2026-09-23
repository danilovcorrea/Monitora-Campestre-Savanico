# v3.0.2 — numeração dos relatórios analíticos

Os relatórios passam a numerar tabelas, figuras e seções depois da composição
final. Elementos vazios, inaplicáveis ou não selecionados não deixam lacunas;
as sequências reiniciam em cada relatório e UC. Todas as tabelas identificadas
no gerador e nos módulos incorporados receberam título e identidade editorial.

O Word preserva tabelas editáveis e reorganiza as largas sem descartar campos
ou valores. No HTML/PDF, a legenda é associada à tabela por um parágrafo visível,
evitando sua perda nas quebras de página. O índice acompanha a hierarquia das
seções. Auditorias detectam identidade duplicada, sequência divergente,
referência ausente e figura ausente, vazia ou corrompida. HTML reprovado não
origina PDF. O manual explica a numeração e a disposição das tabelas.

## Verificação

Passaram 256 combinações de omissão por sistema, renderização nos cinco formatos
em R Linux e R Windows e casos negativos de integridade editorial. Os módulos
analíticos mantêm os cálculos; o contrato conserva os 129 campos. A composição
PNB foi re-renderizada em cópias, sem alterar a recuperação entregue.

Foram executadas rodadas integrais de PNGSV, FNCS e PNCV com todos os produtos
habilitados. As três concluíram sem interrupção; bases corrigidas e históricos
de correção permaneceram idênticos às fontes. Os relatórios analíticos nos cinco
formatos tiveram integridade conferida. Nos PDFs, todas as legendas de tabelas
e figuras foram extraídas e suas sequências comparadas à auditoria; páginas
representativas foram inspecionadas visualmente.

| UC | Relatório | Tabelas | Figuras | Páginas PDF |
|---|---|---:|---:|---:|
| PNGSV | sintético | 5 | 6 | 11 |
| PNGSV | detalhado | 20 | 16 | 27 |
| FNCS | sintético | 7 | 8 | 14 |
| FNCS | detalhado | 30 | 64 | 82 |
| PNCV | sintético | 7 | 8 | 15 |
| PNCV | detalhado | 32 | 69 | 92 |

As bases de homologação possuem impedimentos preexistentes: PNGSV não gera o
XLSX de importação por falta de coletor em uma coleta; FNCS e PNCV não geram o
QField por pendências espaciais. A comparação com as auditorias da v3.0.0
confirmou os mesmos impedimentos de dados. Em PNCV, a mensagem atual aponta
diretamente uma longitude inválida, preservada da origem; a proteção já estava
presente na v3.0.1. Eles permanecem explícitos nas entregas
institucionais e exigem curadoria; não foram contornados nem classificados como
produtos gerados. O QField da PNGSV passou na conferência estrutural do projeto,
nomes anuais, manifesto, ZIP e dois fundos offline. A promoção editorial se apoia
na ausência de regressões, nos relatórios aprovados e no tratamento não fatal
desses produtos opcionais, preservando os bloqueios de dados.

## Compatibilidade e pendências

O R permanece autônomo: 4.859.107 bytes LF / 4.943.167 bytes CRLF, ambos inferiores
a 5.000.000. Entre candidata testada e fonte final mudaram apenas versão e build;
a equivalência foi conferida byte a byte após normalização desses dois valores.
O pacote público contém somente software e documentação.

Permanecem pendentes os ensaios operacionais por Source no RStudio Windows,
inspeção no Microsoft Word, navegação no QField móvel inclusive offline e
homologação da atualização por UUID no SISMONITORA. Rscript Windows, validação
do DOCX e conferência estrutural dos pacotes não substituem esses ensaios.
Não se declara homologação operacional integral nem validação científica
independente de todas as análises. As fontes ambientais seguem as opções de cache e a proveniência registradas
nas rodadas; não foi solicitada atualização integral dessas fontes.
