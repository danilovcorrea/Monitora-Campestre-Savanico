# v3.0.7 — continuidade de justificativas e estilo dos relatórios

Build: `v3.0.7-20261112-r01`.

## O que muda

- Ocorrências com justificativa vigente deixam de ser oferecidas para novo
  registro e continuam disponíveis para consulta. O histórico acompanha a
  linhagem da mesma base; diagnóstico e severidades permanecem preservados.
- A recuperação verifica dados, histórico e manifesto. Arquivo declarado
  corrompido provoca erro explícito. Cada novo Source inicia outro identificador
  e limpa o estado reservado; a gravação conjunta de dados e linhagem tem
  verificação prévia e restauração em caso de falha.
- O cache é validado antes de importar a linhagem. Fontes anteriores dos
  relatórios de apoio são preservadas; cópias legadas de compatibilidade recebem
  caminhos curtos somente quando necessário, com mapa e hashes das fontes.
- Os relatórios analíticos em Word usam Arial e a paleta verde nos títulos, índice e links
  normais ou visitados. A apresentação acompanha o PDF, preservando tabelas,
  figuras, referências dinâmicas, código, glifos e conteúdo científico.

## Como continuar uma rodada

Copie `output/01_produtos_dados/registros_corrig.csv` para `input/` e **toda** a
pasta `output/02_painel_correcoes/linhagem/` para `input/linhagem/`. Preserve a
rodada anterior e não selecione somente os arquivos de linhagem já conhecidos.

Justificativas de não resolução têm histórico próprio, separado do ledger que
explica correções dos dados. Justificar não resolve uma impeditiva, não altera
sua severidade e não exclui a ocorrência do diagnóstico. Novas ocorrências do
mesmo tipo ainda precisam ser avaliadas.

Histórico legado que não acompanhou os dados exige recuperação auditável com
as fontes, o snapshot e a revisão correspondentes. Não edite manifestos nem
registre novas justificativas para simular essa recuperação. Cache antigo ou
incompatível deve ser regenerado; decisões de uma base corrigida não devem ser
emprestadas ao cache anterior.

## Validação no escopo aprovado

Nas duas rodadas manuais APAI, a segunda entrada preservou os **61 eventos** da
primeira saída, incluindo os 19 campos herdados, e acrescentou **1.221 eventos**
sem justificar novamente as ocorrências já reconhecidas. O resultado dessa
cadeia foi **1.282 justificativas vigentes e 781 ocorrências sem justificativa**.
Dados corrigidos e ledger semântico permaneceram inalterados.

Essas rodadas começaram sem o histórico antigo no input; não recuperaram
automaticamente as 2.063 justificativas da auditoria original. A preservação de
**2.063 eventos na continuidade A → B → C** foi uma prova técnica separada.
As provas anteriores também incluem FNB com 22/22 produtos e testes focais;
não são novas execuções integrais de todas as UCs na revisão gráfica.

A revisão gráfica passou por **16 verificações focais** e gates estáticos em R
Linux e Windows. Os sete módulos anteriores permaneceram byte a byte idênticos.
Foram preservados 457 arquivos de referência e 49 fontes/cópias da prévia.
No detalhado APAI de conferência, mantiveram-se 36 tabelas, 43 mídias e 17 campos
PAGEREF, sem recalcular análises. O usuário confirmou o funcionamento no Word,
revisou a prévia r09 e autorizou a publicação.

## Limites e cuidados

A validação corresponde ao escopo aprovado. Por instrução do usuário, não foram
executados todos os 13 modos nem novas rodadas em todas as UCs de regressão.
A auditoria das rodadas manuais comprova registro, salvamento e continuidade;
não documenta cada filtro, navegação ou reinício de R. Não se repetiram testes
em aparelho QField/offline nem importação no SISMONITORA.

Os índices das duas rodadas institucionais mantêm **três avisos de caminho por
rodada**: um CSV com 212 caracteres para limite recomendado de 210 e dois
arquivos de cache com 271 para limite de 259. Esse gate de entrega não foi
aprovado. A publicação do software não certifica a portabilidade dessas pastas;
confira o índice e os caminhos reais antes de compartilhar.

Após abrir o Word, atualize os campos e o índice com **Ctrl+A, F9** e confira a
paginação. Uma mudança de fonte pode alterar as quebras de página.

O arquivo R continua autônomo: **4.915.963 bytes LF / 4.999.772 bytes CRLF**,
abaixo de 5.000.000 bytes decimais nas duas terminações.
SHA256: `db6333c5e275af245e7d8d069f8bdeb259acf1cae4d46de7187b53fe475816d2`.
Os hashes finais acompanham os arquivos da versão. Bases, relatórios e logs das
UCs permanecem fora do pacote público.
