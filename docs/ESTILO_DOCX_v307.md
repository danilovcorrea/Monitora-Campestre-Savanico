# Estilo dos relatórios analíticos Word — r09

O gerador aplica a paleta verde e a família Arial usadas no PDF aos títulos,
corpo, índice e hyperlinks do DOCX. Reconhece nomes de estilos localizados e
remove atributos concorrentes do tema Office. Preserva código e fontes de
glifos, conteúdo, tabelas, imagens, bookmarks e campos dinâmicos de referências.

Implementação: `tools/v307/estilo_docx.R`, incorporado ao arquivo autônomo.
O renderer chama o ajuste depois da capa/tabelas e antes da auditoria de figuras
e da publicação do DOCX. O novo estilo não recalcula análises científicas.

Validação da r09: 16 verificações focais; parse e contratos em R Linux/Windows;
457 referências preservadas; delta binário limitado ao módulo de estilo, um
hook editorial e identificadores. LF 4.915.983 / CRLF 4.999.792 bytes.

O detalhado para revisão reutiliza Rmd, HTML e recursos finais da rodada APAI,
preservando seus metadados originais. Gera DOCX pelos estágios de produção da
r09 e PDF novamente a partir do HTML. A mudança tipográfica pode alterar a
paginação do Word; os campos permanecem dinâmicos para atualização no Word.
A revisão humana e a autorização posterior do usuário antecedem a publicação.
