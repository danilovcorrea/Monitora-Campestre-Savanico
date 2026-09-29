# Candidata 3.0.5-rc01

Build homologado: `v3.0.5-rc01-20260929-r07`. Não publicado.

A revisão corrige a equivalência editorial dos relatórios analíticos Word/PDF: preserva todas as colunas das tabelas no Word, separa os tópicos do resumo, aplica bordas e larguras compatíveis com os conteúdos e usa folhas em paisagem para tabelas largas. A exportação PDF associa explicitamente a folha física às dimensões calculadas pelo paginador. O índice Word permanece vinculado aos títulos e exige conferência após atualização, salvamento e reabertura no Microsoft Word.

Os relatórios explicitam métricas e recortes da campanha mais recente, contextualizam limitações de calendário, removem motivos climáticos repetidos, resolvem referências a tabelas e figuras pela identidade do elemento e incluem a ressalva de que presença de exóticas não comprova invasão. O glossário contém as siglas presentes em cada documento; a primeira leitura recebe a expansão pertinente. Números de tabelas/figuras não são fixados no texto.

A homologação encontrou dois problemas preexistentes de reprodutibilidade: a comparação editorial por período dependia do estado aleatório anterior, e o perfil de memória podia reduzir o número de reamostragens científicas. A comparação por período agora tem sementes por identidade técnica; as quantidades solicitadas são preservadas em todos os perfis. Fórmulas, pareamento, dados canônicos e curadoria não mudam. Resultados Monte Carlo antigos sem estado aleatório preservado não devem ser tratados como referência determinística.

## Critérios e evidências

- Script abaixo de 5.000.000 bytes em LF e CRLF; alias idêntico e leitura no R nativo Windows.
- Testes de 256 combinações de omissões e referências; recusa de arquivos ausentes/corrompidos e sequências inválidas.
- Comparação completa de células, narrativa, links e imagens entre HTML/PDF e Word; as capas equivalentes têm montagem própria.
- Conferência de todas as páginas físicas PDF, colunas e sequências, seguida de revisão visual. O DOM do navegador, isoladamente, não aprova a impressão.
- Paginação Word conferida após salvar/reabrir, comparando cada campo com a página inicial do título correspondente.
- Rodadas integrais FNB e PNI com todos os produtos, dados e linhagem preservados; QField com imagem institucional Z18.
- Caminhos avaliados no destino efetivo do OneDrive, inclusive o orçamento conservador para planilhas.

Homologação concluída em 29/09/2026: FNB e PNI executadas integralmente com o mesmo build, 22/22 produtos cada. Foram examinadas 529 páginas Word/PDF; tabelas, figuras, narrativa e índices passaram. Dados, UUIDs e linhagem preservados. Script: 4.895.841 bytes LF / 4.980.153 bytes CRLF. Entregas em dados_pre_validados/{FNB,PNI}/v305_rc01, com pareceres, gates e índices. Testes automáticos não substituem a revisão editorial/visual. A navegação física no aplicativo QField e cliques no RStudio não são alegados como ensaiados nesta revisão de relatórios.

## Testes

`test_v305_editorial.R`, `test_v305_larguras.R`, `test_v305_numeracao.R`, `test_v305_contrato.R` e `test_v305_reprodutibilidade.R` e `test_v305_siglas_figuras.R` verificam as funções reais da candidata. `test_v305_paginacao.R <pasta dos relatórios>` produz o manifesto do paginador; `test_v305_pdf_fisico.py <raiz da rodada>` compara esse manifesto com os PDFs finais usando pypdf. Os relatórios institucionais utilizados nos gates não são incorporados ao repositório.

O inventário de siglas cobre também textos rasterizados dos mapas e os códigos dos painéis de evidência. O CSV por relatório vincula cada inventário ao checksum da figura; siglas de estados são contextualizadas para não alterar o significado científico de PA. Legendas ficam junto à primeira linha de dados; blocos curtos recebem proteção adicional contra fragmentação. Continuações de tabelas preservam os cabeçalhos.
