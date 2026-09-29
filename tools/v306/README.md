# Candidata 3.0.6-rc01 — revisão narrativa

Estado: desenvolvimento para avaliação de PNM; não publicada. A aprovação editorial do usuário e os demais gates de promoção continuam pendentes.

## Escopo

- Seção multivariada: explicita pergunta, painel, mudanças observadas, associações isoladas e incrementos condicionais, sensibilidade à escala, denominadores dos eixos e interpretação das duas figuras. A discussão encerra a seção com conclusão específica do recorte e limites de atribuição.
- Distingue a ordenação da parcela ajustada das diferenças entre extremos da PCA das coberturas observadas. O universo dos eixos e o painel constante exibido são informados separadamente.
- Seção de evidências e gestão: introdução com referência semântica, resolvida pela numeração existente. Retira os três resumos desconectados; seus conteúdos permanecem nas seções temáticas e no resumo executivo.
- As fórmulas, critérios de seleção e produtos científicos existentes são preservados. Oito CSVs `mv_*` exportam metadados dos objetos já calculados para a narrativa. A geração limpa esses metadados antes de avaliar a elegibilidade, evitando reaproveitamento indevido de resultados antigos.

## Construção e verificações

`python3 tools/v306/build.py` produz somente a candidata; a versão pública 3.0.5 e seus aliases permanecem intactos. `Rscript tests/test_v306_editorial.R` verifica referências condicionais, ausência de resultados elegíveis, painel, denominadores e limite de 5.000.000 bytes inclusive em CRLF.

Avaliação documental PNM: reutilização dos resultados da homologação 3.0.5; execução apenas dos módulos integrados para obter metadados e regeneração dos documentos pelo renderer real. Não representa uma nova rodada integral nem homologação das demais UCs. Evidências institucionais ficam fora do Git em `pnm_v306_r01_20260929`, ao lado do repositório.

Conferências executadas na preparação de PNM: preservação dos CSVs científicos, paridade de texto/tabelas/imagens, referências, paginação, margens, retrato, figuras sem deformação, ausência de páginas vazias e salvar/reabrir o Word com índice atualizado. Revisão visual dirigida das seções alteradas complementa os testes automáticos; não constitui leitura visual de cada página do documento.
