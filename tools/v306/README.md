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

## Revisão r02 — narrativa do relatório completo

- Os textos editoriais usam os recortes da própria UC: formação, métrica, período, população e elegibilidade. Ausência de resultado não se transforma em ausência de efeito. Há testes com formação única, lacunas, um ano, valores constantes, módulo desativado e falta de suporte espacial.
- O resumo descreve o histórico cartográfico de fogo e as condições climáticas dos 90 dias antecedentes às coletas, com limites explícitos. Não apresenta esses valores como tendência climática ou efeito causal.
- “Época de amostragem” e “reamostragens” substituem os termos anteriores na apresentação. A continuidade das UAs distingue primeira ocorrência na série, manutenção, não reamostragem e retomada, comparando anos observados sucessivos.
- A sequência contextualização → elemento/legenda → descrição dos achados → interpretação foi revisada por conjunto analítico. A descrição específica das trajetórias aparece depois da figura; a discussão climática vem depois das transições. As referências permanecem semânticas e dinâmicas.
- Notas automáticas anteriores são removidas antes de regenerar as definições. Notas substantivas são preservadas. A tabela de percentuais de esforço tem uma nota contextual única.
- `nar_contrastes.csv`, `nar_composicao.csv`, `nar_prioritarias.csv` e `nar_contexto.csv` documentam resultados harmonizados, a seleção efetiva da figura prioritária e o estado dos módulos. Não são novas análises.
- O módulo interno usa compressão XZ, disponível no R base; a leitura foi conferida no R Windows. A candidata tem 4.895.153 bytes em LF e 4.979.387 em CRLF, abaixo do limite de 5.000.000.

Verificações adicionais: `Rscript tests/test_v306_contextos.R`. Evidências r02 em `pnm_v306_r02_20260929`, fora do Git. Para PNM foram reaproveitados 147 CSVs idênticos aos da revisão anterior; somente duas figuras de época foram redesenhadas para atualizar os textos. Os dois relatórios foram regenerados em Rmd, Markdown, HTML, PDF e DOCX. A revisão independente reconciliou as sínteses prioritárias, de fogo, correlações climáticas, transições e predição com os CSVs. A aprovação editorial do usuário e os demais gates de promoção permanecem pendentes.

A inspeção visual do Word detectou um cabeçalho órfão após a ampliação dos textos. A revisão r02 vincula cada cabeçalho à primeira linha de dados, mantendo o restante da tabela paginável. Há teste de regressão e conferência nativa em todas as tabelas de PNM.

## Revisão r03 — consistência das tabelas

Formações identificadas em colunas próprias são apresentadas como Campestre e Savânica na camada comum de tabelas, inclusive nas opcionais. A normalização atua na cópia de apresentação; preserva dados, ausências, outros valores e frases corridas. Os cabeçalhos HTML/PDF passam a usar alinhamento vertical central, acompanhando o Word, sem alterar o alinhamento horizontal.

Verificação pontual com o formatador real e tabela HTML: rótulos, preservação do objeto de entrada e estilos. Nenhum relatório de PNM foi regenerado nesta revisão. Artefato r03: 4.895.281 bytes em LF / 4.979.515 em CRLF, abaixo de 5.000.000 bytes. Permanece candidata não publicada.
