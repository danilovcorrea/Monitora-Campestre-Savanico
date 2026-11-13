# v3.0.8 — evidências temporais completas em páginas legíveis

Build: `v3.0.8-20261113-r01`.

## O que muda

Figuras temporais com muitos indicadores e períodos passam a ser divididas em partes numeradas. O problema foi comprovado no PNB: o conjunto de proporção relativa com 131 células excedia o espaço de uma única imagem e interrompia a geração dos relatórios. O caso de cobertura com 152 células também integra as verificações.

- O desenho mede cabeçalhos, rótulos e células no tamanho físico da figura, mantém fonte mínima de 7 pontos e distribui conjuntos extensos em páginas com até 8 polegadas de altura. Uma página continua sendo usada quando suficiente.
- Cada célula recebida aparece exatamente uma vez. Resultados não estimáveis e informações de composição permanecem representados; a paginação não altera valores, critérios de interpretação ou cálculos científicos.
- O relatório detalhado incorpora todas as partes em HTML, Word e PDF, com legendas, numeração e referências próprias. O índice analítico conserva uma identidade por tema e a contagem estatística não é repetida por página.
- Manifestos de páginas e células reconciliam cobertura, arquivos e hashes. Página ausente, conteúdo adulterado, duplicação ou conjunto incompleto provoca falha explícita. A gravação de um novo conjunto protege a geração anterior em caso de erro.
- A organização final preserva os componentes dos relatórios paginados diante de referências legadas com o mesmo nome de arquivo. As atualizações dos produtos legados continuam disponíveis.

## O que permanece preservado

O contrato XLSForm, os dados, a linhagem, as fórmulas científicas, os critérios de bloqueio e o painel permanecem preservados. O arquivo R continua autônomo; arquivos de construção e teste não precisam acompanhar a execução do usuário.

A continuidade de justificativas da v3.0.7 permanece ativa. Justificativas de não resolução têm histórico próprio, separado do ledger das correções dos dados. Transporte `registros_corrig.csv` junto com toda a pasta de linhagem da mesma saída anterior. Justificar uma ocorrência não altera sua severidade nem resolve uma impeditiva; novas ocorrências do mesmo tipo continuam exigindo avaliação.

Arial e a paleta verde dos relatórios Word, tabelas editáveis e referências dinâmicas são mantidos. A novidade desta versão é a distribuição das evidências em partes, com conferência integral do conjunto.

## Evidências de validação

As provas operacionais referem-se à candidata `3.0.8-dev_r04`. A promoção preserva seu comportamento e altera somente os identificadores de versão, build e arquivo.

- Os casos reais PNB de 131 e 152 células, suas ordens invertidas e os controles de integridade passaram pelas verificações da candidata incorporada no arquivo único. Foram conferidos períodos, formações desiguais, rótulos extensos, estados não estimáveis e falhas de gravação/adulteração.
- O replay documental com tabelas prontas confirmou 12 conjuntos inferenciais, 59 páginas e 1.238 resultados. A auditoria de HTML/Word/PDF passou por 478 verificações: presença única das partes, mídia/pixels preservados e escala sem deformação ou corte. Nos documentos examinados, a fonte mínima após inserção foi de 7,28 pontos no Word e 7,06 no PDF. Esse replay não substitui a rodada manual.
- A rodada manual PNB de 13/11/2026, identificada como `20261113_185645`, concluiu o modo `painel_incremental_completo` no Windows, com R 4.6.0 e data.table 1.18.4. Fogo, clima e Sentinel foram executados. O console concluiu organização/índice, cadeia e restauração e retornou ao prompt; a conferência reconciliou 1.027 hashes e confirmou os 10 documentos analíticos, 59 páginas e 1.238 resultados.
- O organizador foi exercitado em cópia, inclusive com nomes legados coincidentes, destino projetado longo e relatórios sem o marcador novo. Os conjuntos paginados permaneceram íntegros e as referências legadas externas continuaram sendo atualizadas.
- Foram conferidos parse, autonomia, inicialização, contratos e tamanho LF/CRLF. O script público tem **4.799.986 bytes LF / 4.880.527 bytes CRLF**, abaixo de 5.000.000 bytes nas duas terminações.

SHA256 do script público: `0dcbacc8841d9fd58e5b9fcd3c500589bc6f14ce2f2b0d31c3b3d1eeaf820ca1`. Os manifestos de hashes acompanham os artefatos finais. Dados, relatórios e logs reais das UCs ficam fora do pacote público.

## Limites e cuidados

A publicação do software foi expressamente autorizada sem novas rodadas de homologação. A cobertura descrita não é homologação irrestrita dos 13 modos, de todas as UCs ou de todos os ambientes. Não foram repetidos testes em aparelho QField/offline nem importação no SISMONITORA.

Uma tentativa PNB em outro ambiente, cuja captura mostra R 4.6.2, terminou com **R Session Aborted** após a limpeza e antes da conclusão terminal observável. Os logs atualizados são coerentes com essa captura, mas não identificam o módulo nativo, o código da exceção ou a causa. RAM/RSS não foram medidos validamente no Windows. A conclusão na máquina com R 4.6.0 não demonstra correção desse fatal nem estabelece qual diferença de ambiente o explica.

O índice da rodada manual sinaliza **oito projeções de caminhos CSV entre 212 e 228 caracteres**, para limite recomendado de 210. Na pasta local curta, os caminhos reais correspondentes têm de 122 a 138 caracteres. Esses avisos não demonstram falha de abertura local, mas mantêm pendente a portabilidade da entrega no destino de compartilhamento. Informe `MONITORA_DESTINO_COMPARTILHAMENTO` e confira o índice antes de transportar a rodada.

A verificação automática das imagens no Word não comprova os destinos de seu índice nem toda a paginação nativa. A aprovação manual dos índices desta revisão não foi presumida. Após abrir o Word, aguarde a paginação, use **Ctrl+A, F9** e confira links, páginas, legendas e todas as continuações.

## Como usar

Execute somente `monitora_campsav_alvo_global_v3.0.8.R`, conforme o [guia operacional](GUIA_USUARIO_v3.0.8.md) e o [manual completo](manual_usuario/manual_usuario_v3.0.8.html). Guarde a rodada de origem e transporte a pasta do relatório completa, incluindo figuras, continuações e CSVs de auditoria. Não renomeie ou selecione isoladamente a primeira imagem de um conjunto paginado.
