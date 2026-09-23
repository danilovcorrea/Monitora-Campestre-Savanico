# Candidata v3.0.2-rc01 — numeração dos relatórios

Build: `v3.0.2-rc01-20260923-r01`. Registro histórico da candidata promovida à **v3.0.2**; consulte [fechamento das rodadas e pendências](RELEASE_NOTES_v3.0.2.md). Base: versão pública v3.0.1, commit `8d99c18d7d4c85594eeb9765f1ff5be9ad93b019`.

## Alterações

- Tabelas passam a ter título descritivo, identidade estável e número, incluindo fogo, clima, calendário e análises complementares. Foram identificadas todas as 43 chamadas do gerador e dos módulos incorporados.
- Tabelas e figuras recebem números após a seleção e ordenação do conteúdo. As sequências reiniciam por UC e por relatório. Tabelas vazias e elementos não selecionados não reservam números.
- Seções e subseções seguem hierarquia numérica; o índice usa os mesmos títulos e destinos. Os painéis por ano de incremento ficam subordinados à subseção de sensibilidade.
- O Word preserva tabelas editáveis. Tabelas com mais de seis colunas são reorganizadas em identificação e resultados/contexto, mantendo os campos e valores de cada linha.
- HTML/PDF exibem a legenda em um parágrafo associado à tabela por `aria-labelledby`. Essa disposição evita um defeito de paginação que fazia a legenda desaparecer quando a tabela mudava de página. No Word, a legenda continua nativa.
- A composição produz `auditoria_numeracao_<nome_do_relatorio>.csv`, com identidades, números, títulos, seções e situação das tabelas. O motivo genérico de não seleção não é apresentado como comprovação de insuficiência de dados. Figuras emitidas são registradas; as omitidas continuam documentadas nos índices de seleção de gráficos já existentes.
- Os gates detectam sequência divergente, identidade duplicada, legenda sem tabela, referência sem destino e figura esperada ausente/vazia ou PNG corrompida. HTML reprovado é removido e não origina PDF.
- O manual gerado pelo script explica a numeração e a disposição das tabelas no Word.

## Verificação executada

| Verificação | Resultado |
|---|---|
| Sintaxe e expressão externa autônoma | Aprovadas |
| Contrato embutido | 129 campos idênticos à v3.0.1 |
| Cálculos dos módulos incorporados | Árvore sintática preservada, removidas da comparação somente as novas identidades editoriais; filtro de apresentação testado separadamente |
| Omissões | 256 combinações independentes de tabelas/figuras por sistema, Linux e Windows |
| Casos negativos | Duplicatas, salto hierárquico, referência ausente, imagem ausente/corrompida, legenda adulterada e HTML rejeitado detectados |
| Cinco formatos em Linux e Windows | Rmd, MD, HTML, DOCX e PDF aprovados nos ensaios com conteúdo opcional |
| PNB sintético | 7 tabelas e 8 figuras, numeração contínua |
| PNB detalhado | 32 tabelas e 76 figuras, numeração contínua |
| PDFs PNB | Texto extraído de 16 e 101 páginas: todas as legendas de tabelas e figuras presentes e ordenadas; páginas representativas inspecionadas visualmente |
| Tamanho LF | **4.859.117 bytes** |
| Tamanho CRLF | **4.943.177 bytes**, margem de **56.823 bytes** sob o limite de 5.000.000 |

O teste PNB reutiliza conteúdo analítico e figuras da recuperação selada, em cópias de trabalho. Não recalcula análises nem altera a rodada entregue. As linhas das tabelas de origem foram comparadas antes da renderização. A correção de paginação foi aplicada e conferida também nos PDFs; as evidências registram essa etapa complementar. Os exemplos preservam os metadados da rodada de origem e não constituem uma nova rodada oficial.

SHA-256 do script: `70f0a4bc0d1543d7da2ac4160281a63dd4fbafdcb14bd6671c4c47bbc429a4da`.

## Limites e pendências no fechamento inicial da candidata

As rodadas integrais e o pacote foram preparados posteriormente; o resultado atualizado está nas notas da v3.0.2.

- Não foi executada nova rodada integral de processamento dos dados. O escopo é editorial e os cálculos foram preservados.
- Abertura/uso operacional no RStudio Windows e inspeção no Microsoft Word permanecem pendentes; Rscript Windows e validação estrutural do DOCX não substituem esses ensaios.
- Não houve nova homologação do painel ou do QField nesta candidata; o código correspondente foi preservado.
- Não foram regenerados os manuais públicos congelados da v3.0.1 nem criado pacote de publicação. Esses artefatos serão preparados na promoção da versão.

## Evidências e reprodução

Evidências locais: `artifacts/v302_numeracao/`, incluindo `MANIFESTO_CANDIDATA.json`, logs Linux/Windows, auditorias por relatório e `PDFS_CONFERIDOS.json`. Contêm produtos institucionais de teste e **não integram a distribuição pública**.

1. `python3 tools/dev_numeracao_v302.py` recompõe a candidata a partir da fonte congelada v3.0.1 e dos helpers editoriais. O script distribuído continua autônomo.
2. Executar `tests/test_v302_numeracao.R`, `tests/test_v302_modulos.R`, `tests/test_v302_renderizacao.R` e `tests/test_v302_gates_adulteracao.R` com Rscript a partir da raiz do checkout.
3. Para reproduzir o ensaio real, definir `MONITORA_QA_PNB_RELATORIOS` como a pasta de relatórios de origem e executar `tests/test_v302_pnb_renderizacao.R`; todas as saídas são gravadas em `artifacts/v302_numeracao/`.
4. `tools/auditar_pdfs_numeracao_v302.py` confere as sequências nos PDFs e produz páginas para inspeção; requer `pypdf` e `pypdfium2`. Recebe a pasta de evidências e a pasta de saída como argumentos.

Os testes de renderização usam Chrome Linux por padrão; no Windows deve ser usado o resolvedor de navegador já existente no script, conforme o ensaio registrado.
