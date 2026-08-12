# Monitora Campestre-Savânico v2.9.7

Publicação de 12 de agosto de 2026, baseada integralmente na v2.9.6 aprovada.

## Linhagem completa das sessões

A v2.9.7 torna cumulativos os metadados usados pela seção 8.1 do relatório de
validação. Cada continuidade incremental passa a preservar, por `exec_id`, data
e hora, build, modo de execução, responsável, instituição, forma de
encerramento, número de operações e número de itens auditáveis.

O novo `metadados_sessoes_painel_consolidado.csv` acompanha a linhagem, tem hash
e cardinalidade registrados no `manifesto_linhagem.json` e é verificado antes
da importação e depois da organização final do output. Ausência indevida,
adulteração, conflito ou divergência bloqueiam a continuidade antes de aceitar o
histórico.

## Recuperação histórica única

Cadeias anteriores podem ser recuperadas uma única vez a partir dos sidecars
das sessões. A ferramenta de migração reconstrói somente a ancestralidade
canônica ligada por `revision_id` e `parent_revision_id`, exclui e audita runs
de erro ou ramificações paralelas e vincula o artefato à revisão, ao ledger e
aos hashes físicos correspondentes.

Depois da primeira execução na v2.9.7, o consolidado assinado substitui o
artefato transitório: a migração não é propagada nem repetida. As rodadas
seguintes continuam copiando apenas `registros_corrig.csv` e a pasta
`output/02_painel_correcoes/linhagem/` completa para o novo `input/`.

## Relatório de validação

A seção 8.1 passa a usar o ledger cumulativo para preencher todas as sessões,
inclusive execuções sem novas decisões semânticas. O relatório e os CSVs
editáveis apresentam a cronologia, as contagens e verificações explícitas de:

- quantidade de metadados no manifesto e no ledger;
- execuções sem metadados persistidos;
- coincidência entre o hash do manifesto e o arquivo físico;
- preservação dos eventos herdados e falhas de aplicação registradas.

## Compatibilidade, desempenho e preservação

A implementação opera somente sobre tabelas pequenas de sessões e arquivos já
carregados na linhagem. Na homologação nativa do Windows, o inventário levou
0,03 s para FNCS e 0,02 s para APAI. A segunda geração foi validada sem reutilizar
os artefatos de migração, e adulterações simuladas foram bloqueadas.

A arquitetura externa de uma única expressão `base::evalq`, homologada para o
botão **Source** do RStudio no Windows, permanece inalterada. Também permanecem
preservados contrato único, itens congelados, 13 modos, painel, replay,
produtos de dados, estatísticas, relatórios analíticos, Sentinel-2, cartografia,
KML/KMZ, importação SISMONITORA e cautela causal.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.7.R`. O build exibido no console é
`v2.9.7-20260812.1`.
