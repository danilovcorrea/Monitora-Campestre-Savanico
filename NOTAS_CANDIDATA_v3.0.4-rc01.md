# Candidata 3.0.4-rc01 — FNB e PNM

Base pública: v3.0.3. Build de entrega: v3.0.4-rc01-20260925-r08. Execuções integrais r06; revisão documental r07; ajuste final de nomes r08.
Status: homologação automatizada por etapas concluída; ensaios manuais pendentes. Sem publicação ou promoção autorizada nesta etapa.
Dados e evidências detalhadas permanecem nas áreas institucionais locais.

## Correções

- A obrigação de hábito acompanha o protocolo de cada registro, inclusive em movimentos individuais, lotes mistos, triagem, prévia e reaplicação. Samambaia do protocolo de 2023 pode mudar de categoria com hábito ausente. Texto de espécie em coluna histórica não é convertido em hábito, mesmo se estiver escrito “terrestre”.
- Movimentos em lote conservam descritores e são cancelados integralmente diante de conflito. Substituições na mesma lista preservam os dependentes da forma que já existia no destino. Falhas na sanitização de desconhecida ou no recálculo de Encostam impedem alterações parciais.
- Seletores do painel recalculam a obrigação ao trocar coleta ou lote. Edições compostas limitam o preenchimento às identidades que efetivamente exigem hábito.
- As dependências de geração do PDF são declaradas antes do fluxo incremental, corrigindo a falha de relatório de validação desse modo.
- Painéis de evidência temporal são divididos por formação, em blocos de até três períodos e seis indicadores. Todos os resultados permanecem nas figuras e auditorias; cálculos e critérios estatísticos são preservados.
- Tabelas de estado apresentam a campanha efetivamente utilizada. A paginação mantém legenda e tabela juntas quando cabem, limita a largura e elimina cabeçalhos duplicados na mesma tabela paginada.
- Pastas passam a usar palavras reconhecíveis: auditorias, ocorrencias, apoio, operacoes e cache. Nomes repetitivos de arquivos/contextos foram reduzidos; leitores preservam compatibilidade com layouts anteriores. O verificador de ocorrências usa a identidade lógica dos arquivos, e a comparação pré/pós reconhece a fase incremental.

## Verificação já concluída

- 256 combinações de omissões independentes na numeração, além de arquivos ausentes/corrompidos e sequência adulterada.
- Oito cenários de movimento: 42 pontos reais, protocolo moderno, espécie histórica, lote misto, conflito com rollback, reordenação/repetição, falhas posteriores à substituição de desconhecida e preservação intralista.
- Teste Shiny com ambientes léxicos separados e operação composta restrita ao UUID moderno.
- Compatibilidade de 50 prefixos e de 743 nomes reais de produtos anteriores.
- AST dos módulos incorporados: cálculos, símbolos, estrutura e constantes numéricas preservados.
- Teste gráfico: 194 resultados mantidos em 18 painéis.
- Ensaio de paginação do relatório anterior do PNM: 78 páginas sem cabeçalhos duplicados nem tabelas fora das margens laterais. Os relatórios finais também passaram na conferência de sequência, margens, cabeçalhos, células numéricas e preservação de linhas.
- Limite obrigatório: 4.879.093 bytes LF / 4.963.393 bytes CRLF, inferior a 5.000.000 bytes.

## Rodadas finais

- FNB: recuperação autorizada de 42 pontos nas três coletas informadas; hábito ausente preservado. Conferência célula a célula confirmou alterações somente nas listas de formas de vida e Encostam. A rodada completa herda a linhagem dessa recuperação.
- PNM: dados corrigidos e linhagem da entrega anterior, sem nova curadoria.
- Todos os produtos habilitados, com MBTiles de alta resolução em qfield_input.
- Diretório de trabalho: /home/dlinux/Monitora_Dev_20260712/atualizacoes_v304_20260925.
- Teste adicional em R Windows: C:/Monitora_v304_homologacao_20260925/FNB_checkpoint_r04.

A primeira tentativa revelou dependência de prefixo físico no verificador de ocorrências; o problema foi corrigido e coberto por teste. Tentativas substituídas estão preservadas e não são produtos homologados. A execução isolada pelo Rscript Windows precisou indicar o Pandoc instalado com o RStudio; isso foi configurado no iniciador do teste.

## Pendências

A interação completa no RStudio Windows e a navegação no dispositivo QField não foram executadas; testes automatizados não equivalem a esses ensaios manuais.

## Revisões finais — r05/r06

Sete famílias de arquivos ainda usavam nomes longos montados em tempo de execução. Os construtores foram corrigidos e testados, inclusive no R Windows.

A inspeção dos PDFs encontrou números quebrados em colunas estreitas. O HTML agora usa largura automática, preserva cabeçalhos e impede quebra interna de números, notação científica e indicadores de não estimabilidade. O ensaio PDF passou; a paginação final verifica também largura das células numéricas.

A revisão independente demonstrou que a camada de composição censurava anos/esforços no extremo negativo e valores válidos no extremo superior. Ambos os extremos do corredor gráfico são preservados. Testes com dados científicos existentes recuperaram dez etiquetas temporais e o rótulo de 91,7% de serrapilheira campestre de 2024. Dois rótulos externos de 2026 tiveram sua separação ampliada. Contagens, proporções e estatísticas não foram alteradas.

O build r06 foi congelado para novas execuções integrais FNB/r03 e PNM/r04. Tentativas anteriores e o ensaio de migração permanecem preservados; não compõem a entrega final. A migração para o destino Windows ajustou somente nomes e referências operacionais, sem recálculo. CODIGO.json distingue a fonte integral executada r06 e a candidata entregue r08.

## Recuperação documental r07 e margem de caminhos r08

Na revisão integral r06, a inclusão da campanha expôs um defeito no resumo prioritário: o seletor interpretou o ano como indicador. O sintético da FNB também entrou em repetição de páginas por manter colunas redundantes. A r07 separa explicitamente campanha/indicador, exige uma única coluna de categoria e permite quebra apenas no bloco dessa tabela. A recuperação usa os CSVs científicos existentes e o renderer de produção, em uma cópia separada; a fonte e a falha originais são preservadas. Todos os IDs e números editoriais são comparados antes/depois.

A r08 encurta três nomes adicionais para respeitar também a reserva mínima de 120 caracteres da raiz Windows, mantendo os leitores compatíveis. Não há novo cálculo nessa migração. Não se alega uma nova execução integral r08: a homologação final combina execuções integrais r06, recuperação documental r07 e conferência dos caminhos r08. Os registros CODIGO.json, RECUPERACAO_DOCUMENTAL.json e CAMINHOS.json documentam cada etapa.

## Resultados finais

| UC | Produtos | Detalhado: tabelas/figuras/páginas | Sintético: tabelas/figuras/páginas | Execução integral |
|---|---|---|---|---|
| FNB | 22/22 | 32/120/144 | 7/8/16 | 28min46s; PDF sintético recuperado posteriormente |
| PNM | 22/22 | 31/120/141 | 7/8/15 | 39min51s; tabela do sintético corrigida posteriormente |

Numeração contínua conferida nos PDFs e índices, inclusive com elementos omitidos. Os 129 campos foram preservados: 21.008 linhas na FNB e 31.209 no PNM. Comparação entre rodadas r04 e r06 confirmou identidade das 23 tabelas científicas da FNB e das 21 do PNM verificadas. Migração conferida por hashes, vínculos HTML/OOXML e integridade dos pacotes QField. Nenhum caminho excede os limites institucionais no destino de entrega; os construtores também foram testados com reserva de 120 caracteres para a raiz.

No PNM, os seis avisos de remoção nas figuras 25–28 correspondem exclusivamente a proporções indefinidas (denominador zero): sete categorias em 2019, 2021 e 2023, totalizando 21 linhas por formação. Os construtores reais reproduziram os avisos e preservaram todas as 28 proporções válidas por formação. Evidências acompanham a rodada em evidencias_avisos/. Os avisos originais não foram apagados. As ausências de estimativas do modelo de época estão documentadas nos diagnósticos de elegibilidade; não foram inventados resultados para completar arquivos.

As pastas finais ficam em SISMONITORA/dados_organizados/dados_pre_validados/{FNB,PNM}/v3.0.4-rc01. LEIA_ME.md, CODIGO.json, RECUPERACAO_DOCUMENTAL.json, CAMINHOS.json e ENTREGA.json registram procedência, limitações e hashes. A cópia local na pasta OneDrive não comprova conclusão da sincronização na nuvem.
