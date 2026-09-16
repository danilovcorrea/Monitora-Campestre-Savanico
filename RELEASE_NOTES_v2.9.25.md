# Monitora Campestre-Savânico v2.9.25

Publicada em 15 de setembro de 2026 sobre a v2.9.24.

Publicação substitutiva `r02`, de 16 de setembro de 2026, autorizada antes do
uso da publicação `r01`. O serial permanece `2.9.25`; o build interno distingue
inequivocamente as duas materializações.

## Relatório de validação

- Substitui referências editoriais a “bolsistas” por “usuários” e padroniza a
  numeração dos títulos com ` - `.
- Separa sessão atual, sessões herdadas e total documental desde a origem,
  incluindo número de sessões, operações, efeitos físicos e atos técnicos.
  Efeitos sem evidência completa são **não quantificáveis**, não zero presumido.
- Detalha histórico anual de ocorrências e correções, padronização da equipe,
  decisões espaciais e coletas excluídas, com motivo e situação documentados.
- Mantém unidade de contagem e denominador explícitos; evita percentuais com
  precisão excessiva ou soma indevida de variantes de uma mesma curadoria.

## Relatório analítico

- Exibe valores pequenos sem arredondar presença real para zero, melhora o
  contraste do esforço amostral e evita duplicidade entre síntese e tabela
  detalhada das formas de vida nativas.
- Distingue a antiga “serrapilheira” do detalhamento de serrapilheira,
  fragmentos botânicos e material inundável introduzido pelo XLSForm 2025.
  Composição anterior a 2025 não é inventada retrospectivamente.
- Estabilidade e inconclusão não geram hipótese direcional de mudança ou
  recomendação de manejo por si só.
- A seção complementar de época das campanhas fica depois dos resultados,
  sem tabelas redundantes. Figura e texto breve mostram a associação estatística
  do calendário e a sensibilidade à escolha de referência. A recomendação final
  sobre linha de base é condicional: ausência de corte defensável não significa
  comparabilidade direta nem autoriza coleta fora da época prevista no projeto
  de amostragem da UC. Nenhum dado é excluído automaticamente.

## Projeto QField opcional

- `MONITORA_OPCAO_GERAR_PROJETO_QFIELD` ativa a criação do ZIP de navegação em
  `output/09_qfield/`; `MONITORA_OPCAO_IMPORTAR_CAMADAS_QFIELD` admite imagens
  MBTiles e camadas adicionais em `qfield_entrada/<uc_normalizada>/`.
- O projeto combina Sentinel regional com detalhe fornecido pelo usuário e
  camadas anuais `UC_verg_ini_YYYY`, `UC_verg_fin_YYYY`. PAs prioritários e
  alternativos, trajetos e camadas editáveis podem ser incorporados quando
  houver fonte correspondente.
- Pequenas variações anuais de GPS são avaliadas pelos critérios do validador
  espacial central; divergências impeditivas bloqueiam somente o projeto da
  UC. Os extremos de cada UA/ano são preservados literalmente. Uma UA observada
  uma única vez aparece com ressalva explícita, sem falsa aprovação espacial.
- Cobertura RGB de todos os extremos anuais é conferida na imagem detalhada.
  Fonte, licença, data e resolução desconhecidas não são inferidas. O usuário
  deve confirmar licença, qualidade de imagem e funcionamento offline no QField
  antes da navegação de campo. Imagens de execução não integram a release pública.
- Quando a entrada espacial contém `recorte_imagens_500m.gpkg`, com uma camada
  `uas_buffer_500m` auditável e exatamente uma geometria circular de 500 m por
  UA, o MBTiles detalhado JPEG é fisicamente recortado e convertido para WebP
  com transparência. O original permanece intocado, hashes e redução ficam em
  `auditoria_recorte_circular.csv`, e divergências de UCs, UAs, centros, raio,
  CRS ou geometrias bloqueiam somente o projeto QField.
- O papel explícito `acesso` em `camadas_qfield.csv` incorpora somente camadas
  lineares fornecidas pela UC, padronizadas como `UC_acessos`, em verde e
  somente leitura. Elas não se confundem com transectos amostrais nem com as
  camadas editáveis de apoio de campo.

## Homologação e preservação

- Gates Linux e R 4.6.0 do Windows comprovaram escopo, contrato XLSForm
  idêntico à v2.9.24, inicialização congelada do RStudio, tamanho inferior a
  5 MB em CRLF e integração do projeto opcional. A revisão substitutiva alterou
  somente seis funções do módulo QField: quatro revisadas e duas novas.
- No PNM, o recorte preservou os três níveis de zoom, cobriu os 524 extremos
  anuais e reduziu o MBTiles detalhado em 28,3%. A comparação integral dos
  pixels apresentou diferença média de 2,8–2,9 níveis em 255. O QGIS abriu as
  23 camadas, inclusive 18 linhas de acesso, sem erro de renderização.
- A rodada real de PNM concluiu no R do Windows em modo
  `painel_incremental_completo`: 262 vergalhões iniciais e 262 finais anuais,
  mapa regional e detalhe, relatórios e ZIP QField. O produto biológico
  `registros_validados.csv` e as decisões herdadas permaneceram intactos.
- O contrato único, o XLSForm 2025, o fluxo de correções, os estimadores
  principais e as releases anteriores não foram alterados para acomodar dados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.25.R`. O build exibido no console é
`v2.9.25-20260916-r02`.
