# v3.0.3 — recuperação de rodadas e caminhos curtos

Build: `v3.0.3-20260924-r01`. Publicação autorizada pelo responsável em 24/09/2026,
com as pendências operacionais abaixo explicitamente registradas.

## Alterações

- Corrige a comparação pré/pós em pastas transitórias, organizadas e parcialmente
  movidas; reconhece nomes antigos e compactos. Duplicatas divergentes continuam
  bloqueadas. Atualiza o pós também em correções exclusivamente espaciais.
- Consultas de fogo têm limite de conexão e requisição, repetição limitada de
  falhas transitórias e auditoria por tentativa. Console informa HTTP, espera,
  etapa e duração. Falhas não viram zero fogo nem autorizam cache inválido.
- PDF de validação e manual é gerado em processo isolado, com prazo e validação
  antes da substituição. Um PDF antigo não é considerado resultado atual.
- QField sinaliza a pasta incorreta `input_qfield`; a entrada é `qfield_input`.
  Recortes mostram percentual, quantidade e duração; bloqueios ficam explícitos.
- Avisos são preservados e registrados por etapa e mensagem. A medição de legendas
  usa suporte a Unicode, sem alterar símbolos ou cálculos.
- Encurta nomes desde a criação, independentemente do sistema de execução.
  Analíticos: `08_analises/u_<hash>/analitico_sintetico` e `analitico_detalhado`;
  validação: `07_validacao`; auditorias: `03_aud`. Preserva identificação lógica,
  dados, linhagem e conteúdo interno QField. O índice audita o destino Windows,
  incluindo todos os formatos. `MONITORA_DESTINO_COMPARTILHAMENTO` permite informar
  a raiz prevista de compartilhamento; raízes arbitrariamente longas ainda exigem
  atenção aos limites informados pelo índice.

## Verificação e abrangência

| UC | Produtos esperados conferidos | Sintético: tabelas / figuras | Detalhado: tabelas / figuras |
|---|---:|---:|---:|
| Taiamã (EET) | 18/18 | 5 / 7 | 28 / 34 |
| Mapinguari (PNM) | 22/22 | 7 / 8 | 31 / 50 |

Dados corrigidos e linhagem foram preservados. Imagens de alta resolução foram
incluídas em `qfield_input`, com integridade e pacotes QField conferidos. Nas
entregas OneDrive, o maior caminho documental tem 191 caracteres; CSV/XLSX, 208;
o maior caminho geral, 249. A migração conferiu referências e hashes sem refazer
as análises concluídas. As fontes adicionais de clima foram arquivadas em ZIP
com estrutura e conteúdo íntegros, para extração local em caminho curto.

EET conserva a execução completa r01 e incorpora relatórios de apoio/comparação
já produzidos em r02 sobre dados idênticos. PNM concluiu a execução r03 em 2.033 s,
com saída 0. A revisão r04 de caminhos passou por testes focalizados e migração;
as análises completas não foram repetidas após essa revisão. Os registros locais
distinguem o código executado do código entregue. PDFs analíticos e suas
sequências foram conferidos; houve inspeção visual por amostragem.

Passaram testes de comparação em layouts antigos/novos, ambiguidade e ausência,
nove cenários HTTP, PDF isolado e rejeição de resultado antigo, cache íntegro e
inválido, 256 combinações de omissão editorial, contrato de 129 campos, Unicode,
classificação e orçamento de caminhos Windows, inclusive contrabarras. Testes
focalizados foram executados em Linux e R Windows; rodadas integrais, em Linux.
A promoção altera somente versão e build em relação à candidata final; a
reversibilidade foi conferida byte a byte. O pacote contém software e documentação.

Script autônomo: **4.868.777 bytes LF / 4.952.995 bytes CRLF**, ambos abaixo do
limite obrigatório de 5.000.000 bytes para edição no RStudio Windows.

## Pendências e avisos preservados

Permanecem pendentes Source e interação no RStudio Windows, inspeção no Microsoft
Word, navegação em QField móvel inclusive offline e homologação de atualização
por UUID no SISMONITORA. Rscript e conferência estrutural não substituem esses
ensaios. A publicação não declara homologação operacional integral nem revisão
científica independente de todas as análises. A cópia local do OneDrive foi
verificada; isso não certifica a conclusão da sincronização na nuvem.

Os logs históricos EET preservam avisos Unicode anteriores à correção; o PNM r03
não os reproduziu. Avisos GDAL QUALITY ocorreram no driver intermediário, com
integridade e cobertura final aprovadas. Avisos de elementos gráficos ausentes
ou fora da escala e tabelas auxiliares não estimáveis permanecem rastreáveis;
não foram convertidos em dados inventados. Um alerta estimativo de corte em
figura PNM foi inspecionado sem corte visível, dentro da amostragem realizada.
