# v3.0.4 — hábitos históricos e legibilidade dos relatórios

Build: `v3.0.4-20260925-r01`. Publicação autorizada pelo responsável em 25/09/2026, com as pendências abaixo registradas.

## Alterações

- Corrige a mudança de categoria de samambaias com hábito ausente em registros do protocolo de 2023. A exigência acompanha o protocolo de cada registro; espécie histórica não é convertida em hábito. Lotes preservam descritores e são cancelados integralmente se houver conflito.
- Corrige a disponibilidade das dependências do PDF no fluxo incremental de validação.
- Divide painéis temporais extensos em blocos legíveis, sem excluir resultados. Preserva rótulos válidos nos extremos das figuras de composição.
- Explicita a campanha utilizada nas tabelas de estado, corrige a seleção de indicador no resumo e ajusta paginação, largura numérica e cabeçalhos repetidos. A numeração continua sequencial quando elementos não podem ser gerados.
- Restaura nomes reconhecíveis de pastas (auditorias, ocorrencias, apoio, operacoes), mantendo nomes curtos de arquivos, compatibilidade de leitura e limites de caminhos Windows/OneDrive.

## Homologação realizada

| UC | Produtos conferidos | Sintético: tabelas / figuras / páginas | Detalhado: tabelas / figuras / páginas |
|---|---:|---:|---:|
| FNB | 22/22 | 7 / 8 / 16 | 32 / 120 / 144 |
| PNM | 22/22 | 7 / 8 / 15 | 31 / 120 / 141 |

FNB: 42 pontos reclassificados para nativa mediante autorização expressa, preservando hábito ausente e linhagem. PNM: sem nova curadoria. Imagens de alta resolução e pacotes QField conferidos. Dados canônicos e os 129 campos foram preservados; dados institucionais não integram o pacote público.

A homologação ocorreu por etapas: execuções integrais da candidata r06 (FNB: 28min46s; PNM: 39min51s), recuperação documental r07 e revisão final de caminhos r08. A FNB terminou com falha no PDF sintético; a causa foi corrigida e os cinco formatos foram regenerados e revalidados. O resumo PNM também foi corrigido. Não se declara uma nova execução integral r08. Os registros locais distinguem fonte executada, recuperação e fonte entregue.

Passaram testes de hábitos e lotes, estado reativo do painel, rollback, 256 combinações de omissões editoriais, contrato de 129 campos, 50 prefixos novos/legados e 743 nomes anteriores, caminhos dinâmicos, preservação de resultados e rótulos, paginação e seleção de campanha/indicador. Houve testes em Linux e R Windows, conferência de numeração nos PDFs, inspeção visual por amostragem, hashes e vínculos. A promoção pública altera somente versão/build em relação à candidata r08; reversibilidade byte a byte, parse e testes dos bytes finais foram conferidos.

Nas entregas, os maiores caminhos CSV/XLSX têm 208 caracteres e documentos, 175; demais arquivos, 253 na FNB e 248 no PNM. A migração não recalculou análises. Script: **4.879.078 bytes LF / 4.963.378 bytes CRLF**, abaixo de 5.000.000 bytes.

## Pendências e avisos preservados

Permanecem pendentes interação completa no RStudio Windows, inspeção no Microsoft Word, navegação em QField móvel inclusive offline e homologação da atualização por UUID no SISMONITORA. Testes automatizados não substituem esses ensaios. A publicação não declara homologação operacional integral nem revisão científica independente de todas as análises. A cópia local OneDrive foi conferida; isso não certifica sincronização concluída na nuvem.

PNM: seis avisos nas figuras 25–28 decorrem exclusivamente de proporções indefinidas por denominador zero em formas secas/mortas: sete categorias × anos 2019, 2021 e 2023. Foram preservadas todas as 28 proporções válidas por formação; os construtores reais reproduziram os avisos. Tabelas auxiliares do modelo de época não são estimáveis na FNB (cinco anos; mínimo seis) e no PNM (calendário concentrado em 37 dias). Avisos GDAL QUALITY referem-se ao driver intermediário; integridade dos produtos finais conferida. Logs originais e limitações foram preservados.
