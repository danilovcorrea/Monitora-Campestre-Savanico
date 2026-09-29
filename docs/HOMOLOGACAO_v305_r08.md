# Candidata 3.0.5-rc01 — r08 — 29/09/2026

Esta atualização atende à determinação de manter a orientação original em retrato e reunir cada figura de evidência temporal numa única página. A fonte interna é medida no mesmo dispositivo raster usado na exportação e ajustada ao espaço real de cada célula, preservando integralmente os resultados. As tabelas usam larguras e fontes adaptativas, sem páginas em paisagem. Listas de anos podem quebrar entre anos; tabelas curtas recebem uma classe explícita de proteção de paginação.

## Escopo e resultados da homologação

| UC | Figuras temporais completas | Resultados preservados | Tabelas científicas: detalhado/sintético |
|---|---:|---:|---:|
| FNB | 12 | 836 | 32 / 7 |
| PNI | 10 | 180 | 31 / 7 |
| PNM | 10 | 792 | 31 / 7 |

Os relatórios foram regenerados em PDF, DOCX, HTML, Markdown e Rmd. Foram reutilizados dados, cálculos estatísticos, mapas e demais produtos não afetados, inclusive QField e manual. FNB e PNI partem das rodadas de 28/09; PNM parte da rodada v3.0.4-rc01. O inventário de siglas dos mapas reutilizados de PNM foi completado sem modificar seus pixels.

Gates executados: contrato e tamanho do script; 256 combinações de omissões para numeração; conteúdo editorial, siglas e referências; integridade das imagens; todas as células das tabelas no PDF; paridade textual e tabular HTML/DOCX; geometria de todas as páginas; ausência de paisagem; figuras e legendas na mesma página; abertura, atualização do índice, salvamento e reabertura no Word Windows. Os valores científicos das tabelas e dos 1.808 resultados foram comparados com a origem. A revisão visual foi por amostragem dirigida aos casos alterados, complementando a cobertura automática; não se declara leitura visual de cada página.

Esta é uma homologação documental com reuso científico. Não equivale a nova execução integral dos modelos, a uma nova sessão interativa no painel RStudio ou a teste do QField em dispositivo. Os gates históricos dos produtos reutilizados conservam sua proveniência. Não houve publicação nesta etapa.

## Evidências e reprodução

Diretório de trabalho: `/home/dlinux/Monitora_Dev_20260712/atualizacoes_v305_20260929`.

- `figuras.R`: utiliza funções da candidata sobre resultados estatísticos existentes e compara os valores com a origem.
- `documentos_final.R`: regenera os documentos usando o renderizador de produção.
- `homologar.py`: gates editoriais, de conteúdo, paginação e Word.
- `auditar_reuso.py`, `preparar_final.py`, `entregar_final.py`: integridade, metadados e entrega verificável.
- Por UC: `GATES_FINAIS.json`, `REUSO_VERIFICADO.json`, `WORD_*.json`, `AUDITORIA_EDITORIAL.json` e amostra visual.

Destinos: `dados_pre_validados/{FNB,PNI,PNM}/v305_r08` no OneDrive do projeto. `ENTREGA.json` registra os hashes da entrega e `CAMINHOS.json` verifica limites de 210 caracteres para planilhas/CSV, 240 para documentos e 259 para demais arquivos.

Script: 4.893.965 bytes LF; 4.978.191 bytes CRLF; ambos abaixo de 5.000.000 bytes. SHA-256 LF: `fa242de4b990654bd65b86f5109305f6ac03afc1a6df45b8538a2a53bc0fae03`.

## Economia aplicada

Não foram reexecutadas consultas externas ou análises científicas já disponíveis. Operações determinísticas foram agrupadas, saídas resumidas e testes repetidos somente após alterações relevantes. Não foram utilizados novos subagentes. Nenhuma troca automática de modelo ou de esforço do agente principal é alegada.
