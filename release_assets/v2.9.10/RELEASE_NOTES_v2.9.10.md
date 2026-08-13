# Monitora Campestre-Savânico v2.9.10

Hotfix publicada em 13 de agosto de 2026 sobre a v2.9.9.

## Correção

Em diretórios muito longos no Windows, o dispositivo gráfico podia truncar
silenciosamente nomes de PNG. A figura era gravada com outro nome e o gate dos
relatórios recusava corretamente incorporá-la. A v2.9.10 calcula, somente
quando necessário, um nome compacto com hash e propaga esse nome aos índices e
aos relatórios em todos os formatos.

Em caminhos normais, o nome editorial permanece exatamente igual. A operação é
constante por figura e não acrescenta varredura dos dados.

## Homologação

A contraprova real da EEC foi executada no R 4.6 do Windows em um caminho mais
longo que o incidente. A execução terminou em 139,987 s, código 0, com todos os
produtos obrigatórios em estado `ok`, 44 resultados inferenciais incorporados e
relatórios sintético e detalhado em Rmd, Markdown, HTML, DOCX e PDF.

O bloco responsável pelo Source no RStudio permanece byte a byte idêntico ao
da v2.9.9. O desenvolvimento da v2.9.9, inclusive a recuperação RBG, o contrato
único, os itens congelados, a linhagem e os produtos de dados, foi preservado.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.10.R`. O build exibido no console é
`v2.9.10-20260813`.
