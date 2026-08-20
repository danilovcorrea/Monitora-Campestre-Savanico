# Monitora Campestre-Savânico v2.9.15

Publicada em 20 de agosto de 2026 sobre a v2.9.14.

## Robustez analítica

- Recortes vazios por formação vegetacional mantêm o schema necessário às
  tabelas e figuras; o caso de uma única formação deixa de interromper os
  relatórios.
- Os painéis disponíveis `evidencia_estatistica_*.png` passam a integrar o
  relatório detalhado junto ao tema correspondente, sem recalcular testes.
- O relatório sintético permanece executivo. Painéis indisponíveis por
  insuficiência de dados não produzem espaços vazios; duplicatas binárias
  exatas recebem um único representante auditado.

## Cartografia oficial e auditável

- Estados e biomas são obtidos das edições oficiais mais recentes do IBGE.
- Limites de UCs federais permanecem vinculados ao serviço oficial
  ICMBio/INDE.
- Autoridade, edição, escala, atualização, SHA-256 e eventual uso de fallback
  são registrados. Os arquivos espaciais são temporários e não integram o
  script nem a release.
- Nenhuma consulta ou varredura adicional ocorre quando o mapa Sentinel está
  desativado.

## Painel e continuidade semântica

- Triagem, movimento e edição direta que alcançam a mesma ocorrência
  `desconhecida` compartilham um alvo semântico. Operações concorrentes ou
  redundantes são bloqueadas antes da prévia e da mutação.
- O replay v2 reconhece `append_text`, `acrescentar_texto` e
  `adicionar_texto` como acréscimo textual.
- Pré e pós-condições idempotentes impedem a duplicação do fragmento. Ledgers
  v2 anteriores são reparados apenas na memória, preservando o arquivo de
  entrada.

## Integridade e identidade dos documentos

- A conversão para DOCX aceita figuras HTML com atributos opcionais, inclusive
  o mapa Sentinel.
- Um gate OOXML confere, por figura esperada, legenda, descritor, mídia,
  relacionamento e contagem. Documento incompleto não é aprovado apenas por
  existir ou ter tamanho maior que zero.
- A capa do Word passa a reproduzir a identidade do PDF/HTML: ICMBio no alto,
  barra institucional, título e metadados completos, Monitora e CBC lado a
  lado com separação física de 7,5 mm e primeira página sem cabeçalho ou rodapé
  das páginas internas.
- Quebras de linha são calculadas pela largura física disponível, inclusive
  para nomes longos de UCs.

## Homologação

- Matriz real: 13 UCs, 274.845 linhas, 2.345 COLETAs, uma a seis campanhas/anos
  e uma ou duas formações vegetacionais.
- Execução completa FNCS com código de saída zero, Sentinel ativado e relatórios
  Rmd, Markdown, HTML, DOCX e PDF.
- Mapa Sentinel selecionado por nebulosidade local: cena de 15/08/2026 com
  0,0% de nuvens na área, após rejeição da cena mais recente com 56,2%.
- DOCX real: 7/7 figuras no sintético e 46/46 no detalhado; dez painéis
  inferenciais disponíveis incorporados exatamente uma vez.
- Capa sintética e detalhada exportadas pelo Microsoft Word e inspecionadas
  visualmente; conteúdo, cabeçalho, rodapé e numeração das páginas internas
  preservados.
- Testes dirigidos cobriram a regressão analítica, as fontes cartográficas, o
  conflito semântico, o replay idempotente, o gate DOCX positivo e negativo e
  a correspondência documental das figuras.

## Preservação e desempenho

- O bloco congelado de inicialização rápida anterior às variáveis operacionais
  permanece idêntico ao da candidata homologada e à arquitetura pública.
- Contrato único, governança, linhagem, produtos de dados, modos e regras não
  relacionadas ao escopo permanecem preservados.
- Os gates de relatórios e cartografia só são acionados quando o respectivo
  módulo é solicitado. Não há custo novo quando relatórios e mapa estão
  desligados.
- Na execução real FNCS, o total acumulado foi 514,780 s; relatórios analíticos,
  Sentinel e todos os formatos consumiram 188,619 s.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.15.R`. O build exibido no console é
`v2.9.15-20260820`.
