# Monitora Campestre-Savânico v2.9.6

Publicação de 12 de agosto de 2026, baseada integralmente na v2.9.5 aprovada.

## O que muda

A v2.9.6 fecha três problemas operacionais observados em execuções reais:

- justificativas em lotes sucessivos e disjuntos agora usam a mesma fonte
  canônica da tabela e permanecem selecionáveis; inclusão, exclusão,
  reconciliação e salvamento formam uma transação lógica auditável;
- falha no XLSX opcional do SISMONITORA não derruba os demais produtos, embora
  o arquivo afetado continue corretamente registrado como não concluído;
- a impressão PDF dos relatórios analíticos ocorre em processo isolado, evitando
  que o encerramento técnico do navegador polua o console ou retenha a sessão.

O relatório de validação também passa a informar quantas execuções e sessões
compõem a trilha, separando eventos herdados, reaplicados, novos e acumulados.
Uma rodada sem nova decisão continua registrada como execução, sem ser inventada
como sessão de origem de evento semântico.

## Progresso analítico

O console e o CSV de performance passam a identificar catálogo Sentinel,
aquisição, composição RGB, localizador, composição do mapa, conteúdo, DOCX,
HTML e PDF. Essas linhas são subetapas informativas e não duplicam o tempo no
acumulado.

## Novos padrões

O perfil publicado corresponde à continuidade incremental completa e ativa
painel, validação espacial, produtos, planilhas SISMONITORA, manual e relatórios
analíticos. O UUID é preservado no XLSX por padrão. O usuário deve revisar o
modo e todas as variáveis `MONITORA_OPCAO_*` antes de cada execução; uma entrada
bruta não deve ser executada como continuidade sem o produto e a linhagem
correspondentes.

## Compatibilidade e preservação

Permanecem preservados o contrato único, os itens congelados, os 13 modos,
precedência, replay, produtos centrais, estatísticas, relatórios compactos,
Sentinel-2 público, cartografia institucional, KML/KMZ e cautela causal.

A estrutura de inicialização rápida da v2.9.1 não foi modificada. No teste real
pelo botão **Source** do RStudio para Windows, a primeira expressão começou em
0,743 s. A homologação real PNCV gerou os produtos obrigatórios, 174 PNGs, cinco
XLSX e dez documentos analíticos em 866,881 s. Windows e Linux foram testados
fisicamente; os caminhos e resolutores multiplataforma incluem macOS, mas não
havia equipamento físico macOS disponível nesta homologação.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.6.R`. O build exibido no console é
`v2.9.6-20260812.1`.
