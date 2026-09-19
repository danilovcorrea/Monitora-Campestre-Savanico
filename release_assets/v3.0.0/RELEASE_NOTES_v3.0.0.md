# v3.0.0 — análises integradas e relatórios por UC

A versão consolida as análises de fogo, clima, calendário e cobertura vegetal,
o novo padrão editorial dos relatórios e a documentação operacional para quem
executa a curadoria e gera os produtos. A mudança de série identifica essa
ampliação do conjunto público de produtos; a execução por um único R e o
contrato de entrada permanecem compatíveis.

## Novos produtos e leitura dos resultados

O módulo de fogo consulta dados públicos do ICMBio e cruza cicatrizes com os
segmentos amostrais, distinguindo modalidade, anos documentados e limites da
cronologia. Incêndio, queima prescrita, regeneração e equivalência são avaliados
condicionalmente ao suporte dos dados. As margens ±5 e ±10 pontos percentuais
são análises exploratórias de sensibilidade, não limites de manejo homologados.

O módulo climático obtém dados NASA POWER e calcula métricas antecedentes às
campanhas. Réplicas biológicas não são tratadas como réplicas climáticas quando
compartilham célula e período. Séries curtas, calendário concentrado e esforço
insuficiente recebem resultados e limitações específicos. A integração
multivariada diferencia exploração da trajetória e modelos efetivamente
estimáveis; não apresenta ajuste descritivo como prova causal.

Os mapas de fogo usam o contexto Sentinel, histórico por UA e elementos
cartográficos padronizados. Relatórios têm índice com páginas e links, capa sem
numeração visível, metodologia concisa, referências em padrão ABNT e discussão
de evidências, hipóteses e implicações de gestão.

O manual acompanha todas as rotinas de curadoria e geração dos produtos. O
QField inclui camadas de apoio editáveis e importa vetores/imagens locais.
As auditorias verificam os dois extremos das UAs, integridade dos arquivos e
caminhos relativos do projeto.

## Preservação e correções verificadas

- Revisão dos comentários e incorporação compactada dos módulos no próprio R,
  mantendo o arquivo abaixo de 5.000.000 bytes inclusive em CRLF.
- Preservação estrutural do código analítico aprovado, confirmada pela comparação
  das árvores sintáticas e do módulo incorporado decodificado.
- Correção da localização do manual na finalização de uma execução completa.
- Discussão distingue exposição indeterminada de ausência de interseção e evita
  classificar como NA uma evidência cuja consistência entre recortes não foi avaliada.
- Correção de títulos após mensagens condicionais, para manter destinos válidos
  no índice de todos os formatos.
- Harmonização nominal derivada para a validação espacial consumida por fogo,
  clima e QField, usando a equivalência de UC já adotada nas estatísticas.
  Identificadores, coordenadas, registros e decisões do painel são preservados;
  colisões são rejeitadas e pendências espaciais reais continuam bloqueando QField.

- Mapas de detalhe do fogo preservam a projeção da Figura 1 ao atravessar
  fronteiras de zonas UTM, evitando lacunas artificiais de reprojeção.
  O teste reproduziu a falha, confirmou extremos válidos e preservou o bloqueio
  de lacunas reais sem reduzir os limites de cobertura.

- Geometrias inválidas após projeção são reparadas somente na cópia destinada
  ao mapa complementar de cicatrizes, com auditoria. O snapshot e as métricas
  de exposição permanecem preservados.

O pacote público contém software e documentação. Dados e produtos de cada UC
são entregues no ambiente institucional e não integram esta publicação.
