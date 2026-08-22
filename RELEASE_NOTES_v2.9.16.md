# Monitora Campestre-Savânico v2.9.16

Publicada em 21 de agosto de 2026 sobre a v2.9.15.

Revisão pública `r03`: preserva a aquisição Sentinel-2 nativa da `r02` e
acrescenta controles de qualidade para a dimensão final do mapa no relatório,
sem alterar o número da versão, os produtos de dados ou a linhagem.

## Controle cartográfico da revisão r03

- O enquadramento de redes amostrais compactas é ampliado somente até assegurar
  densidade mínima de 150 ppi na largura de impressão do relatório, evitando
  ampliar poucos pixels e preservando a resolução nativa de 10 m.
- Brilho, contraste, nitidez e entropia são avaliados antes e depois da
  composição. Quando estritamente necessário, aplica-se correção radiométrica
  moderada e local, sem nova consulta, download ou tentativa de aquisição.
- Densidade, fator de ampliação, métricas radiométricas, ajustes e alertas
  passam a integrar a auditoria cartográfica e o gate do produto final.
- O orçamento existente de 180 segundos, as janelas de busca e o número de
  consultas permanecem inalterados. Com o mapa desativado, o custo continua
  nulo.
- Cinco mapas anteriormente abaixo do novo padrão foram substituídos
  focalmente em EEC, FNC, PNCA, PNGSV e PNM, sem reprocessar dados, estatística
  ou linhagem. PNCG recebeu uma atualização completa; PNCV e PNCF já atendiam
  aos novos critérios e não precisaram de nova execução.

## Sentinel-2 resiliente e auditável

- A consulta STAC percorre páginas sucessivas e amplia a janela temporal de
  forma progressiva e limitada, sem abandonar o mapa apenas porque a janela
  inicial não atende ao alvo preferencial de qualidade.
- A fonte RGB utiliza os COGs Sentinel-2 em resolução nativa de 10 m. A
  resolução de renderização é dimensionada para a extensão e para o tamanho
  final da figura, com limite de dois milhões de células, sem processar pixels
  que seriam descartados na página do relatório.
- Cobertura da área, nuvens e sombras locais orientam a seleção; recência é
  usada como desempate. Quando necessário, a rotina utiliza a melhor aquisição
  integral disponível ou uma composição temporal auditável.
- Prévia georreferenciada de baixa resolução pode apoiar o diagnóstico, mas
  não é aceita como mapa final. COG em cor natural, bandas RGB nativas e o
  último cache de alta resolução validado são as contingências permitidas.
- O mapa somente é concluído quando apresenta cobertura visual integral, todas
  as UAs sobre pixels válidos e resolução aprovada; fonte, resolução efetiva,
  nuvens, composição e tempos ficam registrados na auditoria.
- Localizador, limite oficial de UC, estados e biomas continuam vinculados às
  fontes oficiais ICMBio/INDE e IBGE, sem incorporar arquivos espaciais locais
  ao script ou à release.

## Continuidade e inventário de sessões

- `inventario_sessoes_linhagem.csv` passa a ser importado e verificado junto à
  linhagem incremental. Quando declarado pelo manifesto, ausência, alteração
  de hash ou cardinalidade divergente bloqueiam a continuidade antes da
  mutação.
- Execuções legadas sem decisões semânticas permanecem no inventário sem criar
  eventos artificiais. Ledger e inventário conservam responsabilidades
  distintas.
- A ordem histórica herdada permanece estável mesmo quando uma sessão antiga
  não possui data registrada. A sessão atual é acrescentada ao final.
- O inventário resultante recebe hash SHA-256 e quantidade de sessões no
  manifesto final, permitindo verificar a continuidade seguinte.

## Homologação real

- Recuperação e atualização cumulativa auditadas em 12 UCs: APAI, EEC, EEP,
  EET, FNC, FNCS, PNCA, PNE, PNGSV, PNM, RBG e RVSVOB.
- Todas as 159.176 linhas foram preservadas; nenhuma linhagem semântica
  regrediu e cada rodada recebeu sua sessão técnica.
- As dez execuções completas geraram `registros_validados.csv`, dois relatórios
  PDF, dois DOCX, mapa Sentinel com cobertura visual integral e localizador
  completo. EEP e RVSVOB permaneceram corretamente no modo parcial por ainda
  possuírem pendências impeditivas.
- PNGSV, com uma campanha, manteve relatório transversal sem inferência
  temporal indevida.
- A revisão `r02` foi homologada cartograficamente em dez UCs. Todas alcançaram
  100% de cobertura visual, nuvens e sombras entre 0% e 0,0058%, fonte nativa
  de 10 m e resolução efetiva entre 10,0 m e 92,1 m conforme a extensão.
- Um teste de continuidade sobre linhagem já assinada preservou as duas sessões
  herdadas, acrescentou uma terceira, manteve 23.937 registros e confirmou o
  novo hash do inventário.

## Preservação e desempenho

- O bloco congelado de inicialização rápida anterior às variáveis operacionais
  permanece idêntico ao da v2.9.15, descontados versão e build.
- Contrato único, painel, replay, produtos de dados, estatística, relatórios,
  modos e rotinas alheias ao escopo permanecem preservados.
- Com o mapa Sentinel desativado, não há consulta, varredura de cache nem custo
  adicional. O inventário trabalha apenas com pequenos arquivos de linhagem e
  não percorre `registros_corrig.csv`.
- O script público continua autossuficiente: não depende de arquivos locais e
  busca temporariamente em fontes on-line apenas os recursos cartográficos
  solicitados.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.16.R`. O build exibido no console é
`v2.9.16-20260821-r03`.
