# Monitora Campestre-Savânico v2.9.16

Publicada em 21 de agosto de 2026 sobre a v2.9.15.

## Sentinel-2 resiliente e auditável

- A consulta STAC percorre páginas sucessivas e amplia a janela temporal de
  forma progressiva e limitada, sem abandonar o mapa apenas porque a janela
  inicial não atende ao alvo preferencial de qualidade.
- A extensão técnica máxima é compartilhada entre catálogo, assets e
  renderização. A rotina evita reconstruções redundantes do mesmo mosaico.
- Cobertura da área, nuvens e sombras locais orientam a seleção; recência é
  usada como desempate. Quando necessário, a rotina utiliza a melhor aquisição
  integral disponível ou uma composição temporal auditável.
- As contingências por COG, prévia georreferenciada e último cache validado da
  mesma UC são limitadas em tempo e registradas. O mapa somente é considerado
  concluído quando o arquivo e sua auditoria existem e são válidos.
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
`v2.9.16-20260821`.
