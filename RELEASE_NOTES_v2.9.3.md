# v2.9.3 — Coletores, justificativas espaciais e relatórios inferenciais

A versão `v2.9.3` parte integralmente da última versão pública aprovada,
`v2.9.2`, e consolida as revisões operacionais do painel, a sanitização de
coletores, o diagnóstico de vegetação seca ou morta e o núcleo ecológico e
inferencial dos relatórios analíticos. O contrato único, os defaults, os 13
modos, a linhagem, o replay e os produtos centrais permanecem preservados.

## Equipe da COLETA

- Coletores são tratados como repeat: cada integrante ocupa uma linha e o CPF
  correspondente permanece na mesma posição.
- A inclusão e a exclusão atuam sobre o integrante selecionado, sem replicar o
  nome nas 101 linhas da COLETA.
- Nome continua obrigatório para o integrante; CPF continua opcional.
- Formatos históricos comprovados são saneados automaticamente antes do painel.
- CPF só é preservado quando sua associação com um único nome é inequívoca;
  valores ambíguos, parciais ou inválidos são descartados sem adivinhação.
- A auditoria da sanitização registra contagens e motivos sem expor nomes ou
  CPFs.

## Impactos de manejo e uso

- `impact_manejo_uso` e `tipos_impacto_manejo_uso` exibem `label — name` do
  contrato único.
- A lista **Quais?** somente é editável no estado compatível com a resposta Sim.
- Adição, remoção e substituição são realizadas por tokens contratuais.
- A mudança do pai para Não limpa os filhos condicionais de forma auditável.

## Justificativas remanescentes

- A aba **Justificar pendências** aceita seleção múltipla e aplicação em lote.
- Pendências e alertas espaciais passam a integrar o mesmo catálogo auditável,
  preservando seus tipos e alvos físicos.
- O catálogo é recalculado depois das correções espaciais; ocorrências resolvidas
  são encerradas pela persistência e não permanecem como pendências obsoletas.
- Justificar não muda coordenadas, não corrige dados, não oculta a ocorrência e
  não libera gates impeditivos.

## Vegetação seca ou morta

- Foi criado diagnóstico não impeditivo por linha e forma de vida, com resumo
  por UC, esforço amostral, UA, ano e COLETA.
- A ocorrência orienta revisão e pode subsidiar hipóteses relacionadas a
  fenologia, seca, fogo, herbivoria ou outros processos.
- Nenhum vetor é atribuído causalmente sem evidência temporal, espacial ou
  independente.

## Relatórios analíticos

- Cobertura e proporção relativa passam a abranger explicitamente estrutura
  herbácea/lenhosa viva, categorias gerais, formas nativas, formas exóticas,
  formas secas ou mortas e material botânico em decomposição.
- As séries anuais usam a UA como unidade analítica e exibem média, IC95% e
  número de UAs disponíveis.
- Os painéis inferenciais incorporam testes em UAs pareadas, efeito com IC95%
  bootstrap, ajuste FDR-BH, comparação com linha de base, equivalência e teste
  multivariado da composição.
- As médias recebem `↑`, `↓`, `≈`, `?` ou `—` exclusivamente a partir dos
  resultados estatísticos já materializados. Ausência de significância não é
  chamada de estabilidade.
- Achados prioritários são agrupados por métrica e formação. Resultado
  observado, hipótese compatível, explicações alternativas e evidência
  necessária permanecem semanticamente separados.
- Tabelas visuais compactam somente contexto elegível. Formação vegetacional,
  medidas, contagens, testes e CSVs editáveis não são mesclados nem alterados.
- Parágrafos, títulos, notas e figuras recebem alinhamento editorial consistente
  em Rmd, Markdown, HTML, DOCX e PDF.

## Manual e auditabilidade

- O script gera manual detalhado antes da abertura do painel, com configuração,
  modos, produtos, controles, replay, continuidade, sanitizações, relatórios e
  roteiro de conferência.
- `auditoria_simbolos_medias_anuais_relatorio.csv` liga cada símbolo ao resultado
  estatístico correspondente.
- `auditoria_integracao_estatistica_graficos_relatorio.csv` documenta a
  incorporação dos testes aos painéis.
- `auditoria_robustez_inferencial_relatorio.csv` declara implementações e limites
  metodológicos.

## Desempenho e opções desligadas

- Relatórios analíticos, Sentinel e planilhas SISMONITORA permanecem desligados
  por padrão.
- Os caminhos em `N` retornam antes de dependências, consultas, I/O ou
  materialização própria.
- A revisão editorial reutiliza resultados estatísticos existentes e não
  recomputa testes.

## Validação

- Parse integral e testes de justificativas espaciais, símbolos estatísticos,
  coletores, CPF opcional, impactos condicionais, seca/morta e painel Shiny com
  dados RBG reais foram aprovados.
- A sanitização foi exercitada sobre 232 CSVs históricos; 217 continham campos
  de coletores e 29.595 linhas apresentavam formatos legados.
- O XLSX SISMONITORA foi aprovado com 5.858 linhas e 58 coletas FNCS 2026.
- A homologação analítica real FNCS 2023–2026 processou 16.766 pontos, 58 UAs e
  duas formações vegetacionais, gerou mapa Sentinel-2 e dez artefatos
  documentais em 68,035 s.
- Foram inspecionadas integralmente 58 páginas PDF e 56 páginas DOCX, sem cortes,
  sobreposições, títulos órfãos ou regressões cartográficas observadas.
- Há execução funcional e renderização em Linux/WSL2 e Microsoft Word no
  Windows. A compatibilidade macOS foi revisada estaticamente; não se declara
  execução real sem host.

## Preservado

- contrato único e projeção SISMONITORA;
- defaults e 13 modos públicos;
- CPF opcional e opção de remoção/preservação de UUID;
- precedência, operações atômicas e semântica ecológica;
- linhagem, replay, oráculo e gates impeditivos;
- produtos centrais, estatísticas, gráficos e KML/KMZ;
- cartografia institucional e consulta temporária de referências oficiais;
- inicialização interna e autocontida no RStudio.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.3.R`. O build declarado no console é
`v2.9.3-20260807.1`.
