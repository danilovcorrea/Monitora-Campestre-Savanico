# Fecho transitivo de dependências de `monitora_correcao_painel()` via `codetools` (2026-07-05)

## Objetivo

Avançar o item 1 da seção 17 do plano executivo do motor único
(`diagnostics/plano_executivo_motor_unico_20260704/PLANO_EXECUTIVO_MOTOR_UNICO_20260704.md`):
substituir a seleção manual/grep do fecho de dependências do painel
completo por um método programático e auditável
(`codetools::findGlobals()` recursivo), reduzindo o risco de
omissão/divergência já sinalizado naquela seção. Este documento **não**
executa `runApp()` do painel completo — permanece pendência real,
documentada abaixo, consistente com as três rodadas anteriores (03.5U-C,
03.5V-C) que concluíram não ser seguro fazê-lo nesta janela.

## Método (seguro por construção)

Script: `fecho_painel_codetools.R` (neste diretório).

1. `parse()` do script inteiro (`monitora_campsav_alvo_global_v2.6.0.R`,
   48235 linhas) — **não** `source()`; nada é avaliado nesta etapa.
2. Filtro AST (não regex) das expressões top-level: mantém só atribuições
   (`<-`/`=`/`<<-`) cujo lado direito é literalmente `function(...) {...}`.
   Todo o resto (chamadas de pipeline, `if`, leitura de arquivo etc.)
   nunca é avaliado.
3. Cada atribuição de função é avaliada isoladamente num ambiente cujo
   parent é `baseenv()`. Definir uma função não executa seu corpo nem
   argumentos default (avaliação preguiçosa do R) — portanto isso não
   toca disco/rede mesmo que o corpo interno chame
   `normalizePath()`/`fwrite()`/etc.
4. `codetools::findGlobals()` sobre cada closure (análise estática,
   também sem executar) para obter os identificadores livres.
5. Busca em largura (BFS) a partir de `monitora_correcao_painel`,
   expandindo recursivamente as funções referenciadas que têm definição
   top-level capturada.

Nenhum dado real foi lido, nenhum arquivo foi escrito pelo script exceto o
JSON de saída, nenhum processo Shiny foi iniciado.

## Resultado

Comando: `Rscript fecho_painel_codetools.R monitora_campsav_alvo_global_v2.6.0.R fecho_painel_codetools.json monitora_correcao_painel`
Saída bruta: `console.log` (`exit=0`) e `fecho_painel_codetools.json` (neste diretório).

| Métrica | Valor |
|---|---|
| Expressões top-level no arquivo | ver `fecho_painel_codetools.json` |
| Definições `nome <- function(...)` top-level capturadas | 508 |
| **Funções no fecho transitivo de `monitora_correcao_painel`** | **231** |
| Linha da primeira/última definição do fecho no arquivo | 582 / 26043 (52,8% da extensão do arquivo) |
| Identificadores "globais não-função" referenciados (bruto) | 486 |

Decomposição dos 486 "globais não-função" (não são todos constantes
reais — ver limitação 1 abaixo):

- 45 constantes `MONITORA_*`/`.MONITORA_*` de fato (candidatas a stub
  sintético numa rodada futura — ex.: `MONITORA_LOG_DIR`,
  `MONITORA_OUTPUT_DIR`, `MONITORA_EXEC_ID`, `MONITORA_CORRECOES_DIR`,
  `MONITORA_PROGRESSO_BACKEND`, entre outras).
- 25 nomes `monitora_*` minúsculos que **são** funções mas não foram
  capturados pelo filtro do passo 2 (provável definição fora do padrão
  `nome <- function(...)` simples — ex. via `assign()`, closure aninhada,
  ou assinatura que o parser não associou como top-level). Ficam como
  gap explícito para uma rodada dedicada, não resolvidos aqui.
- 416 símbolos de R base/`data.table` (operadores como `%in%`, `$`, `!`,
  variáveis especiais do `data.table` como `.SD`, `.N`, `.BY`,
  `..coluna`) — artefato esperado do `codetools::findGlobals`, que lista
  todo identificador livre, não só chamadas de função "de negócio". Não
  são dependências reais a resolver.

## Comparação com a estimativa manual da seção 17 (123 definições)

A seção 17 do plano executivo mediu 123 definições via grep de **um
nível** (identificadores `monitora_*`/`MONITORA_*` referenciados
diretamente no corpo de `monitora_correcao_painel`, com definição
top-level encontrada por regex simples). Este documento mede o **fecho
transitivo completo** (funções chamadas por funções chamadas por...) via
AST, achando **231** funções — quase o dobro.

Isso **não é uma contradição**: são métricas diferentes (dependência
direta vs. transitiva). O achado relevante é que a métrica mais completa
(transitiva) é maior, não menor — reforça, com evidência mais forte que a
anterior, a conclusão já registrada na seção 17 de que uma extração
manual do fecho carregaria risco real de omissão/divergência silenciosa,
e que o método programático (este) é o caminho correto para uma futura
extração segura.

## O que este documento NÃO faz

- Não executa nenhuma das 231 funções.
- Não resolve os 45 globais `MONITORA_*`/`.MONITORA_*` com stubs
  sintéticos (passo 2 do "próximo passo recomendado" da seção 17) — fica
  para rodada dedicada.
- Não tenta `shiny::runApp()` do painel completo.
- Não altera nenhuma linha do script principal.

## Limitações

1. O filtro do passo 2 é sintático (AST de atribuição direta de
   `function(...)`); os 25 nomes `monitora_*` não capturados (ver acima)
   indicam que o método, embora mais forte que regex de texto, ainda não
   é 100% completo — uma rodada dedicada deve investigar esses 25 casos
   individualmente antes de declarar o fecho fechado.
2. `codetools::findGlobals()` não distingue globais que serão
   sobrescritos por argumentos/`local()` internos de globais realmente
   livres em tempo de execução; o número 231 é um **teto seguro
   (superconjunto)**, não uma garantia de mínimo necessário.
3. Este documento não avalia os 12 pontos de escrita em disco já
   inspecionados na seção 17 (permanece válido o achado anterior: todos
   dentro de `observeEvent()` de botão, nenhum na renderização inicial).

## Conclusão

Progresso real e seguro no item 1 da seção 17 (método programático em vez
de manual). O `runApp()` do painel completo **continua não executado**
nesta rodada — permanece pendência documentada, agora com fecho medido de
forma mais rigorosa (231 funções, 45 constantes globais candidatas a
stub) para orientar o orçamento da rodada dedicada futura.
