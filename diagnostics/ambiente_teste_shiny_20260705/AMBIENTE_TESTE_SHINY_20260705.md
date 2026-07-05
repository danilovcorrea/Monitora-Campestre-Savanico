# Ambiente de teste Shiny headless — 2026-07-05

Registro da instalação (sem sudo, em biblioteca de usuário) dos pacotes
necessários para testes de navegador/headless do painel Shiny, e dos
smoke tests mínimos executados para validar o ambiente.

## Escopo desta etapa

Apenas instalação de dependências R e verificação de ambiente. Nenhum
arquivo do projeto foi alterado; nenhum dado real foi processado; nenhum
`runApp()` do painel foi executado; nenhum servidor Shiny ou processo de
navegador foi deixado em execução.

## Ambiente R

- R version: 4.6.1 (2026-06-24)
- `.libPaths()`:
  1. `/home/dfed/R/x86_64-redhat-linux-gnu-library/4.6` (biblioteca de usuário, já existia e é gravável)
  2. `/usr/lib64/R/library`
  3. `/usr/share/R/library`
- `R_LIBS_USER`: `/home/dfed/R/x86_64-redhat-linux-gnu-library/4.6`

## Pacotes — estado antes/depois

| Pacote      | Antes | Depois | Origem            |
|-------------|-------|--------|--------------------|
| shiny       | TRUE  | TRUE   | já instalado       |
| DT          | TRUE  | TRUE   | já instalado       |
| httpuv      | TRUE  | TRUE   | já instalado       |
| later       | TRUE  | TRUE   | já instalado       |
| promises    | TRUE  | TRUE   | já instalado       |
| shinytest2  | FALSE | TRUE   | instalado (CRAN, user lib), v0.5.1 |
| chromote    | FALSE | TRUE   | instalado (CRAN, user lib), v0.5.1 |
| webshot2    | FALSE | TRUE   | instalado (CRAN, user lib), v0.1.2 |

Instalação via `install.packages()` apontando para
`Sys.getenv("R_LIBS_USER")`, repositório `https://cloud.r-project.org`,
sem uso de sudo/dnf/yum/rpm. Dependências transitivas trazidas pelo CRAN
(ex.: `testthat`, `shinytest`, `vdiffr`, `spelling`) também foram
instaladas na mesma biblioteca de usuário, como parte normal da árvore
de dependências desses pacotes.

## Navegador para `chromote`

- `chromote::find_chrome()` encontrou: `/usr/bin/google-chrome`
- Também presentes no PATH: `google-chrome-stable`, `chromium-browser`
- Nenhum pacote de sistema foi instalado para viabilizar isso; o Chrome
  já estava disponível no sistema.

## Smoke tests executados

1. `library(shinytest2)` — OK.
2. `library(chromote)` — OK.
3. `chromote::find_chrome()` — OK, retornou `/usr/bin/google-chrome`.
4. Sessão headless mínima navegando para `about:blank` seguida de
   espera pelo evento `Page.loadEventFired` — **timeout** (limitação
   conhecida do evento de load em `about:blank`, não indica falha do
   ambiente).
5. Sessão headless mínima navegando para uma `data:` URL estática,
   avaliação de `document.body.innerText` e fechamento explícito da
   sessão — **OK** (conteúdo lido corretamente, sessão encerrada sem
   deixar processo órfão).

Após os testes, verificação de processos (`ps -eo pid,ppid,comm`)
confirmou que nenhum processo `google-chrome`/`chromium`/`chromote`
ficou em execução em segundo plano. O único processo Chrome-relacionado
observado no sistema (`chrome_crashpad`, PID diferente) pertence ao
Surfshark (Electron), pré-existente e sem relação com este teste.

## Limitações observadas

- O smoke test com `about:blank` + `Page.loadEventFired` não é
  confiável isoladamente; recomenda-se, em testes futuros, navegar
  diretamente para a URL/app alvo (ou usar `wait_ = TRUE` na própria
  chamada de navegação) em vez de esperar por load em página em branco.
- Este teste não cobriu autenticação, dados reais, nem o `runApp()`
  completo do painel — está fora do escopo desta etapa.

## Próximo passo recomendado

Com `shinytest2`/`chromote`/`webshot2` disponíveis e Chrome localizado,
a etapa seguinte é usar `shinytest2::AppDriver` para testar o
`runApp()` real do painel (aba opt-in por contrato único), reaproveitando
os achados da auditoria de exposição opt-in já registrada em
`diagnostics/` (ver séries `auditoria_035*` e `dev_035*`), sem dados reais
e com encerramento garantido da sessão ao final do teste.
