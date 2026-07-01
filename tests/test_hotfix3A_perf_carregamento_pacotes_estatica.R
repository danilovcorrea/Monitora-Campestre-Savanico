script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

exigir(
  !grepl("library\\s*\\(\\s*[\"']tidyverse[\"']", texto, perl = TRUE) &&
    !grepl("require\\s*\\(\\s*[\"']tidyverse[\"']", texto, perl = TRUE),
  "Nao deve haver library/require(tidyverse)."
)

for (pkg in c("shiny", "DT", "sf", "rmarkdown", "knitr", "kableExtra", "pagedown")) {
  exigir(
    !grepl(paste0("library\\s*\\(\\s*[\"']", pkg, "[\"']"), texto, perl = TRUE) &&
      !grepl(paste0("require\\s*\\(\\s*[\"']", pkg, "[\"']"), texto, perl = TRUE),
    paste0(pkg, " nao deve ser carregado incondicionalmente por library/require.")
  )
}

exigir(
  grepl('monitora_pkg_obrigatorio("data.table")', texto, fixed = TRUE) &&
    grepl("library\\s*\\(\\s*[\"']data.table[\"']", texto, perl = TRUE),
  "data.table deve continuar obrigatorio e carregado."
)
exigir(
  grepl("monitora_pkg_carregar_se\\(\"ggplot2\", isTRUE\\(MONITORA_GERAR_OBJETOS_GRAFICOS\\)\\)", texto, perl = TRUE) &&
    grepl("monitora_pkg_carregar_se\\(\"ggrepel\", isTRUE\\(MONITORA_GERAR_OBJETOS_GRAFICOS\\)\\)", texto, perl = TRUE),
  "ggplot2/ggrepel devem ser carregados somente quando objetos graficos forem gerados."
)
exigir(
  grepl("MONITORA_SF_DISPONIVEL <- isTRUE\\(MONITORA_VALIDAR_ESPACIAL_COLETAS\\)", texto, perl = TRUE),
  "sf deve ser apenas detectado quando validacao espacial estiver ligada."
)

exigir(
  !grepl('if \\(!\\(MONITORA_MODO_EXECUCAO %in% c\\("abrir_painel_cache", "painel_incremental_registros_corrig"\\)\\)\\)', texto, perl = TRUE),
  "Nao deve haver if top-level gigante envolvendo o fluxo normal apos os modos cache/incremental."
)
exigir(
  grepl("monitora_execucao_encerrar_ramo_cache_incremental <- function", texto, fixed = TRUE) &&
    grepl('MONITORA_MODO_EXECUCAO %in% c("abrir_painel_cache", "painel_incremental_registros_corrig")', texto, fixed = TRUE),
  "Modos cache/incremental devem encerrar de forma controlada antes do fluxo normal."
)
exigir(
  !grepl("pb_spin", texto, fixed = TRUE),
  "Formato cli nao deve referenciar pb_spin, que pode estar indisponivel."
)

exigir(
  grepl("MONITORA_PAINEL_INICIAL_LEVE <- monitora_painel_cfg_bool", texto, fixed = TRUE),
  "Painel deve ter modo inicial leve configuravel por ambiente."
)
exigir(
  grepl("if (isTRUE(painel_inicial_leve)) shiny::req(isTRUE(esp_mapa_carregado()))", texto, fixed = TRUE),
  "renderLeaflet deve depender de flag/botao de carregamento no modo inicial leve."
)

ini_modal <- regexpr("monitora_painel_mostrar_modal_impeditivas <- function", texto, fixed = TRUE)
exigir(ini_modal[[1]] > 0L, "Modal de pendencias impeditivas nao encontrado.")
fim_modal <- regexpr("monitora_painel_salvar_e_fechar <- function", texto, fixed = TRUE)
exigir(fim_modal[[1]] > ini_modal[[1]], "Fim do trecho do modal de pendencias nao encontrado.")
trecho_modal <- substring(texto, ini_modal[[1]], fim_modal[[1]] - 1L)
exigir(
  grepl("modal_impeditivas_inicio", trecho_modal, fixed = TRUE) &&
    grepl("modal_impeditivas_exibido", trecho_modal, fixed = TRUE),
  "Modal de pendencias deve registrar eventos de responsividade."
)
exigir(
  grepl("msg_estimativa <-", trecho_modal, fixed = TRUE) &&
    grepl("Estimativa detalhada", trecho_modal, fixed = TRUE) &&
    grepl("preservar responsividade", trecho_modal, fixed = TRUE),
  "Modo leve nao deve recalcular preview pesado antes de exibir o modal."
)
exigir(
  grepl("cancelar_sem_materializar_stopApp", texto, fixed = TRUE),
  "Cancelamento sem materializar deve registrar stopApp imediato."
)

ini_pre <- regexpr("monitora_painel_resumo_impeditivas_pre <- function", texto, fixed = TRUE)
exigir(ini_pre[[1]] > 0L, "Resumo impeditivo pre-painel nao encontrado.")
trecho_pre <- substring(texto, ini_pre[[1]], ini_pre[[1]] + 500L)
exigir(
  grepl("data.table::copy(resumo_impeditivas_pre_cache)", trecho_pre, fixed = TRUE) &&
    !grepl("monitora_painel_resumo_impeditivas_dados(dt)", trecho_pre, fixed = TRUE),
  "Resumo impeditivo pre-painel deve usar cache local, sem recalcular dt no clique."
)

trecho_modal_curto <- trecho_modal
exigir(
  !grepl("monitora_painel_resumo_impeditivas_preview", trecho_modal_curto, fixed = TRUE) &&
    !grepl("monitora_painel_preview_dados_pos_operacoes", trecho_modal_curto, fixed = TRUE) &&
    !grepl("monitora_painel_pendencias_impeditivas_detalhadas_preview", trecho_modal_curto, fixed = TRUE),
  "Modal de fechamento nao deve chamar previews pesados."
)

ini_cancelar <- regexpr("shiny::observeEvent(input$confirmar_encerrar_sem_materializar", texto, fixed = TRUE)
exigir(ini_cancelar[[1]] > 0L, "Handler de confirmar_encerrar_sem_materializar nao encontrado.")
fim_cancelar <- regexpr("shiny::observeEvent(input$confirmar_descartar_correcoes", texto, fixed = TRUE)
exigir(fim_cancelar[[1]] > ini_cancelar[[1]], "Fim do handler de confirmar_encerrar_sem_materializar nao encontrado.")
trecho_cancelar <- substring(texto, ini_cancelar[[1]], fim_cancelar[[1]] - 1L)
exigir(
  grepl("monitora_painel_encerrar_sem_materializar", trecho_cancelar, fixed = TRUE) &&
    !grepl("monitora_painel_iniciar_botao", trecho_cancelar, fixed = TRUE) &&
    !grepl("monitora_painel_liberar_botao", trecho_cancelar, fixed = TRUE),
  "Confirmar cancelamento sem materializar deve chamar encerramento diretamente."
)

cat("HOTFIX3A_PERF_CARREGAMENTO_PACOTES_ESTATICA_OK\n")
