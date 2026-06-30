script_path <- "monitora_campsav_alvo_global.R"
runner_path <- "tests/run_funcional_real_pequeno_v2_6_2.sh"

if (!file.exists(script_path)) stop("Script principal nao encontrado: ", script_path, call. = FALSE)
if (!file.exists(runner_path)) stop("Runner nao encontrado: ", runner_path, call. = FALSE)

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto  <- paste(linhas, collapse = "\n")

runner_linhas <- readLines(runner_path, warn = FALSE, encoding = "UTF-8")
runner_texto  <- paste(runner_linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

# --- 1. Defaults do script principal ---

exigir(
  grepl('(?m)^MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS\\s*<-\\s*"S"', texto, perl = TRUE),
  "Default de MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS deve ser 'S' no script principal."
)
exigir(
  grepl('(?m)^MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL\\s*<-\\s*"S"', texto, perl = TRUE),
  "Default de MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL deve ser 'S' no script principal."
)

# --- 2. Modo incremental nao sobrescreve OPCAO com N ---

# O bloco incremental deve forcar TRUE quando OPCAO="S", sem jamais atribuir "N" ao OPCAO global
linhas_incremental_idx <- grep("painel_incremental_registros_corrig", linhas, fixed = TRUE)
exigir(length(linhas_incremental_idx) > 0L, "Bloco painel_incremental_registros_corrig nao encontrado no script.")

# Verifica que o bloco de forcagem de TRUE para espacial existe
exigir(
  grepl('MONITORA_VALIDAR_ESPACIAL_COLETAS\\s*<<-\\s*TRUE', texto, perl = TRUE),
  "Modo incremental deve forcar MONITORA_VALIDAR_ESPACIAL_COLETAS=TRUE quando OPCAO=S."
)
exigir(
  grepl('MONITORA_ABRIR_ABA_VALIDACAO_ESPACIAL\\s*<<-\\s*TRUE', texto, perl = TRUE),
  "Modo incremental deve forcar MONITORA_ABRIR_ABA_VALIDACAO_ESPACIAL=TRUE quando OPCAO=S."
)

# --- 3. Runner: ABRIR_PAINEL=S preserva ESPACIAL_FLAG=S ---

exigir(
  grepl('ESPACIAL_FLAG', runner_texto, fixed = TRUE),
  "Runner deve definir ESPACIAL_FLAG para controlar MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS."
)
exigir(
  grepl('ABRIR_PAINEL.*==.*"S".*ESPACIAL_FLAG.*=.*"S"|ESPACIAL_FLAG.*=.*"S".*ABRIR_PAINEL', runner_texto, perl = TRUE) ||
    grepl('\\[\\[ "\\$ABRIR_PAINEL" == "S" \\]\\]|\\[\\[ \\$ABRIR_PAINEL.*==.*S', runner_texto, perl = TRUE) ||
    grepl('ABRIR_PAINEL.*S.*ESPACIAL_FLAG', runner_texto, perl = TRUE),
  "Runner deve setar ESPACIAL_FLAG=S quando ABRIR_PAINEL=S."
)
exigir(
  !grepl('MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS="N"', runner_texto, fixed = TRUE),
  "Runner nao deve hardcodar MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS='N'; deve usar ESPACIAL_FLAG."
)
exigir(
  grepl('MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS=.*ESPACIAL_FLAG', runner_texto, perl = TRUE),
  "Runner deve usar ESPACIAL_FLAG para MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS."
)

# --- 4. Aba espacial incluida no UI quando espacial=S ---

exigir(
  grepl('MONITORA_VALIDAR_ESPACIAL_COLETAS', texto, fixed = TRUE) &&
    grepl('shiny::tabPanel', texto, fixed = TRUE) &&
    grepl('"Valida', texto, fixed = TRUE),
  "Aba espacial (shiny::tabPanel) deve existir e depender de MONITORA_VALIDAR_ESPACIAL_COLETAS."
)

# --- 5. Mapa/render espacial nao depende de COLETA selecionada (filtro e opcional) ---

exigir(
  grepl('esp_coletas_escopo_painel', texto, fixed = TRUE),
  "Reactive esp_coletas_escopo_painel deve existir para gerenciar escopo opcional de COLETA."
)
exigir(
  grepl('if.*length.*coletas_escopo.*COLETA.*%in%|coletas_escopo.*length.*v.*COLETA', texto, perl = TRUE) ||
    grepl('if \\(length\\(coletas_escopo\\)', texto, perl = TRUE),
  "Filtro por COLETA no mapa deve ser condicional (aplicado somente quando coleta_escopo nao e vazio)."
)

# --- 6. UI exibe diagnostico se nao houver dados espaciais ---

exigir(
  grepl("Sem dados de validação espacial", texto, fixed = TRUE),
  "Mapa deve exibir diagnostico quando sem dados de validacao espacial."
)

# --- 7. Valor novo sem NA (numericInput com value=NA removido) ---

exigir(
  !grepl('numericInput.*valor_novo.*value\\s*=\\s*NA[^_]|valor_novo.*numericInput.*value\\s*=\\s*NA[^_]', texto, perl = TRUE),
  "numericInput para valor_novo nao deve usar value=NA (mostra string 'NA' na UI)."
)

# --- 8. Mensagem Atributo 101 nao resolvido tem detalhe ---

exigir(
  grepl("coluna esperada", texto, fixed = TRUE) &&
    grepl("aliases procurados", texto, fixed = TRUE),
  "Mensagem de atributo_101_nao_resolvido deve incluir coluna esperada e aliases procurados."
)
exigir(
  grepl("Corrigível pelo painel|Nao corrigivel pelo painel|Não corrigível", texto, perl = TRUE),
  "Mensagem de atributo_101_nao_resolvido deve indicar se e corrigivel pelo painel."
)

cat("ESPACIAL_DEFAULTS_INCREMENTAL_ESTATICA_OK\n")
