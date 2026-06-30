script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) {
  stop("Script principal nao encontrado: ", script_path, call. = FALSE)
}

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

# --- 1. UA/CICLO/CAMPANHA permitem entrada manual no filtro (create=TRUE) ---

exigir(
  grepl("filtro_ciclo", texto, fixed = TRUE) &&
    grepl("create\\s*=\\s*TRUE", texto, perl = TRUE),
  "filtro_ciclo deve ter create=TRUE para aceitar digitacao manual."
)
exigir(
  grepl("filtro_campanha", texto, fixed = TRUE) &&
    grepl("create\\s*=\\s*TRUE", texto, perl = TRUE),
  "filtro_campanha deve ter create=TRUE para aceitar digitacao manual."
)
exigir(
  grepl("filtro_ua", texto, fixed = TRUE) &&
    grepl("create\\s*=\\s*TRUE", texto, perl = TRUE),
  "filtro_ua deve ter create=TRUE para aceitar digitacao manual."
)

# --- 2. filtrar_multi aceita permitir_fora_dominio ---

exigir(
  grepl("permitir_fora_dominio", texto, fixed = TRUE),
  "monitora_painel_filtrar_multi deve ter parametro permitir_fora_dominio."
)
exigir(
  grepl("permitir_fora_dominio\\s*=\\s*TRUE", texto, perl = TRUE),
  "Reactivos de ciclo/campanha/ua devem passar permitir_fora_dominio=TRUE."
)

# --- 3. Widget valor_novo usa textInput para UA/CICLO/CAMPANHA ---

exigir(
  grepl("col_norm_widget", texto, fixed = TRUE) &&
    grepl("textInput.*Valor novo de UA", texto, perl = TRUE),
  "ui_valor_novo_controle deve retornar textInput para UA antes do branch select_one."
)
exigir(
  grepl("Ciclo-AAAA_Xxx", texto, fixed = TRUE),
  "Placeholder de formato para CICLO ausente."
)
exigir(
  grepl("Campanha-AAAA_Xxx", texto, fixed = TRUE),
  "Placeholder de formato para CAMPANHA ausente."
)
exigir(
  grepl("UA-NNN_Xxx", texto, fixed = TRUE),
  "Placeholder de formato para UA ausente."
)

# --- 4. Validacao de formato ativa para UA/CICLO/CAMPANHA ---

exigir(
  grepl("bloqueada_formato_ua", texto, fixed = TRUE) &&
    grepl("UA-", texto, fixed = TRUE) &&
    grepl("formato.*UA|UA.*formato", texto, perl = TRUE),
  "Validacao de formato de UA ausente."
)
exigir(
  grepl("bloqueada_formato_ciclo", texto, fixed = TRUE) &&
    grepl("Ciclo-", texto, fixed = TRUE),
  "Validacao de formato de CICLO ausente."
)
exigir(
  grepl("bloqueada_formato_campanha", texto, fixed = TRUE) &&
    grepl("Campanha-", texto, fixed = TRUE),
  "Validacao de formato de CAMPANHA ausente."
)

# --- 5. EA restrita a EA-001_Cps ---

exigir(
  grepl("choices = \"EA-001_Cps\"", texto, fixed = TRUE) ||
    grepl('choices\\s*=\\s*"EA-001_Cps"', texto, perl = TRUE),
  "filtro_ea deve ter choices hardcoded para EA-001_Cps."
)
exigir(
  grepl("return(c(\"EA-001_Cps\"))", texto, fixed = TRUE),
  "choices_dominio_dinamico_sismonitora deve retornar c(\"EA-001_Cps\") para EA."
)
exigir(
  grepl("Regra provis", texto, fixed = TRUE),
  "Mensagem de regra provisoria para EA ausente."
)

# --- 6. VALIDAR_ESPACIAL=S ativa aba independente de ABRIR_ABA ---

exigir(
  grepl("identical(MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS, \"S\")", texto, fixed = TRUE),
  "Condicao VALIDAR_ESPACIAL=S para forcar ABRIR_ABA deve existir."
)
exigir(
  !grepl("!identical.*MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL.*\"N\"", texto, perl = TRUE),
  "Condicao que permitia ABRIR_ABA=N impedir o mapa ainda presente; deve ser removida."
)

# --- 7. Texto informativo menciona formato contratual (nao dominio fechado) ---

exigir(
  grepl("Controle por formato contratual", texto, fixed = TRUE),
  "Caixa informativa deve mencionar 'Controle por formato contratual' para UA/CICLO/CAMPANHA."
)

cat("PAINEL_FILTROS_UA_CICLO_CAMPANHA_EA_ESTATICA_OK\n")
