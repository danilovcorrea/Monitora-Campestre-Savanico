script_path <- "monitora_campsav_alvo_global.R"

if (!file.exists(script_path)) stop("Arquivo principal nao encontrado: ", script_path, call. = FALSE)

linhas <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
texto <- paste(linhas, collapse = "\n")

falhar <- function(...) stop(paste0(...), call. = FALSE)
exigir <- function(cond, msg) if (!isTRUE(cond)) falhar(msg)

trecho_entre <- function(linhas, inicio_padrao, fim_padrao = NULL, apos_inicio = TRUE) {
  ini <- grep(inicio_padrao, linhas, perl = TRUE)
  if (!length(ini)) return(character())
  ini <- ini[1L]
  if (is.null(fim_padrao)) return(linhas[ini:length(linhas)])
  fim_rel <- grep(fim_padrao, linhas[seq.int(ini + as.integer(apos_inicio), length(linhas))], perl = TRUE)
  if (!length(fim_rel)) return(linhas[ini:length(linhas)])
  fim <- ini + as.integer(apos_inicio) + fim_rel[1L] - 2L
  linhas[ini:max(ini, fim)]
}

painel <- paste(trecho_entre(
  linhas,
  "^monitora_correcao_painel\\s*<-\\s*function\\s*\\(",
  "^monitora_cache_painel_hash_inputs\\s*<-\\s*function\\s*\\("
), collapse = "\n")
if (!nzchar(painel)) falhar("Nao foi possivel localizar monitora_correcao_painel().")

choices <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "monitora_painel_choices_triagem_coleta\\s*<-\\s*function",
  "choices_triagem_coleta\\s*<-"
), collapse = "\n")
if (!nzchar(choices)) falhar("Helper de choices diagnosticos nao encontrado.")

for (padrao in c("encostam_desatualizado", "encostam_solo_nu_conflitante", "encostam_token_sem_inferior")) {
  exigir(grepl(padrao, choices, fixed = TRUE), paste0("Checkbox deve incluir pendencia Encostam: ", padrao))
}
exigir(
  grepl("linhas;", choices, fixed = TRUE) && grepl("coletas", choices, fixed = TRUE),
  "Labels dos checkboxes devem exibir N linhas; M coletas."
)
exigir(
  grepl("monitora_painel_resumo_impeditivas_dados", choices, fixed = TRUE),
  "Checkboxes devem tentar usar o mesmo resumo impeditivo da previa/tabela para evitar divergencia."
)

exigir(
  grepl("ui_pendencias_tecnicas_contrato", painel, fixed = TRUE) &&
    grepl("Pendência técnica de contrato/schema", painel, fixed = TRUE) &&
    grepl("Não corrigível pela bolsista", painel, fixed = TRUE) &&
    grepl("suporte técnico", painel, fixed = TRUE),
  "Contrato/schema deve aparecer em bloco tecnico nao corrigivel com orientacao ao suporte."
)
exigir(
  grepl("tipos_tecnicos <- c\\(\"atributo_101_nao_resolvido\", \"encostam_coluna_nao_resolvida\"\\)", painel, perl = TRUE) &&
    grepl("det <- det\\[!\\(tipo_pendencia %in% tipos_tecnicos\\)\\]", painel, perl = TRUE),
  "Pendencias tecnicas nao devem permanecer na tabela operacional da bolsista."
)

esp <- paste(trecho_entre(
  strsplit(painel, "\n", fixed = TRUE)[[1L]],
  "esp_coletas_escopo_painel\\s*<-\\s*shiny::reactive",
  "output\\$esp_mapa\\s*<-\\s*leaflet::renderLeaflet"
), collapse = "\n")
exigir(
  grepl("if \\(length\\(coletas_escopo\\)", esp, perl = TRUE),
  "Mapa/tabela espacial deve filtrar por COLETA somente quando houver escopo selecionado."
)
exigir(
  grepl('checkboxInput\\("esp_somente_pendencias".*value\\s*=\\s*FALSE', painel, perl = TRUE),
  "Mapa inicial deve abrir sem restringir a somente pendencias."
)
exigir(
  grepl("limitado às primeiras 300", painel, fixed = TRUE) &&
    grepl("sem coordenadas plotáveis", painel, fixed = TRUE),
  "Mapa deve ter aviso de cap e diagnostico claro para ausencia de coordenadas."
)

cat("HOTFIX3A_PAINEL_PENDENCIAS_MAPA_ESTATICA_OK\n")
