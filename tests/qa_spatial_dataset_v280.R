args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L) stop("Uso: Rscript tests/qa_spatial_dataset_v280.R SCRIPT REGISTROS_CORRIG [LIMITE_SEGUNDOS]", call. = FALSE)
script_path <- normalizePath(args[[1L]], mustWork = TRUE)
csv_path <- normalizePath(args[[2L]], mustWork = TRUE)
limite <- if (length(args) == 3L) as.numeric(args[[3L]]) else Inf
suppressPackageStartupMessages(library(data.table))

exprs <- parse(script_path, keep.source = FALSE, encoding = "UTF-8")
for (expr in exprs) {
  if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
  rhs <- expr[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
}
assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

MONITORA_RAIO_VALIDACAO_ESPACIAL_M <- 20
MONITORA_RAIO_ALERTA_ESPACIAL_M <- 10
MONITORA_MIN_COLETAS_CONSENSO_ESPACIAL <- 2L
MONITORA_MIN_COLETAS_CONSENSO_LOO_ESPACIAL <- 1L
MONITORA_VALIDACAO_ESPACIAL_LEAVE_ONE_OUT <- "S"
MONITORA_USAR_ACURACIA_GPS_NA_TRIAGEM <- "S"
MONITORA_ACURACIA_GPS_MAX_ALERTA_M <- 10
MONITORA_VALIDAR_COMPRIMENTO_TRANSECTO <- "S"
MONITORA_COMPRIMENTO_TRANSECTO_ESPERADO_M <- 50
MONITORA_TOLERANCIA_COMPRIMENTO_TRANSECTO_M <- 20

dt <- data.table::fread(csv_path, encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)
t0 <- proc.time()[["elapsed"]]
coletas <- monitora_esp_preparar_coletas(dt)
res <- monitora_esp_validar_coletas(coletas)
elapsed <- proc.time()[["elapsed"]] - t0

assert(nrow(coletas) > 0L, "nenhuma COLETA espacial preparada")
assert(uniqueN(coletas$id_coleta_espacial) == nrow(coletas), "identidade espacial de COLETA não é única")
assert(nrow(res$validacao) == nrow(coletas), "validação perdeu ou duplicou COLETAS")
assert(!any(res$consensos$consenso_valido == TRUE & (res$consensos$consenso_ambiguo == TRUE | res$consensos$consenso_dominante != TRUE), na.rm = TRUE), "consenso inválido/ambíguo foi promovido")
assert(!any(res$validacao$status_espacial == "coordenadas_conflitantes_na_coleta" & res$validacao$pendencia_espacial != TRUE, na.rm = TRUE), "conflito interno foi marcado como válido")
grupos_robustos <- res$validacao[consenso_robusto_ua == TRUE, .(todas_pendentes = all(pendencia_espacial == TRUE)), by = .(UC, EA, UA)]
assert(!any(grupos_robustos$todas_pendentes), "uma série temporal com consenso robusto teve todas as campanhas marcadas como pendentes")
plano <- monitora_esp_plano_correcoes(res)
if (nrow(plano)) assert(!any(plano$elegivel_sanitizacao_automatica == TRUE & plano$status_espacial != "possivel_transecto_invertido"), "sanitização automática incluiu diagnóstico não determinístico")
assert(is.finite(elapsed) && elapsed <= limite, paste0("validação espacial excedeu limite: ", round(elapsed, 3), "s > ", limite, "s"))

res$rejeicoes_preparacao <- monitora_esp_preparar_rejeicoes(dt)
qa_dir <- tempfile("qa_spatial_dataset_products_")
dir.create(file.path(qa_dir, "out"), recursive = TRUE)
dir.create(file.path(qa_dir, "log"), recursive = TRUE)
monitora_esp_gravar_produtos(res, momento = "diagnostico", output_dir = file.path(qa_dir, "out"), log_dir = file.path(qa_dir, "log"), exec_id = "QA_DATASET")
prod_dir <- file.path(qa_dir, "out", "validacao_espacial", "diagnostico")
oc <- fread(file.path(prod_dir, "validacao_espacial_ocorrencias_diagnosticas.csv"), encoding = "UTF-8", showProgress = FALSE)
pc <- fread(file.path(prod_dir, "plano_correcoes_espaciais.csv"), encoding = "UTF-8", showProgress = FALSE)
pend_ids <- unique(res$validacao[pendencia_espacial == TRUE, id_coleta_espacial])
oc_pend_ids <- unique(oc[impeditiva == TRUE, id_coleta_espacial])
assert(setequal(pend_ids, oc_pend_ids), "relatório diagnóstico não representa exatamente as pendências espaciais")
ids_plano <- unique(pc$id_coleta_espacial)
ids_revisao <- unique(res$validacao[pendencia_espacial == TRUE | alerta_espacial == TRUE, id_coleta_espacial])
assert(setequal(ids_plano, ids_revisao), "plano de correções não representa todas as coletas pendentes ou sob alerta")

cat(sprintf(
  "QA_SPATIAL_DATASET_V280_OK arquivo=%s linhas=%d coletas=%d uas=%d pendencias=%d alertas=%d consensos_robustos=%d ambiguos=%d sanitizacoes_seguras=%d elapsed=%.3f\n",
  basename(csv_path), nrow(dt), nrow(coletas), uniqueN(paste(coletas$UC, coletas$EA, coletas$UA, sep = "||")),
  sum(res$validacao$pendencia_espacial, na.rm = TRUE), sum(res$validacao$alerta_espacial, na.rm = TRUE),
  sum(res$consensos$consenso_robusto, na.rm = TRUE), sum(res$consensos$consenso_ambiguo, na.rm = TRUE),
  if (nrow(plano)) sum(plano$elegivel_sanitizacao_automatica, na.rm = TRUE) else 0L, elapsed
))
