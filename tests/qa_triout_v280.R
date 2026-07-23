args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: Rscript tests/qa_triout_v280.R SCRIPT", call. = FALSE)
script_path <- normalizePath(args[[1L]], mustWork = TRUE)
suppressPackageStartupMessages(library(data.table))

exprs <- parse(script_path, keep.source = FALSE, encoding = "UTF-8")
for (expr in exprs) {
  if (!is.call(expr) || !identical(expr[[1L]], as.name("<-")) || !is.symbol(expr[[2L]])) next
  rhs <- expr[[3L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("function"))) eval(expr, envir = .GlobalEnv)
}

assert <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

# Fixture reduzida do defeito APAI: "Outra espécie..." descreve o arbusto
# válido e não é descritor legado da forma de vida "outra".
dt <- data.table(
  exotica = c("arbusto_abaixo outra", "arbusto_abaixo outra", "outra"),
  especie_arbusto = c("MM", "MM", NA_character_),
  descritor_legado = c(NA_character_, NA_character_, "musgo"),
  forma_vida_outros = c(NA_character_, NA_character_, NA_character_),
  encostam = c("exotica seca_morta", "nativa seca_morta exotica", "outra_forma_vida")
)

monitora_correcao_colunas_limpeza_outras_formas <- function(dt, dicionario = NULL) {
  data.table(
    classe = c("lista_principal_forma_vida", "descritor_outra_forma_vida", "campo_atual_forma_vida_outros", "campo_superior_tipo_forma_vida"),
    coluna = c("exotica", "descritor_legado", "forma_vida_outros", "encostam"),
    categoria = c("exotica", NA_character_, NA_character_, NA_character_),
    usar_para_escrita_solo_nu = c(FALSE, FALSE, FALSE, TRUE)
  )
}

res <- monitora_correcao_aplicar_limpeza_outras_formas_atomica(dt, 1:3, id_correcao = "TRIOUT_TEST")
assert(!isTRUE(res$falha), "TRIOUT falhou no fixture APAI")
assert(all(dt$exotica[1:2] == "arbusto_abaixo"), "TRIOUT removeu a forma de vida válida do fixture APAI")
assert(all(dt$especie_arbusto[1:2] == "MM"), "TRIOUT apagou o campo legítimo de espécie de arbusto")
assert(dt$forma_vida_outros[[3L]] == "musgos", "descritor inequívoco musgo não foi convertido")
assert(dt$encostam[[3L]] == "outra_forma_vida", "Encostam não foi recalculado após conversão de musgo")
assert(!grepl("conflito_descendente_significativo", paste(deparse(body(monitora_correcao_reconciliar_plano_semantico)), collapse = " "), fixed = TRUE), "reconciliador ainda contém o falso bloqueio descendente da TRIOUT")

cat("QA_TRIOUT_V280_OK\n")
