#!/usr/bin/env Rscript
# Calculo estatico (sem execucao/pipeline/I-O real) do fecho transitivo de
# dependencias de monitora_correcao_painel(), via codetools::findGlobals.
#
# Metodo (seguro por construcao):
#   1. parse() do script inteiro (nao source()) -> so produz expressoes,
#      nada e avaliado.
#   2. Filtra, entre as expressoes top-level, SOMENTE as que sao atribuicao
#      (<-, =, <<-) de um literal function(...){...}. Ignora tudo o mais
#      (chamadas de pipeline, if/for, leitura de arquivo, etc.) - essas
#      expressoes nunca sao avaliadas.
#   3. Avalia cada atribuicao de funcao isolada num ambiente proprio
#      (parent = baseenv()). Definir uma funcao NAO executa seu corpo nem
#      seus argumentos default (lazy eval em R) - portanto isso continua
#      sem I/O e sem side effects, mesmo que o corpo interno chame
#      normalizePath()/fwrite()/etc.
#   4. codetools::findGlobals() sobre cada closure (analise estatica do
#      bytecode/AST, tambem sem executar) para obter globais referenciados.
#   5. BFS recursivo a partir de monitora_correcao_painel para obter o
#      fecho transitivo de funcoes (dentre as que temos definicao) e o
#      conjunto de variaveis/constantes globais referenciadas (que nao
#      resolvemos - so listamos os nomes).

args <- commandArgs(trailingOnly = TRUE)
script_path <- if (length(args) >= 1) args[[1]] else "monitora_campsav_alvo_global_v2.6.0.R"
out_json <- if (length(args) >= 2) args[[2]] else "fecho_painel_codetools.json"
entry_point <- if (length(args) >= 3) args[[3]] else "monitora_correcao_painel"

stopifnot(requireNamespace("codetools", quietly = TRUE))
stopifnot(file.exists(script_path))

exprs <- parse(file = script_path, keep.source = FALSE)

is_function_assign <- function(e) {
  if (!is.call(e)) return(FALSE)
  op <- as.character(e[[1]])
  if (!(op %in% c("<-", "=", "<<-"))) return(FALSE)
  if (length(e) < 3) return(FALSE)
  target <- e[[2]]
  if (!is.symbol(target)) return(FALSE)
  rhs <- e[[3]]
  is.call(rhs) && identical(as.character(rhs[[1]]), "function")
}

fn_env <- new.env(parent = baseenv())
fn_names <- character(0)
n_total_top_level <- length(exprs)

for (e in exprs) {
  if (is_function_assign(e)) {
    nm <- as.character(e[[2]])
    rhs <- e[[3]]
    fn_obj <- tryCatch(eval(rhs, envir = fn_env), error = function(err) NULL)
    if (is.function(fn_obj)) {
      assign(nm, fn_obj, envir = fn_env)
      fn_names <- c(fn_names, nm)
    }
  }
}
fn_names <- unique(fn_names)

stopifnot(exists(entry_point, envir = fn_env, inherits = FALSE))

visited_fn <- character(0)
all_vars <- character(0)
queue <- entry_point

while (length(queue) > 0) {
  cur <- queue[[1]]
  queue <- queue[-1]
  if (cur %in% visited_fn) next
  visited_fn <- c(visited_fn, cur)
  fn_obj <- get(cur, envir = fn_env, inherits = FALSE)
  globs <- tryCatch(
    codetools::findGlobals(fn_obj, merge = FALSE),
    error = function(err) list(functions = character(0), variables = character(0))
  )
  fn_refs <- unique(globs$functions)
  var_refs <- unique(globs$variables)
  all_vars <- c(all_vars, var_refs)
  for (r in fn_refs) {
    if (r %in% fn_names && !(r %in% visited_fn) && !(r %in% queue)) {
      queue <- c(queue, r)
    } else if (!(r %in% fn_names)) {
      # referencia a funcao sem definicao top-level capturada (ex.: funcao
      # base, ou definida de outra forma nao coberta pelo filtro acima) -
      # registrar como variavel/global tambem, para nao perder o rastro.
      all_vars <- c(all_vars, r)
    }
  }
}

all_vars <- sort(unique(all_vars))
visited_fn_sorted <- sort(unique(visited_fn))

result <- list(
  metodo = "parse()+eval() so de atribuicoes de function(), sem source() do arquivo, sem execucao de corpo/pipeline, sem I/O real",
  script_path = normalizePath(script_path, mustWork = TRUE),
  entry_point = entry_point,
  n_expressoes_top_level_no_arquivo = n_total_top_level,
  n_definicoes_function_top_level_capturadas = length(fn_names),
  n_funcoes_no_fecho_transitivo = length(visited_fn_sorted),
  funcoes_no_fecho_transitivo = as.list(visited_fn_sorted),
  n_globais_nao_funcao_referenciados = length(all_vars),
  globais_nao_funcao_referenciados = as.list(all_vars),
  r_version = R.version.string,
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)

jsonlite_ok <- requireNamespace("jsonlite", quietly = TRUE)
if (jsonlite_ok) {
  writeLines(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), out_json)
} else {
  # fallback sem dependencia externa: escreve um JSON minimo manualmente
  esc <- function(x) gsub('"', '\\\\"', x)
  vec_json <- function(v) paste0("[", paste0('"', esc(v), '"', collapse = ", "), "]")
  con <- file(out_json, "w")
  writeLines(c(
    "{",
    sprintf('  "metodo": "%s",', esc(result$metodo)),
    sprintf('  "script_path": "%s",', esc(result$script_path)),
    sprintf('  "entry_point": "%s",', esc(result$entry_point)),
    sprintf('  "n_expressoes_top_level_no_arquivo": %d,', result$n_expressoes_top_level_no_arquivo),
    sprintf('  "n_definicoes_function_top_level_capturadas": %d,', result$n_definicoes_function_top_level_capturadas),
    sprintf('  "n_funcoes_no_fecho_transitivo": %d,', result$n_funcoes_no_fecho_transitivo),
    sprintf('  "funcoes_no_fecho_transitivo": %s,', vec_json(visited_fn_sorted)),
    sprintf('  "n_globais_nao_funcao_referenciados": %d,', result$n_globais_nao_funcao_referenciados),
    sprintf('  "globais_nao_funcao_referenciados": %s,', vec_json(all_vars)),
    sprintf('  "r_version": "%s",', esc(result$r_version)),
    sprintf('  "timestamp": "%s"', esc(result$timestamp)),
    "}"
  ), con)
  close(con)
}

cat("OK\n")
cat("n_definicoes_function_top_level_capturadas:", length(fn_names), "\n")
cat("n_funcoes_no_fecho_transitivo (a partir de", entry_point, "):", length(visited_fn_sorted), "\n")
cat("n_globais_nao_funcao_referenciados:", length(all_vars), "\n")
