# Prova explícita de reutilização de testes dos componentes inalterados.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
source("tools/v308/carregar.R")
a <- monitora_v308_carregar(args[[1L]])
b <- monitora_v308_carregar(args[[2L]])
names_a <- ls(a, all.names = TRUE)
functions <- names_a[vapply(names_a, function(n) is.function(a[[n]]), logical(1L))]
stopifnot(all(functions %in% ls(b, all.names = TRUE)))
changed <- functions[!vapply(functions, function(n)
  identical(body(a[[n]]), body(b[[n]])) && identical(formals(a[[n]]), formals(b[[n]])), logical(1L))]
stopifnot(identical(changed, "monitora_output_reescrever_referencias"))
jsonlite::write_json(list(status = "PASS", previous_sha256 = digest::digest(file = args[[1L]], algo = "sha256"),
  candidate_sha256 = digest::digest(file = args[[2L]], algo = "sha256"),
  functions_compared = length(functions), changed_functions = changed,
  renderer_and_documental_components_identical = TRUE,
  scope = "Os testes gráficos e sintéticos da revisão anterior cobrem as mesmas funções; a organização e o replay documental devem ser testados na candidata atual."),
  args[[3L]], auto_unbox = TRUE, pretty = TRUE)
cat("PASS equivalência: ", length(functions), " funções; única diferença: reescrita de referências.\n", sep = "")
