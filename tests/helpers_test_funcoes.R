monitora_test_funcoes <- function(caminho) {
  arvore <- parse(caminho, keep.source = FALSE, encoding = "UTF-8")
  stopifnot(length(arvore) == 1L, identical(arvore[[1L]][[1L]], quote(base::evalq)))
  env <- new.env(parent = globalenv())
  carregadas <- character()
  visitar <- function(no) {
    if (!is.call(no)) return(invisible(NULL))
    atribuicao <- as.character(no[[1L]])[1L] %in% c("<-", "=") && length(no) >= 3L
    rhs_funcao <- atribuicao && is.call(no[[3L]]) &&
      identical(as.character(no[[3L]][[1L]])[1L], "function")
    if (rhs_funcao && (is.symbol(no[[2L]]) || is.character(no[[2L]]))) {
      nome <- as.character(no[[2L]])
      env[[nome]] <- eval(no[[3L]], envir = env)
      carregadas <<- c(carregadas, nome)
      return(invisible(NULL))
    }
    if (length(no) > 1L) for (ii in 2:length(no)) visitar(no[[ii]])
    invisible(NULL)
  }
  visitar(arvore[[1L]])
  list(env = env, funcoes = unique(carregadas), arvore = arvore)
}
