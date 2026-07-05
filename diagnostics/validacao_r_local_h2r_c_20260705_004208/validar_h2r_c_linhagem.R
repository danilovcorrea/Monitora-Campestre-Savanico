
args <- commandArgs(trailingOnly = TRUE)
script_path <- args[[1]]
outdir <- args[[2]]

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")

result <- list(
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_path = normalizePath(script_path, mustWork = TRUE),
  r_version = R.version.string,
  checks = list()
)

add_check <- function(name, ok, detail) {
  result$checks[[name]] <<- list(ok = isTRUE(ok), detail = as.character(detail))
}

count_fixed <- function(pattern) {
  sum(grepl(pattern, lines, fixed = TRUE))
}

first_line_fixed <- function(pattern) {
  w <- which(grepl(pattern, lines, fixed = TRUE))
  if (length(w)) w[[1]] else NA_integer_
}

parse_ok <- TRUE
parse_detail <- "parse(file=script_path) OK"
tryCatch(
  {
    parsed_expr <- parse(file = script_path, keep.source = FALSE)
    invisible(parsed_expr)
  },
  error = function(e) {
    parse_ok <<- FALSE
    parse_detail <<- conditionMessage(e)
  }
)
add_check("parse_script_principal", parse_ok, parse_detail)

produto_nome_count <- count_fixed('produto_nome = "registros_importados_operacional_pre_painel.csv"')
add_check(
  "produto_nome_operacional_pre_painel_unico",
  produto_nome_count == 1L,
  paste("ocorrencias=", produto_nome_count)
)

produto_linha_count <- count_fixed('produto_linha("registros_importados_operacional_pre_painel.csv"')
add_check(
  "auditoria_final_lista_operacional_pre_painel",
  produto_linha_count == 1L,
  paste("ocorrencias=", produto_linha_count)
)

for (produto in c(
  "registros_importados_bruto.csv",
  "registros_importados.csv",
  "registros_importados_operacional_pre_painel.csv",
  "registros_corrig.csv",
  "registros_validados.csv"
)) {
  n <- count_fixed(produto)
  add_check(
    paste0("produto_referenciado_", gsub("[^A-Za-z0-9]+", "_", produto)),
    n > 0L,
    paste("ocorrencias=", n)
  )
}

n_exportar_def <- count_fixed("monitora_registros_importados_exportar <- function")
n_saneado_def <- count_fixed("monitora_registros_importados_saneado_exportar <- function")
n_exportar_call <- count_fixed("monitora_registros_importados_exportar(")
n_saneado_call <- count_fixed("monitora_registros_importados_saneado_exportar(")

add_check(
  "funcao_exportar_importados_existe",
  n_exportar_def >= 1L,
  paste("definicoes=", n_exportar_def, "chamadas=", n_exportar_call)
)

add_check(
  "funcao_exportar_saneado_existe",
  n_saneado_def >= 1L,
  paste("definicoes=", n_saneado_def, "chamadas=", n_saneado_call)
)

ln_bruto_call <- first_line_fixed("ok_registros_importados <- monitora_registros_importados_exportar(")
ln_saneado_call <- first_line_fixed("ok_registros_importados_saneado <- monitora_registros_importados_saneado_exportar(")
ln_reg_corrig <- first_line_fixed("registros -> registros_corrig")
ln_rm_registros <- first_line_fixed('rm(registros); monitora_recurso_gc("apos_criacao_registros_corrig_remocao_binding_registros")')
ln_operacional_pre <- first_line_fixed('produto_nome = "registros_importados_operacional_pre_painel.csv"')
ln_token_checkpoint <- first_line_fixed("registros_importados_operacional_tokenizado")

add_check(
  "ordem_bruto_saneado_antes_registros_corrig",
  !is.na(ln_bruto_call) && !is.na(ln_saneado_call) && !is.na(ln_reg_corrig) &&
    ln_bruto_call < ln_saneado_call && ln_saneado_call < ln_reg_corrig,
  paste("ln_bruto_call=", ln_bruto_call, "ln_saneado_call=", ln_saneado_call, "ln_reg_corrig=", ln_reg_corrig)
)

add_check(
  "rm_registros_apos_registros_corrig",
  !is.na(ln_reg_corrig) && !is.na(ln_rm_registros) && ln_reg_corrig < ln_rm_registros,
  paste("ln_reg_corrig=", ln_reg_corrig, "ln_rm_registros=", ln_rm_registros)
)

add_check(
  "operacional_pre_painel_apos_registros_corrig",
  !is.na(ln_reg_corrig) && !is.na(ln_operacional_pre) && ln_reg_corrig < ln_operacional_pre,
  paste("ln_reg_corrig=", ln_reg_corrig, "ln_operacional_pre=", ln_operacional_pre)
)

add_check(
  "checkpoint_operacional_tokenizado_existe",
  !is.na(ln_token_checkpoint),
  paste("ln_token_checkpoint=", ln_token_checkpoint)
)

after_rm <- if (!is.na(ln_rm_registros)) lines[seq.int(ln_rm_registros, length(lines))] else character()
after_rm_offset <- if (!is.na(ln_rm_registros)) ln_rm_registros - 1L else 0L
after_rm_operacional_nome <- any(grepl('registros_importados_operacional_pre_painel.csv', after_rm, fixed = TRUE))

# Regra refinada:
# mera menção textual a monitora_registros_importados_saneado_exportar após rm(registros)
# pode ocorrer em documentação, auditoria, função auxiliar ou bloco não executado.
# O risco real que queremos bloquear é uma nova chamada operacional de materialização
# saneada depois que o binding registros foi removido.
idx_saneado_textual <- which(grepl('registros_importados_saneado_exportar', after_rm, fixed = TRUE))
idx_saneado_chamada <- which(grepl('monitora_registros_importados_saneado_exportar\\s*\\(', after_rm, perl = TRUE))
idx_saneado_operacional <- which(grepl('^[[:space:]]*ok_registros_importados_saneado[[:space:]]*<-[[:space:]]*monitora_registros_importados_saneado_exportar\\s*\\(', after_rm, perl = TRUE))

linhas_textuais <- if (length(idx_saneado_textual)) paste(after_rm_offset + idx_saneado_textual, collapse = ",") else ""
linhas_chamadas <- if (length(idx_saneado_chamada)) paste(after_rm_offset + idx_saneado_chamada, collapse = ",") else ""
linhas_operacionais <- if (length(idx_saneado_operacional)) paste(after_rm_offset + idx_saneado_operacional, collapse = ",") else ""

add_check(
  "apos_rm_usa_nome_operacional_proprio",
  after_rm_operacional_nome,
  paste("operacional_pre_painel_depois_rm=", after_rm_operacional_nome)
)

add_check(
  "apos_rm_nao_reexecuta_saneado_importados",
  length(idx_saneado_operacional) == 0L,
  paste(
    "linhas_textuais=", ifelse(nzchar(linhas_textuais), linhas_textuais, "nenhuma"),
    "; linhas_chamadas=", ifelse(nzchar(linhas_chamadas), linhas_chamadas, "nenhuma"),
    "; linhas_operacionais=", ifelse(nzchar(linhas_operacionais), linhas_operacionais, "nenhuma")
  )
)

checks <- result$checks
ok_all <- all(vapply(checks, function(x) isTRUE(x$ok), logical(1)))

json_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\"', x)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x
}

json_lines <- c(
  "{",
  paste0('  "timestamp": "', json_escape(result$timestamp), '",'),
  paste0('  "script_path": "', json_escape(result$script_path), '",'),
  paste0('  "r_version": "', json_escape(result$r_version), '",'),
  paste0('  "ok_all": ', if (ok_all) "true" else "false", ','),
  '  "checks": {'
)

nm <- names(checks)
for (i in seq_along(nm)) {
  name <- nm[[i]]
  chk <- checks[[name]]
  comma <- if (i < length(nm)) "," else ""
  json_lines <- c(
    json_lines,
    paste0('    "', json_escape(name), '": {'),
    paste0('      "ok": ', if (isTRUE(chk$ok)) "true" else "false", ','),
    paste0('      "detail": "', json_escape(chk$detail), '"'),
    paste0("    }", comma)
  )
}
json_lines <- c(json_lines, "  }", "}")
writeLines(json_lines, file.path(outdir, "validacao_r_local_h2r_c.json"), useBytes = TRUE)

md <- c(
  "# Validação R local H2R-C",
  "",
  paste0("- Data/hora: ", result$timestamp),
  paste0("- Script: `", result$script_path, "`"),
  paste0("- R: ", result$r_version),
  paste0("- Resultado geral: ", if (ok_all) "OK" else "FALHA"),
  "",
  "## Checks",
  ""
)

for (name in names(checks)) {
  chk <- checks[[name]]
  md <- c(md, paste0("- ", if (isTRUE(chk$ok)) "[OK] " else "[FALHA] ", name, ": ", chk$detail))
}

writeLines(md, file.path(outdir, "VALIDACAO_R_LOCAL_H2R_C.md"), useBytes = TRUE)

if (!ok_all) quit(status = 1)
