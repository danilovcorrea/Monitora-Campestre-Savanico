from pathlib import Path
import re,base64,gzip,textwrap
root=Path(__file__).resolve().parents[1]
s=(root/'monitora_campsav_alvo_global_v3.0.2.R').read_text()
def replace(old,new,count=1):
 global s
 assert s.count(old)>=count,old[:100]
 s=s.replace(old,new,count)
replace('# Versão 3.0.0 —','# Versão 3.0.3-rc01 —')
replace('MONITORA_SCRIPT_VERSAO <- "3.0.2"','MONITORA_SCRIPT_VERSAO <- "3.0.3-rc01"')
replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.2-20260923-r01"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.3-rc01-20260924-r02"')
helper=(root/'tools/v303/observabilidade.R').read_text()
replace('base::evalq({','base::evalq({\n'+helper+'''\nMONITORA_AVISOS_ESTADO <- new.env(parent = emptyenv())
MONITORA_AVISOS_ESTADO$chaves <- character()
MONITORA_AVISOS_ESTADO$itens <- list()
MONITORA_AVISOS_ESTADO$etapa <- "inicializacao"
MONITORA_AVISOS_ESTADO$id <- format(Sys.time(), "%Y%m%d_%H%M%S")
withCallingHandlers({''')
replace('}, envir = .GlobalEnv)','}, warning = monitora_aviso_registrar)\n}, envir = .GlobalEnv)')
replace('monitora_perf_registrar_checkpoint <- function(etapa, detalhe = NA_character_, objeto = NULL) {', '''monitora_perf_registrar_checkpoint <- function(etapa, detalhe = NA_character_, objeto = NULL) {
  if (exists("MONITORA_AVISOS_ESTADO", inherits = TRUE)) MONITORA_AVISOS_ESTADO$etapa <- paste0("apos_", etapa)''')
# Renderizador documental: não aceitar produto antigo nem ocultar o erro real.
a=s.index('monitora_doc_render_rmd <- function(');b=s.index('monitora_doc_link_relativo <-',a);part=s[a:b]
part=part.replace('  out <- character()', '  out <- character()\n  causa_pdf <- "HTML ou dependências de renderização indisponíveis."')
part=part.replace('  html <- sub(', '  inicio_doc <- Sys.time()\n  monitora_operacao_msg("Documento", "Iniciando ", tipo_documento, "; formatos: ", paste(formatos, collapse = ", "))\n  html <- sub(',1)
part=part.replace('    ok_pdf <- FALSE','    ok_pdf <- FALSE\n    candidato_pdf <- tempfile("monitora_pdf_", tmpdir = dirname(pdf), fileext = ".pdf")\n    on.exit(unlink(candidato_pdf), add = TRUE)',1)
part=part.replace('output_file = basename(pdf)', 'output_file = basename(candidato_pdf)')
part=part.replace('      log_msg("Falha ao gerar PDF via rmarkdown/pdf_document: ", conditionMessage(e))','      causa_pdf <<- conditionMessage(e)\n      log_msg("Falha ao gerar PDF via rmarkdown/pdf_document: ", causa_pdf)')
part=part.replace('&& file.exists(html)) {','&& ok_html && file.exists(html)) {')
x=part.index('    timeout_pdf <-');y=part.index('    if (ok_pdf && file.exists(pdf))',x)
part=part[:x]+'''    timeout_pdf <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_PDF_CHROME_TIMEOUT_SEG", "180")))
    if (!is.finite(timeout_pdf) || timeout_pdf < 3) timeout_pdf <- 180
    ok_pdf <- tryCatch({
      navegador <- monitora_relatorios_analiticos_resolver_navegador()
      if (!isTRUE(navegador$ok)) stop(navegador$mensagem)
      resultado <- monitora_relatorios_analiticos_chrome_print_isolado(html, candidato_pdf,
        browser = navegador$caminho, timeout = timeout_pdf)
      log_msg(resultado$mensagem, " Duração: ", round(resultado$duracao_seg, 1), "s.")
      if (!isTRUE(resultado$ok)) stop(resultado$mensagem)
      TRUE
    }, error = function(e) {
      causa_pdf <<- conditionMessage(e)
      log_msg("Falha ao gerar PDF via Chrome isolado: ", causa_pdf)
      FALSE
    })
    }
    ok_pdf <- isTRUE(ok_pdf) && file.exists(candidato_pdf) && isTRUE(file.info(candidato_pdf)$size > 1000)
    if (ok_pdf) {
      ok_pdf <- tryCatch({ monitora_doc_validacao_publicar(candidato_pdf, pdf); TRUE },
        error = function(e) { causa_pdf <<- conditionMessage(e); FALSE })
    }
''' +part[y:]
part=part.replace('    log_msg("PDF gerado: ", pdf)','    log_msg("PDF gerado: ", pdf)\n    aviso <- file.path(dirname(rmd), paste0("PDF_NAO_GERADO_", tools::file_path_sans_ext(basename(rmd)), ".txt"))\n    if (file.exists(aviso)) unlink(aviso)\n    monitora_operacao_msg("Documento", "PDF concluído: ", pdf)')
part=part.replace('      "Consulte o HTML/MD/JSON. A falha de PDF geralmente decorre de ausência de Chrome/pagedown funcional, LaTeX ou dependências de renderização."','      paste0("Causa registrada: ", causa_pdf),\n      "Consulte os formatos gerados nesta execução. MONITORA_PDF_CHROME_TIMEOUT_SEG controla o prazo do Chrome (padrão 180s)."')
part=part.replace('    log_msg("PDF não gerado; aviso gravado em: ", aviso)','    log_msg("PDF não gerado; aviso gravado em: ", aviso)\n    monitora_operacao_msg("Documento ERRO", causa_pdf, "; detalhes: ", aviso)')
part=part.replace('  out\n}', '  monitora_operacao_msg("Documento", tipo_documento, ": concluído em ", round(as.numeric(difftime(Sys.time(), inicio_doc, units = "secs")), 1), "s; formatos gerados: ", paste(tools::file_ext(out), collapse = ", "))\n  out\n}')
s=s[:a]+part+s[b:]
part_marker = None
# Processo Chrome existente ganha pulso de vida e prazo externo em ambos executores.
a=s.index('monitora_relatorios_analiticos_chrome_print_isolado <-');b=s.index('monitora_relatorios_analiticos_indice <-',a);p=s[a:b]
p=p.replace('  inicio <- Sys.time()', '  inicio <- Sys.time()\n  monitora_operacao_msg("PDF", "Iniciando ", basename(input), "; Chrome até ", timeout, "s; processo até ", timeout + 30, "s.")',1)
x=p.index('    processx::run(');y=p.index('    error = function(e) e',x)
p=p[:x]+'''    {
      processo <- processx::process$new(command = rscript, args = argumentos,
        stdout = log_filho, stderr = log_filho, windows_verbatim_args = FALSE, cleanup_tree = TRUE)
      on.exit(if (processo$is_alive()) processo$kill_tree(), add = TRUE)
      pulso <- Sys.time()
      while (processo$is_alive()) {
        processo$wait(timeout = 1000)
        decorrido <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
        if (decorrido > timeout + 30) {
          processo$kill_tree()
          stop("Processo PDF excedeu o limite externo de ", timeout + 30, "s.")
        }
        if (as.numeric(difftime(Sys.time(), pulso, units = "secs")) >= 15) {
          monitora_operacao_msg("PDF", basename(input), ": renderização em andamento há ", round(decorrido), "s.")
          pulso <- Sys.time()
        }
      }
      list(status = processo$get_exit_status(), stdout = paste(readLines(log_filho, warn = FALSE), collapse = "\\n"), stderr = "")
    },
''' +p[y:]
p=p.replace('      wait = TRUE\n', '      wait = TRUE, timeout = as.numeric(timeout) + 30\n')
p=p.replace('  list(\n    ok = isTRUE(ok),', '  monitora_operacao_msg("PDF", if (ok) "Concluído" else "Falhou", " em ", round(duracao, 1), "s: ", basename(output))\n  list(\n    ok = isTRUE(ok),')
s=s[:a]+p+s[b:]
# Resolver arquivos com organização parcial, nunca arbitrar entre versões divergentes.
a=s.index('monitora_relatorios_comparar_pre_pos_correcoes <-');b=s.index('  md5 <- function',a)
s=s[:s.index('  dir_pre <-',a)]+'''  pre <- monitora_relatorios_resolver_fase(base_dir, "pre_painel")
  pos <- monitora_relatorios_resolver_fase(base_dir, "pos_painel")
  if (!length(pre) || !length(pos)) {
    motivo <- paste0("Comparação pré/pós não gerada: pré=", length(pre), "; pós=", length(pos),
      " arquivo(s). Conferir relatórios de apoio em ", base_dir, " e na pasta organizada.")
    monitora_operacao_msg("Comparação AVISO", motivo)
    monitora_log_registrar_evento("comparacao_relatorios_pre_pos", "AVISO", base_dir, motivo, "gerar as duas fases antes de comparar")
    return(invisible(data.table::data.table()))
  }
  caminho <- function(mapa, nome) if (nome %in% names(mapa)) unname(mapa[[nome]]) else file.path(base_dir, "__ausente__", nome)
  files <- sort(unique(c(names(pre), names(pos))))
''' +s[b:]
a=s.index('monitora_relatorios_comparar_pre_pos_correcoes <-');b=s.index('\n}',a)+2
p=s[a:b].replace('file.path(dir_pre, ff)','caminho(pre, ff)').replace('file.path(dir_pos, ff)','caminho(pos, ff)')
p=p.replace('file.path(dir_pre, f)', 'caminho(pre, f)').replace('file.path(dir_pos, f)', 'caminho(pos, f)')
p=p.replace('dir_pre)', 'paste(unique(dirname(pre)), collapse = "; "))').replace('dir_pos)', 'paste(unique(dirname(pos)), collapse = "; "))')
assert 'dir_pre' not in p and 'dir_pos' not in p
p=p.replace('  invisible(out)', '  monitora_operacao_msg("Comparação", nrow(out), " arquivos comparados; resultado: ", arq_csv)\n  invisible(out)')
s=s[:a]+p+s[b:]
# O pós precisa refletir também mudanças espaciais/identificadores, não somente tokens.
a=s.index('    if (isTRUE(get0("MONITORA_OPCAO_OTIMIZAR_RELATORIOS_SUPORTE_POS"');b=s.index('  } else if (isTRUE(MONITORA_GERAR_RELATORIOS_SUPORTE_PAINEL))',a)
s=s[:a]+'''    MONITORA_RELATORIOS_SUPORTE_POS_CORRECOES <- monitora_relatorios_suporte_painel_gravar(registros_corrig, fase = "pos_correcoes", atualizar_saida_principal = FALSE)
    MONITORA_COMPARACAO_RELATORIOS_PRE_POS_CORRECOES <- monitora_relatorios_comparar_pre_pos_correcoes(MONITORA_CORRECOES_DIR)
    monitora_perf_registrar_checkpoint("relatorios_suporte_pos_correcoes", "relatórios pós-correções gerados e comparados com o pré", registros_corrig)
''' +s[b:]
replace('  monitora_qfield_isolar_entrada(entrada_dir, biologicos)', '  if (isTRUE(importar)) monitora_qfield_orientar_entrada(base_dir, entrada_dir)\n  monitora_qfield_isolar_entrada(entrada_dir, biologicos)')
replace('    warning("Falha ao gerar relatórios analíticos: ", conditionMessage(e), call. = FALSE)', '    monitora_operacao_msg("Relatórios ERRO", conditionMessage(e), "; registro: ", erro_arq, ". Demais produtos seguem; conclusão integral permanece bloqueada.")\n    warning("Falha ao gerar relatórios analíticos: ", conditionMessage(e), call. = FALSE)')
# Mensagens e relatórios solicitados também em atualização sem painel.
replace('    log_msg("Falha ao renderizar HTML: ", conditionMessage(e))', '    causa_pdf <<- paste0("HTML não gerado: ", conditionMessage(e))\n    log_msg(causa_pdf)\n    monitora_operacao_msg("Documento ERRO", causa_pdf)')
replace('  isTRUE(MONITORA_GERAR_DICIONARIOS_CORRECOES) ||', '  isTRUE(MONITORA_GERAR_DICIONARIOS_CORRECOES) ||\n  isTRUE(MONITORA_GERAR_RELATORIOS_SUPORTE_PAINEL) ||')
# Ramos curtos: não tratar mudanças espaciais como relatórios equivalentes.
a=s.index('  if (isTRUE(get0("MONITORA_OPCAO_OTIMIZAR_RELATORIOS_SUPORTE_POS"')
b=s.index('  if (!exists("monitora_relatorios_suporte_painel_gravar"',a)
s=s[:a]+s[b:]
# Recorte de centenas de imagens: quantidade real processada e duração, sem ETA fictício.
a=s.index('monitora_qfield_recortar_mbtiles <-');b=s.index('monitora_qfield_recorte_compativel <-',a)
p=s[a:b]
p=p.replace('  n_destino <- 0L;', '  pulso <- inicio; processados <- 0L\n  monitora_operacao_msg("QField", "Recortando ", n_fonte, " tiles de ", basename(fonte), ".")\n  n_destino <- 0L;',1)
p=p.replace('    if (!nrow(tile)) break', '    if (!nrow(tile)) break\n    processados <- processados + 1L\n    if (proc.time()[["elapsed"]] - pulso >= 15) {\n      monitora_operacao_msg("QField", "Recorte ", processados, "/", n_fonte, " tiles (", round(100 * processados / n_fonte), "%), ", round(proc.time()[["elapsed"]] - inicio), "s decorridos.")\n      pulso <- proc.time()[["elapsed"]]\n    }',1)
s=s[:a]+p+s[b:]
a=s.index('monitora_qfield_gerar <-');b=s.index('monitora_output_papel_produto <-',a)
p=s[a:b]
p=p.replace('    inicio <- proc.time()[["elapsed"]]', '    inicio <- proc.time()[["elapsed"]]\n    monitora_operacao_msg("QField", "Iniciando projeto de ", uc, "; insumos: ", entrada_dir)',1)
p=p.replace('    resultados[[uc]] <- res', '    monitora_operacao_msg("QField", uc, ": ", res$status[[1L]], "; ", round(res$segundos[[1L]], 1), "s; ", if (nzchar(res$zip[[1L]])) res$zip[[1L]] else res$motivo[[1L]])\n    resultados[[uc]] <- res')
s=s[:a]+p+s[b:]
replace('if (!is.null(MONITORA_ERRO_AUDITORIA_PRODUTOS_FINAIS)) {\n  nome_auditoria_final <-', 'if (length(MONITORA_AVISOS_ESTADO$itens)) {\n  monitora_operacao_msg("Avisos", sum(vapply(MONITORA_AVISOS_ESTADO$itens, function(x) x$ocorrencias, integer(1L))),\n    " ocorrências em ", length(MONITORA_AVISOS_ESTADO$itens), " grupos; consulte ",\n    file.path(MONITORA_LOG_DIR, paste0("avisos_execucao_", MONITORA_EXEC_ID, ".csv")), ".")\n}\nif (!is.null(MONITORA_ERRO_AUDITORIA_PRODUTOS_FINAIS)) {\n  nome_auditoria_final <-')
# Medir legendas no mesmo tipo de dispositivo Unicode usado para imagens, sem mexer no dispositivo do usuário.
replace('  plot_sem_caption <- plot_obj +', '  dispositivo_anterior <- grDevices::dev.cur()\n  arquivo_medicao <- tempfile(fileext = ".png")\n  if (requireNamespace("ragg", quietly = TRUE)) {\n    ragg::agg_capture(width = largura_total_in, height = 7, units = "in", res = 96)\n  } else {\n    args_medicao <- list(filename = arquivo_medicao, width = largura_total_in, height = 7, units = "in", res = 96)\n    if (isTRUE(capabilities("cairo"))) args_medicao$type <- "cairo"\n    do.call(grDevices::png, args_medicao)\n  }\n  dispositivo_medicao <- grDevices::dev.cur()\n  on.exit({\n    if (dispositivo_medicao %in% grDevices::dev.list()) grDevices::dev.off(dispositivo_medicao)\n    if (dispositivo_anterior %in% grDevices::dev.list()) grDevices::dev.set(dispositivo_anterior)\n    unlink(arquivo_medicao)\n  }, add = TRUE)\n  plot_sem_caption <- plot_obj +')
# Editar exclusivamente o módulo incorporado, preservando cálculos e fontes.
start=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(')
pos=start+s[start:].index('\n')+1;chunks=[]
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 chunks.append(m[1]);last=pos+m.end();pos=s.index('\n',pos)+1
module=gzip.decompress(base64.b64decode(''.join(chunks))).decode()
(root/'artifacts/v303/modulos_base.R').write_text(module)
a=module.index('monitora_fogo_api <-');b=module.index('monitora_fogo_cache_vigente <-',a)
module=module[:a]+(root/'tools/v303/fogo_api.R').read_text()+module[b:]
module=module.replace('  catuc <- monitora_fogo_api(', '  monitora_operacao_msg("Fogo", "Verificando fonte oficial para ", uc, "; aquisição completa sem snapshot vigente.")\n  catuc <- monitora_fogo_api(',1)
module=module.replace('returnGeometry="false"))\n  attrs', 'returnGeometry="false"), etapa="catalogo_ucs")\n  attrs',1)
module=module.replace('outSR=4326,returnGeometry="true"))', 'outSR=4326,returnGeometry="true"), etapa="limite_uc")',1)
module=module.replace('c(param,list(returnIdsOnly="true")))', 'c(param,list(returnIdsOnly="true")), etapa=paste0("ids_",basename(dirname(dirname(url)))))',1)
module=module.replace('meta <- monitora_fogo_api(url);', 'meta <- monitora_fogo_api(url, etapa=paste0("metadados_",ano));',1)
module=module.replace('returnDistinctValues="true",returnGeometry="false"))', 'returnDistinctValues="true",returnGeometry="false"), etapa=paste0("nomes_",ano))',1)
module=module.replace('outFields="*",returnGeometry="true"))', 'outFields="*",returnGeometry="true"), etapa=paste0("ano_",ano,"_lote_",j,"_de_",length(lotes)))',1)
module=module.replace('depois <- monitora_fogo_api(url);', 'depois <- monitora_fogo_api(url, etapa=paste0("verificacao_final_",ano));',1)
import sys
sys.path.insert(0,str(root/'tools/v303'))
exec((root/'tools/v303/aplicar_caminhos.py').read_text())
# Recalcular limites do payload após alterações físicas no fonte.
start=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(')
pos=start+s[start:].index('\n')+1
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 last=pos+m.end();pos=s.index('\n',pos)+1
encoded=base64.b64encode(gzip.compress(module.encode(),compresslevel=9,mtime=0)).decode()
s=s[:start]+s[start:start+s[start:].index('\n')+1]+',\n'.join('"'+v+'"' for v in textwrap.wrap(encoded,12000))+s[last:]
(root/'artifacts/v303/modulos_candidata.R').write_text(module)
(root/'R_monitora_campsav_alvo_global.R').write_text(s)
(root/'monitora_campsav_alvo_global_v3.0.3-rc01.R').write_text(s)
size=len(s.encode());crlf=size+s.count('\n')
assert crlf<5_000_000,(size,crlf)
print('Candidata LF',size,'CRLF',crlf,'bytes')
