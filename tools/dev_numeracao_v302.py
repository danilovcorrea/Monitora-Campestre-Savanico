from pathlib import Path
import re,base64,gzip,textwrap
root=Path(__file__).resolve().parents[1]
s=(root/'monitora_campsav_alvo_global_v3.0.1.R').read_text()
def endparen(t,p):
 depth=1;q=None
 while depth:
  c=t[p]
  if q:
   if c=='\\':p+=2;continue
   if c==q:q=None
  elif c in "\"'`":q=c
  elif c=='(':depth+=1
  elif c==')':depth-=1
  p+=1
 return p
start=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(')
match=list(re.finditer(r'^"([A-Za-z0-9+/=]+)"',s[start:],re.M))
# Embedded payload is followed by regular R code; base64-only lines belong to this one block.
chunks=[];pos=start+s[start:].index('\n')+1
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 chunks.append(m.group(1));last=pos+m.end();pos=s.index('\n',pos)+1
module=gzip.decompress(base64.b64decode(''.join(chunks))).decode()
main_ids=['esforco-fluxos','esforco-percentuais','esforco-fluxos','esforco-percentuais','esforco-formacao','estado-atual','estado-prioritario','calendario','recomendacoes','robustez','esforco-formacao','continuidade','estado-atual','herbaceas-lenhosas','nativas','exoticas','secas-mortas','material','achados-temporais','composicao','calendario','hipoteses-gestao','contexto-impactos','recomendacoes','rastreabilidade']
module_ids=['fogo-hipoteses','fogo-registros','fogo-contrastes','fogo-combustivel','fogo-fonte','fogo-cobertura','fogo-elegibilidade','clima-diagnosticos','clima-coeficientes','clima-predicao','clima-anomalias','clima-associacoes','clima-elegibilidade','multivariada-metodos','multivariada-recorte','multivariada-ajustes','revisitas','metodos-perguntas']
def annotate(t,ids):
 matches=list(re.finditer(r'monitora_relatorios_analiticos_kable\(',t));assert len(matches)==len(ids)
 for m,id_ in reversed(list(zip(matches,ids))):
  p=endparen(t,m.end())-1;t=t[:p]+', id="'+id_+'"'+t[p:]
 return t
(root/'artifacts/v302_numeracao').mkdir(parents=True,exist_ok=True)
(root/'artifacts/v302_numeracao/modulos_base.R').write_text(module)
module=annotate(module,module_ids)
module=module.replace('l<-l[!grepl("^', 'l<-l[!startsWith(l,"<!-- monitora-tabela multivariada-metodos ") & !grepl("^',1)
# Repack only the embedded module; other original resources remain byte-for-byte unchanged.
encoded=base64.b64encode(gzip.compress(module.encode(),compresslevel=9,mtime=0)).decode()
s=s[:start]+s[start: start+s[start:].index('\n')+1]+',\n'.join('"'+v+'"' for v in textwrap.wrap(encoded,12000))+s[last:]
s=annotate(s,main_ids)
s=s.replace('MONITORA_SCRIPT_VERSAO <- "3.0.1"','MONITORA_SCRIPT_VERSAO <- "3.0.2-rc01"').replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.1-20260922-r01"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.2-rc01-20260923-r01"')
a=s.index('monitora_relatorios_analiticos_kable <- function(');b=s.index('monitora_relatorios_analiticos_figura <- function(',a)
s=s[:a]+(root/'tools/v302_editorial_helpers.R').read_text()+'\n'+s[b:]
# Word: preserve numbered tables as native editable tables, compacting wide ones losslessly.
a=s.index('  tabela_docx <- function(bloco) {');b=s.index('  corpo_tabelas <- character(0)',a)
s=s[:a]+'''  tabela_docx <- function(bloco) {
    if (length(bloco) < 3L) return(bloco)
    cab <- tabela_celulas(bloco[[1L]])
    if (length(cab) <= 6L) return(bloco)
    linhas <- lapply(bloco[-c(1L, 2L)], tabela_celulas)
    if (any(lengths(linhas) != length(cab))) stop("Tabela Word com número inconsistente de células.", call. = FALSE)
    rotular <- function(x, ii) paste(paste0(cab[ii], ": ", x[ii]), collapse = "; ")
    c("| Identificação | Resultados e contexto |", "| --- | --- |",
      vapply(linhas, function(x) paste0("| ", rotular(x, 1:2), " | ", rotular(x, 3:length(cab)), " |"), character(1L)))
  }
'''+s[b:]
# Honor escaped pipe cells while transforming a wide table.
s=s.replace('trimws(strsplit(x, "\\\\|", perl = TRUE)[[1L]])','trimws(strsplit(x, "(?<!\\\\\\\\)\\\\|", perl = TRUE)[[1L]])')
# Preserve Markdown table/caption syntax through the Word prose converter.
s=s.replace('  transformar <- function(linha) {', '  transformar <- function(linha) {\n    if (startsWith(linha, "|") || startsWith(linha, "Table: ")) return(linha)',1)
# Final assembly now numbers figures. Module standalone documents retain their original captions.
a=s.index('  criar_figura_numerada <- function() {');b=s.index('  rel_mapa_resumo <-',a)
s=s[:a]+'''  fig_sint <- fig_det <- function(relativo, legenda, largura = "100%") {
    if (is.na(relativo) || !nzchar(relativo)) return("")
    monitora_relatorios_analiticos_figura(relativo, legenda, largura)
  }
'''+s[b:]
# Correct hierarchy: individual annual panels are children of the sensitivity subsection.
s=s.replace('paste0("### Resultados com o esforço incorporado a partir de ", ano_i)','paste0("#### Resultados com o esforço incorporado a partir de ", ano_i)')
# Index consumes the numbered headings and their final IDs, not a second numbering system.
a=s.index('  titulos <- sub("^# +", "", x[pos])',s.index('monitora_relatorios_analiticos_indice <-'))
b=s.index('  indice <- c(',a)
s=s[:a]+'''  titulos <- sub(" +\\\\{#.*$", "", sub("^# +", "", x[pos]))
  ids <- sub("^.*\\\\{#([^}]+)\\\\}$", "\\\\1", x[pos])
  sem_id <- !grepl("\\\\{#[^}]+\\\\}$", x[pos])
  ids[sem_id] <- sprintf("monitora-secao-%02d", seq_along(pos))[sem_id]
  x[pos[sem_id]] <- paste0(x[pos[sem_id]], " {#", ids[sem_id], "}")
'''+s[b:]
s=s.replace('  pos <- grep("^# [^#]", x)', '  pos <- which(grepl("^# [^#]", x) & monitora_relatorios_analiticos_linhas_ativas(x))',1)
s=s.replace('com tabelas editáveis, esforço real, continuidade, séries anuais por UA e painéis inferenciais auditáveis.', 'com tabelas editáveis e numeradas, esforço real, continuidade, séries anuais por UA e painéis inferenciais auditáveis. Seções, tabelas e figuras recebem números após a seleção do conteúdo, por UC e por relatório; itens omitidos não reservam números. O arquivo auditoria_numeracao registra os elementos incluídos e as tabelas não selecionadas. No Word, tabelas largas podem apresentar identificação e resultados em duas colunas, preservando os valores.',1)
s=s.replace('  conteudo <- monitora_relatorios_analiticos_indice(conteudo)','''  editorial <- monitora_relatorios_analiticos_numerar(conteudo, dir_relatorio)
  conteudo <- monitora_relatorios_analiticos_indice(editorial$conteudo)
  monitora_relatorios_analiticos_validar_numeracao(conteudo, editorial$auditoria)
  data.table::fwrite(editorial$auditoria, file.path(dir_relatorio,
    paste0("auditoria_numeracao_", base_nome, ".csv")), bom = TRUE, na = "")''',1)
s=s.replace('      monitora_relatorios_analiticos_docx_preservar_linhas_tabela(docx_candidato)','''      monitora_relatorios_analiticos_auditar_numeracao_formato(docx_candidato, editorial$auditoria)
      monitora_relatorios_analiticos_docx_preservar_linhas_tabela(docx_candidato)''',1)
s=s.replace('xml2::xml_find_first(secao,"./h1")', 'xml2::xml_find_first(secao,"./h1|./h2|./h3|./h4|./h5|./h6")')
s=s.replace('      registrar_erro("html", "renderizacao_html", conditionMessage(e))', '      if(file.exists(html))unlink(html)\n      registrar_erro("html", "renderizacao_html", conditionMessage(e))',1)
s=s.replace('      monitora_relatorios_analiticos_html_mesclar_contexto(html)','''      monitora_relatorios_analiticos_auditar_numeracao_formato(html, editorial$auditoria)
      monitora_relatorios_analiticos_html_mesclar_contexto(html)
      monitora_relatorios_analiticos_html_legendas_tabelas(html)
      monitora_relatorios_analiticos_auditar_numeracao_formato(html, editorial$auditoria)''',1)
s=s.replace('"figcaption { color:', '"caption { caption-side:top; text-align:left; font-weight:600; break-after:avoid; page-break-after:avoid; }",\n    "figcaption { color:',1)
(root/'R_monitora_campsav_alvo_global.R').write_text(s)
(root/'artifacts/v302_numeracao/modulos_candidata.R').write_text(module)
print('Candidata:',len(s.encode()),'bytes LF;',len(s.encode())+s.count('\n'),'bytes CRLF')
