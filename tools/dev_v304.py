from pathlib import Path
import re,base64,gzip,textwrap,json
root=Path(__file__).resolve().parents[1]
(root/'artifacts/v304').mkdir(parents=True,exist_ok=True)
s=(root/'monitora_campsav_alvo_global_v3.0.3.R').read_text()
def change(a,b,n=1):
 global s
 assert s.count(a)>=n,a[:120]
 s=s.replace(a,b,n)
def function(name):
 a=s.index(name+' <- function(');b=s.index('\n}\n',a)+3
 return s[a:b]
change('# Versão 3.0.3-rc01','# Versão 3.0.4-rc01')
change('MONITORA_SCRIPT_VERSAO <- "3.0.3"','MONITORA_SCRIPT_VERSAO <- "3.0.4-rc01"')
change('MONITORA_SCRIPT_BUILD_ID <- "v3.0.3-20260924-r01"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.4-rc01-20260925-r08"')
# Dependências documentais declaradas antes de qualquer controlador ou painel.
parts=[]
for n in ['monitora_relatorios_analiticos_resolver_candidato_navegador','monitora_relatorios_analiticos_resolver_navegador','monitora_relatorios_analiticos_rscript_executavel','monitora_relatorios_analiticos_chrome_print_isolado']:
 p=function(n);s=s.replace(p,'',1);parts.append(p)
change('monitora_doc_render_rmd <- function(',''.join(parts)+'monitora_doc_render_rmd <- function(')
# Painéis de evidência: divisão editorial sem repetir ou eliminar resultados.
a=s.index('  painel_inferencial <- function(');b=s.index('  temas_inferenciais <-',a);p=s[a:b]
start=p.index('    largura_titulo <-')
p=p[:start]+'''    dados_integrais <- data.table::copy(dados_plot)
    partes <- list()
    for (ff in unique(dados_integrais$Formação)) {
      z <- dados_integrais[Formação == ff]
      ps <- levels(droplevels(z$Periodo)); ins <- levels(droplevels(z$Indicador))
      for (pp in split(ps, ceiling(seq_along(ps)/3L)))
        for (ii in split(ins, ceiling(seq_along(ins)/6L))) {
          zz <- z[as.character(Periodo) %in% pp & as.character(Indicador) %in% ii]
          if(nrow(zz)) partes[[length(partes)+1L]] <- zz
        }
    }
    stopifnot(sum(vapply(partes,nrow,integer(1L))) == nrow(dados_integrais))
    id_original <- id
    for (parte in seq_along(partes)) {
    dados_plot <- partes[[parte]]
    id <- paste0(id_original, "_p", sprintf("%02d", parte))
    titulo_parte <- paste0(titulo, " — ", unique(dados_plot$Formação), " — painel ", parte, "/", length(partes))
'''+p[start:]
p=p.replace('size = 2.45,','size = 3.5,').replace('lineheight = 0.90,','lineheight = 1.02,')
p=p.replace('strwrap(titulo, width = largura_titulo)','strwrap(titulo_parte, width = 68L)')
p=p.replace('"LB ", classe_linha_base_curta,','"LB ", classe_linha_base_curta,') # labels are built earlier; wrap separately below
p=p.replace('axis.text.y = ggplot2::element_text(size = 8.4),','axis.text.y = ggplot2::element_text(size = 10.5),')
p=p.replace('    ggplot2::scale_fill_manual(', '    ggplot2::scale_y_discrete(labels=function(x) vapply(x,function(v) paste(strwrap(v,24L),collapse="\\n"),character(1L))) +\n    ggplot2::scale_fill_manual(')
p=p.replace('0.60 * max(', '0.86 * max(')
p=p.replace('    invisible(NULL)\n  }','    }\n    invisible(NULL)\n  }')
s=s[:a]+p+s[b:]
# Ano explícito na mesma tabela que determina o recorte, conservado no CSV/Word.
change('    out[]\n  }\n  estado_categorias <- tabela_estado_nucleo', '    out[, `Campanha (ano)` := ano_recente]\n    data.table::setcolorder(out, c("Campanha (ano)", setdiff(names(out), "Campanha (ano)")))\n    out[]\n  }\n  estado_categorias <- tabela_estado_nucleo')
change('"## Categorias gerais na campanha mais recente"','"## Categorias gerais"')
change('"# Indicadores ecológicos prioritários",','"# Indicadores ecológicos prioritários",\n    "As tabelas de estado indicam a campanha efetivamente utilizada: o último ano disponível em cada conjunto. As figuras temporais apresentam os períodos próprios, que podem ser diferentes. Ausência de dados atuais não representa ausência do indicador.",')
# Paginação: legenda e começo da tabela juntos; largura limitada e cabeçalhos únicos.
change('    xml2::xml_add_sibling(tab,p,.where="before")\n    xml2::xml_set_attr(tab,"aria-labelledby",id);xml2::xml_remove(cap)', '''    xml2::xml_set_attr(tab,"aria-labelledby",id);xml2::xml_remove(cap)
    bloco<-xml2::xml_add_sibling(tab,"div",.where="before",class="monitora-bloco-tabela")
    xml2::xml_add_child(bloco,p);xml2::xml_add_child(bloco,tab);xml2::xml_remove(tab)''')
change('    "table { width:100%; border-collapse:collapse;', '    ".monitora-bloco-tabela { break-inside:avoid; page-break-inside:avoid; }",\n    "table { table-layout:fixed; max-width:100%; width:100%; overflow-wrap:anywhere; border-collapse:collapse;')
# Colunas automáticas respeitam o conteúdo mínimo; números não podem partir em linhas.
helper = """monitora_relatorios_analiticos_html_colunas <- function(doc) {
  xml2::xml_add_child(xml2::xml_find_first(doc,".//head"),"style",
    "table{table-layout:auto!important}th{overflow-wrap:normal}td.monitora-numero{white-space:nowrap;overflow-wrap:normal}")
  for(td in xml2::xml_find_all(doc,".//td")) {
    valor<-trimws(xml2::xml_text(td))
    if(nzchar(valor) && grepl("^(NA|NE|[−+<>=≤≥±0-9eE.,% ()/:–-]+)$",valor)) {
      atual<-xml2::xml_attr(td,"class");if(is.na(atual))atual<-""
      xml2::xml_set_attr(td,"class",trimws(paste(atual,"monitora-numero")))
    }
  }
  invisible(doc)
}
"""
change('monitora_relatorios_analiticos_html_legendas_tabelas <- function(arquivo) {',helper+'monitora_relatorios_analiticos_html_legendas_tabelas <- function(arquivo) {')
change('  tabelas<-xml2::xml_find_all(doc,".//table")','  monitora_relatorios_analiticos_html_colunas(doc)\n  tabelas<-xml2::xml_find_all(doc,".//table")')
# Handler registrado depois dos recursos Paged.js e antes do preview automático.
js='''(function(){if(!window.Paged)return;class MonitoraTabelas extends Paged.Handler{afterPageLayout(page){page.querySelectorAll('table').forEach(t=>{let seen=new Set();t.querySelectorAll('thead tr').forEach(r=>{let key=r.textContent.replace(/\\s+/g,' ').trim();if(seen.has(key))r.remove();else seen.add(key)});t.querySelectorAll('thead').forEach(h=>{if(!h.children.length)h.remove()})})}}Paged.registerHandlers(MonitoraTabelas)})();'''
# Insert into legend helper only, not generic HTML output functions.
a=s.index('monitora_relatorios_analiticos_html_legendas_tabelas <-');b=s.index('monitora_relatorios_analiticos_auditar_numeracao_formato <-',a);p=s[a:b]
p=p.replace('  xml2::write_html(doc,arquivo', '  xml2::xml_add_child(xml2::xml_find_first(doc,".//body"),"script", '+json.dumps(js)+')\n  xml2::write_html(doc,arquivo')
s=s[:a]+p+s[b:]
# Remaining patches kept separately for review.
for file in ['habitos.py','caminhos.py','graficos.py','tabela_sintetica.py']:
 if (root/'tools/v304'/file).exists():exec((root/'tools/v304'/file).read_text())
(root/'R_monitora_campsav_alvo_global.R').write_text(s)
(root/'monitora_campsav_alvo_global_v3.0.4-rc01.R').write_text(s)
lf=len(s.encode());crlf=lf+s.count('\n');assert crlf<5_000_000,(lf,crlf)
print('Candidata:',lf,'bytes LF;',crlf,'CRLF')
