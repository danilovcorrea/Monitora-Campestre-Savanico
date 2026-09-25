# Normalização documental do estado prioritário; não recalcula indicadores.
a = s.index('  estado_resumir_relatorio <- function(')
b = s.index('  estado_prioritario_sintetico <-', a)
p = s[a:b]
old = 'c("Formação", "Cobertura (%)", "Nº de UAs",'
assert p.count(old) == 1
p = p.replace(old, 'c("Campanha (ano)", "Formação", "Cobertura (%)", "Nº de UAs",')
old = '    if (!length(col_indicador)) return(data.table::data.table())'
assert p.count(old) == 1
p = p.replace(old, '''    if (length(col_indicador) != 1L)
      stop("Estado prioritário: esperada uma única coluna de indicador além dos metadados e métricas.", call. = FALSE)''')
old = 'ordem_estado <- c("Eixo", "Formação", "Indicador",'
assert p.count(old) == 1
p = p.replace(old, 'ordem_estado <- c("Eixo", "Campanha (ano)", "Formação", "Indicador",')
s = s[:a] + p + s[b:]

# Esta síntese pode exceder uma página. Permitir quebra só no seu bloco,
# preservando a legenda junto ao início e as regras existentes das linhas.
a = s.index('monitora_relatorios_analiticos_html_legendas_tabelas <- function(')
b = s.index('monitora_relatorios_analiticos_auditar_numeracao_formato <-', a)
p = s[a:b]
old = '    bloco<-xml2::xml_add_sibling(tab,"div",.where="before",class="monitora-bloco-tabela")'
assert p.count(old) == 1
p = p.replace(old, '''    estado_prioritario <- length(xml2::xml_find_all(tab,
      "preceding-sibling::*[1]//*[@id='monitora-tab-estado-prioritario']")) == 1L
    bloco<-xml2::xml_add_sibling(tab,"div",.where="before",class="monitora-bloco-tabela")
    if (estado_prioritario) xml2::xml_set_attr(bloco,"style","break-inside:auto; page-break-inside:auto")''')
s = s[:a] + p + s[b:]
