monitora_relatorios_analiticos_catalogo_tabelas <- function() c(
  `esforco-fluxos`="Entrada, retenção, ausência e retorno de UAs por ano",
  `esforco-percentuais`="Indicadores proporcionais do esforço amostral",
  `esforco-formacao`="Esforço amostral por formação vegetacional e ano",
  `estado-atual`="Cobertura e composição das categorias gerais na campanha mais recente",
  `estado-prioritario`="Estado dos indicadores ecológicos prioritários",
  calendario="Calendário das campanhas e UAs amostradas",
  recomendacoes="Recomendações para a UC e seus fundamentos",
  robustez="Auditoria da robustez inferencial",
  continuidade="Continuidade das unidades amostrais",
  `herbaceas-lenhosas`="Estado dos componentes herbáceo e lenhoso vivos",
  nativas="Estado das formas de vida nativas",
  exoticas="Estado das formas de vida exóticas",
  `secas-mortas`="Estado das formas de vida secas ou mortas",
  material="Estado do material botânico em decomposição e solo/rochas",
  `achados-temporais`="Seleção de resultados temporais e consistência da evidência",
  composicao="Resultados da análise temporal da composição geral",
  `hipoteses-gestao`="Evidências, hipóteses ecológicas e implicações para gestão",
  `contexto-impactos`="Contexto declarado de impactos, manejo e uso",
  rastreabilidade="Produtos analíticos e rastreabilidade",
  `fogo-hipoteses`="Hipóteses e decisões dos testes condicionais de fogo",
  `fogo-registros`="Registros espaciais de fogo por ano e modalidade",
  `fogo-contrastes`="Contrastes inferenciais entre fogo e cobertura vegetal",
  `fogo-combustivel`="Triagem da cobertura de material potencialmente combustível",
  `fogo-fonte`="Qualidade e cobertura da fonte cartográfica de fogo",
  `fogo-cobertura`="Cobertura vegetal por modalidade de fogo e ano cartográfico",
  `fogo-elegibilidade`="Elegibilidade e alcance das análises de fogo",
  `clima-diagnosticos`="Diagnósticos dos modelos climáticos condicionais",
  `clima-coeficientes`="Coeficientes das associações climáticas condicionais",
  `clima-predicao`="Avaliação preditiva dos modelos climáticos",
  `clima-anomalias`="Clima antecedente e anomalias por campanha",
  `clima-associacoes`="Associações exploratórias entre clima e cobertura vegetal",
  `clima-elegibilidade`="Elegibilidade e interpretação das análises climáticas",
  `multivariada-metodos`="Métodos da análise multivariada e seus limites",
  `multivariada-recorte`="Recorte temporal e comparabilidade da análise multivariada",
  `multivariada-ajustes`="Ajustes exploratórios da associação multivariada",
  revisitas="Calendário e revisitas: suporte por coleta",
  `metodos-perguntas`="Métodos por pergunta e limites de inferência"
)
monitora_relatorios_analiticos_kable <- function(x, align = NULL, id = NULL) {
  catalogo <- monitora_relatorios_analiticos_catalogo_tabelas()
  if (length(id) != 1L || is.na(id) || !id %in% names(catalogo))
    stop("Tabela analítica sem identidade/título no catálogo editorial.", call. = FALSE)
  vazio <- is.null(x) || !is.data.frame(x) || !nrow(x)
  marcador <- paste0("<!-- monitora-tabela ", id, " ", if (vazio) "omitida" else "incluida", " -->")
  if (vazio) return(paste(marcador, "*Não há dados suficientes para esta tabela.*", sep="\n\n"))
  paste(marcador, paste0("Table: ", catalogo[[id]]),
    paste(capture.output(knitr::kable(as.data.frame(x), format="pipe", align=align,
      row.names=FALSE, escape=TRUE)), collapse="\n"), sep="\n\n")
}
monitora_relatorios_analiticos_numerar <- function(conteudo, dir_relatorio) {
  linhas <- unlist(strsplit(paste(conteudo, collapse="\n"), "\n", fixed=TRUE), use.names=FALSE)
  catalogo <- monitora_relatorios_analiticos_catalogo_tabelas()
  registros <- list(); saida <- character(); usados <- character()
  cont <- c(tabela=0L, figura=0L); secoes <- integer(6); secao <- ""
  em_codigo <- FALSE; cerca <- ""; nivel_anterior <- 0L
  adicionar <- function(tipo, id, numero, titulo, status="incluido", motivo="", arquivo="") {
    chave <- paste(tipo,id)
    if (chave %in% usados) stop("Identidade editorial duplicada: ", chave, call.=FALSE)
    usados <<- c(usados,chave)
    registros[[length(registros)+1L]] <<- data.frame(tipo,id,numero=as.character(numero),titulo,
      secao,status,motivo,arquivo,stringsAsFactors=FALSE)
  }
  i <- 1L
  while (i <= length(linhas)) {
    l <- linhas[i]
    if (grepl("^\\s*(```+|~~~+)",l)) {
      atual <- substr(trimws(l),1L,3L)
      if (!em_codigo) {em_codigo<-TRUE;cerca<-atual} else if (identical(atual,cerca)) em_codigo<-FALSE
      saida<-c(saida,l); i<-i+1L; next
    }
    if (!em_codigo && startsWith(l,"<!-- monitora-tabela ")) {
      m <- regmatches(l,regexec("^<!-- monitora-tabela ([a-z0-9-]+) (incluida|omitida) -->$",l))[[1L]]
      if (length(m)!=3L || !m[2] %in% names(catalogo)) stop("Marcador de tabela inválido.",call.=FALSE)
      id<-m[2];titulo<-catalogo[[id]]
      if (m[3]=="omitida") {
        adicionar("tabela",id,NA_character_,titulo,"omitido","Tabela sem linhas disponíveis")
      } else {
        j<-i+1L
        while(j<=length(linhas) && !nzchar(trimws(linhas[j])))j<-j+1L
        if(j>length(linhas) || !identical(linhas[j],paste0("Table: ",titulo)))stop("Legenda de tabela ausente ou divergente: ",id,call.=FALSE)
        cont['tabela']<-cont['tabela']+1L
        adicionar("tabela",id,cont['tabela'],titulo)
        saida<-c(saida,"",paste0("[]{#monitora-tab-",id,"}"),"",paste0("Table: Tabela ",cont['tabela']," — ",titulo))
        i<-j
      }
    } else if (!em_codigo && grepl("^#{1,6} ",l) && !grepl("\\.unnumbered",l)) {
      nivel<-nchar(sub("^(#+).*","\\1",l))
      if(nivel>nivel_anterior+1L)stop("Salto de nível na hierarquia de seções: ",l,call.=FALSE)
      titulo<-sub("^#+ +","",l)
      if(grepl("^[0-9]+([.][0-9]+)* ",titulo))stop("Seção já numerada antes da composição final.",call.=FALSE)
      secoes[nivel]<-secoes[nivel]+1L
      if(nivel<6L)secoes[(nivel+1L):6L]<-0L
      secao<-paste(secoes[seq_len(nivel)],collapse=".");nivel_anterior<-nivel
      id<-paste0("monitora-secao-",gsub(".","-",secao,fixed=TRUE))
      adicionar("secao",id,secao,titulo)
      saida<-c(saida,"",paste0(strrep("#",nivel)," ",secao," ",titulo," {#",id,"}"))
    } else if (!em_codigo && grepl("^<figure",l)) {
      m<-regmatches(l,regexec('^<figure[^>]*><img src="([^"]+)"[^>]*><figcaption>(.*)</figcaption></figure>$',l))[[1L]]
      if(length(m)!=3L)stop("Figura analítica fora do formato editorial esperado.",call.=FALSE)
      relativo<-m[2];arquivo<-file.path(dir_relatorio,relativo)
      if(!file.exists(arquivo) || is.na(file.info(arquivo)$size) || file.info(arquivo)$size<=0)
        stop("Figura esperada ausente ou vazia: ",relativo,call.=FALSE)
      if(grepl("[.]png$",arquivo,ignore.case=TRUE))tryCatch(
        {invisible(png::readPNG(arquivo,native=TRUE))},
        error=function(e)stop("Figura PNG inválida: ",relativo," — ",conditionMessage(e),call.=FALSE))
      id<-paste0("fig-",substr(digest::digest(relativo,algo="sha256",serialize=FALSE),1L,16L))
      titulo<-sub("^Figura [0-9]+[.] +","",m[3])
      cont['figura']<-cont['figura']+1L
      adicionar("figura",id,cont['figura'],titulo,arquivo=relativo)
      l<-sub("<figcaption>.*</figcaption>",paste0("<figcaption>Figura ",cont['figura'],". ",titulo,"</figcaption>"),l)
      saida<-c(saida,"",paste0("[]{#monitora-",id,"}"),"",l,"")
    } else saida<-c(saida,l)
    i<-i+1L
  }
  secao <- ""
  for(id in setdiff(names(catalogo),sub("^tabela ","",usados[startsWith(usados,"tabela ")])))
    adicionar("tabela",id,NA_character_,catalogo[[id]],"omitido","Não selecionada para esta versão editorial ou pelas condições da análise")
  auditoria<-do.call(rbind,registros);rownames(auditoria)<-NULL
  # References use semantic IDs and are resolved only after final selection.
  for(k in seq_along(saida)) {
    refs<-regmatches(saida[k],gregexpr("\\[\\[(tabela|figura):[a-z0-9-]+\\]\\]",saida[k]))[[1L]]
    for(ref in refs) {
      v<-strsplit(sub("\\]\\]$","",sub("^\\[\\[","",ref)),":",fixed=TRUE)[[1L]]
      z<-auditoria[auditoria$tipo==v[1] & auditoria$id==v[2] & auditoria$status=="incluido",]
      if(nrow(z)!=1L)stop("Referência a elemento não incluído: ",ref,call.=FALSE)
      alvo<-if(v[1]=="tabela")paste0("monitora-tab-",v[2])else paste0("monitora-",v[2])
      rotulo<-if(v[1]=="tabela")"Tabela" else "Figura"
      saida[k]<-gsub(ref,paste0("[",rotulo," ",z$numero,"](#",alvo,")"),saida[k],fixed=TRUE)
    }
  }
  monitora_relatorios_analiticos_validar_numeracao(saida,auditoria)
  list(conteudo=saida,auditoria=auditoria)
}
monitora_relatorios_analiticos_linhas_ativas <- function(x) {
  ativo <- rep(TRUE, length(x)); cerca <- ""
  for(i in seq_along(x)) {
    if(grepl("^\\s*(```+|~~~+)",x[i])) {
      marca <- substr(trimws(x[i]),1L,3L)
      if(!nzchar(cerca))cerca<-marca else if(identical(cerca,marca))cerca<-""
      ativo[i]<-FALSE
    } else ativo[i]<-!nzchar(cerca)
  }
  ativo
}
monitora_relatorios_analiticos_validar_numeracao <- function(conteudo, auditoria) {
  x<-unlist(strsplit(paste(conteudo,collapse="\n"),"\n",fixed=TRUE),use.names=FALSE)
  x<-x[monitora_relatorios_analiticos_linhas_ativas(x)]
  falhar<-function(msg)stop("Gate de numeração: ",msg,call.=FALSE)
  for(tipo in c("tabela","figura")) {
    z<-auditoria[auditoria$tipo==tipo & auditoria$status=="incluido",]
    esperado<-as.character(seq_len(nrow(z)))
    if(!identical(z$numero,esperado))falhar(paste("sequência inválida no manifesto de",tipo))
    if(tipo=="tabela") {
      legendas<-x[grepl("^Table: Tabela ",x)]
      corretas<-if(nrow(z)) paste0("Table: Tabela ",z$numero," — ",z$titulo) else character()
      # One Markdown table separator per caption; no table may bypass the registry.
      nt<-sum(grepl("^\\|[ :|-]+\\|[[:space:]]*$",x))
      if(nt!=nrow(z))falhar("tabela sem legenda ou legenda sem tabela")
      for(j in which(grepl("^Table: Tabela ",x))) {
        k<-j+1L;while(k<=length(x) && !nzchar(trimws(x[k])))k<-k+1L
        if(k+1L>length(x) || !startsWith(trimws(x[k]),"|") || !grepl("^\\|[ :|-]+\\|[[:space:]]*$",x[k+1L]))falhar("legenda de tabela fora de posição")
      }
    } else {
      figuras<-x[grepl("^<figure",x)]
      legendas<-sub("^.*<figcaption>(.*)</figcaption>.*$","\\1",figuras)
      corretas<-if(nrow(z)) paste0("Figura ",z$numero,". ",z$titulo) else character()
    }
    if(!identical(unname(legendas),unname(corretas)))falhar(paste("legendas divergentes de",tipo))
  }
  z<-auditoria[auditoria$tipo=="secao" & auditoria$status=="incluido",]
  titulos<-x[grepl("^#{1,6} [0-9]",x)]
  esperados<-if(nrow(z)) paste0(vapply(strsplit(z$numero,".",fixed=TRUE),function(v)strrep("#",length(v)),character(1L))," ",z$numero," ",z$titulo," {#",z$id,"}") else character()
  if(!identical(unname(titulos),unname(esperados)))falhar("hierarquia/títulos divergentes")
  if(any(grepl("^<!-- monitora-tabela |\\[\\[(tabela|figura):",x)))falhar("marcador não resolvido")
  ids<-unlist(regmatches(paste(x,collapse="\n"),gregexpr("\\{#monitora-[^}]+\\}",paste(x,collapse="\n"))))
  if(anyDuplicated(ids))falhar("âncora duplicada")
  texto<-paste(x,collapse="\n")
  links<-unlist(regmatches(texto,gregexpr("\\]\\(#monitora-[^)]+\\)",texto)))
  destinos<-sub("^\\]\\(#(.*)\\)$","\\1",links)
  alvos<-sub("^\\{#(.*)\\}$","\\1",ids)
  if(any(!destinos %in% alvos))falhar("referência interna sem destino")
  invisible(TRUE)
}
monitora_relatorios_analiticos_html_legendas_tabelas <- function(arquivo) {
  doc<-xml2::read_html(arquivo,encoding="UTF-8")
  tabelas<-xml2::xml_find_all(doc,".//table")
  for(i in seq_along(tabelas)) {
    tab<-tabelas[[i]];cap<-xml2::xml_find_first(tab,"./caption")
    if(inherits(cap,"xml_missing"))stop("Tabela HTML sem legenda antes da paginação.",call.=FALSE)
    # Paged.js can discard a native caption when moving a table to the next page.
    # An external paragraph retains the visible caption; aria-labelledby labels the table.
    id<-paste0("monitora-legenda-tab-",i)
    p<-xml2::xml_new_root("p");xml2::xml_set_attr(p,"id",id)
    xml2::xml_set_attr(p,"class","monitora-legenda-tabela")
    xml2::xml_set_attr(p,"style","font-weight:600; text-align:left; break-after:avoid; page-break-after:avoid; margin-bottom:0")
    xml2::xml_set_text(p,trimws(xml2::xml_text(cap)))
    xml2::xml_add_sibling(tab,p,.where="before")
    xml2::xml_set_attr(tab,"aria-labelledby",id);xml2::xml_remove(cap)
  }
  xml2::write_html(doc,arquivo,options=c("format","no_declaration"))
  invisible(TRUE)
}
monitora_relatorios_analiticos_auditar_numeracao_formato <- function(arquivo, auditoria) {
  tipo<-tolower(tools::file_ext(arquivo)); ns<-c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  if(tipo=="docx") {
    pasta<-tempfile("numeracao_docx_");dir.create(pasta);on.exit(unlink(pasta,recursive=TRUE),add=TRUE)
    utils::unzip(arquivo,files="word/document.xml",exdir=pasta)
    doc<-xml2::read_xml(file.path(pasta,"word/document.xml"))
    paragrafos<-xml2::xml_find_all(doc,".//w:body/w:p",ns)
    texto<-vapply(paragrafos,function(p)paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),collapse=""),character(1L))
    estilos<-vapply(paragrafos,function(p)xml2::xml_attr(xml2::xml_find_first(p,"./w:pPr/w:pStyle",ns),"val"),character(1L))
    titulos_docx<-texto[!is.na(estilos) & grepl("^Heading[1-6]$",estilos)]
    tabelas<-xml2::xml_find_all(doc,".//w:body/w:tbl",ns)
    legendas_tab<-texto[startsWith(texto,"Tabela ")]
    legendas_fig<-texto[grepl("^Figura [0-9]+[.]",texto)]
    for(tab in tabelas) {
      anterior<-xml2::xml_find_first(tab,"preceding-sibling::*[1][self::w:p]",ns)
      legenda<-paste(xml2::xml_text(xml2::xml_find_all(anterior,".//w:t",ns)),collapse="")
      if(!startsWith(legenda,"Tabela "))stop("Gate de numeração DOCX: tabela sem legenda associada.",call.=FALSE)
    }
  } else if(tipo=="html") {
    doc<-xml2::read_html(arquivo,encoding="UTF-8")
    tabelas<-xml2::xml_find_all(doc,".//table")
    legendas_tab<-trimws(xml2::xml_text(xml2::xml_find_all(doc,".//table/caption|.//p[@class='monitora-legenda-tabela']")))
    for(tab in tabelas) {
      if(length(xml2::xml_find_all(tab,"./caption")))next
      p<-xml2::xml_find_first(tab,"preceding-sibling::*[1][self::p]")
      if(!identical(xml2::xml_attr(p,"id"),xml2::xml_attr(tab,"aria-labelledby")) || is.na(xml2::xml_attr(p,"id")))
        stop("Gate de numeração HTML: tabela sem legenda associada.",call.=FALSE)
    }
    legendas_fig<-trimws(xml2::xml_text(xml2::xml_find_all(doc,".//figure/figcaption")))
    texto<-trimws(xml2::xml_text(xml2::xml_find_all(doc,".//h1|.//h2|.//h3|.//h4|.//h5|.//h6")))
  } else stop("Formato não previsto no gate editorial.",call.=FALSE)
  normalizar<-function(x)trimws(gsub("[[:space:]]+"," ",gsub("<[^>]*>","",x)))
  for(t in c("tabela","figura","secao")) {
    z<-auditoria[auditoria$tipo==t & auditoria$status=="incluido",]
    esperado<-if(t=="tabela")paste0("Tabela ",z$numero," — ",z$titulo) else if(t=="figura")paste0("Figura ",z$numero,". ",z$titulo)else paste(z$numero,z$titulo)
    if(!nrow(z))esperado<-character()
    obtido<-if(t=="tabela")legendas_tab else if(t=="figura")legendas_fig else if(tipo=="docx")titulos_docx[grepl("^[0-9]+([.][0-9]+)* ",titulos_docx)]else texto[grepl("^[0-9]+([.][0-9]+)* ",texto)]
    if(!identical(unname(normalizar(obtido)),unname(normalizar(esperado))))stop("Gate de numeração ",tipo,": divergência em ",t,call.=FALSE)
    if(t=="tabela" && length(tabelas)!=nrow(z))stop("Gate de numeração ",tipo,": quantidade de tabelas divergente.",call.=FALSE)
  }
  invisible(TRUE)
}
