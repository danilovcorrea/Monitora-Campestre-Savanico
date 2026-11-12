# Harmoniza a apresentação do Word com a paleta do PDF, sem reconstruir conteúdo.
monitora_v307_docx_estilo_aplicar <- function(arquivo_docx) {
  stopifnot(file.exists(arquivo_docx))
  if (!requireNamespace("xml2", quietly=TRUE) || !requireNamespace("zip", quietly=TRUE))
    stop("A apresentação DOCX exige xml2 e zip.", call.=FALSE)
  pasta <- tempfile("docx_estilo_"); dir.create(pasta)
  on.exit(unlink(pasta, recursive=TRUE), add=TRUE)
  utils::unzip(arquivo_docx, exdir=pasta)
  arq <- file.path(pasta, "word/styles.xml")
  est <- xml2::read_xml(arq)
  uri <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  ns <- c(w=uri)
  ord <- list(style=c("name","aliases","basedOn","next","link","autoRedefine","hidden","uiPriority","semiHidden","unhideWhenUsed","qFormat","locked","personal","personalCompose","personalReply","rsid","pPr","rPr","tblPr","trPr","tcPr","tblStylePr"),rPr=c("rStyle","rFonts","b","bCs","i","iCs","caps","smallCaps","strike","dstrike","outline","shadow","emboss","imprint","noProof","snapToGrid","vanish","webHidden","color","spacing","w","kern","position","sz","szCs","highlight","u","effect","bdr","shd","fitText","vertAlign","rtl","cs","em","lang","eastAsianLayout","specVanish","oMath"),
    pPr=c("pStyle","keepNext","keepLines","pageBreakBefore","framePr","widowControl","numPr","suppressLineNumbers","pBdr","shd","tabs","suppressAutoHyphens","kinsoku","wordWrap","overflowPunct","topLinePunct","autoSpaceDE","autoSpaceDN","bidi","adjustRightInd","snapToGrid","spacing","ind","contextualSpacing","mirrorIndents","suppressOverlap","jc","textDirection","textAlignment","textboxTightWrap","outlineLvl","divId","cnfStyle","rPr","sectPr"),pBdr=c("top","left","bottom","right","between","bar"))
  prop <- function(p, tag) {
    z <- xml2::xml_find_first(p, paste0("./w:",tag), ns)
    if (inherits(z,"xml_missing")) {
      ordem <- ord[[xml2::xml_name(p)]]; filhos <- xml2::xml_children(p)
      pos <- if (is.null(ordem)) length(filhos) else sum(match(xml2::xml_name(filhos),ordem,nomatch=999L) < match(tag,ordem,nomatch=999L))
      z <- xml2::xml_add_child(p, xml2::read_xml(paste0('<w:',tag,' xmlns:w="',uri,'"/>')), .where=pos)
    }
    z
  }
  attr <- function(p,k,v) xml2::xml_set_attr(p,paste0("w:",k),as.character(v),ns=ns)
  limpar_tema <- function(p) for (a in names(xml2::xml_attrs(p))[grepl("theme",names(xml2::xml_attrs(p)),ignore.case=TRUE)]) xml2::xml_set_attr(p,paste0("w:",sub("^.*:","",a)),NULL,ns=ns)
  fonte <- function(p) {
    f <- prop(p,"rFonts"); limpar_tema(f)
    for (a in c("ascii","hAnsi","eastAsia","cs")) attr(f,a,"Arial")
  }
  cor <- function(p,v) {z<-prop(p,"color");limpar_tema(z);attr(z,"val",v)}
  normalizar <- function(x) gsub("[^a-z0-9]","",tolower(iconv(x,from="UTF-8",to="ASCII//TRANSLIT")))
  especial <- "Consolas|Courier|mono|Cambria Math|Symbol|Wingdings|Mtextra"
  if (!length(xml2::xml_find_all(est,"./w:style[@w:styleId='FollowedHyperlink' or w:name[@w:val='FollowedHyperlink']]",ns))) {
    s<-xml2::xml_add_child(est,xml2::read_xml(paste0('<w:style xmlns:w="',uri,'" w:type="character" w:styleId="FollowedHyperlink"><w:name w:val="FollowedHyperlink"/></w:style>')))
    h<-xml2::xml_find_first(est,"./w:style[w:name[@w:val='Hyperlink']]",ns)
    if (!inherits(h,"xml_missing")) attr(prop(s,"basedOn"),"val",xml2::xml_attr(h,"styleId"))
  }
  ss <- xml2::xml_find_all(est,"./w:style",ns)
  if (!length(ss)) stop("DOCX sem estilos.",call.=FALSE)
  ids <- xml2::xml_attr(ss,"styleId")
  nomes <- normalizar(xml2::xml_attr(xml2::xml_find_first(ss,"./w:name",ns),"val"))
  pais <- match(xml2::xml_attr(xml2::xml_find_first(ss,"./w:basedOn",ns),"val"),ids)
  fontes <- vapply(ss,function(s)paste(xml2::xml_attrs(xml2::xml_find_first(s,"./w:rPr/w:rFonts",ns)),collapse=" "),character(1))
  protegido <- grepl("code|verbatim|preformat|tok$",nomes) | grepl(especial,fontes,ignore.case=TRUE)
  capa <- nomes %in% c("title","subtitle","author","date")
  corpo <- nomes %in% c("normal","bodytext","firstparagraph","compact")
  for (k in seq_along(ss)) {
    anterior <- list(protegido,capa,corpo)
    protegido <- protegido | ifelse(is.na(pais),FALSE,protegido[pais])
    capa <- capa | ifelse(is.na(pais),FALSE,capa[pais])
    corpo <- corpo | ifelse(is.na(pais),FALSE,corpo[pais])
    if (identical(anterior,list(protegido,capa,corpo))) break
  }
  niveis <- suppressWarnings(as.integer(xml2::xml_attr(xml2::xml_find_first(ss,"./w:pPr/w:outlineLvl",ns),"val"))+1L)
  for (i in seq_along(ss)) {
    if (protegido[i] || capa[i]) next
    nome <- nomes[i]; n <- niveis[i]
    if (grepl("^heading[1-9]$|^titulo[1-9]$",nome)) n <- as.integer(sub(".*([1-9])$","\\1",nome))
    indice <- grepl("toc|sumario|indice",nome)
    titulo <- !is.na(n) && n>=1L && n<=9L
    if (!corpo[i] && !titulo && !indice && !grepl("hyperlink|hiperlink|caption",nome)) next
    rp <- prop(ss[[i]],"rPr"); fonte(rp)
    if (nome=="normal") {
      cor(rp,"26342E"); attr(prop(rp,"sz"),"val",20);attr(prop(rp,"szCs"),"val",20)
      pp<-prop(ss[[i]],"pPr");attr(prop(pp,"jc"),"val","both")
      sp<-prop(pp,"spacing");attr(sp,"line",355);attr(sp,"lineRule","auto");attr(sp,"after",120)
    }
    if (grepl("hyperlink|hiperlink",nome)) cor(rp,"276A56")
    if (indice) cor(rp,"174B3B")
    if (titulo || grepl("tocheading",nome)) {
      if (!titulo) n<-1L
      niveis[i]<-n
      j<-min(n,3L); cor(rp,c("174B3B","276A56","3F6658")[j])
      for (a in c("sz","szCs")) attr(prop(rp,a),"val",c(40,30,24)[j])
      for (a in c("b","bCs")) attr(prop(rp,a),"val",1)
      pp<-prop(ss[[i]],"pPr");attr(prop(pp,"keepNext"),"val",1);attr(prop(pp,"keepLines"),"val",1);attr(prop(pp,"jc"),"val","left")
      sp<-prop(pp,"spacing");attr(sp,"before",c(300,255,200)[j]);attr(sp,"after",100);attr(sp,"line",264);attr(sp,"lineRule","auto")
      if (j==1L) {b<-prop(prop(pp,"pBdr"),"bottom");limpar_tema(b);attr(b,"val","single");attr(b,"sz",12);attr(b,"space",4);attr(b,"color","C7922E")}
    }
  }
  xml2::write_xml(est,arq)
  arq <- file.path(pasta,"word/document.xml");doc<-xml2::read_xml(arq)
  indice <- FALSE
  for (p in xml2::xml_find_all(doc,"./w:body/w:p",ns)) {
    sid<-xml2::xml_attr(xml2::xml_find_first(p,"./w:pPr/w:pStyle",ns),"val");j<-match(sid,ids)
    if (!is.na(j) && !is.na(niveis[j]) && niveis[j]==1L)
      indice<-normalizar(paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),collapse="")) %in% c("indice","sumario")
    for (r in xml2::xml_find_all(p,".//w:hyperlink//w:r",ns)) {
      ri<-match(xml2::xml_attr(xml2::xml_find_first(r,"./w:rPr/w:rStyle",ns),"val"),ids)
      if (!is.na(ri) && protegido[ri]) next
      if (any(grepl(especial,xml2::xml_attrs(xml2::xml_find_first(r,"./w:rPr/w:rFonts",ns)),ignore.case=TRUE))) next
      rp<-prop(r,"rPr");cor(rp,if(indice)"174B3B"else"276A56");fonte(rp)
    }
  }
  xml2::write_xml(doc,arq)
  novo<-tempfile(fileext=".docx");on.exit(unlink(novo),add=TRUE)
  zip::zipr(novo,list.files(pasta,recursive=TRUE,all.files=TRUE,no..=TRUE),root=pasta,include_directories=FALSE,mode="mirror")
  if (!file.copy(novo,arquivo_docx,overwrite=TRUE)) stop("Falha ao aplicar estilo DOCX.",call.=FALSE)
  invisible(arquivo_docx)
}
