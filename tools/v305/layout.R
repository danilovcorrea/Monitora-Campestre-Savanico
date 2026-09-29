monitora_relatorios_analiticos_html_mesclar_contexto <- function(arquivo_html) invisible(TRUE)
monitora_v305_clonar_xml <- function(q) {
  ns<-xml2::xml_ns(q)
  raiz<-xml2::read_xml(paste0('<root ',paste0('xmlns:',names(ns),'="',unname(ns),'"',collapse=' '),'>',as.character(q),'</root>'))
  xml2::xml_children(raiz)[[1]]
}
# Ordem dos elementos conforme o esquema WordprocessingML; propriedades simples são únicas.
monitora_v305_ordenar_ooxml <- function(doc) {
  ns<-xml2::xml_ns(doc)
  ordens<-list(
    sectPr=c('headerReference','footerReference','footnotePr','endnotePr','type','pgSz','pgMar','paperSrc','pgBorders','lnNumType','pgNumType','cols','formProt','vAlign','noEndnote','titlePg','textDirection','bidi','rtlGutter','docGrid','printerSettings','sectPrChange'),
    tblPr=c('tblStyle','tblpPr','tblOverlap','bidiVisual','tblStyleRowBandSize','tblStyleColBandSize','tblW','jc','tblCellSpacing','tblInd','tblBorders','shd','tblLayout','tblCellMar','tblLook','tblCaption','tblDescription','tblPrChange'),
    tcPr=c('cnfStyle','tcW','gridSpan','hMerge','vMerge','tcBorders','shd','noWrap','tcMar','textDirection','tcFitText','vAlign','hideMark','headers','cellIns','cellDel','cellMerge','tcPrChange'),
    pPr=c('pStyle','keepNext','keepLines','pageBreakBefore','framePr','widowControl','numPr','suppressLineNumbers','pBdr','shd','tabs','suppressAutoHyphens','kinsoku','wordWrap','overflowPunct','topLinePunct','autoSpaceDE','autoSpaceDN','bidi','adjustRightInd','snapToGrid','spacing','ind','contextualSpacing','mirrorIndents','suppressOverlap','jc','textDirection','textAlignment','textboxTightWrap','outlineLvl','divId','cnfStyle','rPr','sectPr','pPrChange'),
    rPr=c('rStyle','rFonts','b','bCs','i','iCs','caps','smallCaps','strike','dstrike','outline','shadow','emboss','imprint','noProof','snapToGrid','vanish','webHidden','color','spacing','w','kern','position','sz','szCs','highlight','u','effect','bdr','shd','fitText','vertAlign','rtl','cs','em','lang','eastAsianLayout','specVanish','oMath','rPrChange'),
    trPr=c('cnfStyle','divId','gridBefore','gridAfter','wBefore','wAfter','cantSplit','trHeight','tblHeader','tblCellSpacing','jc','hidden','ins','del','trPrChange'))
  for(tag in names(ordens))for(p in xml2::xml_find_all(doc,paste0('.//w:',tag),ns)) {
    ch<-xml2::xml_children(p);n<-xml2::xml_name(ch);keep<-!duplicated(n,fromLast=TRUE)|n%in%c('headerReference','footerReference')
    if(all(keep)&&identical(order(match(n,ordens[[tag]]),na.last=TRUE),seq_along(n)))next
    xml2::xml_remove(ch[!keep]);ch<-xml2::xml_children(p);nn<-xml2::xml_name(ch)
    xml2::xml_remove(ch,free=FALSE)
    for(k in order(match(nn,ordens[[tag]]),na.last=TRUE))xml2::xml_add_child(p,ch[[k]],.copy=FALSE)
  }
  invisible(doc)
}
# Dimensões em twips; conteúdo científico não é reduzido nem serializado.
monitora_v305_larguras <- function(cab,mat,portrait=9978L) {
  cab<-as.character(cab);stopifnot(length(cab)>0L,ncol(mat)==length(cab))
  # Medir palavras inteiras no dispositivo tipográfico, preservando o retrato.
  dev<-tempfile(fileext=".pdf");grDevices::cairo_pdf(dev,family="sans");on.exit({grDevices::dev.off();unlink(dev)},add=TRUE)
  tokens<-lapply(seq_along(cab),function(j){
    v<-c(cab[j],as.character(mat[,j]));v<-v[!is.na(v)]
    v<-gsub("https?://[^ ]+|[^ ]+[.](csv|xlsx|png|zip)","arquivo",v)
    unlist(strsplit(paste(v,collapse=" "),"[[:space:];]+"))
  })
  for(fonte in seq(9,7,by=-.5)) {
    minimos<-vapply(tokens,function(v)max(c(420,graphics::strwidth(v,units="inches",cex=fonte/12)*1440+140)),numeric(1))
    if(sum(minimos)<=portrait)break
  }
  if(sum(minimos)>portrait)stop("Tabela não cabe em retrato com fonte mínima de 7 pt; revisar cabeçalhos e conteúdo editorial: ",paste(cab,collapse=" | "),call.=FALSE)
  pesos<-vapply(seq_along(cab),function(j)sqrt(max(c(nchar(cab[j]),pmin(nchar(as.character(mat[,j])),80)),na.rm=TRUE)),numeric(1))
  w<-floor(minimos+(portrait-sum(minimos))*pesos/sum(pesos));w[length(w)]<-w[length(w)]+portrait-sum(w)
  list(larguras=as.integer(w),paisagem=FALSE,total=portrait,minimos=minimos,fonte=fonte)
}
monitora_v305_html_base <- monitora_relatorios_analiticos_html_colunas
monitora_relatorios_analiticos_html_colunas <- function(doc) {
  monitora_v305_html_base(doc)
  xml2::xml_add_child(xml2::xml_find_first(doc,".//head"),"style",
    "@page{size:A4 portrait}table{table-layout:fixed!important;font-size:9pt!important}td,th{overflow-wrap:normal!important;word-break:normal!important;hyphens:none;vertical-align:top}th{font-size:8.5pt!important}.monitora-centro{text-align:center!important}.monitora-texto{text-align:left!important}td.monitora-caminho{overflow-wrap:anywhere!important}div.monitora-bloco-tabela{break-inside:auto!important;page-break-inside:auto!important}.monitora-legenda-tabela{break-inside:avoid!important;page-break-inside:avoid!important;break-after:avoid!important}table.monitora-tabela-curta,div.monitora-bloco-tabela.monitora-tabela-curta{break-inside:avoid!important;page-break-inside:avoid!important}table{break-before:avoid!important}thead{display:table-header-group;break-inside:avoid;break-after:avoid}tbody>tr:first-child{break-before:avoid}tr{break-inside:avoid}figure{margin:6mm 0!important;break-inside:avoid}figcaption{margin-top:3mm}")
  for(tab in xml2::xml_find_all(doc,".//table")) {
    cab<-xml2::xml_text(xml2::xml_find_all(tab,"./thead/tr[1]/th"));rows<-xml2::xml_find_all(tab,"./tbody/tr")
    if(!length(cab)||!length(rows))next
    vals<-lapply(rows,function(row)xml2::xml_text(xml2::xml_find_all(row,"./td")))
    if(any(lengths(vals)!=length(cab)))stop("Tabela HTML com células inconsistentes")
    m<-do.call(rbind,vals);w<-monitora_v305_larguras(cab,m);centro<-monitora_v305_colunas_centrais(cab,m)
    altura_estimada<-sum(apply(sweep(nchar(m),2,pmax(5,(w$larguras-140)/(w$fonte*10)),"/"),1,function(v)max(ceiling(v))))*w$fonte*1.35+nrow(m)*6+60
    if(nrow(m)<=24L&&altura_estimada<640) {
      xml2::xml_set_attr(tab,"class",paste(na.omit(xml2::xml_attr(tab,"class")),"monitora-tabela-curta"))
      wrapper<-xml2::xml_parent(tab);cl<-xml2::xml_attr(wrapper,"class");if(is.na(cl))cl<-""
      xml2::xml_set_attr(wrapper,"class",paste(cl,"monitora-tabela-curta"))
    }
    for(q in xml2::xml_find_all(tab,".//td|.//th"))xml2::xml_set_attr(q,"style",paste0("font-size:",w$fonte,"pt!important;padding:3px!important"))
    xml2::xml_remove(xml2::xml_find_all(tab,"./colgroup"));cg<-xml2::xml_add_child(tab,"colgroup",.where=0)
    for(v in w$larguras)xml2::xml_add_child(cg,"col",style=paste0("width:",round(100*v/w$total,3),"%"))
    for(j in seq_along(cab)) {
      for(cell in c(as.list(xml2::xml_find_all(tab,"./thead/tr[1]/th"))[j],lapply(rows,function(row)xml2::xml_find_all(row,"./td")[[j]]))) {
        cl<-xml2::xml_attr(cell,"class");if(is.na(cl))cl<-""
        cl<-gsub("monitora-numero|monitora-codigo|monitora-centro|monitora-texto","",cl)
        xml2::xml_set_attr(cell,"class",paste(cl,if(centro[j])"monitora-centro"else"monitora-texto"))
      }
    }
    for(j in seq_along(cab)) for(row in rows) {
      td<-xml2::xml_find_all(row,"./td")[[j]];v<-xml2::xml_text(td)
      if(grepl("[0-9]{4};[0-9]{4}",v))xml2::xml_text(td)<-gsub(";",";\u200B",v,fixed=TRUE)
      cl<-xml2::xml_attr(td,"class");if(is.na(cl))cl<-""

      if(grepl("[/\\\\]|[.]csv|[.]png|[.]xlsx",v))cl<-paste(cl,"monitora-caminho")
      xml2::xml_set_attr(td,"class",cl)
    }
  }
  invisible(doc)
}
# Recriar somente a apresentação OOXML; preservar a matriz de células intacta.
monitora_relatorios_analiticos_docx_preservar_linhas_tabela <- function(arquivo_docx) {
  pasta<-tempfile("docx_editorial_");dir.create(pasta);on.exit(unlink(pasta,recursive=TRUE),add=TRUE)
  utils::unzip(arquivo_docx,exdir=pasta);arq<-file.path(pasta,"word/document.xml");doc<-xml2::read_xml(arq);ns<-xml2::xml_ns(doc)
  wuri<-"http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  node<-function(x)xml2::read_xml(paste0('<w:root xmlns:w="',wuri,'">',x,'</w:root>'))
  add<-function(p,x,where=Inf){for(n in xml2::xml_children(node(x)))xml2::xml_add_child(p,n,.where=where)}
  prop<-function(p,tag) {q<-xml2::xml_find_first(p,paste0('./w:',tag),ns);if(inherits(q,'xml_missing')) {add(p,paste0('<w:',tag,'/>'),0);q<-xml2::xml_find_first(p,paste0('./w:',tag),ns)};q}
  text<-function(p)paste(xml2::xml_text(xml2::xml_find_all(p,'.//w:t',ns)),collapse='')
  for(tab in xml2::xml_find_all(doc,'.//w:body/w:tbl',ns)) {
    rows<-xml2::xml_find_all(tab,'./w:tr',ns);cells<-lapply(rows,function(z)xml2::xml_find_all(z,'./w:tc',ns));cab<-vapply(cells[[1]],text,character(1))
    if(length(rows)<2L)next
    if(any(lengths(cells)!=length(cab)))stop('Tabela Word com matriz irregular')
    m<-do.call(rbind,lapply(cells[-1L],function(cs)vapply(cs,text,character(1))))
    width<-monitora_v305_larguras(cab,m);centro<-monitora_v305_colunas_centrais(cab,m);pr<-prop(tab,'tblPr')
    xml2::xml_remove(xml2::xml_find_all(pr,'./w:tblW|./w:tblLayout|./w:tblInd|./w:tblBorders|./w:tblCellMar|./w:jc',ns))
    add(pr,paste0('<w:tblW w:type="dxa" w:w="',width$total,'"/><w:tblLayout w:type="fixed"/><w:jc w:val="center"/><w:tblCellMar><w:top w:w="60" w:type="dxa"/><w:left w:w="60" w:type="dxa"/><w:bottom w:w="60" w:type="dxa"/><w:right w:w="60" w:type="dxa"/></w:tblCellMar>'))
    add(pr,paste0('<w:tblBorders>',paste0('<w:',c('top','left','bottom','right','insideH','insideV'),' w:val="single" w:sz="4" w:color="BCC9C3"/>',collapse=''),'</w:tblBorders>'))
    grid<-prop(tab,'tblGrid');xml2::xml_remove(xml2::xml_children(grid));for(v in width$larguras)add(grid,paste0('<w:gridCol w:w="',v,'"/>'))
    for(i in seq_along(rows)) {
      rp<-prop(rows[[i]],'trPr');xml2::xml_remove(xml2::xml_find_all(rp,'./w:trHeight|./w:cantSplit',ns))
      # Linhas excepcionalmente altas podem continuar na página seguinte.
      altura<-max(vapply(seq_along(cab),function(j)ceiling(nchar(text(cells[[i]][[j]]))/max(8,(width$larguras[j]-150)/90)),numeric(1)))*240+150
      if(altura<13000)add(rp,'<w:cantSplit/>')
      if(i==1L)add(rp,'<w:tblHeader/>')
      for(j in seq_along(cab)) {
        c<-cells[[i]][[j]];cp<-prop(c,'tcPr');xml2::xml_remove(xml2::xml_find_all(cp,'./w:tcW|./w:shd|./w:vAlign',ns));add(cp,paste0('<w:tcW w:type="dxa" w:w="',width$larguras[j],'"/><w:vAlign w:val="center"/><w:shd w:fill="',if(i==1)'24543C'else if(i%%2L)'F3F7F4'else'FFFFFF','"/>'))
        for(t in xml2::xml_find_all(c,'.//w:t',ns)){v0<-xml2::xml_text(t);if(grepl('[0-9]{4};[0-9]{4}',v0))xml2::xml_text(t)<-gsub(';',';\u200B',v0,fixed=TRUE)}
        v<-text(c);num<-grepl('^[−+<>=≤≥±0-9eE.,% ()/:–-]+$|^(NA|NE)$',v)
        align<-if(centro[j])'center'else'left'
        for(p in xml2::xml_find_all(c,'./w:p',ns)){pp<-prop(p,'pPr');xml2::xml_remove(xml2::xml_find_all(pp,'./w:jc|./w:spacing|./w:keepNext',ns));add(pp,paste0('<w:jc w:val="',align,'"/><w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/>'))}
        for(run in xml2::xml_find_all(c,'.//w:r',ns)){rp2<-prop(run,'rPr');xml2::xml_remove(xml2::xml_find_all(rp2,'./w:sz|./w:szCs|./w:color',ns));add(rp2,paste0('<w:sz w:val="',width$fonte*2,'"/><w:szCs w:val="',width$fonte*2,'"/><w:color w:val="',if(i==1)'FFFFFF'else'24332D','"/>'));if(i==1)add(rp2,'<w:b/>')}
      }
    }
    caption<-xml2::xml_find_first(tab,'preceding-sibling::w:p[1]',ns);add(prop(caption,'pPr'),'<w:keepNext/>')

  }
  for(p in xml2::xml_find_all(doc,'.//w:body/w:p',ns)) {
    pp<-prop(p,'pPr');style<-xml2::xml_attr(xml2::xml_find_first(pp,'./w:pStyle',ns),'val');if(is.na(style))style<-''
    if(length(xml2::xml_find_all(p,'.//w:drawing',ns))) {add(pp,'<w:keepNext/><w:spacing w:before="180" w:after="90"/>');next}
    if(grepl('^Heading',style)) {add(pp,'<w:keepNext/>');next}
    if(startsWith(text(p),'Figura ')) {xml2::xml_remove(xml2::xml_find_all(pp,'./w:keepNext|./w:spacing',ns));add(pp,'<w:spacing w:before="60" w:after="240"/>')}
  }
  # Capa e índice podem pedir a mesma quebra; uma única quebra evita página vazia.
  for(p in xml2::xml_find_all(doc,'.//w:body/w:p[w:r/w:br[@w:type="page"]]',ns)) {
    anterior<-xml2::xml_find_first(p,'preceding-sibling::*[1]',ns)
    if(!nzchar(text(p))&&!inherits(anterior,'xml_missing')&&!nzchar(text(anterior))&&length(xml2::xml_find_all(anterior,'./w:r/w:br[@w:type="page"]',ns)))xml2::xml_remove(p)
  }
  monitora_v305_ordenar_ooxml(doc)
  xml2::write_xml(doc,arq)
  z<-tempfile(fileext='.docx');on.exit(unlink(z),add=TRUE);zip::zipr(z,list.files(pasta,recursive=TRUE,all.files=TRUE,no..=TRUE),root=pasta,include_directories=FALSE,mode='mirror');stopifnot(file.copy(z,arquivo_docx,overwrite=TRUE));invisible(arquivo_docx)
}
monitora_v305_docx_capa_base <- monitora_relatorios_analiticos_docx_adequar_capa
monitora_relatorios_analiticos_docx_adequar_capa <- function(arquivo_docx,arquivo_auditoria=NULL) {
  z<-monitora_v305_docx_capa_base(arquivo_docx,arquivo_auditoria)
  pasta<-tempfile('indice_gate_');dir.create(pasta);on.exit(unlink(pasta,recursive=TRUE),add=TRUE);utils::unzip(arquivo_docx,files='word/document.xml',exdir=pasta)
  d<-xml2::read_xml(file.path(pasta,'word/document.xml'));ns<-xml2::xml_ns(d)
  campos<-xml2::xml_find_all(d,".//w:fldSimple[contains(@w:instr,'PAGEREF')]",ns)
  textos<-vapply(campos,function(p)paste(xml2::xml_text(xml2::xml_find_all(p,'.//w:t',ns)),collapse=''),character(1))
  # Após salvar no Word, campos simples podem ser convertidos a campos complexos.
  instr<-xml2::xml_text(xml2::xml_find_all(d,'.//w:instrText',ns));complexos<-sum(grepl('PAGEREF',instr,fixed=TRUE))
  pendente<-any(!grepl('^[0-9]+$',trimws(textos))) || (length(campos)+complexos)==0L
  status<-if(pendente)'pendente_paginacao_word'else'campos_preenchidos_requer_conferencia_destinos'
  cat(sprintf('[Word] Índice: %s; %d referências. A paginação deve ser conferida no documento final do Word.\n',status,length(campos)+complexos))
  if(!is.null(arquivo_auditoria))data.table::fwrite(data.table::data.table(status=status,referencias=length(campos)+complexos,conferencia_destinos_aprovada=FALSE),sub('[.]csv$','_indice_status.csv',arquivo_auditoria),bom=TRUE)
  invisible(z)
}
