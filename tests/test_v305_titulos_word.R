source('tools/v305/carregar.R');e<-monitora_v305_carregar()
d<-tempfile();dir.create(d);dir.create(file.path(d,'word'))
xml<-'<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:pPr><w:pStyle w:val="Heading2"/></w:pPr><w:r><w:t>Título</w:t></w:r></w:p><w:p><w:bookmarkStart w:id="1" w:name="monitora-fig-teste"/><w:bookmarkEnd w:id="1"/></w:p><w:p><w:bookmarkStart w:id="2" w:name="monitora-tab-teste"/><w:bookmarkEnd w:id="2"/></w:p><w:p><w:r><w:drawing/></w:r></w:p><w:p><w:r><w:t>Figura 1. Legenda.</w:t></w:r></w:p></w:body></w:document>'
writeLines(xml,file.path(d,'word/document.xml'));z<-tempfile(fileext='.docx');zip::zipr(z,'word/document.xml',root=d,include_directories=FALSE,mode='mirror')
e$monitora_relatorios_analiticos_docx_preservar_linhas_tabela(z)
u<-tempfile();dir.create(u);unzip(z,exdir=u);x<-xml2::read_xml(file.path(u,'word/document.xml'));ns<-xml2::xml_ns(x)
stopifnot(length(xml2::xml_find_all(x,'//w:p[w:bookmarkStart[starts-with(@w:name,"monitora-fig-") or starts-with(@w:name,"monitora-tab-")]]/w:pPr/w:keepNext',ns))==2L)
stopifnot(identical(xml2::xml_text(xml2::xml_find_all(x,'//w:t',ns)),c('Título','Figura 1. Legenda.')))
cat('PASS: marcador da figura acompanha título e imagem sem alterar texto ou marcador.\n')
