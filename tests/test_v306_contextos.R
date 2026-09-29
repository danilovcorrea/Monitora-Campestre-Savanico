suppressPackageStartupMessages(library(data.table));source('tools/v305/carregar.R');e<-monitora_v305_carregar('monitora_campsav_alvo_global_v3.0.6-rc01.R');p<-tempfile();dir.create(p)
write<-function(x,n)fwrite(x,file.path(p,paste0(n,'.csv')),bom=TRUE);txt<-function(x)paste(x,collapse='\n')
# Ausência de blocos opcionais nunca apaga a entrada.
x<-c('# Teste','Contextualização preservada.','<figure><img src="figuras/epoca_linha_base_sintese.png"><figcaption>Época</figcaption></figure>')
y<-e$monitora_v306_amplo(x,p);stopifnot(any(y=='Contextualização preservada.'),any(startsWith(y,'<figure')))
# Uma formação, lacuna e valores constantes: nenhuma outra formação ou tendência inventada.
s<-data.table(ANO=c(2019,2021),form_veg='savanica',grupo_grafico='categorias_gerais',tipo_metrica='cobertura',categoria='nativa',categoria_label='Nativas',media_percent=20,n_UA=12)
write(s,'series_anuais_relatorios_por_ua');y<-txt(e$monitora_v306_series(p,'categorias_gerais','cobertura'));stopifnot(grepl('Savânica',y),!grepl('Campestre',y),grepl('2020',y),grepl('0,0 p.p.',y,fixed=TRUE))
write(s[1],'series_anuais_relatorios_por_ua');y<-txt(e$monitora_v306_series(p,'categorias_gerais','cobertura'));stopifnot(grepl('apenas um ano',y),!grepl('diferença descritiva de',y))
write(data.table(estado='exposição indeterminada',posicoes_com_intersecao=0,posicoes_elegiveis=0,anos_consultados=''),'fogo_situacao_historico');y<-e$monitora_v306_fogo(p);stopifnot(grepl('indeterminada',y),!grepl('0 de 0',y))
write(data.table(ANO=c(2019,2021),metrica='temp_c',media=25,anomalia_media=0,n_completas=1),'clima_resumo_90dias');stopifnot(grepl('sem variação',e$monitora_v306_clima(p,TRUE)))
# Clima funciona sem bloco Fogo; CSV antigo não sobrepõe desativação nesta execução.
x<-c('# Resumo','<div class="callout">Clima — resumo anterior</div>')
write(data.table(fogo='desativado',clima='desativado'),'nar_contexto');y<-txt(e$monitora_v306_amplo(x,p));stopifnot(grepl('não há confirmação',y),!grepl('25,0',y))
write(data.table(fogo='desativado',clima='concluído'),'nar_contexto');y<-txt(e$monitora_v306_amplo(x,p));stopifnot(grepl('25,0',y),!grepl('Fogo:',y))
# Notas automáticas são reconstruídas uma única vez; notas científicas permanecem.
x<-c('# Métodos',e$monitora_relatorios_analiticos_kable(data.frame(Ano=2019,Valor=NA_real_),id='esforco-percentuais'),'Nota da [[tabela:esforco-percentuais]]: Amostragem obtida em campo; limitação específica.','# Resultados','Fim preservado.')
processar<-function(x)e$monitora_v306_notas_finais(e$monitora_v305_editorial(e$monitora_v306_amplo(e$monitora_v306_editorial(e$monitora_v306_limpar_notas(x,p),p),p),p,'teste'))
a<-processar(x);b<-processar(a);for(z in list(a,b)){stopifnot(sum(grepl('^Nota da .*monitora-nota-automatica',z))==1L,sum(grepl('NA identifica indicadores',z))==1L,any(grepl('Amostragem obtida em campo; limitação específica.',z,fixed=TRUE)),sum(grepl('^# Glossário',z))==1L)}
# Mudança de termos não renomeia produtos técnicos ou caminhos.
z<-e$monitora_v306_termos('Calendário e revisitas; `calendario_revisitas.csv`; <img src="figuras/revisitas.png">');stopifnot(grepl('Época de amostragem e reamostragens',z),grepl('`calendario_revisitas.csv`',z,fixed=TRUE),grepl('src="figuras/revisitas.png"',z,fixed=TRUE))
# Cabeçalho deve acompanhar dados; demais linhas continuam pagináveis.
d<-file.path(p,'docx');dir.create(d);dir.create(file.path(d,'word'))
cel<-function(s)paste0('<w:tc><w:p><w:r><w:t>',s,'</w:t></w:r></w:p></w:tc>')
xml<-paste0('<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:r><w:t>Tabela</w:t></w:r></w:p><w:tbl>',paste0('<w:tr>',vapply(c('Ano','2024','2026'),cel,character(1)),'</w:tr>',collapse=''),'</w:tbl></w:body></w:document>')
writeLines(xml,file.path(d,'word/document.xml'));arq<-file.path(p,'teste.docx');zip::zipr(arq,'word/document.xml',root=d,include_directories=FALSE,mode='mirror')
e$monitora_relatorios_analiticos_docx_preservar_linhas_tabela(arq);unzip(arq,exdir=d);doc<-xml2::read_xml(file.path(d,'word/document.xml'));ns<-xml2::xml_ns(doc)
stopifnot(length(xml2::xml_find_all(doc,'//w:tr[1]//w:pPr/w:keepNext',ns))==1L,length(xml2::xml_find_all(doc,'//w:tr[position()>1]//w:keepNext',ns))==0L,identical(xml2::xml_text(xml2::xml_find_all(doc,'//w:t',ns)),c('Tabela','Ano','2024','2026')))
unlink(p,recursive=TRUE);cat('PASS: formação única, lacunas, ano único, constantes, indeterminação, módulo desativado, ordem, notas idempotentes, caminhos e cabeçalhos Word.\n')
