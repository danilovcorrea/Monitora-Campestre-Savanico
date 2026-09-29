# Uma imagem por tema e métrica; ajuste medido no tamanho físico do relatório.
monitora_v305_figura_evidencia <- function(dados,titulo,arquivo,paleta,cfg_num) {
  dados<-data.table::copy(dados)
  dados[,rotulo_celula:=gsub('; ', ';',rotulo_celula,fixed=TRUE)]
  nr<-dados[,data.table::uniqueN(Indicador),by=Formação];nc<-data.table::uniqueN(dados$Periodo)
  largura<-6.65;altura<-min(8.15,max(4.8,.22*sum(nr$V1)+2.35))
  p<-ggplot2::ggplot(dados,ggplot2::aes(Periodo,Indicador,fill=classe_periodo_label))+
    ggplot2::geom_tile(colour='white',linewidth=.3)+
    ggplot2::geom_text(ggplot2::aes(label=rotulo_celula),size=2,lineheight=.9,colour='#17231E')+
    ggplot2::facet_grid(rows=ggplot2::vars(Formação),scales='free_y',space='free_y')+
    ggplot2::scale_y_discrete(labels=function(x)vapply(x,function(v)paste(strwrap(v,27),collapse='\n'),character(1)))+
    ggplot2::scale_fill_manual(values=paleta,drop=TRUE,na.value='#F0F0F0')+
    ggplot2::labs(x='Período comparado',y=NULL,fill=NULL,
      title=paste(strwrap(titulo,72),collapse='\n'),
      subtitle=paste(strwrap('Cada célula: efeito [IC95%], n e q (FDR-BH). LB = linha de base acumulada; AUM/RED/EST/INC/PAR = aumento/redução/estabilidade/inconclusivo/pares insuficientes; MUD/EST-C = mudança/estabilidade da composição; H = distância de Hellinger.',105),collapse='\n'),
      caption=paste(strwrap(paste0('Mudança direcional: q ≤ ',cfg_num('alpha',.05),', IC95% sem cruzar zero e efeito mínimo de ',cfg_num('efeito_minimo_pp',2),' p.p. Estabilidade: IC95% contido em ± ',cfg_num('margem_equivalencia_pp',5),' p.p. Valores completos da linha de base permanecem na auditoria. Associação temporal não implica causalidade.'),112),collapse='\n'))+
    ggplot2::theme_minimal(base_size=8)+ggplot2::theme(
      plot.title=ggplot2::element_text(size=9,face='bold',colour='#174B3B'),plot.title.position='plot',
      plot.subtitle=ggplot2::element_text(size=6.5),plot.caption=ggplot2::element_text(size=6.5,hjust=0),plot.caption.position='plot',
      panel.grid=ggplot2::element_blank(),panel.spacing=grid::unit(2,'mm'),
      axis.text.x=ggplot2::element_text(size=6.5,face='bold'),axis.text.y=ggplot2::element_text(size=6.5,lineheight=.9),
      strip.text.y=ggplot2::element_text(size=7,face='bold'),legend.position='bottom',legend.text=ggplot2::element_text(size=6.5),
      legend.key.height=grid::unit(3,'mm'),legend.key.width=grid::unit(3,'mm'),plot.margin=ggplot2::margin(4,4,4,4))+
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=3,byrow=TRUE))
  # Medir as células após a distribuição real de eixos, título e legendas.
  dev<-tempfile(fileext=".png");grDevices::png(dev,width=largura,height=altura,units="in",res=300,type="cairo");on.exit({grDevices::dev.off();unlink(dev)},add=TRUE)
  g<-ggplot2::ggplotGrob(p);grid::grid.newpage();grid::grid.draw(g);grid::grid.force()
  nomes<-grid::grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)$name
  nomes<-unique(nomes[grepl('^panel-[0-9]+-[0-9]+[.]',nomes)])
  stopifnot(length(nomes)==nrow(nr))
  celulas<-lapply(seq_along(nomes),function(i){grid::seekViewport(nomes[i]);w<-grid::convertWidth(grid::unit(1,'npc'),'mm',valueOnly=TRUE);h<-grid::convertHeight(grid::unit(1,'npc'),'mm',valueOnly=TRUE);grid::upViewport(0);c(w/(nc+.2),h/(nr$V1[i]+.2))})
  cw<-min(vapply(celulas,`[`,numeric(1),1));ch<-min(vapply(celulas,`[`,numeric(1),2))
  medidas<-function(pt) {
    gs<-lapply(as.character(dados$rotulo_celula),function(z)grid::textGrob(z,gp=grid::gpar(fontsize=pt,lineheight=.9)))
    c(max(vapply(gs,function(z)grid::convertWidth(grid::grobWidth(z),'mm',valueOnly=TRUE),numeric(1))),max(vapply(gs,function(z)grid::convertHeight(grid::grobHeight(z),'mm',valueOnly=TRUE),numeric(1))))
  }
  pt<-7;repeat{m<-medidas(pt);if(all(m<c(cw,ch)*.93)||pt<4)break;pt<-pt-.1}
  if(pt<4)stop('Figura de evidência não cabe em uma página com fonte legível: ',titulo,call.=FALSE)
  p$layers[[2]]$aes_params$size<-pt/ggplot2::.pt
  # Rótulos dos indicadores precisam respeitar a mesma altura de linha.
  p<-p+ggplot2::theme(axis.text.y=ggplot2::element_text(size=min(6.5,pt+1),lineheight=.9))
  ggplot2::ggsave(arquivo,p,device=function(filename,width,height,bg,...)grDevices::png(filename,width=width,height=height,units='in',res=300,bg=bg,type='cairo'),width=largura,height=altura,dpi=300,bg='white',limitsize=FALSE)
  medida<-data.table::data.table(arquivo=basename(arquivo),resultados=nrow(dados),periodos=nc,linhas=sum(nr$V1),fonte_pt=pt,largura_celula_mm=cw,altura_celula_mm=ch,texto_largura_mm=m[1],texto_altura_mm=m[2],largura_pol=largura,altura_pol=altura)
  destino<-file.path(dirname(dirname(arquivo)),"layout_evidencias.csv")
  if(file.exists(destino)){anteriores<-data.table::fread(destino);keep<-which(anteriores$arquivo!=basename(arquivo));medida<-data.table::rbindlist(list(anteriores[keep],medida),fill=TRUE)}
  data.table::fwrite(medida,destino,bom=TRUE)
  invisible(arquivo)
}
