# Mesmos escores e mesma unidade geométrica; somente apresentação dos painéis.
monitora_v305_figura_trajetoria <- function(centro,percentuais,arquivo) {
  d<-data.table::copy(centro)
  if(!'protocolo_rotulo'%in%names(d))d[,protocolo_rotulo:=sub('^.*_CAMPSAV_','Protocolo ',protocolo_origem)]
  stopifnot(nrow(d)>0L,length(percentuais)>=2L,all(is.finite(d$Eixo1)),all(is.finite(d$Eixo2)))
  linhas<-d[,if(.N>=2L).SD,by=.(painel,trecho)]
  xr<-range(d$Eixo1);yr<-range(d$Eixo2);dx<-max(diff(xr),1)
  xl<-xr+c(-1,1)*dx*.12
  # Espaço para rótulos, sem alterar a razão 1:1 nem as coordenadas dos pontos.
  dy<-max(diff(yr)*1.4,diff(xl)*.28,1);yl<-mean(yr)+c(-.5,.5)*dy
  p<-ggplot2::ggplot(d,ggplot2::aes(Eixo1,Eixo2,group=interaction(painel,trecho),color=form_veg))+
    ggplot2::geom_path(data=linhas,arrow=grid::arrow(length=grid::unit(.08,'inches')),linewidth=.55)+
    ggplot2::geom_point(size=2.4)+
    ggrepel::geom_text_repel(ggplot2::aes(label=paste0(ano_num,' (n=',UAs,')')),size=3,seed=30550,
      box.padding=.45,point.padding=.3,min.segment.length=0,max.overlaps=Inf,max.iter=10000,max.time=2,
      segment.color='grey45',segment.size=.25,show.legend=FALSE)+
    ggplot2::facet_wrap(~protocolo_rotulo,ncol=1,labeller=ggplot2::label_wrap_gen(60))+
    ggplot2::coord_equal(xlim=xl,ylim=yl,expand=FALSE)+ggplot2::theme_bw(base_size=10)+
    ggplot2::theme(legend.position='bottom',panel.spacing=grid::unit(4,'mm'),
      plot.title=ggplot2::element_text(size=11,face='bold'),plot.caption=ggplot2::element_text(size=8,hjust=0),
      strip.text=ggplot2::element_text(size=9),plot.margin=ggplot2::margin(5,6,5,5))+
    ggplot2::labs(color='Formação',x=sprintf('Eixo 1 (%.1f%%)',percentuais[1]),y=sprintf('Eixo 2 (%.1f%%)',percentuais[2]),
      title='Trajetórias da cobertura em eixos comuns',caption=paste('Médias das mesmas UAs por versão/formação.','Eixos descritivos; setas não representam causalidade ou significância.',sep='\n'))
  width<-6.65;tmp<-tempfile(fileext='.png');grDevices::png(tmp,width=width,height=8,units='in',res=300,type='cairo')
  g<-ggplot2::ggplotGrob(p)
  fw<-grid::convertWidth(sum(g$widths),'in',valueOnly=TRUE);fh<-grid::convertHeight(sum(g$heights),'in',valueOnly=TRUE)
  nw<-sum(as.numeric(g$widths[grid::unitType(g$widths)=='null']));nh<-sum(as.numeric(g$heights[grid::unitType(g$heights)=='null']))
  height<-fh+(width-fw)*nh/nw
  grDevices::dev.off();unlink(tmp)
  if(!is.finite(height)||height<=0||height>8.15)stop('Figura de trajetórias excede uma página em retrato; revisar a disposição dos painéis.')
  ggplot2::ggsave(arquivo,p,width=width,height=height,units='in',dpi=300,bg='white',
    device=function(filename,width,height,bg,...)grDevices::png(filename,width=width,height=height,units='in',res=300,type='cairo',bg=bg,...))
  invisible(list(largura_pol=width,altura_pol=height,escala_xy=1,xlim=xl,ylim=yl,paineis=data.table::uniqueN(d$protocolo_rotulo)))
}
