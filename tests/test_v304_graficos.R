# Rscript tests/test_v304_graficos.R [script] [prop_rel_material_botanico.csv] [pasta_evidencia]
suppressPackageStartupMessages({library(data.table);library(ggplot2)})
source('tests/helpers_v304.R')
args<-commandArgs(TRUE)
script<-if(length(args))args[1L] else 'R_monitora_campsav_alvo_global.R'
e<-monitora_v304_funcoes(script)
# Carrega apenas constantes gráficas; não executa o pipeline nem estatísticas.
constants<-function(x) {
 if(!is.call(x))return()
 if(identical(x[[1L]],as.name('<-')) && is.symbol(x[[2L]]) &&
    grepl('^MONITORA_(FONTE_|LINEHEIGHT_|LIMIAR_ROTULO_|N_CATEGORIAS_|LAYOUT_)',as.character(x[[2L]])))try(eval(x,e),silent=TRUE)
 if(length(x)>1L)for(i in 2:length(x))constants(x[[i]])
}
constants(parse(script,keep.source=FALSE)[[1L]])
# Exercita a sequência real: eixo externo primeiro, composição depois.
d<-CJ(ANO=as.character(2022:2026),form_veg=c('campestre','savanica'))
d[,`:=`(veg_cover=50,n_UA=37L,categoria_label='Nativas')]
snapshot<-copy(d)
preparar_comp_original<-e$monitora_stat_preparar_dados_plot_composicao
e$monitora_stat_preparar_dados_plot_composicao<-function(d,...)unique(as.data.table(d)[,.(ANO,form_veg,simbolo_mudanca_composicao='◆')])
p<-ggplot(d,aes(veg_cover,factor(ANO)))+geom_col()+facet_wrap(~form_veg)+scale_x_continuous()
p<-e$monitora_stat_adicionar_rotulos_eixo_cobertura(p,'plot_teste_veg_cover_com_rotulo')
p<-e$monitora_stat_adicionar_simbolo_composicao_borda(p,'categorias_gerais','cobertura')
b<-ggplot_build(p)
labels<-b$data[[2L]]
stopifnot(nrow(labels)==10L,all(is.finite(labels$x)),all(labels$x<0),all(grepl('202[2-6]',labels$label)),identical(d,snapshot))
avisos<-character()
invisible(withCallingHandlers(ggplotGrob(p),warning=function(w){avisos<<-c(avisos,conditionMessage(w))}))
stopifnot(!any(grepl('Removed.*geom_text',avisos)))
cat('PASS: dez rótulos de ANO/esforço sobrevivem à camada de composição.\n')
if(length(args)>=2L) {
 real<-fread(args[2L]);real<-real[tolower(form_veg)=='campestre'];snapshot<-copy(real)
 # O CSV auxiliar acrescenta ordem de catálogo após a plotagem. Reconstituir
 # a ordem gráfica original pelo mesmo fallback de categoria_label.
 entrada_plot<-copy(real);entrada_plot[,ordem_categoria:=NULL]
 r<-e$monitora_plot_preparar_rotulos_proporcao_obrigatorios(entrada_plot)
 ext<-r[as.character(ANO)=='2026' & rotulo_prop_externo!='']
 stopifnot(nrow(ext)==2L,all(is.finite(ext$y_alvo_rotulo)),diff(sort(ext$y_alvo_rotulo))>=0.459,
   all(abs(ext$y_alvo_rotulo-ext$y_base_rotulo)<=0.231),identical(real,snapshot))
 scientific<-c('ANO','form_veg','categoria','n','prop')
 a<-copy(real[,..scientific]);z<-copy(r[,..scientific]);setorderv(a,c('ANO','form_veg','categoria'));setorderv(z,c('ANO','form_veg','categoria'))
 stopifnot(identical(a,z))
 if(length(args)>=3L) {
  dir.create(args[3L],recursive=TRUE,showWarnings=FALSE)
  q<-ggplot(r)+e$monitora_plot_camada_barras_proporcao_obrigatorios(r)+
    e$monitora_plot_scale_x_proporcao_obrigatorios(r)+e$monitora_plot_camadas_rotulos_proporcao_obrigatorios(r)+
    scale_fill_manual(values=c('Fragmentos botânicos'='#b88347','Material inundável'='#507fab','Serrapilheira'='#8b702f'))+
    labs(title='Material botânico — FNB campestre',subtitle='Dados existentes; somente separação dos rótulos externos corrigida',x='Proporção relativa',y='Ano',fill='Material botânico')+theme_minimal(base_size=13)
  ggsave(file.path(args[3L],'fig014_rotulos_corrigidos.png'),q,width=11,height=8.8,dpi=160)
  ggsave(file.path(args[3L],'eixo_cobertura_corrigido.png'),p,width=11,height=7,dpi=160)
  fwrite(ext[,.(ANO,categoria,n,prop,y_base_rotulo,y_alvo_rotulo,x_alvo_rotulo)],file.path(args[3L],'posicoes_rotulos_2026.csv'))
 }
 cat('PASS: rótulos reais FNB2026 separados; contagens, proporções e fonte preservadas.\n')
 # Limite superior: símbolos só existem para parte dos anos; sua extensão
 # não pode censurar os rótulos/ICs dos demais anos. Dados científicos salvos.
 e$monitora_stat_preparar_dados_plot_composicao<-preparar_comp_original
 base<-dirname(args[2L])
 e$MONITORA_STAT_COMPOSICAO_GERAL<-fread(file.path(base,'estatisticas_composicao_geral_ano_a_ano.csv'))
 e$MONITORA_STAT_COMPOSICAO_GERAL_PAINEL_ANO_INICIAL<-fread(file.path(base,'estatisticas_composicao_geral_ano_a_ano_paineis_ano_inicial.csv'))
 for(spec in list(c('cob_veg_material_botanico.csv','material_botanico',NA),c('cob_veg_categ.csv','categorias_gerais','2023'),c('cob_veg_form_vida_nat.csv','formas_vida_nativas','2023'))) {
  dados<-fread(file.path(base,spec[1L]));dados[,ANO:=as.character(ANO)]
  if(!is.na(spec[3L])){dados<-dados[as.integer(ANO)>=as.integer(spec[3L])];dados[,ano_inicial_painel:=as.integer(spec[3L])]}
  before<-copy(dados)
  graf<-ggplot(dados,aes(veg_cover,factor(ANO),fill=categoria_label))+geom_col(position=position_dodge(.7))+
    e$monitora_plot_camadas_rotulos_cobertura_externos(FALSE)+facet_wrap(~form_veg)+scale_x_continuous()
  graf<-e$monitora_stat_adicionar_rotulos_eixo_cobertura(graf,'plot_teste_veg_cover_com_rotulo')
  graf<-e$monitora_stat_adicionar_simbolo_composicao_borda(graf,spec[2L],'cobertura')
  built<-ggplot_build(graf)
  stopifnot(all(is.finite(built$data[[3L]]$x)),all(is.finite(built$data[[3L]]$y)),identical(dados,before))
  if(identical(spec[2L],'material_botanico')) {
   stopifnot(any(built$data[[3L]]$label=='91.7%'))
   if(length(args)>=3L)ggsave(file.path(args[3L],'cobertura_material_rotulo_917_preservado.png'),graf,width=11,height=7,dpi=160)
  }
 }
 cat('PASS: rótulo real91,7% e demais rótulos/ICs preservados além da extensão dos símbolos; sem recalcular estatísticas.\n')
}
