# Teste mecanico independente: sem Source do pipeline e sem dados institucionais no Git.
v308q_ctx <- function(out) {
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  e<-new.env(parent=emptyenv());e$out<-normalizePath(out,winslash="/",mustWork=TRUE)
  e$checks<-0L;e$cases<-list();e
}
v308q_check <- function(ok,message,ctx) {
  ctx$checks<-ctx$checks+1L
  if(!isTRUE(ok))stop(message,call.=FALSE)
  invisible(TRUE)
}
v308q_load <- function(script=NULL) {
  for(n in c("data.table","ggplot2","grid","digest","jsonlite"))
    if(!requireNamespace(n,quietly=TRUE))stop("Pacote necessario ao teste: ",n,call.=FALSE)
  if(!is.null(script)&&nzchar(script)) {
    source("tools/v308/carregar.R",local=TRUE)
    e<-monitora_v308_carregar(script)
  } else {
    e<-new.env(parent=globalenv());source("tools/v308/evidencia.R",local=e,encoding="UTF-8")
  }
  stopifnot(exists("monitora_v305_figura_evidencia",envir=e,inherits=FALSE))
  e
}
v308q_hash <- function(p) digest::digest(p,algo="sha256",serialize=FALSE,file=TRUE)
v308q_snapshot <- function(x) serialize(x,NULL,version=2)
v308q_canon <- function(x) gsub("[\\p{Z}\\s]+","",enc2utf8(as.character(x)),perl=TRUE)
v308q_palette <- function() c("Aumento"="#377D51","Redução"="#B25142",
  "Estabilidade/equivalência"="#C2B26A","Mudança na composição"="#7D629D",
  "Estabilidade composicional"="#7B9B71","Inconclusivo"="#AAAAAA","Pares insuficientes"="#DDDDDD")
v308q_cfg <- function(nome,padrao) as.numeric(padrao)
v308q_png <- function(path) {
  raw<-readBin(path,"raw",24L)
  stopifnot(length(raw)==24L,identical(as.integer(raw[1:8]),c(137L,80L,78L,71L,13L,10L,26L,10L)))
  n<-function(z)sum(as.numeric(as.integer(z))*256^(3:0))
  c(width=n(raw[17:20]),height=n(raw[21:24]))
}
v308q_capture <- function(fun) {
  cap<-new.env(parent=emptyenv());cap$items<-list()
  ns<-asNamespace("ggplot2")
  original<-get("ggsave",ns)
  stopifnot(!inherits(original,"functionWithTrace"))
  proxy<-function(...) {
    dots<-list(...)
    value<-do.call(original,dots)
    filename<-if(!is.null(dots$filename))dots$filename else dots[[1L]]
    if(!is.null(dots$path))filename<-file.path(dots$path,filename)
    plot<-if(!is.null(dots$plot))dots$plot else dots[[2L]]
    cap$items[[length(cap$items)+1L]]<-list(filename=filename,plot=plot,
      width=dots$width,height=dots$height,units=if(is.null(dots$units))"in"else dots$units,
      scale=if(is.null(dots$scale))1 else dots$scale,dpi=if(is.null(dots$dpi))300 else dots$dpi,
      generated_sha256=v308q_hash(filename))
    value
  }
  unlockBinding("ggsave",ns);assign("ggsave",proxy,envir=ns);lockBinding("ggsave",ns)
  on.exit({unlockBinding("ggsave",ns);assign("ggsave",original,envir=ns);lockBinding("ggsave",ns)},add=TRUE)
  answer<-withVisible(fun())
  list(value=answer$value,visible=answer$visible,plots=cap$items)
}
v308q_text_fonts <- function(g,font=12) {
  if(!is.null(g$gp$fontsize))font<-g$gp$fontsize
  values<-numeric()
  if(inherits(g,"text")&&length(g$label)&&any(nzchar(as.character(g$label))))values<-as.numeric(font)
  children<-c(if(!is.null(g$grobs))g$grobs else list(),if(!is.null(g$children))as.list(g$children) else list())
  for(child in children)values<-c(values,v308q_text_fonts(child,font))
  values
}
v308q_geometry <- function(saved,source,page_row,ctx) {
  # Dispositivo igual ao contratado pelo renderer; PNG final tambem e medido pelo IHDR.
  w<-as.numeric(saved$width);h<-as.numeric(saved$height)
  v308q_check(identical(saved$units,"in")&&isTRUE(all.equal(saved$scale,1)),"Unidades/escala de saida inesperadas",ctx)
  tmp<-tempfile("medida_independente_",fileext=".png")
  grDevices::png(tmp,width=w,height=h,units="in",res=300,type="cairo")
  on.exit({grDevices::dev.off();unlink(tmp)},add=TRUE)
  g<-ggplot2::ggplotGrob(saved$plot);b<-ggplot2::ggplot_build(saved$plot)
  grid::grid.newpage();grid::grid.draw(g);grid::grid.force()
  fs<-v308q_text_fonts(g)
  v308q_check(length(fs)>0L&&all(is.finite(fs))&&min(fs)>=7-1e-7,"Fonte fisica de um elemento menor que 7pt",ctx)
  layout<-b$layout$layout
  fixed<-c("PANEL","ROW","COL","SCALE_X","SCALE_Y","COORD")
  vars<-setdiff(names(layout),fixed)
  form_key<-vars[vapply(vars,function(n)all(v308q_canon(layout[[n]])%in%v308q_canon(unique(source[["Formação"]]))),logical(1))]
  v308q_check(length(form_key)==1L,"Facet nao reconciliado por chave de formacao",ctx)
  tile<-Filter(function(x)all(c("xmin","xmax","ymin","ymax","PANEL")%in%names(x)),b$data)
  texts<-Filter(function(x)all(c("label","x","y","PANEL","size")%in%names(x)),b$data)
  v308q_check(length(tile)==1L&&length(texts)==1L,"Camadas de celulas/rotulos ambiguas",ctx)
  tile<-tile[[1L]];texts<-texts[[1L]]
  v308q_check(nrow(tile)==nrow(source)&&nrow(texts)==nrow(source),"Quantidade grafica de celulas diverge das fontes",ctx)
  vps<-grid::grid.ls(grobs=FALSE,viewports=TRUE,recursive=TRUE,print=FALSE)$name
  seen<-integer();measurements<-list()
  for(j in seq_len(nrow(layout))) {
    panel<-as.integer(layout$PANEL[j]);form<-as.character(layout[[form_key]][j])
    name<-paste0("panel-",layout$COL[j],"-",layout$ROW[j])
    if(!name%in%g$layout$name&&"panel"%in%g$layout$name)name<-"panel"
    hits<-unique(vps[startsWith(vps,paste0(name,"."))])
    v308q_check(length(hits)==1L,"Viewport real do painel nao encontrado",ctx)
    grid::upViewport(0);grid::seekViewport(hits[[1L]])
    pw<-grid::convertWidth(grid::unit(1,"npc"),"mm",valueOnly=TRUE)
    ph<-grid::convertHeight(grid::unit(1,"npc"),"mm",valueOnly=TRUE)
    pp<-b$layout$panel_params[[panel]]
    xr<-pp$x.range;if(is.null(xr))xr<-pp$x$continuous_range
    yr<-pp$y.range;if(is.null(yr))yr<-pp$y$continuous_range
    xb<-as.character(pp$x$breaks);yb<-as.character(pp$y$breaks)
    positions<-which(as.integer(tile$PANEL)==panel)
    for(i in positions) {
      t<-texts[as.integer(texts$PANEL)==panel & abs(texts$x-tile$x[i])<1e-8 & abs(texts$y-tile$y[i])<1e-8,,drop=FALSE]
      v308q_check(nrow(t)==1L,"Rotulo nao e unico no centro da celula",ctx)
      period<-xb[round(tile$x[i])];indicator<-yb[round(tile$y[i])]
      idx<-which(v308q_canon(source[["Formação"]])==v308q_canon(form)&
        v308q_canon(source$Periodo)==v308q_canon(period)&v308q_canon(source$Indicador)==v308q_canon(indicator))
      v308q_check(length(idx)==1L,"Celula visual sem reconciliacao unica Formacao/Periodo/Indicador",ctx)
      seen<-c(seen,idx)
      v308q_check(identical(v308q_canon(t$label),v308q_canon(source$rotulo_celula[idx])),"Rotulo cientifico alterado no grafico",ctx)
      cell_w<-(tile$xmax[i]-tile$xmin[i])/diff(xr)*pw
      cell_h<-(tile$ymax[i]-tile$ymin[i])/diff(yr)*ph
      font<-t$size*ggplot2::.pt
      family<-if("family"%in%names(t))as.character(t$family)else"sans"
      face<-if("fontface"%in%names(t))t$fontface else 1
      lineheight<-if("lineheight"%in%names(t))t$lineheight else 1.2
      tg<-grid::textGrob(as.character(t$label),gp=grid::gpar(fontsize=font,fontfamily=family,fontface=face,lineheight=lineheight))
      tw<-grid::convertWidth(grid::grobWidth(tg),"mm",valueOnly=TRUE)
      th<-grid::convertHeight(grid::grobHeight(tg),"mm",valueOnly=TRUE)
      v308q_check(font>=7-1e-7&&tw<=cell_w+1e-4&&th<=cell_h+1e-4,"Texto fisico ultrapassa sua celula real",ctx)
      measurements[[length(measurements)+1L]]<-c(cell_w=cell_w,cell_h=cell_h,text_w=tw,text_h=th,font_pt=font)
    }
  }
  v308q_check(identical(sort(seen),seq_len(nrow(source))),"Celulas perdidas ou duplicadas na geometria",ctx)
  mm<-do.call(rbind,measurements)
  list(min_font_pt=min(fs),cell_min_mm=apply(mm[,c("cell_w","cell_h"),drop=FALSE],2,min),text_max_mm=apply(mm[,c("text_w","text_h"),drop=FALSE],2,max),cells_measured=length(seen))
}
v308q_run <- function(e,data,out,name,palette=v308q_palette(),cfg=v308q_cfg,filename="evidencia.png") {
  dir<-file.path(out$out,name);stopifnot(!dir.exists(dir));dir.create(dir)
  arquivo<-file.path(dir,filename);before<-v308q_snapshot(data)
  cap<-v308q_capture(function()e$monitora_v305_figura_evidencia(data,"Evidência temporal — teste de preservação",arquivo,palette,cfg))
  ans<-cap$value;pages<-attr(ans,"paginas");cells<-attr(ans,"celulas")
  check<-function(x,msg)v308q_check(x,paste(name,msg),out)
  check(identical(before,v308q_snapshot(data)),"modificou a tabela de entrada")
  check(!cap$visible&&is.character(ans),"contrato de retorno invisivel")
  check(is.data.frame(pages)&&is.data.frame(cells),"metadados ausentes")
  check(all(c("arquivo_canonico","arquivo","pagina","n_paginas","n_resultados","sha256","fonte_pt","largura_pol","altura_pol","fit")%in%names(pages)),"schema de paginas incompleto")
  check(all(c(names(data),"input_row","cell_id","pagina","arquivo")%in%names(cells)),"campos fonte nao preservados no mapa")
  check(nrow(cells)==nrow(data)&&length(unique(cells$cell_id))==nrow(data),"reconciliacao/IDs divergentes")
  check(identical(sort(as.integer(cells$input_row)),seq_len(nrow(data))),"cada linha fonte deve aparecer uma vez")
  ordered<-cells[order(cells$input_row),,drop=FALSE]
  for(col in names(data)) {
    a<-data[[col]];z<-ordered[[col]]
    same<-if(is.factor(a)||is.factor(z))identical(as.character(a),as.character(z))else identical(unname(a),unname(z))
    check(same,paste("alterou campo fonte",col))
  }
  check(length(ans)==nrow(pages)&&length(ans)>0L&&all(file.exists(ans)),"PNG ausente")
  check(identical(basename(ans[[1L]]),filename),"primeiro arquivo nao preserva nome legado")
  check(all(nchar(basename(ans),type="chars")<=64L),"nome PNG maior que 64 caracteres")
  check(identical(as.integer(pages$pagina),seq_along(ans))&&all(pages$n_paginas==length(ans)),"numeracao de paginas")
  check(all(pages$fonte_pt>=7)&all(pages$altura_pol<=8+1e-8)&all(pages$largura_pol<=6.65+1e-8)&all(pages$fit),"pagina declarada fora do contrato")
  measures<-list()
  for(i in seq_along(ans)) {
    check(v308q_hash(ans[[i]])==pages$sha256[i],"hash de PNG incorreto")
    px<-v308q_png(ans[[i]])
    check(abs(px[[1L]]/300-pages$largura_pol[i])<=1/300+1e-8&&abs(px[[2L]]/300-pages$altura_pol[i])<=1/300+1e-8,"dimensoes PNG fisicas divergem")
    which_plot<-which(vapply(cap$plots,function(z)identical(z$generated_sha256,v308q_hash(ans[[i]])),logical(1)))
    check(length(which_plot)==1L,"plot final nao capturado uma unica vez")
    s<-cells[cells$pagina==i,,drop=FALSE]
    check(nrow(s)==pages$n_resultados[i],"n_resultados por pagina")
    measures[[i]]<-v308q_geometry(cap$plots[[which_plot]],s,pages[i,,drop=FALSE],out)
  }
  record<-list(name=name,source_rows=nrow(data),pages=length(ans),files=basename(ans),measurements=measures,input_unchanged=TRUE,source_fields_preserved=names(data),tested_code=out$tested_code)
  jsonlite::write_json(record,file.path(dir,"PROVA_CASO.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
  cat("CASE_PASS ",name," rows=",nrow(data)," pages=",length(ans),"\n",sep="");flush.console()
  out$cases[[length(out$cases)+1L]]<-record
  list(paths=ans,pages=pages,cells=cells,record=record,dir=dir)
}
v308q_data <- function(periods=8L,indicators=c(2L,15L),long=FALSE,ne=FALSE,composition=FALSE) {
  forms<-if(long)c("Formação campestre savânica com identificação extensa para a leitura institucional","Formação florestal savânica com identificação alternativa e extensa para a leitura")else c("Formação A","Formação B")
  period_names<-paste0(2000+seq_len(periods),"-",2001+seq_len(periods))
  blocks<-lapply(seq_along(indicators),function(i){
    x<-expand.grid(ind=seq_len(indicators[i]),per=seq_len(periods),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
    data.frame(tipo_resultado=if(composition)"composicao"else"categoria",grupo_grafico="grupo_teste",tipo_metrica="proporcao_relativa",
      form_veg=paste0("form_",i),categoria=paste0("cat_",x$ind),ano_1=2000L+x$per,ano_2=2001L+x$per,
      estimativa=if(ne)NA_real_ else x$ind/10,ci95_lower=if(ne)NA_real_ else -x$ind/10,ci95_upper=if(ne)NA_real_ else x$ind/5,
      q_ajustado=if(ne)NA_real_ else .05,classe_periodo=if(ne)"pares_insuficientes"else"inconclusivo",
      classe_linha_base=if(ne)"pares_insuficientes"else"inconclusivo",proveniencia=paste0("origem_",i,"_",x$ind,"_",x$per),
      Formação=forms[i],Periodo=factor(period_names[x$per],levels=period_names),
      Indicador=if(long)paste0("Indicador de forma de vida exótica com descrição extensa para identificar o resultado ",x$ind)else paste0("Indicador ",x$ind),
      classe_periodo_label=if(ne)"Pares insuficientes"else if(composition)"Mudança na composição"else"Inconclusivo",
      rotulo_celula=if(ne)"NE\nPares insuficientes"else paste0("Δ ",x$ind,",0 p.p. [−2;4]\nq=0,050\nLB INC"),stringsAsFactors=FALSE)
  })
  data.table::as.data.table(do.call(rbind,blocks))
}
v308q_compare_order <- function(a,b,ctx) {
  ca<-a$cells[order(a$cells$cell_id),,drop=FALSE];cb<-b$cells[order(b$cells$cell_id),,drop=FALSE]
  v308q_check(identical(as.character(ca$cell_id),as.character(cb$cell_id))&&identical(as.integer(ca$pagina),as.integer(cb$pagina)),"Permutacao alterou IDs/paginas semanticas",ctx)
}
v308q_expect_error <- function(e,data,ctx,name) {
  folder<-file.path(ctx$out,name);dir.create(folder);before<-v308q_snapshot(data)
  error<-tryCatch({e$monitora_v305_figura_evidencia(data,"Controle invalido",file.path(folder,"evidencia.png"),v308q_palette(),v308q_cfg);NULL},error=identity)
  v308q_check(inherits(error,"error"),paste(name,"deveria ser recusado"),ctx)
  v308q_check(!length(list.files(folder,recursive=TRUE,all.files=TRUE,no..=TRUE)),paste(name,"gravou arquivo antes de recusar"),ctx)
  v308q_check(identical(before,v308q_snapshot(data)),paste(name,"modificou fonte"),ctx)
}
v308q_transactions <- function(e,ctx) {
  folder<-file.path(ctx$out,"publicacao_atomica");dir.create(folder)
  originals<-file.path(folder,c("a.txt","b.txt","obsoleto.txt"))
  inputs<-file.path(folder,c("novo_a.txt","novo_b.txt"))
  for(i in seq_along(originals))writeLines(paste0("original_",i),originals[i])
  for(i in seq_along(inputs))writeLines(paste0("novo_",i),inputs[i])
  snapshot<-vapply(originals,v308q_hash,character(1));input_snapshot<-vapply(inputs,v308q_hash,character(1))
  pub<-e$monitora_v308_evidencia_publicar
  for(point in 0:2) {
    err<-tryCatch({pub(inputs,originals[1:2],remover=originals[3],falhar_apos=as.integer(point));NULL},error=identity)
    v308q_check(inherits(err,"error"),paste("Falha de publicacao nao propagada",point),ctx)
    v308q_check(identical(snapshot,vapply(originals,v308q_hash,character(1))),paste("Rollback nao restaurou todos bytes",point),ctx)
    v308q_check(identical(input_snapshot,vapply(inputs,v308q_hash,character(1))),"Publicacao alterou fontes",ctx)
  }
  env<-environment(pub);had<-exists("file.copy",env,inherits=FALSE)
  previous<-if(had)get("file.copy",env,inherits=FALSE)else NULL
  assign("file.copy",function(from,to,...){if(any(grepl("ev_backup_",from,fixed=TRUE)))stop("Restore injetado");base::file.copy(from,to,...)},env)
  err<-tryCatch({pub(inputs,originals[1:2],remover=originals[3],falhar_apos=1L);NULL},error=identity)
  if(had)assign("file.copy",previous,env)else rm("file.copy",envir=env)
  v308q_check(inherits(err,"error")&&grepl("restaura|backup|incomplet",conditionMessage(err),ignore.case=TRUE),"Rollback incompleto nao bloqueou explicitamente",ctx)
  backups<-list.dirs(folder,recursive=FALSE,full.names=TRUE)
  inventories<-file.path(backups,"inventario.csv");inventories<-inventories[file.exists(inventories)]
  v308q_check(length(inventories)==1L,"Rollback incompleto perdeu backup/inventario",ctx)
  invtext<-paste(readLines(inventories[1L],warn=FALSE),collapse="\n")
  v308q_check(all(vapply(unname(snapshot),function(h)grepl(h,invtext,fixed=TRUE),logical(1))),"Inventario de backup nao preserva SHA das fontes",ctx)
  ctx$cases[[length(ctx$cases)+1L]]<-list(name="publicacao_rollback",failure_points=0:2,rollback_incomplete_backup_preserved=TRUE)
}
v308q_obsolete <- function(e,ctx) {
  folder<-file.path(ctx$out,"familia_obsoleta");dir.create(folder)
  path<-file.path(folder,"evidencia.png")
  first<-e$monitora_v305_figura_evidencia(v308q_data(10L,c(2L,15L)),"Familia anterior",path,v308q_palette(),v308q_cfg)
  v308q_check(length(first)>1L,"Controle de obsoletas nao produziu continuacoes",ctx)
  unrelated<-file.path(folder,"nao_pertence_a_familia.png");writeLines("PRESERVAR",unrelated);unrelated_hash<-v308q_hash(unrelated)
  small<-v308q_data(1L,1L);last<-e$monitora_v305_figura_evidencia(small,"Familia nova",path,v308q_palette(),v308q_cfg)
  v308q_check(length(last)==1L&&identical(last[[1L]],path),"Familia pequena nao preserva primeiro nome",ctx)
  v308q_check(!any(file.exists(setdiff(first,last))),"Continuacao obsoleta permaneceu apos geracao nova",ctx)
  v308q_check(v308q_hash(unrelated)==unrelated_hash,"Limpeza removeu arquivo de outra familia",ctx)
  empty<-small[0L];before<-v308q_snapshot(empty)
  returned<-e$monitora_v305_figura_evidencia(empty,"Sem resultados",path,v308q_palette(),v308q_cfg)
  v308q_check(is.character(returned)&&!length(returned)&&!nrow(attr(returned,"paginas"))&&!nrow(attr(returned,"celulas")),"Vazio atestou arquivo/metadata anterior",ctx)
  v308q_check(identical(before,v308q_snapshot(empty)),"Vazio alterou fonte",ctx)
  ctx$cases[[length(ctx$cases)+1L]]<-list(name="obsoletas_e_vazio",old_pages=length(first),new_pages=1L,stale_not_attested=TRUE)
}
v308q_main <- function(args=commandArgs(TRUE)) {
  script<-if(length(args))args[[1L]]else NULL
  out<-if(length(args)>1L)args[[2L]]else tempfile("v308_testes_")
  ctx<-v308q_ctx(out);e<-v308q_load(script);start<-proc.time()[["elapsed"]]
  tested<-if(!is.null(script)&&nzchar(script))script else "tools/v308/evidencia.R"
  ctx$tested_code<-list(path=normalizePath(tested,winslash="/",mustWork=TRUE),sha256=v308q_hash(tested))
  for(p in c(7L,8L,10L)) {
    v308q_run(e,v308q_data(p,3L),ctx,paste0("um_form_",p))
    x<-v308q_data(p,c(2L,15L));a<-v308q_run(e,x,ctx,paste0("desiguais_",p))
    b<-v308q_run(e,x[rev(seq_len(nrow(x)))],ctx,paste0("desiguais_invertidos_",p))
    v308q_compare_order(a,b,ctx)
  }
  x<-v308q_data(8L,c(3L,5L),long=TRUE)
  a<-v308q_run(e,x,ctx,"nomes_longos",filename=paste0(strrep("a",59),".png"))
  b<-v308q_run(e,x[order((seq_len(nrow(x))*37L)%%nrow(x))],ctx,"nomes_longos_permutados",filename=paste0(strrep("a",59),".png"))
  v308q_compare_order(a,b,ctx)
  v308q_run(e,v308q_data(7L,c(2L,4L),ne=TRUE),ctx,"todos_NE")
  v308q_run(e,v308q_data(8L,c(2L,3L),composition=TRUE),ctx,"composicao")
  x<-v308q_data(7L,c(2L,4L));x[["Formação"]]<-factor(x[["Formação"]],levels=rev(unique(x[["Formação"]])))
  a<-v308q_run(e,x,ctx,"formacao_fator")
  b<-v308q_run(e,x[rev(seq_len(nrow(x)))],ctx,"formacao_fator_invertida")
  v308q_compare_order(a,b,ctx)
  x<-v308q_data(7L,1L)
  for(col in c("Formação","Periodo","Indicador"))for(value in c("NA","vazio")) {
    z<-data.table::copy(x);z[[col]]<-as.character(z[[col]]);z[[col]][1L]<-if(value=="NA")NA_character_ else ""
    v308q_expect_error(e,z,ctx,paste0("invalido_",match(col,c("Formação","Periodo","Indicador")),"_",value))
  }
  v308q_expect_error(e,data.table::rbindlist(list(x,x[1L])),ctx,"duplicata_identica")
  z<-data.table::rbindlist(list(x,x[1L]));z$estimativa[nrow(z)]<-999
  v308q_expect_error(e,z,ctx,"duplicata_conflitante")
  z<-data.table::copy(x[1:2]);z[["Formação"]]<-c(NA_character_,"")
  v308q_expect_error(e,z,ctx,"NA_e_vazio_com_mesmo_label")
  z<-data.table::copy(x[1L]);z$rotulo_celula<-paste(rep(strrep("W",120),100L),collapse="\n")
  v308q_expect_error(e,z,ctx,"uma_celula_fisicamente_impossivel")
  empty<-x[0L];ans<-e$monitora_v305_figura_evidencia(empty,"Vazio",file.path(ctx$out,"vazio.png"),v308q_palette(),v308q_cfg)
  v308q_check(is.character(ans)&&!length(ans)&&!nrow(attr(ans,"paginas"))&&!nrow(attr(ans,"celulas")),"Vazio atestou resultado",ctx)
  v308q_transactions(e,ctx)
  v308q_obsolete(e,ctx)
  report<-list(status="PASS",checks=ctx$checks,cases=ctx$cases,tested_code=ctx$tested_code,elapsed_seconds=round(proc.time()[["elapsed"]]-start,3),R=as.character(getRversion()),ggplot2=as.character(packageVersion("ggplot2")),full_pipeline_Source=FALSE)
  jsonlite::write_json(report,file.path(ctx$out,"RESULTADO_TESTES_V308.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
  cat("PASS V308: ",ctx$checks," verificacoes; ",length(ctx$cases)," casos graficos; ",report$elapsed_seconds,"s\n",sep="")
  invisible(report)
}
if(sys.nframe()==0L){
  file_arg<-grep("^--file=",commandArgs(FALSE),value=TRUE)
  if(length(file_arg)==1L)setwd(normalizePath(file.path(dirname(sub("^--file=","",file_arg)),".."),winslash="/",mustWork=TRUE))
  v308q_main()
}
