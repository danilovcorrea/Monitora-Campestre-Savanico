# Uso: Rscript tools/v308/testar_PNB_real.R CANDIDATA FIXTURE_RDS DESTINO_NOVO
# Fixture agregado externo ao Git; nenhum Source do pipeline nem nova estatistica.
args<-commandArgs(TRUE);stopifnot(length(args)==3L)
file_arg<-grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(file_arg)==1L)setwd(normalizePath(file.path(dirname(sub("^--file=","",file_arg)),"..",".."),winslash="/",mustWork=TRUE))
source("tests/test_v308_evidencia_paginacao.R",local=TRUE,encoding="UTF-8")
script<-args[[1L]];fixture<-normalizePath(args[[2L]],winslash="/",mustWork=TRUE)
ctx<-v308q_ctx(args[[3L]]);e<-v308q_load(if(nzchar(script))script else NULL)
tested<-if(nzchar(script))script else "tools/v308/evidencia.R"
ctx$tested_code<-list(path=normalizePath(tested,winslash="/",mustWork=TRUE),sha256=v308q_hash(tested))
before<-v308q_hash(fixture);fx<-readRDS(fixture)
stopifnot(nrow(fx$proporcao$data)==131L,nrow(fx$cobertura$data)==152L)
cfg_env<-new.env(parent=globalenv());cfg_env$config_stat<-data.table::copy(fx$config)
cfg_num<-eval(as.call(c(as.name("function"),list(fx$cfg_formals,fx$cfg_body))),cfg_env)
start<-proc.time()[["elapsed"]]
for(kind in c("proporcao","cobertura")) {
  x<-fx[[kind]]$data
  a<-v308q_run(e,x,ctx,paste0("PNB_",kind),palette=fx[[kind]]$palette,cfg=cfg_num)
  b<-v308q_run(e,x[rev(seq_len(nrow(x)))],ctx,paste0("PNB_",kind,"_invertida"),palette=fx[[kind]]$palette,cfg=cfg_num)
  v308q_compare_order(a,b,ctx)
}
v308q_check(identical(before,v308q_hash(fixture)),"Fixture real foi alterado",ctx)
report<-list(status="PASS_PNB_REAL",fixture_sha256=before,source_rows=c(proporcao=131L,cobertura=152L),checks=ctx$checks,cases=ctx$cases,tested_code=ctx$tested_code,elapsed_seconds=round(proc.time()[["elapsed"]]-start,3),R=as.character(getRversion()),ggplot2=as.character(packageVersion("ggplot2")),full_pipeline_Source=FALSE,scientific_statistics_recomputed=FALSE)
jsonlite::write_json(report,file.path(ctx$out,"RESULTADO_PNB_REAL_V308.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
cat("PASS PNB V308: ",ctx$checks," verificacoes; 131/152 fontes reconciliadas; ",report$elapsed_seconds,"s\n",sep="")
