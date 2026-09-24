source('tests/helpers_test_funcoes.R');e<-monitora_test_funcoes('R_monitora_campsav_alvo_global.R')$env
old<-monitora_test_funcoes('monitora_campsav_alvo_global_v3.0.2.R')$env
out<-'artifacts/v303/legendas';dir.create(out,recursive=TRUE,showWarnings=FALSE)
grDevices::pdf(file.path(out,'dispositivo_legado.pdf'));anterior<-grDevices::dev.cur()
p<-ggplot2::ggplot(data.frame(x=1:3,y=1:3),ggplot2::aes(x,y))+ggplot2::geom_point()+ggplot2::labs(title='Teste de legendas')
caption<-'▲ aumento; ▼ redução; ≈ estabilidade; △ aumento; ▽ redução; α = 0,05 — interpretação.'
w_old<-w_new<-character()
a<-withCallingHandlers(old$monitora_stat_adicionar_caption_painel(p,caption,11),warning=function(w){w_old<<-c(w_old,conditionMessage(w));invokeRestart('muffleWarning')})
b<-withCallingHandlers(e$monitora_stat_adicionar_caption_painel(p,caption,11),warning=function(w){w_new<<-c(w_new,conditionMessage(w));invokeRestart('muffleWarning')})
if (.Platform$OS.type != 'windows') stopifnot(any(grepl('conversion failure',w_old)))
stopifnot(!length(w_new),identical(grDevices::dev.cur(),anterior))
grDevices::dev.off()
ggplot2::ggsave(file.path(out,'legenda_unicode.png'),b$plot,width=11,height=7,dpi=130,bg="white")
cat('PASS: medição Unicode sem avisos; dispositivo do usuário restaurado; avisos no controle legado=',length(w_old),'\n')
