# Testes de comportamento: o metadado histórico não muda a análise harmonizada.
source('tools/v305/carregar.R');e<-monitora_v305_carregar();library(data.table)
s<-CJ(UA=paste0('U',1:8),ANO=2019:2023,visita=1:2)
s[,`:=`(motivo='elegível',fase=ifelse(visita==1,80,230),protocolo=paste0('historico_',ANO),data_coleta=as.Date(paste0(ANO,'-01-01'))+ifelse(visita==1,80,230),valor_pp=as.numeric(factor(UA))+ANO/100+visita*4+sin(seq_len(.N)))]
a<-e$monitora_v30_modelo_visitas(copy(s));s[,protocolo:='um_rotulo'];b<-e$monitora_v30_modelo_visitas(copy(s));stopifnot(identical(a,b),a$status=='associação descritiva estimada')
s[,`:=`(fase=100,data_coleta=as.Date(paste0(ANO,'-01-01'))+100)];bad<-e$monitora_v30_modelo_visitas(s);stopifnot(bad$status=='NE',grepl('concentrado',bad$motivo))
# A seleção da trajetória e do recorte deve preservar formação e continuidade.
f<-paste(deparse(body(e$monitora_v30_complementos)),collapse='\n')
stopifnot(!grepl('by = .(protocolo_origem',f,fixed=TRUE),!grepl('form_veg, protocolo_origem',f,fixed=TRUE))
f<-paste(deparse(body(e$monitora_fogo_desenho)),collapse='\n')
stopifnot(!grepl('protocolo_origem',f,fixed=TRUE),grepl('101',f),grepl('form_veg',f))
old<-monitora_v305_carregar('monitora_campsav_alvo_global_v3.0.4.R')
stopifnot(identical(body(e$monitora_relatorios_analiticos_material_documentado),body(old$monitora_relatorios_analiticos_material_documentado)))
cat('PASS: calendário invariável ao rótulo, insuficiência real bloqueada, guardas espaciais/formação mantidas, material histórico preservado.\n')
