library(data.table);source('tests/helpers_v304.R');e<-monitora_v304_funcoes('R_monitora_campsav_alvo_global.R')
args<-commandArgs(TRUE);d<-fread(args[1],colClasses='character',na.strings='NA')[1:2];d[2,PROTOCOLO:='PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25'];ch<-e$monitora_correcao_colunas_chave(d);d[,(ch$coleta):=c('historica','moderna')]
# Coleta definições diretas da função real, sem achatar os escopos aninhados.
outer<-new.env(parent=e);outer$dt<-d;outer$chaves<-ch;outer$meta_xls<-NULL;outer$dict<-NULL
b<-as.list(body(e$monitora_correcao_painel))[-1]
select<-function(b,names,envir)for(x in b)if(is.call(x)&&identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]])&&as.character(x[[2]])%in%names)eval(x,envir)
n<-c('MONITORA_TRIAGEM_FORMAS_CONDICIONAIS','MONITORA_TRIAGEM_ALIASES_FORMAS_CONDICIONAIS','monitora_painel_canonizar_forma_habito','monitora_painel_valor','monitora_painel_detalhar_forma_vida_contratual','monitora_painel_expandir_operacao_contratual','server')
select(b,n,outer)
col<-e$monitora_correcao_coluna_forma_vida(d,'nativa')
outer$monitora_painel_info_contrato_atributo<-function(...)list(papel='lista_principal_forma_vida',categoria='nativa',col=col)
outer$formas_validas_por_categoria<-list(nativa=c(samambaia='samambaia'))
outer$monitora_painel_habito_choices_contratuais<-function(...)c(terrestre='terrestre')
stopifnot(!length(outer$monitora_painel_detalhar_forma_vida_contratual(col,'append_token','samambaia',d,1L)$exige_habito),identical(outer$monitora_painel_detalhar_forma_vida_contratual(col,'append_token','samambaia',d,2L)$exige_habito,'samambaia'))
op<-e$monitora_correcao_criar_operacao('TEST','teste','contratual',1L,'linhas_diagnosticas_ocorrencia','historica | moderna',atributo=col,acao='append_token',valor_novo='samambaia',n_esperado=2L,n_alvo=2L,motivo='teste')
z<-outer$monitora_painel_expandir_operacao_contratual(op,d,1:2,col,'append_token','samambaia','terrestre',2L,2L);stopifnot(z$ok)
h<-z$ops[tipo_correcao=='correcao_contratual_xlsform_habito'];stopifnot(nrow(h)==1L,h$n_linhas_esperado=='1')
stopifnot(identical(as.integer(e$monitora_correcao_localizar_linhas(d,h,ch)),2L))
# Executa os helpers originais do servidor em Shiny, com o escopo externo preservado.
sb<-as.list(body(outer$server))[-1];names_server<-c('monitora_painel_linhas_habito','monitora_painel_forma_exige_habito','monitora_painel_usar_lote_coletas')
chosen<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]])&&as.character(x[[2]])%in%names_server,sb)
server<-eval(as.call(list(as.name('function'),formals(outer$server),as.call(c(list(as.name('{')),chosen)))),outer)
shiny::testServer(server,{
 session$setInputs(coleta='historica',escopo_coletas='coleta_individual');stopifnot(!monitora_painel_forma_exige_habito('samambaia'))
 session$setInputs(coleta='moderna');stopifnot(monitora_painel_forma_exige_habito('samambaia'))
 session$setInputs(escopo_coletas='coletas_do_lote',coletas_lote='historica');stopifnot(!monitora_painel_forma_exige_habito('samambaia'))
 session$setInputs(coletas_lote=c('historica','moderna'));stopifnot(monitora_painel_forma_exige_habito('samambaia'))
})
cat('PASS: closures externas e Shiny preservam contexto histórico, moderno e misto; hábito extra só atinge UUID moderno.\n')
