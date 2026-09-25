suppressPackageStartupMessages(library(data.table))
source('tests/helpers_v304.R');e<-monitora_v304_funcoes('R_monitora_campsav_alvo_global.R')
args<-commandArgs(TRUE);stopifnot(length(args)>=1L)
dt<-fread(args[1],colClasses='character',na.strings='NA');original<-copy(dt)
e$MONITORA_CORRECOES_DIR<-tempdir();e$MONITORA_LOG_DIR<-tempdir();e$MONITORA_EXEC_ID<-'v304_test'
ch<-e$monitora_correcao_colunas_chave(dt)
make<-function(d,rr,hab=NA_character_,id='TEST') {
 c<-e$monitora_correcao_colunas_chave(d)
 op<-e$monitora_correcao_criar_operacao(id,'Homologação','movimento_forma_vida_atomico',1L,'uuid_registro',d[[c$coleta]][rr],
  uuid_registro=d[[c$uuid_registro]][rr],ponto_amostral=d[[c$ponto_amostral]][rr],atributo='__mover_forma_vida__',acao='mover_forma_vida',
  valor_original=d[[e$monitora_correcao_coluna_forma_vida(d,'exotica')]][rr],valor_novo='samambaia',n_esperado=1L,n_alvo=1L,
  motivo='Teste multiversão sem modificar a fonte',token_pai='samambaia',categoria_origem='exotica',categoria_destino='nativa',token_removido='samambaia',habito_escolhido=hab)
 e$monitora_correcao_anexar_contexto_operacao(op,d,rr,c)
}
for(rr in seq_len(nrow(dt))) {
 z<-e$monitora_correcao_aplicar_movimento_forma_vida_atomico(dt,make(dt,rr,id=paste0('TEST',rr)),ch)
 if(z$falha){print(z$audit);stop('falha linha ',rr)}
 dt<-z$dt
}
cn<-e$monitora_correcao_coluna_forma_vida(dt,'nativa');ce<-e$monitora_correcao_coluna_forma_vida(dt,'exotica')
stopifnot(all(e$monitora_correcao_token_presente_vec(dt[[cn]],'samambaia')),!any(e$monitora_correcao_token_presente_vec(dt[[ce]],'samambaia')))
allowed<-c(cn,ce,ch$tipo_forma_vida)
for(n in setdiff(names(dt),allowed))stopifnot(identical(dt[[n]],original[[n]]))
cat('PASS42: samambaias movidas, outros campos preservados integralmente.\n')
# Protocolo moderno: ausência de hábito bloqueia sem mutação.
modern<-copy(original[1]);modern[,PROTOCOLO:='PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25']
snapshot<-copy(modern);z<-e$monitora_correcao_aplicar_movimento_forma_vida_atomico(modern,make(modern,1L))
stopifnot(z$falha,identical(as.list(modern),as.list(snapshot)))
z<-e$monitora_correcao_aplicar_movimento_forma_vida_atomico(copy(modern),make(modern,1L,'terrestre'))
if(z$falha)print(z$audit);stopifnot(!z$falha)
cat('PASS: protocolo2025 bloqueia ausência e aceita hábito explícito.\n')
# Texto histórico não é tratado como hábito mesmo que a grafia seja terrestre.
legacy<-copy(original[1]);src<-'amostragem/registro/forma_vida_exotica_samambaia';dst<-'amostragem/registro/forma_vida_nativa_samambaia_sp'
legacy[,(src):='terrestre'];z<-e$monitora_correcao_aplicar_movimento_forma_vida_atomico(legacy,make(legacy,1L))
if(z$falha)print(z$audit)
stopifnot(!z$falha,identical(z$dt[[dst]],'terrestre'),is.na(z$dt[[src]]))
cat('PASS: espécie histórica preservada no campo de espécie.\n')
make_lote<-function(d,hab=NA_character_) {
 op<-make(d,1L,hab,'MVLOTETEST')
 op[,`:=`(tipo_correcao='movimento_forma_vida_lote_atomico',acao='mover_forma_vida_lote',atributo_coluna_registros_corrig='__mover_forma_vida_lote__',escopo_aplicacao='linhas_diagnosticas_ocorrencia',n_linhas_esperado=as.character(nrow(d)),n_linhas_alvo=as.character(nrow(d)),forma_valida_escolhida='samambaia',token_removido='samambaia',valor_original_esperado='samambaia')]
 op[,`:=`(uuid_registro=e$monitora_correcao_colapsar_lista_serializada(d[[ch$uuid_registro]]),linhas_alvo_serializadas=e$monitora_correcao_colapsar_lista_serializada(as.character(seq_len(nrow(d)))),ponto_amostral=NA_character_,ponto_metro=NA_character_)]
 e$monitora_correcao_anexar_contexto_operacao(op,d,seq_len(nrow(d)),e$monitora_correcao_colunas_chave(d))
}
mix<-copy(original[1:2]);mix[2,PROTOCOLO:='PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25']
op<-make_lote(mix,'terrestre');z<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(copy(mix),op)
if(z$falha)print(z$audit);stopifnot(!z$falha)
h<-e$monitora_correcao_resolver_coluna_habito(z$dt,'nativa','samambaia')
stopifnot(is.na(z$dt[[h]][1]),z$dt[[h]][2]=='terrestre',all(e$monitora_correcao_token_presente_vec(z$dt[[cn]],'samambaia')))
cat('PASS: lote misto aplica hábito exclusivamente na linha2025.\n')
# O conflito na última linha desfaz também a primeira.
conf<-copy(original[1:2]);conf[2,(src):='nome origem'];conf[2,(dst):='nome destino'];before<-copy(conf)
z<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(conf,make_lote(conf))
stopifnot(z$falha,identical(as.list(z$dt),as.list(before)),identical(as.list(conf),as.list(before)),!nrow(z$afetacoes))
cat('PASS: conflito final cancela lote integral, fonte imutável.\n')
# Reordenar linhas preserva identidade dos alvos; repetir não modifica dados.
legacy<-copy(original[1:2]);op<-make_lote(legacy)
z<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(copy(legacy[2:1]),op);stopifnot(!z$falha)
z2<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(copy(z$dt),op)
if(z2$falha)print(z2$audit);stopifnot(!z2$falha,identical(as.list(z$dt),as.list(z2$dt)))
cat('PASS: ordem independente e repetição sem nova alteração.\n')
# Desconhecida: os dois passos posteriores à mutação são obrigatórios e transacionais.
unk<-copy(original[1:2]);unk[,(ce):='desconhecida'];op<-make_lote(unk);op[,`:=`(token_pai='desconhecida',token_removido='desconhecida',valor_original_esperado='desconhecida')]
for(fn in c('monitora_correcao_sanitizar_dependentes_desconhecida','monitora_correcao_recalcular_tipo_forma_vida')) {
 saved<-e[[fn]];e[[fn]]<-function(...)stop('FALHA_INJETADA')
 z<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(unk,op)
 e[[fn]]<-saved
 stopifnot(z$falha,identical(as.list(z$dt),as.list(unk)),!nrow(z$afetacoes))
}
z<-e$monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(copy(unk),op);stopifnot(!z$falha,all(e$monitora_correcao_token_presente_vec(z$dt[[cn]],'samambaia')))
cat('PASS: desconhecida cancela integralmente se sanitização ou Encostam falham.\n')
# Intralista preserva espécie do destino já existente.
intra<-copy(original[1]);intra[,(ce):='samambaia | graminoide'];intra[,(src):='espécie preservada']
op<-make(intra,1L);op[,`:=`(categoria_destino='exotica',token_removido='samambaia | graminoide',token_pai='graminoide')]
z<-e$monitora_correcao_aplicar_movimento_forma_vida_atomico(copy(intra),op)
if(z$falha)print(z$audit);stopifnot(!z$falha,z$dt[[src]]=='espécie preservada')
cat('PASS: substituição intralista conserva descritor da forma destino.\n')
