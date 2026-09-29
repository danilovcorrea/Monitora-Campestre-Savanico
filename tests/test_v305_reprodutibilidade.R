library(data.table);source('tools/v305/carregar.R')
configurar<-function(e) {
  cfg<-list(MONITORA_STAT_MIN_PARES=5L,MONITORA_STAT_BOOT=1999L,MONITORA_STAT_PERM=4999L,MONITORA_STAT_PERM_CHUNK=1000L,MONITORA_STAT_RECURSOS_ADAPTATIVO=FALSE,MONITORA_STAT_REPRODUTIBILIDADE_ATIVA=TRUE,MONITORA_STAT_SEMENTE_BASE=20260625L,MONITORA_STAT_MARGEM_PP=5,MONITORA_STAT_MIN_EFEITO_PP=2,MONITORA_STAT_ALPHA=.05)
  list2env(cfg,e);e$monitora_progresso_loop_configurar<-e$monitora_progresso_loop_avancar<-function(...)invisible(NULL)
  e$MONITORA_STAT_SIMBOLOS_EDITORIAIS<-list(medicao_anterior=c(aumento='▲',reducao='▼',estabilidade_equivalente='≈',inconclusivo='.',pares_insuficientes='—'))
  e
}
e<-configurar(monitora_v305_carregar());old<-configurar(monitora_v305_carregar('monitora_campsav_alvo_global_v3.0.4.R'))
d<-CJ(UA=sprintf('UA%02d',1:24),ANO=2024:2026,categoria=c('a','b'));d[,`:=`(UC='Teste',grupo_grafico='nativas',tipo_metrica='cobertura',form_veg='campestre',categoria_label=categoria)]
d[,valor:=.3+as.numeric(sub('UA','',UA))/100+(ANO-2024)*(.025+.08*sin(as.numeric(sub('UA','',UA))))]
set.seed(1);a<-old$monitora_editorial_testar_pareado_periodo_categoria(copy(d));set.seed(2);b<-old$monitora_editorial_testar_pareado_periodo_categoria(copy(d));stopifnot(!identical(as.data.frame(a),as.data.frame(b)))
set.seed(1);antes<-.Random.seed;a<-e$monitora_editorial_testar_pareado_periodo_categoria(copy(d));stopifnot(identical(antes,.Random.seed))
set.seed(999);invisible(runif(311));ordem<-sample(nrow(d));antes<-.Random.seed;b<-e$monitora_editorial_testar_pareado_periodo_categoria(d[ordem]);stopifnot(identical(antes,.Random.seed),identical(as.data.frame(a),as.data.frame(b)))
# A inclusão de outro indicador não altera os resultados dos indicadores existentes.
extra<-copy(d[categoria=='a']);extra[,`:=`(categoria='c',categoria_label='c',valor=valor*.9)]
c<-e$monitora_editorial_testar_pareado_periodo_categoria(rbind(extra,d));cols<-c('ci95_lower','ci95_upper','p_valor_perm_pareado','media_ano_1','media_ano_2','diferenca')
stopifnot(identical(as.data.frame(a[,..cols]),as.data.frame(c[categoria!='c',..cols])))
# O ajuste FDR pode mudar ao incluir testes; por isso essa coluna não entra nesta última comparação.
stopifnot(all(abs(a$diferenca-(a$media_ano_2-a$media_ano_1))<1e-12))
e$MONITORA_STAT_RECURSOS_ADAPTATIVO<-TRUE
for(perfil in c('critico','economico','equilibrado','amplo')) {
  e$monitora_stat_controlar_recursos_execucao<-function(...)list(modo=perfil)
  stopifnot(e$monitora_stat_definir_iteracoes_efetivas(1999L,'boot')==1999L,e$monitora_stat_definir_iteracoes_efetivas(4999L,'perm')==4999L)
  z<-e$monitora_editorial_testar_pareado_periodo_categoria(copy(d))
  stopifnot(identical(as.data.frame(a),as.data.frame(z)))
}
cat('PASS: defeito v304 reproduzido; v305 independente do RNG anterior, da ordem e de outros indicadores; médias/fórmula e RNG do chamador preservados.\n')
cat('PASS: resultados e quantidades configuradas iguais nos quatro perfis de recursos.\n')
