# Camada editorial: opera apenas em cópias para apresentação.
monitora_v305_motivos <- function(x) {
  z<-trimws(unlist(strsplit(as.character(x),";",fixed=TRUE)));z<-z[!is.na(z)&nzchar(z)]
  paste(unique(z),collapse="; ")
}
monitora_v305_calendario <- function(x) {
  x<-as.data.frame(x);ch<-intersect(c("form_veg","Formação","formacao"),names(x))
  grupo<-intersect(c('indicador_rotulo','indicador'),names(x));if(length(grupo))grupo<-grupo[1L]
  z<-unique(x[,unique(c(ch,grupo,"motivo")),drop=FALSE])
  v<-as.character(z$motivo)
  contexto<-if(length(ch))monitora_relatorio_rotulo_formacao(z[[ch[1]]],TRUE)else rep('',nrow(z))
  if(nrow(z)>1L&&length(unique(v))==1L)return(paste0('Calendário — ',paste(unique(contexto),collapse=' e '),' (todos os indicadores avaliados): ',v[1L],'.'))
  if(length(grupo))contexto<-paste(contexto,z[[grupo]],sep=' / ')
  if(any(nzchar(contexto)))v<-paste0(contexto,": ",v)
  paste0("Calendário — ",paste(unique(v),collapse="; "),".")
}
monitora_v305_siglas <- function() c(
 LB="linha de base acumulada",AUM="aumento",RED="redução",EST="estabilidade/equivalência",INC="inconclusivo",PAR="pares insuficientes",MUD="mudança da composição conjunta",`EST-C`="estabilidade da composição conjunta",H="distância de Hellinger",
 UA="unidade amostral",UAs="unidades amostrais",UC="unidade de conservação",UCs="unidades de conservação",
 ICMBio="Instituto Chico Mendes de Conservação da Biodiversidade",CBC="Centro Nacional de Pesquisa e Conservação em Biodiversidade e Restauração Ecológica",
 LPI="interceptação por pontos em linha (Line-Point Intercept)",
 IC="intervalo de confiança",ICs="intervalos de confiança",`IC95%`="intervalo de confiança de 95%",`IC90%`="intervalo de confiança de 90%",IC95="intervalo de confiança de 95%",IC90="intervalo de confiança de 90%",NE="não estimável sob os critérios desta análise",`NA`="valor não disponível",
 `FDR-BH`="controle da taxa de falsas descobertas pelo procedimento de Benjamini–Hochberg",FDR="taxa de falsas descobertas",BH="procedimento de Benjamini–Hochberg",
 BACI="desenho antes–depois com controle e impacto (Before–After–Control–Impact)",TOST="dois testes unilaterais de equivalência (Two One-Sided Tests)",
 RDA="análise de redundância (Redundancy Analysis)",PCA="análise de componentes principais (Principal Component Analysis)",
 `db-RDA`="análise de redundância baseada em distâncias",PERMANOVA="análise multivariada de variância por permutações",NMDS="escalonamento multidimensional não métrico",GAMM="modelos aditivos generalizados mistos",GLLVM="modelos lineares generalizados de variáveis latentes",SEM="modelagem por equações estruturais",HTZ="teste de Wald conjunto com aproximação de Hotelling T² e ajuste de Zhang",
 VIF="fator de inflação da variância",D30="diâmetro do tronco medido a 30 cm do solo",VPD="déficit de pressão de vapor",UR="umidade relativa",
 PMIF="Plano de Manejo Integrado do Fogo",MIF="manejo integrado do fogo",QP="queima prescrita",
 NASA="Administração Nacional da Aeronáutica e Espaço dos Estados Unidos",POWER="Prediction Of Worldwide Energy Resources, projeto de dados ambientais da agência espacial dos Estados Unidos",
 `MERRA-2`="Modern-Era Retrospective Analysis for Research and Applications, versão 2",`FAO-56`="publicação 56 da Organização das Nações Unidas para a Alimentação e a Agricultura sobre evapotranspiração",
 API="interface de programação de aplicações",AWS="Amazon Web Services",L2A="nível de processamento 2A, reflectância de superfície",CNUC="Cadastro Nacional de Unidades de Conservação",UTC="Tempo Universal Coordenado",LST="horário solar local (Local Solar Time)",JSON="formato de intercâmbio de dados JavaScript Object Notation",PRECTOTCORR="precipitação corrigida da fonte climática",T2M="temperatura do ar a 2 m",T2M_MIN="temperatura mínima do ar a 2 m",T2M_MAX="temperatura máxima do ar a 2 m",RH2M="umidade relativa do ar a 2 m",WS2M="velocidade do vento a 2 m",
 CSV="arquivo de valores separados por delimitador",CSVs="arquivos de valores separados por delimitador",PDF="formato portátil de documento",DOCX="formato de documento editável do Word",XLSX="formato de planilha do Excel",HTML="linguagem de marcação de hipertexto",PNG="formato de imagem Portable Network Graphics",PNGs="imagens no formato Portable Network Graphics",ID="identificador",IDs="identificadores",XLSForm="padrão de formulário definido em planilha eletrônica",
 RMSE="raiz do erro quadrático médio",MAE="erro absoluto médio",CR2="correção de pequena amostra para variância robusta por agrupamentos",GL="graus de liberdade",
 PA="ponto amostral",PAs="pontos amostrais",SIRGAS="Sistema de Referência Geocêntrico para as Américas",EPSG="identificador do sistema de referência espacial",GPS="Sistema de Posicionamento Global",
 SISMONITORA="sistema de informações do Programa Monitora",INPE="Instituto Nacional de Pesquisas Espaciais",IBGE="Instituto Brasileiro de Geografia e Estatística"
)
monitora_v305_catalogo_base <- monitora_relatorios_analiticos_catalogo_tabelas
monitora_relatorios_analiticos_catalogo_tabelas <- function() {
  x<-monitora_v305_catalogo_base()
  for(id in intersect(c("herbaceas-lenhosas","nativas","exoticas","secas-mortas","material","estado-prioritario"),names(x)))x[id]<-paste0(x[id]," na campanha mais recente disponível")
  c(x,stats::setNames(monitora_v305_grupos_glossario(),paste0("glossario-",seq_len(7))))
}
monitora_v305_rotulo_coluna <- function(x) {
  mapa<-c(analise="Análise",motivo="Motivo",Formacao="Formação",Data_inicial="Data inicial",Data_final="Data final",inicio="Ano inicial",fim="Ano final",anos="Anos",fracao_rede_final="Fração da rede final",criterio="Critério",Hipotese="Hipótese",Situacao="Situação",Posicoes="Posições",Poligonos="Polígonos",Extrapolacoes="Extrapolações",Simbolo="Símbolo",form_veg="Formação",indicador="Indicador",metrica="Métrica",status="Situação")
  sel<-x %in% names(mapa);x[sel]<-unname(mapa[x[sel]]);x
}
monitora_v305_kable_base <- monitora_relatorios_analiticos_kable
monitora_relatorios_analiticos_kable <- function(x,id,align=NULL) {
  if(is.data.frame(x)) {x<-as.data.frame(x);names(x)<-monitora_v305_rotulo_coluna(names(x))}
  out<-monitora_v305_kable_base(x,id=id,align=align)
  if(id%in%c('herbaceas-lenhosas','nativas','exoticas','secas-mortas','material','estado-prioritario')&&is.data.frame(x)&&nrow(x)&&'Campanha (ano)'%in%names(x)) {
    ch<-intersect(c('Formação','Campanha (ano)'),names(x));recorte<-unique(x[,ch,drop=FALSE])
    nota<-paste(apply(recorte,1,function(z)paste(z,collapse=': ')),collapse='; ')
    out<-c(out,'',paste0('Recorte da campanha mais recente disponível — ',nota,'.'),'')
  }
  out
}
monitora_v305_editorial <- function(conteudo,dir_relatorio,base_nome) {
  x<-monitora_v305_normalizar_indice(conteudo);out<-character();siglas<-monitora_v305_siglas();usadas<-character();audit<-list();em_codigo<-FALSE;em_refs<-FALSE;exoticas_aviso<-FALSE;tabela_defs<-character();tabela_em_curso<-FALSE;tabela_id<-NULL
  # Só o corpo participa; capa, código, referências e URLs não consomem definições.
  inicio<-which(grepl("^# [^#]",x))[1];if(is.na(inicio))stop("Relatório sem corpo editorial.")
  for(i in seq_along(x)) {
    l<-x[i];nota_elemento<-character()
    if(tabela_em_curso && !startsWith(l,"|")) {
      if(length(tabela_defs))out<-c(out,"",paste0("Nota da [[tabela:",tabela_id,"]]: ",paste(tabela_defs,collapse="; "),"."),"")
      tabela_defs<-character();tabela_em_curso<-FALSE
    }
    if(startsWith(l,"|"))tabela_em_curso<-TRUE
    if(grepl("^```",l))em_codigo<-!em_codigo
    if(grepl("^# Referências",l))em_refs<-TRUE
    ativo<-i>=inicio&&!em_codigo&&!em_refs&&!startsWith(l,"<!--")
    if(ativo) {
      visivel<-gsub("`[^`]*`|https?://[^[:space:]<>)]+|</?[A-Za-z][^>]*>|\\]\\([^)]*\\)"," ",l,perl=TRUE)
      mapa<-data.frame()
      if(startsWith(l,"<figure")&&grepl('src="',l)) {
        arquivo_figura<-sub('^.*src="([^"]+)".*$','\\1',l)
        mapa<-monitora_v305_ler_siglas_figura(arquivo_figura,dir_relatorio)
        if(nrow(mapa))siglas[mapa$sigla]<-mapa$significado
      }
      nomes<-names(siglas)[order(nchar(names(siglas)),decreasing=TRUE)]
      padrao<-paste0('(?<![[:alnum:]_])(?:',paste0('\\Q',nomes,'\\E',collapse='|'),')(?![[:alnum:]_])')
      encontradas<-unique(unlist(regmatches(visivel,gregexpr(padrao,visivel,perl=TRUE))))
      # Abreviações emitidas na legenda dos painéis raster também pertencem ao relatório.
      if(startsWith(l,"<figure")&&grepl('src="[^"]*evidencia_estatistica_',l))encontradas<-unique(c(encontradas,c('LB','AUM','RED','EST','INC','PAR','MUD','EST-C','H')))
      if(nrow(mapa))encontradas<-unique(c(encontradas,mapa$sigla))
      novas<-encontradas[!monitora_v305_familia_sigla(encontradas)%in%monitora_v305_familia_sigla(usadas)]
      novas<-novas[!duplicated(monitora_v305_familia_sigla(novas))]
      # Definir no texto; notas pertencem somente ao elemento tabular/gráfico.
      nota_elemento<-character()
      if(startsWith(l,"<figure") || startsWith(l,"|") || startsWith(l,"Table:")) {
        if(length(novas)) {
          defs<-paste(paste0(unname(siglas[novas])," (",novas,")"),collapse="; ")
          if(startsWith(l,"<figure")) {
            relativo<-sub('^.*src="([^"]+)".*$','\\1',l)
            id<-paste0("fig-",substr(digest::digest(relativo,algo="sha256",serialize=FALSE),1,16))
            nota_elemento<-paste0("Nota da [[figura:",id,"]]: ",defs,".")
          } else {
            pos<-max(c(0L,which(startsWith(out,"<!-- monitora-tabela "))))
            if(pos>0L) {
              id<-sub('^<!-- monitora-tabela ([^ ]+).*$','\\1',out[pos])
              tabela_id<-id;tabela_defs<-c(tabela_defs,defs)
            }
          }
        }
      } else {
        z<-monitora_v305_siglas_no_texto(l,siglas,usadas);l<-z$texto
      }
      if(length(novas)) {
        for(k in novas)audit[[length(audit)+1L]]<-data.frame(sigla=k,significado=siglas[[k]],linha_fonte=i,primeira_ocorrencia=l,stringsAsFactors=FALSE)
        usadas<-unique(c(usadas,novas))
      }
      usadas<-unique(c(usadas,encontradas))
      if(!exoticas_aviso && grepl('exótic',l,ignore.case=TRUE) && (grepl('^#{1,3} |^<figure|^Table:|^[|]',l))) {
        nota<-'A presença registrada de plantas exóticas, isoladamente, não demonstra invasão biológica. Avaliar identidade, estabelecimento, expansão e impactos com evidências adicionais.'
        pos<-if(startsWith(l,'|')||startsWith(l,'Table:'))max(c(0L,which(startsWith(out,'<!-- monitora-tabela '))))else 0L
        if(pos>0L)out<-append(out,c(nota,''),after=pos-1L)else out<-c(out,'',nota,'')
        exoticas_aviso<-TRUE
      }
      # Referência dinâmica: o arquivo identifica a figura mesmo quando o número muda.
      if(startsWith(l,"<figure")&&grepl("Síntese balanceada de mudanças",l,fixed=TRUE)) {
        relativo<-sub('^.*src="([^"]+)".*$','\\1',l);id<-paste0("fig-",substr(digest::digest(relativo,algo="sha256",serialize=FALSE),1,16))
        out<-c(out,"",paste0("A [[figura:",id,"]] reúne a síntese visual dos resultados temporais discutidos nesta seção, incluindo mudanças direcionais, equivalência e resultados inconclusivos conforme os critérios de seleção. Cobertura vegetal e proporção relativa são métricas distintas."),"")
      }
    }
    # Separadores explícitos para títulos, figuras e listas em todos os destinos.
    if(grepl("^#{1,6} |^<figure|^[-*] ",l))out<-c(out,"")
    out<-c(out,l,if(length(nota_elemento))c("",nota_elemento,""))
  }
  institucionais<-intersect(c('ICMBio','CBC'),names(siglas));institucionais<-institucionais[vapply(institucionais,function(k)any(grepl(k,x[seq_len(inicio)],fixed=TRUE)),logical(1))]
  for(k in setdiff(institucionais,usadas)){j<-which(grepl(k,x[seq_len(inicio)],fixed=TRUE))[1L];audit[[length(audit)+1L]]<-data.frame(sigla=k,significado=siglas[[k]],linha_fonte=j,primeira_ocorrencia=x[j],stringsAsFactors=FALSE)}
  usadas<-unique(c(usadas,institucionais))
  if(length(usadas)) {
    out<-c(out,"",'<div class="page-break"></div>',"","# Glossário de siglas, unidades e símbolos","","Organização por tema e, dentro de cada bloco, em ordem alfabética pela sigla. Os símbolos seguem seu nome por extenso.","")
    unidades<-c(`%`="porcentagem",`p.p. / pp`="pontos percentuais",mm="milímetro",cm="centímetro",m="metro",km="quilômetro",`°C`="grau Celsius",kPa="quilopascal",`N/S/L/O`="norte/sul/leste/oeste",n="número de observações identificado no contexto",p="valor de p",q="valor de p ajustado conforme o método declarado",`Δ`="diferença",`R²`="coeficiente de determinação")
    grupos<-monitora_v305_grupos_glossario();categoria<-vapply(usadas,monitora_v305_grupo_sigla,integer(1))
    for(g in seq_along(grupos)) {
      termos<-if(g==7L)names(unidades)else usadas[categoria==g]
      if(!length(termos))next
      significado<-if(g==7L)unidades[termos]else siglas[termos]
      chave<-if(g==7L)significado else termos
      ordem<-order(tolower(iconv(chave,to="ASCII//TRANSLIT")),termos,method="radix")
      gl<-data.frame(Sigla=termos[ordem],`Significado e uso`=unname(significado[ordem]),check.names=FALSE)
      out<-c(out,"",monitora_relatorios_analiticos_kable(gl,id=paste0("glossario-",g)),"")
    }
    out<-c(out,"Cobertura vegetal e proporção relativa têm denominadores distintos; variação em pontos percentuais não equivale a variação percentual relativa.","")
    data.table::fwrite(data.table::rbindlist(audit),file.path(dir_relatorio,paste0("siglas_",base_nome,".csv")),bom=TRUE)
  }
  out
}
