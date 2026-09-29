# Regras editoriais determinísticas; nunca alteram os dados de origem.
monitora_v305_colunas_centrais <- function(cab,mat) {
  vapply(seq_along(cab),function(j) {
    v<-trimws(as.character(mat[,j]));v<-v[!is.na(v)&nzchar(v)]
    grepl('^Ano|^Anos$|Campanha|UAs|^Nº|^Data',cab[j]) ||
      (length(v)>0L && all(grepl('^[−+<>=≤≥±0-9eE.,% ()/:;–-]+$|^(NA|NE|—)$',v)))
  },logical(1))
}
monitora_v305_normalizar_indice <- function(conteudo) {
  x<-unlist(strsplit(paste(conteudo,collapse='\n'),'\n',fixed=TRUE))
  repeat {
    p<-which(x=='<div class="monitora-indice">');if(!length(p))break
    a<-p[1];b<-which(seq_along(x)>a & x=='</div>')[1];if(is.na(b))stop('Índice sem fechamento')
    x<-x[-seq(a,b)]
  }
  # A capa já encerra sua página; descartar somente quebras vazias anteriores ao corpo.
  inicio<-which(grepl('^# [^#]',x))[1]
  if(!is.na(inicio))x<-x[!(seq_along(x)<inicio & grepl('^<div class="page-break">[[:space:]]*</div>$',trimws(x)))]
  x
}
monitora_v305_grupos_glossario <- function() c('Instituições, programas e gestão','Amostragem e vegetação','Estatística e interpretação dos resultados','Fogo e clima','Cartografia e sensoriamento remoto','Sistemas e formatos de arquivos','Unidades e símbolos')
monitora_v305_grupo_sigla <- function(k) {
  grupos<-list(
    c('ICMBio','CBC','MMA','CNUC','NASA','INPE','IBGE','SISMONITORA'),
    c('UA','UAs','UC','UCs','PA','PAs','LPI','D30'),
    c('LB','AUM','RED','EST','INC','PAR','MUD','EST-C','H','IC','ICs','IC95%','IC90%','IC95','IC90','NE','NA','FDR-BH','FDR','BH','BACI','TOST','RDA','PCA','db-RDA','PERMANOVA','NMDS','GAMM','GLLVM','SEM','HTZ','VIF','RMSE','MAE','CR2','GL'),
    c('PMIF','MIF','QP','VPD','UR','POWER','MERRA-2','FAO-56','UTC','LST','PRECTOTCORR','T2M','T2M_MIN','T2M_MAX','RH2M','WS2M','AAF-ICMBio'),
    c('SIRGAS','SIRGAS2000','EPSG','GPS','UTM','WGS84','WGS 84','MSI','RGB','B04','B03','B02','PEC','PEC-PCD','L2A'),
    c('API','AWS','JSON','CSV','CSVs','PDF','DOCX','XLSX','HTML','PNG','PNGs','ID','IDs','XLSForm'))
  if(grepl(' [(]localizador[)]$',k))return(5L)
  i<-which(vapply(grupos,function(x)k%in%x,logical(1)));if(length(i)!=1L)stop('Sigla sem bloco temático: ',k)
  as.integer(i)
}
monitora_v305_siglas_no_texto <- function(l,siglas,usadas) {
  l<-gsub('não estimável: NE:', 'não estimável (NE):', l, fixed=TRUE)
  # As alternativas longas reconhecem definições existentes antes dos tokens isolados.
  nomes<-names(siglas)[order(nchar(names(siglas)),decreasing=TRUE)]
  variantes<-lapply(nomes,function(k)unique(c(siglas[[k]],sub(' \\([^()]+\\)$','',siglas[[k]]),if(k=='LPI')'interceptação linear por pontos',if(k=='NE')'não estimável',if(k=='NA')'não disponível')))
  q<-function(x)paste0('\\Q',x,'\\E')
  definicoes<-unlist(lapply(seq_along(nomes),function(i)paste0('(?i:',q(variantes[[i]]),')\\s*\\(',q(nomes[i]),'\\)')))
  chave<-rep(nomes,lengths(variantes))
  padrao<-paste0('`[^`]*`|https?://[^[:space:]<>)]+|<[^>]+>|\\]\\([^)]*\\)|(?<![[:alnum:]_])(?:',paste(c(definicoes,q(nomes)),collapse='|'),')(?![[:alnum:]_])')
  pos<-gregexpr(padrao,l,perl=TRUE)[[1]];if(pos[1]<0L)return(list(texto=l))
  tamanho<-attr(pos,'match.length');partes<-character();ultimo<-1L;seen<-usadas
  familia<-list(c('UA','UAs'),c('UC','UCs'),c('PA','PAs'),c('IC','ICs'),c('CSV','CSVs'),c('PNG','PNGs'),c('ID','IDs'))
  for(i in seq_along(pos)) {
    a<-pos[i];b<-a+tamanho[i]-1L;t<-substr(l,a,b);novo<-t
    if(!grepl('^`|^https?://|^<|^\\]\\(',t)) {
      k<-if(t%in%nomes)t else {
        hit<-which(vapply(definicoes,function(p)grepl(paste0('^',p,'$'),t,perl=TRUE),logical(1)))
        if(length(hit))chave[hit[1]]else NA_character_
      }
      if(!is.na(k)) {
        fam<-unique(c(k,unlist(familia[vapply(familia,function(g)k%in%g,logical(1))])))
        if(any(fam%in%seen))novo<-k
        else if(t==k)novo<-paste0(siglas[[k]],' (',k,')')
        seen<-unique(c(seen,fam))
      }
    }
    partes<-c(partes,if(a>ultimo)substr(l,ultimo,a-1L),novo);ultimo<-b+1L
  }
  list(texto=paste0(c(partes,if(ultimo<=nchar(l))substr(l,ultimo,nchar(l))),collapse=''))
}

monitora_v305_familia_sigla <- function(x) {
  mapa<-c(UAs="UA",UCs="UC",PAs="PA",ICs="IC",CSVs="CSV",PNGs="PNG",IDs="ID")
  hit<-x%in%names(mapa);x[hit]<-unname(mapa[x[hit]]);x
}
