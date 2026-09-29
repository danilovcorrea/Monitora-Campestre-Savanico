# Inventário editorial dos textos efetivamente desenhados; não altera os PNGs.
# Referências: IBGE (SIRGAS/UTM/PEC), Copernicus SentiWiki (MSI/RGB/bandas),
# ICMBio (Áreas Atingidas por Fogo), gov.br/mma (denominação institucional).
monitora_v305_inventariar_siglas_mapa <- function(paragrafos_quadro, siglas_uf=character(), logos=character()) {
  d <- c(
    SIRGAS2000="Sistema de Referência Geocêntrico para as Américas, realização 2000",
    WGS84="Sistema Geodésico Mundial de 1984 (World Geodetic System 1984)",
    `WGS 84`="Sistema Geodésico Mundial de 1984 (World Geodetic System 1984)",
    UTM="projeção Universal Transversa de Mercator",
    EPSG="identificador do sistema de referência espacial",
    MSI="instrumento multiespectral do Sentinel-2 (MultiSpectral Instrument)",
    RGB="composição nos canais vermelho, verde e azul (Red, Green, Blue)",
    B04="banda 4 do Sentinel-2, vermelho na composição em cor natural",
    B03="banda 3 do Sentinel-2, verde na composição em cor natural",
    B02="banda 2 do Sentinel-2, azul na composição em cor natural",
    PEC="Padrão de Exatidão Cartográfica; qualidade não avaliada neste mapa",
    `PEC-PCD`="Padrão de Exatidão Cartográfica para Produtos Cartográficos Digitais; qualidade não avaliada neste mapa",
    `AAF-ICMBio`="Áreas Atingidas por Fogo do Instituto Chico Mendes de Conservação da Biodiversidade",
    MMA="Ministério do Meio Ambiente e Mudança do Clima",
    ICMBio="Instituto Chico Mendes de Conservação da Biodiversidade",
    CBC="Centro Nacional de Pesquisa e Conservação em Biodiversidade e Restauração Ecológica",
    AWS="Amazon Web Services",
    L2A="nível de processamento 2A, reflectância de superfície",
    UC="unidade de conservação", UA="unidade amostral", UAs="unidades amostrais")
  texto <- paste(c(as.character(paragrafos_quadro),as.character(logos)),collapse="\n")
  presente <- vapply(names(d),function(k)grepl(paste0("(?<![[:alnum:]_])",k,"(?![[:alnum:]_])"),texto,perl=TRUE),logical(1))
  r <- data.frame(sigla=names(d)[presente], significado=unname(d[presente]), contexto=rep("mapa",sum(presente)),stringsAsFactors=FALSE)
  # Estados vêm dos rótulos realmente desenhados, nunca da varredura do texto.
  uf <- c(AC="Acre",AL="Alagoas",AP="Amapá",AM="Amazonas",BA="Bahia",CE="Ceará",DF="Distrito Federal",ES="Espírito Santo",GO="Goiás",MA="Maranhão",MT="Mato Grosso",MS="Mato Grosso do Sul",MG="Minas Gerais",PA="Pará",PB="Paraíba",PR="Paraná",PE="Pernambuco",PI="Piauí",RJ="Rio de Janeiro",RN="Rio Grande do Norte",RS="Rio Grande do Sul",RO="Rondônia",RR="Roraima",SC="Santa Catarina",SP="São Paulo",SE="Sergipe",TO="Tocantins")
  usados <- unique(as.character(siglas_uf)); usados <- usados[!is.na(usados) & usados %in% names(uf)]
  if(length(usados))r <- rbind(r,data.frame(sigla=paste0(usados," (localizador)"),significado=unname(uf[usados]),contexto="localizador",stringsAsFactors=FALSE))
  rownames(r)<-NULL
  r
}

monitora_v305_raiz_siglas_figura <- function(arquivo) {
  origem <- dirname(normalizePath(arquivo,winslash="/",mustWork=TRUE))
  p <- origem
  repeat {
    if(basename(p)=="figuras")return(dirname(p))
    anterior<-p;p<-dirname(p)
    if(identical(anterior,p))return(origem) # Chamadas avulsas mantêm inventário ao lado da figura.
  }
}

monitora_v305_chave_siglas_figura <- function(arquivo,dir_relatorio) {
  raiz<-paste0(sub("/+$","",normalizePath(dir_relatorio,winslash="/",mustWork=TRUE)),"/")
  p<-normalizePath(arquivo,winslash="/",mustWork=TRUE)
  if(!startsWith(p,raiz))stop("Figura fora do diretório do relatório.")
  substring(p,nchar(raiz)+1L)
}

monitora_v305_registrar_siglas_figura <- function(destino,entradas,dir_relatorio=NULL) {
  if(is.null(dir_relatorio))dir_relatorio<-monitora_v305_raiz_siglas_figura(destino)
  chave<-monitora_v305_chave_siglas_figura(destino,dir_relatorio)
  cols<-c("sigla","significado","contexto")
  stopifnot(is.data.frame(entradas),all(cols %in% names(entradas)))
  entradas<-unique(entradas[,cols,drop=FALSE])
  arquivo<-file.path(dir_relatorio,"siglas_figuras.csv")
  schema<-c("arquivo",cols,"md5")
  anterior<-if(file.exists(arquivo))utils::read.csv(arquivo,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8") else as.data.frame(setNames(rep(list(character()),length(schema)),schema))
  if(!identical(names(anterior),schema))stop("Esquema inválido em siglas_figuras.csv")
  atual<-data.frame(arquivo=rep(chave,nrow(entradas)),entradas,md5=rep(unname(tools::md5sum(destino)),nrow(entradas)),stringsAsFactors=FALSE)
  r<-rbind(anterior[anterior$arquivo!=chave,,drop=FALSE],atual)
  r<-r[order(r$arquivo,r$contexto,r$sigla,method="radix"),,drop=FALSE]
  utils::write.csv(r,arquivo,row.names=FALSE,fileEncoding="UTF-8",na="")
  invisible(r)
}

monitora_v305_ler_siglas_figura <- function(arquivo_figura,dir_relatorio) {
  vazio<-data.frame(sigla=character(),significado=character(),contexto=character(),stringsAsFactors=FALSE)
  arquivo<-file.path(dir_relatorio,"siglas_figuras.csv")
  if(!file.exists(arquivo))return(vazio)
  # Chamador pode fornecer o src relativo do elemento figure ou caminho absoluto.
  candidato<-file.path(dir_relatorio,arquivo_figura)
  if(!file.exists(candidato))candidato<-arquivo_figura
  if(!file.exists(candidato))return(vazio)
  chave<-monitora_v305_chave_siglas_figura(candidato,dir_relatorio)
  d<-utils::read.csv(arquivo,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
  if(!all(c("arquivo",names(vazio),"md5") %in% names(d)))stop("Esquema inválido em siglas_figuras.csv")
  d<-d[d$arquivo==chave,,drop=FALSE]
  if(!nrow(d))return(vazio)
  if(any(is.na(d$md5)|d$md5!=unname(tools::md5sum(candidato))))stop("Inventário de siglas desatualizado para a figura: ",chave)
  unique(d[,names(vazio),drop=FALSE])
}
