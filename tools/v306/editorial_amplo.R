# Apresentação condicionada aos produtos da própria UC. Nenhum modelo é reajustado.
monitora_v306_dados <- function(p,n,cols=character()) {
  z<-monitora_v306_ler(p,n)
  if(nrow(z)&&!all(cols%in%names(z))){warning('Relatório: estrutura indisponível para a síntese de ',n,call.=FALSE);return(data.table::data.table())}
  z
}
monitora_v306_par <- function(x)as.vector(rbind(x,rep('',length(x))))
monitora_v306_form <- function(x)monitora_relatorio_rotulo_formacao(x,TRUE)
monitora_v306_anos <- function(x)paste(sort(unique(x[is.finite(x)])),collapse=', ')
monitora_v306_lacunas <- function(x) {
  y<-sort(unique(x[is.finite(x)]));if(length(y)<2)return('Há somente um ano observado; não se descreve uma mudança temporal.')
  aus<-setdiff(seq.int(min(y),max(y)),y)
  if(length(aus))paste0('Anos sem observações neste recorte: ',paste(aus,collapse=', '),'; não foram preenchidos nem interpretados como ausência dos indicadores.')else''
}
monitora_v306_limpar_notas <- function(x,p) {
  x<-strsplit(paste(x,collapse='\n'),'\n',fixed=TRUE)[[1]];x[grepl('<!-- monitora-nota-automatica -->',x,fixed=TRUE)]<-''
  gl<-grep('^# ([0-9]+ )?Glossário de siglas, unidades e símbolos',x)
  if(length(gl)&&any(grepl('monitora-(tabela glossario-|tab-glossario-)',x))){x<-head(x,gl[1]-1L);while(length(x)&&(!nzchar(trimws(tail(x,1)))||tail(x,1)=='<div class="page-break"></div>'))x<-head(x,-1L)}
  sig<-monitora_v305_siglas()
  f<-file.path(p,'siglas_figuras.csv')
  if(file.exists(f)){m<-data.table::fread(f);if(all(c('sigla','significado')%in%names(m)))sig<-c(sig,stats::setNames(m$significado,m$sigla))}
  defs<-unique(c(paste0(unname(sig),' (',names(sig),')'),names(sig)));defs<-defs[order(nchar(defs),decreasing=TRUE)]
  for(i in grep('^Nota da (\\[\\[(tabela|figura):|\\[(Tabela|Figura) [0-9])',x)) {
    t<-sub('^Nota da .*?\\]\\](?:\\:)?[[:space:]]*','',x[i],perl=TRUE)
    if(identical(t,x[i]))t<-sub('^Nota da \\[[^]]+\\]\\([^)]+\\):[[:space:]]*','',x[i])
    for(d in defs)t<-gsub(d,'',t,fixed=TRUE)
    # Remover somente uma nota inteiramente reconhecida como definição automática.
    if(!nzchar(gsub('[[:space:];.,()]','',t)))x[i]<-''
  }
  x
}
monitora_v306_termos <- function(x) {
  trocar<-function(s) {
    pares<-c('no calendário'='no ciclo anual','O calendário abaixo'='A distribuição das datas abaixo','Calendário das campanhas'='Épocas das campanhas',
      'Calendário concentrado'='Amostragens concentradas','calendário concentrado'='amostragens concentradas',
      'calendário e médias'='época de amostragem e médias','Calendário'='Época de amostragem','calendário'='época de amostragem',
      'Revisitas'='Reamostragens','revisitas'='reamostragens','revisita'='reamostragem',
      'Concentração de datas e confundimento UA/ano impedem interpretar sazonalidade.'='A concentração das datas e a dificuldade de separar época de amostragem, diferenças entre UAs e anos limitam a interpretação da sazonalidade.',
      'O época de amostragem'='A época de amostragem','A época de amostragem pode permanecer confundido'='A época de amostragem pode permanecer confundida','da época de amostragem de amostragem'='da época de amostragem',
      'o época de amostragem de amostragem'='a época de amostragem','o época de amostragem'='a época de amostragem',
      'Época de amostragem e reamostragens'='Época de amostragem e reamostragens',
      'Entrantes (%)'='Primeira amostragem (%)','Retenção (%)'='Manutenção (%)','Entrantes'='Primeira amostragem','Retidas'='Mantidas',
      'Ausentes'='Não reamostradas','Retornantes'='Retomadas','Entrada, retenção, ausência e retorno de UAs por ano'='Continuidade da amostragem das UAs por ano',
      'ano anterior'='ano observado anterior')
    for(k in names(pares))s<-gsub(k,pares[[k]],s,fixed=TRUE)
    s
  }
  # Preservar nomes de produtos, URLs, âncoras e atributos; traduzir somente apresentação.
  vapply(x,function(l){partes<-strsplit(l,'(`[^`]*`|<[^>]*>|https?://[^[:space:]<>]+|\\]\\([^)]*\\))',perl=TRUE)[[1]]
    hits<-gregexpr('`[^`]*`|<[^>]*>|https?://[^[:space:]<>]+|\\]\\([^)]*\\)',l,perl=TRUE)[[1]]
    if(hits[1]<0)return(trocar(l));out<-character();a<-1L
    for(j in seq_along(hits)){b<-hits[j];if(b>a)out<-c(out,trocar(substr(l,a,b-1L)));e<-b+attr(hits,'match.length')[j]-1L;out<-c(out,substr(l,b,e));a<-e+1L}
    if(a<=nchar(l))out<-c(out,trocar(substr(l,a,nchar(l))));paste(out,collapse='')
  },character(1),USE.NAMES=FALSE)
}
monitora_v306_fig_pos <- function(x,padrao)which(startsWith(x,'<figure')&grepl(padrao,x,perl=TRUE))
monitora_v306_ref <- function(l) {
  f<-sub('^.*src="([^"]+)".*$','\\1',l)
  paste0('[[figura:fig-',substr(digest::digest(f,algo='sha256',serialize=FALSE),1,16),']]')
}
monitora_v306_apos <- function(x,i,t) {
  if(!length(i)||!length(t))return(x)
  append(x,c('',monitora_v306_par(t)),after=max(i))
}
monitora_v306_fogo <- function(p) {
  d<-monitora_v306_dados(p,'fogo_situacao_historico',c('estado','posicoes_com_intersecao','posicoes_elegiveis','anos_consultados'))
  h<-monitora_v306_dados(p,'fogo_hipoteses_e_decisoes',c('Situacao'))
  if(!nrow(d))return('Fogo: não há diagnóstico cartográfico disponível nesta execução; isso não permite concluir ausência de fogo.')
  if(!is.finite(d$posicoes_elegiveis[1])||d$posicoes_elegiveis[1]<=0||!is.finite(d$posicoes_com_intersecao[1]))return(paste0('Fogo: ',d$estado[1],'. Não há suporte espacial elegível para quantificar interseções; a indeterminação não foi convertida em ausência de fogo.'))
  anos<-suppressWarnings(as.integer(strsplit(as.character(d$anos_consultados[1]),';',fixed=TRUE)[[1]]));anos<-anos[is.finite(anos)]
  periodo<-if(length(anos))paste0(min(anos),'–',max(anos),'; ',length(unique(anos)),' camadas anuais')else'anos não informados'
  t<-paste0('Fogo: no histórico cartográfico consultado (',periodo,'), ',d$posicoes_com_intersecao[1],' de ',d$posicoes_elegiveis[1],' posições elegíveis de campanha intersectam registros de fogo. Essas posições incluem amostragens repetidas das mesmas UAs; não são episódios independentes nem comprovação de fogo anterior a cada coleta.')
  if(nrow(h)&&all(grepl('^NE',h$Situacao)))t<-paste0(t,' Não foi possível atribuir as mudanças de cobertura às modalidades de fogo nem estimar regeneração.')
  else if(nrow(h))t<-paste0(t,' O alcance de cada hipótese depende dos resultados condicionais apresentados na seção de fogo; uma interseção, isoladamente, não atribui causa.')
  else t<-paste0(t,' As hipóteses de efeitos das modalidades não têm resultado disponível para síntese nesta execução.')
  paste0(t,' Ausência de registro não comprova ausência de fogo.')
}
monitora_v306_clima <- function(p,curto=FALSE) {
  d<-monitora_v306_dados(p,'clima_resumo_90dias',c('ANO','metrica','media','anomalia_media','n_completas'));num<-monitora_v306_num
  if(!nrow(d))return('Clima: não há resumo válido das condições antecedentes às coletas nesta execução.')
  out<-character();labs<-c(chuva_mm='chuva acumulada média',temp_c='temperatura média',ur_pct='umidade relativa média');units<-c(chuva_mm='mm',temp_c='°C',ur_pct='%')
  for(k in names(labs)) {
    q<-d[metrica==k & is.finite(media)&n_completas>0];if(!nrow(q))next;data.table::setorder(q,ANO)
    if(nrow(q)==1L)z<-paste0(labs[k],': ',num(q$media),' ',units[k],' em ',q$ANO,'; somente um ano disponível')
    else if(diff(range(q$media))<1e-8)z<-paste0(labs[k],': ',num(q$media[1]),' ',units[k],' nos anos disponíveis; sem variação entre essas médias')
    else {lo<-q[which.min(media)];hi<-q[which.max(media)];z<-paste0(labs[k],': ',num(lo$media),' ',units[k],' (',lo$ANO,') a ',num(hi$media),' ',units[k],' (',hi$ANO,')')}
    out<-c(out,z)
  }
  if(!length(out))return('Clima: não há janelas completas para descrever as condições antecedentes.')
  t<-paste0('Clima: nas janelas de 90 dias anteriores às coletas, ',paste(out,collapse='; '),'. As médias resumem consultas com datas e composição variáveis; não são médias anuais de toda a UC e não estabelecem tendência climática ou causa das mudanças da vegetação.')
  if(curto)return(t)
  a<-d[is.finite(anomalia_media)&n_completas>0 & metrica%in%names(labs)]
  for(k in names(labs)){q<-a[metrica==k];if(!nrow(q))next;lo<-q[which.min(anomalia_media)];hi<-q[which.max(anomalia_media)];t<-c(t,paste0('Para ',labs[k],', as anomalias médias em relação a 1991–2020 variaram de ',num(lo$anomalia_media),' (',lo$ANO,') a ',num(hi$anomalia_media),' (',hi$ANO,') ',if(k=='ur_pct')'p.p.'else units[k],'. O sinal indica diferença da referência nas janelas observadas.'))}
  c(t,monitora_v306_lacunas(d$ANO))
}
monitora_v306_epoca <- function(p) {
  d<-monitora_v306_dados(p,'calendario_modelos_por_coleta',c('form_veg','status','motivo','amplitude_dias'))
  if(!nrow(d))return('Época de amostragem: suporte por coleta indisponível nesta execução.')
  z<-unique(d[,.(form_veg,status,motivo,amplitude_dias)]);out<-character()
  for(f in unique(z$form_veg)){q<-z[form_veg==f];a<-unique(q$amplitude_dias[is.finite(q$amplitude_dias)]);m<-unique(q$motivo[!is.na(q$motivo)&nzchar(q$motivo)])
    out<-c(out,paste0(monitora_v306_form(f),': ',if(length(a))paste0('datas distribuídas em faixa(s) de ',paste(monitora_v306_num(a,0),collapse=', '),' dias do ciclo anual. ')else'',if(all(q$status=='NE'))paste0('Associação por coleta não estimável: ',paste(m,collapse='; '),'.')else'Há resultados por coleta disponíveis; a leitura deve considerar o suporte e o diagnóstico de cada indicador.'))}
  paste0('Época de amostragem — ',paste(out,collapse=' '),' A faixa se refere à posição das datas no ciclo anual, não à duração da pesquisa. Falta de contraste não demonstra ausência de influência sazonal.')
}
monitora_v306_series <- function(p,g,metricas) {
  d<-monitora_v306_dados(p,'series_anuais_relatorios_por_ua',c('grupo_grafico','tipo_metrica','form_veg','ANO','categoria','categoria_label','media_percent','n_UA'));num<-monitora_v306_num
  if(!nrow(d))return('Não há série tabulada disponível para descrever os resultados desta figura.')
  out<-c('**Resultados descritivos das séries.** A síntese abaixo destaca, por formação e métrica, até dois componentes exibidos com maior diferença absoluta entre a primeira e a última média disponível. A escolha independe de significância; as figuras preservam os demais componentes. As médias podem representar conjuntos diferentes de UAs; não constituem, por si, um contraste pareado.')
  for(m in metricas) {
    q<-data.table::copy(d[grupo_grafico==g & tipo_metrica==m & is.finite(media_percent)&is.finite(ANO)])
    if(!nrow(q))next
    q[,rot:=monitora_relatorios_analiticos_rotulo_categoria(categoria,categoria_label)]
    # Mesma seleção dos gráficos temporais: até oito rótulos por máximo observado.
    ordem<-q[,.(v=max(media_percent)),by=rot][order(-v)];q<-q[rot%in%head(ordem$rot,8L)]
    for(f in unique(q$form_veg)) {
      a<-q[form_veg==f];data.table::setorder(a,rot,ANO)
      z<-a[,.(ini=ANO[1],fim=tail(ANO,1),v1=media_percent[1],v2=tail(media_percent,1),n1=n_UA[1],n2=tail(n_UA,1),anos=monitora_v306_anos(ANO)),by=rot]
      z[,delta:=v2-v1];data.table::setorder(z,-delta);z<-z[order(-abs(delta),rot)][seq_len(min(2L,nrow(z)))]
      for(i in seq_len(nrow(z))){b<-z[i];txt<-if(b$ini==b$fim)paste0(b$rot,': ',num(b$v1),'% em ',b$ini,' (',b$n1,' UAs); apenas um ano disponível')else paste0(b$rot,': ',num(b$v1),'% em ',b$ini,' (',b$n1,' UAs) para ',num(b$v2),'% em ',b$fim,' (',b$n2,' UAs), diferença descritiva de ',num(b$delta),' p.p.')
        out<-c(out,paste0(monitora_v306_form(f),' — ',if(m=='cobertura')'cobertura vegetal'else'proporção relativa',': ',txt,'.'))}
      lac<-monitora_v306_lacunas(a$ANO);if(nzchar(lac))out<-c(out,paste0(monitora_v306_form(f),': ',lac))
    }
  }
  c(out,'A interpretação inferencial depende dos resultados pareados e da linha de base apresentados nos painéis de evidência, com seus próprios períodos e populações. Diferenças entre extremos não descrevem necessariamente uma trajetória monotônica.')
}
monitora_v306_classes <- function(d,col) {
  if(!nrow(d))return('sem resultados disponíveis')
  labs<-c(aumento='aumentos',reducao='reduções',estabilidade_equivalente='estabilidade/equivalência',inconclusivo='inconclusivos',pares_insuficientes='pares insuficientes',mudanca_composicao='mudanças de composição',estabilidade_composicional='estabilidade composicional')
  v<-as.character(d[[col]]);v[is.na(v)|!nzchar(v)]<-'sem classificação';t<-table(v);nome<-ifelse(names(t)%in%names(labs),labs[names(t)],names(t))
  paste(paste(as.integer(t),nome),collapse='; ')
}
monitora_v306_evidencias <- function(p,g=NULL) {
  d<-monitora_v306_dados(p,'nar_contrastes',c('grupo_grafico','form_veg','tipo_metrica','classe_mudanca','ano_1','ano_2'))
  if(!nrow(d))return('A síntese textual dos contrastes não está disponível; os resultados devem ser consultados nos painéis e produtos estatísticos desta execução.')
  if(!is.null(g))d<-d[grupo_grafico==g]
  if(!nrow(d))return(character())
  out<-'**Síntese dos contrastes entre períodos.** As contagens abaixo abrangem os resultados disponíveis deste tema, sem somar as comparações adicionais com a linha de base. Cada resultado pertence a um indicador, formação, métrica e par de períodos; não representa uma UA ou um evento independente.'
  for(f in unique(d$form_veg))for(m in unique(d$tipo_metrica)) {
    a<-d[form_veg==f & tipo_metrica==m];if(!nrow(a))next
    out<-c(out,paste0(monitora_v306_form(f),' — ',if(m=='cobertura')'cobertura vegetal'else'proporção relativa',': ',monitora_v306_classes(a,'classe_mudanca'),'.'))
  }
  c(out,'Equivalência depende da margem e dos critérios adotados; resultados inconclusivos ou pares insuficientes não foram classificados como estabilidade. Cobertura e proporção relativa não são contagens intercambiáveis.')
}
monitora_v306_amplo <- function(x,p) {
  if(any(x=='<!-- monitora-v306-revisado -->'))return(x)
  num<-monitora_v306_num;ler<-function(n,cols=character())monitora_v306_dados(p,n,cols)
  contexto<-ler('nar_contexto',c('fogo','clima'));ativo<-function(k)nrow(contexto)==1L&&identical(contexto[[k]][1],'concluído')
  form<-ler('esforco_amostral_por_uc_formacao_ano',c('form_veg'))
  if(nrow(form)){ff<-unique(form$form_veg);frase<-paste0('Os resultados disponíveis abrangem ',paste(monitora_v306_form(ff),collapse=' e '),'. ',if(length(ff)==1L)'Não há comparação entre formações nesta execução.'else'As formações são interpretadas separadamente.');x<-gsub('Formações campestre e savânica são interpretadas separadamente.',frase,x,fixed=TRUE)}
  # Resumo executivo: substituir apenas os blocos correspondentes, sem tocar nos achados vegetacionais.
  for(i in which(startsWith(x,'<div class="callout">')&grepl('Fogo:|Clima|Calendário',x))) {
    z<-strsplit(x[i],'<br><br>',fixed=TRUE)[[1]]
    for(j in seq_along(z)) {
      prefixo<-if(startsWith(z[j],'<div'))sub('^(<div[^>]*>).*','\\1',z[j])else''
      if(nzchar(prefixo))z[j]<-substring(z[j],nchar(prefixo)+1L)
      fim<-if(endsWith(z[j],'</div>'))'</div>'else''
      if(startsWith(z[j],'Fogo:'))z[j]<-paste0(if(ativo('fogo'))monitora_v306_fogo(p)else'Fogo: não há confirmação de resultados cartográficos disponíveis nesta execução; isso não demonstra ausência de fogo.',fim)
      if(startsWith(z[j],'Clima'))z[j]<-paste0(if(ativo('clima'))monitora_v306_clima(p,TRUE)else'Clima: não há confirmação de resultados climáticos disponíveis nesta execução; isso não demonstra ausência de influência climática.',fim)
      if(startsWith(z[j],'Calendário'))z[j]<-paste0(monitora_v306_epoca(p),fim)
      z[j]<-paste0(prefixo,z[j])
    }
    x[i]<-paste(z,collapse='<br><br>')
  }
  # O significado dos fluxos é calculado por ano OBSERVADO anterior.
  j<-grep('^Entrantes são UAs observadas',x)
  x[j]<-'Primeira amostragem identifica a primeira ocorrência da UA na série disponível, sem comprovar sua implantação naquele ano. Mantidas são UAs observadas no ano atual e no ano observado anterior; não reamostradas estavam no anterior e não aparecem no atual; retomadas já haviam sido observadas, estavam ausentes no anterior e reaparecem. Ano observado anterior pode não ser o ano civil imediatamente anterior. A presença da UA não comprova igualdade da intensidade do esforço.'
  # Esforço observado e lacunas.
  z<-ler('esforco_amostral_por_uc_ano',c('ANO','n_UAs_amostradas'));i<-monitora_v306_fig_pos(x,'src="figuras/esforco_amostral_temporal[.]png')
  if(length(i)&&nrow(z)){data.table::setorder(z,ANO);t<-paste0('Na ',monitora_v306_ref(x[i[1]]),', o esforço observado reúne ',length(unique(z$ANO)),' anos: ',monitora_v306_anos(z$ANO),'. O número anual de UAs variou de ',min(z$n_UAs_amostradas),' a ',max(z$n_UAs_amostradas),'; foram ',z$n_UAs_amostradas[1],' no primeiro ano e ',tail(z$n_UAs_amostradas,1),' no último.')
    c<-ler('continuidade_uas',c('classe_continuidade_label'));if(nrow(c)){a<-c[,.N,by=classe_continuidade_label];t<-c(t,paste0('Continuidade na série disponível: ',paste(paste0(a$N,' UAs — ',a$classe_continuidade_label),collapse='; '),'.'))}
    x<-monitora_v306_apos(x,i,c(t,monitora_v306_lacunas(z$ANO),'A composição da rede deve ser considerada nas médias anuais; as comparações inferenciais usam as UAs pareadas de cada contraste.'))
  }
  z<-ler('esforco_incremental_entrada_retencao_retorno',c('ANO','n_UAs_entrantes','n_UAs_retidas_do_ano_anterior','n_UAs_ausentes_desde_ano_anterior','n_UAs_retornantes'))
  i<-monitora_v306_fig_pos(x,'src="figuras/esforco_incremental_grupos_ano_entrada[.]png')
  if(length(i)&&nrow(z)){data.table::setorder(z,ANO);a<-z[-1L];t<-paste0('Na série disponível, ',z$n_UAs_entrantes[1],' UAs aparecem no primeiro ano observado (',z$ANO[1],').')
    if(nrow(a)){b<-a[n_UAs_entrantes>0];t<-c(t,if(nrow(b))paste0('Primeiras amostragens posteriores: ',paste(paste0(b$ANO,': ',b$n_UAs_entrantes,' UAs'),collapse='; '),'.')else'Não houve primeira amostragem de novas UAs nos anos posteriores.')
      t<-c(t,paste0('Nas comparações entre anos observados sucessivos, ocorreram ',sum(a$n_UAs_ausentes_desde_ano_anterior),' registros de não reamostragem e ',sum(a$n_UAs_retornantes),' de retomada. São contagens de transições; uma UA pode contribuir mais de uma vez.'))}
    x<-monitora_v306_apos(x,i,t)
  }
  # Pares de métricas compartilham a síntese, com valores e formações explícitos.
  mapa<-c(categorias_gerais='categorias_gerais',herbaceas_lenhosas='herbaceas_lenhosas',formas_vida_nativas='formas_nativas',formas_vida_exoticas='formas_exoticas',formas_vida_secas_mortas='formas_secas_mortas',material_botanico='material_botanico')
  # Estado da cobertura: manter figuras e interpretação, sem as duas enumerações adicionais.
  for(g in setdiff(names(mapa),'categorias_gerais')) {
    i<-monitora_v306_fig_pos(x,paste0('src="figuras/(cobertura|proporcao)_',mapa[g],'_serie_temporal[.]png'))
    if(length(i)){m<-c(if(any(grepl('figuras/cobertura_',x[i],fixed=TRUE)))'cobertura',if(any(grepl('figuras/proporcao_',x[i],fixed=TRUE)))'proporcao_relativa');x<-monitora_v306_apos(x,i,monitora_v306_series(p,g,m))}
    i<-monitora_v306_fig_pos(x,paste0('src="figuras/evidencia_estatistica_',g,'_'))
    if(length(i))x<-monitora_v306_apos(x,i,monitora_v306_evidencias(p,g))
  }
  i<-monitora_v306_fig_pos(x,'src="figuras/mudancas_temporais_prioritarias[.]png')
  if(length(i)){z<-ler('nar_prioritarias',c('classe_mudanca'));t<-if(nrow(z))paste0('Os contrastes exibidos nesta figura reúnem ',monitora_v306_classes(z,'classe_mudanca'),'. Ela é uma seleção dos contrastes, não a contagem de UAs nem de processos independentes. Os painéis temáticos e os produtos estatísticos conservam os resultados não selecionados.')else'Os resultados da síntese devem ser lidos com os períodos e populações indicados na figura; não há índice tabulado disponível para ampliar a descrição.'
    x<-monitora_v306_apos(x,i,t)
  }
  # Composição: texto após tabela, sem redefinir a classificação estatística.
  z<-monitora_v306_tabela(x,'composicao');j<-grep('^A análise de composição complementa',x)
  if(length(j)&&nrow(z)&&all(c('Interpretação','Formação','Métrica','Referência')%in%names(z))){t<-paste0('Na seleção apresentada na tabela, ',paste(paste0(names(table(z[['Interpretação']])),': ',as.integer(table(z[['Interpretação']]))),collapse='; '),'. As linhas correspondem aos grupos, métricas e referências explicitados na tabela; não são contagens de UAs nem um balanço de todos os testes da série.');x<-append(x,c(monitora_v306_par(t)),after=j[1]-1L)}
  # Época: contexto permanece antes, resultados específicos passam a seguir a figura.
  i<-monitora_v306_fig_pos(x,'src="figuras/epoca_linha_base_sintese[.]png')
  if(length(i)){j<-which(seq_along(x)<i[1]&grepl('^Associação global |^Para secas/mortas,|^A janela exploratória de ',x));t<-x[j];if(length(j))x<-x[-j];i<-monitora_v306_fig_pos(x,'src="figuras/epoca_linha_base_sintese[.]png');x<-monitora_v306_apos(x,i,t)}
  i<-monitora_v306_fig_pos(x,'src="figuras/calendario_coletas_observadas[.]png');if(length(i))x<-monitora_v306_apos(x,i,monitora_v306_epoca(p))
  # Fogo: sínteses entre conjuntos funcionais de figuras.
  i<-monitora_v306_fig_pos(x,'histórico geral[.]')
  if(length(i))x<-monitora_v306_apos(x,i,monitora_v306_fogo(p))
  i<-monitora_v306_fig_pos(x,'queimas prescritas[.]');z<-ler('fogo_suporte_modalidades',c('Ano','Modalidade','UAs','Posicoes'))
  if(length(i)&&nrow(z)){z<-z[order(Ano,Modalidade)];t<-vapply(unique(z$Modalidade),function(m){a<-z[Modalidade==m];paste0('Modalidade ',m,': ',paste(paste0(a$Ano,' — ',a$UAs,' UAs'),collapse='; '),'.')},character(1));x<-monitora_v306_apos(x,i,c('**Interseções por ano cartográfico e modalidade.** O histórico consultado contém os seguintes registros espaciais:',t,'Cada número conta UAs com segmentos intersectados, não eventos independentes ou UAs comprovadamente queimadas antes da coleta. Modalidades e anos podem compartilhar UAs; seus totais não devem ser somados.'))}
  i<-monitora_v306_fig_pos(x,'ampliação do setor [0-9]+[.]');if(length(i))x<-monitora_v306_apos(x,i,'As ampliações detalham a distribuição espacial dos mesmos registros do conjunto de mapas. Elas não acrescentam novas UAs ou novos eventos ao balanço descrito acima; a posição dos símbolos deve ser interpretada com a modalidade e o ano cartográfico.')
  i<-monitora_v306_fig_pos(x,'Cobertura vegetal e observações anteriores');z<-ler('fogo_resumo_descritivo',c('ANO','form_veg','indicador_rotulo','grupo_observacao','n_UAs','media_cobertura'))
  if(length(i)&&nrow(z)){t<-character();for(f in unique(z$form_veg)){a<-z[form_veg==f & is.finite(media_cobertura)];if(!nrow(a))next;ano<-max(a$ANO);a<-a[ANO==ano];t<-c(t,paste0(monitora_v306_form(f),' — ano da campanha ',ano,': ',paste(paste0(a$indicador_rotulo,' / ',a$grupo_observacao,': ',num(a$media_cobertura),'% (',a$n_UAs,' UAs)'),collapse='; '),'.'))}
    x<-monitora_v306_apos(x,i,c('No último ano disponível de cada formação, as médias descritivas por categoria e grupo de observação foram:',t,'Os grupos refletem registros observados, sem controle de diferenças prévias, cronologia do evento ou composição das amostras. Essas médias não medem efeito da queima.'))}
  i<-monitora_v306_fig_pos(x,'fogo_matriz_modalidades_[0-9]+[.]png')
  if(length(i)){for(k in seq_along(i))x[i[k]]<-sub('</figcaption>',paste0(' — parte ',k,' de ',length(i),'.</figcaption>'),x[i[k]],fixed=TRUE)
    x<-monitora_v306_apos(x,i,'As partes do histórico distribuem as UAs para leitura, mantendo as modalidades e lacunas documentadas. Ausência de registro em um ano não demonstra ausência de fogo; registros de modalidades diferentes no mesmo ano não equivalem a eventos independentes.')}
  # Clima: condições/ anomalias, associações, modelos e transições; discussão ao final.
  i<-monitora_v306_fig_pos(x,'clima_(antecedentes|anomalias)_90dias[.]png');if(length(i))x<-monitora_v306_apos(x,i,monitora_v306_clima(p))
  i<-monitora_v306_fig_pos(x,'clima_cobertura_chuva[.]png');j<-grep('^## Associações exploratórias com a cobertura vegetal',x)
  if(length(i)&&length(j)){t<-x[i];x<-x[-i];j<-grep('^## Associações exploratórias com a cobertura vegetal',x);x<-append(x,c('',t,''),after=j[1]+1L)}
  z<-ler('clima_associacoes_exploratorias',c('janela_dias','metrica','form_veg','indicador_rotulo','rho_descritivo','rho_anomalia_descritivo'));i<-monitora_v306_fig_pos(x,'clima_cobertura_chuva[.]png')
  if(length(i)&&nrow(z)){a<-z[janela_dias==90 & metrica=='chuva_mm'];t<-character();for(f in unique(a$form_veg)){q<-a[form_veg==f];t<-c(t,paste0(monitora_v306_form(f),' — chuva antecedente: ',paste(paste0(q$indicador_rotulo,': correlação descritiva ',num(q$rho_descritivo,2),'; com anomalia ',num(q$rho_anomalia_descritivo,2)),collapse='; '),'.'))};x<-monitora_v306_apos(x,i,c(t,'As correlações resumem grupos de célula/data e podem compartilhar UAs, anos e janelas. Não isolam uma resposta temporal, não estabelecem efeito e não possuem teste de significância neste módulo.'))}
  z<-ler('clima_transicoes_por_celula',c('form_veg','indicador_rotulo','ANO_anterior','ANO','delta_cobertura_media','celula'))
  i<-monitora_v306_fig_pos(x,'clima_transicoes_celulas[.]png')
  if(length(i)&&nrow(z)){t<-character();for(f in unique(z$form_veg)){a<-z[form_veg==f];for(ind in unique(a$indicador_rotulo)){q<-a[indicador_rotulo==ind & is.finite(delta_cobertura_media)];if(!nrow(q))next;t<-c(t,paste0(monitora_v306_form(f),' — ',ind,': nas ',nrow(q),' combinações de célula/transição disponíveis, a diferença média de cobertura variou de ',num(min(q$delta_cobertura_media)),' a ',num(max(q$delta_cobertura_media)),' p.p.; ',sum(q$delta_cobertura_media>1e-8),' positivas, ',sum(q$delta_cobertura_media< -1e-8),' negativas e ',sum(abs(q$delta_cobertura_media)<=1e-8),' nulas.'))}}
    x<-monitora_v306_apos(x,i,c(t,'As contagens descrevem sinais de diferenças, não testes de aumento, redução ou equivalência. Cada transição conserva suas próprias UAs e células; não são réplicas independentes.'))}
  z<-ler('clima_desempenho_preditivo_por_ano',c('form_veg','indicador','ganho_RMSE','ANO','n_extrapolacoes'));j<-grep('^Ganho positivo de RMSE indica',x)
  if(length(j)&&nrow(z)){t<-character();for(f in unique(z$form_veg)){a<-z[form_veg==f];for(ind in unique(a$indicador)){q<-a[indicador==ind & is.finite(ganho_RMSE)];if(!nrow(q))next;t<-c(t,paste0(monitora_v306_form(f),' — ',monitora_v306_ind(ind),': o modelo com clima melhorou o erro preditivo em ',sum(q$ganho_RMSE>1e-8),' de ',nrow(q),' anos avaliados, piorou em ',sum(q$ganho_RMSE< -1e-8),' e empatou em ',sum(abs(q$ganho_RMSE)<=1e-8),'; ganho de RMSE entre ',num(min(q$ganho_RMSE)),' e ',num(max(q$ganho_RMSE)),' p.p. Anos com extrapolação: ',if(any(q$n_extrapolacoes>0))monitora_v306_anos(q$ANO[q$n_extrapolacoes>0])else'nenhum sinalizado','.'))}}
    x<-monitora_v306_apos(x,j,t)}
  a<-which(x=='## Discussão climática');b<-which(x=='## Séries curtas: mudanças observadas por célula climática')
  if(length(a)==1L&&length(b)==1L&&a<b){fim<-which(seq_along(x)>b & startsWith(x,'# '))[1];if(is.na(fim))fim<-length(x)+1L;dis<-x[a:(b-1L)];x<-c(head(x,a-1L),x[b:(fim-1L)],'',dis,if(fim<=length(x))x[fim:length(x)]else character())}
  # Conexão entre matriz de evidências e prioridades, sem repetir suas linhas.
  i<-grep('^A matriz completa permanece',x);if(length(i))x<-monitora_v306_apos(x,i,'As prioridades de investigação decorrem dos resultados e limites indicados na matriz. A ação de manejo deve ser condicionada à confirmação das hipóteses na UC, considerando o recorte amostral e as evidências independentes necessárias em cada linha.')
  x<-monitora_v306_termos(x)
  c(gsub('p.p..','p.p.',x,fixed=TRUE),'<!-- monitora-v306-revisado -->')
}
monitora_v306_tabela <- function(x,id) {
  i<-grep(paste0('monitora-tabela ',id,' incluida'),x,fixed=TRUE)
  if(!length(i))return(data.table::data.table())
  a<-which(seq_along(x)>i[1]&startsWith(x,'|'))[1];if(is.na(a))return(data.table::data.table())
  b<-a;while(b<length(x)&&startsWith(x[b+1L],'|'))b<-b+1L
  split<-function(s){v<-strsplit(s,'|',fixed=TRUE)[[1]];trimws(v[-1L])}
  h<-split(x[a]);rows<-x[seq.int(a,b)];rows<-rows[!grepl('^[| :\\-]+$',rows)]
  if(length(rows)<2L)return(data.table::data.table())
  v<-lapply(rows[-1L],split);if(any(lengths(v)!=length(h)))return(data.table::data.table())
  z<-data.table::as.data.table(do.call(rbind,v));data.table::setnames(z,h);z
}

monitora_v306_nota_esforco <- function() 'NA identifica indicadores não calculados no primeiro ano, por ausência de ano observado anterior para comparação. Primeira amostragem (%) usa o total de UAs do ano atual; manutenção (%) usa o total do ano observado anterior. Não reamostradas são relativas à comparação anterior, não todas as UAs historicamente ausentes'
monitora_v306_notas_finais <- function(x) {
  x<-strsplit(paste(x,collapse='\n'),'\n',fixed=TRUE)[[1]]
  j<-grep('^<!-- monitora-tabela esforco-percentuais incluida -->$',x)
  if(!length(j))return(x)
  x<-x[!grepl('^Nota da \\[\\[tabela:esforco-percentuais\\]\\]:.*monitora-nota-automatica',x)]
  j<-grep('^<!-- monitora-tabela esforco-percentuais incluida -->$',x)
  a<-which(seq_along(x)>j[1]&startsWith(x,'|'))[1];if(is.na(a))return(x)
  b<-a;while(b<length(x)&&startsWith(x[b+1L],'|'))b<-b+1L
  monitora_v306_apos(x,b,paste0('Nota da [[tabela:esforco-percentuais]]: ',monitora_v306_nota_esforco(),'. <!-- monitora-nota-automatica -->'))
}
monitora_v306_catalogo_base <- monitora_relatorios_analiticos_catalogo_tabelas
monitora_relatorios_analiticos_catalogo_tabelas <- function() {
  z<-monitora_v306_catalogo_base();stats::setNames(monitora_v306_termos(unname(z)),names(z))
}
