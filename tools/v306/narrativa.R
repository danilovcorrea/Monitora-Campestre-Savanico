# Metadados de apresentação derivados dos objetos já calculados; não alteram ajustes.
monitora_v306_gravar <- function(x,n,p) data.table::fwrite(x,file.path(p,paste0('mv_',n,'.csv')),bom=TRUE,na='')
monitora_v306_meta_painel <- function(w,es,blocos,p) {
  monitora_v306_gravar(es,'painel_recorte',p)
  z<-data.table::melt(data.table::copy(w),id.vars=c('UC','UA','form_veg'),measure.vars=c('nativa','exotica','seca_morta','serrapilheira','solo_nu'),variable.name='indicador',value.name='delta_pp')
  z<-z[,.(UAs=data.table::uniqueN(paste(UC,UA)),delta_medio_pp=mean(delta_pp),minimo_pp=min(delta_pp),maximo_pp=max(delta_pp)),by=.(form_veg,indicador)]
  monitora_v306_gravar(z,'mudancas_observadas',p)
  z<-data.table::rbindlist(lapply(names(blocos),function(k)data.table::data.table(bloco=k,variavel=blocos[[k]],varia=vapply(w[,blocos[[k]],with=FALSE],function(x)is.finite(stats::sd(x))&&stats::sd(x)>1e-8,logical(1)))))
  monitora_v306_gravar(z,'blocos_variaveis',p)
}
monitora_v306_meta_ordenacao <- function(s,Y,p) {
  monitora_v306_gravar(data.table::data.table(eixo=seq_along(s$d),parcela_ajustada_pct=100*s$d^2/sum(s$d^2),variacao_total_pct=100*s$d^2/sum(Y^2)),'ord_eixos',p)
  monitora_v306_gravar(data.table::data.table(indicador=colnames(Y),Eixo1=s$v[,1],Eixo2=s$v[,2]),'ord_cargas',p)
}
monitora_v306_meta_trajetoria <- function(pc,w,ww,centro,p) {
  monitora_v306_gravar(data.table::data.table(eixo=seq_along(pc$sdev),variancia_pct=100*pc$sdev^2/sum(pc$sdev^2),coletas_universo=nrow(w),UAs_universo=data.table::uniqueN(paste(w$UC,w$UA))),'traj_eixos',p)
  ids<-unique(ww[,.(UC,UA,form_veg)]);monitora_v306_gravar(ids,'traj_populacao',p)
  yy<-c('nativa','exotica','seca_morta','serrapilheira','solo_nu')
  z<-ww[,lapply(.SD,mean),by=.(form_veg,ano_num),.SDcols=yy]
  monitora_v306_gravar(data.table::melt(z,id.vars=c('form_veg','ano_num'),variable.name='indicador',value.name='cobertura_media_pct'),'traj_coberturas',p)
}
monitora_v306_ler <- function(p,n) {
  f<-file.path(p,paste0(n,'.csv'));if(!file.exists(f)||file.info(f)$size==0)return(data.table::data.table())
  data.table::fread(f,encoding='UTF-8')
}
monitora_v306_num <- function(x,d=1L) ifelse(is.finite(x),formatC(x,format='f',digits=d,decimal.mark=','),'não disponível')
monitora_v306_ind <- function(x) {
  m<-c(nativa='plantas nativas',exotica='plantas exóticas',seca_morta='vegetação seca/morta',serrapilheira='material botânico (categoria geral)',solo_nu='solo/rochas')
  unname(m[as.character(x)])
}
monitora_v306_bloco <- function(x) ifelse(x=='Epoca','Época',as.character(x))
monitora_v306_ref_fig <- function(nome) paste0('[[figura:fig-',substr(digest::digest(paste0('figuras/',nome,'.png'),algo='sha256',serialize=FALSE),1,16),']]')
monitora_v306_secao <- function(x,p) {
  ler<-function(n)monitora_v306_ler(p,n);num<-monitora_v306_num
  paragrafos<-function(z)as.vector(rbind(z,rep('',length(z))))
  cabe<-grep('^## ',x);partes<-list();intro<-if(length(cabe))head(x,cabe[1]-1L)else x
  if(length(cabe))for(i in seq_along(cabe))partes[[sub('^## ','',x[cabe[i]])]]<-x[cabe[i]:if(i<length(cabe))cabe[i+1]-1L else length(x)]
  rec<-partes[['Recorte temporal e comparabilidade']];assoc<-partes[['Associação multivariada exploratória']];traj<-partes[['Trajetória da série e anos intermediários']]
  # Reentrância: a entrada já revisada não recebe uma segunda camada narrativa.
  if('Recortes e populações analisadas'%in%names(partes))return(x)
  intro<-c('# Análise multivariada integrada da cobertura vegetal','',
    'O objetivo é investigar quanto das diferenças observadas na cobertura vegetal está associado à época de amostragem, ao clima e aos registros de fogo, considerando também formação e posição espacial. A análise distingue associação descritiva de contribuição causal; a disponibilidade do desenho amostral determina o alcance da conclusão.','',
    'Duas perguntas complementares organizam esta seção: a associação entre fatores e mudanças do início ao fim de um recorte comparável; e o percurso das coberturas nos anos intermediários. A ordenação da parcela ajustada e a trajetória das coberturas observadas têm universos e eixos próprios, que não devem ser comparados numericamente entre figuras ou entre UCs.','')
  if(is.null(rec))rec<-c('## Recortes e populações analisadas','',x[!grepl('^#|Uma parcela compartilhada|As próximas investigações|Referências:',x)])else rec[1]<-'## Recortes e populações analisadas'
  painel<-ler('mv_painel_recorte');mud<-ler('mv_mudancas_observadas');aj<-ler('multivariada_associacoes_blocos_descritivas')
  ids<-ler('mv_traj_populacao');ct<-ler('multivariada_trajetoria_painel_comum');te<-ler('mv_traj_eixos');obs<-ler('mv_traj_coberturas')
  tem_ord<-any(grepl('multivariada_ordenacao_exploratoria.png',x,fixed=TRUE));tem_traj<-any(grepl('multivariada_trajetoria_eixos_comuns.png',x,fixed=TRUE))
  if(nrow(painel)&&length(assoc)) {
    rec<-c(rec,'**Mudanças observadas no painel selecionado.** As médias abaixo descrevem exatamente as UAs usadas no ajuste integrado. São diferenças em pontos percentuais entre os extremos, não testes de significância. A conclusão inferencial deve ser lida com os contrastes pareados da seção de resultados temporais, respeitando seus próprios pares e períodos. Nenhum indicador foi selecionado apenas por ter resultado significativo.','')
    if(nrow(mud))for(f in unique(mud$form_veg)) {
      q<-mud[form_veg==f];txt<-paste0(monitora_v306_ind(q$indicador),': ',num(q$delta_medio_pp),' p.p.')
      rec<-c(rec,paste0('- ',monitora_relatorio_rotulo_formacao(f,TRUE),' (',q$UAs[1],' UAs; ',painel$inicio[1],'–',painel$fim[1],'): ',paste(txt,collapse='; '),'.'))
    }
    rec<-c(rec,'','Diferenças positivas indicam aumento médio e negativas, redução média; valores próximos de zero podem coexistir com alterações opostas entre UAs. Equivalência, tendência e atribuição a um fator não decorrem dessas médias.','')
  }
  resumo_aj<-character();sensibilidade<-character();traj_conclusao<-character()
  if(length(assoc)) {
    assoc[1]<-'## Associação multivariada das mudanças entre extremos'
    assoc<-assoc[!startsWith(assoc,'No recorte observado,')]
    if(nrow(aj)) {
      aa<-aj[escala=='pontos_percentuais'];total<-aa[bloco=='Conjunto'];bb<-aa[bloco!='Conjunto'];ss<-aj[escala=='padronizada_sensibilidade' & bloco!='Conjunto']
      if(nrow(total))resumo_aj<-c(resumo_aj,paste0('A [[tabela:multivariada-ajustes]] mostra ajuste conjunto de ',num(100*total$R2_descritivo[1]),'% da variação entre UAs nas mudanças dos cinco indicadores. Os ',num(100*(1-total$R2_descritivo[1])),'% restantes não são representados pelo conjunto neste recorte. Ambos são resultados dentro da amostra, sem validação causal.'))
      for(bl in c('Epoca','Clima','Fogo','Contexto')) {
        q<-bb[bloco==bl]
        if(nrow(q))resumo_aj<-c(resumo_aj,paste0('- ',monitora_v306_bloco(bl),': associação isolada de ',num(100*q$R2_descritivo[1]),'%; incremento exclusivo de ',num(100*q$incremento_exclusivo[1]),'% após considerar os demais blocos.',if(q$posto[1]==0)' Os preditores deste bloco são constantes no painel; ausência de incremento não demonstra ausência de efeito.'else''))
        else resumo_aj<-c(resumo_aj,paste0('- ',monitora_v306_bloco(bl),': contribuição não disponível neste ajuste; sua ausência não foi convertida em efeito nulo.'))
      }
      resumo_aj<-c(resumo_aj,'Os percentuais isolados não se somam: os blocos compartilham informação. O incremento exclusivo mede a perda de ajuste ao retirar o bloco, mantendo os demais. Clima reúne chuva, temperatura e umidade; fogo reúne modalidades com informação disponível. A tabela não separa causalmente cada variável dentro desses blocos. Contexto representa formação e posição quando variáveis. Um incremento exclusivo pode superar a associação isolada, porque considerar os demais preditores altera a projeção; essas medidas não formam uma decomposição aditiva em parcelas positivas.','')
      if(nrow(bb)&&nrow(ss)&&any(bb$incremento_exclusivo>1e-8)&&any(ss$incremento_exclusivo>1e-8)) {
        top<-bb$bloco[which.max(bb$incremento_exclusivo)];top2<-ss$bloco[which.max(ss$incremento_exclusivo)]
        sensibilidade<-paste0('Na escala original, o maior incremento exclusivo é de ',monitora_v306_bloco(top),'; após padronizar a dispersão dos indicadores, é de ',monitora_v306_bloco(top2),'. ',if(top!=top2)'A mudança de ordem indica sensibilidade à escala das respostas; não há um fator predominante estável entre as duas apresentações.'else'A manutenção do primeiro bloco nas duas escalas é uma consistência descritiva, não comprovação de predominância causal.');resumo_aj<-c(resumo_aj,sensibilidade)
      }else resumo_aj<-c(resumo_aj,'Não há suporte para destacar um bloco predominante nas duas escalas; ajustes ausentes ou nulos não demonstram ausência de influência dos fatores.')
    }
    j<-grep('^<figure.*multivariada_ordenacao_exploratoria',assoc)
    if(length(j)) {
      eo<-ler('mv_ord_eixos');co<-ler('mv_ord_cargas');ref<-monitora_v306_ref_fig('multivariada_ordenacao_exploratoria')
      antes<-c(paste0('**Como ler a ',ref,'.** Cada ponto é uma UA, posicionada pela parcela ajustada das diferenças entre os extremos. Proximidade indica semelhança nessa parcela ajustada; não significa coberturas totais iguais nem ausência de mudança. As cores identificam formações, que também podem integrar o bloco Contexto.'),'')
      depois<-character()
      if(nrow(eo)>=2L)depois<-c(depois,paste0('Os dois primeiros eixos representam ',num(sum(eo$parcela_ajustada_pct[1:2]),2),'% da parcela ajustada (',num(eo$parcela_ajustada_pct[1],2),'% e ',num(eo$parcela_ajustada_pct[2],2),'%), equivalentes a ',num(sum(eo$variacao_total_pct[1:2])),'% da variação total das diferenças. A figura resume apenas essa projeção; não substitui o ajuste conjunto da tabela.'))
      if(nrow(co))for(k in c('Eixo1','Eixo2')) {
        z<-co[order(-abs(get(k)))][1:min(2,.N)];depois<-c(depois,paste0(sub('Eixo','Eixo ',k),': os maiores pesos absolutos são de ',paste(paste0(monitora_v306_ind(z$indicador),' (',num(z[[k]],2),')'),collapse=' e '),'. Os sinais orientam a leitura nesta figura; não representam benefício, dano ou efeito do fator explicativo.'))
      }
      pts<-ler('multivariada_ordenacao_exploratoria')
      if(nrow(pts)) {
        for(f in unique(pts$Formacao)) {
          q<-pts[Formacao==f];depois<-c(depois,paste0(f,': ',nrow(q),' UAs; centro dos escores (',num(mean(q$Eixo1)), '; ',num(mean(q$Eixo2)),'); amplitudes ',num(min(q$Eixo1)),' a ',num(max(q$Eixo1)),' no eixo 1 e ',num(min(q$Eixo2)),' a ',num(max(q$Eixo2)),' no eixo 2.'))
        }
        depois<-c(depois,'Essas posições e amplitudes descrevem heterogeneidade do ajuste; não delimitam grupos testados. Uma separação visual por formação não seria evidência independente do efeito da formação, pois ela participa do próprio ajuste quando disponível.')
      }
      assoc<-c(head(assoc,j[1]-1L),paragrafos(resumo_aj),paragrafos(antes),assoc[j[1]],'',paragrafos(depois),tail(assoc,length(assoc)-j[1]))
    }else assoc<-c(assoc,paragrafos(resumo_aj),'A ordenação em dois eixos não foi produzida nesta execução. O resultado disponível não será ilustrado por uma figura antiga.','')
  }
  if(length(traj)) {
    traj[1]<-'## Trajetórias das coberturas nos anos intermediários'
    traj<-gsub('A cobertura proporcional de cada eixo resume dispersão geométrica','A proporção da variância representada por cada eixo resume dispersão geométrica',traj,fixed=TRUE)
    fig<-grep('^<figure',traj);texto<-traj[-c(1L,fig)]
    antes<-character();depois<-character()
    if(tem_traj&&nrow(ids)&&nrow(ct)&&nrow(te)) {
      n<-nrow(ids);total<-if(nrow(painel))painel$UAs[1]else NA_real_
      antes<-c(paste0('A ',monitora_v306_ref_fig('multivariada_trajetoria_eixos_comuns'),' apresenta médias anuais de ',n,' UAs constantes por formação: ',paste(vapply(unique(ct$form_veg),function(f){q<-ct[form_veg==f];paste0(monitora_relatorio_rotulo_formacao(f,TRUE),' — ',q$UAs[1],' UAs, ',min(q$ano_num),'–',max(q$ano_num),' (',nrow(q),' anos observados)')},character(1)),collapse='; '),'.'))
      if(is.finite(total))antes<-c(antes,paste0('Em número de UAs, esse painel corresponde a ',num(100*n/total),'% das ',total,' UAs do ajuste entre extremos. Os períodos e critérios dos dois painéis devem ser considerados antes de relacionar seus resultados.'))
      antes<-c(antes,paste0('Os eixos foram calculados com ',te$coletas_universo[1],' coletas completas e elegíveis de ',te$UAs_universo[1],' UAs; as linhas representam somente o subconjunto constante descrito acima. Os eixos 1 e 2 representam ',num(te$variancia_pct[1]),'% e ',num(te$variancia_pct[2]),'% da variância desse universo. Não são os eixos da ordenação da parcela ajustada.'))
      cargas<-ler('multivariada_trajetoria_cargas')
      if(nrow(cargas))for(k in 1:2){z<-cargas[which.max(abs(get(paste0('PC',k))))];antes<-c(antes,paste0('O maior peso absoluto no eixo ',k,' é de ',monitora_v306_ind(z$indicador),' (',num(z[[paste0('PC',k)]],2),'). A análise mantém a escala original dos indicadores; maior dispersão pode dominar a ordenação.'))}
      for(f in unique(ct$form_veg)) {
        z<-data.table::copy(ct[form_veg==f]);data.table::setorder(z,ano_num);base<-z[1];dist_inicio<-sqrt((z$Eixo1-base$Eixo1)^2+(z$Eixo2-base$Eixo2)^2);jmax<-which.max(dist_inicio)
        traj_conclusao<-c(traj_conclusao,paste0(monitora_relatorio_rotulo_formacao(f,TRUE),': maior afastamento da posição inicial em ',z$ano_num[jmax],', com distância ',num(dist_inicio[jmax]),'; distância entre os extremos ',num(tail(dist_inicio,1)),'.'))
        depois<-c(depois,paste0(monitora_relatorio_rotulo_formacao(f,TRUE),': no plano exibido, a distância entre ',z$ano_num[1],' e ',tail(z$ano_num,1),' é ',num(tail(dist_inicio,1)),'. O maior afastamento da posição inicial é ',num(dist_inicio[jmax]),', observado em ',z$ano_num[jmax],'. As distâncias comparam posições; lacunas entre anos continuam sem conexão.'))
        z[,desloc:=sqrt((Eixo1-data.table::shift(Eixo1))^2+(Eixo2-data.table::shift(Eixo2))^2)];z[,anterior:=data.table::shift(ano_num)];z<-z[ano_num-anterior==1L & is.finite(desloc)]
        if(!nrow(z)){depois<-c(depois,paste0(monitora_relatorio_rotulo_formacao(f,TRUE),': não há anos consecutivos conectáveis; as lacunas permanecem abertas.'));next}
        z<-z[which.max(desloc)];txt<-paste0(monitora_relatorio_rotulo_formacao(f,TRUE),': o maior deslocamento entre anos consecutivos, no plano dos dois eixos, ocorreu em ',z$anterior,'–',z$ano_num,' (distância ',num(z$desloc),' em unidades dos escores).')
        if(nrow(obs)) {
          q<-merge(obs[form_veg==f & ano_num==z$anterior],obs[form_veg==f & ano_num==z$ano_num],by=c('form_veg','indicador'),suffixes=c('_antes','_depois'));q[,delta:=cobertura_media_pct_depois-cobertura_media_pct_antes];if(nrow(q))q<-q[order(-abs(delta))][seq_len(min(2,.N))]
          if(nrow(q))txt<-paste0(txt,' Nesse mesmo painel, as maiores diferenças médias de cobertura foram ',paste(paste0(monitora_v306_ind(q$indicador),': ',num(q$delta),' p.p.'),collapse='; '),'.')
        }
        depois<-c(depois,txt)
      }
      depois<-c(depois,'Os deslocamentos são descritivos e sua comparação usa somente os dois eixos exibidos. Retorno no gráfico não demonstra equivalência ecológica ou regeneração; afastamento não identifica a causa da mudança. Anos ausentes não são ligados e o sentido dos eixos não é comparável entre UCs.')
    }
    traj<-c(traj[1],'',texto,'',paragrafos(antes),traj[fig],'',paragrafos(depois))
  }
  conclusao<-c('## Discussão integrada e alcance dos achados','')
  if(nrow(painel)&&length(assoc)&&nrow(aj)) {
    conclusao<-c(conclusao,paste0('**Conclusão para este recorte.** Foram descritas as diferenças de cobertura de ',painel$UAs[1],' UAs entre ',painel$inicio[1],' e ',painel$fim[1],'. O ajuste conjunto permite comparar associações dos blocos com essas diferenças; não estima a fração da mudança causada por época, clima ou fogo.'))
    a<-aj[escala=='pontos_percentuais'&bloco=='Conjunto'];if(nrow(a))conclusao<-c(conclusao,paste0('A associação conjunta representa ',num(100*a$R2_descritivo[1]),'% da variação entre UAs; os incrementos exclusivos e a sensibilidade à padronização apresentados acima delimitam quais associações merecem investigação. Valores compartilhados não podem ser atribuídos integralmente a cada fator.'))
  }else conclusao<-c(conclusao,'**Conclusão para esta execução.** Não há ajuste integrado elegível que permita quantificar as associações dos blocos. Essa ausência não demonstra ausência de mudança da vegetação nem ausência de influência dos fatores.')
  if(length(sensibilidade))conclusao<-c(conclusao,sensibilidade)
  if(nrow(aj)&&length(assoc)){q<-aj[escala=='pontos_percentuais' & bloco!='Conjunto'];if(nrow(q))conclusao<-c(conclusao,paste0('Os incrementos exclusivos neste recorte são ',paste(paste0(monitora_v306_bloco(q$bloco),': ',num(100*q$incremento_exclusivo),'%'),collapse='; '),'. Eles quantificam associações condicionais no ajuste, não percentuais de cobertura causados por cada fator.'))}
  if(tem_traj)conclusao<-c(conclusao,'A trajetória acrescenta os anos intermediários e permite localizar alterações que a comparação dos extremos pode ocultar. Ela descreve coberturas observadas de um painel constante, enquanto a primeira ordenação descreve a parcela ajustada dos deltas; concordância visual entre elas não constitui validação independente.',traj_conclusao)
  conclusao<-c(conclusao,'**Alcance da inferência.** Os testes inferenciais integrados permanecem não estimáveis. Não se estabeleceu um esquema de permutação que preserve simultaneamente dependências de célula climática, evento de fogo, ano e UA. A seção não conclui que um fator causou a mudança ou foi mais importante causalmente.','',
    'O registro cartográfico de fogo empregado no ajuste, quando disponível, se refere ao ano e à posição explicitados no recorte; não comprova exposição durante todo o intervalo entre coletas. Clima compartilhado não ganha replicação independente pelo aumento do número de UAs. O calendário pode permanecer confundido com ano e espaço, mesmo com incremento descritivo no ajuste.','',
    'Para avançar da associação à explicação, é necessário confirmar a cronologia e modalidade dos episódios de fogo entre coletas, controles comparáveis, continuidade dos transectos, contraste de datas e suporte climático independente. Os resultados orientam essas verificações e a formulação de hipóteses para gestão, sem recomendar intervenção com base apenas na ordenação.','')
  c(intro,rec,'',assoc,'',traj,'',paragrafos(conclusao))
}
monitora_v306_editorial <- function(conteudo,dir_relatorio) {
  x<-strsplit(paste(conteudo,collapse='\n'),'\n',fixed=TRUE)[[1]]
  # O resumo executivo conserva a informação climática, com identificação inequívoca.
  x<-gsub('Clima e trajetória:','Clima — transições entre coletas:',x,fixed=TRUE)
  a<-which(x=='# Análise multivariada integrada da cobertura vegetal')
  if(length(a)==1L){b<-which(seq_along(x)>a & startsWith(x,'# '))[1];if(is.na(b))b<-length(x)+1L;x<-c(head(x,a-1L),monitora_v306_secao(x[a:(b-1L)],dir_relatorio),if(b<=length(x))x[b:length(x)]else character())}
  a<-which(x=='# Evidências, hipóteses e gestão')
  if(length(a)==1L) {
    b<-which(seq_along(x)>a & grepl('^#{1,2} ',x))[1];if(is.na(b))b<-length(x)+1L
    z<-x[(a+1L):(b-1L)];z<-z[!grepl('^- (Fogo:|Clima e trajetória:|Clima — transições entre coletas:|Calendário —)',z)]
    tem<-any(grepl('^<!-- monitora-tabela hipoteses-gestao incluida -->',z))
    intro<-if(tem)'A [[tabela:hipoteses-gestao]] reúne as evidências observadas, as hipóteses ecológicas compatíveis e suas possíveis implicações para a gestão. A interpretação considera as limitações de amostragem, calendário e informações sobre fogo e clima discutidas anteriormente; as associações observadas não demonstram, isoladamente, relações causais.'else character()
    x<-c(head(x,a),'',intro,'',z,if(b<=length(x))x[b:length(x)]else character())
  }
  gsub('p.p..','p.p.',x,fixed=TRUE)
}
