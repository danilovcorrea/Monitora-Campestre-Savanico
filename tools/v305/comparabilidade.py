"""Correção restrita às coberturas gerais harmonizadas; preserva metadado histórico."""
def revisar(module):
 def troca(a,b,n=1):
  nonlocal module
  assert module.count(a)==n,(a[:120],module.count(a),n)
  module=module.replace(a,b)
 troca(' || anyNA(z$protocolo_origem)','')
 troca('if(any(z[,data.table::uniqueN(paste(form_veg,protocolo_origem)),by=UA]$V1!=1L))fail("Mudança de formação/protocolo dentro da UA.")','if(any(z[,data.table::uniqueN(form_veg),by=UA]$V1!=1L))fail("Mudança de formação dentro da UA.")')
 troca('    if(any(z[,data.table::uniqueN(protocolo_origem),by=.(bloco,form_veg)]$V1!=1L))fail("Protocolos distintos dentro do contraste.")\n','')
 troca('!grupo_data_incerta & !is.na(protocolo_origem) & protocolo_origem==protocolo_anterior &','!grupo_data_incerta &')
 troca('      !is.na(protocolo_origem) & nzchar(protocolo_origem) & is.finite(cobertura_percentual) &','      is.finite(cobertura_percentual) &')
 troca('if(nlevels(x$protocolo_origem)>1L)"protocolo_origem",','')
 troca('    if(nlevels(x$protocolo_origem)>1L)motivos<-c(motivos,"harmonização de protocolos distintos não documentada para o ajuste conjunto")\n','')
 a=module.index('      protocolos_com_suporte<-');b=module.index('      fo<-stats::reformulate(terms)',a)
 module=module[:a]+'''      te<-te[as.character(te$UA)%in%as.character(tr$UA),,drop=FALSE]
      motivo<-if(!nrow(te))"NE: UA inédita no treino"else if(length(unique(tr$ANO))<3L)"NE: menos de três anos de treino"else"avaliado; uso exclusivamente exploratório"
      folds[[length(folds)+1L]]<-data.table::data.table(form_veg=g$form_veg,indicador=g$indicador,ano_teste=ano,n_total=total,n_comparavel=nrow(te),status=motivo)
      if(startsWith(motivo,"NE"))next
      tr$UA<-droplevels(tr$UA);te$UA<-factor(te$UA,levels=levels(tr$UA))
      terms<-c(if(nlevels(tr$UA)>1L)"UA","seno","cosseno")
'''+module[b:]
 troca('completo==TRUE & protocolos==1L & formacoes==1L','completo==TRUE & formacoes==1L')
 troca('mesmas UAs/formação; rótulo de protocolo idêntico; <=100 m; uma coleta/ano; respostas completas','mesmas UAs/formação; categorias gerais harmonizadas; <=100 m; uma coleta/ano; respostas completas')
 troca('Rótulos de protocolo diferentes indicam necessidade de harmonização documentada, não incompatibilidade comprovada.','O rótulo histórico do formulário é preservado na linhagem e não separa as categorias gerais harmonizadas da base aprovada.')
 troca('protocolos!=1L|is.na(protocolo)|!nzchar(protocolo),"Protocolo de origem não unívoco",','')
 troca('  # Formação e versão constantes por UA são requisitos de comparabilidade local.\n  comp <- d[,.(protocolos=data.table::uniqueN(protocolo)),by=UA]\n  if(any(comp$protocolos>1L)){out[,motivo:="Mudança de protocolo: requer análise por versão; não combinar automaticamente"];return(out)}\n','')
 troca(' & protocolo==protocolo_anterior &',' &')
 troca('    protocolo_origem==protocolo_origem_anterior & form_veg==form_veg_anterior','    form_veg==form_veg_anterior')
 troca('    protocolo_origem!=protocolo_origem_anterior,"Protocolos diferentes; harmonização não documentada",\n','')
 # A PCA continua usando as cinco categorias gerais; população comum por formação.
 a=module.index('      # Uma mesma UA fornece todos os anos');b=module.index('        f2<-file.path(dir_figuras,"multivariada_trajetoria',a)
 z=module[a:b].replace(',protocolo_origem','').replace('protocolo_origem,','').replace('"protocolo_origem",','')
 z=z.replace('centro[,painel:=paste(form_veg,sep=" · ")]','centro[,painel:=form_veg]')
 z='\n'.join(l for l in z.split('\n') if 'protocolo_rotulo:='not in l)
 z=z.replace('painel de protocolo/formação','painel de formação').replace('nem misturar versões em uma trajetória','na trajetória')
 module=module[:a]+z+module[b:]
 troca('dentro da formação e do protocolo','dentro da formação')
 troca('versões diferentes e anos ausentes não são ligados','anos ausentes não são ligados; o rótulo histórico do formulário não divide as categorias gerais harmonizadas')
 troca('dentro da mesma formação e protocolo','dentro da mesma formação')
 troca('mantendo lacunas e protocolos separados','preservando lacunas e a continuidade espacial')
 troca('mudanças de protocolo, formação ou célula e anos ausentes não são atravessados','mudanças de formação ou célula e anos ausentes não são atravessados')
 troca('preservou simultaneamente protocolo, formação, célula, data e elegibilidade','preservou simultaneamente formação, célula, data e elegibilidade')
 # Metadado não determina a validade do desenho de fogo; chaves de resposta já auditadas.
 a=module.index('    pr<-data.table::as.data.table(base)');b=module.index('    z<-merge(d,r,',a)
 module=module[:a]+module[b:]
 for a,b in {
  'perdas por protocolo/UA inéditos':'perdas por UAs inéditas ou suporte insuficiente',
  'mesma formação/protocolo':'mesma formação e categorias gerais harmonizadas',
  'UA, ano, protocolo e seno/cosseno':'UA, ano e seno/cosseno',
  'Sem harmonização documentada, protocolos distintos bloqueiam o ajuste conjunto. ':'',
  'modelo de UA, protocolo e calendário':'modelo de UA e calendário',
  'Exige ao menos três anos de treino com o mesmo rótulo de protocolo do ano de teste; O treino fica restrito ao mesmo protocolo do ano de teste; UAs inéditas não são previstas. Rótulos distintos não provam incompatibilidade, mas exigem harmonização documentada antes de ampliar esse recorte.':'Exige ao menos três anos de treino; UAs inéditas não são previstas. O rótulo histórico não restringe as categorias gerais harmonizadas da base aprovada.',
  'mesma UA/formação, mesmo protocolo de origem observado, mesma célula':'mesma UA/formação, categorias gerais harmonizadas, mesma célula',
  'Transições preservam protocolo e formação':'Transições preservam formação e categorias gerais harmonizadas',
  'trajetória mantém formação, protocolo e painel comum':'trajetória mantém formação, categorias gerais harmonizadas e painel comum'
 }.items():troca(a,b)
 return module
