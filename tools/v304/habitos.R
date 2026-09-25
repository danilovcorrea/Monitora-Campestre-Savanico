monitora_correcao_habito_alvos <- function(dt, categoria, forma, linhas) {
  forma <- monitora_correcao_normalizar_forma_habito(forma)
  if (!forma %in% monitora_correcao_tokens_formas_exigem_habito()) return(rep(FALSE,length(linhas)))
  monitora_correcao_habito_requerido_por_linha(dt,categoria,forma,linhas_com_forma=linhas)[linhas]
}
monitora_correcao_movimento_lote_por_linha <- function(dt, op, linhas, origem, destino, forma, presenca, chaves, arquivo, dicionario) {
  original <- data.table::copy(dt); trabalho <- data.table::copy(dt)
  audits <- list(); afs <- list(); atingidas <- integer()
  get1 <- function(n) if(n %in% names(op)) as.character(op[[n]][1L]) else NA_character_
  for (kk in seq_along(linhas)) {
    rr <- linhas[kk]
    formas <- names(presenca)[vapply(presenca,function(v)isTRUE(v[kk]),logical(1L))]
    if(identical(origem,destino)) formas <- setdiff(formas,forma)
    if(!length(formas)) next
    col <- monitora_correcao_coluna_forma_vida(trabalho,origem)
    atuais <- monitora_correcao_tokenizar(trabalho[[col]][rr])
    remover <- atuais[monitora_correcao_forma_canonica_lote(atuais) %in% formas]
    if(!length(remover)) next
    hab <- get1('habito_escolhido')
    if(!any(monitora_correcao_habito_alvos(trabalho,destino,forma,rr))) hab <- NA_character_
    uma <- monitora_correcao_criar_operacao(get1('id_correcao'),get1('responsavel'),'movimento_forma_vida_atomico',get1('ordem_operacao'),
      'uuid_registro',as.character(trabalho[[chaves$coleta]][rr]),
      uuid_registro=as.character(trabalho[[chaves$uuid_registro]][rr]),
      ponto_amostral=as.character(trabalho[[chaves$ponto_amostral]][rr]),
      atributo='__mover_forma_vida__',acao='mover_forma_vida',valor_original=as.character(trabalho[[col]][rr]),valor_novo=forma,
      n_esperado=1L,n_alvo=1L,motivo=get1('motivo'),token_pai=formas[1L],categoria_origem=origem,categoria_destino=destino,
      token_removido=monitora_correcao_colapsar_lista_serializada(remover),habito_escolhido=hab)
    uma <- monitora_correcao_anexar_contexto_operacao(uma,trabalho,rr,chaves)
    res <- monitora_correcao_aplicar_movimento_forma_vida_atomico(trabalho,uma,chaves,NULL,arquivo,dicionario)
    if(isTRUE(res$falha)) {
      res$dt <- original;res$linhas<-integer();res$afetacoes<-data.table::data.table(linha_indice=integer(),atributo=character())
      res$audit[,mensagem:=paste0('Lote cancelado integralmente: ',mensagem)]
      return(res)
    }
    trabalho<-res$dt;audits[[length(audits)+1L]]<-res$audit;afs[[length(afs)+1L]]<-res$afetacoes;atingidas<-c(atingidas,res$linhas)
  }
  audit<-data.table::rbindlist(audits,fill=TRUE);af<-data.table::rbindlist(afs,fill=TRUE)
  if(nrow(audit))audit[,`:=`(formas_origem=paste(names(presenca),collapse=' | '),forma_destino=forma,modo_movimento='lote_atomico_por_registro')]
  list(dt=trabalho,audit=audit,afetacoes=unique(af),linhas=unique(atingidas),falha=FALSE)
}
