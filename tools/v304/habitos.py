change('monitora_correcao_tokens_formas_exigem_habito <-', (root/'tools/v304/habitos.R').read_text()+'\nmonitora_correcao_tokens_formas_exigem_habito <-')
# Dois motores atômicos: preencher apenas linhas cujo protocolo exige hábito.
for name,endname,forma in [('monitora_correcao_aplicar_movimento_forma_vida_atomico','monitora_correcao_aplicar_movimento_forma_vida_atomico__v291','forma_destino'),('monitora_correcao_aplicar_triagem_desconhecida_atomica','monitora_correcao_aplicar_triagens_desconhecida_atomicas','forma')]:
 a=s.index(name+' <- function(');b=s.index(endname+' <-',a);p=s[a:b]
 old=f'  forma_exige_habito <- monitora_correcao_limpar_texto({forma}) %in% monitora_correcao_tokens_formas_exigem_habito()'
 assert old in p
 p=p.replace(old,f'  linhas_habito <- linhas[monitora_correcao_habito_alvos(dt, destino, {forma}, linhas)]\n  forma_exige_habito <- length(linhas_habito) > 0L')
 x=p.index('  atualizacao_habito <- NULL');y=p.index('\n  } else if (nzchar(habito))',x)
 q=p[x:y].replace('[[dep_destino]][linhas]','[[dep_destino]][linhas_habito]').replace('length(linhas)','length(linhas_habito)').replace('idx = linhas[mudou_hab]','idx = linhas_habito[mudou_hab]')
 p=p[:x]+q+p[y:]
 # No intralista, a forma já existente no destino conserva seus dependentes.
 if forma=='forma_destino':
  p=p.replace('  tokens_origem_canon <-', '  if (identical(origem,destino)) tokens_remover <- tokens_remover[monitora_correcao_forma_canonica_lote(tokens_remover) != monitora_correcao_forma_canonica_lote(forma_destino)]\n  tokens_origem_canon <-',1)
  p=p.replace('    for (li in linhas) {','    for (li in linhas) {\n      if (!any(monitora_correcao_habito_alvos(dt, origem, tok, li))) next',1)
 s=s[:a]+p+s[b:]
# Wrapper preserva espécie histórica que partilha nome físico com hábito moderno.
a=s.index('monitora_correcao_aplicar_movimento_forma_vida_atomico <- function(',s.index('monitora_correcao_aplicar_movimento_forma_vida_atomico__v291 <-'));b=s.index('monitora_correcao_aplicar_movimentos_forma_vida_atomicos <-',a);p=s[a:b]
p=p.replace('  plano <- list()','  if (identical(origem,destino)) tokens_origem <- setdiff(tokens_origem, forma_destino)\n  plano <- list()',1)
x=p.index('    deps_origem <- deps_origem[!vapply');y=p.index('    if (!length(deps_origem))',x);p=p[:x]+p[y:]
p=p.replace('      rr <- linhas[kk]\n      valor_origem', '      rr <- linhas[kk]\n      eh_habito <- monitora_correcao_coluna_habito_segura(original,dep_origem,origem,tok_origem)\n      if (eh_habito && any(monitora_correcao_habito_alvos(original,origem,tok_origem,rr))) next\n      valor_origem',1)
p=p.replace('        original, origem, destino, forma_destino, dep_origem, dicionario','        original, origem, destino, forma_destino, dep_origem, dicionario, linha = rr',1)
s=s[:a]+p+s[b:]
# Resolver legado nunca escolhe campo de hábito para receber texto de espécie.
change('forma, coluna_origem, dicionario = NULL) {','forma, coluna_origem, dicionario = NULL, linha = NULL) {')
a=s.index('monitora_correcao_resolver_dependente_destino_lote <-');i=s.index('  mapa <-',a)
s=s[:i]+'''  if (!is.null(linha) && identical(monitora_correcao_normalizar_forma_habito(forma),"samambaia") &&
      !any(monitora_correcao_habito_alvos(dt,origem,forma,linha)) &&
      monitora_correcao_coluna_habito_segura(dt,coluna_origem,origem,forma)) {
    alvo <- paste0("amostragem/registro/forma_vida_",destino,"_samambaia_sp")
    col <- monitora_validados_resolver_coluna(names(dt),alvo)
    if (!is.na(col) && col %in% names(dt) && !monitora_correcao_coluna_habito_segura(dt,col,destino,forma)) return(col)
    return(NA_character_)
  }
'''+s[i:]
# Lote normal compartilha a transação e o plano de descritores do motor atômico.
a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico <- function(');a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico <- function(',a+1)
b=s.index('\n}\n',a)+3;p=s[a:b];x=p.index('  habito_destino <-')
p=p[:x]+'''  monitora_correcao_movimento_lote_por_linha(dt,linha_lote,linhas,origem,destino,forma_destino,presenca,chaves,arquivo_correcao,dicionario)
}
''';s=s[:a]+p+s[b:]
# Pré-triagem considera o protocolo da linha; contador inclui bloqueios por hábito.
change('      forma_condicional <- forma_val %in% monitora_correcao_tokens_formas_exigem_habito()', '      forma_condicional <- any(monitora_correcao_habito_alvos(dt,destino_val,forma_val,linhas_globais))')
change('        exige_habito <- c(exige_habito, forma_val)\n        next','        exige_habito <- c(exige_habito, forma_val)\n        bloqueadas <- bloqueadas + 1L\n        next')
change('length(unique(exige_habito)), " forma(s) que exigem hábito.', 'length(exige_habito), " registro(s) que exigem hábito pelo protocolo.')
# Seletores do servidor usam coletas selecionadas, nunca a versão de toda a base.
change('  server <- function(input, output, session) {', '''  server <- function(input, output, session) {
    monitora_painel_forma_exige_habito <- function(forma, linhas = NULL, categoria = "nativa") {
      forma <- monitora_painel_canonizar_forma_habito(forma)
      if (!nzchar(forma) || !forma %in% MONITORA_TRIAGEM_FORMAS_CONDICIONAIS) return(FALSE)
      if (is.null(linhas)) {
        coletas <- if (monitora_painel_usar_lote_coletas()) input$coletas_lote else input$coleta
        linhas <- if (!is.na(chaves$coleta)) which(as.character(dt[[chaves$coleta]]) %in% as.character(coletas)) else integer()
      }
      if(!length(linhas)) return(FALSE)
      any(monitora_correcao_habito_alvos(dt,categoria,forma,linhas))
    }''')
# Seleção deve atualizar também quando o usuário troca coleta/lote mantendo a forma.
change('    shiny::observeEvent(input$mv_forma_destino, {','    shiny::observeEvent(list(input$mv_forma_destino,input$coleta,input$coletas_lote,input$escopo_coletas), {')
# Preview movimento usa o mesmo aplicador já invocado pela validação semântica.
# Os ramos de replay de desconhecida recebem obrigação antes da primeira mutação.
a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico__v262 <-');a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico <- function(',a);b=s.index('\n}\n',a)+3;p=s[a:b]
x=p.index('  tokens_desc <-')
p=p[:x]+'''  linhas_habito <- linhas_alvo[monitora_correcao_habito_alvos(dt,destino,forma_destino,linhas_alvo)]
  habito <- if("habito_escolhido" %in% names(linha_lote)) as.character(linha_lote$habito_escolhido[1L]) else ""
  if(length(linhas_habito)) {
    if(is.na(habito) || !habito %in% c("terrestre","epifita","rupicola")) return(falhar("falha_habito_obrigatorio","informe hábito válido para o protocolo das linhas de destino"))
    dep_habito <- monitora_correcao_resolver_coluna_habito(dt,destino,forma_destino,linha=linhas_habito[1L])
    if(!monitora_correcao_coluna_habito_segura(dt,dep_habito,destino,forma_destino)) return(falhar("falha_coluna_habito_destino","destino de hábito não localizado"))
  }
'''+p[x:]
x=p.index('  res_desc_dep <-')
p=p[:x]+'''  if(length(linhas_habito)) {
    antes_hab <- as.character(dt[[dep_habito]][linhas_habito])
    mudou_hab <- monitora_correcao_na_para_vazio(antes_hab) != habito
    if(any(mudou_hab)) {
      idx <- linhas_habito[mudou_hab];data.table::set(dt,i=idx,j=dep_habito,value=habito)
      audit <- data.table::rbindlist(list(audit,registrar("aplicada_atomica","Hábito aplicado conforme protocolo do registro",dep_habito,idx,antes_hab[mudou_hab],habito)),fill=TRUE)
      afetacoes <- data.table::rbindlist(list(afetacoes,data.table::data.table(linha_indice=idx,atributo=dep_habito)),fill=TRUE)
    }
  }
'''+p[x:];s=s[:a]+p+s[b:]
# Preview usa o mesmo aplicador do replay, inclusive máscaras históricas.
a=s.index('            vals_o <- as.character(xprev[[col_o]][linhas_p])');b=s.index('          }',s.index('monitora_correcao_recalcular_tipo_forma_vida(xprev, linhas_p)',a))
# Fim do bloco da operação: inclui qualquer try/silent da chamada final.
p=s[a:b]
# A última operação da versão-base é a atualização de Encostam.
last=p.rfind('\n');assert 'hab_p' in p
s=s[:a]+'''            res_preview <- monitora_correcao_aplicar_movimento_forma_vida_lote_atomico(data.table::copy(xprev),op_mvd,chaves,dicionario=dicionario_painel,gravar_relatorio_ambiguidades=FALSE)
            if (isTRUE(res_preview$falha)) stop(paste(res_preview$audit$mensagem,collapse="; "),call.=FALSE)
            xprev <- res_preview$dt
'''+s[b:]
change('      shiny::updateSelectInput(session, input_id, choices = MONITORA_TRIAGEM_HABITO_NAO_APLICA_CHOICES, selected = "")','      shiny::updateSelectInput(session, input_id, choices = c("(não exigido para esta forma/protocolo)"=""), selected = "")')
# Contrato de edição fora do servidor recebe explicitamente a base e as linhas.
change('monitora_painel_detalhar_forma_vida_contratual <- function(atributo, acao, valor_novo)', 'monitora_painel_detalhar_forma_vida_contratual <- function(atributo, acao, valor_novo, x = dt, linhas = seq_len(nrow(x)))')
change('    exige <- tokens[vapply(tokens, monitora_painel_forma_exige_habito, logical(1))]', '    exige <- tokens[vapply(tokens, function(ff) any(monitora_correcao_habito_alvos(x,categoria,ff,linhas)), logical(1))]')
change('monitora_painel_mensagem_contrato_entrada <- function(atributo, acao, valor_novo)', 'monitora_painel_mensagem_contrato_entrada <- function(atributo, acao, valor_novo, x = dt, linhas = seq_len(nrow(x)))')
change('det <- monitora_painel_detalhar_forma_vida_contratual(atributo, acao, valor_novo)', 'det <- monitora_painel_detalhar_forma_vida_contratual(atributo, acao, valor_novo, x, linhas)',2)
a=s.index('  monitora_painel_expandir_operacao_contratual <-');b=s.index('  formas_exoticas_observadas <-',a);p=s[a:b]
p=p.replace('    for (ff in det$exige_habito) {','    for (ff in det$exige_habito) {\n      linhas_h <- linhas[monitora_correcao_habito_alvos(x,det$categoria,ff,linhas)]\n      if(!length(linhas_h)) next')
x=p.index('    for (ff in det$exige_habito) {');y=p.index('    tipo_col <-',x);q=p[x:y]
q=q.replace('length(linhas)', 'length(linhas_h)').replace('linhas[1L]','linhas_h[1L]').replace('[[dep_col]][linhas]','[[dep_col]][linhas_h]')
q=q.replace('escopo = as.character(op_base$escopo_aplicacao[1L])','escopo = "linhas_diagnosticas_ocorrencia"')
q=q.replace('n_esperado = n_esperado, n_alvo = n_alvo','n_esperado = length(linhas_h), n_alvo = length(linhas_h)')
q=q.replace('op_h, x, linhas, chaves','op_h, x, linhas_h, chaves')
q=q.replace('      op_h <- monitora_correcao_anexar_contexto_operacao','      op_h[, `:=`(uuid_registro=monitora_correcao_colapsar_lista_serializada(x[[chaves$uuid_registro]][linhas_h]),linhas_alvo_serializadas=monitora_correcao_colapsar_lista_serializada(as.character(linhas_h)))]\n      op_h <- monitora_correcao_anexar_contexto_operacao')
p=p[:x]+q+p[y:];s=s[:a]+p+s[b:]
change('  server <- function(input, output, session) {','''  server <- function(input, output, session) {
    monitora_painel_linhas_habito <- function() {
      coletas <- if(monitora_painel_usar_lote_coletas()) input$coletas_lote else input$coleta
      if(is.na(chaves$coleta)) integer() else which(as.character(dt[[chaves$coleta]]) %in% as.character(coletas))
    }''')
for expr in ['monitora_painel_detalhar_forma_vida_contratual(atributo, input$acao, input$valor_novo)', 'monitora_painel_mensagem_contrato_entrada(atributo, input$acao, input$valor_novo)', 'monitora_painel_detalhar_forma_vida_contratual(atributo_sel, acao_val, monitora_painel_valor(input$valor_novo))']:
 change(expr,expr[:-1]+', x=dt, linhas=monitora_painel_linhas_habito())')
for field in ['triagem_forma_valida','mv_lote_forma_destino']:
 change(f'shiny::observeEvent(input${field}, {{',f'shiny::observeEvent(list(input${field},input$coleta,input$coletas_lote,input$escopo_coletas), {{')
# A substituição semântica de desconhecida é transacional também em caso de erro.
a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico__v262 <-');a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico <- function(',a);b=s.index('\n}\n',a)+3;p=s[a:b]
p=p.replace('  id_cor <-','  original <- data.table::copy(dt);dt <- data.table::copy(dt)\n  id_cor <-',1)
p=p.replace('  ), error = function(e) NULL)\n  if (is.list(res_desc_dep))', '''  ), error = function(e) e)
  if(inherits(res_desc_dep,"error") || !is.list(res_desc_dep)) {
    dt <- original
    return(falhar("falha_dependentes_desconhecida","Lote cancelado integralmente: falha ao tratar dependentes de desconhecida"))
  }
  if (is.list(res_desc_dep))''',1)
s=s[:a]+p+s[b:]
# Erros no recálculo do pai também cancelam a substituição semântica integral.
a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico__v262 <-');a=s.index('monitora_correcao_aplicar_movimento_forma_vida_lote_atomico <- function(',a);b=s.index('\n}\n',a)+3;p=s[a:b]
p=p.replace('    try(monitora_correcao_recalcular_tipo_forma_vida(dt, linhas_alvo), silent = TRUE)', '''    recalc <- tryCatch({monitora_correcao_recalcular_tipo_forma_vida(dt,linhas_alvo);TRUE},error=function(e)FALSE)
    if(!isTRUE(recalc)) {dt <- original;return(falhar("falha_recalculo_encostam","Lote cancelado integralmente: falha ao recalcular Encostam"))}''')
s=s[:a]+p+s[b:]
change('  tokens_origem_canon <- tryCatch(monitora_correcao_forma_canonica_lote(tokens_remover)', '  if (!length(tokens_remover)) return(falhar("bloqueada_operacao_nula","origem e destino indicam a mesma forma na mesma categoria"))\n  tokens_origem_canon <- tryCatch(monitora_correcao_forma_canonica_lote(tokens_remover)')
