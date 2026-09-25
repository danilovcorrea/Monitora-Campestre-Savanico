import re,json,base64,gzip,textwrap
DIRS={'03_aud':'03_auditorias','aud':'auditorias','oc':'ocorrencias','ap':'apoio','operacoes_sessao':'operacoes','cache_sessao':'cache'}
NOMES={
 'resumo_pendencias_impeditivas_registros_corrig':'resumo_pendencias_registros_corrig',
 'auditoria_registros_corrig_contrato_xlsform21_resumo':'resumo_contrato_xlsform21',
 'aud_pipes_registros_importados_operacional_':'aud_pipes_importados_',
 'aud_pipe_registros_importados_operacional_':'aud_pipe_importados_',
 'p_pos_export_pre_analises_registros_corrig_':'p_pre_analises_',
 'ordem_contrato_checkpoint2_pos_tokenizacao_':'ordem_contrato_pos_tokens_',
 'resumo_pipes_contrato_pos_export_registros_corrig_':'resumo_pipes_pos_export_',
 'auditoria_transacao_justificativas_':'transacao_justificativas_',
 'registros_ponto_sem_interceptacao_':'ponto_sem_interceptacao_',
 'registros_seca_morta_sem_forma_':'seca_morta_sem_forma_',
 'resumo_seca_morta_por_forma_vida_':'seca_morta_por_forma_',
 'registros_nativa_sem_forma_vida_':'nativa_sem_forma_',
 'indice_relatorios_ocorrencias_diagnosticas':'indice_ocorrencias',
 'trajetorias_seca_morta_por_ua_':'trajetorias_seca_morta_',
 'relatorio_operacional_seca_morta_':'relatorio_seca_morta_',
 'registros_exotica_sem_forma_vida_':'exotica_sem_forma_',
 'registros_exotica_com_especie_':'exotica_com_especie_',
 'resumo_seca_morta_por_ua_ano_':'seca_morta_por_ua_ano_',
 'resumo_seca_morta_em_revisao_':'seca_morta_em_revisao_',
 'registros_seca_morta_em_revisao_':'pontos_seca_em_revisao_',
 'resumo_ocorrencias_impeditivas_':'resumo_impeditivas_',
 'registros_forma_vida_desconhecida_':'forma_desconhecida_',
 'resumo_ocorrencias_diagnosticas_':'resumo_ocorrencias_',
 'registros_ua_duplicada_mesmo_ano_':'ua_duplicada_mesmo_ano_',
 'registros_exotica_sem_especie_':'exotica_sem_especie_',
 'registros_desconhecida_invalida_':'desconhecida_invalida_',
}
NOMES.update({'registros_formas_vida_exoticas_com_especies': 'exoticas_com_especies', 'registros_formas_vida_exoticas_sem_especies': 'exoticas_sem_especies', 'resumo_formas_vida_desconhecida_por_unidade': 'desconhecida_por_unidade', 'auditoria_conciliacao_semantica_operacoes': 'aud_conciliacao_operacoes', 'auditoria_conflitos_semanticos_fila': 'aud_conflitos_fila', 'auditoria_mapa_colunas_canonicas': 'aud_mapa_colunas', 'auditoria_semantica_formas_vida_': 'aud_formas_vida_', 'auditoria_semantica_papeis_colunas': 'aud_papel', 'dicionario_atributos_registros_corrig_incremental': 'dicionario_atributos_incremental', 'metadados_relatorio_operacional_seca_morta': 'meta_seca', 'registros_outra_forma_vida_': 'outra_forma_vida_', 'resumo_seca_morta_por_ano_': 'seca_morta_por_ano_', 'correcoes_semanticas_sessao_painel_incremental_': 'correcoes_painel_incremental_', 'auditoria_registros_validados_gate_ocorrencias_diagnosticas': 'aud_gate_ocorrencias', 'auditoria_colunas_protegidas_': 'aud_protegidas_', 'auditoria_operacoes_atomicas_resumo_': 'aud_atomicas_', 'validacao_espacial_consensos_uas_coincidentes': 'espacial_uas_coincidentes', 'validacao_espacial_ocorrencias_diagnosticas': 'espacial_ocorrencias', 'validacao_espacial_pendencias_e_alertas': 'espacial_pendencias_alertas', 'validacao_espacial_rejeicoes_preparacao': 'espacial_rejeicoes_preparacao'})
NOMES['aud_coletas_duplicadas_']='aud_coletas_'
NOMES.update({'resumo_ordem_checkpoint2_pos_tokenizacao_': 'resumo_ordem_pos_tokens_', 'auditoria_resolucao_colunas_estruturais_estatisticas': 'aud_resolucao_colunas_estatisticas', 'material_botanico_comparacoes_elegiveis_composicao_base': 'material_botanico_comp_elegiveis_base'})
TOKEN=re.compile(r'#[^\n]*|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'',re.S)
def dirs(v):
 for a,b in DIRS.items():v=re.sub(r'(?<![\w])'+a+r'(?=/|$)',b,v)
 return v
def physical(v):
 v=dirs(v)
 for a,b in sorted(NOMES.items(),key=lambda v:-len(v[0])):
  if a.endswith("_") and v == a[:-1]: v=b[:-1]
  else:v=v.replace(a,b)
 return v
def transform(v,names=True):return TOKEN.sub(lambda m:m[0] if m[0].startswith('#') else m[0][0]+(physical if names else dirs)(m[0][1:-1])+m[0][-1],v)
# Identity readers retain their logical patterns and only update directory outputs.
protected={n:function(n) for n in ['monitora_nome_arquivo_logico','monitora_output_classificar_arquivo_raiz','monitora_output_destino_correcao']}
s=transform(s)
for n,v in protected.items():s=s.replace(function(n),transform(v,False),1)
a=s.index('monitora_nome_arquivo_logico <- function(x) {')+len('monitora_nome_arquivo_logico <- function(x) {')
inv='\n  if (!any(vapply(c('+','.join(json.dumps(x) for x in NOMES)+'),function(p) startsWith(x,p),logical(1L)))) {\n'
for i,(old,new) in enumerate(sorted(NOMES.items(),key=lambda x:-len(x[1]))):
 inv+=('    if' if i==0 else '    else if')+' (startsWith(x,'+json.dumps(new)+')) x <- paste0('+json.dumps(old)+',substring(x,'+str(len(new)+1)+'))\n'
inv+='  }\n'
s=s[:a]+inv+s[a:]
# Read legacy layouts after physical directories changed.
s=s.replace('  caminhos <- unique(c(legado, organizado, organizado_anterior))','  caminhos <- unique(c(legado, organizado, organizado_anterior, file.path(raiz,"02_painel_correcoes","ap",fase)))')
s=s.replace('unique(c(candidatos, gsub("/ocorrencias/", "/ocorrencias_diagnosticas/", candidatos, fixed = TRUE)))','unique(c(candidatos, gsub("/ocorrencias/", "/oc/", candidatos, fixed=TRUE), gsub("/ocorrencias/", "/ocorrencias_diagnosticas/", candidatos, fixed = TRUE)))')
# Incorporated modules use the same physical names; calculations are untouched.
start=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(');pos=s.index('\n',start)+1;chunks=[]
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 chunks.append(m[1]);last=pos+m.end();pos=s.index('\n',pos)+1
module=gzip.decompress(base64.b64decode(''.join(chunks))).decode();module2=transform(module)
enc=base64.b64encode(gzip.compress(module2.encode(),9,mtime=0)).decode()
s=s[:s.index('\n',start)+1]+',\n'.join('"'+v+'"' for v in textwrap.wrap(enc,12000))+s[last:]
(root/'artifacts/v304/caminhos.json').write_text(json.dumps({'diretorios':DIRS,'nomes':NOMES},ensure_ascii=False,indent=2))
(root/'artifacts/v304/modulos_antes.R').write_text(module);(root/'artifacts/v304/modulos_depois.R').write_text(module2)
# A integridade compara identidades, sem depender do prefixo físico abreviado.
a=s.index('monitora_diag_validar_ocorrencias_materializadas <-');b=s.index('\n}\n',a)+3;p=s[a:b]
x=p.index('  arqs_tipo <- list.files');y=p.index('  partes <-',x)
p=p[:x]+'''  arqs_tipo <- list.files(base_dir, pattern = paste0("_",fase_rx,"\\\\.csv$"), full.names=TRUE)
  nomes_logicos <- vapply(basename(arqs_tipo),monitora_nome_arquivo_logico,character(1L))
  arqs_tipo <- arqs_tipo[startsWith(nomes_logicos,"registros_") & !startsWith(nomes_logicos,"registros_ocorrencias_diagnosticas_")]
'''+p[y:];s=s[:a]+p+s[b:]
# Fase incremental tem diretório próprio e só é fallback quando não há pré comum.
change('if (fase == "pre_painel") "relatorios_pre_painel" else "relatorios_pos_correcoes"','if (startsWith(fase,"pre_painel")) paste0("relatorios_",fase) else "relatorios_pos_correcoes"')
change('  pre <- monitora_relatorios_resolver_fase(base_dir, "pre_painel")','  pre <- monitora_relatorios_resolver_fase(base_dir, "pre_painel")\n  if(!length(pre)) pre <- monitora_relatorios_resolver_fase(base_dir,"pre_painel_incremental")')

# Contextos operacionais abreviados antes de compor os nomes de arquivos.
change('contexto = "pos_export_checkpoint_registros_corrig"','contexto = "pos_checkpoint"')
change('paste0("correcoes_campos_", modo)','paste0("correcoes_",sub("painel_incremental_registros_corrig","incremental",modo,fixed=TRUE))')
change('paste0("correcoes_campos_", MONITORA_MODO_EXECUCAO)','paste0("correcoes_",sub("painel_incremental_registros_corrig","incremental",MONITORA_MODO_EXECUCAO,fixed=TRUE))')
change('paste0("pos_correcoes_", modo, "_", MONITORA_EXEC_ID)','paste0("pos_",sub("painel_incremental_registros_corrig","incremental",modo,fixed=TRUE),"_",MONITORA_EXEC_ID)')
change('fase_dup <- paste0("pos_correcoes_", modo)','fase_dup <- paste0("pos_",sub("painel_incremental_registros_corrig","incremental",modo,fixed=TRUE))')
# Quatro nomes são montados dinamicamente; abreviar só a expressão do caminho.
s=s.replace('paste0("aud_pipes_", gsub("[^A-Za-z0-9]+", "_", produto)', 'paste0("aud_pipes_", sub("registros_importados_operacional_","importados_",gsub("[^A-Za-z0-9]+", "_", produto),fixed=TRUE)')
s=s.replace('paste0("aud_pipe_", sufixo,', 'paste0("aud_pipe_", sub("registros_importados_operacional_","importados_",sufixo,fixed=TRUE),')
s=s.replace('paste0("p_", contexto,', 'paste0("p_", sub("pos_export_pre_analises_registros_corrig","pre_analises",contexto,fixed=TRUE),')
s=s.replace('paste0("ordem_contrato_", sufixo,', 'paste0("ordem_contrato_", sub("checkpoint2_pos_tokenizacao","pos_tokens",sufixo,fixed=TRUE),')
# Dois construtores incrementais também usam componentes separados.
s=s.replace('paste0("resumo_", nome_base, "_por_unidade.csv")','if (identical(nome_base,"formas_vida_desconhecida")) "desconhecida_por_unidade.csv" else paste0("resumo_", nome_base, "_por_unidade.csv")')
s=s.replace('paste0("- resumo_", nome_base, "_por_unidade.csv")','paste0("- ",if (identical(nome_base,"formas_vida_desconhecida")) "desconhecida_por_unidade.csv" else paste0("resumo_",nome_base,"_por_unidade.csv"))')
s=s.replace('paste0("correcoes_semanticas_", fase, "_", exec_id, ".csv")','paste0(if (identical(fase,"sessao_painel_incremental")) "correcoes_painel_incremental" else paste0("correcoes_semanticas_",fase),"_",exec_id,".csv")')
# Resumo do mesmo contrato também era composto depois da compactação estática.
s=s.replace('paste0("resumo_pipes_contrato_", sufixo,','paste0("resumo_pipes_", if (identical(sufixo,"pos_export_registros_corrig")) "pos_export" else paste0("contrato_",sufixo),')

s=s.replace('paste0("resumo_ordem_", sufixo,','paste0("resumo_ordem_", sub("checkpoint2_pos_tokenizacao","pos_tokens",sufixo,fixed=TRUE),')
