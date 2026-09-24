"""Regras físicas explícitas; nunca substituir identificadores/colunas R."""
import re
DIRS={
 '03_auditorias':'03_aud', '07_relatorio_validacao':'07_validacao',
 '08_relatorios_analiticos':'08_analises',
 'auditorias_operacionais':'aud', 'ocorrencias_diagnosticas':'oc',
 'relatorios_apoio_tematicos':'ap',
 'diagnosticos_contrato_unico_registros_importados':'contrato',
 'diagnosticos_pipes_contrato':'pipes',
 'migracoes_divergentes':'div',
}
# Prefixos exclusivos de arquivos; não mudar rótulos de ocorrência ou contextos.
NOMES={
 'auditoria_fechamento_hierarquico_':'aud_fechamento_',
 'auditoria_sanitizacao_placeholders_semanticos_':'aud_placeholders_',
 'indice_relatorios_suporte_painel_':'indice_apoio_',
 'auditoria_dependencias_condicionais_regras_':'aud_regras_',
 'auditoria_simbolos_medias_anuais':'aud_simbolos_anuais',

 'pendencias_remanescentes_com_justificativas.csv':'pendencias_justificadas.csv',
 'auditoria_pendencias_impeditivas_registros_corrig':'aud_pendencias_registros_corrig',
 'comparacao_registros_campos_alterados_':'cmp_campos_alterados_',
 'comparacao_registros_':'cmp_reg_',
 'comparacao_resumos_indices_':'cmp_resumos_',
 'relatorio_detalhado_rejeicoes_validacao_registros_corrig':'rejeicoes_registros_corrig',
 'registros_ocorrencias_diagnosticas_':'registros_oc_',
 'registros_seca_morta_sem_forma_vida_':'registros_seca_morta_sem_forma_',
 'resumo_formas_vida_exoticas_por_campo_especie':'resumo_exoticas_por_especie',
 'auditoria_reutilizacao_etapas_contratuais':'aud_etapas_contratuais',
 'ocorrencias_seca_morta_linha_forma_relatorio_analitico':'ocorrencias_seca_morta_linha_forma',
 'material_botanico_edicoes_retrospectivas_documentadas':'material_edicoes_retrospectivas',
 'auditoria_harmonizacao_grafias_uc_registros_relatorio':'aud_grafias_uc',

 'diagnostico_pipes_contrato_':'diag_pipes_',
 'registros_token_fora_dominio_contrato_':'registros_token_dominio_',
 'registros_habito_obrigatorio_ausente_':'registros_habito_ausente_',
 'registros_solo_nu_com_outra_categoria_':'registros_solo_conflito_',
 'registros_forma_vida_exotica_sem_especie_':'registros_exotica_sem_especie_',
 'registros_forma_vida_exotica_com_especie_':'registros_exotica_com_especie_',
 'registros_forma_vida_desconhecida_invalida_':'registros_desconhecida_invalida_',
 'registros_formas_vida_exoticas_sem_forma_detalhada':'registros_exoticas_sem_forma',
 'auditoria_canonicalizacao_rotulos_listas_forma_vida':'aud_rotulos_forma_vida',
 'auditoria_alias_bromelioide_contrato_resumo':'aud_bromelioide',
 'auditoria_materiais_botanicos_contrato_resumo':'aud_material_botanico',
 'auditoria_conciliacao_habito_historico_definitivo_':'aud_habito_',
 'auditoria_persistencia_correcoes_':'p_',
 'auditoria_pipe_residual_resumo_':'aud_pipe_',
 'auditoria_pipes_':'aud_pipes_',
 'comparacao_ordem_legado_vs_contrato_':'ordem_contrato_',
 'resumo_mapa_observado_canonico_':'resumo_mapa_',
 'mapa_observado_canonico_':'mapa_',
 'resumo_comparacao_ordem_':'resumo_ordem_',
 'auditoria_coletas_ua_ano_duplicadas_':'aud_coletas_duplicadas_',
 'registros_formacao_vegetacional_inconsistente_ua_':'registros_formacao_ua_',
 'auditoria_identidades_ocorrencias_diagnosticas_':'aud_identidades_',
 'metadados_relatorio_operacional_seca_morta_':'metadados_seca_morta_',
 'material_botanico_comparacoes_elegiveis_composicao_base':'material_composicao_base',
 'resumo_pendencias_remanescentes_com_justificativas_':'resumo_pendencias_',
 'pendencias_remanescentes_com_justificativas_':'pendencias_',
}
def dirs(s):
 for a,b in DIRS.items():
  if s==a:s=b
  else:s=re.sub(r'(?<![\w])'+re.escape(a)+r'(?=/|$)',b,s)
 return s
def texto(s):
 s=dirs(s)
 for a,b in NOMES.items():s=s.replace(a,b)
 return s
# Comentários e símbolos R ficam intocados. Texto científico sem esses nomes também.
TOKEN=re.compile(r'#[^\n]*|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'',re.S)
def codigo(s):
 return TOKEN.sub(lambda m: m[0] if m[0].startswith('#') else m[0][0]+texto(m[0][1:-1])+m[0][-1],s)
