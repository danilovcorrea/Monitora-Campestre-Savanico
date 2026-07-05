# Validação R local H2R-C

- Data/hora: 2026-07-05 00:42:08 -0300
- Script: `/home/dfed/Projetos/Monitora-Campestre-Savanico_v262_rollforward_v260/monitora_campsav_alvo_global_v2.6.0.R`
- R: R version 4.6.1 (2026-06-24)
- Resultado geral: OK

## Checks

- [OK] parse_script_principal: parse(file=script_path) OK
- [OK] produto_nome_operacional_pre_painel_unico: ocorrencias= 1
- [OK] auditoria_final_lista_operacional_pre_painel: ocorrencias= 1
- [OK] produto_referenciado_registros_importados_bruto_csv: ocorrencias= 36
- [OK] produto_referenciado_registros_importados_csv: ocorrencias= 77
- [OK] produto_referenciado_registros_importados_operacional_pre_painel_csv: ocorrencias= 17
- [OK] produto_referenciado_registros_corrig_csv: ocorrencias= 157
- [OK] produto_referenciado_registros_validados_csv: ocorrencias= 103
- [OK] funcao_exportar_importados_existe: definicoes= 2 chamadas= 3
- [OK] funcao_exportar_saneado_existe: definicoes= 2 chamadas= 4
- [OK] ordem_bruto_saneado_antes_registros_corrig: ln_bruto_call= 34939 ln_saneado_call= 34994 ln_reg_corrig= 35038
- [OK] rm_registros_apos_registros_corrig: ln_reg_corrig= 35038 ln_rm_registros= 35039
- [OK] operacional_pre_painel_apos_registros_corrig: ln_reg_corrig= 35038 ln_operacional_pre= 39256
- [OK] checkpoint_operacional_tokenizado_existe: ln_token_checkpoint= 34991
- [OK] apos_rm_usa_nome_operacional_proprio: operacional_pre_painel_depois_rm= TRUE
- [OK] apos_rm_nao_reexecuta_saneado_importados: linhas_textuais= 39248,39249 ; linhas_chamadas= 39249 ; linhas_operacionais= nenhuma
