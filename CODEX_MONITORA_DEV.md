# Monitora-Campestre-Savanico — instruções para Codex local

Você está no Fedora, dentro do repositório Monitora-Campestre-Savanico, em uma branch dev isolada:

dev-v2.6.2-painel101-registros-corrig-mestre

## Objetivo geral

Gerar uma candidata dev segura a partir da v2.6.1 pública, preservando os avanços já produzidos e evitando regressão estrutural ou funcional.

A base de desenvolvimento é a branch atual, derivada de main após v2.6.1.

Use:
- v2.6.1 pública como base estrutural;
- v2.6.0 como oráculo funcional do painel;
- a dev local/anexada, se existir, apenas como fonte seletiva de avanços;
- nunca faça rollback geral para v2.6.0;
- nunca publique.

## Resultado esperado

1. Todos os 101 atributos editáveis devem aparecer em "Atributo a corrigir".
2. O painel deve manter comportamento funcional validado na v2.6.0:
   - abrir sem COLETA pré-selecionada;
   - só calcular diagnóstico depois da seleção;
   - seleção de COLETA normal deve mostrar os 101 pontos/linhas esperados;
   - contadores devem atualizar após operação simples;
   - contadores devem atualizar após operação em lote.
3. Manual do usuário e relatório consolidado de validação da v2.6.1/dev devem ser preservados.
4. registros_corrig é o produto mestre de validação.
5. Se registros_corrig tiver pendências:
   - registrar pendências;
   - gravar dirty flags/selo pendente quando aplicável;
   - salvar checkpoint marcado, se a rotina permitir;
   - não emitir selo válido;
   - bloquear registros_validados.
6. Se registros_corrig não tiver pendências:
   - emitir selo válido;
   - materializar registros_corrig final.
7. registros_validados só pode nascer se:
   - MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = "S";
   - registros_corrig tiver selo válido.
8. registros_validados deve apenas selecionar e reordenar colunas de registros_corrig validado.
   Não deve:
   - derivar alias;
   - formatar hora/data/número;
   - sanitizar;
   - recalcular Encostam;
   - aplicar regra inédita;
   - corrigir valor.
9. Encostam/tipo_forma_vida deve ser tratado como derivado controlado:
   - aparece no painel como ação controlada de recálculo;
   - não deve ser edição textual livre;
   - é recalculado a partir dos campos inferiores;
   - inconsistências Encostam ↔ inferiores viram pendências impeditivas.
10. O painel deve mostrar ao bolsista, antes de salvar, se ainda há pendências impeditivas que bloqueiam o selo de registros_corrig.

## Restrições absolutas

Não faça:
- push;
- tag;
- release;
- publicação;
- sudo;
- apagamento de dados;
- alteração fora do repositório;
- ativação de validação espacial por padrão;
- abertura de aba espacial por padrão;
- reescrita total do painel;
- rollback geral para v2.6.0;
- uso de registros_validados como validador principal;
- validações exclusivas novas em registros_validados;
- criação de colunas dentro da função do dropdown;
- mistura de muitos blocos em um único diff.

## Defaults públicos seguros

Preservar ou restaurar, salvo se já houver equivalente seguro:

MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL <- "N"
MONITORA_OPCAO_GERAR_MANUAL_USUARIO <- "S"
MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO <- "S"

## Contrato mestre de registros_corrig

Criar ou consolidar função explícita:

monitora_registros_corrig_contrato_mestre_2025()

Esse contrato deve integrar:
- XLSForm/template 2025;
- atributos cadastrais/administrativos;
- aliases históricos EA/ea, UA/ua, Ciclo/ciclo, Campanha/campanha, UC/uc quando aplicável;
- 101 atributos editáveis do painel;
- campos protegidos;
- campos derivados;
- regras de domínio/formato/materialização.

Colunas desejadas no contrato, quando possível:
- atributo_canonico;
- coluna_materializada;
- aliases_aceitos;
- origem_contrato;
- ordem_template;
- painel_101;
- editavel_painel;
- protegido;
- tipo_base;
- dominio_tipo;
- list_name;
- obrigatorio_registros_corrig;
- obrigatorio_registros_validados;
- acao_painel;
- validacao_materializacao.

## Ocorrências diagnósticas impeditivas

O painel deve distinguir:
- pendências impeditivas corrigíveis no painel;
- pendências impeditivas estruturais;
- alertas não impeditivos.

Blocos impeditivos mínimos:
1. duplicidade/estrutura da coleta;
2. Encostam incoerente com campos inferiores;
3. desconhecida/outra forma de vida;
4. 101 atributos editáveis inválidos;
5. aliases e atributos cadastrais administrativos;
6. dirty flags/edição não persistida.

Tipos esperados, quando aplicáveis:
- uas_duplicadas_mesmo_ano;
- ponto_sem_interceptacao;
- nativa_sem_forma_vida;
- exotica_sem_forma_vida;
- seca_morta_sem_forma_vida;
- outra_forma_vida;
- forma_vida_desconhecida;
- atributo_101_nao_resolvido;
- atributo_101_alias_conflitante;
- atributo_101_valor_fora_dominio;
- atributo_101_formato_invalido;
- atributo_101_dependente_sem_parent;
- atributo_101_parent_sem_dependente_obrigatorio;
- cadastro_uc_ausente_ou_invalida;
- cadastro_ea_ausente_ou_invalida;
- cadastro_ua_ausente_ou_invalida;
- cadastro_ciclo_ausente_ou_invalido;
- cadastro_campanha_ausente_ou_invalida;
- cadastro_validado_invalido;
- cadastro_data_validacao_invalida;
- cadastro_validador_inconsistente;
- edicao_nao_persistida;
- dirty_flag_pendente;
- atributo_editado_reprovado_pos_validacao;
- encostam_desatualizado;
- encostam_token_sem_inferior;
- encostam_inferior_sem_token;
- encostam_solo_nu_conflitante;
- encostam_vazio_apos_recalculo;
- encostam_token_desconhecido;
- encostam_desconhecida_superior_only;
- encostam_outra_forma_vida.

## Ordem segura de implementação

Trabalhe em blocos pequenos:

Bloco 1:
- inspecionar v2.6.1, v2.6.0 e dev local;
- apresentar plano de commits;
- listar funções a criar/alterar;
- listar testes Rscript;
- parar antes de editar.

Bloco 2:
- restaurar/conferir defaults públicos seguros;
- criar validação estática de defaults;
- não alterar painel.

Bloco 3:
- criar/consolidar contrato mestre de registros_corrig;
- criar teste do contrato 101;
- não conectar ainda ao painel.

Bloco 4:
- corrigir dropdown "Atributo a corrigir" para usar contrato mestre;
- garantir 101 atributos;
- não alterar motor diagnóstico.

Bloco 5:
- restaurar comportamento funcional do painel com v2.6.0 como oráculo;
- não alterar registros_validados.

Bloco 6:
- ampliar ocorrências diagnósticas impeditivas;
- conectar pendências ao status/selo de registros_corrig.

Bloco 7:
- tratar Encostam como derivado controlado;
- adicionar pendências específicas Encostam ↔ inferiores.

Bloco 8:
- fechar materialização de registros_corrig;
- emitir selo válido apenas sem pendências;
- checkpoint marcado quando houver pendências.

Bloco 9:
- restringir registros_validados à herança por seleção/reordenação.

Bloco 10:
- rodar validação final;
- mostrar git diff --stat;
- mostrar logs;
- parar para revisão humana.

## Saídas esperadas a cada bloco

Ao final de cada bloco, mostre:
- git status --short;
- git diff --stat;
- lista de arquivos alterados;
- testes executados;
- marcadores finais, por exemplo:
  - PARSE_OK;
  - DEFAULTS_PUBLICOS_OK;
  - CONTRATO_MESTRE_OK;
  - CONTRATO_PAINEL_101_OK;
  - PAINEL_ORACULO_260_OK;
  - REGISTROS_CORRIG_SELO_OK;
  - REGISTROS_VALIDADOS_HERANCA_OK.

Não faça commit sem autorização explícita.
