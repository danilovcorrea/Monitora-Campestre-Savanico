# Monitora-Campestre-Savânico — snapshot de teste para bolsistas

Snapshot: v2.6.2 teste bolsistas hotfix2  
Data: 20260630

Este arquivo é uma versão de teste operacional da futura v2.6.2.

Não é release pública.  
Não é tag.  
Não substitui a versão pública estável.

## Arquivo principal

`monitora_campsav_alvo_global_v2.6.2_teste_bolsistas_20260630_hotfix2.R`

## Validações já realizadas

- painel abriu com 1, 3 e 5 COLETAS;
- aba/mapa espacial carregou por padrão;
- mapa exibiu múltiplas coletas/pontos ou diagnóstico claro;
- seletor de COLETA atualizou tabela/preview;
- Data (data_hora/data) mostrou valor original esperado;
- CICLO, CAMPANHA, UA e EA mostraram valor original esperado;
- Valor novo abriu vazio;
- CAMPANHA, CICLO e UA aceitaram cadastro manual válido;
- EA permaneceu restrito a EA-001_Cps;
- pendência técnica apareceu como não corrigível pela bolsista;
- Cancelar execução sem materializar encerrou sem criar novo registros_corrig.csv;
- testes estáticos v2.6.2 passaram após o hotfix.

## Testar principalmente

- abertura do painel de correções;
- aba/mapa de validação espacial carregando;
- Data (data_hora/data), UA, Ciclo e Campanha;
- bloqueio de EA diferente de EA-001_Cps;
- mensagens amigáveis em operações incompletas;
- fechamento sem salvar;
- cancelamento sem materializar;
- geração de registros_corrig quando salvar/continuar;
- geração de registros_validados apenas quando não houver pendências impeditivas.

## Limitações conhecidas ainda em desenvolvimento

- movimento assistido para "outras plantas terrestres";
- padronização geral das listas suspensas como label + token;
- revisão completa do motor de operações/fila;
- otimização da latência do preview;
- revisão geral dos textos, avisos e mensagens do painel.

## Ao relatar problema, enviar

- print da tela;
- COLETA;
- UC, EA, UA;
- atributo ou operação tentada;
- mensagem exibida;
- se salvou, fechou sem salvar ou cancelou sem materializar;
- caminho da pasta output/log gerada.
