# Monitora-Campestre-Savânico — snapshot de teste para bolsistas

Este arquivo é uma versão de teste operacional da futura v2.6.2.

Não é release pública.
Não é tag.
Não substitui a versão pública estável.

## Arquivo principal

`monitora_campsav_alvo_global_v2.6.2_teste_bolsistas_20260629.R`

## Testar principalmente

- abertura do painel de correções;
- presença de nativas no dropdown;
- presença de material botânico / serrapilheira;
- edição de Data (data_hora) sem preenchimento automático com data atual;
- tabela de pendências que impedem registros_validados;
- geração de registros_corrig;
- geração de registros_validados quando não houver pendências impeditivas;
- preenchimento de validado, validador, data_validacao e obs_validacao em registros_corrig.

## Limitações conhecidas ainda em desenvolvimento

- movimento assistido para "outras plantas terrestres";
- padronização geral das listas suspensas como label + token;
- revisão do motor de operações/fila/duplo clique;
- painel de validação espacial carregar por padrão;
- otimização da latência do preview;
- revisão geral dos textos, avisos e mensagens do painel.

## Observação sobre validador

O campo validador só será preenchido quando houver responsável explícito informado no painel ou em variável de execução.

## Ao relatar problema, enviar

- print da tela;
- COLETA;
- UC, EA, UA;
- atributo ou operação tentada;
- mensagem exibida;
- se salvou ou não;
- caminho da pasta output/log gerada.
