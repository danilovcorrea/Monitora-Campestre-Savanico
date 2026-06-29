# v2.6.0 - Roll-forward semântico, relatório consolidado e governança de validação

Esta release publica a linha pública v2.6.0 do script `monitora_campsav_alvo_global`, incorporando a versão operacional validada para continuidade de produção.

## Destaques

- Roll-forward semântico por `input/correcoes_semanticas.csv`.
- Contrato `replay_semantico_v1` para preservar compatibilidade de intenções de correção entre versões.
- Relatório consolidado de validação com registro da cadeia de modificações desde os arquivos de input até os produtos finais.
- Seção específica para sanitizações automáticas e operações assistidas do painel.
- Manual do usuário com passo a passo dos modos de execução.
- Defaults públicos seguros: oráculo desligado, replay desligado, painel desligado, registros validados opcionais desligados e validação espacial desligada.

## Padrões públicos

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
MONITORA_OPCAO_REPLAY_DIAGNOSTICO_NAO_ABORTAR <- "S"
MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO <- "N"
MONITORA_DIR_RUN_ORACULO <- "input/oraculo_replay"
MONITORA_OPCAO_REPLAY_ORACULO_ABORTAR_DIVERGENCIA <- "N"
```

## Privacidade

A release não inclui dados de entrada, produtos locais, logs de execução, coordenadas, UUIDs, fotos ou arquivos derivados sensíveis.

## Como citar

CBC/ICMBio-MMA. 2026. *Script de tratamento, validação e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas do Componente Campestre Savânico do Programa Monitora*. Versão `v2.6.0`. Repositório GitHub: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
