# v2.4.1 - registros_validados e contrato XLSForm/SISMONITORA

Publicação pública `v2.4.1` do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Mantém `registros_corrig.csv` como versão canônica corrigida, auditável e usada pelo pipeline.
- Adiciona `registros_validados.csv` como produto público opcional compatível com o contrato do XLSForm vigente e com a estrutura exportada pelo SISMONITORA.
- Inclui a variável pública `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"`, desligada por padrão.
- Quando ativado com `"S"`, o produto `registros_validados.csv` é criado a partir de `registros_corrig.csv` final, sem template externo.
- Amplia auditorias de schema, formatos, domínios XLSForm, condicionais/relevance, chaves, UUIDs e sanitizações.
- Consolida a sanitização de outras formas de vida em fontes históricas, com conversão apenas de descritores inequívocos e limpeza auditada de resíduos legados.
- Consolida a sanitização de fotos e descritores de forma de vida desconhecida, preservando-os apenas quando o token `desconhecida` permanece na categoria correspondente.
- Corrige a geração de resumos por unidade vazios para sobrescrever arquivos residuais antigos em `output/`.

## Validação operacional

A versão foi validada com execução sem erro, persistência de correções em `registros_corrig.csv`, criação de `registros_validados.csv` e auditorias contratuais sem bloqueios.

## Arquivo principal

- `monitora_campsav_alvo_global_v2.4.1.R`
