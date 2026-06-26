# v2.5.5 - Painel contratual, registros importados saneados e controles de edição

Publicação pública corretiva e evolutiva do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Corrige os modos de painel para que `painel_e_parar`, `abrir_painel_cache` e modos `painel_incremental_*` forcem a abertura do painel quando esse é o comportamento esperado.
- Melhora o detalhamento da barra de progresso em etapas pós-painel, fechamento contratual, auditorias e materializações finais.
- Reestrutura `registros_importados.csv` como produto saneado e comparável com `registros_corrig.csv`, preservando o snapshot técnico bruto em `registros_importados_bruto.csv`.
- Adiciona auditorias automáticas de comparação entre `registros_importados.csv`, `registros_corrig.csv` e `registros_validados.csv`.
- Restringe o seletor de atributos do painel aos atributos do template SISMONITORA/XLSForm 2025, com lista global independente da COLETA, ponto ou valor preenchido.
- Ordena o seletor do painel conforme a ordem do template SISMONITORA/XLSForm.
- Garante a presença no seletor de atributos como `forma_serrapilheira` e `forma_vida_outros`.
- Implementa controles dinâmicos de edição por tipo de atributo: listas XLSForm, data, hora, número, coordenada e texto.
- Bloqueia edição de campos de domínio quando a lista XLSForm não está carregada.
- Em listas de tokens/select_multiple, remove operações de substituição/limpeza do valor inteiro; permite somente adicionar, remover ou substituir tokens.
- Adiciona `output/auditoria_painel_controle_atributos.csv` para auditar a cobertura dos controles de edição.

## Padrões públicos seguros

    MONITORA_MODO_EXECUCAO <- "completo"
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
    MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
    MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
    MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"

## Arquivos principais

- `monitora_campsav_alvo_global_v2.5.5.R`
- `monitora_campsav_alvo_global.R`
- `MONITORA_CAMPSAV_Alvo_Global.R`
- `R/monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
- `Monitora-Campestre-Savanico_v2.5.5_release_completa.zip`

## Privacidade

O release público não inclui `input/`, `output/`, `log/`, `extracted/` nem produtos de dados gerados localmente.
