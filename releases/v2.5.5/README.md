# Monitora Campestre-Savânico — Alvo Global

## Versão pública atual

- Versão: `v2.5.5`
- Script principal versionado: `monitora_campsav_alvo_global_v2.5.5.R`
- Script canônico: `monitora_campsav_alvo_global.R`
- Cópia congelada no repositório: `releases/v2.5.5/monitora_campsav_alvo_global_v2.5.5.R`
- Assets auxiliares da publicação: `release_assets/v2.5.5/`
- Modo padrão público seguro: `MONITORA_MODO_EXECUCAO <- "completo"`, `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`, `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"`, `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"` e `MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"`.

## Principais recursos da versão atual

A versão `v2.5.5` consolida a curadoria assistida por painel com contrato SISMONITORA/XLSForm 2025, produtos de entrada saneados para comparação, controles de edição por tipo de atributo e auditorias adicionais de rastreabilidade.

- Corrige os modos de painel para que `painel_e_parar`, `abrir_painel_cache` e os modos `painel_incremental_*` abram o painel quando esperado.
- Melhora a granularidade da barra de progresso nas etapas pós-painel e nos fechamentos contratuais.
- Gera `registros_importados.csv` saneado/comparável e preserva `registros_importados_bruto.csv` como snapshot técnico.
- Adiciona auditorias de comparação entre `registros_importados.csv`, `registros_corrig.csv` e `registros_validados.csv`.
- Restringe e ordena o seletor de atributos do painel pelo template SISMONITORA/XLSForm 2025.
- Adiciona controles dinâmicos de valor por atributo e bloqueia domínios XLSForm ausentes.
- Em listas de tokens/select_multiple, restringe operações a adicionar/remover/substituir tokens, sem substituição integral do campo.

## Versão anterior

A versão pública imediatamente anterior foi `v2.5.4`. O histórico detalhado fica no `CHANGELOG.md` e nas páginas de release do GitHub.

## Modos de execução

Configure `MONITORA_MODO_EXECUCAO` no início do script conforme o objetivo da rodada.

- `completo`: executa todo o pipeline, com bases tratadas, auditorias, estatísticas, relatórios e gráficos.
- `sem_png`: executa o pipeline sem exportar PNGs.
- `estatisticas_sem_graficos`: mantém tabelas estatísticas e relatório textual, sem gráficos e KML.
- `ate_registros_corrig`: grava `registros_corrig.csv` sem abrir o painel e encerra de forma controlada.
- `painel_e_parar`: abre o painel, aplica correções salvas e encerra após gravar `registros_corrig.csv`.
- `abrir_painel_cache`: reabre o painel a partir do cache pré-painel, sem reconstruir toda a leitura e sem reaplicar correções antigas em `input/`.
- `painel_incremental_registros_corrig`: abre o painel a partir de um `registros_corrig.csv` já produzido pelo script, colocado em `input/`, para continuar a curadoria.

O padrão público permanece seguro:

    MONITORA_MODO_EXECUCAO <- "completo"
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
    MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
    MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
    MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"

Nos modos `painel_e_parar`, `abrir_painel_cache` e `painel_incremental_*`, o script força internamente a abertura do painel.

## Privacidade e produtos locais sensíveis

Produtos locais de dados, especialmente `registros_importados.csv`, `registros_importados_bruto.csv`, `registros_corrig.csv`, `registros_validados.csv`, relatórios e arquivos de correção, podem conter dados institucionais sensíveis. O release público não deve conter `input/`, `output/`, `log/`, `extracted/` nem produtos gerados por execução local.

## Licença

Este projeto está licenciado sob a licença GPL-3.0. Consulte `LICENSE`.
