# Monitora Campestre-Savânico

Script de tratamento, validação e análise de dados do **Alvo Global Plantas Herbáceas e Lenhosas do Componente Campestre Savânico** do **Programa Monitora**.

**Versão pública atual:** `v2.6.4`
**Script principal:** [`monitora_campsav_alvo_global_v2.6.4.R`](monitora_campsav_alvo_global_v2.6.4.R)

## Finalidade

O script lê, padroniza, audita, deduplica, corrige e analisa registros exportados do SISMONITORA para campanhas amostrais do alvo global Plantas Herbáceas e Lenhosas.

Também pode abrir painel Shiny para correções assistidas, validar espacialmente COLETAS, gerar relatórios de apoio, estatísticas, gráficos, KMLs e produtos compatíveis com o contrato XLSForm/SISMONITORA quando habilitado.

## Novidades da v2.6.4

- Hotfix do painel de correções: o seletor "Atributo a corrigir" fica reconciliado entre dropdown, auditoria e log (98 atributos editáveis de fato, sem divergência silenciosa e sem bloqueio tardio no botão "Adicionar correção").
- Domínios XLSForm normalizados para atributos cujo nome de coluna carregava aspas HTML herdadas do formulário de origem, incluindo os campos de espécie exótica que antes ficavam bloqueados por "domínio ausente".
- `UA`/`EA`/`CICLO`/`CAMPANHA`, `Data`/`Horário` e as coordenadas inicial/final seguem editáveis e validados (geopoint) no painel.
- `forma_vida_nativa_samambaia` tratado como hábito obrigatório (`select_one`); `forma_vida_nativa_samambaia_sp` como texto livre de espécie.
- Persistência de correções pós-exclusão de coletas duplicadas (EXCCOL) por chave estável (`uuid_registro`/chave composta), sem usar o próprio atributo corrigido como filtro de contexto de si mesmo.
- Importação mais robusta: reparo determinístico de mojibake (UTF-8/Latin-1) em colunas estruturadas e diagnóstico específico por COLETA/UA quando uma lista de valores separada por `|` não tem correspondência segura com os pontos amostrais, no lugar de uma mensagem de erro genérica.
- Gráficos consolidados em `output/06_graficos`.
- Replay semântico (`replay_semantico_v1`) preservado.
- Publicação sem dados reais, como sempre.

## Padrões públicos seguros

A execução pública permanece segura por padrão e já está configurada para gerar `registros_validados.csv` quando a base não tiver pendências impeditivas:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
```

Para curadoria assistida, use:

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "S"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "S"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
```

Os modos orientados a painel forçam internamente a abertura do Shiny. Quando `MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "S"`, a aba de validação espacial também é ativada internamente.

## Uso básico

1. Crie uma pasta de execução limpa.
2. Copie `monitora_campsav_alvo_global_v2.6.4.R` para a raiz.
3. Crie `input/` e coloque ali somente os arquivos brutos de entrada.
4. Ajuste o bloco operacional do script, se necessário.
5. Execute no RStudio ou com `Rscript monitora_campsav_alvo_global_v2.6.4.R`.
6. Consulte `output/`, `log/` e, quando gerados, `docs/`. Os gráficos ficam em `output/06_graficos`.

## Modos principais

- `completo`: executa o pipeline completo conforme opções ativas.
- `painel_e_parar`: abre o painel de correções assistidas, aplica correções e para após `registros_corrig.csv`.
- `ate_registros_corrig`: materializa `registros_corrig.csv` sem abrir painel automaticamente.
- `abrir_painel_cache`: reabre o painel a partir do cache pré-painel mais recente.
- `painel_incremental_registros_corrig`: continua uma curadoria a partir de `registros_corrig*.csv` colocado deliberadamente em `input/`.
- `registros_corrig_*` e `painel_incremental_*`: retomadas controladas para estatísticas, execução sem PNG ou execução completa.

## Replay semântico

Para reaplicar correções já consolidadas em uma nova execução, preserve o input bruto sem edição manual e copie a trilha semântica anterior para `input/`:

```bash
cp output/02_painel_correcoes/correcoes_semanticas_consolidada.csv input/correcoes_semanticas.csv
```

Depois ative no script:

```r
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
MONITORA_REPLAY_CONTRATO_SEMANTICO_VERSAO <- "replay_semantico_v1"
```

O replay registra intenções de correção e permite reconstruir o estado corrigido a partir do input bruto quando o contrato semântico é preservado.

## Produtos principais

- `output/01_produtos_dados/registros_corrig.csv`: base corrigida e auditável.
- `output/01_produtos_dados/registros_validados.csv`: produto opcional no contrato XLSForm/SISMONITORA, quando habilitado e sem bloqueios.
- `output/01_produtos_dados/registros_corrig_stat.csv`: tabela estatística por COLETA/UC/UA/ano.
- `output/02_painel_correcoes/`: correções semânticas, auditorias, relatórios pré/pós-painel e diagnósticos.
- `output/relatorios_validacao/`: relatório consolidado de validação e arquivos de apoio.
- `output/validacao_espacial/`: produtos de validação espacial, quando habilitada.
- `output/06_graficos/`: gráficos gerados a partir dos produtos de dados, quando habilitados.
- `docs/`: manual do usuário quando habilitado.
- `log/`: execução, performance, auditorias e rastreabilidade.

## Privacidade

Arquivos em `input/`, `output/`, `log/` e `extracted/` podem conter dados sensíveis, como coordenadas, UUIDs, fotos, observações de campo, nomes de UC e dados cadastrais.

Esses arquivos não integram a release pública.

## Histórico recente

- `v2.6.4`: hotfix do contrato do painel (98 atributos editáveis reconciliados), domínios XLSForm normalizados, persistência por chave estável e importação com reparo de mojibake e diagnóstico específico.
- `v2.6.3`: painel operacional, replay semântico, produtos finais, manual e relatório consolidado pós-correção documental.
- `v2.6.1`: painel administrativo, `registros_validados.csv` e relatório de validação.

Consulte `CHANGELOG.md` e `RELEASE_NOTES_v2.6.4.md` para detalhes.

## Como citar

CBC/ICMBio-MMA. 2026. *Script de tratamento, validação e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora*. Versão `v2.6.4`. Repositório GitHub: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
