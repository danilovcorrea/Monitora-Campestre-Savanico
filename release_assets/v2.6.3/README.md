# Monitora Campestre-Savânico

Script de tratamento, validação e análise de dados do **Alvo Global Plantas Herbáceas e Lenhosas do Componente Campestre Savânico** do **Programa Monitora**.

**Versão pública atual:** `v2.6.3`
**Script principal:** [`monitora_campsav_alvo_global_v2.6.3.R`](monitora_campsav_alvo_global_v2.6.3.R)

## Finalidade

O script lê, padroniza, audita, deduplica, corrige e analisa registros exportados do SISMONITORA para campanhas amostrais do alvo global Plantas Herbáceas e Lenhosas.

Também pode abrir painel Shiny para correções assistidas, validar espacialmente COLETAS, gerar relatórios de apoio, estatísticas, gráficos, KMLs e produtos compatíveis com o contrato XLSForm/SISMONITORA quando habilitado.

## Novidades da v2.6.3

- Publica a versão v2.6.3 validada, com painel operacional e produtos finais completos.
- Mantém operações semânticas auditáveis: EXCCOL, PENDHAB, SANEORF, TRIDESC/TRIOUT e correções simples/lote.
- Preserva o replay semântico por `correcoes_semanticas_consolidada.csv` e contrato `replay_semantico_v1`.
- Gera manual do usuário e relatório consolidado de validação quando habilitados.
- Incorpora a correção documental da seção 5.2 do relatório consolidado, evitando tabelas quebradas com cabeçalhos `V1..Vn`.

## Padrões públicos seguros

A execução pública permanece segura por padrão:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS <- "N"
MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS <- "N"
MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS <- "N"
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "N"
```

Para curadoria assistida por bolsistas, use:

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
```

Os modos orientados a painel forçam internamente a abertura do Shiny mesmo que o padrão público seja painel desligado.

## Uso básico

1. Crie uma pasta de execução limpa.
2. Copie `monitora_campsav_alvo_global_v2.6.3.R` para a raiz.
3. Crie `input/` e coloque ali somente os arquivos brutos de entrada.
4. Ajuste o bloco operacional do script, se necessário.
5. Execute no RStudio ou com `Rscript monitora_campsav_alvo_global_v2.6.3.R`.
6. Consulte `output/`, `log/` e, quando gerados, `docs/`.

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
- `docs/`: manual do usuário quando habilitado.
- `log/`: execução, performance, auditorias e rastreabilidade.

## Privacidade

Arquivos em `input/`, `output/`, `log/` e `extracted/` podem conter dados sensíveis, como coordenadas, UUIDs, fotos, observações de campo, nomes de UC e dados cadastrais.

Esses arquivos não integram a release pública.

## Histórico recente

- `v2.6.3`: painel operacional, replay semântico, produtos finais, manual e relatório consolidado pós-correção documental.
- `v2.6.1`: painel administrativo, `registros_validados.csv` e relatório de validação.
- `v2.6.0`: roll-forward semântico, relatório consolidado e governança de validação.

Consulte `CHANGELOG.md` e `RELEASE_NOTES_v2.6.3.md` para detalhes.

## Como citar

CBC/ICMBio-MMA. 2026. *Script de tratamento, validação e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa Monitora*. Versão `v2.6.3`. Repositório GitHub: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
