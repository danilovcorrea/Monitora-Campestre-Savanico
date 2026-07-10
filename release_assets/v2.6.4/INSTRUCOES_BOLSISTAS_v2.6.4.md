# Instruções para bolsistas — Monitora Campestre-Savânico v2.6.4

## Preparar a execução

1. Crie uma pasta nova para a rodada.
2. Copie `monitora_campsav_alvo_global_v2.6.4.R` para a raiz dessa pasta.
3. Crie uma subpasta chamada `input`.
4. Coloque em `input/` somente os arquivos brutos que serão processados.
5. Não coloque arquivos brutos, CSVs, ZIPs ou XLSX na raiz da pasta.

## Rodar sem painel

Use o padrão:

```r
MONITORA_MODO_EXECUCAO <- "completo"
MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"
```

Depois execute o script no RStudio ou com `Rscript`.

## Rodar com painel de curadoria

Para abrir o painel:

```r
MONITORA_MODO_EXECUCAO <- "painel_e_parar"
```

O script abrirá o Shiny, permitirá as correções e parará após gravar `registros_corrig.csv`. O seletor
"Atributo a corrigir" traz 98 atributos editáveis; nenhum deles deve ficar bloqueado depois de selecionado
no botão "Adicionar correção" — se isso acontecer, é um problema a reportar, não um comportamento esperado.

## Operações principais do painel

- **EXCCOL**: excluir coletas duplicadas.
- **PENDHAB**: preencher hábito obrigatório (`epifita`, `terrestre`, `rupicola`) em lote ou individualmente.
- **SANEORF**: limpar órfãos em `Encostam`.
- **TRIDESC**: substituir forma desconhecida por forma válida.
- **TRIOUT**: limpar outras formas de vida.
- **Correção simples/lote**: corrigir campos permitidos pelo contrato.

## Salvar e fechar

Ao terminar a curadoria, use a opção de salvar/fechar do painel. Guarde:

- `output/01_produtos_dados/registros_corrig.csv`
- `output/02_painel_correcoes/correcoes_semanticas_consolidada.csv`
- relatórios em `output/relatorios_validacao/`
- gráficos em `output/06_graficos/`, quando habilitados
- logs e auditorias da rodada

## Se a importação for bloqueada por resíduo em campo estruturado

Em alguns casos, o script bloqueia a importação de uma COLETA/UA porque um campo estruturado (lista de
valores separada por `|`) não tem correspondência segura com os pontos amostrais esperados. Isso é
proposital — protege a integridade dos dados. O relatório de bloqueio identifica a COLETA/UA/coluna/faixa
de pontos afetados; a correção precisa ser feita nos dados de origem (arquivo/planilha de campo), nunca
inventando ou repetindo valores para preencher os pontos que faltam.

## Usar replay semântico em rodada futura

1. Preserve o input bruto original.
2. Copie a trilha anterior:

```bash
cp output/02_painel_correcoes/correcoes_semanticas_consolidada.csv input/correcoes_semanticas.csv
```

3. No script, ative:

```r
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
MONITORA_REPLAY_CONTRATO_SEMANTICO_VERSAO <- "replay_semantico_v1"
```

O replay ocorre antes do painel e antes dos relatórios pré-painel.

## Cuidados de privacidade

Não compartilhe publicamente `input/`, `output/`, `log/`, `extracted/`, CSVs, XLSX, ZIPs de dados, KMLs reais ou produtos gerados com dados institucionais.
