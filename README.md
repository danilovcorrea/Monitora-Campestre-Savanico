# Monitora Campestre-Savânico

Script de tratamento, validação e análise de dados do **Alvo Global Plantas Herbáceas e Lenhosas do Componente Campestre Savânico** do **Programa Monitora**.

**Versão pública atual:** `v2.6.1`  
**Script principal:** [`monitora_campsav_alvo_global_v2.6.1.R`](https://github.com/danilovcorrea/Monitora-Campestre-Savanico/blob/main/monitora_campsav_alvo_global_v2.6.1.R)

## Finalidade

O script lê, padroniza, audita, deduplica, corrige e analisa registros exportados do SISMONITORA para campanhas amostrais do alvo global Plantas Herbáceas e Lenhosas. Também pode abrir painel Shiny para correções assistidas, validar espacialmente COLETAS, gerar relatórios de apoio, estatísticas, gráficos e produtos compatíveis com o contrato XLSForm/SISMONITORA quando habilitado.

## Novidades da v2.6.1

- Painel de correções assistidas com atributos administrativos controlados pelo contrato SISMONITORA/XLSForm 2025.
- `registros_corrig.csv` tratado como produto mestre organizado, saneado e auditado antes da exportação contratual.
- `registros_validados.csv` gerado por seleção e reordenação dos 129 atributos do template XLSForm/SISMONITORA 2025, quando não houver pendências impeditivas.
- Relatório consolidado de validação em HTML por padrão, com auditorias de apoio e rastreabilidade das decisões.
- Correções de aliases administrativos, estruturais, datas, formação vegetacional e cardinalidade de UUID na auditoria final.

## Destaques preservados da v2.6.0



- Roll-forward semântico de correções anteriores por `input/correcoes_semanticas.csv`.
- Contrato público de replay `replay_semantico_v1`, com diagnóstico não abortivo por padrão.
- Comparação com run-oráculo disponível como recurso avançado, mas desligada por padrão.
- Relatório consolidado de validação com cadeia de modificações, sanitizações automáticas, operações do painel e descrição dos produtos de dados.
- Manual do usuário com passo a passo dos modos de execução e orientação operacional para bolsistas.
- Padrões públicos seguros: execução completa sem painel, sem geração automática de produtos sensíveis opcionais e sem validação espacial automática.

## Uso básico

1. Crie uma pasta de execução limpa.
2. Copie `monitora_campsav_alvo_global_v2.6.1.R` para a raiz da pasta.
3. Crie `input/` e coloque ali somente os arquivos brutos de entrada.
4. Ajuste o bloco operacional do script, se necessário.
5. Execute no RStudio ou com `Rscript monitora_campsav_alvo_global_v2.6.1.R`.
6. Consulte `output/`, `log/` e, quando gerados, `docs/`.

## Modos principais

- `completo`: executa o pipeline completo conforme opções ativas.
- `painel_e_parar`: abre o painel de correções assistidas, aplica correções e para após `output/registros_corrig.csv`.
- `ate_registros_corrig`: materializa `registros_corrig.csv` sem abrir painel automaticamente.
- `abrir_painel_cache`: reabre painel a partir do cache pré-painel mais recente.
- `painel_incremental_registros_corrig`: continua uma curadoria a partir de `registros_corrig*.csv` colocado deliberadamente em `input/`.
- `registros_corrig_*` e `painel_incremental_*`: retomadas controladas para estatísticas, execução sem PNG ou execução completa.

## Roll-forward semântico

Para reaplicar correções já consolidadas em uma nova execução, preserve o input bruto sem edição manual, copie:

```bash
cp output/correcoes_campos/correcoes_semanticas_consolidada.csv input/correcoes_semanticas.csv
```

e altere no script:

```r
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES <- "S"
```

O arquivo `correcoes_semanticas.csv` registra intenções de correção, não apenas índices de linha, permitindo reconstruir o estado corrigido a partir do input bruto quando o contrato semântico for preservado. Em transições de versão, pode-se ativar temporariamente a comparação com run-oráculo para auditoria.

## Produtos principais

- `output/registros_corrig.csv`: base corrigida e auditável.
- `output/registros_corrig_stat.csv`: tabela estatística por COLETA/UC/UA/ano.
- `output/registros_validados.csv`: produto opcional no contrato de estrutura/formato, quando habilitado.
- `output/correcoes_campos/`: correções semânticas, auditorias, relatórios pré/pós-painel e diagnósticos.
- `output/relatorios_validacao/`: relatório consolidado de validação e arquivos de apoio.
- `output/validacao_espacial/`: produtos de validação espacial, quando habilitada.
- `log/`: execução, performance, auditorias e rastreabilidade.

## Privacidade

Arquivos em `input/`, `output/`, `log/` e `extracted/` podem conter dados sensíveis, como coordenadas, UUIDs, fotos, observações de campo, nomes de UC e dados cadastrais. Esses arquivos não integram a release pública.

## Como citar

CBC/ICMBio-MMA. 2026. *Script de tratamento, validação e análise de dados do Alvo Global Plantas Herbáceas e Lenhosas do Componente Campestre Savânico do Programa Monitora*. Versão `v2.6.1`. Repositório GitHub: https://github.com/danilovcorrea/Monitora-Campestre-Savanico

## Repositório

https://github.com/danilovcorrea/Monitora-Campestre-Savanico

## Atualização v2.6.1

A versão v2.6.1 consolida o painel de correções assistidas com atributos administrativos, reforça o contrato XLSForm/SISMONITORA 2025 para `registros_corrig.csv` e gera `registros_validados.csv` por seleção e reordenação dos 129 atributos do template, quando não houver pendências impeditivas.

O relatório consolidado de validação passa a priorizar HTML por padrão. Arquivo principal versionado: [`monitora_campsav_alvo_global_v2.6.1.R`](https://github.com/danilovcorrea/Monitora-Campestre-Savanico/blob/main/monitora_campsav_alvo_global_v2.6.1.R). Notas da release: [`RELEASE_NOTES_v2.6.1.md`](https://github.com/danilovcorrea/Monitora-Campestre-Savanico/blob/main/RELEASE_NOTES_v2.6.1.md).
