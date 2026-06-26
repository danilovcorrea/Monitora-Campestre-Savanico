# v2.5.4 - Contrato XLSForm, registros validados e governança de dados

Esta versão consolida o fluxo público de auditoria e geração dos produtos finais do Monitora Campestre-Savânico.

## Destaques

- Fechamento contratual de `registros_corrig.csv` antes da materialização do produto final.
- Geração auditável de `registros_validados.csv` com 129 atributos na ordem do contrato/template.
- Padronização de datas, horas, domínios XLSForm e campos condicionais.
- Tratamento consistente de ausências: `registros_corrig.csv` usa `NA` físico; `registros_validados.csv` mantém vazio efetivo.
- Sanitização de resíduos de traços em campos vazios.
- Correção semântica para registros históricos de `canela_de_ema`, preservando a classificação como forma de vida nativa.
- Painel de correções assistidas mantido como recurso opcional e auditável.
- Modo sem gráficos disponível para fluxos operacionais mais rápidos.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.5.4.R`
- `monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
- `Monitora-Campestre-Savanico_v2.5.4_release_completa.zip`
