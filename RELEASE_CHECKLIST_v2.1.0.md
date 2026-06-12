# Checklist de publicação — v2.1.0

## Conferências antes do commit

```bash
cd ~/Projetos/Monitora-Campestre-Savanico

VERSAO="2.1.0"
TAG="v${VERSAO}"

sha256sum \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

grep -Rni "Versão pública" \
  "monitora_campsav_alvo_global_${TAG}.R" \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  "releases/${TAG}/monitora_campsav_alvo_global_${TAG}.R"

cat VERSION
```

Critérios:

- os cinco scripts devem ter o mesmo hash SHA256;
- todos devem indicar `Versão pública: v2.1.0`;
- `VERSION` deve conter somente `2.1.0`;
- os plots validados localmente devem estar aceitáveis;
- `README.md` e `CHANGELOG.md` devem estar atualizados.

## Comandos sugeridos de publicação

```bash
git status

git add \
  VERSION \
  README.md \
  CHANGELOG.md \
  monitora_campsav_alvo_global_v2.1.0.R \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  releases/v2.1.0/monitora_campsav_alvo_global_v2.1.0.R

git commit -m "release: publica v2.1.0"

git tag -a v2.1.0 -m "v2.1.0 - Gráficos editoriais, coortes e auditorias consolidadas"

git push origin main

git push origin v2.1.0
```

## Texto curto para release no GitHub

```text
v2.1.0 consolida a evolução desde v2.0.2, com gráficos temporais editoriais, escopos amostrais explícitos, painéis pareados, análise de coortes, relatório textual estatístico, auditorias ampliadas e refinamento de rótulos, símbolos estatísticos, margens e legendas inferiores.
```
