# Comandos sugeridos para publicação da v2.2.0

Executar a partir da raiz do repositório após copiar os arquivos revisados.

```bash
VERSAO="2.2.0"
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

git status --short

git add \
  README.md \
  CHANGELOG.md \
  VERSION \
  SHA256SUMS.txt \
  COMANDOS_PUBLICACAO_v2.2.0.md \
  docs/uso_de_ia.md \
  docs/versionamento.md \
  monitora_campsav_alvo_global_v2.2.0.R \
  monitora_campsav_alvo_global.R \
  MONITORA_CAMPSAV_Alvo_Global.R \
  R/monitora_campsav_alvo_global.R \
  releases/v2.2.0/

git commit -m "release: publica v2.2.0 com painel de validação assistida"
git tag -a "${TAG}" -m "v2.2.0 - Painel de validação assistida de registros_corrig"
git push origin main
git push origin "${TAG}"
```
