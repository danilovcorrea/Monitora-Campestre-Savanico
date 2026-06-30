#!/usr/bin/env bash
# =============================================================================
# Bloco 9b — Teste funcional real pequeno da branch v2.6.2
# =============================================================================
#
# Uso mínimo:
#   REGISTROS_ENTRADA=/path/to/registros_corrig.csv \
#   bash tests/run_funcional_real_pequeno_v2_6_2.sh
#
# Com XLSForm (para auditoria 101 e validação de contrato):
#   REGISTROS_ENTRADA=/path/to/registros_corrig.csv \
#   XLSFORM_ENTRADA=/path/to/PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25.xlsx \
#   bash tests/run_funcional_real_pequeno_v2_6_2.sh
#
# Com painel interativo (abre Shiny; bloqueia até fechar):
#   REGISTROS_ENTRADA=/path/to/registros_corrig.csv \
#   ABRIR_PAINEL=S \
#   bash tests/run_funcional_real_pequeno_v2_6_2.sh
#
# Preservar dir temporário para inspeção após o teste:
#   MANTER_TMP=S ...
#
# Variáveis opcionais:
#   N_COLETAS=3                    — número de COLETAs a amostrar automaticamente (1-5; padrão: 3)
#   COLETAS_FIXAS="6005,10813"     — lista de COLETAs específicas separadas por vírgula;
#                                    se preenchida, substitui N_COLETAS para seleção;
#                                    máximo 5; aborta se alguma não existir no arquivo
#   ABRIR_PAINEL=N                 — S abre painel Shiny (interativo, bloqueia)
#   MANTER_TMP=N                   — S preserva $TMP_RUN após execução
#   XLSFORM_ENTRADA=               — caminho para o xlsx do XLSForm (opcional)
#   PROGRESSO_BACKEND=cli          — backend de progresso R: cli, nenhum, etc. (padrão: cli)
#   HEARTBEAT=S                    — S imprime status a cada 30 s durante execução R (padrão: S)
#
# Garante:
#   - nunca altera o diretório de trabalho original (output/, log/, input/)
#   - nunca comita, publica nem altera VERSION/README
#   - usa apenas cópia dos dados; o original não é modificado
#   - todo output vai para /tmp/monitora_funcional_v262_*/
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJETO_DIR="$(dirname "$SCRIPT_DIR")"
SCRIPT_PRINCIPAL="$PROJETO_DIR/monitora_campsav_alvo_global.R"

# -----------------------------------------------------------------------------
# 1. Validações iniciais
# -----------------------------------------------------------------------------
if [[ ! -f "$SCRIPT_PRINCIPAL" ]]; then
  echo "ERRO: script principal nao encontrado: $SCRIPT_PRINCIPAL" >&2
  exit 1
fi

if [[ -z "${REGISTROS_ENTRADA:-}" ]]; then
  echo "" >&2
  echo "ERRO: variavel REGISTROS_ENTRADA nao informada." >&2
  echo "" >&2
  echo "Uso:" >&2
  echo "  REGISTROS_ENTRADA=/path/to/registros_corrig.csv \\" >&2
  echo "  bash tests/run_funcional_real_pequeno_v2_6_2.sh" >&2
  echo "" >&2
  exit 1
fi

if [[ ! -f "$REGISTROS_ENTRADA" ]]; then
  echo "ERRO: arquivo nao encontrado: $REGISTROS_ENTRADA" >&2
  exit 1
fi

XLSFORM_ENTRADA="${XLSFORM_ENTRADA:-}"
N_COLETAS="${N_COLETAS:-3}"
COLETAS_FIXAS="${COLETAS_FIXAS:-}"
ABRIR_PAINEL="${ABRIR_PAINEL:-N}"
ABRIR_PAINEL="${ABRIR_PAINEL^^}"
MANTER_TMP="${MANTER_TMP:-N}"
MANTER_TMP="${MANTER_TMP^^}"
PROGRESSO_BACKEND="${PROGRESSO_BACKEND:-cli}"
HEARTBEAT="${HEARTBEAT:-S}"
HEARTBEAT="${HEARTBEAT^^}"

if ! [[ "$N_COLETAS" =~ ^[0-9]+$ ]] || (( N_COLETAS < 1 || N_COLETAS > 5 )); then
  echo "ERRO: N_COLETAS deve ser inteiro entre 1 e 5 (recebido: $N_COLETAS)" >&2
  exit 1
fi

if [[ -n "$COLETAS_FIXAS" ]]; then
  # Contar elementos: remover espaços, contar vírgulas
  _n_fixas=$(echo "$COLETAS_FIXAS" | tr -cd ',' | wc -c)
  _n_fixas=$(( _n_fixas + 1 ))
  if (( _n_fixas > 5 )); then
    echo "ERRO: COLETAS_FIXAS tem $_n_fixas coletas; maximo permitido e 5." >&2
    exit 1
  fi
fi

if [[ "$ABRIR_PAINEL" != "S" && "$ABRIR_PAINEL" != "N" ]]; then
  echo "ERRO: ABRIR_PAINEL deve ser S ou N (recebido: $ABRIR_PAINEL)" >&2
  exit 1
fi

if [[ "$HEARTBEAT" != "S" && "$HEARTBEAT" != "N" ]]; then
  echo "ERRO: HEARTBEAT deve ser S ou N (recebido: $HEARTBEAT)" >&2
  exit 1
fi

PAINEL_FLAG="$ABRIR_PAINEL"
# Espacial: S quando ABRIR_PAINEL=S (painel interativo); N em modo não-interativo para velocidade.
ESPACIAL_FLAG="N"
[[ "$ABRIR_PAINEL" == "S" ]] && ESPACIAL_FLAG="S"

# -----------------------------------------------------------------------------
# 2. Dir temporário (isolado; MONITORA_BASE_DIR apontará para ele)
# -----------------------------------------------------------------------------
TMP_RUN="$(mktemp -d /tmp/monitora_funcional_v262_XXXXXX)"

_limpar() {
  if [[ "$MANTER_TMP" == "S" ]]; then
    echo ""
    echo "MANTER_TMP=S: diretorio preservado para inspeção: $TMP_RUN"
  else
    echo ""
    echo "Removendo dir temporario: $TMP_RUN"
    rm -rf "$TMP_RUN"
  fi
}
trap _limpar EXIT

echo "============================================================"
echo "Bloco 9b — Teste funcional real pequeno v2.6.2"
echo "============================================================"
echo "Projeto:          $PROJETO_DIR"
echo "Entrada:          $REGISTROS_ENTRADA"
echo "Dir temporario:   $TMP_RUN"
if [[ -n "$COLETAS_FIXAS" ]]; then
  echo "COLETAS_FIXAS:    $COLETAS_FIXAS"
else
  echo "N_COLETAS:        $N_COLETAS"
fi
echo "ABRIR_PAINEL:     $ABRIR_PAINEL"
echo "MANTER_TMP:       $MANTER_TMP"
echo "PROGRESSO_BACKEND: $PROGRESSO_BACKEND"
echo "HEARTBEAT:        $HEARTBEAT"
if [[ -n "$XLSFORM_ENTRADA" ]]; then
  echo "XLSForm:          $XLSFORM_ENTRADA"
fi
echo ""

# -----------------------------------------------------------------------------
# 3. Estrutura de diretórios no temp
# -----------------------------------------------------------------------------
mkdir -p "$TMP_RUN/input"
mkdir -p "$TMP_RUN/output"
mkdir -p "$TMP_RUN/log"
mkdir -p "$TMP_RUN/extracted"

# -----------------------------------------------------------------------------
# 4. Copiar script principal (define MONITORA_BASE_DIR = $TMP_RUN/)
#    O resolver de base usa --file= e dirname() do caminho do script.
#    Copiar (não symlink) garante que normalizePath() aponte para $TMP_RUN/.
# -----------------------------------------------------------------------------
echo "[1/5] Copiando script principal para dir temporario..."
cp "$SCRIPT_PRINCIPAL" "$TMP_RUN/monitora_campsav_alvo_global.R"

# -----------------------------------------------------------------------------
# 5. Amostrar COLETAs da entrada com R (preserva encoding, BOM e CSV)
# -----------------------------------------------------------------------------
if [[ -n "$COLETAS_FIXAS" ]]; then
  echo "[2/5] Filtrando COLETAs fixas de: $REGISTROS_ENTRADA"
  echo "      COLETAS_FIXAS: $COLETAS_FIXAS"
else
  echo "[2/5] Amostrando $N_COLETAS coleta(s) de: $REGISTROS_ENTRADA"
fi

REGISTROS_ENTRADA_ESC="${REGISTROS_ENTRADA//\'/\'\\\'\'}"
COLETAS_FIXAS_ESC="${COLETAS_FIXAS//\'/\'\\\'\'}"
AMOSTRA_DEST="$TMP_RUN/input/registros_corrig.csv"

Rscript --vanilla -e "
suppressPackageStartupMessages(library(data.table))
dt <- data.table::fread('${REGISTROS_ENTRADA_ESC}', encoding = 'UTF-8', nThread = 1L)
if (!('COLETA' %in% names(dt))) {
  stop('Coluna COLETA nao encontrada em REGISTROS_ENTRADA.', call. = FALSE)
}
coletas_fixas_str <- trimws('${COLETAS_FIXAS_ESC}')
if (nzchar(coletas_fixas_str)) {
  solicitadas <- trimws(strsplit(coletas_fixas_str, ',', fixed = TRUE)[[1L]])
  solicitadas <- solicitadas[nzchar(solicitadas)]
  presentes   <- unique(as.character(dt[['COLETA']]))
  ausentes    <- setdiff(solicitadas, presentes)
  if (length(ausentes)) {
    stop('COLETAS_FIXAS solicitadas nao encontradas no arquivo: ',
         paste(ausentes, collapse = ', '), call. = FALSE)
  }
  coletas <- solicitadas
} else {
  coletas <- head(unique(as.character(dt[['COLETA']])), ${N_COLETAS}L)
  coletas <- coletas[!is.na(coletas) & nzchar(coletas)]
  if (!length(coletas)) stop('Nenhuma COLETA valida encontrada.', call. = FALSE)
}
amostra <- dt[COLETA %in% coletas]
data.table::fwrite(amostra, '${AMOSTRA_DEST}', bom = TRUE)
cat('COLETAs selecionadas:', length(coletas), '\n')
cat('Linhas amostradas:   ', nrow(amostra), '\n')
cat('COLETAs:             ', paste(coletas, collapse = ', '), '\n')
" 2>&1 || {
  echo "ERRO: falha ao amostrar entrada. Verifique se data.table esta instalado e REGISTROS_ENTRADA e valido." >&2
  exit 1
}

# -----------------------------------------------------------------------------
# 6. XLSForm (opcional; necessário para auditoria 101 e contrato XLSFORM21)
# -----------------------------------------------------------------------------
echo "[3/5] Preparando XLSForm..."
if [[ -n "$XLSFORM_ENTRADA" ]]; then
  if [[ -f "$XLSFORM_ENTRADA" ]]; then
    cp "$XLSFORM_ENTRADA" "$TMP_RUN/input/"
    echo "      XLSForm copiado: $(basename "$XLSFORM_ENTRADA")"
  else
    echo "      AVISO: XLSFORM_ENTRADA nao encontrado: $XLSFORM_ENTRADA" >&2
    echo "      Auditoria 101 de contrato e validacao XLSFORM21 podem nao funcionar."
  fi
else
  echo "      (XLSFORM_ENTRADA nao informado; auditoria de contrato pode ser parcial)"
fi

# -----------------------------------------------------------------------------
# 7. Modo de execução
# -----------------------------------------------------------------------------
if [[ "$ABRIR_PAINEL" == "S" ]]; then
  MODO="painel_incremental_registros_corrig"
  echo ""
  echo "AVISO: ABRIR_PAINEL=S — modo painel_incremental_registros_corrig."
  echo "       O painel Shiny sera aberto. O script bloqueara ate o painel ser fechado."
  echo "       Nao adequado para execucao nao-interativa/CI."
  echo ""
else
  MODO="registros_corrig_estatisticas_sem_graficos"
fi

EXEC_ID="teste_v262_$(date +%Y%m%d_%H%M%S)"
LOG_PRINCIPAL="$TMP_RUN/log/run_${EXEC_ID}.log"

echo "[4/5] Executando script principal..."
echo "      Modo:    $MODO"
echo "      exec_id: $EXEC_ID"
echo "      Log:     $LOG_PRINCIPAL"
echo ""

# -----------------------------------------------------------------------------
# 8. Execução principal (captura saída sem abortar por exit não-zero)
# -----------------------------------------------------------------------------
set +e
if [[ "$HEARTBEAT" == "N" ]]; then
  env \
    MONITORA_MODO_EXECUCAO="$MODO" \
    MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS="S" \
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES="$PAINEL_FLAG" \
    MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS="N" \
    MONITORA_OPCAO_GERAR_MANUAL_USUARIO="N" \
    MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO="N" \
    MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS="$ESPACIAL_FLAG" \
    MONITORA_OPCAO_SALVAR_CACHE_PAINEL="N" \
    MONITORA_OPCAO_USAR_CACHE_PAINEL="N" \
    MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES="N" \
    MONITORA_PROGRESSO_BACKEND="$PROGRESSO_BACKEND" \
    Rscript "$TMP_RUN/monitora_campsav_alvo_global.R" 2>&1 \
    | tee "$LOG_PRINCIPAL"
  STATUS_R="${PIPESTATUS[0]}"
else
  # Heartbeat: Rscript escreve direto no log; tail -f espelha no terminal.
  # A cada 30 s imprime timestamp, PID, stats de processo, tamanho do log
  # e os 5 arquivos mais recentes em output/ e log/.
  touch "$LOG_PRINCIPAL"
  env \
    MONITORA_MODO_EXECUCAO="$MODO" \
    MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS="S" \
    MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES="$PAINEL_FLAG" \
    MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS="N" \
    MONITORA_OPCAO_GERAR_MANUAL_USUARIO="N" \
    MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO="N" \
    MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS="$ESPACIAL_FLAG" \
    MONITORA_OPCAO_SALVAR_CACHE_PAINEL="N" \
    MONITORA_OPCAO_USAR_CACHE_PAINEL="N" \
    MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES="N" \
    MONITORA_PROGRESSO_BACKEND="$PROGRESSO_BACKEND" \
    Rscript "$TMP_RUN/monitora_campsav_alvo_global.R" >> "$LOG_PRINCIPAL" 2>&1 &
  RSCRIPT_PID=$!

  tail -f "$LOG_PRINCIPAL" &
  TAIL_PID=$!

  (
    while kill -0 "$RSCRIPT_PID" 2>/dev/null; do
      sleep 30
      kill -0 "$RSCRIPT_PID" 2>/dev/null || break
      echo ""
      echo "--- [heartbeat] $(date '+%Y-%m-%d %H:%M:%S') ---"
      echo "    PID Rscript: $RSCRIPT_PID"
      if command -v ps &>/dev/null; then
        ps -p "$RSCRIPT_PID" -o pid,etime,%cpu,%mem --no-headers 2>/dev/null \
          | awk '{printf "    pid=%-8s etime=%-12s cpu=%-6s mem=%s\n",$1,$2,$3,$4}' || true
      fi
      _log_sz="$(du -sh "$LOG_PRINCIPAL" 2>/dev/null | cut -f1)"
      echo "    Log: ${_log_sz:-?} — $(basename "$LOG_PRINCIPAL")"
      echo "    Ultimos 5 arquivos modificados em output/ e log/:"
      find "$TMP_RUN/output" "$TMP_RUN/log" -type f -printf '%T@ %p\n' 2>/dev/null \
        | sort -rn | head -5 \
        | while read -r _ts _fp; do
            echo "      ${_fp#"$TMP_RUN/"}"
          done
    done
  ) &
  HB_PID=$!

  wait "$RSCRIPT_PID"
  STATUS_R=$?
  sleep 1
  kill "$TAIL_PID" 2>/dev/null || true
  wait "$TAIL_PID" 2>/dev/null || true
  kill "$HB_PID" 2>/dev/null || true
  wait "$HB_PID" 2>/dev/null || true
fi
set -e

# -----------------------------------------------------------------------------
# 9. Resumo dos produtos
# -----------------------------------------------------------------------------
echo ""
echo "[5/5] Inspecionando resultados..."
echo ""
echo "============================================================"
echo "RESUMO — Teste funcional real pequeno v2.6.2"
echo "============================================================"
echo ""

# --- registros_corrig.csv ---
echo ">> registros_corrig.csv"
RC_CANONICO="$TMP_RUN/output/01_produtos_dados/registros_corrig.csv"
RC_RAIZ="$TMP_RUN/output/registros_corrig.csv"
if [[ -f "$RC_CANONICO" ]]; then
  echo "   GERADO (canonico): $RC_CANONICO"
  Rscript --vanilla -e "
    dt <- data.table::fread('${RC_CANONICO}', nThread=1L)
    cat('   Linhas:', nrow(dt), '| Colunas:', ncol(dt), '\n')
  " 2>/dev/null || true
elif [[ -f "$RC_RAIZ" ]]; then
  echo "   GERADO (raiz): $RC_RAIZ"
  Rscript --vanilla -e "
    dt <- data.table::fread('${RC_RAIZ}', nThread=1L)
    cat('   Linhas:', nrow(dt), '| Colunas:', ncol(dt), '\n')
  " 2>/dev/null || true
else
  echo "   NAO GERADO — verificar log para erro ou checkpoint"
fi

# --- registros_validados.csv ---
echo ""
echo ">> registros_validados.csv"
RV_CANONICO="$TMP_RUN/output/01_produtos_dados/registros_validados.csv"
RV_RAIZ="$TMP_RUN/output/registros_validados.csv"
if [[ -f "$RV_CANONICO" || -f "$RV_RAIZ" ]]; then
  echo "   GERADO — ATENÇÃO: se havia pendências impeditivas, isso indica falha no bloqueio"
  [[ -f "$RV_CANONICO" ]] && echo "   Caminho: $RV_CANONICO"
  [[ -f "$RV_RAIZ"     ]] && echo "   Caminho: $RV_RAIZ"
else
  echo "   NAO GERADO (esperado quando ha pendencias impeditivas — bloqueio OK)"
fi

# --- Auditorias de pendências ---
echo ""
echo ">> Auditorias de pendencias impeditivas"
PEND_DIR="$TMP_RUN/output/03_auditorias/pendencias_impeditivas"
if [[ -d "$PEND_DIR" ]]; then
  find "$PEND_DIR" -maxdepth 1 -type f | sort | while read -r arq; do
    NLIN="$(Rscript --vanilla -e "cat(nrow(data.table::fread('${arq}', nThread=1L)), '\n')" 2>/dev/null || echo '?')"
    printf "   %-55s (%s linhas)\n" "$(basename "$arq")" "$NLIN"
  done
else
  echo "   Nenhum arquivo em $PEND_DIR"
fi

# --- Auditoria 101 / Encostam (log) ---
echo ""
echo ">> Auditorias 101 e Encostam (log/)"
find "$TMP_RUN/log" -maxdepth 1 -name "*101*" -o -name "*encostam*" -o \
     -name "*pendencias*" -o -name "*impeditiv*" 2>/dev/null | sort | \
  while read -r arq; do echo "   $arq"; done || echo "   (nenhum encontrado)"

# --- Outros CSVs relevantes em output ---
echo ""
echo ">> Outros arquivos de auditoria em output/"
find "$TMP_RUN/output" -name "*.csv" -not -path "*/01_produtos_dados/*" \
     2>/dev/null | sort | head -20 | while read -r arq; do
  printf "   %s\n" "${arq#"$TMP_RUN/output/"}"
done

# --- Status do Rscript ---
echo ""
echo ">> Status da execucao R"
if [[ $STATUS_R -eq 0 ]]; then
  echo "   OK (exit 0)"
else
  echo "   FALHOU com exit $STATUS_R"
  echo "   --- Ultimas 30 linhas do log ---"
  tail -30 "$LOG_PRINCIPAL" | sed 's/^/   /'
fi

# --- Status git (confirma que nada foi alterado no projeto) ---
echo ""
echo ">> Status git (projeto — deve estar limpo ou sem alteracoes pelo teste)"
git -C "$PROJETO_DIR" status --short

echo ""
echo "Dir temporario: $TMP_RUN"
echo "============================================================"
