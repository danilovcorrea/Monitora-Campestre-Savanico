### Script de tratamento e análise de dados do Alvo Global do Componente Campestre Savânico do
### Programa Monitora
### Versão pública: v2.0.2
###
### Finalidade:
###   Ler, padronizar, auditar, deduplicar e analisar registros do SISMONITORA para o alvo
###   Plantas Herbáceas e Lenhosas do Componente Campestre Savânico.
###
### Entradas aceitas:
###   - arquivos ZIP exportados individualmente pelo SISMONITORA;
###   - arquivos ZIP/CSV/XLSX de exportação em lote;
###   - arquivos registros_corrig*.csv gerados por execuções anteriores, quando usados como nova
###   entrada.
###
### Saídas principais:
###   - registros_corrig.csv: registros padronizados, deduplicados e auditáveis;
###   - registros_corrig_stat.csv: tabela estatística por UC, UA e ano;
###   - output/: tabelas analíticas, gráficos e arquivos geográficos, quando habilitados;
###   - log/: relatórios de execução, auditorias, performance, memória e rastreabilidade de
###   arquivos.
###
### Uso básico:
###   1. Coloque este script no diretório de trabalho do projeto.
###   2. Coloque os arquivos de entrada no subdiretório input/ ou no mesmo diretório do script.
###   3. Execute o script completo no RStudio.
###   4. Ao final, consulte os produtos em output/ e os relatórios de auditoria em log/.
###
### Observações:
###   - Não é necessário extrair manualmente os arquivos ZIP.
###   - O script usa controle de origem para evitar reprocessar output/, log/ e extrações antigas.
###   - Parâmetros de performance e memória podem ser ajustados por variáveis de ambiente.
###
### Citação:
###   CBC - ICMBio/MMA, 2026. Scripts de tratamento e análise de dados do Alvo Global Plantas
###   Herbáceas e Lenhosas, Nativas e Exóticas do Componente Campestre Savânico do Programa
###   Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.
###
### Repositório: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
### Contato: danilo.correa@icmbio.gov.br

### Verificação e carregamento dos pacotes necessários:

if (!require("rstudioapi"))
  install.packages("rstudioapi")
library("rstudioapi")
if (!require("dplyr"))
  install.packages("dplyr")
library("dplyr")
if (!require("data.table"))
  install.packages("data.table")
library("data.table")
if (!require("purrr"))
  install.packages("purrr")
library("purrr")
if (!require("stringr"))
  install.packages("stringr")
library("stringr")
if (!require("tidyverse"))
  install.packages("tidyverse")
library("tidyverse")
if (!require("ggplot2"))
  install.packages("ggplot2")
library("ggplot2")
if (!require("ggrepel"))
  install.packages("ggrepel")
library("ggrepel")
if (!require("readxl"))
  install.packages("readxl")
library("readxl")
if (!require("openxlsx"))
  install.packages("openxlsx")
library("openxlsx")
if (!require("sf"))
  install.packages("sf")
library("sf")


### Definição robusta do diretório de trabalho como o diretório onde está o script
###
### Observação: em algumas formas de execução no RStudio,
### rstudioapi::getActiveDocumentContext()$path pode retornar caminho vazio,
### NA, caminho temporário ou caminho de arquivo ainda não salvo.
### Por isso, a resolução abaixo tenta múltiplas fontes e só chama setwd()
### quando o diretório existe. Caso contrário, mantém getwd() e registra aviso.

monitora_resolver_base_dir <- function() {
  candidatos <- character(0)

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    # 1) Documento ativo no RStudio
    ctx_path <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                         error = function(e) NA_character_)
    if (!is.null(ctx_path) && length(ctx_path) > 0) {
      candidatos <- c(candidatos, ctx_path[1])
    }

    # 2) Documento-fonte aberto no RStudio, quando disponível
    src_path <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                         error = function(e) NA_character_)
    if (!is.null(src_path) && length(src_path) > 0) {
      candidatos <- c(candidatos, src_path[1])
    }
  }

  # 3) Caminho disponível quando o script é executado por source().
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
  if (!is.null(ofile) && length(ofile) > 0) {
    candidatos <- c(candidatos, ofile[1])
  }

  # 4) Argumento --file=, útil para execução por Rscript.
  cmd_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(cmd_file) > 0) {
    candidatos <- c(candidatos, sub("^--file=", "", cmd_file[1]))
  }

  candidatos <- unique(candidatos)
  candidatos <- candidatos[!is.na(candidatos) & nzchar(candidatos)]

  for (cand in candidatos) {
    cand <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    dir_cand <- if (dir.exists(cand)) cand else dirname(cand)
    dir_cand <- normalizePath(dir_cand, winslash = "/", mustWork = FALSE)
    if (dir.exists(dir_cand)) return(dir_cand)
  }

  warning(
    "Não foi possível identificar com segurança o diretório do script. ",
    "Mantendo o diretório de trabalho atual: ", getwd(),
    call. = FALSE
  )
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

MONITORA_BASE_DIR <- monitora_resolver_base_dir()

if (!dir.exists(MONITORA_BASE_DIR)) {
  stop("MONITORA_BASE_DIR não existe: ", MONITORA_BASE_DIR)
}

tryCatch(
  setwd(MONITORA_BASE_DIR),
  error = function(e) {
    stop(
      "Não foi possível definir o diretório de trabalho para MONITORA_BASE_DIR: ",
      MONITORA_BASE_DIR,
      "\nDiretório atual: ", getwd(),
      "\nErro original: ", conditionMessage(e),
      call. = FALSE
    )
  }
)

message("MONITORA_BASE_DIR definido como: ", MONITORA_BASE_DIR)

### Estrutura defensiva de pastas e relatório de execução
### - input/: opcional; se existir e tiver arquivos, será a origem preferencial dos ZIP/CSV/XLSX
### brutos.
### - extracted/: destino limpo das extrações recursivas.
### - output/: destino das tabelas, KML e gráficos.
### - log/: destino dos relatórios de auditoria da execução.

MONITORA_INPUT_DIR <- file.path(MONITORA_BASE_DIR, "input")
MONITORA_EXTRACT_DIR <- file.path(MONITORA_BASE_DIR, "extracted")
MONITORA_OUTPUT_DIR <- file.path(MONITORA_BASE_DIR, "output")
MONITORA_LOG_DIR <- file.path(MONITORA_BASE_DIR, "log")

dir.create(MONITORA_INPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MONITORA_EXTRACT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MONITORA_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MONITORA_LOG_DIR, showWarnings = FALSE, recursive = TRUE)

MONITORA_EXEC_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
MONITORA_REPORT <- data.table(
  etapa = character(),
  severidade = character(),
  arquivo = character(),
  detalhe = character(),
  acao = character()
)

monitora_log <- function(etapa, severidade = "INFO", arquivo = NA_character_, detalhe = NA_character_, acao = NA_character_) {
  # Mantém o log de execução em estrutura data.table, usando rbindlist() para reduzir sobrecarga
  # em relação a rbind() incremental.
  MONITORA_REPORT <<- data.table::rbindlist(
    list(
      MONITORA_REPORT,
      data.table::data.table(
        etapa = as.character(etapa),
        severidade = as.character(severidade),
        arquivo = as.character(arquivo),
        detalhe = as.character(detalhe),
        acao = as.character(acao)
      )
    ),
    fill = TRUE,
    use.names = TRUE
  )
  invisible(TRUE)
}

monitora_as_dt_ref <- function(x) {
  # Converte para data.table por referência sempre que possível.
  # Evita cópias desnecessárias de objetos grandes.
  if (!data.table::is.data.table(x)) data.table::setDT(x)
  x
}


monitora_join_padronizar_chaves <- function(x, y, by) {
  # Padroniza tipos das chaves antes das junções com data.table.
  # Isso evita erros como integer vs character em ANO e reduz coerções implícitas caras.
  for (cc in by) {
    if (!(cc %in% names(x)) || !(cc %in% names(y))) next
    cx <- class(x[[cc]])[1]
    cy <- class(y[[cc]])[1]
    if (!identical(cx, cy)) {
      data.table::set(x, j = cc, value = as.character(x[[cc]]))
      data.table::set(y, j = cc, value = as.character(y[[cc]]))
    }
  }
  invisible(TRUE)
}

monitora_dt_unique_por_chaves <- function(dt, by, contexto = "join") {
  # Garante uma linha por chave antes das junções de anotação ou estatística.
  # Evita explosões muitos-para-muitos em data.table sem usar allow.cartesian=TRUE.
  dt <- monitora_as_dt_ref(dt)
  by <- by[by %in% names(dt)]
  if (length(by) == 0 || nrow(dt) == 0) return(dt)
  dup <- dt[, .N, by = by][N > 1L]
  if (nrow(dup) > 0L) {
    if (exists("monitora_stat_msg", mode = "function")) {
      monitora_stat_msg(sprintf(
        "deduplicando %s antes do join: %s chave(s) duplicada(s)",
        contexto,
        format(nrow(dup), big.mark = ".", decimal.mark = ",")
      ))
    }
    dt <- unique(dt, by = by)
  }
  dt[]
}

monitora_dt_left_join <- function(x, y, by) {
  # Junção à esquerda com chaves data.table, preservando a ordem original de x.
  # Evita o merge() genérico e reduz cópias em junções frequentes.
  x <- monitora_as_dt_ref(x)
  y <- monitora_as_dt_ref(y)
  by <- by[by %in% names(x) & by %in% names(y)]
  if (length(by) == 0) return(x)
  ord_col <- ".MONITORA_ORDEM_JOIN_TMP"
  while (ord_col %in% names(x) || ord_col %in% names(y)) ord_col <- paste0(ord_col, "_")
  data.table::set(x, j = ord_col, value = seq_len(nrow(x)))
  monitora_join_padronizar_chaves(x, y, by)
  y <- monitora_dt_unique_por_chaves(y, by, contexto = "tabela direita em left_join")
  data.table::setkeyv(x, by)
  data.table::setkeyv(y, by)
  out <- y[x]
  data.table::setorderv(out, ord_col)
  out[, (ord_col) := NULL]
  if (ord_col %in% names(x)) x[, (ord_col) := NULL]
  out[]
}

monitora_dt_full_join_list <- function(lst, by) {
  # Junção completa de várias tabelas por data.table: primeiro cria a união das chaves,
  # depois anexa cada tabela por junção à esquerda chaveada. Evita Reduce()+merge().
  lst <- Filter(function(z) !is.null(z) && nrow(z) > 0, lst)
  if (length(lst) == 0) return(data.table::data.table())
  lst <- lapply(lst, monitora_as_dt_ref)
  by <- by[Reduce(`&`, lapply(lst, function(z) by %in% names(z)))]
  if (length(by) == 0) return(data.table::rbindlist(lst, fill = TRUE, use.names = TRUE))
  keys <- data.table::rbindlist(lapply(lst, function(z) unique(z[, ..by])), fill = TRUE, use.names = TRUE)
  keys <- unique(keys)
  data.table::setkeyv(keys, by)
  out <- keys
  for (ii in seq_along(lst)) {
    z <- lst[[ii]]
    monitora_join_padronizar_chaves(out, z, by)
    z <- monitora_dt_unique_por_chaves(z, by, contexto = paste0("tabela ", ii, " em full_join"))
    data.table::setkeyv(out, by)
    data.table::setkeyv(z, by)
    out <- z[out]
    if (exists("monitora_deve_gc", mode = "function") && monitora_deve_gc()) gc(verbose = FALSE)
  }
  out[]
}

monitora_move_before <- function(dt, cols, before) {
  # Reposiciona colunas por referência, sem copiar objetos grandes.
  if (!data.table::is.data.table(dt)) data.table::setDT(dt)
  cols <- cols[cols %in% names(dt)]
  if (length(cols) == 0 || !(before %in% names(dt))) return(invisible(dt))
  base <- setdiff(names(dt), cols)
  pos <- match(before, base)
  if (is.na(pos)) return(invisible(dt))
  new_order <- append(base, cols, after = pos - 1L)
  data.table::setcolorder(dt, new_order)
  invisible(dt)
}

monitora_sum_cols_dt <- function(dt, cols) {
  # Soma colunas por linha usando .SD; em geral é usado em registros_corrig_stat,
  # que é menor que registros_corrig, mas ainda deve evitar cópias desnecessárias.
  if (!data.table::is.data.table(dt)) data.table::setDT(dt)
  cols <- cols[cols %in% names(dt)]
  if (length(cols) == 0) return(rep(0, nrow(dt)))
  dt[, rowSums(.SD, na.rm = TRUE), .SDcols = cols]
}

monitora_split_coord_cols_dt <- function(dt, col, novos) {
  # Divide coordenadas já normalizadas por espaço, sem depender de objetos intermediários grandes.
  if (!data.table::is.data.table(dt)) data.table::setDT(dt)
  if (!(col %in% names(dt))) {
    for (nm in novos) data.table::set(dt, j = nm, value = NA_character_)
    return(invisible(dt))
  }
  parts <- data.table::tstrsplit(as.character(dt[[col]]), "[[:space:]]+", perl = TRUE, keep = seq_along(novos), type.convert = FALSE)
  for (i in seq_along(novos)) {
    val <- if (length(parts) >= i && !is.null(parts[[i]])) parts[[i]] else rep(NA_character_, nrow(dt))
    data.table::set(dt, j = novos[i], value = val)
  }
  invisible(dt)
}

### Controle dinâmico de memória e processamento
### Objetivo: preservar velocidade quando há memória disponível e reduzir uso de recursos
### somente quando o volume de dados ou o ambiente indicarem risco de estouro de RAM.
### Perfis configuráveis por variável de ambiente:
### MONITORA_PERFIL_EXECUCAO = "auto" (padrão: equilíbrio adaptativo)
### MONITORA_PERFIL_EXECUCAO = "rapido" (maior performance; exige RAM folgada)
### MONITORA_PERFIL_EXECUCAO = "economico" (menor pico de RAM; pode ser mais lento)
monitora_env_bool <- function(nome, default = FALSE) {
  val <- Sys.getenv(nome, unset = ifelse(default, "true", "false"))
  tolower(trimws(val)) %in% c("1", "true", "t", "sim", "s", "yes", "y")
}
monitora_env_int <- function(nome, default) {
  val <- suppressWarnings(as.integer(Sys.getenv(nome, unset = as.character(default))))
  ifelse(is.na(val) || val < 1L, as.integer(default), val)
}
monitora_env_chr <- function(nome, default) {
  val <- Sys.getenv(nome, unset = default)
  val <- tolower(trimws(val))
  if (!nzchar(val)) default else val
}

MONITORA_PERFIL_EXECUCAO <- monitora_env_chr("MONITORA_PERFIL_EXECUCAO", "auto")
if (!(MONITORA_PERFIL_EXECUCAO %in% c("auto", "rapido", "economico", "conservador"))) {
  MONITORA_PERFIL_EXECUCAO <- "auto"
}
if (identical(MONITORA_PERFIL_EXECUCAO, "conservador")) MONITORA_PERFIL_EXECUCAO <- "economico"

perfil_rapido <- identical(MONITORA_PERFIL_EXECUCAO, "rapido")
perfil_economico <- identical(MONITORA_PERFIL_EXECUCAO, "economico")

# Valores padrão por perfil. Variáveis de ambiente explícitas continuam tendo prioridade.
default_batch <- if (perfil_rapido) 100L else if (perfil_economico) 10L else 50L
default_threads <- if (perfil_rapido) {
  max(1L, parallel::detectCores(logical = FALSE))
} else if (perfil_economico) {
  2L
} else {
  min(4L, max(1L, parallel::detectCores(logical = FALSE)))
}

MONITORA_BATCH_SIZE_CSV <- monitora_env_int("MONITORA_BATCH_SIZE_CSV", default_batch)
MONITORA_AUDITORIA_COORDENADAS_COMPLETA <- monitora_env_bool("MONITORA_AUDITORIA_COORDENADAS_COMPLETA", FALSE)
MONITORA_EXPORTAR_GRAFICOS <- monitora_env_bool("MONITORA_EXPORTAR_GRAFICOS", !perfil_economico)
MONITORA_EXPORTAR_KML <- monitora_env_bool("MONITORA_EXPORTAR_KML", !perfil_economico)
MONITORA_MAX_LINHAS_GRAFICOS_AUTO <- monitora_env_int("MONITORA_MAX_LINHAS_GRAFICOS_AUTO", if (perfil_rapido) 500000L else 250000L)
MONITORA_MAX_UAS_KML_AUTO <- monitora_env_int("MONITORA_MAX_UAS_KML_AUTO", if (perfil_rapido) 10000L else 5000L)
MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA <- monitora_env_int("MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA", if (perfil_economico) 50000L else 100000L)

# Coleta de lixo: "auto" preserva desempenho e executa gc() apenas quando a memória disponível fica
# baixa.
# "agressivo" força gc() nos pontos instrumentados; "desligado" evita gc() manual.
MONITORA_GC_MODO <- monitora_env_chr("MONITORA_GC_MODO", if (perfil_economico) "auto" else "auto")
if (!(MONITORA_GC_MODO %in% c("auto", "agressivo", "desligado", "false", "true"))) MONITORA_GC_MODO <- "auto"
if (identical(MONITORA_GC_MODO, "true")) MONITORA_GC_MODO <- "agressivo"
if (identical(MONITORA_GC_MODO, "false")) MONITORA_GC_MODO <- "desligado"
MONITORA_GC_AGRESSIVO <- identical(MONITORA_GC_MODO, "agressivo")
MONITORA_GC_AUTO_MIN_MEM_MB <- monitora_env_int("MONITORA_GC_AUTO_MIN_MEM_MB", if (perfil_economico) 6000L else 4000L)

MONITORA_DT_THREADS <- monitora_env_int("MONITORA_DT_THREADS", default_threads)
tryCatch(data.table::setDTthreads(MONITORA_DT_THREADS), error = function(e) NULL)

### Usa subdiretório de extração por execução, evitando que resquícios de execuções anteriores
### sejam reprocessados ou aumentem o custo de varredura em lotes grandes.
MONITORA_EXTRACT_BASE_DIR <- MONITORA_EXTRACT_DIR
MONITORA_EXTRACT_DIR <- file.path(MONITORA_EXTRACT_BASE_DIR, paste0("exec_", MONITORA_EXEC_ID))
dir.create(MONITORA_EXTRACT_DIR, showWarnings = FALSE, recursive = TRUE)
monitora_log("configuracao_v19", "INFO", NA_character_, paste0("perfil=", MONITORA_PERFIL_EXECUCAO, "; batch_csv=", MONITORA_BATCH_SIZE_CSV, "; dt_threads=", MONITORA_DT_THREADS, "; gc_modo=", MONITORA_GC_MODO, "; gc_auto_min_mem_mb=", MONITORA_GC_AUTO_MIN_MEM_MB, "; auditoria_coord_completa=", MONITORA_AUDITORIA_COORDENADAS_COMPLETA, "; exportar_graficos=", MONITORA_EXPORTAR_GRAFICOS, "; exportar_kml=", MONITORA_EXPORTAR_KML), "configuracoes adaptativas de performance/RAM; v19 adiciona controlador dinamico")

monitora_memoria_rss_mb <- function() {
  status_file <- "/proc/self/status"
  if (!file.exists(status_file)) return(NA_real_)
  ln <- grep("^VmRSS:", readLines(status_file, warn = FALSE), value = TRUE)
  if (length(ln) == 0) return(NA_real_)
  kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", ln[1])))
  round(kb / 1024, 3)
}
monitora_memoria_sistema <- function() {
  meminfo <- "/proc/meminfo"
  if (!file.exists(meminfo)) {
    return(data.table(mem_total_mb = NA_real_, mem_available_mb = NA_real_, swap_total_mb = NA_real_, swap_free_mb = NA_real_))
  }
  x <- readLines(meminfo, warn = FALSE)
  get_kb <- function(prefix) {
    ln <- grep(paste0("^", prefix, ":"), x, value = TRUE)
    if (length(ln) == 0) return(NA_real_)
    suppressWarnings(as.numeric(gsub("[^0-9]", "", ln[1])))
  }
  data.table(
    mem_total_mb = round(get_kb("MemTotal") / 1024, 1),
    mem_available_mb = round(get_kb("MemAvailable") / 1024, 1),
    swap_total_mb = round(get_kb("SwapTotal") / 1024, 1),
    swap_free_mb = round(get_kb("SwapFree") / 1024, 1)
  )
}
monitora_deve_gc <- function() {
  if (identical(MONITORA_GC_MODO, "agressivo")) return(TRUE)
  if (identical(MONITORA_GC_MODO, "desligado")) return(FALSE)
  ms <- monitora_memoria_sistema()
  mem_avail <- suppressWarnings(as.numeric(ms$mem_available_mb[1]))
  if (is.na(mem_avail)) return(FALSE)
  isTRUE(mem_avail < MONITORA_GC_AUTO_MIN_MEM_MB)
}

monitora_gc <- function(etapa = NA_character_, force = FALSE) {
  gc_executado <- isTRUE(force) || monitora_deve_gc()
  if (gc_executado) invisible(gc(verbose = FALSE))
  ms <- monitora_memoria_sistema()
  monitora_log(
    "memoria_gc", "INFO", NA_character_,
    paste0("RSS_MB=", monitora_memoria_rss_mb(), "; mem_available_mb=", ms$mem_available_mb[1], "; gc_executado=", gc_executado),
    paste0("apos=", etapa)
  )
  invisible(TRUE)
}

MONITORA_HARDWARE <- cbind(
  data.table(
    exec_id = MONITORA_EXEC_ID,
    sysname = Sys.info()[["sysname"]],
    release = Sys.info()[["release"]],
    machine = Sys.info()[["machine"]],
    cpu_threads_r = parallel::detectCores(logical = TRUE),
    cpu_cores_r = parallel::detectCores(logical = FALSE),
    data_table_threads = data.table::getDTthreads()
  ),
  monitora_memoria_sistema()
)
fwrite(MONITORA_HARDWARE, file.path(MONITORA_LOG_DIR, paste0("hardware_memoria_", MONITORA_EXEC_ID, ".csv")))
fwrite(MONITORA_HARDWARE, file.path(MONITORA_OUTPUT_DIR, "hardware_memoria_ultima_execucao.csv"))
monitora_log("hardware_memoria", "INFO", file.path(MONITORA_LOG_DIR, paste0("hardware_memoria_", MONITORA_EXEC_ID, ".csv")), paste0("MemAvailable_MB=", MONITORA_HARDWARE$mem_available_mb, "; SwapTotal_MB=", MONITORA_HARDWARE$swap_total_mb), "usar para diagnosticar estouro de RAM")


### Controlador dinâmico de recursos
### Objetivo: usar o máximo de CPU, processamento paralelo e lotes grandes quando há RAM disponível,
### mas preservar uma reserva para sistema, Firefox/RStudio e evitar falta de memória. Quando a RAM
### volta a ficar disponível, o controlador sobe novamente o nível de paralelismo.
MONITORA_RESOURCE_CONTROL <- data.table(
  ordem = integer(),
  timestamp = character(),
  etapa = character(),
  risco = character(),
  modo_anterior = character(),
  modo_novo = character(),
  mem_total_mb = numeric(),
  mem_available_mb = numeric(),
  rss_mb = numeric(),
  reserva_usuario_mb = numeric(),
  dt_threads_anterior = integer(),
  dt_threads_novo = integer(),
  batch_anterior = integer(),
  batch_novo = integer(),
  gc_executado = logical(),
  motivo = character()
)

monitora_env_num <- function(nome, default) {
  val <- suppressWarnings(as.numeric(Sys.getenv(nome, unset = as.character(default))))
  ifelse(is.na(val) || val < 0, as.numeric(default), val)
}

.MONITORA_MEM_TOTAL_INICIAL <- suppressWarnings(as.numeric(MONITORA_HARDWARE$mem_total_mb[1]))
.MONITORA_CPU_THREADS_LOGICOS <- max(1L, suppressWarnings(as.integer(parallel::detectCores(logical = TRUE))))
.MONITORA_CPU_CORES_FISICOS <- max(1L, suppressWarnings(as.integer(parallel::detectCores(logical = FALSE))))

# Reserva padrão: pelo menos 6 GB para sistema/Firefox/RStudio, ou ~18% da RAM se isso for maior.
# Pode ser ajustada: MONITORA_RAM_RESERVA_USUARIO_MB=8192, por exemplo.
.default_reserva_mb <- if (is.na(.MONITORA_MEM_TOTAL_INICIAL)) 6144 else max(6144, round(.MONITORA_MEM_TOTAL_INICIAL * 0.18))
MONITORA_RAM_RESERVA_USUARIO_MB <- monitora_env_int("MONITORA_RAM_RESERVA_USUARIO_MB", .default_reserva_mb)
MONITORA_RAM_HISTERese_MB <- monitora_env_int("MONITORA_RAM_HISTERESE_MB", 1024L)
MONITORA_THREADS_RESERVA_SISTEMA <- monitora_env_int("MONITORA_THREADS_RESERVA_SISTEMA", 2L)
MONITORA_DT_THREADS_MAX <- monitora_env_int("MONITORA_DT_THREADS_MAX", max(1L, .MONITORA_CPU_THREADS_LOGICOS - MONITORA_THREADS_RESERVA_SISTEMA))
MONITORA_DT_THREADS_MIN <- monitora_env_int("MONITORA_DT_THREADS_MIN", 1L)
MONITORA_BATCH_CSV_RAPIDO <- monitora_env_int("MONITORA_BATCH_CSV_RAPIDO", max(MONITORA_BATCH_SIZE_CSV, 100L))
MONITORA_BATCH_CSV_EQUILIBRADO <- monitora_env_int("MONITORA_BATCH_CSV_EQUILIBRADO", max(25L, min(MONITORA_BATCH_CSV_RAPIDO, 50L)))
MONITORA_BATCH_CSV_ECONOMICO <- monitora_env_int("MONITORA_BATCH_CSV_ECONOMICO", 10L)
MONITORA_BATCH_CSV_CRITICO <- monitora_env_int("MONITORA_BATCH_CSV_CRITICO", 5L)
MONITORA_CONTROLE_RECURSOS_GRAVAR_AO_VIVO <- monitora_env_bool("MONITORA_CONTROLE_RECURSOS_GRAVAR_AO_VIVO", TRUE)

MONITORA_RECURSOS_MODO_ATUAL <- "inicial"
MONITORA_BATCH_SIZE_CSV_ATUAL <- MONITORA_BATCH_SIZE_CSV
MONITORA_DT_THREADS_ATUAL <- data.table::getDTthreads()

monitora_recurso_modo_por_memoria <- function(mem_available_mb, risco = "normal") {
  if (is.na(mem_available_mb)) {
    if (identical(MONITORA_PERFIL_EXECUCAO, "rapido")) return("rapido")
    if (identical(MONITORA_PERFIL_EXECUCAO, "economico")) return("economico")
    return("equilibrado")
  }
  reserva <- MONITORA_RAM_RESERVA_USUARIO_MB
  folga <- mem_available_mb - reserva

  # Limiares operacionais:
  # crítico: preservar usuário/sistema; econômico: evitar falta de memória; equilibrado: boa
  # velocidade;
  # rápido: usar o máximo possível de recursos, preservando reserva para sistema/Firefox/RStudio.
  modo <- if (folga <= 0) {
    "critico"
  } else if (folga <= 2048) {
    "economico"
  } else if (folga <= 6144) {
    "equilibrado"
  } else {
    "rapido"
  }

  # Etapas sabidamente pesadas em RAM descem um nível preventivamente; se a RAM estiver folgada,
  # o modo volta a subir no próximo checkpoint.
  if (risco %in% c("alto", "muito_alto")) {
    if (identical(modo, "rapido")) modo <- "equilibrado"
    else if (identical(modo, "equilibrado")) modo <- "economico"
    else if (identical(modo, "economico") && identical(risco, "muito_alto")) modo <- "critico"
  }
  modo
}

monitora_threads_por_modo <- function(modo) {
  mx <- max(MONITORA_DT_THREADS_MIN, MONITORA_DT_THREADS_MAX)
  if (identical(modo, "rapido")) return(mx)
  if (identical(modo, "equilibrado")) return(max(MONITORA_DT_THREADS_MIN, min(mx, ceiling(mx * 0.75))))
  if (identical(modo, "economico")) return(max(MONITORA_DT_THREADS_MIN, min(mx, ceiling(mx * 0.40))))
  MONITORA_DT_THREADS_MIN
}

monitora_batch_por_modo <- function(modo) {
  if (identical(modo, "rapido")) return(MONITORA_BATCH_CSV_RAPIDO)
  if (identical(modo, "equilibrado")) return(MONITORA_BATCH_CSV_EQUILIBRADO)
  if (identical(modo, "economico")) return(MONITORA_BATCH_CSV_ECONOMICO)
  MONITORA_BATCH_CSV_CRITICO
}

monitora_batch_size_csv <- function() {
  as.integer(MONITORA_BATCH_SIZE_CSV_ATUAL)
}

monitora_controlar_recursos <- function(etapa = NA_character_, risco = "normal", objeto = NULL, force_log = FALSE) {
  ms <- monitora_memoria_sistema()
  mem_avail <- suppressWarnings(as.numeric(ms$mem_available_mb[1]))
  mem_total <- suppressWarnings(as.numeric(ms$mem_total_mb[1]))
  rss <- monitora_memoria_rss_mb()
  modo_prev <- MONITORA_RECURSOS_MODO_ATUAL
  threads_prev <- suppressWarnings(as.integer(data.table::getDTthreads()))
  batch_prev <- suppressWarnings(as.integer(MONITORA_BATCH_SIZE_CSV_ATUAL))

  # Se o objeto informado já é muito grande em relação à folga, aumenta o nível de risco para
  # reduzir pico
  # em operações subsequentes que podem copiar vetores temporários.
  objeto_mb <- NA_real_
  if (!is.null(objeto)) {
    objeto_mb <- tryCatch(round(as.numeric(object.size(objeto)) / 1024^2, 1), error = function(e) NA_real_)
    folga_apos_reserva <- mem_avail - MONITORA_RAM_RESERVA_USUARIO_MB
    if (!is.na(objeto_mb) && !is.na(folga_apos_reserva)) {
      if (objeto_mb > folga_apos_reserva * 0.50) risco <- "muito_alto"
      else if (objeto_mb > folga_apos_reserva * 0.30 && identical(risco, "normal")) risco <- "alto"
    }
  }

  modo <- monitora_recurso_modo_por_memoria(mem_avail, risco)
  threads_new <- monitora_threads_por_modo(modo)
  batch_new <- monitora_batch_por_modo(modo)

  if (!identical(threads_prev, threads_new)) {
    tryCatch(data.table::setDTthreads(threads_new), error = function(e) NULL)
  }
  MONITORA_DT_THREADS_ATUAL <<- suppressWarnings(as.integer(data.table::getDTthreads()))
  MONITORA_BATCH_SIZE_CSV_ATUAL <<- as.integer(batch_new)
  MONITORA_RECURSOS_MODO_ATUAL <<- modo

  # GC adaptativo: executa em modo crítico/econômico ou quando a memória ficou abaixo da reserva.
  gc_executado <- FALSE
  if (modo %in% c("critico", "economico") || (!is.na(mem_avail) && mem_avail < MONITORA_RAM_RESERVA_USUARIO_MB)) {
    invisible(gc(verbose = FALSE))
    gc_executado <- TRUE
    ms <- monitora_memoria_sistema()
    mem_avail <- suppressWarnings(as.numeric(ms$mem_available_mb[1]))
    rss <- monitora_memoria_rss_mb()
  }

  mudou <- !identical(modo_prev, modo) || !identical(threads_prev, threads_new) || !identical(batch_prev, batch_new)
  if (isTRUE(force_log) || mudou || gc_executado) {
    motivo <- paste0("objeto_mb=", objeto_mb, "; perfil=", MONITORA_PERFIL_EXECUCAO)
    MONITORA_RESOURCE_CONTROL <<- data.table::rbindlist(
      list(MONITORA_RESOURCE_CONTROL,
      data.table::data.table(
        ordem = nrow(MONITORA_RESOURCE_CONTROL) + 1L,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        etapa = as.character(etapa),
        risco = as.character(risco),
        modo_anterior = as.character(modo_prev),
        modo_novo = as.character(modo),
        mem_total_mb = mem_total,
        mem_available_mb = mem_avail,
        rss_mb = rss,
        reserva_usuario_mb = MONITORA_RAM_RESERVA_USUARIO_MB,
        dt_threads_anterior = threads_prev,
        dt_threads_novo = MONITORA_DT_THREADS_ATUAL,
        batch_anterior = batch_prev,
        batch_novo = MONITORA_BATCH_SIZE_CSV_ATUAL,
        gc_executado = gc_executado,
        motivo = motivo
      )),
      fill = TRUE,
      use.names = TRUE
    )
    if (isTRUE(MONITORA_CONTROLE_RECURSOS_GRAVAR_AO_VIVO)) {
      tryCatch({
        fwrite(MONITORA_RESOURCE_CONTROL, file.path(MONITORA_LOG_DIR, paste0("controle_recursos_", MONITORA_EXEC_ID, ".csv")))
        fwrite(MONITORA_RESOURCE_CONTROL, file.path(MONITORA_OUTPUT_DIR, "controle_recursos_ultima_execucao.csv"))
      }, error = function(e) NULL)
    }
    monitora_log(
      "controle_recursos_v19", "INFO", NA_character_,
      paste0("etapa=", etapa, "; modo=", modo, "; threads=", MONITORA_DT_THREADS_ATUAL,
             "; batch_csv=", MONITORA_BATCH_SIZE_CSV_ATUAL, "; MemAvailable_MB=", mem_avail,
             "; RSS_MB=", rss, "; reserva_usuario_mb=", MONITORA_RAM_RESERVA_USUARIO_MB,
             "; gc=", gc_executado),
      motivo
    )
  }
  invisible(list(modo = modo, threads = MONITORA_DT_THREADS_ATUAL, batch_csv = MONITORA_BATCH_SIZE_CSV_ATUAL, mem_available_mb = mem_avail))
}

monitora_resource_control_write <- function() {
  if (!exists("MONITORA_RESOURCE_CONTROL", inherits = TRUE)) return(invisible(FALSE))
  if (nrow(MONITORA_RESOURCE_CONTROL) == 0) {
    monitora_controlar_recursos("fim_execucao", force_log = TRUE)
  }
  fwrite(MONITORA_RESOURCE_CONTROL, file.path(MONITORA_LOG_DIR, paste0("controle_recursos_", MONITORA_EXEC_ID, ".csv")))
  fwrite(MONITORA_RESOURCE_CONTROL, file.path(MONITORA_OUTPUT_DIR, "controle_recursos_ultima_execucao.csv"))
  invisible(TRUE)
}

# Primeira decisão dinâmica após ler o hardware real.
monitora_controlar_recursos("inicio_controlador_v19", force_log = TRUE)

### Instrumentação de performance
### Registra a duração incremental e acumulada das principais etapas do script.
### O objetivo é identificar gargalos, especialmente em normalização, consolidação de colunas,
### deduplicação semântica, análises e exportações.

MONITORA_PERF_ENABLED <- TRUE
MONITORA_PERF_START_TIME <- Sys.time()
MONITORA_PERF_LAST_TIME <- MONITORA_PERF_START_TIME
MONITORA_PERFORMANCE <- data.table(
  ordem = integer(),
  etapa = character(),
  inicio = character(),
  fim = character(),
  duracao_seg = numeric(),
  duracao_acumulada_seg = numeric(),
  n_linhas = integer(),
  n_colunas = integer(),
  objeto_mb = numeric(),
  rss_mb = numeric(),
  mem_available_mb = numeric(),
  detalhe = character()
)

monitora_perf_checkpoint <- function(etapa, detalhe = NA_character_, objeto = NULL) {
  if (!isTRUE(MONITORA_PERF_ENABLED)) return(invisible(TRUE))
  now <- Sys.time()
  inicio <- MONITORA_PERF_LAST_TIME
  dur <- as.numeric(difftime(now, inicio, units = "secs"))
  dur_total <- as.numeric(difftime(now, MONITORA_PERF_START_TIME, units = "secs"))
  n_linhas <- NA_integer_
  n_colunas <- NA_integer_
  objeto_mb <- NA_real_

  obj <- NULL
  if (!is.null(objeto)) {
    if (is.character(objeto) && length(objeto) == 1 && exists(objeto, inherits = TRUE)) {
      obj <- get(objeto, inherits = TRUE)
    } else if (!is.character(objeto)) {
      obj <- objeto
    }
  }

  if (!is.null(obj)) {
    n_linhas <- tryCatch(as.integer(NROW(obj)), error = function(e) NA_integer_)
    n_colunas <- tryCatch(if (!is.null(dim(obj))) as.integer(NCOL(obj)) else NA_integer_, error = function(e) NA_integer_)
    objeto_mb <- tryCatch(round(as.numeric(object.size(obj)) / 1024^2, 3), error = function(e) NA_real_)
  }

  MONITORA_PERFORMANCE <<- data.table::rbindlist(
    list(MONITORA_PERFORMANCE,
    data.table::data.table(
      ordem = nrow(MONITORA_PERFORMANCE) + 1L,
      etapa = as.character(etapa),
      inicio = format(inicio, "%Y-%m-%d %H:%M:%S"),
      fim = format(now, "%Y-%m-%d %H:%M:%S"),
      duracao_seg = round(dur, 3),
      duracao_acumulada_seg = round(dur_total, 3),
      n_linhas = n_linhas,
      n_colunas = n_colunas,
      objeto_mb = objeto_mb,
      rss_mb = monitora_memoria_rss_mb(),
      mem_available_mb = monitora_memoria_sistema()$mem_available_mb,
      detalhe = as.character(detalhe)
    )),
    fill = TRUE,
    use.names = TRUE
  )

  MONITORA_PERF_LAST_TIME <<- now
  message(sprintf("[PERF] %s: %.3fs acumulado %.3fs", etapa, dur, dur_total))
  invisible(TRUE)
}

monitora_perf_write <- function() {
  if (!isTRUE(MONITORA_PERF_ENABLED)) return(invisible(TRUE))
  monitora_perf_checkpoint("fim_execucao", "checkpoint final antes da gravação do relatório de performance")
  fwrite(MONITORA_PERFORMANCE, file.path(MONITORA_LOG_DIR, paste0("performance_execucao_", MONITORA_EXEC_ID, ".csv")))
  fwrite(MONITORA_PERFORMANCE, file.path(MONITORA_OUTPUT_DIR, "performance_execucao_ultima_execucao.csv"))
  invisible(TRUE)
}

monitora_norm_empty <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "Na", "na", "N/A", "n/a", "NULL", "null", "NaN", "nan")] <- NA_character_
  x
}

monitora_base_colname <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, '\\"{4}', '\\"\\"')
  x <- stringr::str_squish(x)
  # Remove sufixos introduzidos por leitores de CSV/XLSX para nomes repetidos, como uc...5.
  # Não remove sufixos .2, pois no formulário eles podem distinguir perguntas diferentes.
  x <- stringr::str_replace(x, "\\.\\.\\.[0-9]+$", "")
  x
}

monitora_coalesce_vec <- function(...) {
  vals <- list(...)
  if (length(vals) == 0) return(character())
  n <- max(vapply(vals, length, integer(1)))
  vals <- lapply(vals, function(v) rep_len(as.character(v), n))
  out <- rep(NA_character_, n)
  for (v in vals) {
    v <- monitora_norm_empty(v)
    idx <- is.na(out) & !is.na(v)
    out[idx] <- v[idx]
  }
  out
}

monitora_merge_duplicate_columns <- function(dt) {
  # Consolidação adaptativa de colunas duplicadas.
  #
  # Em bases muito grandes, a consolidação completa de todas as colunas duplicadas pode
  # gerar picos altos de memória ao materializar vetores inteiros para cada grupo
  # duplicado. A estratégia adotada evita esse padrão:
  #   1) em bases grandes, só consolida automaticamente grupos críticos para a
  #      análise; grupos não críticos são preservados com nomes originais e
  #      registrados como "adiados", sem coalesce;
  #   2) para grupos críticos em bases grandes, faz coalesce por blocos de linhas,
  #      sem criar vetor consolidado do tamanho da tabela inteira;
  #   3) por padrão não concatena valores conflitantes com "__CONFLITO__" em bases
  #      grandes, pois isso pode multiplicar strings e RAM; os conflitos ficam em
  #      auditoria.
  dt <- monitora_as_dt_ref(dt)

  # Tratamento de cabeçalhos duplicados: alguns CSVs do SISMONITORA podem chegar com nomes de
  # colunas exatamente repetidos no próprio cabeçalho. data.table permite esse
  # estado, mas operações como dt[, (cols) := NULL] falham quando `cols` contém
  # o mesmo nome mais de uma vez. Além disso, dt[[nome]] sempre recupera a
  # primeira coluna com aquele nome, impedindo coalesce correto por posição.
  #
  # Estratégia: guardar os nomes originais para formar os grupos de fusão e,
  # antes de operar por referência, tornar os nomes fisicamente únicos por
  # posição. Assim, colunas originalmente iguais continuam no mesmo grupo, mas
  # passam a ser endereçáveis sem ambiguidade.
  original_names_raw <- names(dt)
  original_names <- original_names_raw
  if (anyDuplicated(original_names_raw)) {
    original_names <- make.unique(original_names_raw, sep = "__dup")
    data.table::setnames(dt, original_names)
  }

  base_names <- monitora_base_colname(original_names_raw)

  protected <- grepl("^MONITORA_", original_names_raw) | original_names_raw %in% c(".id")
  base_names[protected] <- original_names[protected]

  n_linhas_dt <- nrow(dt)
  n_colunas_dt <- ncol(dt)
  objeto_mb <- tryCatch(round(as.numeric(object.size(dt)) / 1024^2, 1), error = function(e) NA_real_)

  modo_merge <- Sys.getenv("MONITORA_MERGE_DUPLICATE_COLUMNS_MODO", unset = "auto")
  limite_obj_mb <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_MERGE_DUP_COLS_SELECTIVE_MB", unset = "4096")))
  limite_ncols <- suppressWarnings(as.integer(Sys.getenv("MONITORA_MERGE_DUP_COLS_SELECTIVE_NCOLS", unset = "350")))
  chunk_n <- suppressWarnings(as.integer(Sys.getenv("MONITORA_MERGE_DUP_COLS_CHUNK_ROWS", unset = "50000")))
  if (is.na(chunk_n) || chunk_n < 1000L) chunk_n <- 50000L

  seletivo <- FALSE
  if (identical(modo_merge, "seletivo")) seletivo <- TRUE
  if (identical(modo_merge, "completo")) seletivo <- FALSE
  if (identical(modo_merge, "auto")) {
    seletivo <- (!is.na(objeto_mb) && objeto_mb >= limite_obj_mb) || (!is.na(n_colunas_dt) && n_colunas_dt >= limite_ncols)
  }

  # Bases/canônicos que são necessários para identificação, deduplicação,
  # estatísticas e compatibilidade dos formatos SISMONITORA. Em modo seletivo,
  # estes grupos ainda são consolidados; demais grupos duplicados ficam preservados
  # e auditados para evitar pico de RAM.
  critical_aliases <- c(
    "uc", "ua", "ciclo", "campanha", "coleta", "uuid", "coleta_uuid",
    "registro_uuid", "data", "data do registro", "data_do_registro",
    "data (data_hora)", "horario (data_hora)", "horário (data_hora)",
    "protocolo", "formulario", "formulário",
    "amostragem/registro/ponto_amostral", "amostragem/registro/ponto_metro",
    "amostragem/registro/uuid", "ponto_amostral", "ponto_metro",
    "coordenada inicial da amostragem (amostragem)",
    "coordenada final da amostragem (amostragem)",
    "amostragem/ponto_inicio_transecto", "amostragem/ponto_fim_transecto",
    "ponto inicio transecto", "ponto fim transecto",
    "ponto_inicio_transecto", "ponto_fim_transecto",
    "categoria", "forma de vida", "forma_de_vida", "origem",
    "amostragem/registro/categoria", "amostragem/registro/forma_de_vida",
    "amostragem/registro/origem", "familia", "família", "genero", "gênero",
    "especie", "espécie", "nome_cientifico", "nome científico",
    "seca_morta", "seca/morta", "nativa", "exotica", "exótica"
  )
  critical_canon <- unique(monitora_canonical_col(critical_aliases))

  groups <- split(original_names, base_names)
  audit_list <- vector("list", length(groups))
  audit_i <- 0L

  make_safe_name <- function(target, current_col) {
    if (is.na(target) || !nzchar(target)) target <- paste0("COLUNA_SEM_NOME_", seq_along(names(dt))[1])
    if (identical(target, current_col)) return(target)
    if (!(target %in% names(dt))) return(target)
    i <- 2L
    candidate <- paste0(target, "__merge", i)
    while (candidate %in% names(dt)) {
      i <- i + 1L
      candidate <- paste0(target, "__merge", i)
    }
    candidate
  }

  merge_group_i <- 0L
  for (base_col in names(groups)) {
    merge_group_i <- merge_group_i + 1L
    if (merge_group_i == 1L || merge_group_i %% 25L == 0L) {
      # Não passa o objeto inteiro aqui: object.size(dt) em tabela de muitos GB
      # é caro e pode aumentar pressão de memória. O tamanho já foi medido uma vez.
      monitora_controlar_recursos(paste0("merge_duplicate_columns_grupo_", merge_group_i), risco = "alto", objeto = NULL, force_log = FALSE)
    }

    cols_orig <- groups[[base_col]]
    cols <- cols_orig[cols_orig %in% names(dt)]
    if (length(cols) == 0) next

    safe_col <- base_col
    if (is.na(safe_col) || !nzchar(safe_col)) safe_col <- paste0("COLUNA_SEM_NOME_", which(names(groups) == base_col)[1])

    if (length(cols) == 1) {
      current_col <- cols[1]
      # Em modo seletivo e tabela grande, evita renomear colunas não críticas
      # apenas para remover sufixo ...N; isso reduz risco de colisões e mantém
      # rastreabilidade do cabeçalho original.
      base_can <- monitora_canonical_col(safe_col)
      if (!isTRUE(seletivo) || base_can %in% critical_canon || grepl("^MONITORA_", current_col) || current_col %in% c(".id")) {
        new_name <- make_safe_name(safe_col, current_col)
        if (!identical(current_col, new_name)) data.table::setnames(dt, current_col, new_name)
      }
      next
    }

    base_can <- monitora_canonical_col(safe_col)
    cols_can <- unique(monitora_canonical_col(cols))
    grupo_critico <- base_can %in% critical_canon || any(cols_can %in% critical_canon)

    if (isTRUE(seletivo) && !isTRUE(grupo_critico)) {
      # Não coalesce grupos não críticos em bases enormes. Manter as colunas como
      # vieram do fread/rbindlist é mais seguro que materializar vetores enormes.
      audit_i <- audit_i + 1L
      audit_list[[audit_i]] <- data.table::data.table(
        coluna = safe_col,
        nomes_originais = paste(cols_orig, collapse = " | "),
        n_colunas_fundidas = 0L,
        linhas_com_conflito = NA_integer_,
        exemplos = NA_character_,
        acao = "adiado_modo_seletivo",
        motivo = paste0("base grande: objeto_mb=", objeto_mb, "; ncol=", n_colunas_dt)
      )
      next
    }

    primary <- cols[1]
    dup_cols <- cols[-1]
    conflito_total_n <- 0L
    exemplos_conflito <- character()

    # Tabelas pequenas/médias: caminho vetorial mais rápido.
    usar_chunk <- isTRUE(seletivo) || (!is.na(objeto_mb) && objeto_mb >= limite_obj_mb)
    if (!usar_chunk) {
      merged <- monitora_norm_empty(as.character(dt[[primary]]))
      for (col in dup_cols) {
        v <- monitora_norm_empty(as.character(dt[[col]]))
        fill_idx <- is.na(merged) & !is.na(v)
        if (any(fill_idx)) merged[fill_idx] <- v[fill_idx]
        conflito <- !is.na(merged) & !is.na(v) & merged != v
        n_conf <- sum(conflito, na.rm = TRUE)
        if (n_conf > 0) {
          conflito_total_n <- conflito_total_n + n_conf
          if (length(exemplos_conflito) < 5L) {
            exemplos_conflito <- unique(c(exemplos_conflito, utils::head(unique(paste(merged[conflito], v[conflito], sep = "__CONFLITO__")), 5L)))
            exemplos_conflito <- utils::head(exemplos_conflito, 5L)
          }
          # Concatenação de conflito mantida só no caminho pequeno/médio.
          merged[conflito] <- paste(merged[conflito], v[conflito], sep = "__CONFLITO__")
        }
        rm(v, fill_idx, conflito)
      }
      data.table::set(dt, j = primary, value = as.character(merged))
      rm(merged)
    } else {
      # Caminho em blocos: processa apenas blocos de linhas e atualiza a coluna
      # primária por referência. Isso evita criar um vetor consolidado do tamanho
      # completo da tabela para cada grupo duplicado.
      seq_starts <- seq.int(1L, n_linhas_dt, by = chunk_n)
      for (col in dup_cols) {
        for (st in seq_starts) {
          en <- min(st + chunk_n - 1L, n_linhas_dt)
          ii <- st:en
          p <- monitora_norm_empty(dt[[primary]][ii])
          v <- monitora_norm_empty(dt[[col]][ii])

          fill_idx <- is.na(p) & !is.na(v)
          if (any(fill_idx)) {
            data.table::set(dt, i = ii[fill_idx], j = primary, value = v[fill_idx])
          }

          conflito <- !is.na(p) & !is.na(v) & p != v
          n_conf <- sum(conflito, na.rm = TRUE)
          if (n_conf > 0) {
            conflito_total_n <- conflito_total_n + n_conf
            if (length(exemplos_conflito) < 5L) {
              exemplos_conflito <- unique(c(exemplos_conflito, utils::head(unique(paste(p[conflito], v[conflito], sep = "__CONFLITO__")), 5L)))
              exemplos_conflito <- utils::head(exemplos_conflito, 5L)
            }
            # Em bases grandes não concatena conflitos na tabela principal:
            # manter o valor primário evita explosão de strings. A auditoria
            # registra a existência e exemplos dos conflitos.
          }

          rm(p, v, fill_idx, conflito)
        }
        if (monitora_deve_gc()) gc(verbose = FALSE)
      }
    }

    dt[, (dup_cols) := NULL]

    new_name <- make_safe_name(safe_col, primary)
    if (!identical(primary, new_name) && primary %in% names(dt)) {
      data.table::setnames(dt, primary, new_name)
    }

    audit_i <- audit_i + 1L
    audit_list[[audit_i]] <- data.table::data.table(
      coluna = new_name,
      nomes_originais = paste(cols_orig, collapse = " | "),
      n_colunas_fundidas = length(cols_orig),
      linhas_com_conflito = as.integer(conflito_total_n),
      exemplos = paste(utils::head(exemplos_conflito, 5L), collapse = " | "),
      acao = ifelse(usar_chunk, "fundido_chunked", "fundido_vetorial"),
      motivo = paste0("seletivo=", seletivo, "; critico=", grupo_critico, "; objeto_mb=", objeto_mb)
    )

    rm(conflito_total_n, exemplos_conflito)
    if (monitora_deve_gc()) gc(verbose = FALSE)
  }

  audit <- if (audit_i > 0L) data.table::rbindlist(audit_list[seq_len(audit_i)], fill = TRUE, use.names = TRUE) else data.table::data.table(
    coluna = character(), nomes_originais = character(), n_colunas_fundidas = integer(),
    linhas_com_conflito = integer(), exemplos = character(), acao = character(), motivo = character()
  )
  attr(dt, "audit_duplicate_columns") <- audit
  dt
}


monitora_drop_legacy_technical_columns <- function(dt, momento = "importacao") {
  dt <- monitora_as_dt_ref(dt)
  # Arquivos pós-tratamento usados como entrada podem trazer colunas técnicas
  # de execuções anteriores, como .id.1, .id.2 etc. Elas não representam
  # dados do protocolo nem a origem da execucao atual. A coluna .id da execucao
  # atual é preservada/reconstruida separadamente.
  legacy_cols <- grep("^\\.id\\.[0-9]+$", names(dt), value = TRUE)
  if (length(legacy_cols) > 0) {
    monitora_log(
      "limpeza_colunas_tecnicas_legadas",
      "INFO",
      NA_character_,
      paste0(length(legacy_cols), " coluna(s) tecnica(s) legada(s) removida(s) em ", momento, ": ", paste(legacy_cols, collapse = ", ")),
      "colunas .id.N provenientes de execucoes anteriores nao foram mantidas no produto atual"
    )
    dt[, (legacy_cols) := NULL]
  }
  dt
}


monitora_strip_accents <- function(x) {
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  y
}

monitora_canonical_col <- function(x) {
  y <- monitora_base_colname(x)
  y <- tolower(monitora_strip_accents(y))
  y <- stringr::str_replace_all(y, "[[:space:]]+", " ")
  y <- stringr::str_squish(y)
  y
}

monitora_consolidate_aliases <- function(dt) {
  # Consolidação de aliases com menor uso de memória.
  # Evita materializar várias colunas grandes ao mesmo tempo durante a consolidação.
  # A consolidação é feita par a par, com remoção imediata das colunas auxiliares.
  # Isso reduz o pico de memória em bases grandes.
  dt <- monitora_as_dt_ref(dt)
  canon <- monitora_canonical_col(names(dt))
  alias_map <- list(
    "UC" = c("uc", "unidade de conservacao", "unidade de conservação"),
    "UA" = c("ua", "unidade amostral", "amostragem/unidade_amostral"),
    "CICLO" = c("ciclo", "ano ciclo", "ano_ciclo"),
    "CAMPANHA" = c("campanha"),
    "PROTOCOLO" = c("protocolo", "formulario", "formulário"),
    "UUID" = c("uuid", "registro uuid", "registro_uuid"),
    "coleta_uuid" = c("coleta uuid", "coleta_uuid", "uuid_coleta", "amostragem/uuid"),
    "Data (data_hora)" = c("data (data_hora)", "data_hora/data", "data", "data do registro", "data_do_registro", "DATA DO REGISTRO"),
    "Horário (data_hora)" = c("horario (data_hora)", "horário (data_hora)", "data_hora/hora", "hora"),
    "Coordenada inicial da amostragem (amostragem)" = c("coordenada inicial da amostragem (amostragem)", "amostragem/ponto_inicio_transecto", "ponto inicio transecto", "ponto_inicio_transecto"),
    "Coordenada final da amostragem (amostragem)" = c("coordenada final da amostragem (amostragem)", "amostragem/ponto_fim_transecto", "ponto fim transecto", "ponto_fim_transecto"),
    # Aliases de registro amostral precisam ser consolidados antes da deduplicação.
    # Não misturar UUID de coleta (UUID/uuid) com uuid do registro amostral.
    "ponto_amostral (amostragem/registro)" = c("ponto_amostral (amostragem/registro)", "amostragem/registro/ponto_amostral", "ponto_amostral"),
    "ponto_metro (amostragem/registro)" = c("ponto_metro (amostragem/registro)", "amostragem/registro/ponto_metro", "ponto_metro"),
    "uuid (amostragem/registro)" = c("uuid (amostragem/registro)", "amostragem/registro/uuid")
  )
  alias_i <- 0L
  for (target in names(alias_map)) {
    alias_i <- alias_i + 1L
    monitora_controlar_recursos(paste0("consolidate_aliases_", alias_i, "_", target), risco = "alto", objeto = dt, force_log = FALSE)
    idx <- which(canon %in% monitora_canonical_col(alias_map[[target]]))
    if (length(idx) == 0) next
    alias_cols <- names(dt)[idx]
    alias_cols <- alias_cols[alias_cols %in% names(dt)]
    if (length(alias_cols) == 0) next

    if (!(target %in% names(dt))) {
      first_col <- alias_cols[1]
      if (!identical(first_col, target)) {
        if (target %in% names(dt)) {
          # proteção redundante contra colisão
          data.table::setnames(dt, first_col, paste0(target, "__alias"))
        } else {
          data.table::setnames(dt, first_col, target)
        }
      }
      alias_cols <- setdiff(alias_cols, first_col)
    }

    for (col in alias_cols) {
      if (!(col %in% names(dt)) || identical(col, target)) next
      alvo <- monitora_norm_empty(as.character(dt[[target]]))
      aux <- monitora_norm_empty(as.character(dt[[col]]))
      fill_idx <- is.na(alvo) & !is.na(aux)
      if (any(fill_idx)) alvo[fill_idx] <- aux[fill_idx]
      data.table::set(dt, j = target, value = alvo)
      dt[, (col) := NULL]
      monitora_log("consolidacao_aliases", "INFO", NA_character_, paste0(target, " consolidado de: ", col), "coluna auxiliar removida apos coalesce")
      rm(alvo, aux, fill_idx)
      if (monitora_deve_gc()) gc(verbose = FALSE)
    }

    # Recalcula nomes canônicos porque colunas podem ter sido removidas/renomeadas.
    canon <- monitora_canonical_col(names(dt))
  }
  dt
}


# Deduplicação semântica compatível com exportação em lote.
# Não usa UUID de coleta como UUID de registro amostral.

### Detecção formal do tipo de entrada por nome de arquivo e cabeçalho.
### O objetivo é distinguir: exportação individual do SISMONITORA, exportação em lote
### de exportação em lote e arquivos pós-tratamento registros_corrig*.csv usados como entrada.
monitora_detectar_tipo_csv <- function(path) {
  base <- basename(path)
  header <- tryCatch(
    names(fread(path, nrows = 0, encoding = "UTF-8", check.names = FALSE, showProgress = FALSE)),
    error = function(e) character()
  )
  header_canon <- monitora_canonical_col(header)

  if (grepl("registros[._-]?corrig|corrigido|corrigida|pos[._-]?trat|p[oó]s[._-]?trat|registros_corrig", base, ignore.case = TRUE) ||
      any(header %in% c("MONITORA_TIPO_ENTRADA", "MONITORA_ARQUIVO_ENTRADA", "MONITORA_ARQUIVO_ORIGEM_EXECUCAO"))) {
    return("pos_tratamento_script")
  }

  # Exportação em lote: normalmente CSV consolidado com caminhos internos
  # do formulário e colunas com sufixos de nomes repetidos, como uc...5.
  if (any(header %in% c("amostragem/registro/ponto_amostral", "amostragem/registro/ponto_metro", "amostragem/registro/uuid", "coleta_uuid", "data_do_registro")) ||
      any(grepl("^uc\\.\\.\\.[0-9]+$", header))) {
    return("bruto_lote_sismonitora_dev")
  }

  # Exportação individual tradicional: CSVs de uma coleta, frequentemente extraídos de ZIPs
  # internos,
  # com colunas em maiúsculas no cabeçalho principal.
  if (all(c("UUID", "COLETA", "UC", "CICLO", "CAMPANHA", "UA") %in% header) ||
      all(c("uuid", "coleta", "uc", "ciclo", "campanha", "ua") %in% header_canon)) {
    return("bruto_individual_sismonitora")
  }

  "desconhecido"
}

monitora_resumir_fontes_por_tipo <- function(csv_audit) {
  if (is.null(csv_audit) || nrow(csv_audit) == 0 || !"tipo_entrada" %in% names(csv_audit)) return(invisible(NULL))
  resumo <- csv_audit[, .(
    n_arquivos = .N,
    n_linhas = sum(as.integer(n_linhas), na.rm = TRUE),
    n_colunas_min = suppressWarnings(min(as.integer(n_colunas), na.rm = TRUE)),
    n_colunas_max = suppressWarnings(max(as.integer(n_colunas), na.rm = TRUE)),
    arquivos_exemplo = paste(utils::head(basename, 10), collapse = " | ")
  ), by = tipo_entrada]
  resumo[is.infinite(n_colunas_min), n_colunas_min := NA_integer_]
  resumo[is.infinite(n_colunas_max), n_colunas_max := NA_integer_]
  fwrite(resumo, file.path(MONITORA_LOG_DIR, paste0("auditoria_tipos_entrada_", MONITORA_EXEC_ID, ".csv")))
  monitora_log("tipos_entrada", "INFO", NA_character_, paste(paste0(resumo$tipo_entrada, ": ", resumo$n_arquivos, " arquivo(s)"), collapse = " | "), "ver auditoria_tipos_entrada")
  invisible(resumo)
}


# Anota o tipo de entrada antes da deduplicação, permitindo auditoria pré-deduplicação real.
# A coluna MONITORA_TIPO_ENTRADA precisa existir antes das auditorias de compatibilidade.
# Sem essa coluna, a auditoria pré-deduplicação ficaria limitada.
monitora_anotar_tipo_entrada <- function(dt, arquivos_entrada = NULL) {
  dt <- monitora_as_dt_ref(dt)
  if ("MONITORA_TIPO_ENTRADA" %in% names(dt) && any(!is.na(dt$MONITORA_TIPO_ENTRADA))) {
    return(dt)
  }
  source_col <- if ("MONITORA_ARQUIVO_ENTRADA" %in% names(dt)) {
    "MONITORA_ARQUIVO_ENTRADA"
  } else if (".id" %in% names(dt)) {
    ".id"
  } else {
    NA_character_
  }
  if (is.na(source_col)) {
    # Evita cópia rasa de data.table ao adicionar coluna em objeto já copiado pelo R.
    data.table::setalloccol(dt, ncol(dt) + 8L)
    data.table::set(dt, j = "MONITORA_TIPO_ENTRADA", value = rep("desconhecido", nrow(dt)))
    monitora_log("tipos_entrada", "AVISO", NA_character_, "Coluna de proveniência ausente; MONITORA_TIPO_ENTRADA definido como desconhecido", "auditoria pre_dedup limitada")
    return(dt)
  }
  id_chr <- basename(as.character(dt[[source_col]]))
  tipo <- rep("desconhecido", nrow(dt))
  if (!is.null(arquivos_entrada) && all(c("basename", "tipo_entrada") %in% names(arquivos_entrada))) {
    tipo_map <- setNames(as.character(arquivos_entrada$tipo_entrada), basename(as.character(arquivos_entrada$basename)))
    idx <- match(id_chr, names(tipo_map))
    tipo[!is.na(idx)] <- tipo_map[idx[!is.na(idx)]]
  }
  # setalloccol() + set() reduz o risco de cópia rasa ao adicionar colunas em data.table.
  data.table::setalloccol(dt, ncol(dt) + 8L)
  data.table::set(dt, j = "MONITORA_TIPO_ENTRADA", value = tipo)
  dt
}


monitora_auditar_compatibilidade_fontes_v12 <- function(dt, fase = "pre_dedup") {
  dt <- monitora_as_dt_ref(dt)
  fase <- as.character(fase)[1]
  if (is.na(fase) || fase == "") fase <- "sem_fase"
  fase_segura <- gsub("[^A-Za-z0-9_]+", "_", fase)

  if (!"MONITORA_TIPO_ENTRADA" %in% names(dt)) {
    aud <- data.table(
      fase = fase,
      status_global = "sem_coluna_MONITORA_TIPO_ENTRADA",
      n_registros_total = nrow(dt)
    )
    fwrite(aud, file.path(MONITORA_LOG_DIR, paste0("auditoria_compatibilidade_fontes_v12_", fase_segura, "_", MONITORA_EXEC_ID, ".csv")))
    monitora_log("compatibilidade_fontes_v12", "AVISO", NA_character_, "Coluna MONITORA_TIPO_ENTRADA ausente; auditoria limitada", paste0("fase=", fase))
    return(invisible(aud))
  }

  tipos <- sort(unique(na.omit(as.character(dt$MONITORA_TIPO_ENTRADA))))

  get_col <- function(nm) if (nm %in% names(dt)) dt[[nm]] else rep(NA_character_, nrow(dt))
  primeira_col_existente <- function(candidatas) {
    candidatas[candidatas %in% names(dt)][1]
  }

  ponto_col <- primeira_col_existente(c(
    "ponto_amostral (amostragem/registro)",
    "Ponto amostral (amostragem/registro)",
    "amostragem/registro/ponto_amostral",
    "ponto_amostral",
    "ponto_metro (amostragem/registro)",
    "Ponto metro (amostragem/registro)",
    "amostragem/registro/ponto_metro",
    "ponto_metro"
  ))
  uc_col <- primeira_col_existente(c("UC", "uc", "uc...5", "uc...17"))
  ciclo_col <- primeira_col_existente(c("CICLO", "ciclo"))
  campanha_col <- primeira_col_existente(c("CAMPANHA", "campanha"))
  ua_col <- primeira_col_existente(c("UA", "ua"))

  cols_minimas <- c(uc_col, ciclo_col, campanha_col, ua_col, ponto_col)
  if (any(is.na(cols_minimas))) {
    aud <- data.table(
      fase = fase,
      status_global = "colunas_minimas_ausentes",
      coluna_uc = ifelse(is.na(uc_col), NA_character_, uc_col),
      coluna_ciclo = ifelse(is.na(ciclo_col), NA_character_, ciclo_col),
      coluna_campanha = ifelse(is.na(campanha_col), NA_character_, campanha_col),
      coluna_ua = ifelse(is.na(ua_col), NA_character_, ua_col),
      coluna_ponto = ifelse(is.na(ponto_col), NA_character_, ponto_col),
      tipos_entrada = paste(tipos, collapse = " | "),
      n_registros_total = nrow(dt)
    )
    fwrite(aud, file.path(MONITORA_LOG_DIR, paste0("auditoria_compatibilidade_fontes_v12_", fase_segura, "_", MONITORA_EXEC_ID, ".csv")))
    monitora_log("compatibilidade_fontes_v12", "AVISO", NA_character_, "Colunas mínimas ausentes; auditoria de compatibilidade limitada", paste0("fase=", fase))
    return(invisible(aud))
  }

  norm_key <- function(x) stringr::str_squish(toupper(as.character(monitora_norm_empty(x))))
  tmp <- data.table(
    UC = norm_key(get_col(uc_col)),
    CICLO = norm_key(get_col(ciclo_col)),
    CAMPANHA = norm_key(get_col(campanha_col)),
    UA = norm_key(get_col(ua_col)),
    ponto = norm_key(get_col(ponto_col)),
    MONITORA_TIPO_ENTRADA = as.character(dt$MONITORA_TIPO_ENTRADA)
  )
  tmp <- tmp[!is.na(UC) & !is.na(CICLO) & !is.na(CAMPANHA) & !is.na(UA) & !is.na(ponto) & !is.na(MONITORA_TIPO_ENTRADA)]

  if (nrow(tmp) == 0) {
    aud <- data.table(
      fase = fase,
      status_global = "sem_registros_validos_para_auditoria",
      tipos_entrada = paste(tipos, collapse = " | "),
      n_registros_total = nrow(dt)
    )
    fwrite(aud, file.path(MONITORA_LOG_DIR, paste0("auditoria_compatibilidade_fontes_v12_", fase_segura, "_", MONITORA_EXEC_ID, ".csv")))
    monitora_log("compatibilidade_fontes_v12", "AVISO", NA_character_, "Nenhum registro válido para auditoria de compatibilidade", paste0("fase=", fase))
    return(invisible(aud))
  }

  aud <- tmp[, .(
    n_registros = .N,
    n_pontos_distintos = uniqueN(ponto)
  ), by = .(UC, CICLO, CAMPANHA, UA, MONITORA_TIPO_ENTRADA)]

  aud_reg_wide <- dcast(aud, UC + CICLO + CAMPANHA + UA ~ MONITORA_TIPO_ENTRADA, value.var = "n_registros", fill = 0)
  aud_pts_wide <- dcast(aud, UC + CICLO + CAMPANHA + UA ~ MONITORA_TIPO_ENTRADA, value.var = "n_pontos_distintos", fill = 0)

  reg_tipo_cols <- setdiff(names(aud_reg_wide), c("UC", "CICLO", "CAMPANHA", "UA"))
  setnames(aud_reg_wide, reg_tipo_cols, paste0("n_registros__", reg_tipo_cols))
  pts_tipo_cols <- setdiff(names(aud_pts_wide), c("UC", "CICLO", "CAMPANHA", "UA"))
  setnames(aud_pts_wide, pts_tipo_cols, paste0("n_pontos__", pts_tipo_cols))

  aud_wide <- merge(aud_reg_wide, aud_pts_wide, by = c("UC", "CICLO", "CAMPANHA", "UA"), all = TRUE)
  aud_wide[, fase := fase]
  setcolorder(aud_wide, c("fase", "UC", "CICLO", "CAMPANHA", "UA", setdiff(names(aud_wide), c("fase", "UC", "CICLO", "CAMPANHA", "UA"))))

  reg_cols <- grep("^n_registros__", names(aud_wide), value = TRUE)
  pts_cols <- grep("^n_pontos__", names(aud_wide), value = TRUE)

  if (length(reg_cols) >= 2) {
    aud_wide[, status := fifelse(rowSums(.SD > 0) == length(reg_cols), "presente_em_todas_as_fontes", "ausente_em_alguma_fonte"), .SDcols = reg_cols]
    aud_wide[, registros_min := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = reg_cols]
    aud_wide[, registros_max := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = reg_cols]
    aud_wide[registros_min != registros_max, status := "divergencia_n_registros"]
    if (length(pts_cols) >= 2) {
      aud_wide[, pontos_min := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = pts_cols]
      aud_wide[, pontos_max := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = pts_cols]
      aud_wide[pontos_min != pontos_max & status == "presente_em_todas_as_fontes", status := "divergencia_n_pontos"]
    }
  } else {
    aud_wide[, status := "fonte_unica_apos_filtro_ou_deduplicacao"]
    if (length(reg_cols) == 1) {
      aud_wide[, registros_min := get(reg_cols[1])]
      aud_wide[, registros_max := get(reg_cols[1])]
    }
    if (length(pts_cols) == 1) {
      aud_wide[, pontos_min := get(pts_cols[1])]
      aud_wide[, pontos_max := get(pts_cols[1])]
    }
  }

  saida <- file.path(MONITORA_LOG_DIR, paste0("auditoria_compatibilidade_fontes_v12_", fase_segura, "_", MONITORA_EXEC_ID, ".csv"))
  fwrite(aud_wide, saida)
  monitora_log("compatibilidade_fontes_v12", "INFO", NA_character_, paste0(nrow(aud_wide), " grupos UC+CICLO+CAMPANHA+UA auditados; fase=", fase), basename(saida))
  invisible(aud_wide)
}

# Relatório-resumo de achados relevantes para verificação e validação.
# Consolida, em um único CSV, os principais achados espalhados nos demais relatórios.
# Inclui CSV interno, ZIP externo baixado e ZIP interno, quando existirem.
monitora_criar_resumo_achados_validacao <- function() {
  add_row <- function(categoria, severidade = "INFO", status = NA_character_, fase = NA_character_,
                      UC = NA_character_, CICLO = NA_character_, CAMPANHA = NA_character_, UA = NA_character_, COLETA = NA_character_,
                      tipo_entrada = NA_character_, n_registros = NA_integer_, n_pontos = NA_integer_,
                      arquivo_csv = NA_character_, nome_csv = NA_character_,
                      arquivo_entrada_externo = NA_character_, nome_arquivo_entrada_externo = NA_character_,
                      arquivo_zip_externo = NA_character_, nome_zip_externo = NA_character_,
                      arquivo_zip_interno = NA_character_, nome_zip_interno = NA_character_,
                      caminho_relativo_extraido = NA_character_,
                      arquivo_csv_mantido = NA_character_, arquivo_csv_excluido = NA_character_,
                      nome_csv_mantido = NA_character_, nome_csv_excluido = NA_character_,
                      nome_zip_externo_mantido = NA_character_, nome_zip_externo_excluido = NA_character_,
                      nome_zip_interno_mantido = NA_character_, nome_zip_interno_excluido = NA_character_,
                      md5 = NA_character_, detalhe = NA_character_, relatorio_origem = NA_character_, acao_recomendada = NA_character_) {
    data.table(
      categoria = as.character(categoria),
      severidade = as.character(severidade),
      status = as.character(status),
      fase = as.character(fase),
      UC = as.character(UC),
      CICLO = as.character(CICLO),
      CAMPANHA = as.character(CAMPANHA),
      UA = as.character(UA),
      COLETA = as.character(COLETA),
      tipo_entrada = as.character(tipo_entrada),
      n_registros = suppressWarnings(as.integer(n_registros)),
      n_pontos = suppressWarnings(as.integer(n_pontos)),
      arquivo_csv = as.character(arquivo_csv),
      nome_csv = as.character(nome_csv),
      arquivo_entrada_externo = as.character(arquivo_entrada_externo),
      nome_arquivo_entrada_externo = as.character(nome_arquivo_entrada_externo),
      arquivo_zip_externo = as.character(arquivo_zip_externo),
      nome_zip_externo = as.character(nome_zip_externo),
      arquivo_zip_interno = as.character(arquivo_zip_interno),
      nome_zip_interno = as.character(nome_zip_interno),
      caminho_relativo_extraido = as.character(caminho_relativo_extraido),
      arquivo_csv_mantido = as.character(arquivo_csv_mantido),
      arquivo_csv_excluido = as.character(arquivo_csv_excluido),
      nome_csv_mantido = as.character(nome_csv_mantido),
      nome_csv_excluido = as.character(nome_csv_excluido),
      nome_zip_externo_mantido = as.character(nome_zip_externo_mantido),
      nome_zip_externo_excluido = as.character(nome_zip_externo_excluido),
      nome_zip_interno_mantido = as.character(nome_zip_interno_mantido),
      nome_zip_interno_excluido = as.character(nome_zip_interno_excluido),
      md5 = as.character(md5),
      detalhe = as.character(detalhe),
      relatorio_origem = as.character(relatorio_origem),
      acao_recomendada = as.character(acao_recomendada)
    )
  }

  rows <- list()
  origem_tipos <- paste0("auditoria_tipos_entrada_", MONITORA_EXEC_ID, ".csv")
  origem_csv <- paste0("auditoria_arquivos_csv_", MONITORA_EXEC_ID, ".csv")
  origem_dup_exatos <- paste0("auditoria_arquivos_csv_duplicados_exatos_", MONITORA_EXEC_ID, ".csv")
  origem_rel <- paste0("relatorio_execucao_", MONITORA_EXEC_ID, ".csv")

  if (exists("csv_audit") && is.data.table(csv_audit) && nrow(csv_audit) > 0 && "tipo_entrada" %in% names(csv_audit)) {
    tipos <- csv_audit[, .(
      n_arquivos = .N,
      n_linhas = sum(as.integer(n_linhas), na.rm = TRUE),
      n_colunas_min = suppressWarnings(min(as.integer(n_colunas), na.rm = TRUE)),
      n_colunas_max = suppressWarnings(max(as.integer(n_colunas), na.rm = TRUE)),
      exemplos_csv = paste(utils::head(basename, 5), collapse = " | "),
      exemplos_zip_externo = paste(unique(na.omit(utils::head(nome_zip_externo, 5))), collapse = " | "),
      exemplos_zip_interno = paste(unique(na.omit(utils::head(nome_zip_interno, 5))), collapse = " | ")
    ), by = tipo_entrada]
    for (i in seq_len(nrow(tipos))) {
      rows[[length(rows) + 1]] <- add_row(
        categoria = "resumo_tipo_entrada",
        severidade = "INFO",
        status = "detectado",
        tipo_entrada = tipos$tipo_entrada[i],
        n_registros = tipos$n_linhas[i],
        nome_zip_externo = tipos$exemplos_zip_externo[i],
        nome_zip_interno = tipos$exemplos_zip_interno[i],
        detalhe = paste0(tipos$n_arquivos[i], " arquivo(s); colunas min/max=", tipos$n_colunas_min[i], "/", tipos$n_colunas_max[i], "; exemplos_csv=", tipos$exemplos_csv[i]),
        relatorio_origem = paste(origem_tipos, origem_csv, sep = " | "),
        acao_recomendada = "Conferir se os tipos detectados correspondem às entradas esperadas. Use nome_zip_externo/nome_zip_interno para localizar os pacotes baixados."
      )
    }
  }

  if (exists("MONITORA_DUPLICIDADES_ARQUIVO") && is.data.table(MONITORA_DUPLICIDADES_ARQUIVO) && nrow(MONITORA_DUPLICIDADES_ARQUIVO) > 0) {
    for (i in seq_len(nrow(MONITORA_DUPLICIDADES_ARQUIVO))) {
      d <- MONITORA_DUPLICIDADES_ARQUIVO[i]
      rows[[length(rows) + 1]] <- add_row(
        categoria = "csv_duplicado_exato",
        severidade = "AVISO",
        status = "arquivo_excluido_da_importacao",
        UC = if ("UC" %in% names(d)) d$UC else NA_character_,
        CICLO = if ("CICLO" %in% names(d)) d$CICLO else NA_character_,
        CAMPANHA = if ("CAMPANHA" %in% names(d)) d$CAMPANHA else NA_character_,
        UA = if ("UA" %in% names(d)) d$UA else NA_character_,
        COLETA = if ("COLETA" %in% names(d)) d$COLETA else NA_character_,
        tipo_entrada = if ("tipo_entrada" %in% names(d)) d$tipo_entrada else NA_character_,
        n_registros = if ("n_linhas" %in% names(d)) d$n_linhas else NA_integer_,
        arquivo_csv_mantido = d$arquivo_mantido,
        arquivo_csv_excluido = d$arquivo_excluido,
        nome_csv_mantido = d$basename_mantido,
        nome_csv_excluido = d$basename_excluido,
        nome_zip_externo_mantido = if ("nome_zip_externo_mantido" %in% names(d)) d$nome_zip_externo_mantido else NA_character_,
        nome_zip_externo_excluido = if ("nome_zip_externo_excluido" %in% names(d)) d$nome_zip_externo_excluido else NA_character_,
        nome_zip_interno_mantido = if ("nome_zip_interno_mantido" %in% names(d)) d$nome_zip_interno_mantido else NA_character_,
        nome_zip_interno_excluido = if ("nome_zip_interno_excluido" %in% names(d)) d$nome_zip_interno_excluido else NA_character_,
        md5 = d$md5,
        detalhe = "Dois ou mais CSVs tinham mesmo MD5 e mesmo tamanho em bytes; o primeiro foi mantido e o(s) seguinte(s) excluído(s) antes da consolidação.",
        relatorio_origem = paste(origem_rel, origem_csv, origem_dup_exatos, sep = " | "),
        acao_recomendada = "Localizar pelo nome_zip_externo/nome_zip_interno e confirmar se é duplicidade de exportação; verificar se alguma UA esperada ficou ausente no conjunto individual."
      )
    }
  }

  processa_compat <- function(obj_name, fase_nome) {
    if (!exists(obj_name, inherits = TRUE)) return(NULL)
    aud <- get(obj_name, inherits = TRUE)
    if (!data.table::is.data.table(aud)) data.table::setDT(aud)
    if (nrow(aud) == 0 || !"status" %in% names(aud)) return(NULL)
    origem <- paste0("auditoria_compatibilidade_fontes_v12_", fase_nome, "_", MONITORA_EXEC_ID, ".csv")
    problem <- aud[!is.na(status) & !status %in% c("presente_em_todas_as_fontes", "fonte_unica_apos_filtro_ou_deduplicacao")]
    if (nrow(problem) == 0) {
      return(add_row(
        categoria = "compatibilidade_fontes",
        severidade = "INFO",
        status = "sem_divergencias_relevantes",
        fase = fase_nome,
        n_registros = nrow(aud),
        detalhe = paste0(nrow(aud), " grupo(s) auditado(s) sem divergências relevantes nesta fase."),
        relatorio_origem = origem,
        acao_recomendada = "Nenhuma ação necessária para esta fase."
      ))
    }
    out <- list()
    reg_cols <- grep("^n_registros__", names(problem), value = TRUE)
    pts_cols <- grep("^n_pontos__", names(problem), value = TRUE)
    for (i in seq_len(nrow(problem))) {
      r <- problem[i]
      det_regs <- if (length(reg_cols) > 0) paste(paste0(sub("^n_registros__", "", reg_cols), "=", as.character(unlist(r[, ..reg_cols]))), collapse = "; ") else NA_character_
      det_pts <- if (length(pts_cols) > 0) paste(paste0(sub("^n_pontos__", "", pts_cols), "=", as.character(unlist(r[, ..pts_cols]))), collapse = "; ") else NA_character_
      rows_lacuna <- NULL
      out[[length(out) + 1]] <- add_row(
        categoria = "compatibilidade_fontes",
        severidade = ifelse(as.character(r$status) %in% c("divergencia_n_registros", "ausente_em_alguma_fonte"), "AVISO", "INFO"),
        status = as.character(r$status),
        fase = fase_nome,
        UC = if ("UC" %in% names(r)) r$UC else NA_character_,
        CICLO = if ("CICLO" %in% names(r)) r$CICLO else NA_character_,
        CAMPANHA = if ("CAMPANHA" %in% names(r)) r$CAMPANHA else NA_character_,
        UA = if ("UA" %in% names(r)) r$UA else NA_character_,
        n_registros = if ("registros_max" %in% names(r)) r$registros_max else NA_integer_,
        n_pontos = if ("pontos_max" %in% names(r)) r$pontos_max else NA_integer_,
        detalhe = paste0("Registros por fonte: ", det_regs, " | Pontos por fonte: ", det_pts),
        relatorio_origem = origem,
        acao_recomendada = "Conferir a UA/fonte indicada; se uma fonte estiver zerada, verificar duplicidade exata ou ausência no pacote de entrada."
      )
    }
    rbindlist(out, fill = TRUE)
  }
  comp_pre <- processa_compat("MONITORA_AUDITORIA_COMPAT_PRE", "pre_dedup")
  if (!is.null(comp_pre)) rows[[length(rows) + 1]] <- comp_pre
  comp_pos <- processa_compat("MONITORA_AUDITORIA_COMPAT_POST", "pos_dedup")
  if (!is.null(comp_pos)) rows[[length(rows) + 1]] <- comp_pos

  if (exists("MONITORA_DUPLICIDADES_SEMANTICAS_RESUMO") && is.data.table(MONITORA_DUPLICIDADES_SEMANTICAS_RESUMO) && nrow(MONITORA_DUPLICIDADES_SEMANTICAS_RESUMO) > 0) {
    sem <- MONITORA_DUPLICIDADES_SEMANTICAS_RESUMO
    rows[[length(rows) + 1]] <- add_row(
      categoria = "duplicidade_semantica",
      severidade = "AVISO",
      status = "linhas_sobrepostas_removidas_por_preferencia_de_fonte",
      n_registros = sum(as.integer(sem$n_chaves), na.rm = TRUE),
      detalhe = paste(paste0(sem$tipos_entrada, ": ", sem$n_chaves, " chave(s)"), collapse = " | "),
      relatorio_origem = paste0("auditoria_duplicidades_semanticas_", MONITORA_EXEC_ID, ".csv"),
      acao_recomendada = "Verificar se a sobreposição é esperada. Em entradas combinadas individual+lote, é normal haver chaves duplicadas e manter a fonte prioritária."
    )
  }

  if (exists("MONITORA_AUDITORIA_ANOS_INVALIDOS") && is.data.table(MONITORA_AUDITORIA_ANOS_INVALIDOS) && nrow(MONITORA_AUDITORIA_ANOS_INVALIDOS) > 0) {
    anos <- MONITORA_AUDITORIA_ANOS_INVALIDOS
    rows[[length(rows) + 1]] <- add_row(
      categoria = "anos_invalidos",
      severidade = "AVISO",
      status = "ano_invalido_ou_data_nao_convertida",
      n_registros = nrow(anos),
      detalhe = paste0(nrow(anos), " combinação(ões) com ANO inválido/NA ou fora da faixa plausível."),
      relatorio_origem = paste0("auditoria_anos_invalidos_", MONITORA_EXEC_ID, ".csv"),
      acao_recomendada = "Corrigir a data original ou verificar erro de parsing antes de aceitar estatísticas temporais e gráficos."
    )
  }

  if (exists("MONITORA_AUDITORIA_COMPLETUDE_101_PONTOS") && is.data.table(MONITORA_AUDITORIA_COMPLETUDE_101_PONTOS) && nrow(MONITORA_AUDITORIA_COMPLETUDE_101_PONTOS) > 0) {
    pts101 <- MONITORA_AUDITORIA_COMPLETUDE_101_PONTOS
    for (i in seq_len(nrow(pts101))) {
      r <- pts101[i]
      rows[[length(rows) + 1]] <- add_row(
        categoria = "completude_101_pontos",
        severidade = "AVISO",
        status = "grupo_com_pontos_ausentes_duplicados_ou_fora_da_faixa",
        UC = if ("UC" %in% names(r)) r$UC else NA_character_,
        CICLO = if ("CICLO" %in% names(r)) r$CICLO else NA_character_,
        CAMPANHA = if ("CAMPANHA" %in% names(r)) r$CAMPANHA else NA_character_,
        UA = if ("UA" %in% names(r)) r$UA else NA_character_,
        n_registros = if ("n_registros" %in% names(r)) r$n_registros else NA_integer_,
        n_pontos = if ("n_pontos" %in% names(r)) r$n_pontos else NA_integer_,
        detalhe = paste0("pontos_ausentes=", if ("pontos_ausentes" %in% names(r)) r$pontos_ausentes else "", "; pontos_duplicados=", if ("pontos_duplicados" %in% names(r)) r$pontos_duplicados else "", "; pontos_fora_1_101=", if ("pontos_fora_1_101" %in% names(r)) r$pontos_fora_1_101 else ""),
        relatorio_origem = paste0("auditoria_completude_101_pontos_", MONITORA_EXEC_ID, ".csv"),
        acao_recomendada = "Conferir e corrigir a UA/ano para garantir exatamente 101 pontos válidos antes de aceitar os produtos."
      )
    }
  }

  if (exists("MONITORA_REPORT") && is.data.table(MONITORA_REPORT) && nrow(MONITORA_REPORT) > 0 && all(c("severidade", "etapa") %in% names(MONITORA_REPORT))) {
    avisos <- MONITORA_REPORT[severidade %in% c("AVISO", "ERRO") & !etapa %in% c("duplicidade_arquivo", "deduplicacao_semantica")]
    if (nrow(avisos) > 0) {
      for (i in seq_len(nrow(avisos))) {
        rows[[length(rows) + 1]] <- add_row(
          categoria = paste0("relatorio_execucao_", tolower(avisos$severidade[i])),
          severidade = avisos$severidade[i],
          status = avisos$etapa[i],
          arquivo_csv = if ("arquivo" %in% names(avisos)) avisos$arquivo[i] else NA_character_,
          nome_csv = if ("arquivo" %in% names(avisos)) basename(avisos$arquivo[i]) else NA_character_,
          detalhe = if ("detalhe" %in% names(avisos)) avisos$detalhe[i] else NA_character_,
          relatorio_origem = origem_rel,
          acao_recomendada = if (avisos$severidade[i] == "ERRO") "Verificar antes de aceitar a execução." else "Revisar se o aviso afeta a validação da execução."
        )
      }
    }
  }

  if (length(rows) == 0) {
    resumo <- add_row(
      categoria = "resumo_achados",
      severidade = "INFO",
      status = "sem_achados_relevantes",
      detalhe = "Nenhum achado relevante foi identificado nas auditorias disponíveis.",
      acao_recomendada = "Nenhuma ação necessária."
    )
  } else {
    resumo <- rbindlist(rows, fill = TRUE)
  }

  resumo[, id_achado := sprintf("ACH-%04d", seq_len(.N))]
  setcolorder(resumo, c("id_achado", setdiff(names(resumo), "id_achado")))
  saida_log <- file.path(MONITORA_LOG_DIR, paste0("auditoria_resumo_achados_relevantes_", MONITORA_EXEC_ID, ".csv"))
  saida_out <- file.path(MONITORA_OUTPUT_DIR, "auditoria_resumo_achados_relevantes_ultima_execucao.csv")
  fwrite(resumo, saida_log)
  fwrite(resumo, saida_out)
  monitora_log("resumo_achados_validacao", "INFO", saida_log, paste0(nrow(resumo), " achado(s) consolidado(s) para verificação/validação"), "também exportado em output/auditoria_resumo_achados_relevantes_ultima_execucao.csv")
  invisible(resumo)
}

monitora_deduplicar_registros_amostrais <- function(dt, arquivos_entrada = NULL) {
  # Deduplicação semântica com tabela auxiliar estreita.
  # A tabela auxiliar contém apenas colunas necessárias para ordenar e selecionar registros.
  # Isso evita copiar todas as colunas durante a deduplicação.
  dt <- monitora_as_dt_ref(dt)
  source_col <- if ("MONITORA_ARQUIVO_ENTRADA" %in% names(dt)) {
    "MONITORA_ARQUIVO_ENTRADA"
  } else if (".id" %in% names(dt)) {
    ".id"
  } else {
    NA_character_
  }
  if (is.na(source_col)) {
    monitora_log("deduplicacao_semantica", "AVISO", NA_character_, "Coluna de proveniência ausente; deduplicacao semantica por arquivo nao executada", "mantido sem alteracao")
    return(dt)
  }

  id_chr <- as.character(dt[[source_col]])
  tipo <- rep("desconhecido", nrow(dt))
  if (!is.null(arquivos_entrada) && all(c("basename", "tipo_entrada") %in% names(arquivos_entrada))) {
    tipo_map <- setNames(as.character(arquivos_entrada$tipo_entrada), as.character(arquivos_entrada$basename))
    tipo[!is.na(tipo_map[id_chr])] <- tipo_map[id_chr[!is.na(tipo_map[id_chr])]]
  }
  data.table::set(dt, j = "MONITORA_TIPO_ENTRADA", value = tipo)

  norm_key <- function(x) {
    x <- monitora_norm_empty(x)
    x <- stringr::str_squish(toupper(as.character(x)))
    x
  }
  get_col <- function(nm) if (nm %in% names(dt)) dt[[nm]] else rep(NA_character_, nrow(dt))

  ponto_col <- if ("ponto_amostral (amostragem/registro)" %in% names(dt)) {
    "ponto_amostral (amostragem/registro)"
  } else if ("ponto_metro (amostragem/registro)" %in% names(dt)) {
    "ponto_metro (amostragem/registro)"
  } else {
    NA_character_
  }

  uuid_registro <- monitora_norm_empty(monitora_coalesce_vec(
    get_col("uuid (amostragem/registro)"),
    get_col("amostragem/registro/uuid")
  ))
  uuid_fallback <- monitora_norm_empty(monitora_coalesce_vec(get_col("UUID"), get_col("uuid")))
  uuid_fallback[as.character(dt$MONITORA_TIPO_ENTRADA) == "bruto_lote_sismonitora_dev"] <- NA_character_
  uuid_reg <- monitora_norm_empty(monitora_coalesce_vec(uuid_registro, uuid_fallback))
  key_uuid <- ifelse(!is.na(uuid_reg), paste0("UUID_REGISTRO::", uuid_reg), NA_character_)
  rm(uuid_registro, uuid_fallback, uuid_reg); monitora_gc("dedup_chave_uuid")

  key_uc <- norm_key(get_col("UC"))
  key_ciclo <- norm_key(get_col("CICLO"))
  key_campanha <- norm_key(get_col("CAMPANHA"))
  key_ua <- norm_key(get_col("UA"))
  key_ponto <- if (!is.na(ponto_col)) norm_key(get_col(ponto_col)) else rep(NA_character_, nrow(dt))
  key_campos_ok <- !is.na(key_uc) & !is.na(key_ciclo) & !is.na(key_campanha) & !is.na(key_ua) & !is.na(key_ponto)
  key_campos <- rep(NA_character_, nrow(dt))
  key_campos[key_campos_ok] <- paste(key_uc[key_campos_ok], key_ciclo[key_campos_ok], key_campanha[key_campos_ok], key_ua[key_campos_ok], key_ponto[key_campos_ok], sep = "||")
  # Para deduplicar corretamente exportações em lote e exportações individuais, a chave semântica
  # UC+CICLO+CAMPANHA+UA+ponto deve ter prioridade quando disponível nas duas fontes.
  # O UUID do registro pode ser diferente/ausente entre formatos e, se usado primeiro,
  # impede reconhecer sobreposição real entre exportação individual e lote dev.
  key <- ifelse(!is.na(key_campos), paste0("UC_CICLO_CAMPANHA_UA_PONTO::", key_campos), ifelse(!is.na(key_uuid), key_uuid, NA_character_))
  rm(key_uuid, key_uc, key_ciclo, key_campanha, key_ua, key_ponto, key_campos_ok, key_campos); monitora_gc("dedup_chave_campos")

  prioridade <- data.table::fifelse(tipo == "pos_tratamento_script", 1L,
                                    data.table::fifelse(tipo == "bruto_lote_sismonitora_dev", 2L,
                                                        data.table::fifelse(tipo == "bruto_individual_sismonitora", 3L, 9L)))
  aux <- data.table::data.table(
    MONITORA_ORDEM_ORIGINAL = seq_len(nrow(dt)),
    MONITORA_CHAVE_DEDUP = key,
    MONITORA_PRIORIDADE_DEDUP = prioridade,
    MONITORA_TIPO_ENTRADA = tipo,
    MONITORA_FONTE = id_chr
  )
  rm(key, prioridade, tipo, id_chr); monitora_gc("dedup_aux_estreito_criado")

  cand <- aux[!is.na(MONITORA_CHAVE_DEDUP)]
  if (nrow(cand) == 0) {
    monitora_log("deduplicacao_semantica", "AVISO", NA_character_, "Nenhuma chave amostral confiavel encontrada para deduplicacao semantica", "mantido sem alteracao")
    rm(aux, cand); monitora_gc("dedup_sem_chaves")
    return(dt)
  }

  data.table::setorder(cand, MONITORA_CHAVE_DEDUP, MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)
  manter_ordem <- cand[, .SD[1L], by = MONITORA_CHAVE_DEDUP]$MONITORA_ORDEM_ORIGINAL
  duplicadas <- cand[, .N, by = MONITORA_CHAVE_DEDUP][N > 1L]

  if (nrow(duplicadas) > 0) {
    n_dup_total <- nrow(duplicadas)
    exportar_chaves <- head(duplicadas$MONITORA_CHAVE_DEDUP, MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA)
    detalhes <- cand[MONITORA_CHAVE_DEDUP %in% exportar_chaves,
      .(
        n_registros = .N,
        arquivos = paste(unique(as.character(MONITORA_FONTE)), collapse = " | "),
        tipos_entrada = paste(unique(MONITORA_TIPO_ENTRADA), collapse = " | "),
        linhas_originais = paste(MONITORA_ORDEM_ORIGINAL, collapse = " | "),
        linha_mantida = min(MONITORA_ORDEM_ORIGINAL[order(MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)])
      ),
      by = MONITORA_CHAVE_DEDUP
    ][n_registros > 1L]
    detalhes[, auditoria_truncada := n_dup_total > MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA]
    detalhes[, n_chaves_duplicadas_total := n_dup_total]
    detalhes[, n_chaves_duplicadas_exportadas := nrow(detalhes)]
    fwrite(detalhes, file.path(MONITORA_LOG_DIR, paste0("auditoria_duplicidades_semanticas_", MONITORA_EXEC_ID, ".csv")))
    if (n_dup_total > MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA) {
      monitora_log("deduplicacao_semantica", "AVISO", NA_character_, paste0("Auditoria de duplicidades semânticas truncada: ", n_dup_total, " chaves duplicadas no total; ", nrow(detalhes), " exportadas"), "aumentar MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA se precisar do detalhe completo")
    }
    monitora_log(
      "deduplicacao_semantica",
      "AVISO",
      NA_character_,
      paste0(n_dup_total, " chaves amostrais duplicadas detectadas; ", nrow(detalhes), " exportadas na auditoria; mantida uma linha por chave"),
      "preferencia v11: pos_tratamento_script > bruto_lote_sismonitora_dev > bruto_individual_sismonitora > desconhecido; ver auditoria_duplicidades_semanticas"
    )
    rm(detalhes); monitora_gc("auditoria_duplicidades_semanticas")
  }

  sem_chave <- aux[is.na(MONITORA_CHAVE_DEDUP), MONITORA_ORDEM_ORIGINAL]
  keep <- sort(unique(c(sem_chave, manter_ordem)))
  removidas_n <- nrow(dt) - length(keep)
  if (removidas_n > 0) {
    monitora_log("deduplicacao_semantica", "INFO", NA_character_, paste0(removidas_n, " linhas removidas por duplicidade semantica"), "linhas sem chave confiavel foram mantidas")
  }
  rm(aux, cand, duplicadas, sem_chave, manter_ordem); monitora_gc("dedup_antes_subconjunto_final")
  out <- dt[keep]
  out[]
}

monitora_data_ano_plausivel <- function(d, ano_min = 2000L, ano_max = lubridate::year(Sys.Date()) + 1L) {
  ano <- suppressWarnings(as.integer(format(d, "%Y")))
  !is.na(d) & !is.na(ano) & ano >= ano_min & ano <= ano_max
}


monitora_deduplicar_final_por_ponto_ano <- function(dt) {
  # Trava final de integridade antes da construção de registros_corrig_stat.
  # Objetivo: impedir que a mesma unidade amostral/ponto/ano entre duas vezes
  # quando arquivos pós-tratamento são combinados com brutos individuais/lote.
  # A deduplicação inicial ocorre antes de algumas padronizações e pode deixar
  # passar sobreposições raras; esta etapa usa os campos já normalizados e ANO.
  dt <- monitora_as_dt_ref(dt)

  ponto_col <- if ("ponto_amostral (amostragem/registro)" %in% names(dt)) {
    "ponto_amostral (amostragem/registro)"
  } else if ("ponto_metro (amostragem/registro)" %in% names(dt)) {
    "ponto_metro (amostragem/registro)"
  } else {
    NA_character_
  }

  by_cols <- c("UC", "CICLO", "CAMPANHA", "UA", "ANO")
  by_cols <- by_cols[by_cols %in% names(dt)]
  if (is.na(ponto_col) || length(by_cols) < 4L) {
    monitora_log(
      "deduplicacao_final_ponto_ano",
      "AVISO",
      NA_character_,
      paste0("Campos insuficientes para deduplicação final: by_cols=", paste(by_cols, collapse = ", "), "; ponto_col=", ponto_col),
      "mantido sem alteração"
    )
    return(dt)
  }

  norm_key <- function(x) {
    x <- monitora_norm_empty(x)
    stringr::str_squish(toupper(as.character(x)))
  }

  aux <- data.table::data.table(MONITORA_ORDEM_ORIGINAL = seq_len(nrow(dt)))
  for (cc in by_cols) {
    data.table::set(aux, j = cc, value = norm_key(dt[[cc]]))
  }
  data.table::set(aux, j = "ponto", value = norm_key(dt[[ponto_col]]))

  campos_chave <- c(by_cols, "ponto")
  ok <- Reduce(`&`, lapply(campos_chave, function(cc) !is.na(aux[[cc]]) & nzchar(aux[[cc]])))
  aux[, MONITORA_CHAVE_FINAL := NA_character_]
  if (any(ok)) {
    aux[ok, MONITORA_CHAVE_FINAL := do.call(paste, c(.SD, sep = "||")), .SDcols = campos_chave]
  }

  tipo <- if ("MONITORA_TIPO_ENTRADA" %in% names(dt)) as.character(dt[["MONITORA_TIPO_ENTRADA"]]) else rep("desconhecido", nrow(dt))
  fonte <- if ("MONITORA_ARQUIVO_ENTRADA" %in% names(dt)) {
    as.character(dt[["MONITORA_ARQUIVO_ENTRADA"]])
  } else if (".id" %in% names(dt)) {
    as.character(dt[[".id"]])
  } else {
    rep(NA_character_, nrow(dt))
  }

  prioridade <- data.table::fifelse(tipo == "pos_tratamento_script", 1L,
    data.table::fifelse(tipo == "bruto_lote_sismonitora_dev", 2L,
      data.table::fifelse(tipo == "bruto_individual_sismonitora", 3L, 9L)
    )
  )
  data.table::set(aux, j = "MONITORA_TIPO_ENTRADA", value = tipo)
  data.table::set(aux, j = "MONITORA_FONTE", value = fonte)
  data.table::set(aux, j = "MONITORA_PRIORIDADE_DEDUP", value = prioridade)

  cand <- aux[!is.na(MONITORA_CHAVE_FINAL)]
  if (nrow(cand) == 0) {
    monitora_log("deduplicacao_final_ponto_ano", "AVISO", NA_character_, "Nenhuma chave final UC/CICLO/CAMPANHA/UA/ANO/ponto confiável encontrada", "mantido sem alteração")
    rm(aux, cand); monitora_gc("dedup_final_sem_chaves")
    return(dt)
  }

  duplicadas <- cand[, .N, by = MONITORA_CHAVE_FINAL][N > 1L]
  if (nrow(duplicadas) == 0) {
    monitora_log("deduplicacao_final_ponto_ano", "INFO", NA_character_, "Nenhuma duplicidade final UC/CICLO/CAMPANHA/UA/ANO/ponto encontrada", "sem remoção")
    rm(aux, cand, duplicadas); monitora_gc("dedup_final_sem_duplicidade")
    return(dt)
  }

  data.table::setorder(cand, MONITORA_CHAVE_FINAL, MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)
  manter_ordem <- cand[, .SD[1L], by = MONITORA_CHAVE_FINAL]$MONITORA_ORDEM_ORIGINAL
  sem_chave <- aux[is.na(MONITORA_CHAVE_FINAL), MONITORA_ORDEM_ORIGINAL]
  keep <- sort(unique(c(sem_chave, manter_ordem)))

  exportar_chaves <- head(duplicadas$MONITORA_CHAVE_FINAL, MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA)
  detalhes <- cand[MONITORA_CHAVE_FINAL %in% exportar_chaves,
    .(
      n_registros = .N,
      tipos_entrada = paste(unique(MONITORA_TIPO_ENTRADA), collapse = " | "),
      arquivos = paste(unique(as.character(MONITORA_FONTE)), collapse = " | "),
      linhas_originais = paste(MONITORA_ORDEM_ORIGINAL, collapse = " | "),
      linha_mantida = MONITORA_ORDEM_ORIGINAL[order(MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)][1L],
      tipo_mantido = MONITORA_TIPO_ENTRADA[order(MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)][1L],
      arquivo_mantido = MONITORA_FONTE[order(MONITORA_PRIORIDADE_DEDUP, MONITORA_ORDEM_ORIGINAL)][1L]
    ),
    by = MONITORA_CHAVE_FINAL
  ][n_registros > 1L]
  detalhes[, auditoria_truncada := nrow(duplicadas) > MONITORA_MAX_CHAVES_AUDITORIA_DUP_SEMANTICA]
  detalhes[, n_chaves_duplicadas_total := nrow(duplicadas)]
  detalhes[, n_chaves_duplicadas_exportadas := nrow(detalhes)]

  saida <- file.path(MONITORA_LOG_DIR, paste0("auditoria_deduplicacao_final_ponto_ano_", MONITORA_EXEC_ID, ".csv"))
  fwrite(detalhes, saida)
  fwrite(detalhes, file.path(MONITORA_OUTPUT_DIR, "auditoria_deduplicacao_final_ponto_ano_ultima_execucao.csv"))

  removidas_n <- nrow(dt) - length(keep)
  monitora_log(
    "deduplicacao_final_ponto_ano",
    "AVISO",
    saida,
    paste0(nrow(duplicadas), " chaves UC/CICLO/CAMPANHA/UA/ANO/ponto duplicadas detectadas; ", removidas_n, " linha(s) removida(s) antes de registros_corrig_stat"),
    "preferência final: pos_tratamento_script > bruto_lote_sismonitora_dev > bruto_individual_sismonitora > desconhecido"
  )

  rm(aux, cand, duplicadas, detalhes, manter_ordem, sem_chave); monitora_gc("dedup_final_antes_subconjunto")
  out <- dt[keep]
  out[]
}

monitora_parse_date <- function(x) {
  x0 <- monitora_norm_empty(x)
  x1 <- stringr::str_squish(x0)
  x1 <- stringr::str_replace(x1, "T", " ")
  x1 <- stringr::str_replace(x1, "Z$", "")
  x_date_only <- stringr::str_replace(x1, "^([^ ]+).*$", "\\1")
  out <- rep(as.IDate(NA), length(x1))

  # Datas numéricas do Excel: dias desde 1899-12-30.
  suppressWarnings(num <- as.numeric(x1))
  is_excel <- !is.na(num) & num > 20000 & num < 80000
  if (any(is_excel)) {
    d_excel <- as.IDate(as.Date(num[is_excel], origin = "1899-12-30"))
    ok_excel <- monitora_data_ano_plausivel(d_excel)
    out[which(is_excel)[ok_excel]] <- d_excel[ok_excel]
  }

  # Datas com ano de 4 dígitos no final, comuns em CSVs manipulados em planilhas.
  # Estes padrões são tratados antes de qualquer tentativa %Y/%m/%d para evitar converter
  # valores como 3/3/2020 em ano 0003.
  idx_final4 <- which(is.na(out) & !is.na(x_date_only) & grepl("^\\d{1,2}[/-]\\d{1,2}[/-]\\d{4}$", x_date_only))
  if (length(idx_final4) > 0) {
    v <- x_date_only[idx_final4]
    suppressWarnings(d_br <- as.IDate(v, format = "%d/%m/%Y"))
    suppressWarnings(d_us <- as.IDate(v, format = "%m/%d/%Y"))
    # Para dia e mês iguais, d_br == d_us; ainda assim adota-se explicitamente d/m/Y.
    ok_br <- monitora_data_ano_plausivel(d_br)
    ok_us <- monitora_data_ano_plausivel(d_us)
    amb <- ok_br & ok_us & d_br != d_us
    if (any(amb, na.rm = TRUE)) {
      monitora_log(
        "datas", "AVISO", NA_character_,
        paste0(sum(amb), " datas ambiguas dd/mm/yyyy vs mm/dd/yyyy; adotado dd/mm/yyyy por padrão brasileiro"),
        "verificar registros se o arquivo tiver sido exportado/configurado em locale norte-americano"
      )
    }
    escolhido <- rep(as.IDate(NA), length(v))
    escolhido[ok_br] <- d_br[ok_br]
    escolhido[!ok_br & ok_us] <- d_us[!ok_br & ok_us]
    ok_escolhido <- monitora_data_ano_plausivel(escolhido)
    out[idx_final4[ok_escolhido]] <- escolhido[ok_escolhido]
  }

  # ISO primeiro, depois padrão brasileiro, depois padrão norte-americano.
  # Formatos com ano de 4 dígitos ao final já foram priorizados acima.
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
    "%d/%m/%y", "%d-%m-%y", "%d.%m.%y",
    "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y",
    "%m/%d/%y", "%m-%d-%y", "%m.%d.%y",
    "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%d/%m/%Y %H:%M",
    "%m/%d/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S", "%m/%d/%Y %H:%M"
  )
  for (fmt in formats) {
    idx <- which(is.na(out) & !is.na(x1))
    if (length(idx) == 0) break
    suppressWarnings(parsed <- as.IDate(x1[idx], format = fmt))
    ok <- monitora_data_ano_plausivel(parsed)
    if (any(ok)) out[idx[ok]] <- parsed[ok]
  }

  # Segunda tentativa com a parte anterior ao espaço, útil para datas com hora/fuso não
  # padronizados.
  for (fmt in formats[1:18]) {
    idx <- which(is.na(out) & !is.na(x_date_only))
    if (length(idx) == 0) break
    suppressWarnings(parsed <- as.IDate(x_date_only[idx], format = fmt))
    ok <- monitora_data_ano_plausivel(parsed)
    if (any(ok)) out[idx[ok]] <- parsed[ok]
  }

  n_fail <- sum(is.na(out) & !is.na(x1))
  if (n_fail > 0) {
    monitora_log("datas", "AVISO", NA_character_, paste0(n_fail, " datas nao convertidas ou com ano fora da faixa plausivel"), "ANO ficará NA nesses registros; ver auditoria_anos_invalidos")
  }
  out
}

monitora_normalize_coord <- function(x) {
  x0 <- monitora_norm_empty(x)
  out <- rep(NA_character_, length(x0))
  sep_type <- rep(NA_character_, length(x0))
  token_count <- rep(NA_integer_, length(x0))
  for (i in seq_along(x0)) {
    v <- x0[i]
    if (is.na(v)) next
    v <- stringr::str_squish(v)
    nums <- stringr::str_extract_all(v, "[-+]?\\d+(?:[\\.,]\\d+)?")[[1]]
    token_count[i] <- length(nums)
    if (grepl(";", v)) sep_type[i] <- "ponto_e_virgula"
    else if (grepl("\\|", v)) sep_type[i] <- "pipe"
    else if (grepl(",", v) && grepl("\\.", v)) sep_type[i] <- "virgula_separador_decimal_ponto"
    else if (grepl("[-+]?\\d+,\\d+", v) && !grepl("\\.", v)) sep_type[i] <- "virgula_decimal_ou_separador"
    else sep_type[i] <- "espaco"
    if (length(nums) < 2) next
    nums <- gsub(",", ".", nums, fixed = TRUE)
    nums <- nums[1:min(length(nums), 4)]
    out[i] <- paste(nums, collapse = " ")
  }
  attr(out, "coord_audit") <- data.table(separador_detectado = sep_type, n_tokens = token_count)
  out
}

monitora_extract_recursive_zips <- function(source_dirs, extract_dir) {
  if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE, force = TRUE)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  seen <- character()
  repeat {
    zips <- unique(unlist(lapply(source_dirs, function(d) {
      if (!dir.exists(d)) character() else list.files(d, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    })))
    zips <- zips[!grepl(paste0("^", normalizePath(extract_dir, winslash = "/", mustWork = FALSE)), normalizePath(zips, winslash = "/", mustWork = FALSE))]
    inner <- list.files(extract_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    zips <- unique(c(zips, inner))
    todo <- setdiff(normalizePath(zips, winslash = "/", mustWork = FALSE), seen)
    if (length(todo) == 0) break
    for (zf in todo) {
      dest <- file.path(extract_dir, tools::file_path_sans_ext(basename(zf)))
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      ok <- tryCatch({ unzip(zf, exdir = dest); TRUE }, error = function(e) { monitora_log("extracao_zip", "ERRO", zf, conditionMessage(e), "arquivo ignorado"); FALSE })
      if (ok) monitora_log("extracao_zip", "INFO", zf, paste0("extraido em ", dest), "OK")
      seen <- c(seen, zf)
    }
  }
  invisible(seen)
}

# Identifica a cadeia de origem de cada CSV, incluindo ZIP externo e ZIP interno quando houver.
# Isso facilita a conferência operacional: o relatório mostra tanto o CSV interno quanto o ZIP
# baixado pelo usuário.
monitora_mapear_origem_arquivo <- function(paths, source_dirs = MONITORA_SOURCE_DIRS, extract_dir = MONITORA_EXTRACT_DIR) {
  paths_norm <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  extract_norm <- normalizePath(extract_dir, winslash = "/", mustWork = FALSE)

  zip_sources <- unique(unlist(lapply(source_dirs, function(d) {
    if (!dir.exists(d)) character() else list.files(d, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  })))
  zip_sources_norm <- normalizePath(zip_sources, winslash = "/", mustWork = FALSE)

  # Quando a origem é o diretório do script, list.files(..., recursive=TRUE)
  # também encontra ZIPs internos já extraídos em extracted/. Esses ZIPs internos não devem
  # ser tratados como "ZIP externo baixado pelo usuário". Mantê-los em zip_sources fazia
  # registros_j7n30yrt.zip aparecer como nome_zip_externo, em vez do pacote original
  # 2025_PNCV_dados_sismonitora_export_sistema.zip.
  if (length(zip_sources_norm) > 0) {
    bad_prefixes <- normalizePath(c(MONITORA_EXTRACT_DIR, MONITORA_OUTPUT_DIR, MONITORA_LOG_DIR), winslash = "/", mustWork = FALSE)
    keep <- rep(TRUE, length(zip_sources_norm))
    for (bp in bad_prefixes) {
      keep <- keep & !startsWith(zip_sources_norm, paste0(bp, "/")) & zip_sources_norm != bp
    }
    zip_sources_norm <- zip_sources_norm[keep]
  }

  zip_sources_base <- basename(zip_sources_norm)
  zip_sources_stem <- tools::file_path_sans_ext(zip_sources_base)

  # Indexa o conteúdo dos ZIPs externos originais.
  # Isso permite mapear um ZIP interno de coleta, como registros_j7n30yrt.zip,
  # de volta ao pacote baixado pelo usuário, mesmo que a extração recursiva
  # tenha colocado o CSV em extracted/registros_j7n30yrt/registros_*.csv.
  zip_index <- data.table::data.table(
    zip_externo = character(),
    nome_zip_externo = character(),
    item_zip = character(),
    nome_item_zip = character(),
    stem_item_zip = character()
  )
  if (length(zip_sources_norm) > 0) {
    for (zs in zip_sources_norm) {
      lst <- tryCatch(utils::unzip(zs, list = TRUE), error = function(e) NULL)
      if (!is.null(lst) && "Name" %in% names(lst) && nrow(lst) > 0) {
        zi <- data.table::data.table(
          zip_externo = zs,
          nome_zip_externo = basename(zs),
          item_zip = as.character(lst$Name),
          nome_item_zip = basename(as.character(lst$Name))
        )
        zi[, stem_item_zip := tools::file_path_sans_ext(nome_item_zip)]
        zip_index <- data.table::rbindlist(list(zip_index, zi), use.names = TRUE, fill = TRUE)
      }
    }
  }

  # ZIPs encontrados dentro da área extraída. Estes normalmente são ZIPs internos
  # de coleta, como registros_j7n30yrt.zip, contidos no pacote externo baixado.
  zip_extraidos <- if (dir.exists(extract_dir)) list.files(extract_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE) else character()
  zip_extraidos_norm <- normalizePath(zip_extraidos, winslash = "/", mustWork = FALSE)
  zip_extraidos_base <- basename(zip_extraidos_norm)
  zip_extraidos_stem <- tools::file_path_sans_ext(zip_extraidos_base)

  top_zip_full_path <- function(nome_zip) {
    if (is.na(nome_zip) || !nzchar(nome_zip)) return(NA_character_)
    m <- which(zip_sources_base == nome_zip)
    if (length(m) > 0) return(zip_sources_norm[m[1]])
    NA_character_
  }

  external_from_inner_zip_name <- function(inner_zip_name) {
    if (is.na(inner_zip_name) || !nzchar(inner_zip_name) || nrow(zip_index) == 0) return(list(nome = NA_character_, arquivo = NA_character_))
    m <- zip_index[nome_item_zip == inner_zip_name]
    if (nrow(m) > 0) return(list(nome = m$nome_zip_externo[1], arquivo = m$zip_externo[1]))
    inner_stem <- tools::file_path_sans_ext(inner_zip_name)
    m <- zip_index[stem_item_zip == inner_stem]
    if (nrow(m) > 0) return(list(nome = m$nome_zip_externo[1], arquivo = m$zip_externo[1]))
    list(nome = NA_character_, arquivo = NA_character_)
  }

  external_from_rel_parts <- function(parts) {
    if (length(parts) < 1) return(list(nome = NA_character_, arquivo = NA_character_))

    # Caso 1: o primeiro componente do caminho extraído é o stem do pacote externo.
    cand_ext <- paste0(parts[1], ".zip")
    if (parts[1] %in% zip_sources_stem) {
      return(list(nome = cand_ext, arquivo = top_zip_full_path(cand_ext)))
    }

    # Caso 2: o primeiro componente é o stem do ZIP interno; procurar esse ZIP
    # dentro dos pacotes externos originais.
    cand_inner <- paste0(parts[1], ".zip")
    ext <- external_from_inner_zip_name(cand_inner)
    if (!is.na(ext$nome) && nzchar(ext$nome)) return(ext)

    # Caso 3: algum componente intermediário pode ser o stem do pacote externo.
    for (pp in parts) {
      cand <- paste0(pp, ".zip")
      if (pp %in% zip_sources_stem) {
        return(list(nome = cand, arquivo = top_zip_full_path(cand)))
      }
    }

    list(nome = NA_character_, arquivo = NA_character_)
  }

  inner_zip_from_rel_parts <- function(parts, nome_zip_externo = NA_character_) {
    if (length(parts) < 1) return(list(nome = NA_character_, arquivo = NA_character_))

    # Para CSVs extraídos de ZIP interno, o primeiro componente costuma ser
    # o stem do ZIP de coleta.
    cand_inner_name <- paste0(parts[1], ".zip")
    hits <- which(zip_extraidos_base == cand_inner_name | zip_extraidos_stem == parts[1])
    if (length(hits) > 0) {
      cand <- zip_extraidos_norm[hits]
      if (!is.na(nome_zip_externo) && nzchar(nome_zip_externo)) {
        top_stem <- tools::file_path_sans_ext(nome_zip_externo)
        preferred <- cand[grepl(paste0("/", top_stem, "/"), cand, fixed = TRUE)]
        if (length(preferred) > 0) cand <- preferred
      }
      return(list(nome = basename(cand[1]), arquivo = cand[1]))
    }

    # Se não houver o ZIP interno fisicamente no diretório extraído, mas ele
    # aparece indexado dentro do ZIP externo, ainda assim registrar o nome.
    ext <- external_from_inner_zip_name(cand_inner_name)
    if (!is.na(ext$nome) && nzchar(ext$nome)) {
      return(list(nome = cand_inner_name, arquivo = NA_character_))
    }

    list(nome = NA_character_, arquivo = NA_character_)
  }

  out <- data.table(
    arquivo = paths,
    arquivo_csv = paths_norm,
    nome_csv = basename(paths_norm),
    arquivo_entrada_externo = NA_character_,
    nome_arquivo_entrada_externo = NA_character_,
    arquivo_zip_externo = NA_character_,
    nome_zip_externo = NA_character_,
    arquivo_zip_interno = NA_character_,
    nome_zip_interno = NA_character_,
    caminho_relativo_extraido = NA_character_
  )

  for (i in seq_along(paths_norm)) {
    pn <- paths_norm[i]

    if (startsWith(pn, paste0(extract_norm, "/"))) {
      rel <- substring(pn, nchar(paste0(extract_norm, "/")) + 1)
      parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
      out$caminho_relativo_extraido[i] <- rel

      ext <- external_from_rel_parts(parts)
      if (!is.na(ext$nome) && nzchar(ext$nome)) {
        out$nome_zip_externo[i] <- ext$nome
        out$arquivo_zip_externo[i] <- ext$arquivo
        out$nome_arquivo_entrada_externo[i] <- ext$nome
        out$arquivo_entrada_externo[i] <- ext$arquivo
      }

      inn <- inner_zip_from_rel_parts(parts, out$nome_zip_externo[i])
      if (!is.na(inn$nome) && nzchar(inn$nome)) {
        # Não registrar o pacote externo como ZIP interno quando o CSV está
        # diretamente no pacote externo, como no lote dev.
        if (is.na(out$nome_zip_externo[i]) || !identical(inn$nome, out$nome_zip_externo[i])) {
          out$nome_zip_interno[i] <- inn$nome
          out$arquivo_zip_interno[i] <- inn$arquivo
        }
      }

      if ((is.na(out$arquivo_entrada_externo[i]) || !nzchar(out$arquivo_entrada_externo[i])) &&
          (!is.na(out$arquivo_zip_interno[i]) && nzchar(out$arquivo_zip_interno[i]))) {
        out$arquivo_entrada_externo[i] <- out$arquivo_zip_interno[i]
        out$nome_arquivo_entrada_externo[i] <- out$nome_zip_interno[i]
      }

    } else {
      out$arquivo_entrada_externo[i] <- pn
      out$nome_arquivo_entrada_externo[i] <- basename(pn)
      if (grepl("\\.zip$", pn, ignore.case = TRUE)) {
        out$arquivo_zip_externo[i] <- pn
        out$nome_zip_externo[i] <- basename(pn)
      }
    }
  }
  out[]
}

### Origem dos dados: se input/ tiver arquivos, usa input/. Caso contrário, usa o diretório do
### script.
input_has_files <- dir.exists(MONITORA_INPUT_DIR) && length(list.files(MONITORA_INPUT_DIR, recursive = TRUE, all.files = FALSE, no.. = TRUE)) > 0
MONITORA_SOURCE_DIRS <- if (input_has_files) MONITORA_INPUT_DIR else MONITORA_BASE_DIR
monitora_log("configuracao", "INFO", MONITORA_SOURCE_DIRS, "diretorio de origem selecionado", ifelse(input_has_files, "input/", "diretorio do script"))
monitora_perf_checkpoint("configuracao_inicial", paste0("origem: ", paste(MONITORA_SOURCE_DIRS, collapse = " | ")))

### Extração recursiva de ZIPs, incluindo ZIPs internos dentro de ZIPs.
monitora_extract_recursive_zips(MONITORA_SOURCE_DIRS, MONITORA_EXTRACT_DIR)
monitora_controlar_recursos("apos_extracao_recursiva_zips", risco = "normal", force_log = TRUE)
monitora_perf_checkpoint("extracao_recursiva_zips", "extração de ZIPs externos e internos")

### Conversão defensiva de XLSX para CSV apenas nas áreas de entrada/extração, não em output/log.
xlsx_files <- list.files(c(MONITORA_SOURCE_DIRS, MONITORA_EXTRACT_DIR), pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
xlsx_files <- xlsx_files[!grepl("/(output|log)/", normalizePath(xlsx_files, winslash = "/", mustWork = FALSE))]
for (xlsx_file in xlsx_files) {
  csv_file <- sub("\\.xlsx$", ".csv", xlsx_file, ignore.case = TRUE)
  if (!file.exists(csv_file)) {
    data <- read_excel(xlsx_file)
    write.csv(data, file = csv_file, row.names = FALSE, na = "")
    monitora_log("conversao_xlsx", "INFO", xlsx_file, paste0("convertido para ", csv_file), "OK")
  } else {
    monitora_log("conversao_xlsx", "INFO", xlsx_file, paste0("CSV ja existente: ", csv_file), "mantido")
  }
}
if (exists("data")) rm(data)
if (exists("csv_file")) rm(csv_file)
if (exists("xlsx_file")) rm(xlsx_file)
if (exists("xlsx_files")) rm(xlsx_files)
monitora_perf_checkpoint("conversao_xlsx", "conversão defensiva de XLSX para CSV, quando aplicável")

### Leitura e concatenação dos CSVs de entrada.
### Importante: arquivos registros_corrig*.csv são aceitos como entrada válida quando colocados em
### input/
### ou na pasta-base, pois é comum usar produtos pós-tratamento como base para nova rodada.
### Apenas output/, log/ e produtos analíticos/relatórios são excluídos automaticamente.
all_csvfiles <- list.files(c(MONITORA_SOURCE_DIRS, MONITORA_EXTRACT_DIR), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
all_csvfiles_norm <- normalizePath(all_csvfiles, winslash = "/", mustWork = FALSE)
all_csvfiles_base <- basename(all_csvfiles_norm)

# Exclusões de diretórios gerenciados pelo próprio script.
# Importante: quando MONITORA_SOURCE_DIRS = MONITORA_BASE_DIR, list.files(recursive=TRUE)
# também encontraria produtos antigos em output/, log/ e extrações antigas.
exclude_dirs <- grepl("/(output|log|plots_png)/", all_csvfiles_norm)

# Exclusões de produtos analíticos/relatórios conhecidos.
# NÃO exclui registros_corrig*.csv: estes podem ser insumos válidos de nova rodada
# quando o usuário os coloca deliberadamente em input/ ou na pasta-base.
exclude_outputs <- grepl(
  "^(registros_corrig_stat|sum_|cob_|prop_rel_|relatorio_execucao|auditoria_|duplicidades_|conflitos_)",
  all_csvfiles_base,
  ignore.case = TRUE
)

csvfiles_excluidos <- unique(all_csvfiles[exclude_dirs | exclude_outputs])
if (length(csvfiles_excluidos) > 0) {
  fwrite(
    data.table(
      arquivo = csvfiles_excluidos,
      motivo = ifelse(
        grepl("^(registros_corrig_stat|sum_|cob_|prop_rel_|relatorio_execucao|auditoria_|duplicidades_|conflitos_)",
              basename(csvfiles_excluidos), ignore.case = TRUE),
        "produto_analitico_saida_ou_relatorio",
        "diretorio_gerenciado_output_log"
      )
    ),
    file.path(MONITORA_LOG_DIR, paste0("arquivos_csv_excluidos_da_importacao_", MONITORA_EXEC_ID, ".csv"))
  )
  monitora_log(
    "filtro_csv_entrada",
    "INFO",
    NA_character_,
    paste0(length(csvfiles_excluidos), " CSVs excluidos da importacao por serem produtos analiticos/relatorios ou estarem em output/log"),
    "ver arquivos_csv_excluidos_da_importacao"
  )
}

csvfiles <- unique(all_csvfiles[!exclude_dirs & !exclude_outputs])
monitora_controlar_recursos("filtro_csv_entrada", risco = if (length(csvfiles) > 1000) "alto" else "normal", force_log = TRUE)
monitora_perf_checkpoint("filtro_csv_entrada", paste0(length(csvfiles), " CSV(s) candidatos após filtros"))

# Classificação formal de origem por nome de arquivo e cabeçalho.
# Essa informação será usada na auditoria e na deduplicação semântica, não para excluir o arquivo.
csvfiles_tipo_entrada <- vapply(csvfiles, monitora_detectar_tipo_csv, character(1))

if (any(csvfiles_tipo_entrada == "pos_tratamento_script")) {
  monitora_log(
    "csv_pos_tratamento_entrada",
    "INFO",
    paste(csvfiles[csvfiles_tipo_entrada == "pos_tratamento_script"], collapse = " | "),
    paste0(sum(csvfiles_tipo_entrada == "pos_tratamento_script"), " CSV(s) pós-tratamento aceitos como entrada válida"),
    "serao deduplicados semanticamente contra registros brutos/novos quando houver sobreposicao"
  )
}
if (any(csvfiles_tipo_entrada == "bruto_lote_sismonitora_dev")) {
  monitora_log(
    "csv_lote_dev_entrada",
    "INFO",
    paste(csvfiles[csvfiles_tipo_entrada == "bruto_lote_sismonitora_dev"], collapse = " | "),
    paste0(sum(csvfiles_tipo_entrada == "bruto_lote_sismonitora_dev"), " CSV(s) consolidado(s) da ferramenta dev detectado(s)"),
    "CSV consolidado usado como fonte principal; XLSX acompanhante nao e somado quando houver CSV equivalente"
  )
}
if (any(csvfiles_tipo_entrada == "bruto_individual_sismonitora")) {
  monitora_log(
    "csv_individual_sismonitora_entrada",
    "INFO",
    paste(utils::head(csvfiles[csvfiles_tipo_entrada == "bruto_individual_sismonitora"], 20), collapse = " | "),
    paste0(sum(csvfiles_tipo_entrada == "bruto_individual_sismonitora"), " CSV(s) individuais SISMONITORA detectado(s)"),
    "entrada tradicional exportada um a um"
  )
}

if (length(csvfiles) == 0) stop("Nenhum CSV de entrada encontrado. Coloque os ZIP/CSV/XLSX do SISMONITORA ou registros_corrig*.csv no diretório do script ou em input/.")

csv_origem_audit <- monitora_mapear_origem_arquivo(csvfiles)
csv_audit <- data.table(
  arquivo = csvfiles,
  basename = basename(csvfiles),
  tamanho_bytes = file.info(csvfiles)$size,
  md5 = as.character(tools::md5sum(csvfiles)),
  n_linhas = NA_integer_,
  n_colunas = NA_integer_,
  status_leitura = NA_character_,
  tipo_entrada = csvfiles_tipo_entrada
)
csv_audit <- cbind(csv_audit, csv_origem_audit[, .(arquivo_csv, nome_csv, arquivo_entrada_externo, nome_arquivo_entrada_externo, arquivo_zip_externo, nome_zip_externo, arquivo_zip_interno, nome_zip_interno, caminho_relativo_extraido)])

monitora_controlar_recursos("auditoria_csv_hash_md5", risco = if (nrow(csv_audit) > 1000) "alto" else "normal", objeto = csv_audit, force_log = TRUE)
monitora_perf_checkpoint("auditoria_csv_hash_md5", paste0(nrow(csv_audit), " CSV(s) auditados por tamanho e MD5"), csv_audit)

csv_keep <- rep(TRUE, length(csvfiles))
# Duplicidade confiável: mesmo hash MD5 e mesmo tamanho em bytes. Nesses casos, mantém o primeiro
# arquivo.
# Registra CSV interno, ZIP externo e ZIP interno para facilitar a conferência operacional.
monitora_peek_metadados_csv <- function(path) {
  dt0 <- tryCatch(fread(path, nrows = 1, colClasses = "character", encoding = "UTF-8", na.strings = c("", "NA", "N/A", "NULL"), showProgress = FALSE), error = function(e) NULL)
  if (is.null(dt0) || nrow(dt0) == 0) {
    return(data.table(UC = NA_character_, CICLO = NA_character_, CAMPANHA = NA_character_, UA = NA_character_, COLETA = NA_character_, n_linhas = NA_integer_))
  }
  pega <- function(cands) {
    nm <- cands[cands %in% names(dt0)][1]
    if (is.na(nm) || length(nm) == 0) NA_character_ else as.character(dt0[[nm]][1])
  }
  data.table(
    UC = pega(c("UC", "uc", "uc...5", "uc...17")),
    CICLO = pega(c("CICLO", "ciclo")),
    CAMPANHA = pega(c("CAMPANHA", "campanha")),
    UA = pega(c("UA", "ua")),
    COLETA = pega(c("COLETA", "coleta")),
    n_linhas = tryCatch(nrow(fread(path, select = 1, colClasses = "character", encoding = "UTF-8", showProgress = FALSE)), error = function(e) NA_integer_)
  )
}
MONITORA_DUPLICIDADES_ARQUIVO_LIST <- list()
MONITORA_DUPLICIDADES_ARQUIVO_I <- 0L
dup_groups <- split(seq_along(csvfiles), paste(csv_audit$md5, csv_audit$tamanho_bytes, sep = "::"))
for (g in dup_groups) {
  if (length(g) > 1 && !is.na(csv_audit$md5[g[1]])) {
    csv_keep[g[-1]] <- FALSE
    meta <- monitora_peek_metadados_csv(csvfiles[g[1]])
    for (excl in g[-1]) {
      MONITORA_DUPLICIDADES_ARQUIVO_I <- MONITORA_DUPLICIDADES_ARQUIVO_I + 1L
      MONITORA_DUPLICIDADES_ARQUIVO_LIST[[MONITORA_DUPLICIDADES_ARQUIVO_I]] <- cbind(
        data.table::data.table(
          md5 = csv_audit$md5[g[1]],
          tamanho_bytes = csv_audit$tamanho_bytes[g[1]],
          arquivo_mantido = csvfiles[g[1]],
          arquivo_excluido = csvfiles[excl],
          basename_mantido = basename(csvfiles[g[1]]),
          basename_excluido = basename(csvfiles[excl]),
          arquivo_entrada_externo_mantido = csv_audit$arquivo_entrada_externo[g[1]],
          arquivo_entrada_externo_excluido = csv_audit$arquivo_entrada_externo[excl],
          nome_arquivo_entrada_externo_mantido = csv_audit$nome_arquivo_entrada_externo[g[1]],
          nome_arquivo_entrada_externo_excluido = csv_audit$nome_arquivo_entrada_externo[excl],
          arquivo_zip_externo_mantido = csv_audit$arquivo_zip_externo[g[1]],
          arquivo_zip_externo_excluido = csv_audit$arquivo_zip_externo[excl],
          nome_zip_externo_mantido = csv_audit$nome_zip_externo[g[1]],
          nome_zip_externo_excluido = csv_audit$nome_zip_externo[excl],
          arquivo_zip_interno_mantido = csv_audit$arquivo_zip_interno[g[1]],
          arquivo_zip_interno_excluido = csv_audit$arquivo_zip_interno[excl],
          nome_zip_interno_mantido = csv_audit$nome_zip_interno[g[1]],
          nome_zip_interno_excluido = csv_audit$nome_zip_interno[excl],
          caminho_relativo_extraido_mantido = csv_audit$caminho_relativo_extraido[g[1]],
          caminho_relativo_extraido_excluido = csv_audit$caminho_relativo_extraido[excl],
          tipo_entrada = csv_audit$tipo_entrada[g[1]]
        ),
        meta
      )
    }
    monitora_log(
      "duplicidade_arquivo",
      "AVISO",
      paste(csvfiles[g], collapse = " | "),
      paste0("CSV duplicado exato: mesmo MD5 ", csv_audit$md5[g[1]], " e tamanho em bytes. Mantido: ", csvfiles[g[1]]),
      "duplicados excluidos da sequencia de tratamento"
    )
  }
}
MONITORA_DUPLICIDADES_ARQUIVO <- if (MONITORA_DUPLICIDADES_ARQUIVO_I > 0L) {
  data.table::rbindlist(MONITORA_DUPLICIDADES_ARQUIVO_LIST[seq_len(MONITORA_DUPLICIDADES_ARQUIVO_I)], fill = TRUE, use.names = TRUE)
} else {
  data.table::data.table()
}
rm(MONITORA_DUPLICIDADES_ARQUIVO_LIST, MONITORA_DUPLICIDADES_ARQUIVO_I)
if (nrow(MONITORA_DUPLICIDADES_ARQUIVO) > 0) {
  fwrite(MONITORA_DUPLICIDADES_ARQUIVO, file.path(MONITORA_LOG_DIR, paste0("auditoria_arquivos_csv_duplicados_exatos_", MONITORA_EXEC_ID, ".csv")))
}

csvfiles_lidos <- csvfiles[csv_keep]
csvfiles_sucesso <- character()
registros_batches <- list()
batch_lista <- list()
batch_n <- 0L
batch_id <- 0L
flush_batch <- function(force = FALSE) {
  if (length(batch_lista) == 0) return(invisible(FALSE))
  if (!force && length(batch_lista) < monitora_batch_size_csv()) return(invisible(FALSE))
  batch_id <<- batch_id + 1L
  monitora_controlar_recursos(paste0("leitura_csv_batch_", batch_id, "_pre_rbind"), risco = if (length(batch_lista) >= 50) "alto" else "normal", force_log = TRUE)
  registros_batches[[batch_id]] <<- data.table::rbindlist(batch_lista, fill = TRUE, use.names = TRUE)
  batch_lista <<- list()
  monitora_controlar_recursos(paste0("leitura_csv_batch_", batch_id, "_pos_rbind"), risco = "normal", objeto = registros_batches[[batch_id]], force_log = TRUE)
  monitora_gc(paste0("leitura_csv_batch_", batch_id))
  invisible(TRUE)
}
for (i in seq_along(csvfiles_lidos)) {
  f <- csvfiles_lidos[i]
  dt <- tryCatch({
    x <- fread(f, colClasses = "character", encoding = "UTF-8", na.strings = c("", "NA", "N/A", "NULL"), showProgress = FALSE)
    x[, MONITORA_ARQUIVO_ORIGEM_EXECUCAO := basename(f)]
    csv_audit[arquivo == f, `:=`(n_linhas = nrow(x), n_colunas = ncol(x), status_leitura = "OK")]
    csvfiles_sucesso <- c(csvfiles_sucesso, f)
    x
  }, error = function(e) {
    csv_audit[arquivo == f, status_leitura = paste("ERRO:", conditionMessage(e))]
    monitora_log("leitura_csv", "ERRO", f, conditionMessage(e), "arquivo ignorado")
    NULL
  })
  if (!is.null(dt)) {
    batch_n <- batch_n + 1L
    batch_lista[[batch_n]] <- dt
    if (length(batch_lista) >= monitora_batch_size_csv()) {
      flush_batch(force = TRUE)
      batch_n <- 0L
    }
  }
}
flush_batch(force = TRUE)
rm(batch_lista); monitora_gc("leitura_csv_pos_batches")
if (length(registros_batches) == 0) stop("Nenhum CSV pôde ser lido com sucesso.")
monitora_perf_checkpoint("leitura_csv", paste0(length(csvfiles_sucesso), " CSV(s) lidos com sucesso em ", length(registros_batches), " lote(s)"))

monitora_controlar_recursos("concatenacao_rbindlist_pre", risco = "muito_alto", force_log = TRUE)
registros <- data.table::rbindlist(registros_batches, fill = TRUE, use.names = TRUE)

# Remove objetos temporários usados na leitura dos CSVs para evitar que
# tabelas auxiliares, como o último CSV lido, permaneçam no ambiente do RStudio.
rm(list = intersect(
  c("registros_batches", "batch_lista", "batch_n", "batch_id", "dt", "x", "f", "i"),
  ls(envir = .GlobalEnv)
), envir = .GlobalEnv)
monitora_gc("concatenacao_rbindlist")
monitora_controlar_recursos("concatenacao_rbindlist_pos", risco = "normal", objeto = registros, force_log = TRUE)
# Adiciona colunas por referência com setalloccol()/set(), reduzindo risco de cópia rasa.
data.table::setalloccol(registros, ncol(registros) + 8L)
data.table::set(registros, j = "MONITORA_ARQUIVO_ENTRADA", value = as.character(registros[["MONITORA_ARQUIVO_ORIGEM_EXECUCAO"]]))
data.table::set(registros, j = ".id", value = registros[["MONITORA_ARQUIVO_ENTRADA"]])
registros <- monitora_drop_legacy_technical_columns(registros, "apos_concatenacao")
monitora_perf_checkpoint("limpeza_colunas_tecnicas_legadas", "remoção de colunas técnicas legadas de execuções anteriores", registros)
monitora_perf_checkpoint("concatenacao_rbindlist", "concatenação dos CSVs lidos por lotes", registros)
MONITORA_ARQUIVOS_ENTRADA <- csv_audit[arquivo %in% csvfiles_sucesso, .(arquivo, basename, tipo_entrada, md5, tamanho_bytes, n_linhas, n_colunas, arquivo_entrada_externo, nome_arquivo_entrada_externo, arquivo_zip_externo, nome_zip_externo, arquivo_zip_interno, nome_zip_interno, caminho_relativo_extraido)]

# Normaliza sufixos artificiais tipo ...5 e consolida colunas duplicadas/aliases antes das
# renomeações específicas do formulário.
monitora_controlar_recursos("pre_merge_duplicate_columns_importacao", risco = "muito_alto", objeto = registros, force_log = TRUE)
monitora_perf_checkpoint("pre_merge_duplicate_columns_importacao", "antes do merge econômico de colunas duplicadas/sufixadas", registros)
registros <- monitora_merge_duplicate_columns(registros)
monitora_controlar_recursos("pos_merge_duplicate_columns_importacao", risco = "normal", objeto = registros, force_log = TRUE)
monitora_gc("pos_merge_duplicate_columns_importacao")
monitora_perf_checkpoint("normalizacao_colunas_importacao", "merge de colunas duplicadas/sufixadas antes das renomeações", registros)
col_audit_import <- attr(registros, "audit_duplicate_columns")
if (!is.null(col_audit_import) && nrow(col_audit_import) > 0) {
  fwrite(col_audit_import, file.path(MONITORA_LOG_DIR, paste0("auditoria_colunas_duplicadas_importacao_", MONITORA_EXEC_ID, ".csv")))
  monitora_log("colunas_duplicadas", "INFO", NA_character_, paste0(nrow(col_audit_import), " nomes de colunas duplicados/sufixados tratados na importacao"), "ver auditoria_colunas_duplicadas_importacao")
}
registros <- monitora_consolidate_aliases(registros)
monitora_controlar_recursos("consolidacao_aliases_importacao", risco = "alto", objeto = registros, force_log = TRUE)
monitora_perf_checkpoint("consolidacao_aliases_importacao", "coalesce de aliases críticos antes da criação de registros_corrig", registros)

fwrite(csv_audit, file.path(MONITORA_LOG_DIR, paste0("auditoria_arquivos_csv_", MONITORA_EXEC_ID, ".csv")))
monitora_resumir_fontes_por_tipo(csv_audit)

rm(all_csvfiles, all_csvfiles_norm, exclude_dirs, exclude_outputs, csv_keep, csvfiles_lidos, csvfiles_sucesso, registros_lista, dup_groups, csvfiles_tipo_entrada)

### criação do arquivo onde serão realizadas as correções, mantendo o arquivo original:

registros -> registros_corrig
rm(registros); monitora_gc("apos_criacao_registros_corrig_remocao_binding_registros")

###

names(registros_corrig) <- str_replace_all(names(registros_corrig), '\\"{4}', '\\"\"')

###

### Padronização de colunas técnicas para os nomes finais usados nas análises.

# Renomeia colunas técnicas para nomes finais sem criar nomes duplicados.
# Se o nome final já existir, consolida valores da coluna antiga no alvo por referência
# e remove a coluna antiga. Isso evita colnames<- criar duplicatas, que pode zerar
# colunas analíticas na junção posterior e gerar gráficos em branco.
monitora_renomear_ou_consolidar_coluna <- function(dt, old, new, etapa = "renomeacao_colunas_protocolo") {
  dt <- monitora_as_dt_ref(dt)
  if (!(old %in% names(dt))) return(invisible(dt))
  if (identical(old, new)) return(invisible(dt))

  if (!(new %in% names(dt))) {
    data.table::setnames(dt, old = old, new = new)
    return(invisible(dt))
  }

  n <- nrow(dt)
  chunk_n <- suppressWarnings(as.integer(Sys.getenv("MONITORA_RENAME_COALESCE_CHUNK_ROWS", unset = "50000")))
  if (is.na(chunk_n) || chunk_n < 1000L) chunk_n <- 50000L

  conflito_total <- 0L
  preenchidos_total <- 0L
  seq_starts <- seq.int(1L, max(1L, n), by = chunk_n)
  for (st in seq_starts) {
    en <- min(st + chunk_n - 1L, n)
    ii <- st:en
    alvo <- monitora_norm_empty(dt[[new]][ii])
    aux <- monitora_norm_empty(dt[[old]][ii])

    fill_idx <- is.na(alvo) & !is.na(aux)
    if (any(fill_idx)) {
      data.table::set(dt, i = ii[fill_idx], j = new, value = aux[fill_idx])
      preenchidos_total <- preenchidos_total + sum(fill_idx)
    }

    conflito <- !is.na(alvo) & !is.na(aux) & alvo != aux
    if (any(conflito)) conflito_total <- conflito_total + sum(conflito)
    rm(alvo, aux, fill_idx, conflito)
  }

  dt[, (old) := NULL]
  monitora_log(
    etapa,
    if (conflito_total > 0L) "AVISO" else "INFO",
    NA_character_,
    paste0("Coluna técnica consolidada em nome final: ", old, " -> ", new,
           "; preenchidos=", preenchidos_total,
           "; conflitos_preservando_alvo=", conflito_total),
    "consolidacao_segura_sem_criar_colunas_duplicadas"
  )
  if (monitora_deve_gc()) gc(verbose = FALSE)
  invisible(dt)
}


if ("registro_uuid" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "registro_uuid", "UUID")
}
if ("coleta" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "coleta", "COLETA")
}
if ("data do registro" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data do registro", "DATA DO REGISTRO")
}
if ("data_do_registro" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data_do_registro", "DATA DO REGISTRO")
}
if ("data do recebimento" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data do recebimento", "DATA DO RECEBIMENTO")
}
if ("data_do_recebimento" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data_do_recebimento", "DATA DO RECEBIMENTO")
}
if ("ultima_edicao" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "ultima_edicao", "ULTIMA EDICAO")
}

if ("uc" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "uc", "UC")
}
if ("ciclo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "ciclo", "CICLO")
}
if ("campanha" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "campanha", "CAMPANHA")
}
if ("protocolo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "protocolo", "PROTOCOLO")
}
if ("ea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "ea", "EA")
}
if ("ua" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "ua", "UA")
}
if ("usuario" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "usuario", "USUARIO")
}
if ("coletores" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "coletores", "COLETORES")
}
if ("validado" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "validado", "VALIDADO")
}
if ("validado por" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "validado por", "VALIDADO POR")
}
if ("validador" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "validador", "VALIDADO POR")
}
if ("data validacao" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data validacao", "DATA VALIDAÇÃO")
}
if ("data_hora/data" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data_hora/data", "Data (data_hora)")
}
if ("data_hora/hora" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "data_hora/hora", "Horário (data_hora)")
}
if ("form_veg" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "form_veg", "Qual a formação vegetacional onde está situado o transecto?")
}
if ("impact_manejo_uso/impacto_manejo_uso" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "impact_manejo_uso/impacto_manejo_uso", "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)")
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "impact_manejo_uso/tipos_impacto_manejo_uso", "Qual(is)? (impact_manejo_uso)")
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso_outro" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "impact_manejo_uso/tipos_impacto_manejo_uso_outro", "Outros tipos de manejo ou uso: (impact_manejo_uso)")
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso_descricao" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "impact_manejo_uso/tipos_impacto_manejo_uso_descricao", "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)")
}
if ("observacoes_gerais" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "observacoes_gerais", "Descreva observações gerais do transecto, caso necessário:")
}
if ("amostragem/ponto_inicio_transecto" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/ponto_inicio_transecto", "Coordenada inicial da amostragem (amostragem)")
}
if ("amostragem/foto_ponto_inicial" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/foto_ponto_inicial", "Foto do ponto inicial do transecto (amostragem)")
}
if ("amostragem/num_placa" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/num_placa", "Número da plaqueta (amostragem)")
}
if ("amostragem/modulo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/modulo", "Módulo (amostragem)")
}
if ("amostragem/registro/ponto_amostral" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/ponto_amostral", "ponto_amostral (amostragem/registro)")
}
if ("amostragem/registro/ponto_metro" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/ponto_metro", "ponto_metro (amostragem/registro)")
}
if ("amostragem/registro/tipo_forma_vida" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/tipo_forma_vida", "**Encostam** na vareta: (amostragem/registro)")
}
if ("amostragem/registro/forma_serrapilheira" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_serrapilheira", "Materiais botânicos em decomposição no solo observados: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa", "Formas de vida de plantas <span style=\"\"color:red\"\">nativas:</span> (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_bromelioide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_bromelioide", "A erva bromelioide observada é: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_bromelioide_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_bromelioide_sp", "Espécie ou nome popular (Erva bromelioide) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_cactacea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_cactacea", "A cactácea observada é: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_cactacea_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_cactacea_sp", "Espécie ou nome popular (Cactácea) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_orquidea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_orquidea", "A orquídea observada é: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_orquidea_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_orquidea_sp", "Espécie ou nome popular (Orquídea) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_outra", "Outra forma de vida de planta nativa: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_nativa_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_nativa_outra", "Foto de outra forma de vida de planta nativa: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_nativa_desconhecida" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_nativa_desconhecida", "Foto da forma de vida desconhecida de planta nativa: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_graminoide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_graminoide", "Espécie ou nome popular (Erva graminoide) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_erva_nao_graminoide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_erva_nao_graminoide", "Espécie ou nome popular (Erva não graminoide) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_arbusto_abaixo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_arbusto_abaixo", "Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_arbusto_acima" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_arbusto_acima", "Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_arvore_abaixo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_arvore_abaixo", "Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_arvore_acima" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_arvore_acima", "Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_bambu" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_bambu", "Espécie ou nome popular (Bambu) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_lianas" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_lianas", "Espécie ou nome popular (Lianas) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_ervas_de_passarinho" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_ervas_de_passarinho", "Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_palmeira" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_palmeira", "Espécie ou nome popular (Palmeira) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_samambaia" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_samambaia", "Espécie ou nome popular (Samambaia) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_nativa_canela_de_ema" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_nativa_canela_de_ema", "Espécie ou nome popular (Velósia) (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_exotica" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_exotica", "Formas de vida de plantas <span style=\"\"color:red\"\">exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_graminoide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_graminoide", "**Espécies** de <span style=\"\"color:red\"\"> graminóides exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_erva_nao_graminoide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_erva_nao_graminoide", "**Espécies** de <span style=\"\"color:red\"\"> ervas não graminóides exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_arbusto_abaixo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_arbusto_abaixo", "**Espécies** de <span style=\"\"color:red\"\"> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_arbusto_acima" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_arbusto_acima", "**Espécies** de <span style=\"\"color:red\"\"> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_arvore_abaixo" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_arvore_abaixo", "**Espécies** de <span style=\"\"color:red\"\"> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_arvore_acima" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_arvore_acima", "**Espécies** de <span style=\"\"color:red\"\"> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_bambu" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_bambu", "**Espécies** de <span style=\"\"color:red\"\"> bambus exóticos:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_cactacea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_cactacea", "**Espécies** de <span style=\"\"color:red\"\"> cactáceas exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_lianas" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_lianas", "**Espécies** de <span style=\"\"color:red\"\"> lianas exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_orquidea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_orquidea", "**Espécies** de <span style=\"\"color:red\"\"> orquídeas exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_palmeira" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_palmeira", "**Espécies** de <span style=\"\"color:red\"\"> palmeiras exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_samambaia" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_samambaia", "**Espécies** de <span style=\"\"color:red\"\"> samambaias exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/especies_exotica_outros" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/especies_exotica_outros", "**Espécies** de <span style=\"\"color:red\"\"> outros exóticas:</span> (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_exotica_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_exotica_outra", "Outra forma de vida de planta exótica: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_exotica_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_exotica_outra", "Foto de outra forma de vida de planta exótica: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_exotica_desconhecida" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_exotica_desconhecida", "Foto da forma de vida desconhecida de planta exótica: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_exotica_bromelioide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_exotica_bromelioide", "A erva bromelioide observada é: (amostragem/registro).1")
}
if ("amostragem/registro/forma_vida_exotica_cactacea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_exotica_cactacea", "A cactácea observada é: (amostragem/registro).1")
}
if ("amostragem/registro/forma_vida_exotica_orquidea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_exotica_orquidea", "A orquídea observada é: (amostragem/registro).1")
}
if ("amostragem/registro/exotica_graminoide_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_graminoide_outra_sp", "Outra espécie de erva graminoide exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_erva_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_erva_outra_sp", "Outra espécie de erva não graminoide exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_arbusto_abaixo_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_arbusto_abaixo_outra_sp", "Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)")
}
if ("amostragem/registro/exotica_arbusto_acima_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_arbusto_acima_outra_sp", "Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)")
}
if ("amostragem/registro/exotica_arvore_abaixo_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_arvore_abaixo_outra_sp", "Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)")
}
if ("amostragem/registro/exotica_arvore_acima_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_arvore_acima_outra_sp", "Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)")
}
if ("amostragem/registro/exotica_bambu_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_bambu_outra_sp", "Outra espécie de bambu exótico: (amostragem/registro)")
}
if ("amostragem/registro/exotica_cactacea_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_cactacea_outra_sp", "Outra espécie de cactácea exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_orquidea_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_orquidea_outra_sp", "Outra espécie de orquídea exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_palmeira_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_palmeira_outra_sp", "Outra espécie de palmeira exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_samambaia_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_samambaia_outra_sp", "Outra espécie de samambaia exótica: (amostragem/registro)")
}
if ("amostragem/registro/exotica_outros_outra_sp" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/exotica_outros_outra_sp", "Outra espécie exótica: (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_seca_morta" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_seca_morta", "Formas de vida de plantas <span style=\"\"color:red\"\">secas ou mortas:</span> (amostragem/registro)")
}
if ("amostragem/registro/forma_vida_seca_morta_bromelioide" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_seca_morta_bromelioide", "A erva bromelioide observada é: (amostragem/registro).2")
}
if ("amostragem/registro/forma_vida_seca_morta_cactacea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_seca_morta_cactacea", "A cactácea observada é: (amostragem/registro).2")
}
if ("amostragem/registro/forma_vida_seca_morta_orquidea" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_seca_morta_orquidea", "A orquídea observada é: (amostragem/registro).2")
}
if ("amostragem/registro/forma_vida_seca_morta_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/forma_vida_seca_morta_outra", "Outra forma de vida de planta seca e/ou morta: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_seca_morta_outra" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_seca_morta_outra", "Foto de outra forma de vida de planta seca ou morta: (amostragem/registro)")
}
if ("amostragem/registro/foto_forma_vida_seca_morta_desconhecida" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/foto_forma_vida_seca_morta_desconhecida", "Foto da forma de vida desconhecida de planta seca ou morta: (amostragem/registro)")
}
if ("amostragem/registro/observacao" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/observacao", "Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)")
}
if ("amostragem/registro/uuid" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/registro/uuid", "uuid (amostragem/registro)")
}
if ("amostragem/ponto_fim_transecto" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/ponto_fim_transecto", "Coordenada final da amostragem (amostragem)")
}
if ("amostragem/foto_ponto_final" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "amostragem/foto_ponto_final", "Foto do ponto final do transecto (amostragem)")
}
if ("uuid" %in% colnames(registros_corrig)) {
  monitora_renomear_ou_consolidar_coluna(registros_corrig, "uuid", "uuid")
}

### Consolidação de nomes de colunas duplicados e aliases gerados por Excel, R, readxl ou fread:
### - remove sufixos artificiais ...N;
### - consolida valores iguais ou complementares;
### - marca conflitos reais com __CONFLITO__ e registra auditoria.

monitora_controlar_recursos("pre_merge_duplicate_columns_pos_renomeacao", risco = "muito_alto", objeto = registros_corrig, force_log = TRUE)
monitora_perf_checkpoint("pre_merge_duplicate_columns_pos_renomeacao", "antes do merge econômico de colunas duplicadas/sufixadas pós-renomeação", registros_corrig)
registros_corrig <- monitora_merge_duplicate_columns(registros_corrig)
monitora_controlar_recursos("pos_merge_duplicate_columns_pos_renomeacao", risco = "normal", objeto = registros_corrig, force_log = TRUE)
monitora_gc("pos_merge_duplicate_columns_pos_renomeacao")
monitora_perf_checkpoint("normalizacao_colunas_pos_renomeacao", "merge de colunas duplicadas/sufixadas após renomeações", registros_corrig)
col_audit_corrig <- attr(registros_corrig, "audit_duplicate_columns")
if (!is.null(col_audit_corrig) && nrow(col_audit_corrig) > 0) {
  fwrite(col_audit_corrig, file.path(MONITORA_LOG_DIR, paste0("auditoria_colunas_duplicadas_pos_renomeacao_", MONITORA_EXEC_ID, ".csv")))
  monitora_log("colunas_duplicadas", "INFO", NA_character_, paste0(nrow(col_audit_corrig), " nomes de colunas duplicados/sufixados tratados apos renomeacao"), "ver auditoria_colunas_duplicadas_pos_renomeacao")
}
registros_corrig <- monitora_consolidate_aliases(registros_corrig)
monitora_perf_checkpoint("consolidacao_aliases_pos_renomeacao", "coalesce de aliases críticos após renomeações", registros_corrig)

# Anota o tipo de entrada antes da auditoria pré-deduplicação.
# Isso permite auditar a equivalência individual x lote antes de a deduplicação aplicar a
# preferência de fonte.
registros_corrig <- monitora_anotar_tipo_entrada(registros_corrig, MONITORA_ARQUIVOS_ENTRADA)

# Quando mais de um tipo de fonte é fornecido na mesma execução, audita a equivalência
# antes da deduplicação semântica, evitando que a preferência de fonte esconda divergências.
MONITORA_AUDITORIA_COMPAT_PRE <- monitora_auditar_compatibilidade_fontes_v12(registros_corrig, fase = "pre_dedup")
monitora_perf_checkpoint("auditoria_compatibilidade_fontes_v12_pre_dedup", "comparação entre tipos de entrada antes da deduplicação", registros_corrig)

### Auditoria de compatibilidade separada por fase, sempre exportada.

### Deduplicação semântica entre entradas brutas e entradas pós-tratamento aceitas.
### Mantém produtos pós-tratamento quando houver sobreposição com brutos, pois eles podem conter
### validações/correções manuais.
registros_corrig <- monitora_deduplicar_registros_amostrais(registros_corrig, MONITORA_ARQUIVOS_ENTRADA)
registros_corrig <- monitora_drop_legacy_technical_columns(registros_corrig, "pos_deduplicacao")
monitora_controlar_recursos("deduplicacao_semantica", risco = "alto", objeto = registros_corrig, force_log = TRUE)
monitora_perf_checkpoint("deduplicacao_semantica", "deduplicação por UUID ou UC+CICLO+UA+ponto, com preferência v12 por pós-tratamento/lote-dev/individual", registros_corrig)
MONITORA_AUDITORIA_COMPAT_POST <- monitora_auditar_compatibilidade_fontes_v12(registros_corrig, fase = "pos_dedup")
monitora_perf_checkpoint("auditoria_compatibilidade_fontes_v12_pos_dedup", "comparação entre tipos de entrada quando há fontes sobrepostas", registros_corrig)

# Auditoria complementar: identifica duplicidade de ponto dentro de UC+CICLO+UA+ANO mesmo
# quando UUIDs são diferentes. Não remove automaticamente porque pode indicar erro de numeração
# ou edição manual que exige validação de campanha.
monitora_auditar_pontos_duplicados <- function(dt) {
  dt <- monitora_as_dt_ref(dt)
  ponto_col <- if ("ponto_amostral (amostragem/registro)" %in% names(dt)) {
    "ponto_amostral (amostragem/registro)"
  } else if ("ponto_metro (amostragem/registro)" %in% names(dt)) {
    "ponto_metro (amostragem/registro)"
  } else {
    NA_character_
  }
  required <- c("UC", "CICLO", "UA", "ANO")
  if (is.na(ponto_col) || !all(required %in% names(dt))) return(invisible(NULL))
  aud <- dt[, .N, by = c(required, ponto_col)][N > 1]
  if (nrow(aud) > 0) {
    setnames(aud, ponto_col, "ponto")
    fwrite(aud, file.path(MONITORA_LOG_DIR, paste0("auditoria_pontos_duplicados_", MONITORA_EXEC_ID, ".csv")))
    monitora_log("pontos_duplicados", "AVISO", NA_character_, paste0(nrow(aud), " pontos duplicados dentro de UC+CICLO+UA+ANO"), "ver auditoria_pontos_duplicados; nao removido automaticamente")
  }
  invisible(aud)
}
monitora_auditar_pontos_duplicados(registros_corrig)

monitora_auditar_anos_invalidos <- function(dt) {
  dt <- monitora_as_dt_ref(dt)
  if (!"ANO" %in% names(dt)) return(invisible(data.table()))
  ano_num <- suppressWarnings(as.integer(dt[["ANO"]]))
  ano_min <- 2000L
  ano_max <- lubridate::year(Sys.Date()) + 1L
  invalido <- is.na(ano_num) | ano_num < ano_min | ano_num > ano_max
  if (!any(invalido, na.rm = TRUE)) return(invisible(data.table()))
  cols <- intersect(
    c("UC", "CICLO", "CAMPANHA", "UA", "COLETA", "Data (data_hora)", "DATA_MONITORA_PARSEADA", "ANO", "MONITORA_ARQUIVO_ENTRADA", "MONITORA_TIPO_ENTRADA"),
    names(dt)
  )
  aud <- unique(dt[invalido, ..cols])
  aud[, ano_min_plausivel := ano_min]
  aud[, ano_max_plausivel := ano_max]
  aud[, acao_recomendada := "Corrigir a data original ou verificar erro de parsing; registros com ANO inválido/NA não devem ser usados para gráficos ou estatísticas temporais sem validação."]
  saida <- file.path(MONITORA_LOG_DIR, paste0("auditoria_anos_invalidos_", MONITORA_EXEC_ID, ".csv"))
  fwrite(aud, saida)
  fwrite(aud, file.path(MONITORA_OUTPUT_DIR, "auditoria_anos_invalidos_ultima_execucao.csv"))
  monitora_log("anos_invalidos", "AVISO", saida, paste0(nrow(aud), " combinações com ANO inválido/NA ou fora da faixa plausível"), "ver auditoria_anos_invalidos")
  invisible(aud)
}

monitora_auditar_completude_101_pontos <- function(dt) {
  dt <- monitora_as_dt_ref(dt)
  ponto_col <- if ("ponto_amostral (amostragem/registro)" %in% names(dt)) {
    "ponto_amostral (amostragem/registro)"
  } else if ("ponto_metro (amostragem/registro)" %in% names(dt)) {
    "ponto_metro (amostragem/registro)"
  } else {
    NA_character_
  }
  required <- c("UC", "CICLO", "CAMPANHA", "UA", "ANO")
  if (is.na(ponto_col) || !all(required %in% names(dt))) return(invisible(data.table()))
  fonte_col <- if ("MONITORA_ARQUIVO_ENTRADA" %in% names(dt)) "MONITORA_ARQUIVO_ENTRADA" else NA_character_
  aux <- dt[, c(required, ponto_col, if (!is.na(fonte_col)) fonte_col else NULL), with = FALSE]
  setnames(aux, ponto_col, "ponto_original")
  aux[, ponto_num := suppressWarnings(as.integer(gsub("[^0-9]+", "", as.character(ponto_original))))]
  by_cols <- required
  resumo <- aux[, .(
    n_registros = .N,
    n_pontos = data.table::uniqueN(ponto_num[!is.na(ponto_num)]),
    pontos_fora_1_101 = paste(sort(unique(ponto_num[!is.na(ponto_num) & !ponto_num %between% c(1L, 101L)])), collapse = " | "),
    arquivos_origem = if (!is.na(fonte_col)) paste(sort(unique(na.omit(as.character(get(fonte_col))))), collapse = " | ") else NA_character_
  ), by = by_cols]
  dup <- aux[!is.na(ponto_num), .N, by = c(by_cols, "ponto_num")][N > 1L]
  dup_sum <- dup[, .(pontos_duplicados = paste(sort(unique(ponto_num)), collapse = " | ")), by = by_cols]
  resumo <- merge(resumo, dup_sum, by = by_cols, all.x = TRUE, sort = FALSE)
  resumo[is.na(pontos_duplicados), pontos_duplicados := ""]
  # Lista de pontos ausentes calculada apenas por grupo problemático para manter a etapa leve.
  resumo[, pontos_ausentes := ""]
  prob_idx <- which(resumo$n_pontos != 101L | resumo$n_registros != 101L | nzchar(resumo$pontos_duplicados) | nzchar(resumo$pontos_fora_1_101))
  if (length(prob_idx) > 0) {
    chaves_prob <- resumo[prob_idx, ..by_cols]
    aux_prob <- merge(aux, chaves_prob, by = by_cols, all.y = TRUE, sort = FALSE)
    miss <- aux_prob[, .(
      pontos_ausentes = paste(setdiff(1:101, sort(unique(ponto_num[!is.na(ponto_num) & ponto_num %between% c(1L, 101L)]))), collapse = " | ")
    ), by = by_cols]
    resumo <- merge(resumo, miss, by = by_cols, all.x = TRUE, suffixes = c("", "_calc"), sort = FALSE)
    resumo[!is.na(pontos_ausentes_calc), pontos_ausentes := pontos_ausentes_calc]
    resumo[, pontos_ausentes_calc := NULL]
  }
  aud <- resumo[n_pontos != 101L | n_registros != 101L | nzchar(pontos_duplicados) | nzchar(pontos_ausentes) | nzchar(pontos_fora_1_101)]
  if (nrow(aud) > 0) {
    aud[, acao_recomendada := "Conferir se a UA/ano possui exatamente 101 pontos; corrigir pontos ausentes, duplicados ou fora de 1:101 antes de aceitar análises e gráficos."]
    saida <- file.path(MONITORA_LOG_DIR, paste0("auditoria_completude_101_pontos_", MONITORA_EXEC_ID, ".csv"))
    fwrite(aud, saida)
    fwrite(aud, file.path(MONITORA_OUTPUT_DIR, "auditoria_completude_101_pontos_ultima_execucao.csv"))
    monitora_log("completude_101_pontos", "AVISO", saida, paste0(nrow(aud), " grupos UC+CICLO+CAMPANHA+UA+ANO com n_registros/n_pontos diferente de 101, duplicidade ou lacuna"), "ver auditoria_completude_101_pontos")
  }
  invisible(aud)
}


### Padronização de valores de colunas para compatibilidade entre formatos de exportação.

if ("campestre" %in% registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`) {
  registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`[registros_corrig$`Qual a formação vegetacional onde está situado o transecto?` ==
                                                                                   "campestre"] <- "Campestre"
}
if ("savanica" %in% registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`) {
  registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`[registros_corrig$`Qual a formação vegetacional onde está situado o transecto?` ==
                                                                                   "savanica"] <- "Savânica"
}

monitora_perf_checkpoint("renomeacao_e_deduplicacao_inicial", "bloco de renomeação de colunas, valores e deduplicação inicial", registros_corrig)

### Correção de valores exportados como rótulo para os nomes padronizados usados na análise.

## Padronização de categorias gerais.

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Plantas nativas" = "nativa",
      "Plantas nativas," = "nativa",
      "Plantas exóticas" = "exotica",
      "Plantas exóticas," = "exotica",
      "Plantas secas ou mortas" = "seca_morta",
      "Plantas secas ou mortas," = "seca_morta",
      "Solo nu / rochas \\(sem plantas tocando a vareta\\)" = "solo_nu",
      "Solo nu / rochas \\(sem plantas tocando a vareta\\)," = "solo_nu",
      "Material botânico em decomposição no solo" = "serrapilheira",
      "Material botânico em decomposição no solo," = "serrapilheira",
      "Outras plantas terrestres, líquens e/ou fungos" = "solo_nu",
      "Outras plantas terrestres, líquens e/ou fungos," = "solo_nu"
    )
  )

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "nativa," = "nativa",
      "exotica," = "exotica",
      "seca_morta," = "seca_morta",
      "serrapilheira," = "serrapilheira",
      "solo_nu," = "solo_nu"
    )
  )

## Materiais botânicos em decomposição no solo.
## Este campo detalha a categoria geral "serrapilheira" de tipo_forma_vida.
## Mantém a categoria geral em **Encostam** na vareta e padroniza os tipos
## específicos selecionados no XLSForm mais recente.
col_material_botanico <- "Materiais botânicos em decomposição no solo observados: (amostragem/registro)"
if (col_material_botanico %in% colnames(registros_corrig)) {
  registros_corrig[[col_material_botanico]] <- registros_corrig[[col_material_botanico]] %>%
    str_replace_all(
      c(
        "Serrapilheira \\(matéria orgânica em decomposição no solo composta por folhas, cascas, etc\\)" = "serrapilheira",
        "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
        "Troncos, galhos, ramos ou outras partes lenhosas não fragmentadas" = "fragmentos_botanicos",
        "Matéria orgânica inundável \\(turfa, batume ou matéria orgânica subaquática\\)" = "material_inundado",
        "serrapilheira," = "serrapilheira",
        "fragmentos_botanicos," = "fragmentos_botanicos",
        "material_inundado," = "material_inundado"
      )
    ) %>%
    str_squish()
}

registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Padronização de valores exportados como rótulo.
      
      ## Correção de rótulo.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Correção de rótulo seguido de espaço.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ## Correção de rótulo seguido de vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ## Correção de rótulo seguido de espaço e vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ## Correção de nome padronizado.
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ## Correção de nome padronizado seguido de vírgula.
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ## Correção de nome padronizado seguido de espaço e vírgula.
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
    )
  )

registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Padronização de valores exportados como rótulo.
      
      ## Correção de rótulo.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Correção de rótulo seguido de espaço.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ## Correção de rótulo seguido de vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ## Correção de rótulo seguido de espaço e vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ## Correção de nome padronizado.
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ## Correção de nome padronizado seguido de vírgula.
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ## Correção de nome padronizado seguido de espaço e vírgula.
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
    )
  )

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Padronização de valores exportados como rótulo.
      
      ## Correção de rótulo.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ## Correção de rótulo seguido de espaço.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ## Correção de rótulo seguido de vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ## Correção de rótulo seguido de espaço e vírgula.
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ## Correção de nome padronizado.
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ## Correção de nome padronizado seguido de vírgula.
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ## Correção de nome padronizado seguido de espaço e vírgula.
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
    )
  )


### extração do último token em colunas específicas (o SISMONITORA exporta a lista concatenada por
### "|")

## ponto amostral

registros_corrig$`ponto_amostral (amostragem/registro)` <-
  word(registros_corrig$`ponto_amostral (amostragem/registro)`,
       sep = fixed("|"),
       -1)

## metro

registros_corrig$`ponto_metro (amostragem/registro)` <-
  word(registros_corrig$`ponto_metro (amostragem/registro)`,
       sep = fixed("|"),
       -1)

## encostam na vareta

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  word(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
       sep = fixed("|"),
       -1)

### descrição do ponto amostral (corrigido)

if (is.null(registros_corrig[['Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)']]))
  set(registros_corrig, j = 'Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)', value = NA_character_)

# Número de tokens e último token identificado.
obs_pa_tokens <- registros_corrig[, .(
  Tokens = lapply(strsplit(`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`, "\\|"), length),
  Last_token = lapply(strsplit(`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`, "\\|"), function(x) if(length(x) > 0) tail(x, 1) else NA)
), by = .(COLETA, `ponto_amostral (amostragem/registro)`)]

# Número máximo de tokens observado.
obs_pa_tokens[, MaxTokens_coleta := max(unlist(Tokens)), by = .(COLETA)]

# Token válido.
obs_pa_tokens[, `Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` := ifelse(Tokens == MaxTokens_coleta, Last_token, NA_character_)]

# Atualização dos tokens válidos em registros_corrig.
registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` <- obs_pa_tokens$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`

rm(obs_pa_tokens)

# Remoção de quebras de linha em campos descritivos, coletores e plaqueta.

registros_corrig$`Número da plaqueta (amostragem)` <-
  str_replace_all(
    registros_corrig$`Número da plaqueta (amostragem)`,
    "[[:punct:]]",
    " "
  )

registros_corrig$COLETORES <-
  str_replace_all(
    registros_corrig$COLETORES,
    "[\r\n]",
    " "
  )

registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` <-
  str_replace_all(
    registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`,
    "[\r\n]",
    " "
  )

registros_corrig$`Descreva observações gerais do transecto, caso necessário:` <-
  str_replace_all(
    registros_corrig$`Descreva observações gerais do transecto, caso necessário:`,
    "[\r\n]",
    " "
  )

registros_corrig$`Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)` <-
  str_replace_all(
    registros_corrig$`Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)`,
    "[\r\n]",
    " "
  )

## forma de vida de planta nativa

registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "nativa",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )


## erva bromelioide nativa

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      )
    ),default = NA_character_
  )

## cactacea nativa

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro)' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro)',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea nativa

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro)` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro)`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
    ),
    NA_character_
  )

## forma de vida de planta exótica

registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "exot",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## erva bromelioide exótica

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro).1`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      )
    )
  )

## cactacea exótica

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro)' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro).1',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea exótica

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro).1` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro).1`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
    ),
    NA_character_
  )

## forma de vida de planta seca ou morta

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "seca_morta",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## erva bromelioide seca ou morta

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro).2`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      )
    )
  )

## cactacea seca ou morta

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro).2' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro).1',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea seca ou morta

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro).2` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro).2`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
    ),
    NA_character_
  )

### `Outra forma de vida de planta nativa: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta nativa: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta nativa: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta nativa: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta nativa: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### `Outra forma de vida de planta exótica: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### `Outra forma de vida de planta seca e/ou morta: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta seca e/ou morta: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta seca e/ou morta: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta seca e/ou morta: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta seca e/ou morta: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### Protocolo avançado: extração do último token dos atributos de espécies

## Espécies nativas

# Nativa Espécie ou nome popular (Erva graminoide)

if (is.null(registros_corrig[['Espécie ou nome popular (Graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Graminoide) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva graminoide) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Erva não graminoide)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva não graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva não graminoide) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva não graminoide) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "erva_nao_graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Erva não graminoide) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30))

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)` <- 
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do
# solo (D30))

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Bambu)

if (is.null(registros_corrig[['Espécie ou nome popular (Bambu) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Bambu) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Bambu) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "bambu",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Bambu) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Lianas)

if (is.null(registros_corrig[['Espécie ou nome popular (Lianas) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Lianas) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Lianas) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "lianas",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Lianas) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Erva-de-passarinho)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "ervas_de_passarinho",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Palmeira)

if (is.null(registros_corrig[['Espécie ou nome popular (Palmeira) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Palmeira) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Palmeira) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "palmeira",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Palmeira) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Samambaia)

if (is.null(registros_corrig[['Espécie ou nome popular (Samambaia) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Samambaia) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Samambaia) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "samambaia",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Samambaia) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Velósia)

if (is.null(registros_corrig[['Espécie ou nome popular (Velósia) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Velósia) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Velósia) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "canela_de_ema",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Velósia) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

## Espécies exóticas.

# espécies_exotica_graminoide

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_erva_nao_graminoide

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "erva_nao_graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_arbusto_abaixo

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# espécies_exotica_arbusto_acima

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# espécies_exotica_arvore_abaixo

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# espécies_exotica_arvore_acima

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# espécies_exotica_bambu

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "bambu",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_cactacea

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_lianas

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "lianas",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_orquidea

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "orquidea",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_palmeira

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "palmeira",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# espécies_exotica_samambaia

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "samambaia",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

## espécies_exotica_outros

# exotica_graminoide_outra_sp

if (is.null(registros_corrig[['Outra espécie de erva graminoide exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de erva graminoide exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de erva graminoide exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)`,
      "exotica_graminoide_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de erva graminoide exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_erva_nao_graminoide_outra_sp

if (is.null(registros_corrig[['Outra espécie de erva não graminoide exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de erva não graminoide exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de erva não graminoide exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)`,
      "exotica_erva_nao_graminoide_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de erva não graminoide exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arbusto_abaixo_outra_sp

if (is.null(registros_corrig[['Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
      "exotica_arbusto_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arbusto_acima_outra_sp

if (is.null(registros_corrig[['Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
      "exotica_arbusto_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arvore_abaixo_outra_sp

if (is.null(registros_corrig[['Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
      "exotica_arvore_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arvore_acima_outra_sp

if (is.null(registros_corrig[['Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
      "exotica_arvore_acima_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_bambu_outra_sp

if (is.null(registros_corrig[['Outra espécie de bambu exótico: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de bambu exótico: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de bambu exótico: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)`,
      "exotica_bambu_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de bambu exótico: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_cactacea_outra_sp

if (is.null(registros_corrig[['Outra espécie de cactácea exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de cactácea exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de cactácea exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)`,
      "exotica_cactacea_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de cactácea exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_orquidea_outra_sp

if (is.null(registros_corrig[['Outra espécie de orquídea exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de orquídea exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de orquídea exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)`,
      "exotica_orquidea_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de orquídea exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )


# exotica_palmeira_outra_sp

if (is.null(registros_corrig[['Outra espécie de palmeira exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de palmeira exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de palmeira exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)`,
      "exotica_palmeira_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de palmeira exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_samambaia_outra_sp

if (is.null(registros_corrig[['Outra espécie de samambaia exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de samambaia exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de samambaia exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)`,
      "exotica_samambaia_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de samambaia exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )


### extração do ANO a partir da DATA. O SISMONITORA exportou datas em diferentes formatos. A função
### a seguir faz a coerção dos formatos identificados

if ("Data (data_hora)" %in% names(registros_corrig)) {
  registros_corrig[, DATA_MONITORA_PARSEADA := monitora_parse_date(`Data (data_hora)`)]
  registros_corrig[, ANO := as.character(lubridate::year(DATA_MONITORA_PARSEADA))]
} else {
  registros_corrig[, ANO := NA_character_]
  monitora_log("datas", "ERRO", NA_character_, "Coluna Data (data_hora) ausente", "ANO criado como NA")
}
# Deduplicação final: após ANO estar calculado e antes das auditorias/estatísticas,
# remove sobreposições residuais por UC+CICLO+CAMPANHA+UA+ANO+ponto.
registros_corrig <- monitora_deduplicar_final_por_ponto_ano(registros_corrig)
registros_corrig <- monitora_drop_legacy_technical_columns(registros_corrig, "pos_deduplicacao_final_ponto_ano")
monitora_controlar_recursos("deduplicacao_final_ponto_ano", risco = "alto", objeto = registros_corrig, force_log = TRUE)
monitora_perf_checkpoint("deduplicacao_final_ponto_ano", "trava final por UC+CICLO+CAMPANHA+UA+ANO+ponto antes das estatísticas", registros_corrig)

MONITORA_AUDITORIA_ANOS_INVALIDOS <- monitora_auditar_anos_invalidos(registros_corrig)
MONITORA_AUDITORIA_COMPLETUDE_101_PONTOS <- monitora_auditar_completude_101_pontos(registros_corrig)
monitora_perf_checkpoint("auditoria_datas_pontos", "auditoria de ANO plausível e completude de 101 pontos", registros_corrig)

### Tratamento robusto das coordenadas manipuladas em SISMONITORA/Excel/R.
### A função preserva latitude/longitude quando há apenas 2 tokens e aceita 4 tokens quando
### altitude/acurácia existem.
### Também detecta separador por registro e registra auditoria, evitando substituir vírgulas
### decimais cegamente.
coord_cols <- c(
  "Coordenada inicial da amostragem (amostragem)",
  "Coordenada final da amostragem (amostragem)"
)
coord_audit_all_list <- if (isTRUE(MONITORA_AUDITORIA_COORDENADAS_COMPLETA)) list() else NULL
coord_audit_all_i <- 0L
coord_audit_summary_list <- list()
coord_audit_summary_i <- 0L
for (cc in coord_cols) {
  if (cc %in% names(registros_corrig)) {
    normalizada <- monitora_normalize_coord(registros_corrig[[cc]])
    ca <- attr(normalizada, "coord_audit")
    ca[, coluna := cc]
    if (isTRUE(MONITORA_AUDITORIA_COORDENADAS_COMPLETA)) {
      ca[, arquivo_origem := as.character(registros_corrig$.id)]
      coord_audit_all_i <- coord_audit_all_i + 1L
      coord_audit_all_list[[coord_audit_all_i]] <- ca
    }
    coord_audit_summary_i <- coord_audit_summary_i + 1L
    coord_audit_summary_list[[coord_audit_summary_i]] <- ca[, .N, by = .(coluna, separador_detectado, n_tokens)]
    registros_corrig[[cc]] <- normalizada
    rm(ca, normalizada); monitora_gc(paste0("coordenadas_", cc))
  } else {
    monitora_log("coordenadas", "AVISO", NA_character_, paste0("Coluna ausente: ", cc), "mantida ausente")
  }
}
coord_audit_all <- if (isTRUE(MONITORA_AUDITORIA_COORDENADAS_COMPLETA) && coord_audit_all_i > 0L) {
  data.table::rbindlist(coord_audit_all_list[seq_len(coord_audit_all_i)], fill = TRUE, use.names = TRUE)
} else NULL
coord_audit_summary <- if (coord_audit_summary_i > 0L) {
  data.table::rbindlist(coord_audit_summary_list[seq_len(coord_audit_summary_i)], fill = TRUE, use.names = TRUE)
} else data.table::data.table()
rm(coord_audit_all_list, coord_audit_summary_list, coord_audit_all_i, coord_audit_summary_i)
if (nrow(coord_audit_summary) > 0) {
  fwrite(coord_audit_summary, file.path(MONITORA_LOG_DIR, paste0("auditoria_coordenadas_resumo_", MONITORA_EXEC_ID, ".csv")))
  monitora_log("coordenadas", "INFO", NA_character_, paste0(nrow(coord_audit_summary), " linhas de resumo de auditoria de coordenadas geradas"), "ver auditoria_coordenadas_resumo")
}
if (isTRUE(MONITORA_AUDITORIA_COORDENADAS_COMPLETA) && !is.null(coord_audit_all) && nrow(coord_audit_all) > 0) {
  fwrite(coord_audit_all, file.path(MONITORA_LOG_DIR, paste0("auditoria_coordenadas_", MONITORA_EXEC_ID, ".csv")))
  monitora_log("coordenadas", "INFO", NA_character_, paste0(nrow(coord_audit_all), " registros de auditoria completa de coordenadas gerados"), "ver auditoria_coordenadas; para lotes grandes, manter MONITORA_AUDITORIA_COORDENADAS_COMPLETA=false")
}
rm(coord_cols, coord_audit_all, coord_audit_summary); monitora_gc("coordenadas_fim")

### Construção das tabelas estatísticas

## Preparação de registros_corrig_stat com data.table e tabelas estreitas.
## Evita expandir todas as colunas de registros_corrig ao separar tokens,
## reduzindo o pico de memória em lotes grandes.

monitora_grupo_stat_cols <- c(
  ".id",
  "PROTOCOLO",
  "UC",
  "UA",
  "ANO",
  "Coordenada inicial da amostragem (amostragem)",
  "Coordenada final da amostragem (amostragem)"
)

monitora_sum_tokens_by_group <- function(dt, token_col, prefix = NULL) {
  dt <- monitora_as_dt_ref(dt)
  grupo_cols <- monitora_grupo_stat_cols[monitora_grupo_stat_cols %in% names(dt)]
  if (length(grupo_cols) == 0) return(data.table())
  base <- unique(dt[, ..grupo_cols])
  if (!(token_col %in% names(dt))) {
    monitora_log("preparacao_registros_corrig_stat", "AVISO", NA_character_, paste0("Coluna ausente para sumarização: ", token_col), "tabela vazia mantida")
    return(base)
  }
  ## Usa base R para reduzir dependências e cópias temporárias nesta etapa.
  vals <- as.character(dt[[token_col]])
  vals[is.na(vals)] <- ""
  vals <- trimws(gsub("\\s+", " ", vals))
  tokens <- strsplit(vals, "\\s+")
  lens <- lengths(tokens)
  keep <- which(lens > 0L & nzchar(vals))
  if (length(keep) == 0) {
    rm(vals, tokens, lens, keep); monitora_gc(paste0("sum_tokens_vazio_", token_col))
    return(base)
  }
  token_vec <- unlist(tokens[keep], use.names = FALSE)
  row_idx <- rep(keep, lens[keep])
  tmp <- cbind(dt[row_idx, ..grupo_cols], data.table(token = token_vec))
  rm(vals, tokens, lens, keep, token_vec, row_idx); monitora_gc(paste0("sum_tokens_expandido_", token_col))
  tmp <- tmp[nzchar(token)]
  if (nrow(tmp) == 0) return(base)
  cnt <- tmp[, .N, by = c(grupo_cols, "token")]
  rm(tmp); monitora_gc(paste0("sum_tokens_contado_", token_col))
  ## Nomes de colunas como "Coordenada inicial ..." não podem entrar
  ## crus em fórmula textual. Colocar todos os nomes de agrupamento entre crases.
  ## Isso evita erro em str2lang()/as.formula() sem depender de nomes sintáticos.
  grupo_cols_formula <- paste0("`", gsub("`", "", grupo_cols, fixed = TRUE), "`")
  formula_dcast <- stats::as.formula(paste(paste(grupo_cols_formula, collapse = " + "), "~ token"))
  wide <- data.table::dcast(cnt, formula_dcast, value.var = "N", fill = 0)
  if (!is.null(prefix) && nchar(prefix) > 0) {
    token_cols <- setdiff(names(wide), grupo_cols)
    data.table::setnames(wide, token_cols, paste(prefix, token_cols, sep = "_"))
  }
  wide[]
}

monitora_controlar_recursos("preparacao_stat_inicio", risco = "alto", objeto = registros_corrig, force_log = TRUE)
monitora_perf_checkpoint("preparacao_stat_inicio", "início da preparação estatística v17/data.table", registros_corrig)

## somatório de categorias por UC, UA, ANO
sum_categ_by_UC_UA_ANO <- monitora_sum_tokens_by_group(
  registros_corrig,
  "**Encostam** na vareta: (amostragem/registro)",
  prefix = NULL
)
monitora_perf_checkpoint("preparacao_stat_sum_categ", "sumário de categorias encostam na vareta", sum_categ_by_UC_UA_ANO)

## somatório de formas de vida nativas por UC, UA, ANO
sum_form_vida_nat_by_UC_UA_ANO <- monitora_sum_tokens_by_group(
  registros_corrig,
  "Formas de vida de plantas <span style=\"\"color:red\"\">nativas:</span> (amostragem/registro)",
  prefix = "nativa"
)
monitora_perf_checkpoint("preparacao_stat_sum_forma_nativa", "sumário de formas de vida nativas", sum_form_vida_nat_by_UC_UA_ANO)

## somatório de formas de vida exóticas por UC, UA, ANO
sum_form_vida_exot_by_UC_UA_ANO <- monitora_sum_tokens_by_group(
  registros_corrig,
  "Formas de vida de plantas <span style=\"\"color:red\"\">exóticas:</span> (amostragem/registro)",
  prefix = "exot"
)
monitora_perf_checkpoint("preparacao_stat_sum_forma_exotica", "sumário de formas de vida exóticas", sum_form_vida_exot_by_UC_UA_ANO)

## somatório de formas de vida secas ou mortas por UC, UA, ANO
sum_form_vida_seca_morta_by_UC_UA_ANO <- monitora_sum_tokens_by_group(
  registros_corrig,
  "Formas de vida de plantas <span style=\"\"color:red\"\">secas ou mortas:</span> (amostragem/registro)",
  prefix = "seca_morta"
)
monitora_perf_checkpoint("preparacao_stat_sum_forma_seca_morta", "sumário de formas de vida secas ou mortas", sum_form_vida_seca_morta_by_UC_UA_ANO)

## somatório dos tipos de materiais botânicos em decomposição no solo por UC, UA, ANO
sum_material_botanico_by_UC_UA_ANO <- monitora_sum_tokens_by_group(
  registros_corrig,
  "Materiais botânicos em decomposição no solo observados: (amostragem/registro)",
  prefix = "mat_bot"
)
monitora_perf_checkpoint("preparacao_stat_sum_material_botanico", "sumário de materiais botânicos em decomposição no solo", sum_material_botanico_by_UC_UA_ANO)

### formação vegetacional por UC, UA, ANO:
form_cols <- c(
  monitora_grupo_stat_cols,
  "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)",
  "Qual(is)? (impact_manejo_uso)",
  "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)",
  "Descreva observações gerais do transecto, caso necessário:",
  "Qual a formação vegetacional onde está situado o transecto?"
)
form_cols <- form_cols[form_cols %in% names(registros_corrig)]
form_veg <- unique(registros_corrig[, ..form_cols])
if ("Qual a formação vegetacional onde está situado o transecto?" %in% names(form_veg)) {
  data.table::setnames(form_veg, "Qual a formação vegetacional onde está situado o transecto?", "form_veg")
} else {
  form_veg[, form_veg := NA_character_]
}
monitora_perf_checkpoint("preparacao_stat_form_veg", "tabela de formação vegetacional", form_veg)

### Junção das tabelas estatísticas:
merge_keys_stat <- monitora_grupo_stat_cols[monitora_grupo_stat_cols %in% names(form_veg)]
registros_corrig_stat <- monitora_dt_full_join_list(
  list(
    form_veg,
    sum_categ_by_UC_UA_ANO,
    sum_form_vida_nat_by_UC_UA_ANO,
    sum_form_vida_exot_by_UC_UA_ANO,
    sum_form_vida_seca_morta_by_UC_UA_ANO,
    sum_material_botanico_by_UC_UA_ANO
  ),
  by = merge_keys_stat
)
monitora_gc("preparacao_stat_merge_sumarios")
### Criação de colunas "lat_ini", "long_ini", "alt_ini", "acc_ini" e
### "lat_fin", "long_fin", "alt_fin", "acc_fin"
### Divide coordenadas com data.table::tstrsplit()
### e usa atribuições por referência para evitar cópias desnecessárias.

registros_corrig_stat <- monitora_as_dt_ref(registros_corrig_stat)
monitora_split_coord_cols_dt(
  registros_corrig_stat,
  "Coordenada inicial da amostragem (amostragem)",
  c("lat_ini", "long_ini", "alt_ini", "acc_ini")
)
monitora_split_coord_cols_dt(
  registros_corrig_stat,
  "Coordenada final da amostragem (amostragem)",
  c("lat_fin", "long_fin", "alt_fin", "acc_fin")
)

### Remoção de colunas auxiliares redundantes e campos NA

drop_cols_stat <- intersect(names(registros_corrig_stat), c(
  "nativa_<NA>", "exot_<NA>", "seca_morta_<NA>", "mat_bot_<NA>",
  "exotica", "nativa", "seca_morta",
  "nativa_serrapilheira", "exot_serrapilheira", "seca_morta_serrapilheira"
))
if (length(drop_cols_stat) > 0) registros_corrig_stat[, (drop_cols_stat) := NULL]

### Criação de sum_nativa, sum_exotica, sum_seca_morta, material_botanico,
### serrapilheira, sum_herbacea e sum_lenhosa.
### A coluna histórica "serrapilheira" representa a presença geral de material
### botânico em decomposição no solo em tipo_forma_vida. Os tipos específicos
### são preservados separadamente nas colunas mat_bot_*.

nms_stat <- names(registros_corrig_stat)
nativa_cols <- nms_stat[grepl("nativa_", nms_stat) & !grepl("^nativa_serrapilheira", nms_stat)]
exot_cols <- nms_stat[grepl("exot_", nms_stat) & !grepl("^exot_serrapilheira", nms_stat)]
seca_morta_cols <- nms_stat[grepl("seca_morta_", nms_stat) & !grepl("^seca_morta_serrapilheira", nms_stat)]
mat_bot_cols <- nms_stat[grepl("^mat_bot_", nms_stat)]

data.table::set(registros_corrig_stat, j = "sum_nativa", value = monitora_sum_cols_dt(registros_corrig_stat, nativa_cols))
data.table::set(registros_corrig_stat, j = "sum_exotica", value = monitora_sum_cols_dt(registros_corrig_stat, exot_cols))
data.table::set(registros_corrig_stat, j = "sum_seca_morta", value = monitora_sum_cols_dt(registros_corrig_stat, seca_morta_cols))

if ("serrapilheira" %in% names(registros_corrig_stat)) {
  data.table::set(registros_corrig_stat, j = "material_botanico", value = registros_corrig_stat[["serrapilheira"]])
} else {
  data.table::set(registros_corrig_stat, j = "material_botanico", value = NA_integer_)
}
## Mantém o nome histórico por compatibilidade com produtos e análises anteriores.
data.table::set(registros_corrig_stat, j = "serrapilheira", value = registros_corrig_stat[["material_botanico"]])
data.table::set(registros_corrig_stat, j = "sum_material_botanico_tipo", value = monitora_sum_cols_dt(registros_corrig_stat, mat_bot_cols))

herb_sum_cols <- nms_stat[grepl("_graminoide|_bromelioide|_cactacea|_orquidea|_samambaia|_canela_de_ema", nms_stat)]
lenh_sum_cols <- nms_stat[grepl("_arbusto_abaixo|_arbusto_acima|_arvore_abaixo|_arvore_acima|_lianas|_palmeira|_passarinho|_bambu", nms_stat)]
data.table::set(registros_corrig_stat, j = "sum_herbacea", value = monitora_sum_cols_dt(registros_corrig_stat, herb_sum_cols))
data.table::set(registros_corrig_stat, j = "sum_lenhosa", value = monitora_sum_cols_dt(registros_corrig_stat, lenh_sum_cols))

# Ordem aproximada das colunas antes controlada por dplyr::relocate().
monitora_move_before(registros_corrig_stat, c("sum_nativa", "sum_exotica", "sum_seca_morta", "material_botanico", "serrapilheira", "sum_material_botanico_tipo", "sum_herbacea", "sum_lenhosa"), "form_veg")

### Criação presença herbácea e lenhosa

herb_pattern <- paste(
  c("graminoide",
    "erva_nao_graminoide",
    "erva_bromelioide",
    "cactacea",
    "orquidea",
    "samambaia",
    "canela_de_ema"),
  collapse = "|"
)

lenh_pattern <- paste(
  c("arbusto_abaixo",
    "arbusto_acima",
    "arvore_abaixo",
    "arvore_acima",
    "bambu",
    "lianas",
    "ervas_de_passarinho",
    "palmeira"),
  collapse = "|"
)

# Colunas de forma de vida das plantas.
vida_cols <- colnames(registros_corrig)[
  grepl("vida de plantas", colnames(registros_corrig), fixed = TRUE)
]

# Presença herbácea/lenhosa calculada por vetores lógicos, sem criar
# cópias completas de registros_corrig.
monitora_presence_any_pattern <- function(dt, cols, pattern) {
  if (length(cols) == 0) return(rep(FALSE, nrow(dt)))
  acc <- rep(FALSE, nrow(dt))
  for (cc in cols) {
    acc <- acc | grepl(pattern, as.character(dt[[cc]]), perl = TRUE)
  }
  acc[is.na(acc)] <- FALSE
  acc
}

herb_presence <- monitora_presence_any_pattern(registros_corrig, vida_cols, herb_pattern)
lenh_presence <- monitora_presence_any_pattern(registros_corrig, vida_cols, lenh_pattern)

herb_summary <- data.table(
  UC = registros_corrig$UC,
  UA = registros_corrig$UA,
  ANO = registros_corrig$ANO,
  herb_presence = herb_presence
)[, .(sum_presence_herb = sum(herb_presence, na.rm = TRUE)), by = .(UC, UA, ANO)]

lenh_summary <- data.table(
  UC = registros_corrig$UC,
  UA = registros_corrig$UA,
  ANO = registros_corrig$ANO,
  lenh_presence = lenh_presence
)[, .(sum_presence_lenh = sum(lenh_presence, na.rm = TRUE)), by = .(UC, UA, ANO)]

rm(herb_presence, lenh_presence); monitora_gc("presence_herb_lenh_v17")

# Junção com registros_corrig_stat usando data.table.
registros_corrig_stat <- monitora_as_dt_ref(registros_corrig_stat)
herb_summary <- monitora_as_dt_ref(herb_summary)
lenh_summary <- monitora_as_dt_ref(lenh_summary)
registros_corrig_stat <- monitora_dt_left_join(registros_corrig_stat, herb_summary, by = c("UC", "UA", "ANO"))
registros_corrig_stat <- monitora_dt_left_join(registros_corrig_stat, lenh_summary, by = c("UC", "UA", "ANO"))
monitora_move_before(registros_corrig_stat, c("sum_presence_herb", "sum_presence_lenh"), "sum_herbacea")

# Coluna que registra as categorias que encostam na vareta.
encostam_vareta_col <- colnames(registros_corrig)[
  grepl("Encostam.+vareta", colnames(registros_corrig))
][1]

# Presença de nativas, exóticas e seca/morta.
# Cálculo em tabela estreita e junção com data.table.
if (!is.na(encostam_vareta_col) && length(encostam_vareta_col) > 0) {
  enc_vec <- as.character(registros_corrig[[encostam_vareta_col]])
  encostam_vareta_summary <- data.table::data.table(
    UC = registros_corrig$UC,
    UA = registros_corrig$UA,
    ANO = registros_corrig$ANO,
    presence_nativa = grepl("\\bnativa\\b", enc_vec),
    presence_exotica = grepl("\\bexotica\\b", enc_vec),
    presence_seca_morta = grepl("\\bseca_morta\\b", enc_vec)
  )[, .(
    sum_presence_nativa = sum(presence_nativa, na.rm = TRUE),
    sum_presence_exotica = sum(presence_exotica, na.rm = TRUE),
    sum_presence_seca_morta = sum(presence_seca_morta, na.rm = TRUE)
  ), by = .(UC, UA, ANO)]
  rm(enc_vec); monitora_gc("presence_nat_exot_seca_v21")
} else {
  encostam_vareta_summary <- unique(monitora_as_dt_ref(registros_corrig)[, .(UC, UA, ANO)])
  encostam_vareta_summary[, `:=`(sum_presence_nativa = NA_integer_, sum_presence_exotica = NA_integer_, sum_presence_seca_morta = NA_integer_)]
  monitora_log("preparacao_registros_corrig_stat", "AVISO", NA_character_, "Coluna Encostam na vareta ausente para presença nativa/exótica/seca_morta", "sum_presence_* criado como NA")
}

registros_corrig_stat <- monitora_dt_left_join(registros_corrig_stat, encostam_vareta_summary, by = c("UC", "UA", "ANO"))
monitora_move_before(
  registros_corrig_stat,
  c("sum_presence_nativa", "sum_presence_exotica", "sum_presence_seca_morta", "material_botanico", "serrapilheira", "solo_nu"),
  "sum_herbacea"
)

### remoção de objetos não mais necessários:

rm(list = intersect(c(
   "sum_categ_by_UC_UA_ANO",
   "sum_form_vida_nat_by_UC_UA_ANO",
   "sum_form_vida_exot_by_UC_UA_ANO",
   "sum_form_vida_seca_morta_by_UC_UA_ANO",
   "sum_material_botanico_by_UC_UA_ANO",
   "form_veg",
   "encostam_vareta_summary",
   "herb_summary",
   "lenh_summary",
   "vida_cols",
   "herb_pattern",
   "lenh_pattern",
   "encostam_vareta_col",
   "monitora_grupo_stat_cols",
   "form_cols",
   "merge_keys_stat"
), ls()))

monitora_perf_checkpoint("preparacao_registros_corrig_stat", "criação e normalização das variáveis estatísticas", if (exists("registros_corrig_stat")) registros_corrig_stat else registros_corrig)


### Funções auxiliares seguras para blocos analíticos e gráficos.
### Evitam select()/matches()/str_detect() em condições if e tratam NA/NaN de forma segura.
monitora_cols_match <- function(dt, pattern) {
  if (is.null(dt) || !length(names(dt))) return(character())
  grep(pattern, names(dt), value = TRUE)
}

monitora_sum_cols_match <- function(dt, pattern) {
  cols <- monitora_cols_match(dt, pattern)
  if (!length(cols)) return(0)
  total <- 0
  for (cc in cols) {
    x <- suppressWarnings(as.numeric(dt[[cc]]))
    sx <- sum(x, na.rm = TRUE)
    if (is.finite(sx)) total <- total + sx
  }
  total
}

monitora_any_sum_cols_match <- function(dt, pattern) {
  isTRUE(monitora_sum_cols_match(dt, pattern) > 0)
}

monitora_safe_xmax <- function(x, mult = 1.15, fallback = 1) {
  x <- suppressWarnings(as.numeric(x))
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(m) || is.na(m) || m <= 0) return(fallback)
  m * mult
}

monitora_tem_linhas <- function(x) {
  !is.null(x) && is.data.frame(x) && nrow(x) > 0
}

monitora_plot_sem_dados <- function(titulo = "Gráfico não gerado", mensagem = "Sem dados para o filtro aplicado") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = mensagem, size = 5) +
    ggplot2::labs(title = titulo, x = NULL, y = NULL) +
    ggplot2::theme_void()
}

## Converte nomes técnicos usados nos objetos analíticos em rótulos curtos para exibição nos
## gráficos.
## As colunas originais são preservadas para manter compatibilidade com as demais etapas do script.
monitora_label_categoria_grafico <- function(x) {
  x_chr <- as.character(x)

  ## Rótulos de categorias agregadas e materiais botânicos.
  labels_fixos <- c(
    "sum_herbacea" = "Herbácea",
    "sum_lenhosa" = "Lenhosa",
    "sum_presence_herb" = "Herbácea",
    "sum_presence_lenh" = "Lenhosa",
    "sum_nativa" = "Nativas",
    "sum_exotica" = "Exóticas",
    "sum_seca_morta" = "Secas ou mortas",
    "sum_presence_nativa" = "Nativas",
    "sum_presence_exotica" = "Exóticas",
    "sum_presence_seca_morta" = "Secas ou mortas",
    "material_botanico" = "Material Botânico",
    "solo_nu" = "Solo exposto ou rochas",
    "mat_bot_serrapilheira" = "Serrapilheira",
    "mat_bot_fragmentos_botanicos" = "Fragmentos botânicos",
    "mat_bot_material_inundado" = "Material inundável",
    "serrapilheira" = "Serrapilheira",
    "fragmentos_botanicos" = "Fragmentos botânicos",
    "material_inundado" = "Material inundável"
  )

  ## Labels de todas as formas de vida previstas no XLSForm.
  ## As versões nativa/exótica/seca ou morta são explicitadas para garantir
  ## concordância nominal nos gráficos, independentemente dos valores presentes
  ## nos arquivos de entrada.
  labels_formas_vida <- c(
    "graminoide" = "Erva graminoide",
    "erva_nao_graminoide" = "Erva não graminoide",
    "arbusto_abaixo" = "Arbusto < 50cm",
    "arbusto_acima" = "Arbusto ≥ 50cm",
    "arvore_abaixo" = "Árvore D30 < 5cm",
    "arvore_acima" = "Árvore D30 ≥ 5cm",
    "bambu" = "Bambu",
    "bromelioide" = "Erva bromelioide",
    "erva_bromelioide" = "Erva bromelioide",
    "cactacea" = "Cacto",
    "lianas" = "Liana/cipó",
    "ervas_de_passarinho" = "Erva-de-passarinho",
    "orquidea" = "Orquídea",
    "palmeira" = "Palmeira",
    "samambaia" = "Samambaia",
    "canela_de_ema" = "Velósia",
    "desconhecida" = "Forma desconhecida"
  )

  labels_nativas <- c(
    "graminoide" = "Erva graminoide nativa",
    "erva_nao_graminoide" = "Erva não graminoide nativa",
    "arbusto_abaixo" = "Arbusto nativo < 50cm",
    "arbusto_acima" = "Arbusto nativo ≥ 50cm",
    "arvore_abaixo" = "Árvore nativa D30 < 5cm",
    "arvore_acima" = "Árvore nativa D30 ≥ 5cm",
    "bambu" = "Bambu nativo",
    "bromelioide" = "Erva bromelioide nativa",
    "erva_bromelioide" = "Erva bromelioide nativa",
    "cactacea" = "Cacto nativo",
    "lianas" = "Liana/cipó nativo",
    "ervas_de_passarinho" = "Erva-de-passarinho nativa",
    "orquidea" = "Orquídea nativa",
    "palmeira" = "Palmeira nativa",
    "samambaia" = "Samambaia nativa",
    "canela_de_ema" = "Velósia nativa",
    "desconhecida" = "Forma desconhecida nativa"
  )

  labels_exoticas <- c(
    "graminoide" = "Erva graminoide exótica",
    "erva_nao_graminoide" = "Erva não graminoide exótica",
    "arbusto_abaixo" = "Arbusto exótico < 50cm",
    "arbusto_acima" = "Arbusto exótico ≥ 50cm",
    "arvore_abaixo" = "Árvore exótica D30 < 5cm",
    "arvore_acima" = "Árvore exótica D30 ≥ 5cm",
    "bambu" = "Bambu exótico",
    "bromelioide" = "Erva bromelioide exótica",
    "erva_bromelioide" = "Erva bromelioide exótica",
    "cactacea" = "Cacto exótico",
    "lianas" = "Liana/cipó exótico",
    "ervas_de_passarinho" = "Erva-de-passarinho exótica",
    "orquidea" = "Orquídea exótica",
    "palmeira" = "Palmeira exótica",
    "samambaia" = "Samambaia exótica",
    "canela_de_ema" = "Velósia exótica",
    "desconhecida" = "Forma desconhecida exótica"
  )

  labels_secas_mortas <- c(
    "graminoide" = "Erva graminoide seca ou morta",
    "erva_nao_graminoide" = "Erva não graminoide seca ou morta",
    "arbusto_abaixo" = "Arbusto < 50cm seco ou morto",
    "arbusto_acima" = "Arbusto ≥ 50cm seco ou morto",
    "arvore_abaixo" = "Árvore D30 < 5cm seca ou morta",
    "arvore_acima" = "Árvore D30 ≥ 5cm seca ou morta",
    "bambu" = "Bambu seco ou morto",
    "bromelioide" = "Erva bromelioide seca ou morta",
    "erva_bromelioide" = "Erva bromelioide seca ou morta",
    "cactacea" = "Cacto seco ou morto",
    "lianas" = "Liana/cipó seco ou morto",
    "ervas_de_passarinho" = "Erva-de-passarinho seca ou morta",
    "orquidea" = "Orquídea seca ou morta",
    "palmeira" = "Palmeira seca ou morta",
    "samambaia" = "Samambaia seca ou morta",
    "canela_de_ema" = "Velósia seca ou morta",
    "desconhecida" = "Forma desconhecida seca ou morta"
  )

  out <- labels_fixos[x_chr]
  faltantes <- is.na(out) & !is.na(x_chr)

  if (any(faltantes)) {
    y <- x_chr[faltantes]
    base <- y
    base <- stringr::str_remove(base, "^nativa_")
    base <- stringr::str_remove(base, "^exot_")
    base <- stringr::str_remove(base, "^seca_morta_")

    out_faltantes <- dplyr::case_when(
      stringr::str_starts(y, "nativa_") ~ unname(labels_nativas[base]),
      stringr::str_starts(y, "exot_") ~ unname(labels_exoticas[base]),
      stringr::str_starts(y, "seca_morta_") ~ unname(labels_secas_mortas[base]),
      TRUE ~ unname(labels_formas_vida[base])
    )

    faltantes_sem_label <- is.na(out_faltantes) & !is.na(base)
    if (any(faltantes_sem_label)) {
      out_faltantes[faltantes_sem_label] <- stringr::str_to_sentence(
        stringr::str_replace_all(base[faltantes_sem_label], "_", " ")
      )
    }

    out[faltantes] <- out_faltantes
  }

  unname(out)
}

## Define a ordem visual das categorias nos CSVs auxiliares e nos gráficos
## reconstruídos fora do R. A ordem é usada para evitar que planilhas ordenem
## as categorias alfabeticamente de forma diferente dos gráficos originais.
monitora_ordem_categoria_grafico <- function(x) {
  x_chr <- as.character(x)

  ordem_fixa <- c(
    "sum_herbacea" = 1L,
    "sum_presence_herb" = 1L,
    "sum_lenhosa" = 2L,
    "sum_presence_lenh" = 2L,
    "sum_nativa" = 1L,
    "sum_presence_nativa" = 1L,
    "sum_exotica" = 2L,
    "sum_presence_exotica" = 2L,
    "sum_seca_morta" = 3L,
    "sum_presence_seca_morta" = 3L,
    "material_botanico" = 4L,
    "solo_nu" = 5L,
    "serrapilheira" = 1L,
    "mat_bot_serrapilheira" = 1L,
    "fragmentos_botanicos" = 2L,
    "mat_bot_fragmentos_botanicos" = 2L,
    "material_inundado" = 3L,
    "mat_bot_material_inundado" = 3L
  )

  ordem_formas <- c(
    "graminoide" = 1L,
    "erva_nao_graminoide" = 2L,
    "arbusto_abaixo" = 3L,
    "arbusto_acima" = 4L,
    "arvore_abaixo" = 5L,
    "arvore_acima" = 6L,
    "bambu" = 7L,
    "bromelioide" = 8L,
    "erva_bromelioide" = 8L,
    "cactacea" = 9L,
    "lianas" = 10L,
    "ervas_de_passarinho" = 11L,
    "orquidea" = 12L,
    "palmeira" = 13L,
    "samambaia" = 14L,
    "canela_de_ema" = 15L,
    "desconhecida" = 99L
  )

  out <- ordem_fixa[x_chr]
  faltantes <- is.na(out) & !is.na(x_chr)

  if (any(faltantes)) {
    base <- x_chr[faltantes]
    base <- stringr::str_remove(base, "^nativa_")
    base <- stringr::str_remove(base, "^exot_")
    base <- stringr::str_remove(base, "^seca_morta_")
    out[faltantes] <- ordem_formas[base]
  }

  out[is.na(out) & !is.na(x_chr)] <- 999L
  as.integer(unname(out))
}

monitora_formatar_percentual_rotulo <- function(x, digits = 1) {
  x_num <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x_num), NA_character_, paste0(formatC(x_num, format = "f", digits = digits), "%"))
  out
}

## Prepara tabelas de proporção relativa para uso direto em planilhas.
## Mantém as colunas técnicas originais e acrescenta campos padronizados para Excel.
monitora_preparar_auxiliar_prop <- function(dt, grupo_grafico, plot_base) {
  if (is.null(dt) || !is.data.frame(dt)) return(dt)
  out <- data.table::as.data.table(data.table::copy(dt))
  if (!nrow(out)) return(out)

  if (!"categoria_label" %in% names(out) && "categoria" %in% names(out)) {
    out[, categoria_label := monitora_label_categoria_grafico(categoria)]
  }
  if (!"prop" %in% names(out)) out[, prop := NA_real_]

  out[, `:=`(
    tipo_grafico = "proporcao_relativa",
    grupo_grafico = grupo_grafico,
    plot_base = plot_base,
    valor = suppressWarnings(as.numeric(prop)),
    prop_percent = round(suppressWarnings(as.numeric(prop)) * 100, 1),
    valor_percent = round(suppressWarnings(as.numeric(prop)) * 100, 1),
    ordem_categoria = if ("categoria" %in% names(out)) monitora_ordem_categoria_grafico(categoria) else NA_integer_
  )]
  out[, rotulo_plot := paste0("(n=", n, "; ", scales::percent(suppressWarnings(as.numeric(prop)), accuracy = .1), ")")]
  data.table::setorder(out, ANO, form_veg, ordem_categoria, categoria_label)
  out <- monitora_stat_anexar_auxiliar(out, grupo_grafico, "proporcao_relativa")

  cols_prioritarias <- intersect(
    c("tipo_grafico", "grupo_grafico", "plot_base", "ANO", "form_veg", "ordem_categoria", "categoria", "categoria_label", "n", "prop", "prop_percent", "valor", "valor_percent", "rotulo_plot", "ano_comparacao_anterior", "n_UA_pareadas", "diferenca_pp", "ci95_lower_pp", "ci95_upper_pp", "p_valor_perm_pareado", "p_ajustado_fdr", "classe_mudanca", "simbolo_mudanca", "legenda_mudanca", "ano_comparacao_anterior_composicao", "n_UA_pareadas_composicao", "n_categorias_composicao", "distancia_centroide_hellinger", "ci95_lower_dist_hellinger", "ci95_upper_dist_hellinger", "bray_curtis_medio_pareado", "p_valor_perm_multivariado", "p_ajustado_fdr_composicao", "classe_mudanca_composicao", "simbolo_mudanca_composicao", "legenda_mudanca_composicao"),
    names(out)
  )
  data.table::setcolorder(out, c(cols_prioritarias, setdiff(names(out), cols_prioritarias)))
  out
}

## Prepara tabelas de cobertura para uso direto em planilhas, incluindo o caso
## de material botânico, que originalmente era exportado com nomes de colunas diferentes.
monitora_preparar_auxiliar_cobertura <- function(dt, grupo_grafico, plot_base) {
  if (is.null(dt) || !is.data.frame(dt)) return(dt)
  out <- data.table::as.data.table(data.table::copy(dt))
  if (!nrow(out)) return(out)

  if (!"categoria_label" %in% names(out) && "categoria" %in% names(out)) {
    out[, categoria_label := monitora_label_categoria_grafico(categoria)]
  }
  if (!"total_points" %in% names(out) && "n_UA" %in% names(out)) {
    out[, total_points := suppressWarnings(as.numeric(n_UA)) * 101]
  }
  if (!"veg_cover" %in% names(out)) {
    if ("cobertura_percent" %in% names(out)) {
      out[, veg_cover := round(suppressWarnings(as.numeric(cobertura_percent)), 1)]
    } else if (all(c("n", "total_points") %in% names(out))) {
      out[, veg_cover := round((suppressWarnings(as.numeric(n)) / suppressWarnings(as.numeric(total_points))) * 100, 1)]
    } else {
      out[, veg_cover := NA_real_]
    }
  }
  if (!"cobertura" %in% names(out)) {
    out[, cobertura := suppressWarnings(as.numeric(veg_cover)) / 100]
  }
  if (!"cobertura_percent" %in% names(out)) {
    out[, cobertura_percent := suppressWarnings(as.numeric(veg_cover))]
  }
  if (!"se" %in% names(out) && all(c("n", "total_points") %in% names(out))) {
    out[, p_aux_excel := suppressWarnings(as.numeric(n)) / suppressWarnings(as.numeric(total_points))]
    out[, se := sqrt(p_aux_excel * (1 - p_aux_excel) / suppressWarnings(as.numeric(total_points))) * 100]
    out[, ci_lower := round((p_aux_excel - 1.96 * se / 100) * 100, 1)]
    out[, ci_upper := round((p_aux_excel + 1.96 * se / 100) * 100, 1)]
    out[, p_aux_excel := NULL]
  }

  out[, `:=`(
    tipo_grafico = "cobertura",
    grupo_grafico = grupo_grafico,
    plot_base = plot_base,
    valor = suppressWarnings(as.numeric(veg_cover)),
    valor_percent = suppressWarnings(as.numeric(veg_cover)),
    ordem_categoria = if ("categoria" %in% names(out)) monitora_ordem_categoria_grafico(categoria) else NA_integer_
  )]
  out[, rotulo_plot := monitora_formatar_percentual_rotulo(veg_cover, digits = 1)]
  data.table::setorder(out, ANO, form_veg, ordem_categoria, categoria_label)
  out <- monitora_stat_anexar_auxiliar(out, grupo_grafico, "cobertura")

  cols_prioritarias <- intersect(
    c("tipo_grafico", "grupo_grafico", "plot_base", "ANO", "form_veg", "n_UA", "ordem_categoria", "categoria", "categoria_label", "n", "veg_cover", "se", "ci_lower", "ci_upper", "cobertura", "cobertura_percent", "valor", "valor_percent", "rotulo_plot", "ano_comparacao_anterior", "n_UA_pareadas", "diferenca_pp", "ci95_lower_pp", "ci95_upper_pp", "p_valor_perm_pareado", "p_ajustado_fdr", "classe_mudanca", "simbolo_mudanca", "legenda_mudanca", "ano_comparacao_anterior_composicao", "n_UA_pareadas_composicao", "n_categorias_composicao", "distancia_centroide_hellinger", "ci95_lower_dist_hellinger", "ci95_upper_dist_hellinger", "bray_curtis_medio_pareado", "p_valor_perm_multivariado", "p_ajustado_fdr_composicao", "classe_mudanca_composicao", "simbolo_mudanca_composicao", "legenda_mudanca_composicao"),
    names(out)
  )
  data.table::setcolorder(out, c(cols_prioritarias, setdiff(names(out), cols_prioritarias)))
  out
}

monitora_criar_indice_graficos_auxiliares <- function() {
  idx <- data.table::data.table(
    arquivo_csv = c(
      rep("prop_rel_herb_lenh.csv", 4),
      rep("cob_veg_herb_lenh.csv", 2),
      rep("prop_rel_categ.csv", 4),
      rep("cob_veg_categ.csv", 2),
      rep("prop_rel_material_botanico.csv", 4),
      rep("cob_veg_material_botanico.csv", 2),
      rep("prop_rel_form_vida_nat.csv", 4),
      rep("cob_veg_form_vida_nat.csv", 2),
      rep("prop_rel_form_vida_exot.csv", 4),
      rep("cob_veg_form_vida_exot.csv", 2),
      rep("prop_rel_form_vida_seca_morta.csv", 4),
      rep("cob_veg_form_vida_seca_morta.csv", 2)
    ),
    plot_png = c(
      "plot_p1.1.1_prop_rel_herb_lenh_camp_sem_rotulo.png",
      "plot_p1.1.2_prop_rel_herb_lenh_camp_com_rotulo.png",
      "plot_p1.2.1_prop_rel_herb_lenh_sav_sem_rotulo.png",
      "plot_p1.2.2_prop_rel_herb_lenh_sav_com_rotulo.png",
      "plot_p1.3.1_veg_cover_herb_lenh_com_rotulo.png",
      "plot_p1.3.2_veg_cover_herb_lenh_sem_rotulo.png",
      "plot_p2.1.1_prop_rel_categ_camp_sem_rotulo.png",
      "plot_p2.1.2_prop_rel_categ_camp_com_rotulo.png",
      "plot_p2.2.1_prop_rel_categ_sav_sem_rotulo.png",
      "plot_p2.2.2_prop_rel_categ_sav_com_rotulo.png",
      "plot_p2.3.1_veg_cover_categ_com_rotulo.png",
      "plot_p2.3.2_veg_cover_categ_sem_rotulo.png",
      "plot_p2m.1.1_prop_rel_material_botanico_camp_sem_rotulo.png",
      "plot_p2m.1.2_prop_rel_material_botanico_camp_com_rotulo.png",
      "plot_p2m.2.1_prop_rel_material_botanico_sav_sem_rotulo.png",
      "plot_p2m.2.2_prop_rel_material_botanico_sav_com_rotulo.png",
      "plot_p2m.3.1_veg_cover_material_botanico_com_rotulo.png",
      "plot_p2m.3.2_veg_cover_material_botanico_sem_rotulo.png",
      "plot_p3.1.1_prop_rel_nat_camp_sem_rotulo.png",
      "plot_p3.1.2_prop_rel_nat_camp_com_rotulo.png",
      "plot_p3.2.1_prop_rel_nat_sav_sem_rotulo.png",
      "plot_p3.2.2_prop_rel_nat_sav_com_rotulo.png",
      "plot_p3.3.1_veg_cover_nat_com_rotulo.png",
      "plot_p3.3.2_veg_cover_nat_sem_rotulo.png",
      "plot_p4.1.1_prop_rel_exot_camp_sem_rotulo.png",
      "plot_p4.1.2_prop_rel_exot_camp_com_rotulo.png",
      "plot_p4.2.1_prop_rel_exot_sav_sem_rotulo.png",
      "plot_p4.2.2_prop_rel_exot_sav_com_rotulo.png",
      "plot_p4.3.1_veg_cover_exot_com_rotulo.png",
      "plot_p4.3.2_veg_cover_exot_sem_rotulo.png",
      "plot_p5.1.1_prop_rel_seca_morta_camp_sem_rotulo.png",
      "plot_p5.1.2_prop_rel_seca_morta_camp_com_rotulo.png",
      "plot_p5.2.1_prop_rel_seca_morta_sav_sem_rotulo.png",
      "plot_p5.2.2_prop_rel_seca_morta_sav_com_rotulo.png",
      "plot_p5.3.1_veg_cover_seca_morta_com_rotulo.png",
      "plot_p5.3.2_seca_morta_sem_rotulo.png"
    ),
    tipo_grafico = c(
      rep("proporcao_relativa", 4), rep("cobertura", 2),
      rep("proporcao_relativa", 4), rep("cobertura", 2),
      rep("proporcao_relativa", 4), rep("cobertura", 2),
      rep("proporcao_relativa", 4), rep("cobertura", 2),
      rep("proporcao_relativa", 4), rep("cobertura", 2),
      rep("proporcao_relativa", 4), rep("cobertura", 2)
    ),
    grupo_grafico = c(
      rep("herbaceas_lenhosas", 6),
      rep("categorias_gerais", 6),
      rep("material_botanico", 6),
      rep("formas_vida_nativas", 6),
      rep("formas_vida_exoticas", 6),
      rep("formas_vida_secas_mortas", 6)
    ),
    form_veg = c(
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica",
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica",
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica",
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica",
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica",
      "Campestre", "Campestre", "Savânica", "Savânica", "Campestre/Savânica", "Campestre/Savânica"
    ),
    usar_coluna_valor = c(rep("valor ou prop", 4), rep("valor ou veg_cover", 2), rep("valor ou prop", 4), rep("valor ou veg_cover", 2), rep("valor ou prop", 4), rep("valor ou veg_cover", 2), rep("valor ou prop", 4), rep("valor ou veg_cover", 2), rep("valor ou prop", 4), rep("valor ou veg_cover", 2), rep("valor ou prop", 4), rep("valor ou veg_cover", 2)),
    usar_coluna_categoria = "categoria_label",
    usar_coluna_ordem = "ordem_categoria",
    usar_coluna_rotulo = "rotulo_plot",
    observacao_excel = "Filtrar form_veg quando aplicável; usar ordem_categoria para ordenar a legenda; usar categoria_label como série/legenda; usar rotulo_plot nos gráficos com rótulo."
  )
  idx[, tipo_versao := ifelse(grepl("sem_rotulo", plot_png), "sem_rotulo", "com_rotulo")]
  idx[, grupo_complexidade := data.table::fifelse(grupo_grafico %in% c("formas_vida_nativas", "formas_vida_exoticas", "formas_vida_secas_mortas"), "muitas_categorias", "poucas_categorias")]
  idx[, uso_recomendado := data.table::fifelse(
    tipo_versao == "sem_rotulo" & grupo_complexidade == "muitas_categorias", "principal_publicacao_e_apoio_bolsista",
    data.table::fifelse(tipo_versao == "sem_rotulo", "apoio_bolsista",
            data.table::fifelse(grupo_complexidade == "muitas_categorias", "apoio_tecnico", "principal_publicacao"))
  )]
  data.table::setcolorder(idx, c("arquivo_csv", "plot_png", "tipo_grafico", "grupo_grafico", "grupo_complexidade", "tipo_versao", "uso_recomendado", "form_veg", "usar_coluna_valor", "usar_coluna_categoria", "usar_coluna_ordem", "usar_coluna_rotulo", "observacao_excel"))
  idx
}

monitora_limpar_ambiente_temporario <- function() {
  objetos_temporarios <- c(
    "x", "dt", "f", "i", "batch_lista", "batch_n", "batch_id", "csv_keep",
    "idx", "idx_nm", "excl", "g", "meta", "aud", "aud_wide", "resumo",
    "col_audit_import", "col_audit_corrig", "csvs_scan", "files_in_zips",
    "p2_presence_form_veg", "mat_bot_stat_cols", "ano_num", "ano_min", "ano_max", "aux", "resumo", "dup", "dup_sum", "miss", "chaves_prob", "prob_idx", "x_max", "x_max2", "x_max3", "x_max4", "x_max5", "x_max_mat",
    "existing_plots", "plot_list"
  )
  rm(list = intersect(objetos_temporarios, ls(envir = .GlobalEnv)), envir = .GlobalEnv)
  invisible(TRUE)
}

## Tema gráfico padronizado para tornar os gráficos mais legíveis, técnicos e publicáveis.
## Mantém títulos com ggplot2 padrão para evitar instabilidade no dispositivo gráfico do RStudio.
monitora_elemento_titulo_publicavel <- function(size = 14) {
  ggplot2::element_text(
    size = size,
    face = "bold",
    hjust = 0.5,
    lineheight = 1.06,
    margin = ggplot2::margin(b = 8)
  )
}

monitora_quebrar_linhas_publicavel <- function(texto, largura = 76L, justificar = FALSE) {
  # Quebra textos longos para impedir corte lateral nos dispositivos PNG/PDF.
  if (is.null(texto) || !nzchar(as.character(texto))) return("")
  largura <- as.integer(largura)
  if (is.na(largura) || largura < 30L) largura <- 76L

  justificar_linha <- function(linha, largura) {
    linha <- trimws(linha)
    palavras <- unlist(strsplit(linha, "\\s+"), use.names = FALSE)
    if (length(palavras) <= 1L) return(linha)
    caracteres_palavras <- sum(nchar(palavras, type = "width"))
    espacos_minimos <- length(palavras) - 1L
    faltam <- largura - caracteres_palavras
    if (faltam <= espacos_minimos) return(linha)
    extras <- faltam - espacos_minimos
    base <- rep(1L, espacos_minimos)
    base <- base + extras %/% espacos_minimos
    resto <- extras %% espacos_minimos
    if (resto > 0L) base[seq_len(resto)] <- base[seq_len(resto)] + 1L
    paste0(
      paste0(palavras[-length(palavras)], strrep(" ", base), collapse = ""),
      palavras[length(palavras)]
    )
  }

  blocos <- unlist(strsplit(as.character(texto), "\n", fixed = FALSE), use.names = FALSE)
  blocos_formatados <- lapply(blocos, function(bloco) {
    bloco <- trimws(bloco)
    if (!nzchar(bloco)) return("")
    linhas <- strwrap(bloco, width = largura, simplify = FALSE)[[1]]
    if (isTRUE(justificar) && length(linhas) > 1L) {
      linhas[-length(linhas)] <- vapply(linhas[-length(linhas)], justificar_linha, character(1), largura = largura)
    }
    paste(linhas, collapse = "\n")
  })
  paste(unlist(blocos_formatados, use.names = FALSE), collapse = "\n")
}

monitora_elemento_caption_publicavel <- function(size = 7.8) {
  # Mantém sempre a mesma classe de elemento de tema do ggplot2.
  # Isso evita erro de merge entre element_text() e element_textbox_simple().
  ggplot2::element_text(
    size = size,
    hjust = 0,
    lineheight = 1.02,
    margin = ggplot2::margin(t = 7, r = 0, b = 2, l = 0)
  )
}

monitora_theme_prop_publicavel <- function() {
  ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.title = monitora_elemento_titulo_publicavel(size = 14),
      plot.title.position = "plot",
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text = ggplot2::element_text(size = 11, colour = "black"),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = monitora_elemento_caption_publicavel(size = 8.5),
      plot.caption.position = "panel",
      plot.margin = ggplot2::margin(18, 24, 18, 24)
    )
}

monitora_theme_cobertura_publicavel <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = monitora_elemento_titulo_publicavel(size = 14),
      plot.title.position = "plot",
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text = ggplot2::element_text(size = 11, colour = "black"),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = monitora_elemento_caption_publicavel(size = 8.5),
      plot.caption.position = "panel",
      plot.margin = ggplot2::margin(18, 24, 18, 24)
    )
}

## Parâmetros de rótulos: mantêm legibilidade e consistência editorial.
MONITORA_FONTE_ROTULO_PROP <- 3.25
MONITORA_REDUCAO_ROTULO_EXTERNO_MULTIPLO_PT <- 0
MONITORA_REDUCAO_ROTULO_EXTERNO_MULTIPLO <- MONITORA_REDUCAO_ROTULO_EXTERNO_MULTIPLO_PT / ggplot2::.pt
MONITORA_FONTE_ROTULO_COB <- 3.5
MONITORA_LINEHEIGHT_ROTULO <- 0.86
MONITORA_LIMIAR_ROTULO_PROP <- 0.03
MONITORA_LIMIAR_ROTULO_PROP_COMPLEXO <- 0.05
MONITORA_LIMIAR_ROTULO_PROP_MUITAS_CATEGORIAS <- 0.05
MONITORA_LIMIAR_ROTULO_PROP_GRAFICO_MUITO_DENSO <- 0.08
MONITORA_N_CATEGORIAS_ROTULO_COMPLEXO <- 8L
MONITORA_N_CATEGORIAS_ROTULO_MUITO_DENSO <- 12L
MONITORA_LIMIAR_ROTULO_COB <- 2
MONITORA_LIMIAR_ROTULO_COB_COMPLEXO <- 2

monitora_rotulo_prop_plot <- function(prop, n, complexo = FALSE, forcar = FALSE) {
  prop_num <- suppressWarnings(as.numeric(prop))
  n_num <- suppressWarnings(as.numeric(n))
  limiar <- if (isTRUE(complexo)) MONITORA_LIMIAR_ROTULO_PROP_COMPLEXO else MONITORA_LIMIAR_ROTULO_PROP
  forcar_vec <- !is.na(forcar) & as.logical(forcar)

  rotulo <- paste0(
    "n=",
    format(round(n_num, 0), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE),
    "\n",
    scales::percent(prop_num, accuracy = .1, decimal.mark = ",", big.mark = ".")
  )

  ifelse(
    !is.na(prop_num) & prop_num > 0 & (forcar_vec | prop_num >= limiar | prop_num >= 0.50),
    rotulo,
    ""
  )
}

monitora_adicionar_rotulo_prop_plot <- function(dt, complexo = FALSE) {
  out <- data.table::as.data.table(data.table::copy(dt))

  if (!nrow(out) || !all(c("ANO", "form_veg", "prop", "n") %in% names(out))) {
    out[, rotulo_prop_plot := monitora_rotulo_prop_plot(prop, n, complexo = complexo)]
    return(out)
  }

  out[, prop_num_rotulo := suppressWarnings(as.numeric(prop))]
  out[, n_num_rotulo := suppressWarnings(as.numeric(n))]

  out[, n_categorias_positivas_rotulo := sum(!is.na(prop_num_rotulo) & prop_num_rotulo > 0), by = .(ANO, form_veg)]
  out[, rank_prop_rotulo := data.table::frank(-prop_num_rotulo, ties.method = "min", na.last = "keep"), by = .(ANO, form_veg)]

  out[, forcar_rotulo_prop := (
    !is.na(prop_num_rotulo) &
      prop_num_rotulo > 0 &
      (
        n_categorias_positivas_rotulo == 1L |
          prop_num_rotulo >= 0.50 |
          rank_prop_rotulo == 1L
      )
  )]

  out[, rotulo_prop_plot := monitora_rotulo_prop_plot(
    prop = prop_num_rotulo,
    n = n_num_rotulo,
    complexo = complexo,
    forcar = forcar_rotulo_prop
  )]

  out[, c(
    "prop_num_rotulo",
    "n_num_rotulo",
    "n_categorias_positivas_rotulo",
    "rank_prop_rotulo",
    "forcar_rotulo_prop"
  ) := NULL]

  out
}


monitora_ajustar_posicoes_rotulos_externos <- function(dt, dist_min = 0.22, margem_vertical = 0.38) {
  ext <- data.table::as.data.table(data.table::copy(dt))
  if (!nrow(ext)) return(ext)
  if (!all(c("form_veg", "lado_rotulo_prop", "y_alvo_rotulo", "y_base_rotulo") %in% names(ext))) return(ext)

  ext[, id_rotulo_externo_tmp := .I]
  grupos <- unique(ext[, .(form_veg, lado_rotulo_prop)])
  for (i in seq_len(nrow(grupos))) {
    form_i <- grupos$form_veg[[i]]
    lado_i <- grupos$lado_rotulo_prop[[i]]
    idx <- which(ext$form_veg == form_i & ext$lado_rotulo_prop == lado_i)
    if (!length(idx)) next

    bloco <- ext[idx]
    data.table::setorder(bloco, y_alvo_rotulo, ANO_factor_rotulo, x_meio_rotulo)

    y_pref <- bloco$y_alvo_rotulo
    y_base <- bloco$y_base_rotulo
    y_min <- min(y_base, na.rm = TRUE) - margem_vertical
    y_max <- max(y_base, na.rm = TRUE) + margem_vertical
    n_rot <- length(y_pref)
    dist_eff <- if (n_rot <= 1L) 0 else min(dist_min, (y_max - y_min) / (n_rot - 1L))

    y_new <- y_pref
    if (n_rot > 1L) {
      for (j in 2:n_rot) {
        y_new[j] <- max(y_new[j], y_new[j - 1L] + dist_eff)
      }
      if (y_new[n_rot] > y_max) {
        y_new[n_rot] <- y_max
        for (j in seq.int(n_rot - 1L, 1L, by = -1L)) {
          y_new[j] <- min(y_new[j], y_new[j + 1L] - dist_eff)
        }
      }
      if (y_new[1L] < y_min) {
        y_new[1L] <- y_min
        for (j in 2:n_rot) {
          y_new[j] <- max(y_new[j], y_new[j - 1L] + dist_eff)
        }
      }
    } else {
      y_new[1L] <- min(max(y_new[1L], y_min), y_max)
    }

    bloco[, y_alvo_rotulo := y_new]
    ext[match(bloco$id_rotulo_externo_tmp, id_rotulo_externo_tmp), y_alvo_rotulo := bloco$y_alvo_rotulo]
  }

  ext[, id_rotulo_externo_tmp := NULL]
  ext
}



monitora_preparar_rotulos_prop_obrigatorios <- function(dt, prop_min_interno = 0.10, prop_min_exibir = NULL) {

  out <- data.table::as.data.table(data.table::copy(dt))
  if (!nrow(out) || !all(c("ANO", "form_veg", "prop", "n") %in% names(out))) {
    out[, rotulo_prop_plot := character()]
    out[, rotulo_prop_interno := character()]
    out[, rotulo_prop_externo := character()]
    out[, ANO_label_rotulo := character()]
    out[, ANO_factor_rotulo := numeric()]
    return(out)
  }

  if (!"ordem_categoria" %in% names(out)) {
    out[, ordem_categoria := data.table::frank(categoria_label, ties.method = "dense")]
  }

  out[, prop_num_rotulo_obrig := suppressWarnings(as.numeric(prop))]
  out[, n_num_rotulo_obrig := suppressWarnings(as.numeric(n))]
  out[, ANO_label_rotulo := as.character(ANO)]

  anos_rotulo <- unique(out[, .(
    ANO_label_rotulo = as.character(ANO),
    ANO_num_ordem_rotulo = suppressWarnings(as.numeric(as.character(ANO)))
  )])
  data.table::setorder(anos_rotulo, ANO_num_ordem_rotulo, ANO_label_rotulo)
  anos_rotulo[, ANO_factor_rotulo := seq_len(.N)]
  out <- merge(out, anos_rotulo[, .(ANO_label_rotulo, ANO_factor_rotulo)], by = "ANO_label_rotulo", all.x = TRUE, sort = FALSE)

  ## O ggplot2 empilha categorias em sentido inverso ao da legenda em barras horizontais.
  ## As posições de rótulo seguem essa ordem para ancorar cada texto no segmento correto.
  data.table::setorder(out, ANO_factor_rotulo, form_veg, -ordem_categoria)
  out[, prop_stack_rotulo := data.table::fifelse(is.na(prop_num_rotulo_obrig), 0, prop_num_rotulo_obrig)]
  out[, x_fim_rotulo := cumsum(prop_stack_rotulo), by = .(ANO, form_veg)]
  out[, x_inicio_rotulo := x_fim_rotulo - prop_stack_rotulo]
  out[, x_meio_rotulo := x_inicio_rotulo + prop_stack_rotulo / 2]

  out[, rotulo_prop_plot := monitora_rotulo_prop_plot(
    prop = prop_num_rotulo_obrig,
    n = n_num_rotulo_obrig,
    complexo = FALSE,
    forcar = TRUE
  )]
  out[is.na(prop_num_rotulo_obrig) | prop_num_rotulo_obrig <= 0 | is.na(n_num_rotulo_obrig) | n_num_rotulo_obrig <= 0,
      rotulo_prop_plot := ""]

  ## Regra editorial para gráficos densos: quando houver muitas categorias,
  ## não exibir rótulos nem símbolos estatísticos de segmentos inferiores a 5%.
  out[, n_categorias_positivas_ano := sum(!is.na(prop_num_rotulo_obrig) & prop_num_rotulo_obrig > 0),
      by = .(ANO, form_veg)]
  if (is.null(prop_min_exibir)) {
    out[, prop_min_exibir_rotulo := data.table::fifelse(
      n_categorias_positivas_ano >= MONITORA_N_CATEGORIAS_ROTULO_MUITO_DENSO,
      MONITORA_LIMIAR_ROTULO_PROP_GRAFICO_MUITO_DENSO,
      data.table::fifelse(
        n_categorias_positivas_ano >= MONITORA_N_CATEGORIAS_ROTULO_COMPLEXO,
        MONITORA_LIMIAR_ROTULO_PROP_MUITAS_CATEGORIAS,
        0
      )
    )]
  } else {
    prop_min_exibir_num <- suppressWarnings(as.numeric(prop_min_exibir))[1]
    if (is.na(prop_min_exibir_num) || prop_min_exibir_num < 0) prop_min_exibir_num <- 0
    out[, prop_min_exibir_rotulo := prop_min_exibir_num]
  }
  out[prop_num_rotulo_obrig < prop_min_exibir_rotulo, rotulo_prop_plot := ""]

  ## Regra editorial de posicionamento:
  ## 1) rótulos que cabem na barra permanecem dentro dela;
  ## 2) apenas rótulos elegíveis, porém estreitos, vão para a faixa externa;
  ## 3) quando houver múltiplos rótulos externos no mesmo ano/lado, eles são
  ##    distribuídos lado a lado no espaço branco, com a mesma fonte dos internos.
  out[, prop_min_interno_efetivo := data.table::fifelse(
    n_categorias_positivas_ano >= MONITORA_N_CATEGORIAS_ROTULO_COMPLEXO,
    pmin(prop_min_interno, 0.075),
    prop_min_interno
  )]

  out[, rotulo_prop_interno := data.table::fifelse(
    rotulo_prop_plot != "" & prop_num_rotulo_obrig >= prop_min_interno_efetivo,
    rotulo_prop_plot,
    ""
  )]
  out[, rotulo_prop_externo := data.table::fifelse(
    rotulo_prop_plot != "" & prop_num_rotulo_obrig > 0 & prop_num_rotulo_obrig < prop_min_interno_efetivo,
    rotulo_prop_plot,
    ""
  )]

  usar_faixa_branca_rotulo <- any(out$rotulo_prop_externo != "", na.rm = TRUE)
  if (isTRUE(usar_faixa_branca_rotulo)) {
    x_barra_min_prop <- 0.14
    x_barra_max_prop <- 0.86
  } else {
    x_barra_min_prop <- 0.00
    x_barra_max_prop <- 1.00
  }
  x_barra_largura_prop <- x_barra_max_prop - x_barra_min_prop
  out[, x_inicio_plot := x_barra_min_prop + x_inicio_rotulo * x_barra_largura_prop]
  out[, x_fim_plot := x_barra_min_prop + x_fim_rotulo * x_barra_largura_prop]
  out[, x_meio_plot := x_barra_min_prop + x_meio_rotulo * x_barra_largura_prop]
  out[, x_barra_min_plot := x_barra_min_prop]
  out[, x_barra_max_plot := x_barra_max_prop]

  out[, lado_rotulo_prop := data.table::fifelse(x_meio_rotulo < 0.5, "esquerda", "direita")]
  out[, y_base_rotulo := as.numeric(ANO_factor_rotulo)]
  out[, y_alvo_rotulo := y_base_rotulo]

  externos <- out[rotulo_prop_externo != ""]
  if (nrow(externos)) {
    data.table::setorder(externos, ANO_factor_rotulo, form_veg, lado_rotulo_prop, x_meio_rotulo)
    externos[, n_lado_ano := .N, by = .(ANO_factor_rotulo, form_veg, lado_rotulo_prop)]
    externos[lado_rotulo_prop == "esquerda",
             ordem_lado_ano := data.table::frank(-x_meio_plot, ties.method = "first"),
             by = .(ANO_factor_rotulo, form_veg, lado_rotulo_prop)]
    externos[lado_rotulo_prop == "direita",
             ordem_lado_ano := data.table::frank(x_meio_plot, ties.method = "first"),
             by = .(ANO_factor_rotulo, form_veg, lado_rotulo_prop)]

    externos[, hjust_rotulo := 0.5]
    externos[, n_rotulos_externos_ano := .N, by = .(ANO, form_veg)]
    externos[, tamanho_rotulo_externo := MONITORA_FONTE_ROTULO_PROP]

    ## Estimativa da meia largura do rótulo para abrir um espaçamento horizontal
    ## mínimo entre rótulos externos do mesmo ano e evitar colisões visuais.
    externos[, nchar_rotulo_limpo := nchar(gsub("[[:space:]]+", "", rotulo_prop_externo))]
    externos[, meia_largura_rotulo_est := pmin(0.040, pmax(0.019, nchar_rotulo_limpo * 0.0022))]
    externos[, `:=`(
      x_alvo_rotulo = NA_real_,
      x_cotovelo_rotulo = NA_real_,
      x_conector_rotulo = NA_real_,
      y_via_rotulo = y_base_rotulo,
      usar_cotovelo_rotulo = FALSE
    )]

    margem_barra_rotulo <- 0.028
    espaco_min_rotulos <- 0.026
    margem_conector_rotulo <- 0.006
    cotovelo_barra <- 0.014
    espaco_cotovelo_empilhado <- 0.010

    grupos_ext <- unique(externos[, .(ANO_factor_rotulo, form_veg, lado_rotulo_prop)])
    for (i in seq_len(nrow(grupos_ext))) {
      idx <- which(
        externos$ANO_factor_rotulo == grupos_ext$ANO_factor_rotulo[[i]] &
          externos$form_veg == grupos_ext$form_veg[[i]] &
          externos$lado_rotulo_prop == grupos_ext$lado_rotulo_prop[[i]]
      )
      if (!length(idx)) next
      bloco <- data.table::copy(externos[idx])
      data.table::setorder(bloco, ordem_lado_ano)
      n_bloco <- nrow(bloco)
      lado_i <- bloco$lado_rotulo_prop[[1]]
      if (lado_i == "esquerda") {
        borda_livre <- bloco$x_barra_min_plot[[1]] - margem_barra_rotulo
        x_centros <- numeric(n_bloco)
        for (j in seq_len(n_bloco)) {
          meia_j <- bloco$meia_largura_rotulo_est[[j]]
          x_centros[[j]] <- borda_livre - meia_j
          borda_livre <- x_centros[[j]] - meia_j - espaco_min_rotulos
        }
        bloco[, x_alvo_rotulo := x_centros]
        bloco[, x_cotovelo_rotulo := x_barra_min_plot - cotovelo_barra - (seq_len(.N) - 1L) * espaco_cotovelo_empilhado]
        bloco[, x_conector_rotulo := x_alvo_rotulo + meia_largura_rotulo_est + margem_conector_rotulo]
      } else {
        borda_livre <- bloco$x_barra_max_plot[[1]] + margem_barra_rotulo
        x_centros <- numeric(n_bloco)
        for (j in seq_len(n_bloco)) {
          meia_j <- bloco$meia_largura_rotulo_est[[j]]
          x_centros[[j]] <- borda_livre + meia_j
          borda_livre <- x_centros[[j]] + meia_j + espaco_min_rotulos
        }
        bloco[, x_alvo_rotulo := x_centros]
        bloco[, x_cotovelo_rotulo := x_barra_max_plot + cotovelo_barra + (seq_len(.N) - 1L) * espaco_cotovelo_empilhado]
        bloco[, x_conector_rotulo := x_alvo_rotulo - meia_largura_rotulo_est - margem_conector_rotulo]
      }

      if (n_bloco > 1L) {
        ## O rótulo mais próximo da barra recebe ligação simples. Os demais
        ## mantêm o mesmo alinhamento vertical do ano e usam cotovelo a partir
        ## do próprio rótulo, descendo antes de seguir horizontalmente até a
        ## barra. Com isso, a leitura humana do pareamento rótulo-barra fica
        ## mais imediata e as linhas chegam à barra em alturas distintas.
        y_offsets <- c(0, -0.18 - (seq_len(n_bloco - 1L) - 1L) * 0.08)
        bloco[, y_alvo_rotulo := y_base_rotulo]
        bloco[, y_via_rotulo := y_base_rotulo + y_offsets]
        bloco[, usar_cotovelo_rotulo := seq_len(.N) > 1L]
      } else {
        bloco[, y_alvo_rotulo := y_base_rotulo]
        bloco[, y_via_rotulo := y_base_rotulo]
        bloco[, usar_cotovelo_rotulo := FALSE]
      }

      externos[idx] <- bloco
    }

    chaves_ext <- c("ANO", "form_veg", "categoria")
    atualiza <- externos[, c(
      chaves_ext,
      "x_alvo_rotulo", "y_alvo_rotulo", "hjust_rotulo",
      "n_rotulos_externos_ano", "tamanho_rotulo_externo",
      "x_cotovelo_rotulo", "x_conector_rotulo", "y_via_rotulo",
      "usar_cotovelo_rotulo"
    ), with = FALSE]
    out <- merge(out, atualiza, by = chaves_ext, all.x = TRUE, sort = FALSE, suffixes = c("", "_ext"))
    if ("y_alvo_rotulo_ext" %in% names(out)) {
      out[!is.na(y_alvo_rotulo_ext), y_alvo_rotulo := y_alvo_rotulo_ext]
      out[, y_alvo_rotulo_ext := NULL]
    }
  }

  if (!"x_alvo_rotulo" %in% names(out)) out[, x_alvo_rotulo := NA_real_]
  if (!"hjust_rotulo" %in% names(out)) out[, hjust_rotulo := NA_real_]
  if (!"n_rotulos_externos_ano" %in% names(out)) out[, n_rotulos_externos_ano := NA_integer_]
  if (!"tamanho_rotulo_externo" %in% names(out)) out[, tamanho_rotulo_externo := MONITORA_FONTE_ROTULO_PROP]
  if (!"x_cotovelo_rotulo" %in% names(out)) out[, x_cotovelo_rotulo := NA_real_]
  if (!"x_conector_rotulo" %in% names(out)) out[, x_conector_rotulo := NA_real_]
  if (!"y_via_rotulo" %in% names(out)) out[, y_via_rotulo := NA_real_]
  if (!"usar_cotovelo_rotulo" %in% names(out)) out[, usar_cotovelo_rotulo := FALSE]
  out[is.na(x_alvo_rotulo), x_alvo_rotulo := data.table::fifelse(lado_rotulo_prop == "esquerda", 0.070, 0.930)]
  out[is.na(hjust_rotulo), hjust_rotulo := 0.5]
  out[is.na(y_alvo_rotulo), y_alvo_rotulo := y_base_rotulo]
  out[is.na(y_via_rotulo), y_via_rotulo := y_alvo_rotulo]
  out[is.na(tamanho_rotulo_externo), tamanho_rotulo_externo := MONITORA_FONTE_ROTULO_PROP]
  out[is.na(x_cotovelo_rotulo), x_cotovelo_rotulo := data.table::fifelse(lado_rotulo_prop == "esquerda", x_barra_min_plot - 0.014, x_barra_max_plot + 0.014)]
  out[is.na(x_conector_rotulo), x_conector_rotulo := data.table::fifelse(
    lado_rotulo_prop == "esquerda",
    pmin(x_alvo_rotulo + 0.028, x_barra_min_plot - 0.006),
    pmax(x_alvo_rotulo - 0.028, x_barra_max_plot + 0.006)
  )]
  out[, c("prop_stack_rotulo", "n_categorias_positivas_ano", "prop_min_exibir_rotulo", "prop_min_interno_efetivo") := NULL]
  out[]
}


monitora_scale_x_prop_obrigatorios <- function(dados_rotulos = NULL) {
  escala_barra_min <- 0.14
  escala_barra_max <- 0.86
  if (!is.null(dados_rotulos) && NROW(dados_rotulos)) {
    dados_rotulos <- data.table::as.data.table(dados_rotulos)
    min_vals <- unique(stats::na.omit(dados_rotulos$x_barra_min_plot))
    max_vals <- unique(stats::na.omit(dados_rotulos$x_barra_max_plot))
    if (length(min_vals) == 1L) escala_barra_min <- min_vals[[1]]
    if (length(max_vals) == 1L) escala_barra_max <- max_vals[[1]]
  }
  escala_breaks_reais <- seq(0, 1, 0.25)
  ggplot2::scale_x_continuous(
    breaks = escala_barra_min + escala_breaks_reais * (escala_barra_max - escala_barra_min),
    labels = scales::percent(escala_breaks_reais, accuracy = 1, decimal.mark = ","),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0))
  )
}

monitora_camada_barras_prop_obrigatorios <- function(dados_rotulos) {
  ggplot2::geom_rect(
    data = data.table::as.data.table(dados_rotulos)[prop_num_rotulo_obrig > 0],
    ggplot2::aes(
      xmin = x_inicio_plot,
      xmax = x_fim_plot,
      ymin = ANO_factor_rotulo - 0.39,
      ymax = ANO_factor_rotulo + 0.39,
      fill = categoria_label
    ),
    inherit.aes = FALSE,
    color = NA,
    show.legend = TRUE
  )
}


monitora_camadas_rotulos_prop_obrigatorios <- function(dados_rotulos) {
  dados_rotulos <- data.table::as.data.table(dados_rotulos)
  dados_internos <- dados_rotulos[rotulo_prop_interno != ""]
  dados_externos <- dados_rotulos[rotulo_prop_externo != ""]

  camada_interna <- ggplot2::geom_text(
    data = dados_internos,
    ggplot2::aes(
      x = x_meio_plot,
      y = ANO_factor_rotulo,
      label = rotulo_prop_interno
    ),
    inherit.aes = FALSE,
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    hjust = 0.5,
    vjust = 0.5
  )

  camada_guia_1 <- ggplot2::geom_segment(
    data = dados_externos,
    ggplot2::aes(
      x = x_conector_rotulo,
      xend = x_meio_plot,
      y = y_via_rotulo,
      yend = y_via_rotulo
    ),
    inherit.aes = FALSE,
    linewidth = 0.22,
    alpha = 0.72,
    color = "black",
    show.legend = FALSE
  )

  camada_guia_2 <- ggplot2::geom_segment(
    data = dados_externos[usar_cotovelo_rotulo %in% TRUE & abs(y_via_rotulo - y_alvo_rotulo) > 1e-8],
    ggplot2::aes(
      x = x_conector_rotulo,
      xend = x_conector_rotulo,
      y = y_alvo_rotulo,
      yend = y_via_rotulo
    ),
    inherit.aes = FALSE,
    linewidth = 0.22,
    alpha = 0.72,
    color = "black",
    show.legend = FALSE
  )

  ## Para o rótulo mais próximo da barra, a ligação permanece simples.
  ## Para os rótulos mais externos, o cotovelo passa a nascer no próprio
  ## rótulo: primeiro um segmento vertical, depois um segmento horizontal em
  ## direção à barra, evitando que a linha aparente atravessar outro rótulo.
  camada_guia_3 <- NULL
  camada_guia_4 <- NULL

  camada_externa <- ggplot2::geom_label(
    data = dados_externos,
    ggplot2::aes(
      x = x_alvo_rotulo,
      y = y_alvo_rotulo,
      label = rotulo_prop_externo,
      hjust = hjust_rotulo
    ),
    inherit.aes = FALSE,
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    label.size = NA,
    label.padding = grid::unit(0.035, "lines"),
    fill = "white",
    alpha = 1,
    color = "black",
    show.legend = FALSE
  )

  escala_anos <- unique(dados_rotulos[, .(ANO_factor_rotulo, ANO_label_rotulo)])
  data.table::setorder(escala_anos, ANO_factor_rotulo)

  list(
    camada_guia_1,
    camada_guia_2,
    camada_interna,
    camada_externa,
    ggplot2::scale_y_continuous(
      breaks = escala_anos$ANO_factor_rotulo,
      labels = escala_anos$ANO_label_rotulo,
      expand = ggplot2::expansion(mult = c(0.10, 0.12))
    ),
    ggplot2::coord_cartesian(xlim = c(0, 1), clip = "on"),
    ggplot2::theme(plot.margin = ggplot2::margin(14, 24, 14, 24))
  )
}


monitora_scale_y_ano_rotulo <- function(dados_rotulos) {
  dados_rotulos <- data.table::as.data.table(dados_rotulos)
  escala_anos <- unique(dados_rotulos[, .(ANO_factor_rotulo, ANO_label_rotulo)])
  data.table::setorder(escala_anos, ANO_factor_rotulo)
  ggplot2::scale_y_continuous(breaks = escala_anos$ANO_factor_rotulo, labels = escala_anos$ANO_label_rotulo)
}

monitora_rotulo_cobertura_plot <- function(veg_cover, complexo = FALSE) {
  valor <- suppressWarnings(as.numeric(veg_cover))
  limiar <- if (isTRUE(complexo)) MONITORA_LIMIAR_ROTULO_COB_COMPLEXO else MONITORA_LIMIAR_ROTULO_COB
  ifelse(!is.na(valor) & valor >= limiar, monitora_formatar_percentual_rotulo(valor, digits = 1), "")
}

monitora_adicionar_rotulo_cobertura_complexa <- function(dt) {
  out <- data.table::as.data.table(dt)
  if (!nrow(out) || !all(c("ANO", "form_veg", "veg_cover") %in% names(out))) {
    out[, rotulo_cobertura_plot := monitora_rotulo_cobertura_plot(veg_cover, complexo = TRUE)]
    return(out)
  }
  out[, rotulo_cobertura_plot := data.table::fifelse(
    !is.na(veg_cover) & veg_cover >= MONITORA_LIMIAR_ROTULO_COB_COMPLEXO,
    monitora_formatar_percentual_rotulo(veg_cover, digits = 1),
    ""
  )]
  out
}

### Análises

monitora_controlar_recursos("inicio_analises", risco = "normal", objeto = if (exists("registros_corrig_stat")) registros_corrig_stat else NULL, force_log = TRUE)
monitora_perf_checkpoint("inicio_analises", "início dos blocos analíticos", if (exists("registros_corrig_stat")) registros_corrig_stat else NULL)

### Proporção relativa de plantas herbáceas e lenhosas

## Gráficos de plantas herbáceas e lenhosas.

## reg_corrig_stat_summarise_p1

if (monitora_any_sum_cols_match(registros_corrig_stat, "sum_herbacea") ||
    monitora_any_sum_cols_match(registros_corrig_stat, "sum_lenhosa")) {
  reg_corrig_stat_summarise_p1 <- registros_corrig_stat %>%
    dplyr::select(any_of(c(
      "UC", "ANO", "form_veg", "sum_herbacea", "sum_lenhosa"
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = c(sum_herbacea, sum_lenhosa)
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria))
  reg_corrig_stat_summarise_p1 <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_p1, complexo = FALSE)
  
  plot_p1.1.1_prop_rel_herb_lenh_camp_sem_rotulo <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  dados_p1_camp_rotulos <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Campestre") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.001)

  plot_p1.1.2_prop_rel_herb_lenh_camp_com_rotulo <- dados_p1_camp_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p1_camp_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p1_camp_rotulos) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p1_camp_rotulos) +
    monitora_theme_prop_publicavel()
  
  plot_p1.2.1_prop_rel_herb_lenh_sav_sem_rotulo <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  dados_p1_sav_rotulos <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Savânica") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.001)

  plot_p1.2.2_prop_rel_herb_lenh_sav_com_rotulo <- dados_p1_sav_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p1_sav_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p1_sav_rotulos) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p1_sav_rotulos) +
    monitora_theme_prop_publicavel()
}

## Presença de plantas herbáceas e lenhosas.

if (monitora_any_sum_cols_match(registros_corrig_stat, "sum_presence_herb") ||
    monitora_any_sum_cols_match(registros_corrig_stat, "sum_presence_lenh")) {
  reg_corrig_stat_summarise_p1_presence <- registros_corrig_stat %>%
    select(any_of(c("UC", "UA", "ANO", "form_veg", "sum_presence_herb", "sum_presence_lenh"))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = c(sum_presence_herb, sum_presence_lenh)
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    summarise(
      n = sum(soma, na.rm = TRUE),
      n_UA = n_distinct(paste(UC, UA, sep = "_")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    relocate(n_UA, .after = form_veg) %>%
    mutate(
      total_points = n_UA * 101,
      p = n / total_points,
      veg_cover = round(p * 100, 1),
      se = sqrt(p * (1 - p) / total_points) * 100,
      ci_lower = round((p - 1.96 * se / 100) * 100, 1),
      ci_upper = round((p + 1.96 * se / 100) * 100, 1)
    ) %>%
    select(-p, -total_points)
  
  
  x_max <- max(reg_corrig_stat_summarise_p1_presence$veg_cover, na.rm = TRUE) * 1.15
  
  # Calcular limite do eixo x
  x_max <- max(reg_corrig_stat_summarise_p1_presence$veg_cover, na.rm = TRUE) * 1.15
  
  # Gráfico COM rótulos
  plot_p1.3.1_veg_cover_herb_lenh_com_rotulo <- ggplot(
    reg_corrig_stat_summarise_p1_presence %>% 
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2, color = "black"
    ) +
    geom_label(
      aes(label = monitora_rotulo_cobertura_plot(veg_cover, complexo = FALSE)),
      position = position_dodge(width = 0.7),
      color = "black",
      fill = "white",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por plantas herbáceas e lenhosas em formações campestres e savânicas",
      x = "Cobertura vegetal (%)",
      y = "Ano",
      fill = "Categoria",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_manual(
      values = c("Herbácea" = "#66c2a5", "Lenhosa" = "#fc8d62")
    ) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, x_max))
  
  # Gráfico SEM rótulos
  plot_p1.3.2_veg_cover_herb_lenh_sem_rotulo <- ggplot(
    reg_corrig_stat_summarise_p1_presence %>% 
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2, color = "black"
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por plantas herbáceas e lenhosas em formações campestres e savânicas",
      x = "Cobertura vegetal (%)",
      y = "Ano",
      fill = "Categoria",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_manual(
      values = c("Herbácea" = "#66c2a5", "Lenhosa" = "#fc8d62")
    ) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, x_max))
  
  rm(x_max)
  
  list(plot_p1.1.1_prop_rel_herb_lenh_camp_sem_rotulo, 
       plot_p1.1.2_prop_rel_herb_lenh_camp_com_rotulo, 
       plot_p1.2.1_prop_rel_herb_lenh_sav_sem_rotulo, 
       plot_p1.2.2_prop_rel_herb_lenh_sav_com_rotulo,
       plot_p1.3.1_veg_cover_herb_lenh_com_rotulo,
       plot_p1.3.2_veg_cover_herb_lenh_sem_rotulo)
}

monitora_perf_checkpoint("analise_herbaceas_lenhosas", "proporção/cobertura herbáceas e lenhosas")

### Proporção relativa de plantas nativas, exóticas, secas ou mortas,
### material botânico em decomposição no solo e solo exposto ou rochas.

## Gráficos de nativas, exóticas, seca/morta, material botânico em decomposição e solo exposto ou
## rochas.

## reg_corrig_stat_summarise_p2

reg_corrig_stat_summarise_p2 <- registros_corrig_stat %>%
  dplyr::select(any_of(
    c(
      "UC",
      "ANO",
      "form_veg",
      "sum_nativa",
      "sum_exotica",
      "sum_seca_morta",
      "material_botanico",
      "solo_nu"
    )
  )) %>%
  pivot_longer(
    names_to = "categoria",
    values_to = "soma",
    cols = -c("UC", "ANO", "form_veg")
  ) %>%
  group_by(ANO, form_veg, categoria) %>%
  dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
  dplyr::mutate(prop = prop.table(n)) %>%
  dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
  filter(n > 0)
reg_corrig_stat_summarise_p2 <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_p2, complexo = FALSE)

## Presença das categorias gerais.

reg_corrig_stat_summarise_p2_presence <- registros_corrig_stat %>%
  select(any_of(c(
    "UC", 
    "UA", 
    "ANO", 
    "form_veg", 
    "sum_presence_nativa",
    "sum_presence_exotica",
    "sum_presence_seca_morta",
    "material_botanico",
    "solo_nu"
  ))) %>%
  pivot_longer(
    cols = any_of(c(
      "sum_presence_nativa",
      "sum_presence_exotica",
      "sum_presence_seca_morta",
      "material_botanico",
      "solo_nu"
    )),
    names_to = "categoria",
    values_to = "soma"
  ) %>%
  group_by(ANO, form_veg, categoria) %>%
  summarise(
    n = sum(soma, na.rm = TRUE),
    n_UA = n_distinct(paste(UC, UA, sep = "_")),
    .groups = "drop"
  ) %>%
  dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
  relocate(n_UA, .after = form_veg) %>%
  mutate(
    total_points = n_UA * 101,
    p = n / total_points,
    veg_cover = round(p * 100, 1),
    se = sqrt(p * (1 - p) / total_points) * 100,
    ci_lower = round((p - 1.96 * se / 100) * 100, 1),
    ci_upper = round((p + 1.96 * se / 100) * 100, 1)
  ) %>%
  select(-p, -total_points)


# Calcular limite do eixo x dinamicamente com alternativa segura
x_max2 <- monitora_safe_xmax(reg_corrig_stat_summarise_p2_presence$veg_cover, mult = 1.15, fallback = 1)

# Filtra formações para os gráficos facetados; se o filtro ficar vazio, gera gráfico substituto
p2_presence_form_veg <- reg_corrig_stat_summarise_p2_presence %>%
  filter(form_veg %in% c("Campestre", "Savânica"))

if (monitora_tem_linhas(p2_presence_form_veg)) {
  # Gráfico com rótulos
  plot_p2.3.1_veg_cover_categ_com_rotulo <- ggplot(
    p2_presence_form_veg,
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2,
      color = "black"
    ) +
    
    geom_label(
      aes(label = monitora_rotulo_cobertura_plot(veg_cover, complexo = FALSE)),
      position = position_dodge(width = 0.7),
      color = "black",
      fill = "white",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    
    facet_wrap(~form_veg) +
    
    labs(
      title = "Cobertura vegetal por plantas nativas, exóticas, secas ou mortas, material botânico em decomposição e solo exposto ou rochas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Categoria",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    
    scale_fill_brewer(palette = "Set2") +
    
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    
    coord_cartesian(xlim = c(0, x_max2))
  
  # Gráfico sem rótulos
  plot_p2.3.2_veg_cover_categ_sem_rotulo <- ggplot(
    p2_presence_form_veg,
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2,
      color = "black"
    ) +
    
    facet_wrap(~form_veg) +
    
    labs(
      title = "Cobertura vegetal por plantas nativas, exóticas, secas ou mortas, material botânico em decomposição e solo exposto ou rochas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Categoria",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    
    scale_fill_brewer(palette = "Set2") +
    
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    
    coord_cartesian(xlim = c(0, x_max2))
} else {
  plot_p2.3.1_veg_cover_categ_com_rotulo <- monitora_plot_sem_dados(
    "Cobertura vegetal por plantas nativas, exóticas, secas ou mortas, material botânico em decomposição e solo exposto ou rochas em formações campestres e savânicas",
    "Sem dados de Campestre/Savânica para este conjunto"
  )
  plot_p2.3.2_veg_cover_categ_sem_rotulo <- plot_p2.3.1_veg_cover_categ_com_rotulo
}

## Sínteses específicas por tipo de material botânico em decomposição no solo.
## Como forma_serrapilheira é select_multiple, um mesmo ponto pode contribuir
## para mais de um tipo específico de material.
mat_bot_stat_cols <- grep("^mat_bot_", names(registros_corrig_stat), value = TRUE)
if (length(mat_bot_stat_cols) > 0) {
  reg_corrig_stat_summarise_material_botanico <- registros_corrig_stat %>%
    dplyr::select(any_of(c("UC", "ANO", "form_veg", mat_bot_stat_cols))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = any_of(mat_bot_stat_cols)
    ) %>%
    mutate(
      categoria = dplyr::recode(
        categoria,
        "mat_bot_serrapilheira" = "serrapilheira",
        "mat_bot_fragmentos_botanicos" = "fragmentos_botanicos",
        "mat_bot_material_inundado" = "material_inundado",
        .default = categoria
      )
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE), .groups = "drop") %>%
    group_by(ANO, form_veg) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    ungroup() %>%
    filter(n > 0)
  reg_corrig_stat_summarise_material_botanico <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_material_botanico, complexo = FALSE)

  reg_corrig_stat_summarise_material_botanico_presence <- registros_corrig_stat %>%
    dplyr::select(any_of(c("UC", "UA", "ANO", "form_veg", mat_bot_stat_cols))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = any_of(mat_bot_stat_cols)
    ) %>%
    mutate(
      categoria = dplyr::recode(
        categoria,
        "mat_bot_serrapilheira" = "serrapilheira",
        "mat_bot_fragmentos_botanicos" = "fragmentos_botanicos",
        "mat_bot_material_inundado" = "material_inundado",
        .default = categoria
      )
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    summarise(
      n = sum(soma, na.rm = TRUE),
      n_UA = n_distinct(paste(UC, UA, sep = "_")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    relocate(n_UA, .after = form_veg) %>%
    mutate(
      total_points = n_UA * 101,
      cobertura = n / total_points,
      cobertura_percent = cobertura * 100,
      veg_cover = round(cobertura_percent, 1),
      se = sqrt(cobertura * (1 - cobertura) / total_points) * 100,
      ci_lower = round((cobertura - 1.96 * se / 100) * 100, 1),
      ci_upper = round((cobertura + 1.96 * se / 100) * 100, 1)
    ) %>%
    filter(n > 0)

  x_max_mat <- monitora_safe_xmax(reg_corrig_stat_summarise_material_botanico_presence$veg_cover, mult = 1.15, fallback = 1)

  plot_p2m.1.1_prop_rel_material_botanico_camp_sem_rotulo <- reg_corrig_stat_summarise_material_botanico %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de materiais botânicos em decomposição no solo em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Material botânico"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()

  dados_p2m_camp_rotulos <- reg_corrig_stat_summarise_material_botanico %>%
    subset(., form_veg == "Campestre") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

  plot_p2m.1.2_prop_rel_material_botanico_camp_com_rotulo <- dados_p2m_camp_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p2m_camp_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p2m_camp_rotulos) +
    labs(
      title = "Proporção relativa de materiais botânicos em decomposição no solo em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Material botânico"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p2m_camp_rotulos) +
    monitora_theme_prop_publicavel()

  plot_p2m.2.1_prop_rel_material_botanico_sav_sem_rotulo <- reg_corrig_stat_summarise_material_botanico %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de materiais botânicos em decomposição no solo em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Material botânico"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()

  dados_p2m_sav_rotulos <- reg_corrig_stat_summarise_material_botanico %>%
    subset(., form_veg == "Savânica") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

  plot_p2m.2.2_prop_rel_material_botanico_sav_com_rotulo <- dados_p2m_sav_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p2m_sav_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p2m_sav_rotulos) +
    labs(
      title = "Proporção relativa de materiais botânicos em decomposição no solo em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Material botânico"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p2m_sav_rotulos) +
    monitora_theme_prop_publicavel()

  plot_p2m.3.1_veg_cover_material_botanico_com_rotulo <- ggplot(
    reg_corrig_stat_summarise_material_botanico_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2,
      color = "black"
    ) +
    geom_label(
      aes(label = monitora_rotulo_cobertura_plot(veg_cover, complexo = FALSE)),
      position = position_dodge(width = 0.7),
      color = "black",
      fill = "white",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura de materiais botânicos em decomposição no solo (Campestre e Savânica)",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Material botânico",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_brewer(palette = "Set2") +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, x_max_mat))

  plot_p2m.3.2_veg_cover_material_botanico_sem_rotulo <- ggplot(
    reg_corrig_stat_summarise_material_botanico_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      position = position_dodge(width = 0.7),
      height = 0.2,
      color = "black"
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura de materiais botânicos em decomposição no solo (Campestre e Savânica)",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Material botânico",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_brewer(palette = "Set2") +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, x_max_mat))
}


plot_p2.1.1_prop_rel_categ_camp_sem_rotulo <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Campestre") %>%
  ggplot(aes(prop, ANO, fill = categoria_label)) +
  geom_col() +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
material botânico em decomposição no solo e solo exposto ou rochas
em formações campestres",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()


dados_p2_camp_rotulos <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Campestre") %>%
  monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

plot_p2.1.2_prop_rel_categ_camp_com_rotulo <- dados_p2_camp_rotulos %>%
  ggplot() +
  monitora_camada_barras_prop_obrigatorios(dados_p2_camp_rotulos) +
  monitora_camadas_rotulos_prop_obrigatorios(dados_p2_camp_rotulos) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
material botânico em decomposição no solo e solo exposto ou rochas
em formações campestres",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
    monitora_scale_x_prop_obrigatorios(dados_p2_camp_rotulos) +
    monitora_theme_prop_publicavel()

plot_p2.2.1_prop_rel_categ_sav_sem_rotulo <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Savânica") %>%
  ggplot(aes(prop, ANO, fill = categoria_label)) +
  geom_col() +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
material botânico em decomposição no solo e solo exposto ou rochas
em formações savânicas",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()

dados_p2_sav_rotulos <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Savânica") %>%
  monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

plot_p2.2.2_prop_rel_categ_sav_com_rotulo <- dados_p2_sav_rotulos %>%
  ggplot() +
  monitora_camada_barras_prop_obrigatorios(dados_p2_sav_rotulos) +
  monitora_camadas_rotulos_prop_obrigatorios(dados_p2_sav_rotulos) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
material botânico em decomposição no solo e solo exposto ou rochas
em formações savânicas",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
    monitora_scale_x_prop_obrigatorios(dados_p2_sav_rotulos) +
    monitora_theme_prop_publicavel()

list(plot_p2.1.1_prop_rel_categ_camp_sem_rotulo, 
     plot_p2.1.2_prop_rel_categ_camp_com_rotulo, 
     plot_p2.2.1_prop_rel_categ_sav_sem_rotulo, 
     plot_p2.2.2_prop_rel_categ_sav_com_rotulo,
     plot_p2.3.1_veg_cover_categ_com_rotulo,
     plot_p2.3.2_veg_cover_categ_sem_rotulo)

rm(x_max2)

monitora_perf_checkpoint("analise_categorias_nativa_exotica_seca_material_botanico_solo", "proporção/cobertura por categorias gerais")

### Proporção relativa de formas de vida de plantas nativas

## reg_corrig_stat_summarise_p3

if (monitora_any_sum_cols_match(registros_corrig_stat, "nativa_")) {
  reg_corrig_stat_summarise_p3 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria))
  reg_corrig_stat_summarise_p3 <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_p3, complexo = TRUE)
  
  ## Presença de formas de vida nativas.
  
  reg_corrig_stat_summarise_p3_presence <- registros_corrig_stat %>%
    dplyr::select(., UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "UA", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    summarise(
      n = sum(soma, na.rm = TRUE),
      n_UA = n_distinct(paste(UC, UA, sep = "_")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    relocate(n_UA, .after = form_veg) %>%
    mutate(
      total_points = n_UA * 101,
      p = n / total_points,
      veg_cover = round(p * 100, 1),
      se = sqrt(p * (1 - p) / total_points) * 100,
      ci_lower = round((p - 1.96 * se / 100) * 100, 1),
      ci_upper = round((p + 1.96 * se / 100) * 100, 1)
    ) %>%
    select(-p, -total_points)
  reg_corrig_stat_summarise_p3_presence <- monitora_adicionar_rotulo_cobertura_complexa(reg_corrig_stat_summarise_p3_presence)
  
    # Limite X dinâmico
  x_max3 <- monitora_safe_xmax(reg_corrig_stat_summarise_p3_presence$veg_cover, mult = 1.15, fallback = 1)
  
# Gráfico com rótulos
  
  plot_p3.3.1_veg_cover_nat_com_rotulo <- ggplot(
    reg_corrig_stat_summarise_p3_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    geom_label(
      data = function(d) d[!is.na(d$rotulo_cobertura_plot) & d$rotulo_cobertura_plot != "", ],
      aes(label = rotulo_cobertura_plot),
      position = position_dodge(width = 0.7),
      fill = "white",
      color = "black",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas nativas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max3)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
  plot_p3.3.2_veg_cover_nat_sem_rotulo <- ggplot(
    reg_corrig_stat_summarise_p3_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas nativas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max3)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
# Gráfico sem rótulos
  
  plot_p3.1.1_prop_rel_nat_camp_sem_rotulo <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  dados_p3_camp_rotulos <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Campestre") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

  plot_p3.1.2_prop_rel_nat_camp_com_rotulo <- dados_p3_camp_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p3_camp_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p3_camp_rotulos) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p3_camp_rotulos) +
    monitora_theme_prop_publicavel()
  
  plot_p3.2.1_prop_rel_nat_sav_sem_rotulo <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  dados_p3_sav_rotulos <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Savânica") %>%
    monitora_preparar_rotulos_prop_obrigatorios(prop_min_interno = 0.10)

  plot_p3.2.2_prop_rel_nat_sav_com_rotulo <- dados_p3_sav_rotulos %>%
    ggplot() +
    monitora_camada_barras_prop_obrigatorios(dados_p3_sav_rotulos) +
    monitora_camadas_rotulos_prop_obrigatorios(dados_p3_sav_rotulos) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    monitora_scale_x_prop_obrigatorios(dados_p3_sav_rotulos) +
    monitora_theme_prop_publicavel()
  
  list(plot_p3.1.1_prop_rel_nat_camp_sem_rotulo, 
       plot_p3.1.2_prop_rel_nat_camp_com_rotulo, 
       plot_p3.2.1_prop_rel_nat_sav_sem_rotulo, 
       plot_p3.2.2_prop_rel_nat_sav_com_rotulo,
       plot_p3.3.1_veg_cover_nat_com_rotulo,
       plot_p3.3.2_veg_cover_nat_sem_rotulo)
  rm(x_max3)
}

monitora_perf_checkpoint("analise_formas_vida_nativas", "proporção/cobertura de formas de vida nativas")

### Proporção relativa de formas de vida de plantas exóticas

## Gráficos de formas de vida exóticas.

## reg_corrig_stat_summarise_p4

if (monitora_any_sum_cols_match(registros_corrig_stat, "exot_")) {
  reg_corrig_stat_summarise_p4 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria))
  reg_corrig_stat_summarise_p4 <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_p4, complexo = TRUE)
  
  ## Presença de formas de vida exóticas.
  
  reg_corrig_stat_summarise_p4_presence <- registros_corrig_stat %>%
    dplyr::select(., UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "UA", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    summarise(
      n = sum(soma, na.rm = TRUE),
      n_UA = n_distinct(paste(UC, UA, sep = "_")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    relocate(n_UA, .after = form_veg) %>%
    mutate(
      total_points = n_UA * 101,
      p = n / total_points,
      veg_cover = round(p * 100, 1),
      se = sqrt(p * (1 - p) / total_points) * 100,
      ci_lower = round((p - 1.96 * se / 100) * 100, 1),
      ci_upper = round((p + 1.96 * se / 100) * 100, 1)
    ) %>%
    select(-p, -total_points)
  reg_corrig_stat_summarise_p4_presence <- monitora_adicionar_rotulo_cobertura_complexa(reg_corrig_stat_summarise_p4_presence)
  
  
  # Limite X dinâmico
  x_max4 <- monitora_safe_xmax(reg_corrig_stat_summarise_p4_presence$veg_cover, mult = 1.15, fallback = 1)
  
  # Gráfico com rótulos
  plot_p4.3.1_veg_cover_exot_com_rotulo <- ggplot(
    reg_corrig_stat_summarise_p4_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    geom_label(
      data = function(d) d[!is.na(d$rotulo_cobertura_plot) & d$rotulo_cobertura_plot != "", ],
      aes(label = rotulo_cobertura_plot),
      position = position_dodge(width = 0.7),
      fill = "white",
      color = "black",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas exóticas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max4)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
  plot_p4.3.2_veg_cover_exot_sem_rotulo <- ggplot(
    reg_corrig_stat_summarise_p4_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas exóticas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max4)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
  # Gráfico sem rótulos
  
  plot_p4.1.1_prop_rel_exot_camp_sem_rotulo <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  
  plot_p4.1.2_prop_rel_exot_camp_com_rotulo <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    geom_text(aes(label = rotulo_prop_plot),
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    hjust = 0.5,
    vjust = 0.5,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  
  plot_p4.2.1_prop_rel_exot_sav_sem_rotulo <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  plot_p4.2.2_prop_rel_exot_sav_com_rotulo <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    geom_text(aes(label = rotulo_prop_plot),
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    hjust = 0.5,
    vjust = 0.5,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  rm(x_max4)
  
  list(plot_p4.1.1_prop_rel_exot_camp_sem_rotulo,
       plot_p4.1.2_prop_rel_exot_camp_com_rotulo,
       plot_p4.2.1_prop_rel_exot_sav_sem_rotulo,
       plot_p4.2.2_prop_rel_exot_sav_com_rotulo,
       plot_p4.3.1_veg_cover_exot_com_rotulo,
       plot_p4.3.2_veg_cover_exot_sem_rotulo)
}

monitora_perf_checkpoint("analise_formas_vida_exoticas", "proporção/cobertura de formas de vida exóticas")

### Proporção relativa de formas de vida de plantas secas ou mortas

### Gráficos de formas de vida seca ou morta.

### reg_corrig_stat_summarise_p5

if (monitora_any_sum_cols_match(registros_corrig_stat, "seca_morta_")) {
  reg_corrig_stat_summarise_p5 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria))
  reg_corrig_stat_summarise_p5 <- monitora_adicionar_rotulo_prop_plot(reg_corrig_stat_summarise_p5, complexo = TRUE)
  
  ## Presença de formas de vida secas ou mortas.
  
  reg_corrig_stat_summarise_p5_presence <- registros_corrig_stat %>%
    dplyr::select(., UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "UA", "ANO", "form_veg")
    ) %>%
    group_by(ANO, form_veg, categoria) %>%
    summarise(
      n = sum(soma, na.rm = TRUE),
      n_UA = n_distinct(paste(UC, UA, sep = "_")),
      .groups = "drop"
    ) %>%
    dplyr::mutate(categoria_label = monitora_label_categoria_grafico(categoria)) %>%
    relocate(n_UA, .after = form_veg) %>%
    mutate(
      total_points = n_UA * 101,
      p = n / total_points,
      veg_cover = round(p * 100, 1),
      se = sqrt(p * (1 - p) / total_points) * 100,
      ci_lower = round((p - 1.96 * se / 100) * 100, 1),
      ci_upper = round((p + 1.96 * se / 100) * 100, 1)
    ) %>%
    select(-p, -total_points)
  reg_corrig_stat_summarise_p5_presence <- monitora_adicionar_rotulo_cobertura_complexa(reg_corrig_stat_summarise_p5_presence)
  
  
  # Limite X dinâmico
  x_max5 <- monitora_safe_xmax(reg_corrig_stat_summarise_p5_presence$veg_cover, mult = 1.15, fallback = 1)
  
  # Gráfico com rótulos
  plot_p5.3.1_veg_cover_seca_morta_com_rotulo <- ggplot(
    reg_corrig_stat_summarise_p5_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    geom_label(
      data = function(d) d[!is.na(d$rotulo_cobertura_plot) & d$rotulo_cobertura_plot != "", ],
      aes(label = rotulo_cobertura_plot),
      position = position_dodge(width = 0.7),
      fill = "white",
      color = "black",
      label.size = NA,
      size = MONITORA_FONTE_ROTULO_COB,
      show.legend = FALSE
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas secas ou mortas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max5)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  
  # Gráfico sem rótulos
  plot_p5.3.2_seca_morta_sem_rotulo <- ggplot(
    reg_corrig_stat_summarise_p5_presence %>%
      filter(form_veg %in% c("Campestre", "Savânica")),
    aes(x = veg_cover, y = factor(ANO), fill = categoria_label)
  ) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      position = position_dodge(width = 0.7),
      color = "black"
    ) +
    facet_wrap(~form_veg) +
    labs(
      title = "Cobertura vegetal por formas de vida de plantas secas ou mortas em formações campestres e savânicas",
      x = "Cobertura (%)",
      y = "Ano",
      fill = "Forma de vida",
      caption = "IC95% das barras: aproximação normal, p ± 1,96 × EP."
    ) +
    scale_fill_viridis_d(option = "turbo") +
    coord_cartesian(xlim = c(0, x_max5)) +
    monitora_theme_cobertura_publicavel() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 10, hjust = 0)
    )

  #

    plot_p5.1.1_prop_rel_seca_morta_camp_sem_rotulo <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  plot_p5.1.2_prop_rel_seca_morta_camp_com_rotulo <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    geom_text(aes(label = rotulo_prop_plot),
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    hjust = 0.5,
    vjust = 0.5,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  plot_p5.2.1_prop_rel_seca_morta_sav_sem_rotulo <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  plot_p5.2.2_prop_rel_seca_morta_sav_com_rotulo <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria_label)) +
    geom_col() +
    geom_text(aes(label = rotulo_prop_plot),
    size = MONITORA_FONTE_ROTULO_PROP,
    lineheight = MONITORA_LINEHEIGHT_ROTULO,
    hjust = 0.5,
    vjust = 0.5,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    monitora_theme_prop_publicavel()
  
  rm(x_max5)
  
  list(plot_p5.1.1_prop_rel_seca_morta_camp_sem_rotulo, 
       plot_p5.1.2_prop_rel_seca_morta_camp_com_rotulo, 
       plot_p5.2.1_prop_rel_seca_morta_sav_sem_rotulo, 
       plot_p5.2.2_prop_rel_seca_morta_sav_com_rotulo,
       plot_p5.3.1_veg_cover_seca_morta_com_rotulo,
       plot_p5.3.2_seca_morta_sem_rotulo)
  
}

### remoção de objetos não mais necessários:

if (exists("reg_herb_lenh"))
  rm(reg_herb_lenh)
if (exists("reg_categ_plantas_longer"))
  rm(reg_categ_plantas_longer)
if (exists("reg_formas_vida_nat_longer"))
  rm(reg_formas_vida_nat_longer)
if (exists("reg_formas_vida_exot_longer"))
  rm(reg_formas_vida_exot_longer)
if (exists("reg_formas_vida_seca_morta_longer"))
  rm(reg_formas_vida_seca_morta_longer)

monitora_perf_checkpoint("analise_formas_vida_secas_mortas", "proporção/cobertura de formas de vida secas ou mortas")


### Análise estatística de mudança ano a ano por categoria
###
### Fundamento ecológico:
###   A unidade amostral inferencial é a UA/transecto, não cada ponto da vareta.
###   Os 101 pontos descrevem a proporção observada dentro da UA; a comparação temporal
###   usa somente UAs pareadas entre dois anos consecutivos. Isso reduz pseudorrepetição
###   e torna a inferência mais conservadora.
###
### Critério estatístico:
###   - teste de permutação pareado por inversão de sinal das diferenças entre UAs;
###   - intervalo de confiança por bootstrap pareado das UAs;
###   - correção de múltiplas comparações por FDR Benjamini-Hochberg;
###   - classificação ecológica com margem mínima de mudança em pontos percentuais.
###
### Símbolos:
###   ▲ aumento; ▼ redução; ≈ estabilidade/equivalência dentro da margem ecológica;
###   · inconclusivo; ? pares insuficientes.
### A análise compara anos consecutivos disponíveis em cada UC
### e também compara cada ano com a linha de base acumulada anterior.

MONITORA_STAT_VERBOSE <- tolower(Sys.getenv("MONITORA_STAT_VERBOSE", "true")) %in% c("true", "1", "yes", "sim")
MONITORA_STAT_ALPHA <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_STAT_ALPHA", "0.05")))
MONITORA_STAT_MARGEM_PP <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_STAT_MARGEM_PP", "5")))
MONITORA_STAT_MIN_EFEITO_PP <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_STAT_MIN_EFEITO_PP", "2")))
MONITORA_STAT_MIN_PARES <- suppressWarnings(as.integer(Sys.getenv("MONITORA_STAT_MIN_PARES", "5")))
MONITORA_STAT_BOOT <- suppressWarnings(as.integer(Sys.getenv("MONITORA_STAT_BOOT", "1999")))
MONITORA_STAT_PERM <- suppressWarnings(as.integer(Sys.getenv("MONITORA_STAT_PERM", "4999")))
MONITORA_STAT_COMP_MARGEM_DIST <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_STAT_COMP_MARGEM_DIST", "0.05")))
MONITORA_STAT_COMP_MIN_DIST <- suppressWarnings(as.numeric(Sys.getenv("MONITORA_STAT_COMP_MIN_DIST", "0.03")))
MONITORA_STAT_PERM_CHUNK <- suppressWarnings(as.integer(Sys.getenv("MONITORA_STAT_PERM_CHUNK", "1000")))
MONITORA_STAT_RECURSOS_ADAPTATIVO <- tolower(Sys.getenv("MONITORA_STAT_RECURSOS_ADAPTATIVO", "true")) %in% c("true", "1", "yes", "sim")
MONITORA_STAT_BASELINE_ATIVO <- tolower(Sys.getenv("MONITORA_STAT_BASELINE_ATIVO", "true")) %in% c("true", "1", "yes", "sim")
MONITORA_STAT_BASELINE_MODO <- Sys.getenv("MONITORA_STAT_BASELINE_MODO", "acumulada_anterior")
MONITORA_STAT_BASELINE_MIN_ANOS <- suppressWarnings(as.integer(Sys.getenv("MONITORA_STAT_BASELINE_MIN_ANOS", "2")))
MONITORA_STAT_BASELINE_MOSTRAR_APENAS_MUDANCA <- tolower(Sys.getenv("MONITORA_STAT_BASELINE_MOSTRAR_APENAS_MUDANCA", "true")) %in% c("true", "1", "yes", "sim")

if (is.na(MONITORA_STAT_ALPHA) || MONITORA_STAT_ALPHA <= 0 || MONITORA_STAT_ALPHA >= 1) MONITORA_STAT_ALPHA <- 0.05
if (is.na(MONITORA_STAT_MARGEM_PP) || MONITORA_STAT_MARGEM_PP < 0) MONITORA_STAT_MARGEM_PP <- 5
if (is.na(MONITORA_STAT_MIN_EFEITO_PP) || MONITORA_STAT_MIN_EFEITO_PP < 0) MONITORA_STAT_MIN_EFEITO_PP <- 2
if (is.na(MONITORA_STAT_MIN_PARES) || MONITORA_STAT_MIN_PARES < 2) MONITORA_STAT_MIN_PARES <- 5
if (is.na(MONITORA_STAT_BOOT) || MONITORA_STAT_BOOT < 199) MONITORA_STAT_BOOT <- 1999
if (is.na(MONITORA_STAT_PERM) || MONITORA_STAT_PERM < 499) MONITORA_STAT_PERM <- 4999
if (is.na(MONITORA_STAT_COMP_MARGEM_DIST) || MONITORA_STAT_COMP_MARGEM_DIST < 0) MONITORA_STAT_COMP_MARGEM_DIST <- 0.05
if (is.na(MONITORA_STAT_COMP_MIN_DIST) || MONITORA_STAT_COMP_MIN_DIST < 0) MONITORA_STAT_COMP_MIN_DIST <- 0.03
if (is.na(MONITORA_STAT_PERM_CHUNK) || MONITORA_STAT_PERM_CHUNK < 100L) MONITORA_STAT_PERM_CHUNK <- 1000L
if (is.na(MONITORA_STAT_BASELINE_MIN_ANOS) || MONITORA_STAT_BASELINE_MIN_ANOS < 1L) MONITORA_STAT_BASELINE_MIN_ANOS <- 2L
if (!identical(MONITORA_STAT_BASELINE_MODO, "acumulada_anterior")) MONITORA_STAT_BASELINE_MODO <- "acumulada_anterior"

monitora_stat_msg <- function(...) {
  if (isTRUE(MONITORA_STAT_VERBOSE)) message("[estatistica_mudanca] ", paste0(..., collapse = ""))
}

monitora_stat_controlar_recursos <- function(etapa, risco = "normal", objeto = NULL, force_log = FALSE) {
  if (exists("monitora_controlar_recursos", mode = "function")) {
    return(tryCatch(
      monitora_controlar_recursos(etapa, risco = risco, objeto = objeto, force_log = force_log),
      error = function(e) list(modo = "indefinido", threads = data.table::getDTthreads(), batch_csv = NA_integer_, mem_available_mb = NA_real_)
    ))
  }
  list(modo = "indefinido", threads = data.table::getDTthreads(), batch_csv = NA_integer_, mem_available_mb = NA_real_)
}

monitora_stat_iteracoes_efetivas <- function(n_solicitado, tipo = c("perm", "boot"), etapa = "estatistica", risco = "normal", objeto = NULL) {
  tipo <- match.arg(tipo)
  n_solicitado <- as.integer(n_solicitado)
  if (!isTRUE(MONITORA_STAT_RECURSOS_ADAPTATIVO)) return(n_solicitado)
  rc <- monitora_stat_controlar_recursos(etapa, risco = risco, objeto = objeto, force_log = FALSE)
  modo <- as.character(rc$modo %||% "indefinido")
  n_eff <- n_solicitado
  if (tipo == "perm") {
    if (modo == "critico") n_eff <- min(n_eff, 999L)
    if (modo == "economico") n_eff <- min(n_eff, 1999L)
    if (modo == "equilibrado") n_eff <- min(n_eff, 4999L)
    n_eff <- max(499L, n_eff)
  } else {
    if (modo == "critico") n_eff <- min(n_eff, 499L)
    if (modo == "economico") n_eff <- min(n_eff, 999L)
    if (modo == "equilibrado") n_eff <- min(n_eff, 1999L)
    n_eff <- max(199L, n_eff)
  }
  if (!identical(n_eff, n_solicitado)) {
    monitora_stat_msg("ajuste adaptativo: ", etapa, "; tipo=", tipo, "; modo=", modo, "; iteracoes=", n_eff, "/", n_solicitado)
  }
  n_eff
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

monitora_stat_p_permutacao_pareada <- function(dif, n_perm = MONITORA_STAT_PERM) {
  dif <- as.numeric(dif)
  dif <- dif[is.finite(dif)]
  n <- length(dif)
  if (n == 0) return(NA_real_)
  obs <- mean(dif)
  if (!is.finite(obs)) return(NA_real_)
  if (all(abs(dif) < .Machine$double.eps^0.5)) return(1)

  if (n <= 16) {
    grade <- expand.grid(rep(list(c(-1, 1)), n))
    medias <- as.matrix(grade) %*% dif / n
    return(mean(abs(medias) >= abs(obs)))
  }

  n_perm <- monitora_stat_iteracoes_efetivas(n_perm, tipo = "perm", etapa = "permutacao_pareada", risco = "normal")
  chunk <- min(as.integer(MONITORA_STAT_PERM_CHUNK), n_perm)
  cont <- 0L
  feitas <- 0L
  while (feitas < n_perm) {
    k <- min(chunk, n_perm - feitas)
    sinais <- matrix(sample(c(-1, 1), n * k, replace = TRUE), nrow = k, ncol = n)
    medias <- as.vector(sinais %*% dif / n)
    cont <- cont + sum(abs(medias) >= abs(obs))
    feitas <- feitas + k
  }
  (cont + 1) / (n_perm + 1)
}

monitora_stat_boot_ci <- function(dif, n_boot = MONITORA_STAT_BOOT, conf = 0.95) {
  dif <- as.numeric(dif)
  dif <- dif[is.finite(dif)]
  n <- length(dif)
  if (n == 0) return(c(NA_real_, NA_real_))
  if (n == 1 || all(abs(dif - dif[1]) < .Machine$double.eps^0.5)) return(c(mean(dif), mean(dif)))
  n_boot <- monitora_stat_iteracoes_efetivas(n_boot, tipo = "boot", etapa = "bootstrap_pareado", risco = "normal")
  medias <- numeric(n_boot)
  chunk <- min(as.integer(MONITORA_STAT_PERM_CHUNK), n_boot)
  feitas <- 0L
  while (feitas < n_boot) {
    k <- min(chunk, n_boot - feitas)
    for (bb in seq_len(k)) medias[feitas + bb] <- mean(sample(dif, size = n, replace = TRUE))
    feitas <- feitas + k
  }
  stats::quantile(medias, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE, names = FALSE)
}

monitora_stat_long_ua <- function(dt, cols, grupo_grafico, tipo_metrica, denominador = c("relativo", "101")) {
  denominador <- match.arg(denominador)
  cols <- intersect(cols, names(dt))
  if (length(cols) == 0) return(data.table::data.table())

  base_cols <- intersect(c("UC", "UA", "ANO", "form_veg"), names(dt))
  if (!all(c("UC", "UA", "ANO", "form_veg") %in% base_cols)) return(data.table::data.table())

  tmp <- data.table::as.data.table(data.table::copy(dt[, c(base_cols, cols), with = FALSE]))
  for (cc in cols) tmp[, (cc) := suppressWarnings(as.numeric(get(cc)))]

  if (denominador == "relativo") {
    tmp[, MONITORA_TOTAL_REL := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
    long <- data.table::melt(
      tmp,
      id.vars = c(base_cols, "MONITORA_TOTAL_REL"),
      measure.vars = cols,
      variable.name = "categoria",
      value.name = "n_categoria"
    )
    long[, valor := fifelse(MONITORA_TOTAL_REL > 0, n_categoria / MONITORA_TOTAL_REL, NA_real_)]
    long[, total_referencia := MONITORA_TOTAL_REL]
    long[, MONITORA_TOTAL_REL := NULL]
  } else {
    long <- data.table::melt(
      tmp,
      id.vars = base_cols,
      measure.vars = cols,
      variable.name = "categoria",
      value.name = "n_categoria"
    )
    long[, valor := n_categoria / 101]
    long[, total_referencia := 101]
  }

  long <- long[!is.na(ANO) & !is.na(form_veg) & !is.na(categoria) & is.finite(valor)]
  long[, `:=`(
    ANO = suppressWarnings(as.integer(ANO)),
    UC = as.character(UC),
    UA = as.character(UA),
    form_veg = as.character(form_veg),
    grupo_grafico = grupo_grafico,
    tipo_metrica = tipo_metrica,
    categoria = as.character(categoria),
    categoria_label = monitora_label_categoria_grafico(as.character(categoria))
  )]
  long[]
}


monitora_stat_classificar_categoria <- function(res, contexto = c("ano_anterior", "linha_base")) {
  contexto <- match.arg(contexto)
  if (is.null(res) || nrow(res) == 0) return(res)
  margem <- MONITORA_STAT_MARGEM_PP / 100
  min_efeito <- MONITORA_STAT_MIN_EFEITO_PP / 100
  p_col <- if (contexto == "linha_base") "p_ajustado_fdr_linha_base" else "p_ajustado_fdr"
  res[, classe_mudanca := data.table::fcase(
    is.na(get(p_col)) | n_UA_pareadas < MONITORA_STAT_MIN_PARES, "pares_insuficientes",
    get(p_col) <= MONITORA_STAT_ALPHA & ci95_lower > 0 & diferenca >= min_efeito, "aumento",
    get(p_col) <= MONITORA_STAT_ALPHA & ci95_upper < 0 & diferenca <= -min_efeito, "reducao",
    ci95_lower >= -margem & ci95_upper <= margem, "estabilidade_equivalente",
    default = "inconclusivo"
  )]
  if (contexto == "linha_base") {
    res[, simbolo_mudanca := data.table::fcase(
      classe_mudanca == "aumento", "△",
      classe_mudanca == "reducao", "▽",
      classe_mudanca == "estabilidade_equivalente", "≋",
      classe_mudanca == "pares_insuficientes", "?",
      default = "·"
    )]
    res[, legenda_mudanca := data.table::fcase(
      classe_mudanca == "aumento", "aumento estatisticamente sustentado em relação à linha de base acumulada anterior",
      classe_mudanca == "reducao", "redução estatisticamente sustentada em relação à linha de base acumulada anterior",
      classe_mudanca == "estabilidade_equivalente", paste0("mudança dentro da margem ecológica de ±", MONITORA_STAT_MARGEM_PP, " p.p. em relação à linha de base"),
      classe_mudanca == "pares_insuficientes", paste0("menos de ", MONITORA_STAT_MIN_PARES, " UAs pareadas ou menos de ", MONITORA_STAT_BASELINE_MIN_ANOS, " anos de linha de base"),
      default = "resultado inconclusivo em relação à linha de base"
    )]
  } else {
    res[, simbolo_mudanca := data.table::fcase(
      classe_mudanca == "aumento", "▲",
      classe_mudanca == "reducao", "▼",
      classe_mudanca == "estabilidade_equivalente", "≈",
      classe_mudanca == "pares_insuficientes", "?",
      default = "·"
    )]
    res[, legenda_mudanca := data.table::fcase(
      classe_mudanca == "aumento", "aumento estatisticamente sustentado em relação à medição anterior disponível da mesma UC",
      classe_mudanca == "reducao", "redução estatisticamente sustentada em relação à medição anterior disponível da mesma UC",
      classe_mudanca == "estabilidade_equivalente", paste0("mudança dentro da margem ecológica de ±", MONITORA_STAT_MARGEM_PP, " p.p."),
      classe_mudanca == "pares_insuficientes", paste0("menos de ", MONITORA_STAT_MIN_PARES, " UAs pareadas"),
      default = "resultado inconclusivo"
    )]
  }
  res[]
}

monitora_stat_comparar_adjacent <- function(long_dt) {
  # Compara cada ano-alvo com a medição imediatamente anterior disponível
  # dentro de cada UC. Isso evita comparações globais sem sobreposição amostral
  # quando uma UC não foi monitorada em anos estritamente consecutivos.
  if (is.null(long_dt) || nrow(long_dt) == 0) return(data.table::data.table())
  long_dt <- data.table::as.data.table(long_dt)
  out <- vector("list", 0)
  idx <- 0L
  grupos <- unique(long_dt[, .(grupo_grafico, tipo_metrica, form_veg, categoria, categoria_label)])
  for (ii in seq_len(nrow(grupos))) {
    g <- grupos[ii]
    sub <- long_dt[
      grupo_grafico == g$grupo_grafico &
        tipo_metrica == g$tipo_metrica &
        form_veg == g$form_veg &
        categoria == g$categoria
    ]
    anos_uc <- unique(sub[, .(UC, ANO)])
    anos_uc <- anos_uc[!is.na(UC) & !is.na(ANO)]
    data.table::setorder(anos_uc, UC, ANO)
    anos_uc[, ano_1 := data.table::shift(ANO, type = "lag"), by = UC]
    pares_uc <- anos_uc[!is.na(ano_1), .(UC, ano_1 = as.integer(ano_1), ano_2 = as.integer(ANO))]
    if (nrow(pares_uc) == 0) next
    anos_alvo <- sort(unique(pares_uc$ano_2))
    for (y2 in anos_alvo) {
      pares_y <- pares_uc[ano_2 == y2]
      par_lista <- vector("list", nrow(pares_y))
      for (jj in seq_len(nrow(pares_y))) {
        u <- pares_y$UC[jj]
        y1 <- pares_y$ano_1[jj]
        a <- sub[UC == u & ANO == y1, .(UC, UA, valor_1 = valor)]
        b <- sub[UC == u & ANO == y2, .(UC, UA, valor_2 = valor)]
        if (!nrow(a) || !nrow(b)) next
        data.table::setkeyv(a, c("UC", "UA"))
        data.table::setkeyv(b, c("UC", "UA"))
        par_lista[[jj]] <- b[a, nomatch = 0L][, `:=`(ano_1_uc = y1, ano_2 = y2)]
      }
      par <- data.table::rbindlist(par_lista, fill = TRUE)
      n_pares <- nrow(par)
      dif <- par$valor_2 - par$valor_1
      efeito <- if (n_pares > 0) mean(dif, na.rm = TRUE) else NA_real_
      ci <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_boot_ci(dif) else c(NA_real_, NA_real_)
      p_val <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_p_permutacao_pareada(dif) else NA_real_
      idx <- idx + 1L
      out[[idx]] <- data.table::data.table(
        grupo_grafico = g$grupo_grafico,
        tipo_metrica = g$tipo_metrica,
        form_veg = g$form_veg,
        categoria = g$categoria,
        categoria_label = g$categoria_label,
        ano_1 = if (n_pares > 0) min(par$ano_1_uc, na.rm = TRUE) else min(pares_y$ano_1, na.rm = TRUE),
        ano_2 = y2,
        anos_referencia = paste(sort(unique(pares_y$ano_1)), collapse = ";"),
        comparacao = "medicao_anterior_disponivel_por_UC",
        n_UC_pareadas = uniqueN(par$UC),
        n_UA_pareadas = n_pares,
        media_ano_1 = if (n_pares > 0) mean(par$valor_1, na.rm = TRUE) else NA_real_,
        media_ano_2 = if (n_pares > 0) mean(par$valor_2, na.rm = TRUE) else NA_real_,
        diferenca = efeito,
        diferenca_pp = efeito * 100,
        ci95_lower = ci[1],
        ci95_upper = ci[2],
        ci95_lower_pp = ci[1] * 100,
        ci95_upper_pp = ci[2] * 100,
        p_valor_perm_pareado = p_val
      )
    }
  }
  res <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(res) == 0) return(res)
  res[, p_ajustado_fdr := stats::p.adjust(p_valor_perm_pareado, method = "BH"), by = .(grupo_grafico, tipo_metrica)]
  res <- monitora_stat_classificar_categoria(res, contexto = "ano_anterior")
  data.table::setorder(res, grupo_grafico, tipo_metrica, form_veg, categoria, ano_2)
  res[]
}

monitora_stat_comparar_linha_base <- function(long_dt) {
  # Linha de base acumulada anterior: para cada ano-alvo, calcula a média dos anos
  # anteriores da mesma UC+UA e compara o ano-alvo contra essa média basal.
  if (is.null(long_dt) || nrow(long_dt) == 0 || !isTRUE(MONITORA_STAT_BASELINE_ATIVO)) return(data.table::data.table())
  long_dt <- data.table::as.data.table(long_dt)
  out <- vector("list", 0)
  idx <- 0L
  grupos <- unique(long_dt[, .(grupo_grafico, tipo_metrica, form_veg, categoria, categoria_label)])
  for (ii in seq_len(nrow(grupos))) {
    g <- grupos[ii]
    sub <- long_dt[
      grupo_grafico == g$grupo_grafico &
        tipo_metrica == g$tipo_metrica &
        form_veg == g$form_veg &
        categoria == g$categoria
    ]
    anos <- sort(unique(as.integer(sub$ANO)))
    if (length(anos) < MONITORA_STAT_BASELINE_MIN_ANOS + 1L) next
    for (y2 in anos) {
      base_anos <- anos[anos < y2]
      n_anos_base <- length(base_anos)
      atual <- sub[ANO == y2, .(UC, UA, valor_ano = valor)]
      if (!nrow(atual)) next
      if (n_anos_base >= MONITORA_STAT_BASELINE_MIN_ANOS) {
        base <- sub[ANO < y2, .(
          valor_linha_base = mean(valor, na.rm = TRUE),
          n_anos_linha_base_UA = uniqueN(ANO)
        ), by = .(UC, UA)]
        data.table::setkeyv(atual, c("UC", "UA"))
        data.table::setkeyv(base, c("UC", "UA"))
        par <- base[atual, nomatch = 0L]
      } else {
        par <- data.table::data.table()
      }
      n_pares <- nrow(par)
      dif <- par$valor_ano - par$valor_linha_base
      efeito <- if (n_pares > 0) mean(dif, na.rm = TRUE) else NA_real_
      testavel <- n_pares >= MONITORA_STAT_MIN_PARES && n_anos_base >= MONITORA_STAT_BASELINE_MIN_ANOS
      ci <- if (testavel) monitora_stat_boot_ci(dif) else c(NA_real_, NA_real_)
      p_val <- if (testavel) monitora_stat_p_permutacao_pareada(dif) else NA_real_
      idx <- idx + 1L
      out[[idx]] <- data.table::data.table(
        grupo_grafico = g$grupo_grafico,
        tipo_metrica = g$tipo_metrica,
        form_veg = g$form_veg,
        categoria = g$categoria,
        categoria_label = g$categoria_label,
        ano_2 = y2,
        anos_linha_base = paste(base_anos, collapse = ";"),
        n_anos_linha_base = n_anos_base,
        modo_linha_base = MONITORA_STAT_BASELINE_MODO,
        n_UC_pareadas = uniqueN(par$UC),
        n_UA_pareadas = n_pares,
        media_linha_base = if (n_pares > 0) mean(par$valor_linha_base, na.rm = TRUE) else NA_real_,
        media_ano_2 = if (n_pares > 0) mean(par$valor_ano, na.rm = TRUE) else NA_real_,
        diferenca = efeito,
        diferenca_pp = efeito * 100,
        ci95_lower = ci[1],
        ci95_upper = ci[2],
        ci95_lower_pp = ci[1] * 100,
        ci95_upper_pp = ci[2] * 100,
        p_valor_perm_pareado = p_val
      )
    }
  }
  res <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(res) == 0) return(res)
  res[, p_ajustado_fdr_linha_base := stats::p.adjust(p_valor_perm_pareado, method = "BH"), by = .(grupo_grafico, tipo_metrica)]
  res <- monitora_stat_classificar_categoria(res, contexto = "linha_base")
  data.table::setorder(res, grupo_grafico, tipo_metrica, form_veg, categoria, ano_2)
  res[]
}


monitora_stat_padronizar_linhas <- function(mat) {
  mat <- as.matrix(mat)
  mat[!is.finite(mat)] <- 0
  mat[mat < 0] <- 0
  rs <- rowSums(mat, na.rm = TRUE)
  out <- mat
  ok <- is.finite(rs) & rs > 0
  out[ok, ] <- out[ok, , drop = FALSE] / rs[ok]
  out[!ok, ] <- 0
  out
}

monitora_stat_dist_bray_media <- function(x1, x2) {
  x1 <- as.matrix(x1)
  x2 <- as.matrix(x2)
  den <- rowSums(abs(x1) + abs(x2), na.rm = TRUE)
  num <- rowSums(abs(x2 - x1), na.rm = TRUE)
  bc <- ifelse(den > 0, num / den, 0)
  mean(bc, na.rm = TRUE)
}

monitora_stat_composicao_boot_ci <- function(dif_mat, n_boot = MONITORA_STAT_BOOT, conf = 0.95) {
  dif_mat <- as.matrix(dif_mat)
  n <- nrow(dif_mat)
  if (n == 0) return(c(NA_real_, NA_real_))
  stat_fun <- function(m) sqrt(sum(colMeans(m, na.rm = TRUE)^2))
  if (n == 1) return(rep(stat_fun(dif_mat), 2))
  n_boot <- monitora_stat_iteracoes_efetivas(n_boot, tipo = "boot", etapa = "bootstrap_composicao", risco = "alto", objeto = dif_mat)
  vals <- numeric(n_boot)
  chunk <- min(as.integer(MONITORA_STAT_PERM_CHUNK), n_boot)
  feitas <- 0L
  while (feitas < n_boot) {
    k <- min(chunk, n_boot - feitas)
    for (bb in seq_len(k)) {
      idx <- sample(seq_len(n), size = n, replace = TRUE)
      vals[feitas + bb] <- stat_fun(dif_mat[idx, , drop = FALSE])
    }
    feitas <- feitas + k
  }
  stats::quantile(vals, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE, names = FALSE)
}

monitora_stat_p_permutacao_multivariada_pareada <- function(dif_mat, n_perm = MONITORA_STAT_PERM) {
  dif_mat <- as.matrix(dif_mat)
  dif_mat[!is.finite(dif_mat)] <- 0
  n <- nrow(dif_mat)
  if (n == 0) return(NA_real_)
  stat_fun <- function(m) sqrt(sum(colMeans(m, na.rm = TRUE)^2))
  obs <- stat_fun(dif_mat)
  if (!is.finite(obs)) return(NA_real_)
  if (obs < .Machine$double.eps^0.5) return(1)

  if (n <= 16) {
    grade <- expand.grid(rep(list(c(-1, 1)), n))
    stats_perm <- apply(as.matrix(grade), 1, function(s) stat_fun(dif_mat * s))
    return(mean(stats_perm >= obs))
  }

  n_perm <- monitora_stat_iteracoes_efetivas(n_perm, tipo = "perm", etapa = "permutacao_composicao", risco = "alto", objeto = dif_mat)
  cont <- 0L
  chunk <- min(as.integer(MONITORA_STAT_PERM_CHUNK), n_perm)
  feitas <- 0L
  while (feitas < n_perm) {
    k <- min(chunk, n_perm - feitas)
    stats_perm <- numeric(k)
    for (pp in seq_len(k)) {
      sinais <- sample(c(-1, 1), n, replace = TRUE)
      stats_perm[pp] <- stat_fun(dif_mat * sinais)
    }
    cont <- cont + sum(stats_perm >= obs)
    feitas <- feitas + k
  }
  (cont + 1) / (n_perm + 1)
}

monitora_stat_composicao_adjacent <- function(long_dt) {
  # Compara a composição geral com a medição anterior disponível dentro de cada UC.
  # O ano-alvo agrupa as UCs que tenham alguma medição anterior válida, permitindo
  # uso consistente tanto com uma única UC quanto com múltiplas UCs.
  if (is.null(long_dt) || nrow(long_dt) == 0) return(data.table::data.table())
  long_dt <- data.table::as.data.table(long_dt)

  out <- vector("list", 0)
  idx_out <- 0L
  grupos <- unique(long_dt[, .(grupo_grafico, tipo_metrica, form_veg)])
  for (ii in seq_len(nrow(grupos))) {
    g <- grupos[ii]
    sub <- long_dt[
      grupo_grafico == g$grupo_grafico &
        tipo_metrica == g$tipo_metrica &
        form_veg == g$form_veg
    ]
    categorias <- sort(unique(as.character(sub$categoria)))
    if (length(categorias) < 2) next

    wide <- data.table::dcast(
      sub[, .(UC, UA, ANO, categoria, valor)],
      UC + UA + ANO ~ categoria,
      value.var = "valor",
      fun.aggregate = mean,
      fill = 0
    )
    cats_presentes <- intersect(categorias, names(wide))
    if (length(cats_presentes) < 2) next

    anos_uc <- unique(wide[, .(UC, ANO)])
    anos_uc <- anos_uc[!is.na(UC) & !is.na(ANO)]
    data.table::setorder(anos_uc, UC, ANO)
    anos_uc[, ano_1 := data.table::shift(ANO, type = "lag"), by = UC]
    pares_uc <- anos_uc[!is.na(ano_1), .(UC, ano_1 = as.integer(ano_1), ano_2 = as.integer(ANO))]
    if (nrow(pares_uc) == 0) next

    for (y2 in sort(unique(pares_uc$ano_2))) {
      pares_y <- pares_uc[ano_2 == y2]
      par_lista <- vector("list", nrow(pares_y))
      for (jj in seq_len(nrow(pares_y))) {
        u <- pares_y$UC[jj]
        y1 <- pares_y$ano_1[jj]
        a <- data.table::copy(wide[UC == u & ANO == y1, c("UC", "UA", cats_presentes), with = FALSE])
        b <- data.table::copy(wide[UC == u & ANO == y2, c("UC", "UA", cats_presentes), with = FALSE])
        if (!nrow(a) || !nrow(b)) next
        data.table::setnames(a, cats_presentes, paste0(cats_presentes, "_1"))
        data.table::setnames(b, cats_presentes, paste0(cats_presentes, "_2"))
        data.table::setkeyv(a, c("UC", "UA"))
        data.table::setkeyv(b, c("UC", "UA"))
        par_lista[[jj]] <- b[a, nomatch = 0L][, `:=`(ano_1_uc = y1, ano_2 = y2)]
      }
      par <- data.table::rbindlist(par_lista, fill = TRUE)
      n_pares <- nrow(par)
      cols_1 <- paste0(cats_presentes, "_1")
      cols_2 <- paste0(cats_presentes, "_2")
      if (n_pares > 0) {
        x1_raw <- as.matrix(par[, cols_1, with = FALSE])
        x2_raw <- as.matrix(par[, cols_2, with = FALSE])
        x1_rel <- monitora_stat_padronizar_linhas(x1_raw)
        x2_rel <- monitora_stat_padronizar_linhas(x2_raw)
        x1 <- sqrt(x1_rel)
        x2 <- sqrt(x2_rel)
        dif_mat <- x2 - x1
        dist_centroid <- sqrt(sum(colMeans(dif_mat, na.rm = TRUE)^2))
        bray_medio <- monitora_stat_dist_bray_media(x1_rel, x2_rel)
      } else {
        dif_mat <- matrix(numeric(0), nrow = 0, ncol = length(cats_presentes))
        dist_centroid <- NA_real_
        bray_medio <- NA_real_
      }
      ci <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_composicao_boot_ci(dif_mat) else c(NA_real_, NA_real_)
      p_val <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_p_permutacao_multivariada_pareada(dif_mat) else NA_real_
      idx_out <- idx_out + 1L
      out[[idx_out]] <- data.table::data.table(
        grupo_grafico = g$grupo_grafico,
        tipo_metrica = g$tipo_metrica,
        form_veg = g$form_veg,
        ano_1 = if (n_pares > 0) min(par$ano_1_uc, na.rm = TRUE) else min(pares_y$ano_1, na.rm = TRUE),
        ano_2 = y2,
        anos_referencia = paste(sort(unique(pares_y$ano_1)), collapse = ";"),
        comparacao = "composicao_medicao_anterior_disponivel_por_UC",
        n_UC_pareadas = uniqueN(par$UC),
        n_UA_pareadas = n_pares,
        n_categorias = length(cats_presentes),
        categorias_incluidas = paste(cats_presentes, collapse = ";"),
        distancia_centroide_hellinger = dist_centroid,
        ci95_lower_dist_hellinger = ci[1],
        ci95_upper_dist_hellinger = ci[2],
        bray_curtis_medio_pareado = bray_medio,
        p_valor_perm_multivariado = p_val
      )
    }
  }

  res <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(res) == 0) return(res)
  res[, p_ajustado_fdr_composicao := stats::p.adjust(p_valor_perm_multivariado, method = "BH"), by = .(grupo_grafico, tipo_metrica)]
  res[, classe_mudanca_composicao := data.table::fcase(
    is.na(p_valor_perm_multivariado) | n_UA_pareadas < MONITORA_STAT_MIN_PARES, "pares_insuficientes",
    p_ajustado_fdr_composicao <= MONITORA_STAT_ALPHA & distancia_centroide_hellinger >= MONITORA_STAT_COMP_MIN_DIST, "mudanca_composicao",
    ci95_upper_dist_hellinger <= MONITORA_STAT_COMP_MARGEM_DIST, "estabilidade_composicional",
    default = "inconclusivo"
  )]
  res[, simbolo_mudanca_composicao := data.table::fcase(
    classe_mudanca_composicao == "mudanca_composicao", "◆",
    classe_mudanca_composicao == "estabilidade_composicional", "◇",
    classe_mudanca_composicao == "pares_insuficientes", "?",
    default = "·"
  )]
  res[, legenda_mudanca_composicao := data.table::fcase(
    classe_mudanca_composicao == "mudanca_composicao", "mudança estatisticamente sustentada na composição geral em relação à medição anterior disponível da mesma UC",
    classe_mudanca_composicao == "estabilidade_composicional", paste0("composição geral dentro da margem multivariada de ", MONITORA_STAT_COMP_MARGEM_DIST),
    classe_mudanca_composicao == "pares_insuficientes", paste0("menos de ", MONITORA_STAT_MIN_PARES, " UAs pareadas para composição geral"),
    default = "resultado inconclusivo para composição geral"
  )]
  data.table::setorder(res, grupo_grafico, tipo_metrica, form_veg, ano_2)
  res[]
}


monitora_stat_composicao_linha_base <- function(long_dt) {
  # Compara a composição geral com a linha de base acumulada anterior.
  if (is.null(long_dt) || nrow(long_dt) == 0 || !isTRUE(MONITORA_STAT_BASELINE_ATIVO)) return(data.table::data.table())
  long_dt <- data.table::as.data.table(long_dt)
  out <- vector("list", 0)
  idx_out <- 0L
  grupos <- unique(long_dt[, .(grupo_grafico, tipo_metrica, form_veg)])
  for (ii in seq_len(nrow(grupos))) {
    g <- grupos[ii]
    sub <- long_dt[
      grupo_grafico == g$grupo_grafico &
        tipo_metrica == g$tipo_metrica &
        form_veg == g$form_veg
    ]
    categorias <- sort(unique(as.character(sub$categoria)))
    if (length(categorias) < 2) next
    wide <- data.table::dcast(
      sub[, .(UC, UA, ANO, categoria, valor)],
      UC + UA + ANO ~ categoria,
      value.var = "valor",
      fun.aggregate = mean,
      fill = 0
    )
    cats_presentes <- intersect(categorias, names(wide))
    if (length(cats_presentes) < 2) next
    anos <- sort(unique(as.integer(wide$ANO)))
    if (length(anos) < MONITORA_STAT_BASELINE_MIN_ANOS + 1L) next
    for (y2 in anos) {
      base_anos <- anos[anos < y2]
      n_anos_base <- length(base_anos)
      atual <- data.table::copy(wide[ANO == y2, c("UC", "UA", cats_presentes), with = FALSE])
      if (!nrow(atual)) next
      if (n_anos_base >= MONITORA_STAT_BASELINE_MIN_ANOS) {
        base <- wide[ANO < y2, lapply(.SD, mean, na.rm = TRUE), by = .(UC, UA), .SDcols = cats_presentes]
        data.table::setnames(atual, cats_presentes, paste0(cats_presentes, "_2"))
        data.table::setnames(base, cats_presentes, paste0(cats_presentes, "_1"))
        data.table::setkeyv(atual, c("UC", "UA"))
        data.table::setkeyv(base, c("UC", "UA"))
        par <- base[atual, nomatch = 0L]
      } else {
        par <- data.table::data.table()
      }
      n_pares <- nrow(par)
      cols_1 <- paste0(cats_presentes, "_1")
      cols_2 <- paste0(cats_presentes, "_2")
      if (n_pares > 0) {
        x1_raw <- as.matrix(par[, cols_1, with = FALSE])
        x2_raw <- as.matrix(par[, cols_2, with = FALSE])
        x1_rel <- monitora_stat_padronizar_linhas(x1_raw)
        x2_rel <- monitora_stat_padronizar_linhas(x2_raw)
        x1 <- sqrt(x1_rel)
        x2 <- sqrt(x2_rel)
        dif_mat <- x2 - x1
        dist_centroid <- sqrt(sum(colMeans(dif_mat, na.rm = TRUE)^2))
        bray_medio <- monitora_stat_dist_bray_media(x1_rel, x2_rel)
      } else {
        dif_mat <- matrix(numeric(0), nrow = 0, ncol = length(cats_presentes))
        dist_centroid <- NA_real_
        bray_medio <- NA_real_
      }
      testavel <- n_pares >= MONITORA_STAT_MIN_PARES && n_anos_base >= MONITORA_STAT_BASELINE_MIN_ANOS
      ci <- if (testavel) monitora_stat_composicao_boot_ci(dif_mat) else c(NA_real_, NA_real_)
      p_val <- if (testavel) monitora_stat_p_permutacao_multivariada_pareada(dif_mat) else NA_real_
      idx_out <- idx_out + 1L
      out[[idx_out]] <- data.table::data.table(
        grupo_grafico = g$grupo_grafico,
        tipo_metrica = g$tipo_metrica,
        form_veg = g$form_veg,
        ano_2 = y2,
        anos_linha_base = paste(base_anos, collapse = ";"),
        n_anos_linha_base = n_anos_base,
        modo_linha_base = MONITORA_STAT_BASELINE_MODO,
        n_UC_pareadas = uniqueN(par$UC),
        n_UA_pareadas = n_pares,
        n_categorias = length(cats_presentes),
        categorias_incluidas = paste(cats_presentes, collapse = ";"),
        distancia_centroide_hellinger = dist_centroid,
        ci95_lower_dist_hellinger = ci[1],
        ci95_upper_dist_hellinger = ci[2],
        bray_curtis_medio_pareado = bray_medio,
        p_valor_perm_multivariado = p_val
      )
    }
  }
  res <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(res) == 0) return(res)
  res[, p_ajustado_fdr_composicao_linha_base := stats::p.adjust(p_valor_perm_multivariado, method = "BH"), by = .(grupo_grafico, tipo_metrica)]
  res[, classe_mudanca_composicao_linha_base := data.table::fcase(
    is.na(p_valor_perm_multivariado) | n_UA_pareadas < MONITORA_STAT_MIN_PARES | n_anos_linha_base < MONITORA_STAT_BASELINE_MIN_ANOS, "pares_insuficientes",
    p_ajustado_fdr_composicao_linha_base <= MONITORA_STAT_ALPHA & distancia_centroide_hellinger >= MONITORA_STAT_COMP_MIN_DIST, "mudanca_composicao",
    ci95_upper_dist_hellinger <= MONITORA_STAT_COMP_MARGEM_DIST, "estabilidade_composicional",
    default = "inconclusivo"
  )]
  res[, simbolo_mudanca_composicao_linha_base := data.table::fcase(
    classe_mudanca_composicao_linha_base == "mudanca_composicao", "◈",
    default = ""
  )]
  res[, legenda_mudanca_composicao_linha_base := data.table::fcase(
    classe_mudanca_composicao_linha_base == "mudanca_composicao", "mudança estatisticamente sustentada na composição geral em relação à linha de base acumulada anterior",
    classe_mudanca_composicao_linha_base == "estabilidade_composicional", paste0("composição geral dentro da margem multivariada de ", MONITORA_STAT_COMP_MARGEM_DIST, " em relação à linha de base"),
    classe_mudanca_composicao_linha_base == "pares_insuficientes", paste0("menos de ", MONITORA_STAT_MIN_PARES, " UAs pareadas ou menos de ", MONITORA_STAT_BASELINE_MIN_ANOS, " anos de linha de base para composição"),
    default = "resultado inconclusivo para composição geral em relação à linha de base"
  )]
  data.table::setorder(res, grupo_grafico, tipo_metrica, form_veg, ano_2)
  res[]
}


monitora_stat_anexar_composicao_auxiliar <- function(aux, grupo_grafico, tipo_metrica) {
  if (!exists("MONITORA_STAT_COMPOSICAO_GERAL") || is.null(aux) || !is.data.frame(aux)) return(aux)
  out <- data.table::as.data.table(data.table::copy(aux))
  comp <- MONITORA_STAT_COMPOSICAO_GERAL[
    MONITORA_STAT_COMPOSICAO_GERAL[["grupo_grafico"]] == grupo_grafico &
      MONITORA_STAT_COMPOSICAO_GERAL[["tipo_metrica"]] == tipo_metrica
  ]
  if (nrow(comp) == 0 || nrow(out) == 0) return(out)
  comp_join <- comp[, .(
    form_veg,
    ANO = as.character(ano_2),
    ano_comparacao_anterior_composicao = ano_1,
    n_UA_pareadas_composicao = n_UA_pareadas,
    n_categorias_composicao = n_categorias,
    distancia_centroide_hellinger,
    ci95_lower_dist_hellinger,
    ci95_upper_dist_hellinger,
    bray_curtis_medio_pareado,
    p_valor_perm_multivariado,
    p_ajustado_fdr_composicao,
    classe_mudanca_composicao,
    simbolo_mudanca_composicao,
    legenda_mudanca_composicao
  )]
  if ("ANO" %in% names(out)) out[, ANO := as.character(ANO)]
  if ("ANO" %in% names(comp_join)) comp_join[, ANO := as.character(ANO)]
  if ("form_veg" %in% names(out)) out[, form_veg := as.character(form_veg)]
  if ("form_veg" %in% names(comp_join)) comp_join[, form_veg := as.character(form_veg)]
  comp_join <- monitora_dt_unique_por_chaves(comp_join, c("ANO", "form_veg"), contexto = "estatistica de composicao geral")
  data.table::setkeyv(out, c("ANO", "form_veg"))
  data.table::setkeyv(comp_join, c("ANO", "form_veg"))
  out <- comp_join[out]
  out[is.na(simbolo_mudanca_composicao), `:=`(
    ano_comparacao_anterior_composicao = NA_integer_,
    classe_mudanca_composicao = "primeiro_ano_ou_sem_comparacao",
    simbolo_mudanca_composicao = "",
    legenda_mudanca_composicao = "sem ano anterior pareado para comparação de composição geral"
  )]
  out[]
}


monitora_stat_tokenizar_caption_semantica <- function(texto) {
  # Decompõe a legenda inferior em unidades semânticas que não devem ser separadas.
  # Exemplos preservados: "▲ aumento", "▼ redução", "≈ estabilidade" e "? <5 UAs".
  if (is.null(texto) || !nzchar(as.character(texto))) return(list())
  linhas <- unlist(strsplit(as.character(texto), "
", fixed = TRUE), use.names = FALSE)
  linhas <- trimws(linhas)
  linhas <- linhas[nzchar(linhas)]
  if (!length(linhas)) return(list())

  lapply(linhas, function(linha) {
    if (!grepl(":", linha, fixed = TRUE)) return(linha)
    partes <- strsplit(linha, ":", fixed = TRUE)[[1]]
    prefixo <- paste0(partes[1], ":")
    resto <- trimws(paste(partes[-1], collapse = ":"))
    if (!nzchar(resto)) return(prefixo)
    itens <- unlist(strsplit(resto, ";\\s*", perl = TRUE), use.names = FALSE)
    itens <- trimws(itens)
    itens <- itens[nzchar(itens)]
    if (!length(itens)) return(prefixo)
    if (length(itens) > 1L) itens[-length(itens)] <- paste0(itens[-length(itens)], ";")
    c(prefixo, itens)
  })
}

monitora_stat_medir_texto_in <- function(texto, gp) {
  if (is.null(texto) || !nzchar(as.character(texto))) return(0)
  grob_txt <- grid::textGrob(label = as.character(texto), gp = gp)
  as.numeric(grid::convertWidth(grid::grobWidth(grob_txt), "in", valueOnly = TRUE))
}

monitora_stat_quebrar_tokens_por_largura_in <- function(tokens, largura_max_in, gp) {
  tokens <- trimws(as.character(tokens))
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(character())
  largura_max_in <- max(as.numeric(largura_max_in), 0.1)

  linhas <- character()
  linha_atual <- ""
  for (tok in tokens) {
    candidato <- if (nzchar(linha_atual)) paste(linha_atual, tok) else tok
    if (!nzchar(linha_atual) || monitora_stat_medir_texto_in(candidato, gp) <= largura_max_in) {
      linha_atual <- candidato
    } else {
      linhas <- c(linhas, linha_atual)
      linha_atual <- tok
    }
  }
  if (nzchar(linha_atual)) linhas <- c(linhas, linha_atual)
  linhas
}

monitora_stat_quebrar_caption_painel_in <- function(texto, largura_max_in, fontsize = 7.8, lineheight = 1.02) {
  # Quebra a legenda inferior usando a largura real/estimada do painel, não a largura total do plot.
  if (is.null(texto) || !nzchar(as.character(texto))) return(character())
  gp <- grid::gpar(fontsize = fontsize, lineheight = lineheight)
  paragrafos <- monitora_stat_tokenizar_caption_semantica(texto)
  linhas <- unlist(lapply(paragrafos, monitora_stat_quebrar_tokens_por_largura_in, largura_max_in = largura_max_in, gp = gp), use.names = FALSE)
  linhas[nzchar(trimws(linhas))]
}

monitora_stat_caption_append <- function(plot_obj, texto, largura = NULL, reformatar = FALSE) {
  # A renderização final da legenda inferior é feita como grob alinhado ao painel.
  # Aqui o texto é apenas armazenado em labs(caption), sem pré-quebra artificial por caracteres.
  if (is.null(texto) || !nzchar(texto)) return(plot_obj)
  cap_atual <- plot_obj$labels$caption
  if (is.null(cap_atual) || !nzchar(as.character(cap_atual))) {
    cap_novo <- texto
  } else if (grepl(texto, as.character(cap_atual), fixed = TRUE)) {
    cap_novo <- as.character(cap_atual)
  } else {
    cap_novo <- paste(as.character(cap_atual), texto, sep = "
")
  }
  plot_obj +
    ggplot2::labs(caption = cap_novo) +
    ggplot2::theme(
      plot.caption = ggplot2::element_blank(),
      plot.caption.position = "panel",
      plot.margin = ggplot2::margin(t = 16, r = 24, b = 24, l = 24)
    )
}

monitora_stat_caption_chave_mudancas <- function(plot_obj, incluir_categoria = TRUE, incluir_composicao = TRUE, incluir_linha_base = FALSE) {
  # Legenda estatística sucinta para os gráficos com rótulos.
  # A quebra final é calculada depois, com base no comprimento do eixo X/painel.
  segmentos <- c("Símbolos estatísticos (UA/transecto pareada).")
  if (isTRUE(incluir_categoria)) {
    segmentos <- c(segmentos, "Categoria vs. medição anterior: ▲ aumento; ▼ redução; ≈ estabilidade; · inconclusivo; ? <5 UAs.")
  }
  if (isTRUE(incluir_linha_base)) {
    segmentos <- c(segmentos, "Categoria vs. linha de base acumulada: △ aumento; ▽ redução (só exibidos quando há mudança).")
  }
  if (isTRUE(incluir_composicao)) {
    segmentos <- c(segmentos, "Composição geral vs. medição anterior, ao lado do ano: ◆ mudança; ◇ estabilidade; · inconclusivo; ? <5 UAs.")
  }
  if (length(segmentos) <= 1L) return(plot_obj)
  segmentos <- c(segmentos, "Testes: permutação pareada + IC95% bootstrap; p ajustado por FDR-BH.")
  texto <- paste(segmentos, collapse = "
")
  monitora_stat_caption_append(plot_obj, texto, reformatar = FALSE)
}

MONITORA_STAT_GRUPOS_MUITAS_CATEGORIAS <- c(
  "formas_vida_nativas",
  "formas_vida_exoticas",
  "formas_vida_secas_mortas"
)

monitora_stat_plot_com_rotulo <- function(nome_plot) {
  # As anotações estatísticas são editoriais e só devem aparecer nos gráficos com rótulos.
  grepl("_com_rotulo", as.character(nome_plot), fixed = TRUE)
}

monitora_stat_grupo_muitas_categorias <- function(grupo_grafico) {
  as.character(grupo_grafico) %in% MONITORA_STAT_GRUPOS_MUITAS_CATEGORIAS
}

monitora_stat_filtrar_simbolos_editorial <- function(dados, grupo_grafico) {
  # Regra editorial: se o rótulo é exibido, o símbolo estatístico correspondente
  # também deve ser exibido. Portanto, não filtramos símbolos por densidade do gráfico.
  if (is.null(dados) || !data.table::is.data.table(dados) || !nrow(dados)) return(dados)
  dados[]
}

monitora_stat_preparar_dados_composicao_plot <- function(dados_plot, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (!exists("MONITORA_STAT_COMPOSICAO_GERAL") || is.null(dados_plot) || !is.data.frame(dados_plot)) {
    return(data.table::data.table())
  }
  dados <- data.table::as.data.table(data.table::copy(dados_plot))
  if (!nrow(dados) || !"ANO" %in% names(dados)) return(data.table::data.table())

  anos_plot <- unique(as.character(dados$ANO))
  forms_plot <- if ("form_veg" %in% names(dados)) unique(as.character(dados$form_veg)) else character()
  if (!is.null(form_veg)) forms_plot <- as.character(form_veg)

  comp <- MONITORA_STAT_COMPOSICAO_GERAL[
    MONITORA_STAT_COMPOSICAO_GERAL[["grupo_grafico"]] == grupo_grafico &
      MONITORA_STAT_COMPOSICAO_GERAL[["tipo_metrica"]] == tipo_metrica
  ]
  if (length(forms_plot)) {
    comp <- comp[as.character(form_veg) %in% forms_plot]
  }
  comp <- comp[as.character(ano_2) %in% anos_plot]
  comp <- comp[!is.na(simbolo_mudanca_composicao) & nzchar(simbolo_mudanca_composicao)]
  if (!nrow(comp)) return(data.table::data.table())

  comp <- comp[, .(
    ANO = as.character(ano_2),
    form_veg = as.character(form_veg),
    simbolo_mudanca_composicao = as.character(simbolo_mudanca_composicao),
    classe_mudanca_composicao = as.character(classe_mudanca_composicao),
    legenda_mudanca_composicao = as.character(legenda_mudanca_composicao)
  )]
  comp <- monitora_dt_unique_por_chaves(comp, c("ANO", "form_veg"), contexto = "simbolos de composicao por ano/formacao")
  comp[]
}

monitora_stat_adicionar_simbolos_composicao_prop <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_composicao_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (!nrow(dados)) return(plot_obj)
  if ("ANO_factor_rotulo" %in% names(plot_obj$data)) {
    mapa_anos <- unique(data.table::as.data.table(plot_obj$data)[, .(ANO = as.character(ANO), ANO_factor_rotulo = ANO_factor_rotulo)])
    dados[, ANO := as.character(ANO)]
    dados <- merge(dados, mapa_anos, by = "ANO", all.x = TRUE)
    if (is.numeric(mapa_anos$ANO_factor_rotulo)) {
      dados[, y_simbolo_composicao := as.numeric(ANO_factor_rotulo)]
    } else {
      dados[, y_simbolo_composicao := factor(ANO_factor_rotulo, levels = unique(mapa_anos$ANO_factor_rotulo))]
    }
  } else {
    dados[, y_simbolo_composicao := factor(ANO, levels = unique(as.character(plot_obj$data$ANO)))]
  }
  dados[, x_simbolo_composicao := -0.018]
  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_composicao, y = y_simbolo_composicao, label = simbolo_mudanca_composicao),
      inherit.aes = FALSE,
      size = MONITORA_FONTE_ROTULO_PROP,
      fontface = "bold",
      color = "black",
      hjust = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(xlim = c(-0.03, 1.02), clip = "off")
}

monitora_stat_adicionar_simbolos_composicao_cobertura <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_composicao_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (!nrow(dados)) return(plot_obj)
  dados_base <- data.table::as.data.table(data.table::copy(plot_obj$data))
  x_max <- suppressWarnings(max(as.numeric(dados_base$veg_cover), na.rm = TRUE))
  if (!is.finite(x_max) || x_max <= 0) x_max <- 100
  dados[, y_simbolo_composicao := factor(ANO)]
  dados[, x_simbolo_composicao := -x_max * 0.035]
  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_composicao, y = y_simbolo_composicao, label = simbolo_mudanca_composicao),
      inherit.aes = FALSE,
      size = MONITORA_FONTE_ROTULO_COB * 0.78,
      fontface = "bold",
      color = "black",
      hjust = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(xlim = c(-x_max * 0.06, x_max * 1.10), clip = "off")
}

monitora_stat_anotar_grafico_composicao <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (tipo_metrica == "proporcao_relativa") {
    return(monitora_stat_adicionar_simbolos_composicao_prop(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  if (tipo_metrica == "cobertura") {
    return(monitora_stat_adicionar_simbolos_composicao_cobertura(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  plot_obj
}

monitora_stat_preparar_dados_simbolos_plot <- function(dados_plot, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (!exists("MONITORA_STAT_MUDANCA_ANO_A_ANO") || is.null(dados_plot) || !is.data.frame(dados_plot)) {
    return(data.table::data.table())
  }
  dados <- data.table::as.data.table(data.table::copy(dados_plot))
  if (nrow(dados) == 0 || !all(c("ANO", "form_veg", "categoria") %in% names(dados))) {
    return(data.table::data.table())
  }

  stat <- MONITORA_STAT_MUDANCA_ANO_A_ANO[
    MONITORA_STAT_MUDANCA_ANO_A_ANO[["grupo_grafico"]] == grupo_grafico &
      MONITORA_STAT_MUDANCA_ANO_A_ANO[["tipo_metrica"]] == tipo_metrica
  ]
  if (nrow(stat) == 0) return(data.table::data.table())

  dados[, `:=`(
    ANO_JOIN = as.character(ANO),
    form_veg_JOIN = as.character(form_veg),
    categoria_JOIN = as.character(categoria)
  )]
  stat_join <- stat[, .(
    ANO_JOIN = as.character(ano_2),
    form_veg_JOIN = as.character(form_veg),
    categoria_JOIN = as.character(categoria),
    categoria_label_stat = as.character(categoria_label),
    simbolo_mudanca = as.character(simbolo_mudanca),
    classe_mudanca = as.character(classe_mudanca),
    legenda_mudanca = as.character(legenda_mudanca)
  )]
  if (!is.null(form_veg)) {
    dados <- dados[form_veg_JOIN == as.character(form_veg)]
    stat_join <- stat_join[form_veg_JOIN == as.character(form_veg)]
  }
  chaves_simbolo <- c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN")
  dados <- monitora_dt_unique_por_chaves(dados, chaves_simbolo, contexto = "dados do grafico para simbolos")
  stat_join <- monitora_dt_unique_por_chaves(stat_join, chaves_simbolo, contexto = "estatistica por categoria para simbolos")
  data.table::setkeyv(dados, chaves_simbolo)
  data.table::setkeyv(stat_join, chaves_simbolo)
  out <- stat_join[dados, nomatch = 0L]
  out <- out[!is.na(simbolo_mudanca) & nzchar(simbolo_mudanca)]

  if (nrow(out) == 0L && "categoria_label" %in% names(dados) && "categoria_label_stat" %in% names(stat_join)) {
    dados_lab <- data.table::copy(dados)
    stat_lab <- data.table::copy(stat_join)
    dados_lab[, categoria_label_JOIN := as.character(categoria_label)]
    stat_lab[, categoria_label_JOIN := as.character(categoria_label_stat)]
    chaves_lab <- c("ANO_JOIN", "form_veg_JOIN", "categoria_label_JOIN")
    dados_lab <- monitora_dt_unique_por_chaves(dados_lab, chaves_lab, contexto = "dados do grafico para simbolos por label")
    stat_lab <- monitora_dt_unique_por_chaves(stat_lab, chaves_lab, contexto = "estatistica para simbolos por label")
    data.table::setkeyv(dados_lab, chaves_lab)
    data.table::setkeyv(stat_lab, chaves_lab)
    out <- stat_lab[dados_lab, nomatch = 0L]
    out <- out[!is.na(simbolo_mudanca) & nzchar(simbolo_mudanca)]
  }
  out <- monitora_stat_filtrar_simbolos_editorial(out, grupo_grafico)
  out[]
}

monitora_stat_adicionar_simbolos_prop <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_simbolos_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(dados) == 0 || !"prop" %in% names(dados)) return(plot_obj)
  dados[, prop_num_stat_plot := suppressWarnings(as.numeric(prop))]
  dados <- dados[is.finite(prop_num_stat_plot) & prop_num_stat_plot > 0]
  if (nrow(dados) == 0) return(plot_obj)

  if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) {
    # Para gráficos densos, posicionar apenas mudanças reais no centro do segmento observado.
    if (!"ordem_categoria" %in% names(dados)) dados[, ordem_categoria := data.table::frank(categoria_label, ties.method = "dense")]
    data.table::setorder(dados, ANO_JOIN, form_veg_JOIN, -ordem_categoria, categoria_JOIN)
    dados[, x_fim_stat_plot := cumsum(prop_num_stat_plot), by = .(ANO_JOIN, form_veg_JOIN)]
    dados[, x_simbolo_mudanca := x_fim_stat_plot - prop_num_stat_plot / 2]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.72
    vjust_simbolo <- -1.25
  } else if ("x_meio_rotulo" %in% names(dados)) {
    dados[, x_simbolo_mudanca := suppressWarnings(as.numeric(x_meio_rotulo))]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.78
    vjust_simbolo <- -1.65
  } else {
    if (!"ordem_categoria" %in% names(dados)) dados[, ordem_categoria := data.table::frank(categoria_label, ties.method = "dense")]
    data.table::setorder(dados, ANO_JOIN, form_veg_JOIN, -ordem_categoria, categoria_JOIN)
    dados[, x_fim_stat_plot := cumsum(prop_num_stat_plot), by = .(ANO_JOIN, form_veg_JOIN)]
    dados[, x_simbolo_mudanca := x_fim_stat_plot - prop_num_stat_plot / 2]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.78
    vjust_simbolo <- -1.65
  }
  dados[, y_simbolo_mudanca := if ("ANO_factor_rotulo" %in% names(dados)) ANO_factor_rotulo else ANO]

  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_mudanca, y = y_simbolo_mudanca, label = simbolo_mudanca),
      inherit.aes = FALSE,
      size = tamanho,
      fontface = "bold",
      color = "black",
      vjust = vjust_simbolo,
      show.legend = FALSE
    )
}

monitora_stat_adicionar_simbolos_cobertura <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_simbolos_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(dados) == 0 || !"veg_cover" %in% names(dados)) return(plot_obj)
  dados[, veg_cover_num_stat_plot := suppressWarnings(as.numeric(veg_cover))]
  dados <- dados[is.finite(veg_cover_num_stat_plot)]
  if (nrow(dados) == 0) return(plot_obj)

  desloc_x <- max(dados$veg_cover_num_stat_plot, na.rm = TRUE) * if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) 0.035 else 0.055
  if (!is.finite(desloc_x) || desloc_x <= 0) desloc_x <- 0.8
  dados[, x_simbolo_mudanca := veg_cover_num_stat_plot + desloc_x]
  dados[, y_simbolo_mudanca := factor(ANO)]
  tamanho <- if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) MONITORA_FONTE_ROTULO_COB * 0.72 else MONITORA_FONTE_ROTULO_COB * 0.82

  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_mudanca, y = y_simbolo_mudanca, group = categoria_label, label = simbolo_mudanca),
      inherit.aes = FALSE,
      position = ggplot2::position_dodge(width = 0.7),
      size = tamanho,
      fontface = "bold",
      color = "black",
      hjust = 0,
      show.legend = FALSE
    )
}

monitora_stat_anotar_grafico_categoria <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (tipo_metrica == "proporcao_relativa") {
    return(monitora_stat_adicionar_simbolos_prop(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  if (tipo_metrica == "cobertura") {
    return(monitora_stat_adicionar_simbolos_cobertura(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  plot_obj
}


monitora_stat_preparar_dados_linha_base_plot <- function(dados_plot, grupo_grafico, tipo_metrica, form_veg = NULL) {
  # Símbolos de linha de base são editoriais e aparecem apenas quando há mudança
  # sustentada em relação à linha de base acumulada anterior.
  if (!exists("MONITORA_STAT_MUDANCA_LINHA_BASE") || is.null(dados_plot) || !is.data.frame(dados_plot)) {
    return(data.table::data.table())
  }
  dados <- data.table::as.data.table(data.table::copy(dados_plot))
  if (nrow(dados) == 0 || !all(c("ANO", "form_veg", "categoria") %in% names(dados))) {
    return(data.table::data.table())
  }
  stat <- MONITORA_STAT_MUDANCA_LINHA_BASE[
    MONITORA_STAT_MUDANCA_LINHA_BASE[["grupo_grafico"]] == grupo_grafico &
      MONITORA_STAT_MUDANCA_LINHA_BASE[["tipo_metrica"]] == tipo_metrica
  ]
  if (nrow(stat) == 0) return(data.table::data.table())
  # Regra solicitada: explicitar símbolo de linha de base somente quando houver mudança.
  stat <- stat[classe_mudanca %in% c("aumento", "reducao")]
  if (nrow(stat) == 0) return(data.table::data.table())

  dados[, `:=`(
    ANO_JOIN = as.character(ANO),
    form_veg_JOIN = as.character(form_veg),
    categoria_JOIN = as.character(categoria)
  )]
  stat_join <- stat[, .(
    ANO_JOIN = as.character(ano_2),
    form_veg_JOIN = as.character(form_veg),
    categoria_JOIN = as.character(categoria),
    categoria_label_stat = as.character(categoria_label),
    simbolo_linha_base = as.character(simbolo_mudanca),
    classe_mudanca_linha_base = as.character(classe_mudanca),
    legenda_linha_base = as.character(legenda_mudanca)
  )]
  if (!is.null(form_veg)) {
    dados <- dados[form_veg_JOIN == as.character(form_veg)]
    stat_join <- stat_join[form_veg_JOIN == as.character(form_veg)]
  }
  chaves <- c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN")
  dados <- monitora_dt_unique_por_chaves(dados, chaves, contexto = "dados do grafico para simbolos de linha de base")
  stat_join <- monitora_dt_unique_por_chaves(stat_join, chaves, contexto = "estatistica de linha de base para simbolos")
  data.table::setkeyv(dados, chaves)
  data.table::setkeyv(stat_join, chaves)
  out <- stat_join[dados, nomatch = 0L]
  out <- out[!is.na(simbolo_linha_base) & nzchar(simbolo_linha_base)]

  if (nrow(out) == 0L && "categoria_label" %in% names(dados) && "categoria_label_stat" %in% names(stat_join)) {
    dados_lab <- data.table::copy(dados)
    stat_lab <- data.table::copy(stat_join)
    dados_lab[, categoria_label_JOIN := as.character(categoria_label)]
    stat_lab[, categoria_label_JOIN := as.character(categoria_label_stat)]
    chaves_lab <- c("ANO_JOIN", "form_veg_JOIN", "categoria_label_JOIN")
    dados_lab <- monitora_dt_unique_por_chaves(dados_lab, chaves_lab, contexto = "dados do grafico para linha de base por label")
    stat_lab <- monitora_dt_unique_por_chaves(stat_lab, chaves_lab, contexto = "estatistica de linha de base por label")
    data.table::setkeyv(dados_lab, chaves_lab)
    data.table::setkeyv(stat_lab, chaves_lab)
    out <- stat_lab[dados_lab, nomatch = 0L]
    out <- out[!is.na(simbolo_linha_base) & nzchar(simbolo_linha_base)]
  }
  # Também nos gráficos densos a linha de base só mostra mudanças reais, então não há filtro
  # adicional.
  out[]
}

monitora_stat_contar_simbolos_linha_base_plot <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_linha_base_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (is.null(dados) || !nrow(dados)) return(0L)
  as.integer(nrow(dados[!is.na(simbolo_linha_base) & nzchar(simbolo_linha_base)]))
}

monitora_stat_adicionar_simbolos_linha_base_prop <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_linha_base_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(dados) == 0 || !"prop" %in% names(dados)) return(plot_obj)
  dados[, prop_num_stat_plot := suppressWarnings(as.numeric(prop))]
  dados <- dados[is.finite(prop_num_stat_plot) & prop_num_stat_plot > 0]
  if (nrow(dados) == 0) return(plot_obj)

  if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) {
    if (!"ordem_categoria" %in% names(dados)) dados[, ordem_categoria := data.table::frank(categoria_label, ties.method = "dense")]
    data.table::setorder(dados, ANO_JOIN, form_veg_JOIN, -ordem_categoria, categoria_JOIN)
    dados[, x_fim_stat_plot := cumsum(prop_num_stat_plot), by = .(ANO_JOIN, form_veg_JOIN)]
    dados[, x_simbolo_linha_base := x_fim_stat_plot - prop_num_stat_plot / 2]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.68
    vjust_simbolo <- -2.15
  } else if ("x_meio_rotulo" %in% names(dados)) {
    dados[, x_simbolo_linha_base := suppressWarnings(as.numeric(x_meio_rotulo))]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.72
    vjust_simbolo <- -2.55
  } else {
    if (!"ordem_categoria" %in% names(dados)) dados[, ordem_categoria := data.table::frank(categoria_label, ties.method = "dense")]
    data.table::setorder(dados, ANO_JOIN, form_veg_JOIN, -ordem_categoria, categoria_JOIN)
    dados[, x_fim_stat_plot := cumsum(prop_num_stat_plot), by = .(ANO_JOIN, form_veg_JOIN)]
    dados[, x_simbolo_linha_base := x_fim_stat_plot - prop_num_stat_plot / 2]
    tamanho <- MONITORA_FONTE_ROTULO_PROP * 0.72
    vjust_simbolo <- -2.55
  }
  dados[, y_simbolo_linha_base := if ("ANO_factor_rotulo" %in% names(dados)) ANO_factor_rotulo else ANO]

  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_linha_base, y = y_simbolo_linha_base, label = simbolo_linha_base),
      inherit.aes = FALSE,
      size = tamanho,
      fontface = "bold",
      color = "black",
      vjust = vjust_simbolo,
      show.legend = FALSE
    )
}

monitora_stat_adicionar_simbolos_linha_base_cobertura <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_linha_base_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(dados) == 0 || !"veg_cover" %in% names(dados)) return(plot_obj)
  dados[, veg_cover_num_stat_plot := suppressWarnings(as.numeric(veg_cover))]
  dados <- dados[is.finite(veg_cover_num_stat_plot)]
  if (nrow(dados) == 0) return(plot_obj)

  desloc_x <- max(dados$veg_cover_num_stat_plot, na.rm = TRUE) * if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) 0.070 else 0.095
  if (!is.finite(desloc_x) || desloc_x <= 0) desloc_x <- 1.4
  dados[, x_simbolo_linha_base := veg_cover_num_stat_plot + desloc_x]
  dados[, y_simbolo_linha_base := factor(ANO)]
  tamanho <- if (monitora_stat_grupo_muitas_categorias(grupo_grafico)) MONITORA_FONTE_ROTULO_COB * 0.68 else MONITORA_FONTE_ROTULO_COB * 0.76

  plot_obj +
    ggplot2::geom_text(
      data = dados,
      ggplot2::aes(x = x_simbolo_linha_base, y = y_simbolo_linha_base, group = categoria_label, label = simbolo_linha_base),
      inherit.aes = FALSE,
      position = ggplot2::position_dodge(width = 0.7),
      size = tamanho,
      fontface = "bold",
      color = "black",
      hjust = 0,
      show.legend = FALSE
    )
}

monitora_stat_anotar_grafico_linha_base <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (tipo_metrica == "proporcao_relativa") {
    return(monitora_stat_adicionar_simbolos_linha_base_prop(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  if (tipo_metrica == "cobertura") {
    return(monitora_stat_adicionar_simbolos_linha_base_cobertura(plot_obj, grupo_grafico, tipo_metrica, form_veg))
  }
  plot_obj
}


monitora_stat_metadados_plot <- function(nome_plot) {
  # Inferência padronizada do grupo, métrica e formação a partir do nome do objeto/arquivo do
  # gráfico.
  # Esta função é usada imediatamente antes do ggsave(), garantindo que os símbolos
  # sejam aplicados ao objeto efetivamente exportado.
  nome_plot <- as.character(nome_plot)
  mapa <- data.table::data.table(
    prefixo = c("plot_p1", "plot_p2m", "plot_p2", "plot_p3", "plot_p4", "plot_p5"),
    grupo_grafico = c("herbaceas_lenhosas", "material_botanico", "categorias_gerais", "formas_vida_nativas", "formas_vida_exoticas", "formas_vida_secas_mortas")
  )
  mapa <- mapa[order(-nchar(prefixo))]
  hit <- mapa[vapply(prefixo, function(px) startsWith(nome_plot, px), logical(1))]
  if (nrow(hit) == 0) return(NULL)
  tipo_metrica <- if (grepl("prop_rel", nome_plot)) {
    "proporcao_relativa"
  } else if (grepl("veg_cover", nome_plot) || grepl("^plot_p5\\.3\\.2_seca_morta", nome_plot)) {
    "cobertura"
  } else {
    NA_character_
  }
  if (is.na(tipo_metrica)) return(NULL)
  form_plot <- NULL
  if (grepl("_camp_", nome_plot)) form_plot <- "Campestre"
  if (grepl("_sav_", nome_plot)) form_plot <- "Savânica"
  list(
    grupo_grafico = hit$grupo_grafico[1],
    tipo_metrica = tipo_metrica,
    form_veg = form_plot
  )
}

monitora_stat_contar_simbolos_plot <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados <- monitora_stat_preparar_dados_simbolos_plot(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (is.null(dados) || !nrow(dados)) return(0L)
  as.integer(nrow(dados[!is.na(simbolo_mudanca) & nzchar(simbolo_mudanca)]))
}

monitora_stat_prefixo_rotulo <- function(simbolo_mudanca = "", simbolo_linha_base = "") {
  simbolos <- c(as.character(simbolo_mudanca), as.character(simbolo_linha_base))
  simbolos <- simbolos[!is.na(simbolos) & nzchar(trimws(simbolos))]
  if (!length(simbolos)) return("")
  paste(simbolos, collapse = " ")
}

monitora_stat_mapa_prefixos_rotulos <- function(plot_data, grupo_grafico, tipo_metrica, form_veg = NULL) {
  dados_base <- data.table::as.data.table(data.table::copy(plot_data))
  if (!nrow(dados_base)) return(data.table::data.table())
  if (!all(c("ANO", "form_veg", "categoria") %in% names(dados_base))) return(data.table::data.table())

  mapa <- unique(dados_base[, .(
    ANO_JOIN = as.character(ANO),
    form_veg_JOIN = as.character(form_veg),
    categoria_JOIN = as.character(categoria)
  )])

  simb_cat <- monitora_stat_preparar_dados_simbolos_plot(plot_data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(simb_cat)) {
    simb_cat <- unique(simb_cat[, .(
      ANO_JOIN = as.character(ANO_JOIN),
      form_veg_JOIN = as.character(form_veg_JOIN),
      categoria_JOIN = as.character(categoria_JOIN),
      simbolo_mudanca = as.character(simbolo_mudanca)
    )])
  } else {
    simb_cat <- data.table::data.table(ANO_JOIN = character(), form_veg_JOIN = character(), categoria_JOIN = character(), simbolo_mudanca = character())
  }

  simb_lb <- monitora_stat_preparar_dados_linha_base_plot(plot_data, grupo_grafico, tipo_metrica, form_veg)
  if (nrow(simb_lb)) {
    simb_lb <- unique(simb_lb[, .(
      ANO_JOIN = as.character(ANO_JOIN),
      form_veg_JOIN = as.character(form_veg_JOIN),
      categoria_JOIN = as.character(categoria_JOIN),
      simbolo_linha_base = as.character(simbolo_linha_base)
    )])
  } else {
    simb_lb <- data.table::data.table(ANO_JOIN = character(), form_veg_JOIN = character(), categoria_JOIN = character(), simbolo_linha_base = character())
  }

  mapa <- merge(mapa, simb_cat, by = c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN"), all.x = TRUE)
  mapa <- merge(mapa, simb_lb, by = c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN"), all.x = TRUE)
  mapa[, prefixo_rotulo_stat := mapply(monitora_stat_prefixo_rotulo, simbolo_mudanca, simbolo_linha_base, USE.NAMES = FALSE)]
  mapa[]
}

monitora_stat_prefixar_colunas_rotulo <- function(dt, mapa_prefixos) {
  dados <- data.table::as.data.table(data.table::copy(dt))
  if (!nrow(dados) || !nrow(mapa_prefixos)) return(dados)
  if (!all(c("ANO", "form_veg") %in% names(dados))) return(dados)

  dados[, `:=`(ANO_JOIN = as.character(ANO), form_veg_JOIN = as.character(form_veg))]
  if ("categoria" %in% names(dados)) {
    dados[, categoria_JOIN := as.character(categoria)]
  } else if ("categoria_label" %in% names(dados)) {
    dados[, categoria_JOIN := as.character(categoria_label)]
  } else {
    return(dados)
  }

  data.table::setkeyv(dados, c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN"))
  mapa_use <- unique(mapa_prefixos[, .(ANO_JOIN, form_veg_JOIN, categoria_JOIN, prefixo_rotulo_stat)])
  data.table::setkeyv(mapa_use, c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN"))
  dados <- mapa_use[dados]

  cols_rotulo <- intersect(c("rotulo_prop_plot", "rotulo_prop_interno", "rotulo_prop_externo", "rotulo_cobertura_plot"), names(dados))
  for (col in cols_rotulo) {
    # Regra editorial: quando o rótulo for exibido e houver símbolo estatístico,
    # o símbolo deve acompanhar o rótulo. Mantê-lo em linha própria preserva a
    # leitura e evita confundir símbolo, n e percentual.
    dados[, (col) := data.table::fifelse(
      !is.na(prefixo_rotulo_stat) & nzchar(prefixo_rotulo_stat) & !is.na(get(col)) & nzchar(get(col)),
      paste0(prefixo_rotulo_stat, "\n", get(col)),
      get(col)
    )]
  }

  dados[, c("ANO_JOIN", "form_veg_JOIN", "categoria_JOIN", "prefixo_rotulo_stat") := NULL]
  dados[]
}

monitora_stat_incorporar_simbolos_rotulos_plot <- function(plot_obj, grupo_grafico, tipo_metrica, form_veg = NULL) {
  if (!inherits(plot_obj, "ggplot") || is.null(plot_obj$data) || !is.data.frame(plot_obj$data)) return(plot_obj)
  mapa_prefixos <- monitora_stat_mapa_prefixos_rotulos(plot_obj$data, grupo_grafico, tipo_metrica, form_veg)
  if (!nrow(mapa_prefixos)) return(plot_obj)

  plot_obj$data <- monitora_stat_prefixar_colunas_rotulo(plot_obj$data, mapa_prefixos)
  if (length(plot_obj$layers)) {
    for (i in seq_along(plot_obj$layers)) {
      dados_layer <- plot_obj$layers[[i]]$data
      if (is.data.frame(dados_layer) && nrow(dados_layer)) {
        plot_obj$layers[[i]]$data <- monitora_stat_prefixar_colunas_rotulo(dados_layer, mapa_prefixos)
      }
    }
  }
  plot_obj
}

monitora_stat_anotar_plot_exportacao <- function(plot_obj, nome_plot) {
  # Camada final de segurança: anota símbolos no momento da exportação.
  # Para evitar duplicidade editorial, símbolos estatísticos só são desenhados em gráficos com
  # rótulos.
  if (!inherits(plot_obj, "ggplot")) return(plot_obj)
  meta <- monitora_stat_metadados_plot(nome_plot)
  if (is.null(meta)) return(plot_obj)
  tem_rotulo <- monitora_stat_plot_com_rotulo(nome_plot)

  n_simbolos <- 0L
  n_simbolos_composicao <- 0L
  n_simbolos_linha_base <- 0L
  plot_out <- plot_obj

  if (isTRUE(tem_rotulo)) {
    if (exists("MONITORA_STAT_MUDANCA_ANO_A_ANO")) {
      n_simbolos <- monitora_stat_contar_simbolos_plot(
        plot_obj,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
    }
    if (exists("MONITORA_STAT_COMPOSICAO_GERAL")) {
      dados_comp_aud <- monitora_stat_preparar_dados_composicao_plot(
        plot_obj$data,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
      n_simbolos_composicao <- as.integer(nrow(dados_comp_aud))
    }
    if (exists("MONITORA_STAT_MUDANCA_LINHA_BASE")) {
      n_simbolos_linha_base <- monitora_stat_contar_simbolos_linha_base_plot(
        plot_obj,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
    }
    if (identical(meta$tipo_metrica, "proporcao_relativa") || identical(meta$tipo_metrica, "cobertura")) {
      # Acoplar símbolos aos rótulos garante que toda informação estatística siga a
      # mesma regra editorial do rótulo: só aparece quando o rótulo aparece, e fica
      # ancorada no mesmo bloco visual.
      plot_out <- monitora_stat_incorporar_simbolos_rotulos_plot(
        plot_out,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
    } else {
      plot_out <- monitora_stat_anotar_grafico_categoria(
        plot_out,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
      plot_out <- monitora_stat_anotar_grafico_linha_base(
        plot_out,
        meta$grupo_grafico,
        meta$tipo_metrica,
        meta$form_veg
      )
    }
    plot_out <- monitora_stat_anotar_grafico_composicao(
      plot_out,
      meta$grupo_grafico,
      meta$tipo_metrica,
      meta$form_veg
    )
    plot_out <- monitora_stat_caption_chave_mudancas(
      plot_out,
      incluir_categoria = n_simbolos > 0L,
      incluir_composicao = n_simbolos_composicao > 0L,
      incluir_linha_base = n_simbolos_linha_base > 0L
    )
  }

  if (!exists("MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS", envir = .GlobalEnv, inherits = FALSE)) {
    assign(
      "MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS",
      data.table::data.table(),
      envir = .GlobalEnv
    )
  }
  auditoria_linha <- data.table::data.table(
    plot = nome_plot,
    grupo_grafico = meta$grupo_grafico,
    tipo_metrica = meta$tipo_metrica,
    form_veg = if (is.null(meta$form_veg)) NA_character_ else meta$form_veg,
    com_rotulo = isTRUE(tem_rotulo),
    n_simbolos_categoria = n_simbolos,
    n_simbolos_linha_base = n_simbolos_linha_base,
    n_simbolos_composicao = n_simbolos_composicao,
    regra_editorial = if (isTRUE(tem_rotulo)) "simbolos_acoplados_aos_rotulos" else "sem_rotulo_sem_simbolos",
    anotado_em = "exportacao_png"
  )
  assign(
    "MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS",
    data.table::rbindlist(list(get("MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS", envir = .GlobalEnv), auditoria_linha), fill = TRUE),
    envir = .GlobalEnv
  )
  if (exists("monitora_stat_msg", mode = "function")) {
    monitora_stat_msg("plot ", nome_plot, ": ", n_simbolos, " símbolo(s) de categoria, ", n_simbolos_linha_base, " de linha de base e ", n_simbolos_composicao, " de composição preparados para exportação")
  }
  plot_out
}


monitora_stat_anexar_linha_base_auxiliar <- function(aux, grupo_grafico, tipo_metrica) {
  if (!exists("MONITORA_STAT_MUDANCA_LINHA_BASE") || is.null(aux) || !is.data.frame(aux)) return(aux)
  out <- data.table::as.data.table(data.table::copy(aux))
  stat <- MONITORA_STAT_MUDANCA_LINHA_BASE[
    MONITORA_STAT_MUDANCA_LINHA_BASE[["grupo_grafico"]] == grupo_grafico &
      MONITORA_STAT_MUDANCA_LINHA_BASE[["tipo_metrica"]] == tipo_metrica
  ]
  if (nrow(stat) == 0 || nrow(out) == 0) return(out)
  stat_join <- stat[, .(
    form_veg,
    categoria,
    ANO = as.character(ano_2),
    anos_linha_base,
    n_anos_linha_base,
    n_UA_pareadas_linha_base = n_UA_pareadas,
    diferenca_linha_base_pp = diferenca_pp,
    ci95_lower_linha_base_pp = ci95_lower_pp,
    ci95_upper_linha_base_pp = ci95_upper_pp,
    p_valor_perm_linha_base = p_valor_perm_pareado,
    p_ajustado_fdr_linha_base,
    classe_mudanca_linha_base = classe_mudanca,
    simbolo_mudanca_linha_base = simbolo_mudanca,
    legenda_mudanca_linha_base = legenda_mudanca
  )]
  if ("ANO" %in% names(out)) out[, ANO := as.character(ANO)]
  if ("form_veg" %in% names(out)) out[, form_veg := as.character(form_veg)]
  if ("categoria" %in% names(out)) out[, categoria := as.character(categoria)]
  stat_join[, `:=`(ANO = as.character(ANO), form_veg = as.character(form_veg), categoria = as.character(categoria))]
  stat_join <- monitora_dt_unique_por_chaves(stat_join, c("ANO", "form_veg", "categoria"), contexto = "estatistica de linha de base para auxiliar")
  data.table::setkeyv(out, c("ANO", "form_veg", "categoria"))
  data.table::setkeyv(stat_join, c("ANO", "form_veg", "categoria"))
  out <- stat_join[out]
  out[is.na(simbolo_mudanca_linha_base), `:=`(
    classe_mudanca_linha_base = "sem_linha_base_suficiente",
    simbolo_mudanca_linha_base = "",
    legenda_mudanca_linha_base = "sem linha de base acumulada anterior suficiente"
  )]
  out[]
}

monitora_stat_anexar_auxiliar <- function(aux, grupo_grafico, tipo_metrica) {
  if (!exists("MONITORA_STAT_MUDANCA_ANO_A_ANO") || is.null(aux) || !is.data.frame(aux)) return(aux)
  out <- data.table::as.data.table(data.table::copy(aux))
  gg <- grupo_grafico
  tm <- tipo_metrica
  stat <- MONITORA_STAT_MUDANCA_ANO_A_ANO[
    MONITORA_STAT_MUDANCA_ANO_A_ANO[["grupo_grafico"]] == gg &
      MONITORA_STAT_MUDANCA_ANO_A_ANO[["tipo_metrica"]] == tm
  ]
  if (nrow(stat) == 0 || nrow(out) == 0) return(out)
  stat_join <- stat[, .(
    form_veg,
    categoria,
    ANO = as.character(ano_2),
    ano_comparacao_anterior = ano_1,
    n_UA_pareadas,
    diferenca_pp,
    ci95_lower_pp,
    ci95_upper_pp,
    p_valor_perm_pareado,
    p_ajustado_fdr,
    classe_mudanca,
    simbolo_mudanca,
    legenda_mudanca
  )]
  if ("ANO" %in% names(out)) out[, ANO := as.character(ANO)]
  if ("ANO" %in% names(stat_join)) stat_join[, ANO := as.character(ANO)]
  if ("form_veg" %in% names(out)) out[, form_veg := as.character(form_veg)]
  if ("form_veg" %in% names(stat_join)) stat_join[, form_veg := as.character(form_veg)]
  if ("categoria" %in% names(out)) out[, categoria := as.character(categoria)]
  if ("categoria" %in% names(stat_join)) stat_join[, categoria := as.character(categoria)]
  stat_join <- monitora_dt_unique_por_chaves(stat_join, c("ANO", "form_veg", "categoria"), contexto = "estatistica por categoria para auxiliar")
  data.table::setkeyv(out, c("ANO", "form_veg", "categoria"))
  data.table::setkeyv(stat_join, c("ANO", "form_veg", "categoria"))
  out <- stat_join[out]
  out[is.na(simbolo_mudanca), `:=`(
    ano_comparacao_anterior = NA_integer_,
    classe_mudanca = "primeiro_ano_ou_sem_comparacao",
    simbolo_mudanca = "",
    legenda_mudanca = "sem ano anterior pareado para comparação"
  )]
  out <- monitora_stat_anexar_linha_base_auxiliar(out, grupo_grafico, tipo_metrica)
  out <- monitora_stat_anexar_composicao_auxiliar(out, grupo_grafico, tipo_metrica)
  data.table::setorder(out, ANO, form_veg, ordem_categoria, categoria_label)
  out[]
}

if (exists("registros_corrig_stat")) {
  monitora_stat_controlar_recursos("estatistica_inicio_preparacao_series", risco = "alto", objeto = registros_corrig_stat, force_log = TRUE)
  monitora_stat_msg("iniciando preparação das séries UA-ano")
  stat_series <- list()
  stat_series[["prop_herb_lenh"]] <- monitora_stat_long_ua(registros_corrig_stat, c("sum_herbacea", "sum_lenhosa"), "herbaceas_lenhosas", "proporcao_relativa", "relativo")
  stat_series[["cob_herb_lenh"]] <- monitora_stat_long_ua(registros_corrig_stat, c("sum_presence_herb", "sum_presence_lenh"), "herbaceas_lenhosas", "cobertura", "101")
  stat_series[["prop_categ"]] <- monitora_stat_long_ua(registros_corrig_stat, c("sum_nativa", "sum_exotica", "sum_seca_morta", "material_botanico", "solo_nu"), "categorias_gerais", "proporcao_relativa", "relativo")
  stat_series[["cob_categ"]] <- monitora_stat_long_ua(registros_corrig_stat, c("sum_presence_nativa", "sum_presence_exotica", "sum_presence_seca_morta", "material_botanico", "solo_nu"), "categorias_gerais", "cobertura", "101")
  stat_series[["prop_material"]] <- monitora_stat_long_ua(registros_corrig_stat, mat_bot_cols, "material_botanico", "proporcao_relativa", "relativo")
  stat_series[["cob_material"]] <- monitora_stat_long_ua(registros_corrig_stat, mat_bot_cols, "material_botanico", "cobertura", "101")
  stat_series[["prop_nat"]] <- monitora_stat_long_ua(registros_corrig_stat, nativa_cols, "formas_vida_nativas", "proporcao_relativa", "relativo")
  stat_series[["cob_nat"]] <- monitora_stat_long_ua(registros_corrig_stat, nativa_cols, "formas_vida_nativas", "cobertura", "101")
  stat_series[["prop_exot"]] <- monitora_stat_long_ua(registros_corrig_stat, exot_cols, "formas_vida_exoticas", "proporcao_relativa", "relativo")
  stat_series[["cob_exot"]] <- monitora_stat_long_ua(registros_corrig_stat, exot_cols, "formas_vida_exoticas", "cobertura", "101")
  stat_series[["prop_seca"]] <- monitora_stat_long_ua(registros_corrig_stat, seca_morta_cols, "formas_vida_secas_mortas", "proporcao_relativa", "relativo")
  stat_series[["cob_seca"]] <- monitora_stat_long_ua(registros_corrig_stat, seca_morta_cols, "formas_vida_secas_mortas", "cobertura", "101")

  MONITORA_STAT_SERIES_UA_ANO <- data.table::rbindlist(stat_series, fill = TRUE, use.names = TRUE)
  data.table::setkeyv(MONITORA_STAT_SERIES_UA_ANO, c("grupo_grafico", "tipo_metrica", "form_veg", "categoria", "ANO", "UC", "UA"))
  monitora_stat_controlar_recursos("estatistica_series_preparadas", risco = "alto", objeto = MONITORA_STAT_SERIES_UA_ANO, force_log = TRUE)
  monitora_stat_msg("séries preparadas: ", nrow(MONITORA_STAT_SERIES_UA_ANO), " linhas UA-categoria-ano")
  monitora_stat_msg("comparando medições disponíveis com pareamento por UC+UA")
  MONITORA_STAT_MUDANCA_ANO_A_ANO <- monitora_stat_comparar_adjacent(MONITORA_STAT_SERIES_UA_ANO)
  monitora_stat_controlar_recursos("estatistica_categoria_concluida", risco = "normal", objeto = MONITORA_STAT_MUDANCA_ANO_A_ANO, force_log = TRUE)
  monitora_stat_msg("comparando anos-alvo contra linha de base acumulada anterior")
  MONITORA_STAT_MUDANCA_LINHA_BASE <- monitora_stat_comparar_linha_base(MONITORA_STAT_SERIES_UA_ANO)
  monitora_stat_controlar_recursos("estatistica_linha_base_concluida", risco = "normal", objeto = MONITORA_STAT_MUDANCA_LINHA_BASE, force_log = TRUE)
  monitora_stat_msg("avaliando mudança na composição geral por teste multivariado pareado")
  MONITORA_STAT_COMPOSICAO_GERAL <- monitora_stat_composicao_adjacent(MONITORA_STAT_SERIES_UA_ANO)
  monitora_stat_controlar_recursos("estatistica_composicao_concluida", risco = "normal", objeto = MONITORA_STAT_COMPOSICAO_GERAL, force_log = TRUE)
  monitora_stat_msg("avaliando composição geral contra linha de base acumulada anterior")
  MONITORA_STAT_COMPOSICAO_LINHA_BASE <- monitora_stat_composicao_linha_base(MONITORA_STAT_SERIES_UA_ANO)
  monitora_stat_controlar_recursos("estatistica_composicao_linha_base_concluida", risco = "normal", objeto = MONITORA_STAT_COMPOSICAO_LINHA_BASE, force_log = TRUE)
  MONITORA_STAT_CONFIG <- data.table::data.table(
    parametro = c("alpha", "margem_equivalencia_pp", "efeito_minimo_pp", "min_UA_pareadas", "bootstrap_reamostragens_solicitadas", "permutacoes_monte_carlo_solicitadas", "perm_chunk", "recursos_adaptativo", "margem_composicao_dist_hellinger", "efeito_minimo_composicao_dist_hellinger", "baseline_ativo", "baseline_modo", "baseline_min_anos", "baseline_mostrar_apenas_mudanca"),
    valor = c(MONITORA_STAT_ALPHA, MONITORA_STAT_MARGEM_PP, MONITORA_STAT_MIN_EFEITO_PP, MONITORA_STAT_MIN_PARES, MONITORA_STAT_BOOT, MONITORA_STAT_PERM, MONITORA_STAT_PERM_CHUNK, MONITORA_STAT_RECURSOS_ADAPTATIVO, MONITORA_STAT_COMP_MARGEM_DIST, MONITORA_STAT_COMP_MIN_DIST, MONITORA_STAT_BASELINE_ATIVO, MONITORA_STAT_BASELINE_MODO, MONITORA_STAT_BASELINE_MIN_ANOS, MONITORA_STAT_BASELINE_MOSTRAR_APENAS_MUDANCA)
  )
  monitora_stat_msg("comparações por categoria concluídas: ", nrow(MONITORA_STAT_MUDANCA_ANO_A_ANO), " linhas")
  monitora_stat_msg("comparações contra linha de base concluídas: ", nrow(MONITORA_STAT_MUDANCA_LINHA_BASE), " linhas")
  monitora_stat_msg("comparações de composição geral concluídas: ", nrow(MONITORA_STAT_COMPOSICAO_GERAL), " linhas")
  monitora_stat_msg("comparações de composição geral contra linha de base concluídas: ", nrow(MONITORA_STAT_COMPOSICAO_LINHA_BASE), " linhas")
  monitora_log(
    "estatistica_mudanca_ano_a_ano",
    "INFO",
    NA_character_,
    paste0("comparacoes=", nrow(MONITORA_STAT_MUDANCA_ANO_A_ANO), "; alpha=", MONITORA_STAT_ALPHA, "; margem_pp=", MONITORA_STAT_MARGEM_PP),
    "consultar estatisticas_mudanca_ano_a_ano.csv, estatisticas_mudanca_linha_base.csv, estatisticas_composicao_geral_ano_a_ano.csv e colunas de símbolos nos CSV auxiliares"
  )
  monitora_perf_checkpoint("estatistica_mudanca_ano_a_ano", "teste de permutação pareado, bootstrap e FDR por categoria", MONITORA_STAT_MUDANCA_ANO_A_ANO)
  monitora_perf_checkpoint("estatistica_mudanca_linha_base", "teste de permutação pareado contra linha de base acumulada anterior", MONITORA_STAT_MUDANCA_LINHA_BASE)
  monitora_perf_checkpoint("estatistica_composicao_geral", "teste multivariado pareado por permutação com transformação de Hellinger", MONITORA_STAT_COMPOSICAO_GERAL)
  monitora_perf_checkpoint("estatistica_composicao_linha_base", "teste multivariado pareado contra linha de base acumulada anterior", MONITORA_STAT_COMPOSICAO_LINHA_BASE)
}


### Relatório textual estatístico e ecológico

monitora_relatorio_fmt_num <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "NA",
    format(round(x, digits), decimal.mark = ",", big.mark = ".", trim = TRUE, scientific = FALSE)
  )
}

monitora_relatorio_fmt_pp <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "NA",
    paste0(format(round(x, digits), decimal.mark = ",", big.mark = ".", trim = TRUE, scientific = FALSE), " p.p.")
  )
}

monitora_relatorio_fmt_p <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0,001", format(round(x, 4), decimal.mark = ",", big.mark = ".", trim = TRUE, scientific = FALSE))
  )
}

monitora_relatorio_classe_pt <- function(x) {
  x <- as.character(x)
  data.table::fcase(
    x %in% c("aumento"), "aumento",
    x %in% c("reducao", "redução"), "redução",
    x %in% c("estabilidade_equivalente"), "estabilidade/equivalência",
    x %in% c("estabilidade_composicional"), "estabilidade composicional",
    x %in% c("mudanca_composicao", "mudança_composição"), "mudança de composição",
    x %in% c("pares_insuficientes"), "pares insuficientes",
    x %in% c("inconclusivo"), "inconclusivo",
    default = x
  )
}

monitora_relatorio_rotulo_metrica <- function(x) {
  x <- as.character(x)
  data.table::fcase(
    x == "proporcao_relativa", "proporção relativa",
    x == "cobertura", "cobertura vegetal",
    default = x
  )
}

monitora_relatorio_rotulo_grupo <- function(x) {
  x <- as.character(x)
  data.table::fcase(
    x == "herbaceas_lenhosas", "herbáceas/lenhosas",
    x == "categorias_gerais", "categorias gerais",
    x == "material_botanico", "material botânico",
    x == "formas_vida_nativas", "formas de vida nativas",
    x == "formas_vida_exoticas", "formas de vida exóticas",
    x == "formas_vida_secas_mortas", "formas de vida secas ou mortas",
    default = x
  )
}

monitora_relatorio_linha_categoria <- function(dt, linha_base = FALSE, uc_txt = NA_character_) {
  if (is.null(dt) || !nrow(dt)) return(character())
  d <- data.table::as.data.table(data.table::copy(dt))
  data.table::setorder(d, grupo_grafico, tipo_metrica, form_veg, ano_2, categoria_label)

  if (isTRUE(linha_base)) {
    p_col <- "p_ajustado_fdr_linha_base"
    d[, linha := paste0(
      "- UC: ", uc_txt,
      " | formação: ", form_veg,
      " | grupo: ", monitora_relatorio_rotulo_grupo(grupo_grafico),
      " | métrica: ", monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | categoria: ", categoria_label,
      " | ano: ", ano_2,
      " | linha de base: ", anos_linha_base,
      " (", n_anos_linha_base, " ano(s))",
      " | n UAs pareadas: ", n_UA_pareadas,
      " | média linha de base: ", monitora_relatorio_fmt_num(media_linha_base * 100, 1), "%",
      " | média do ano: ", monitora_relatorio_fmt_num(media_ano_2 * 100, 1), "%",
      " | diferença: ", monitora_relatorio_fmt_pp(diferenca_pp, 1),
      " | IC95%: ", monitora_relatorio_fmt_pp(ci95_lower_pp, 1), " a ", monitora_relatorio_fmt_pp(ci95_upper_pp, 1),
      " | p ajustado FDR-BH: ", monitora_relatorio_fmt_p(get(p_col)),
      " | interpretação: ", monitora_relatorio_classe_pt(classe_mudanca)
    )]
  } else {
    p_col <- "p_ajustado_fdr"
    d[, linha := paste0(
      "- UC: ", uc_txt,
      " | formação: ", form_veg,
      " | grupo: ", monitora_relatorio_rotulo_grupo(grupo_grafico),
      " | métrica: ", monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | categoria: ", categoria_label,
      " | comparação: ", ano_1, " → ", ano_2,
      ifelse(!is.na(anos_referencia) & nzchar(as.character(anos_referencia)), paste0(" (referência real: ", anos_referencia, ")"), ""),
      " | n UAs pareadas: ", n_UA_pareadas,
      " | média referência: ", monitora_relatorio_fmt_num(media_ano_1 * 100, 1), "%",
      " | média do ano: ", monitora_relatorio_fmt_num(media_ano_2 * 100, 1), "%",
      " | diferença: ", monitora_relatorio_fmt_pp(diferenca_pp, 1),
      " | IC95%: ", monitora_relatorio_fmt_pp(ci95_lower_pp, 1), " a ", monitora_relatorio_fmt_pp(ci95_upper_pp, 1),
      " | p ajustado FDR-BH: ", monitora_relatorio_fmt_p(get(p_col)),
      " | interpretação: ", monitora_relatorio_classe_pt(classe_mudanca)
    )]
  }
  d$linha
}

monitora_relatorio_linha_composicao <- function(dt, linha_base = FALSE, uc_txt = NA_character_) {
  if (is.null(dt) || !nrow(dt)) return(character())
  d <- data.table::as.data.table(data.table::copy(dt))
  data.table::setorder(d, grupo_grafico, tipo_metrica, form_veg, ano_2)

  if (isTRUE(linha_base)) {
    d[, linha := paste0(
      "- UC: ", uc_txt,
      " | formação: ", form_veg,
      " | grupo: ", monitora_relatorio_rotulo_grupo(grupo_grafico),
      " | métrica: ", monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | ano: ", ano_2,
      " | linha de base: ", anos_linha_base,
      " (", n_anos_linha_base, " ano(s))",
      " | n UAs pareadas: ", n_UA_pareadas,
      " | categorias incluídas: ", n_categorias,
      " | distância Hellinger do centróide: ", monitora_relatorio_fmt_num(distancia_centroide_hellinger, 3),
      " | Bray-Curtis médio pareado: ", monitora_relatorio_fmt_num(bray_curtis_medio_pareado, 3),
      " | p ajustado FDR-BH: ", monitora_relatorio_fmt_p(p_ajustado_fdr_composicao_linha_base),
      " | interpretação: ", monitora_relatorio_classe_pt(classe_mudanca_composicao_linha_base),
      " | categorias: ", categorias_incluidas
    )]
  } else {
    d[, linha := paste0(
      "- UC: ", uc_txt,
      " | formação: ", form_veg,
      " | grupo: ", monitora_relatorio_rotulo_grupo(grupo_grafico),
      " | métrica: ", monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | comparação: ", ano_1, " → ", ano_2,
      ifelse(!is.na(anos_referencia) & nzchar(as.character(anos_referencia)), paste0(" (referência real: ", anos_referencia, ")"), ""),
      " | n UAs pareadas: ", n_UA_pareadas,
      " | categorias incluídas: ", n_categorias,
      " | distância Hellinger do centróide: ", monitora_relatorio_fmt_num(distancia_centroide_hellinger, 3),
      " | Bray-Curtis médio pareado: ", monitora_relatorio_fmt_num(bray_curtis_medio_pareado, 3),
      " | p ajustado FDR-BH: ", monitora_relatorio_fmt_p(p_ajustado_fdr_composicao),
      " | interpretação: ", monitora_relatorio_classe_pt(classe_mudanca_composicao),
      " | categorias: ", categorias_incluidas
    )]
  }
  d$linha
}

monitora_relatorio_resumo_classe <- function(dt, coluna_classe) {
  if (is.null(dt) || !nrow(dt) || !coluna_classe %in% names(dt)) return(character())
  d <- data.table::as.data.table(data.table::copy(dt))
  tab <- d[, .N, by = coluna_classe]
  data.table::setnames(tab, coluna_classe, "classe")
  data.table::setorder(tab, -N, classe)
  paste0("- ", monitora_relatorio_classe_pt(tab$classe), ": ", tab$N)
}

monitora_relatorio_principais_categoria <- function(dt, linha_base = FALSE, n_top = 12L) {
  if (is.null(dt) || !nrow(dt)) return(character())
  d <- data.table::as.data.table(data.table::copy(dt))
  d <- d[classe_mudanca %in% c("aumento", "reducao")]
  if (!nrow(d)) return("- Nenhuma mudança direcional sustentada foi detectada por categoria.")
  d[, abs_dif := abs(suppressWarnings(as.numeric(diferenca_pp)))]
  data.table::setorder(d, -abs_dif, grupo_grafico, tipo_metrica, form_veg, ano_2)
  d <- d[seq_len(min(n_top, .N))]
  if (isTRUE(linha_base)) {
    d[, paste0(
      "- ", form_veg, " | ", monitora_relatorio_rotulo_grupo(grupo_grafico), " | ",
      monitora_relatorio_rotulo_metrica(tipo_metrica), " | ", categoria_label,
      " | ", ano_2, " vs linha de base ", anos_linha_base,
      ": ", monitora_relatorio_classe_pt(classe_mudanca),
      " de ", monitora_relatorio_fmt_pp(diferenca_pp, 1),
      " (n=", n_UA_pareadas, "; p adj.=", monitora_relatorio_fmt_p(p_ajustado_fdr_linha_base), ")"
    )]
  } else {
    d[, paste0(
      "- ", form_veg, " | ", monitora_relatorio_rotulo_grupo(grupo_grafico), " | ",
      monitora_relatorio_rotulo_metrica(tipo_metrica), " | ", categoria_label,
      " | ", ano_1, "→", ano_2,
      ": ", monitora_relatorio_classe_pt(classe_mudanca),
      " de ", monitora_relatorio_fmt_pp(diferenca_pp, 1),
      " (n=", n_UA_pareadas, "; p adj.=", monitora_relatorio_fmt_p(p_ajustado_fdr), ")"
    )]
  }
}

monitora_relatorio_principais_composicao <- function(dt, linha_base = FALSE, n_top = 10L) {
  if (is.null(dt) || !nrow(dt)) return(character())
  classe_col <- if (isTRUE(linha_base)) "classe_mudanca_composicao_linha_base" else "classe_mudanca_composicao"
  p_col <- if (isTRUE(linha_base)) "p_ajustado_fdr_composicao_linha_base" else "p_ajustado_fdr_composicao"
  d <- data.table::as.data.table(data.table::copy(dt))
  d <- d[get(classe_col) == "mudanca_composicao"]
  if (!nrow(d)) return("- Nenhuma mudança sustentada da composição geral foi detectada.")
  data.table::setorder(d, -distancia_centroide_hellinger, grupo_grafico, tipo_metrica, form_veg, ano_2)
  d <- d[seq_len(min(n_top, .N))]
  if (isTRUE(linha_base)) {
    d[, paste0(
      "- ", form_veg, " | ", monitora_relatorio_rotulo_grupo(grupo_grafico), " | ",
      monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | ", ano_2, " vs linha de base ", anos_linha_base,
      ": mudança de composição; distância Hellinger=", monitora_relatorio_fmt_num(distancia_centroide_hellinger, 3),
      "; Bray-Curtis médio=", monitora_relatorio_fmt_num(bray_curtis_medio_pareado, 3),
      "; n=", n_UA_pareadas,
      "; p adj.=", monitora_relatorio_fmt_p(get(p_col))
    )]
  } else {
    d[, paste0(
      "- ", form_veg, " | ", monitora_relatorio_rotulo_grupo(grupo_grafico), " | ",
      monitora_relatorio_rotulo_metrica(tipo_metrica),
      " | ", ano_1, "→", ano_2,
      ": mudança de composição; distância Hellinger=", monitora_relatorio_fmt_num(distancia_centroide_hellinger, 3),
      "; Bray-Curtis médio=", monitora_relatorio_fmt_num(bray_curtis_medio_pareado, 3),
      "; n=", n_UA_pareadas,
      "; p adj.=", monitora_relatorio_fmt_p(get(p_col))
    )]
  }
}

monitora_gerar_relatorio_textual_estatistico <- function(caminho = file.path(MONITORA_OUTPUT_DIR, "relatorio_textual_estatistico.txt")) {
  linhas <- character()

  uc_txt <- "NA"
  if (exists("registros_corrig_stat") && "UC" %in% names(registros_corrig_stat)) {
    uc_vals <- sort(unique(as.character(registros_corrig_stat$UC)))
    uc_vals <- uc_vals[!is.na(uc_vals) & nzchar(uc_vals)]
    if (length(uc_vals)) uc_txt <- paste(uc_vals, collapse = "; ")
  }

  linhas <- c(
    linhas,
    "RELATÓRIO TEXTUAL ESTATÍSTICO - MONITORA CAMPESTRE-SAVÂNICO",
    paste0("Execução: ", if (exists("MONITORA_EXEC_ID")) MONITORA_EXEC_ID else format(Sys.time(), "%Y%m%d_%H%M%S")),
    paste0("UC(s): ", uc_txt),
    "",
    "Síntese metodológica:",
    paste0(
      "- As mudanças por categoria foram avaliadas com UA/transecto como unidade amostral, ",
      "teste pareado por permutação, IC95% por bootstrap pareado e p-valores ajustados por FDR-BH."
    ),
    paste0(
      "- A composição geral foi avaliada por teste multivariado pareado por permutação, ",
      "com transformação de Hellinger e distância de Bray-Curtis como descritor complementar."
    ),
    paste0(
      "- A linha de base é acumulada anterior: para cada ano-alvo, usa os anos anteriores disponíveis ",
      "da mesma UC/UA, exigindo pelo menos ", MONITORA_STAT_BASELINE_MIN_ANOS, " anos de referência e ",
      MONITORA_STAT_MIN_PARES, " UAs pareadas."
    ),
    "",
    "PRINCIPAIS ACHADOS",
    ""
  )

  if (exists("registros_corrig_stat")) {
    dstat <- data.table::as.data.table(registros_corrig_stat)
    if (all(c("UC", "ANO", "form_veg", "UA") %in% names(dstat))) {
      amostra <- dstat[, .(n_UA = data.table::uniqueN(UA)), by = .(UC, ANO, form_veg)]
      data.table::setorder(amostra, UC, ANO, form_veg)
      linhas <- c(
        linhas,
        "Esforço amostral por UC, ano e formação:",
        paste0("- ", amostra$UC, " | ", amostra$ANO, " | ", amostra$form_veg, ": ", amostra$n_UA, " UAs"),
        ""
      )
    }
  }

  if (exists("MONITORA_STAT_MUDANCA_ANO_A_ANO")) {
    linhas <- c(
      linhas,
      "Resumo das mudanças por categoria vs. medição anterior:",
      monitora_relatorio_resumo_classe(MONITORA_STAT_MUDANCA_ANO_A_ANO, "classe_mudanca"),
      "",
      "Principais mudanças por categoria vs. medição anterior:",
      monitora_relatorio_principais_categoria(MONITORA_STAT_MUDANCA_ANO_A_ANO, linha_base = FALSE),
      ""
    )
  }

  if (exists("MONITORA_STAT_MUDANCA_LINHA_BASE")) {
    linhas <- c(
      linhas,
      "Resumo das mudanças por categoria vs. linha de base acumulada anterior:",
      monitora_relatorio_resumo_classe(MONITORA_STAT_MUDANCA_LINHA_BASE, "classe_mudanca"),
      "",
      "Principais mudanças por categoria vs. linha de base acumulada anterior:",
      monitora_relatorio_principais_categoria(MONITORA_STAT_MUDANCA_LINHA_BASE, linha_base = TRUE),
      ""
    )
  }

  if (exists("MONITORA_STAT_COMPOSICAO_GERAL")) {
    linhas <- c(
      linhas,
      "Resumo da composição geral vs. medição anterior:",
      monitora_relatorio_resumo_classe(MONITORA_STAT_COMPOSICAO_GERAL, "classe_mudanca_composicao"),
      "",
      "Principais mudanças da composição geral vs. medição anterior:",
      monitora_relatorio_principais_composicao(MONITORA_STAT_COMPOSICAO_GERAL, linha_base = FALSE),
      ""
    )
  }

  if (exists("MONITORA_STAT_COMPOSICAO_LINHA_BASE")) {
    linhas <- c(
      linhas,
      "Resumo da composição geral vs. linha de base acumulada anterior:",
      monitora_relatorio_resumo_classe(MONITORA_STAT_COMPOSICAO_LINHA_BASE, "classe_mudanca_composicao_linha_base"),
      "",
      "Principais mudanças da composição geral vs. linha de base acumulada anterior:",
      monitora_relatorio_principais_composicao(MONITORA_STAT_COMPOSICAO_LINHA_BASE, linha_base = TRUE),
      ""
    )
  }

  linhas <- c(linhas, "DETALHAMENTO POR CATEGORIA - MEDIÇÃO ANTERIOR", "")
  if (exists("MONITORA_STAT_MUDANCA_ANO_A_ANO")) {
    linhas <- c(linhas, monitora_relatorio_linha_categoria(MONITORA_STAT_MUDANCA_ANO_A_ANO, linha_base = FALSE, uc_txt = uc_txt), "")
  } else {
    linhas <- c(linhas, "- Tabela estatistica de medição anterior não disponível.", "")
  }

  linhas <- c(linhas, "DETALHAMENTO POR CATEGORIA - LINHA DE BASE ACUMULADA ANTERIOR", "")
  if (exists("MONITORA_STAT_MUDANCA_LINHA_BASE")) {
    linhas <- c(linhas, monitora_relatorio_linha_categoria(MONITORA_STAT_MUDANCA_LINHA_BASE, linha_base = TRUE, uc_txt = uc_txt), "")
  } else {
    linhas <- c(linhas, "- Tabela estatistica de linha de base não disponível.", "")
  }

  linhas <- c(linhas, "DETALHAMENTO DA COMPOSIÇÃO GERAL - MEDIÇÃO ANTERIOR", "")
  if (exists("MONITORA_STAT_COMPOSICAO_GERAL")) {
    linhas <- c(linhas, monitora_relatorio_linha_composicao(MONITORA_STAT_COMPOSICAO_GERAL, linha_base = FALSE, uc_txt = uc_txt), "")
  } else {
    linhas <- c(linhas, "- Tabela de composição geral vs. medição anterior não disponível.", "")
  }

  linhas <- c(linhas, "DETALHAMENTO DA COMPOSIÇÃO GERAL - LINHA DE BASE ACUMULADA ANTERIOR", "")
  if (exists("MONITORA_STAT_COMPOSICAO_LINHA_BASE")) {
    linhas <- c(linhas, monitora_relatorio_linha_composicao(MONITORA_STAT_COMPOSICAO_LINHA_BASE, linha_base = TRUE, uc_txt = uc_txt), "")
  } else {
    linhas <- c(linhas, "- Tabela de composição geral vs. linha de base não disponível.", "")
  }

  linhas <- c(
    linhas,
    "Legenda interpretativa:",
    paste0("- aumento/redução: mudança sustentada por teste pareado, p ajustado FDR-BH ≤ ", MONITORA_STAT_ALPHA, " e efeito mínimo ≥ ", MONITORA_STAT_MIN_EFEITO_PP, " p.p."),
    paste0("- estabilidade/equivalência: IC95% dentro da margem ecológica de ±", MONITORA_STAT_MARGEM_PP, " p.p."),
    paste0("- pares insuficientes: menos de ", MONITORA_STAT_MIN_PARES, " UAs pareadas válidas."),
    "- inconclusivo: amostra testável, mas sem evidência suficiente para afirmar mudança ou estabilidade/equivalência."
  )

  linhas <- linhas[!is.na(linhas)]
  writeLines(linhas, con = caminho, useBytes = TRUE)
  if (exists("monitora_stat_msg", mode = "function")) {
    monitora_stat_msg("relatório textual estatístico gravado em output/relatorio_textual_estatistico.txt")
  }
  invisible(caminho)
}


### Exportação das tabelas estatísticas em CSV.

### A partir daqui, todos os produtos finais são gravados em output/.
setwd(MONITORA_OUTPUT_DIR)

if (exists("registros_corrig"))
  fwrite(registros_corrig,
         file.path("registros_corrig.csv"),
         row.names = FALSE)

if (exists("registros_corrig_stat"))
  fwrite(registros_corrig_stat,
         file.path("registros_corrig_stat.csv"),
         row.names = FALSE)

if (exists("reg_corrig_stat_summarise_p1"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_p1, "herbaceas_lenhosas", "plot_p1_prop_rel_herb_lenh"),
    file.path("prop_rel_herb_lenh.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p2"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_p2, "categorias_gerais", "plot_p2_prop_rel_categ"),
    file.path("prop_rel_categ.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_material_botanico"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_material_botanico, "material_botanico", "plot_p2m_prop_rel_material_botanico"),
    file.path("prop_rel_material_botanico.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p3"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_p3, "formas_vida_nativas", "plot_p3_prop_rel_nat"),
    file.path("prop_rel_form_vida_nat.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p4"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_p4, "formas_vida_exoticas", "plot_p4_prop_rel_exot"),
    file.path("prop_rel_form_vida_exot.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p5"))
  fwrite(
    monitora_preparar_auxiliar_prop(reg_corrig_stat_summarise_p5, "formas_vida_secas_mortas", "plot_p5_prop_rel_seca_morta"),
    file.path("prop_rel_form_vida_seca_morta.csv"),
    row.names = FALSE
  )

## Tabelas de cobertura vegetal em formato padronizado para planilhas.

if (exists("reg_corrig_stat_summarise_p1_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_p1_presence, "herbaceas_lenhosas", "plot_p1_veg_cover_herb_lenh"),
    file.path("cob_veg_herb_lenh.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p2_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_p2_presence, "categorias_gerais", "plot_p2_veg_cover_categ"),
    file.path("cob_veg_categ.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_material_botanico_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_material_botanico_presence, "material_botanico", "plot_p2m_veg_cover_material_botanico"),
    file.path("cob_veg_material_botanico.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p3_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_p3_presence, "formas_vida_nativas", "plot_p3_veg_cover_nat"),
    file.path("cob_veg_form_vida_nat.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p4_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_p4_presence, "formas_vida_exoticas", "plot_p4_veg_cover_exot"),
    file.path("cob_veg_form_vida_exot.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p5_presence"))
  fwrite(
    monitora_preparar_auxiliar_cobertura(reg_corrig_stat_summarise_p5_presence, "formas_vida_secas_mortas", "plot_p5_veg_cover_seca_morta"),
    file.path("cob_veg_form_vida_seca_morta.csv"),
    row.names = FALSE
  )

if (exists("MONITORA_STAT_MUDANCA_ANO_A_ANO")) {
  fwrite(
    MONITORA_STAT_MUDANCA_ANO_A_ANO,
    file.path("estatisticas_mudanca_ano_a_ano.csv"),
    row.names = FALSE
  )
  if (exists("MONITORA_STAT_MUDANCA_LINHA_BASE")) {
    fwrite(
      MONITORA_STAT_MUDANCA_LINHA_BASE,
      file.path("estatisticas_mudanca_linha_base.csv"),
      row.names = FALSE
    )
  }
  if (exists("MONITORA_STAT_COMPOSICAO_GERAL")) {
    fwrite(
      MONITORA_STAT_COMPOSICAO_GERAL,
      file.path("estatisticas_composicao_geral_ano_a_ano.csv"),
      row.names = FALSE
    )
  }
  if (exists("MONITORA_STAT_COMPOSICAO_LINHA_BASE")) {
    fwrite(
      MONITORA_STAT_COMPOSICAO_LINHA_BASE,
      file.path("estatisticas_composicao_linha_base.csv"),
      row.names = FALSE
    )
  }
  fwrite(
    MONITORA_STAT_CONFIG,
    file.path("estatisticas_mudanca_config.csv"),
    row.names = FALSE
  )
  monitora_gerar_relatorio_textual_estatistico(file.path(MONITORA_OUTPUT_DIR, "relatorio_textual_estatistico.txt"))
}

fwrite(
  monitora_criar_indice_graficos_auxiliares(),
  file.path("indice_graficos_auxiliares.csv"),
  row.names = FALSE
)


monitora_controlar_recursos("exportacao_tabelas_csv", risco = "normal", force_log = TRUE)
monitora_perf_checkpoint("exportacao_tabelas_csv", "gravação das tabelas finais em output/")

### Relatórios de auditoria da execução

if (exists("registros_corrig")) {
  # Duplicidades lógicas após normalização: não remove automaticamente, apenas informa,
  # pois pode refletir correções manuais legítimas ou ausência de UUID em versões antigas.
  key_cols <- intersect(c("UC", "CICLO", "UA", "ANO", "ponto_amostral (amostragem/registro)", "Ponto amostral (amostragem/registro)", "uuid (amostragem/registro)", "UUID"), names(registros_corrig))
  if (length(key_cols) > 0) {
    dup_logico <- registros_corrig[, .N, by = key_cols][N > 1]
    if (nrow(dup_logico) > 0) {
      fwrite(dup_logico, file.path(MONITORA_LOG_DIR, paste0("duplicidades_logicas_pos_normalizacao_", MONITORA_EXEC_ID, ".csv")))
      monitora_log("duplicidade_logica", "AVISO", NA_character_, paste0(nrow(dup_logico), " combinações de chave com N > 1 após normalização"), "não removido automaticamente; ver duplicidades_logicas_pos_normalizacao")
    }
  }
}

MONITORA_RESUMO_ACHADOS_RELEVANTES <- monitora_criar_resumo_achados_validacao()
fwrite(MONITORA_REPORT, file.path(MONITORA_LOG_DIR, paste0("relatorio_execucao_", MONITORA_EXEC_ID, ".csv")))
fwrite(MONITORA_REPORT, file.path(MONITORA_OUTPUT_DIR, "relatorio_execucao_ultima_execucao.csv"))

monitora_perf_checkpoint("relatorios_auditoria", "gravação dos relatórios de auditoria")

### Exportação dos gráficos.


### Preparação de anotação estatística dos gráficos.
### Os símbolos são aplicados novamente, de forma garantida, no momento do ggsave().
### Esta etapa mantém apenas uma mensagem de controle; a exportação é a fonte final dos PNGs.
if (exists("MONITORA_STAT_MUDANCA_ANO_A_ANO") || exists("MONITORA_STAT_COMPOSICAO_GERAL")) {
  monitora_stat_msg("símbolos estatísticos serão aplicados diretamente no objeto exportado por ggsave")
}

# Nomes de todos os objetos de gráfico.
plot_list_names <- c(
  "plot_p1.1.1_prop_rel_herb_lenh_camp_sem_rotulo",
  "plot_p1.1.2_prop_rel_herb_lenh_camp_com_rotulo",
  "plot_p1.2.1_prop_rel_herb_lenh_sav_sem_rotulo",
  "plot_p1.2.2_prop_rel_herb_lenh_sav_com_rotulo",
  "plot_p1.3.1_veg_cover_herb_lenh_com_rotulo",
  "plot_p1.3.2_veg_cover_herb_lenh_sem_rotulo",
  "plot_p2.1.1_prop_rel_categ_camp_sem_rotulo",
  "plot_p2.1.2_prop_rel_categ_camp_com_rotulo",
  "plot_p2.2.1_prop_rel_categ_sav_sem_rotulo",
  "plot_p2.2.2_prop_rel_categ_sav_com_rotulo",
  "plot_p2.3.1_veg_cover_categ_com_rotulo",
  "plot_p2.3.2_veg_cover_categ_sem_rotulo",
  "plot_p2m.1.1_prop_rel_material_botanico_camp_sem_rotulo",
  "plot_p2m.1.2_prop_rel_material_botanico_camp_com_rotulo",
  "plot_p2m.2.1_prop_rel_material_botanico_sav_sem_rotulo",
  "plot_p2m.2.2_prop_rel_material_botanico_sav_com_rotulo",
  "plot_p2m.3.1_veg_cover_material_botanico_com_rotulo",
  "plot_p2m.3.2_veg_cover_material_botanico_sem_rotulo",
  "plot_p3.1.1_prop_rel_nat_camp_sem_rotulo",
  "plot_p3.1.2_prop_rel_nat_camp_com_rotulo",
  "plot_p3.2.1_prop_rel_nat_sav_sem_rotulo",
  "plot_p3.2.2_prop_rel_nat_sav_com_rotulo",
  "plot_p3.3.1_veg_cover_nat_com_rotulo",
  "plot_p3.3.2_veg_cover_nat_sem_rotulo",
  "plot_p4.1.1_prop_rel_exot_camp_sem_rotulo",
  "plot_p4.1.2_prop_rel_exot_camp_com_rotulo",
  "plot_p4.2.1_prop_rel_exot_sav_sem_rotulo",
  "plot_p4.2.2_prop_rel_exot_sav_com_rotulo",
  "plot_p4.3.1_veg_cover_exot_com_rotulo",
  "plot_p4.3.2_veg_cover_exot_sem_rotulo",
  "plot_p5.1.1_prop_rel_seca_morta_camp_sem_rotulo",
  "plot_p5.1.2_prop_rel_seca_morta_camp_com_rotulo",
  "plot_p5.2.1_prop_rel_seca_morta_sav_sem_rotulo",
  "plot_p5.2.2_prop_rel_seca_morta_sav_com_rotulo",
  "plot_p5.3.1_veg_cover_seca_morta_com_rotulo",
  "plot_p5.3.2_seca_morta_sem_rotulo"
)


monitora_stat_largura_painel_gtable_in <- function(g, largura_total_in) {
  # Calcula a largura das colunas dos painéis no gtable, excluindo a legenda lateral.
  idx_painel <- which(grepl("^panel", g$layout$name))
  if (!length(idx_painel)) return(list(l = 1L, r = length(g$widths), largura_in = largura_total_in))

  l_painel <- min(g$layout$l[idx_painel])
  r_painel <- max(g$layout$r[idx_painel])

  tipos <- grid::unitType(g$widths)
  eh_null <- tipos == "null"
  larguras_fixas <- g$widths[!eh_null]
  largura_fixa_total <- tryCatch(
    sum(as.numeric(grid::convertWidth(larguras_fixas, "in", valueOnly = TRUE))),
    error = function(e) 0
  )

  null_total <- sum(as.numeric(g$widths[eh_null]))
  null_disp_in <- max(as.numeric(largura_total_in) - largura_fixa_total, 0.1)

  idx_sel <- seq.int(l_painel, r_painel)
  sel_null <- eh_null[idx_sel]
  largura_sel_fixa <- tryCatch(
    sum(as.numeric(grid::convertWidth(g$widths[idx_sel][!sel_null], "in", valueOnly = TRUE))),
    error = function(e) 0
  )
  largura_sel_null <- if (null_total > 0) null_disp_in * sum(as.numeric(g$widths[idx_sel][sel_null])) / null_total else 0
  largura_painel_in <- max(largura_sel_fixa + largura_sel_null, 0.1)

  list(l = l_painel, r = r_painel, largura_in = largura_painel_in)
}

monitora_stat_adicionar_caption_painel <- function(plot_obj, caption, largura_total_in, fontsize = 7.8, lineheight = 1.02) {
  # Insere a legenda inferior como grob próprio, alinhado às colunas do painel/eixo X.
  # Assim a legenda lateral não entra no cálculo da largura disponível.
  if (is.null(caption) || !nzchar(as.character(caption))) {
    return(list(plot = plot_obj, n_linhas_caption = 0L))
  }

  plot_sem_caption <- plot_obj +
    ggplot2::labs(caption = NULL) +
    ggplot2::theme(
      plot.caption = ggplot2::element_blank(),
      plot.caption.position = "panel"
    )

  g <- ggplot2::ggplotGrob(plot_sem_caption)
  painel <- monitora_stat_largura_painel_gtable_in(g, largura_total_in = largura_total_in)

  # Pequena folga interna para impedir encostar nas bordas do painel.
  largura_util_in <- max(painel$largura_in - 0.06, 0.1)
  linhas <- monitora_stat_quebrar_caption_painel_in(
    caption,
    largura_max_in = largura_util_in,
    fontsize = fontsize,
    lineheight = lineheight
  )
  if (!length(linhas)) return(list(plot = g, n_linhas_caption = 0L))

  texto_caption <- paste(linhas, collapse = "\n")
  gp <- grid::gpar(fontsize = fontsize, lineheight = lineheight)
  grob_caption <- grid::textGrob(
    label = texto_caption,
    x = grid::unit(0, "npc"),
    y = grid::unit(1, "npc"),
    just = c("left", "top"),
    gp = gp
  )

  altura_caption <- grid::grobHeight(grob_caption) + grid::unit(8, "pt")
  g <- gtable::gtable_add_rows(g, heights = altura_caption, pos = length(g$heights))
  linha_caption <- length(g$heights)
  g <- gtable::gtable_add_grob(
    g,
    grobs = grob_caption,
    t = linha_caption,
    l = painel$l,
    r = painel$r,
    clip = "off",
    name = "caption_painel"
  )
  list(plot = g, n_linhas_caption = length(linhas))
}

monitora_ajustar_layout_exportacao <- function(plot_obj, nome_plot = NULL, width = 11, height = 7) {
  if (!inherits(plot_obj, "ggplot")) return(list(plot = plot_obj, width = width, height = height))

  dados_plot <- tryCatch(data.table::as.data.table(plot_obj$data), error = function(e) data.table::data.table())
  n_categorias <- 0L
  maior_categoria <- 0L
  n_anos <- 0L
  if (nrow(dados_plot)) {
    if ("categoria_label" %in% names(dados_plot)) {
      cats <- unique(stats::na.omit(as.character(dados_plot$categoria_label)))
      n_categorias <- length(cats)
      maior_categoria <- max(nchar(cats, type = "width"), na.rm = TRUE)
      if (!is.finite(maior_categoria)) maior_categoria <- 0L
    }
    if ("ANO" %in% names(dados_plot)) n_anos <- length(unique(stats::na.omit(as.character(dados_plot$ANO))))
    if ("ANO_factor_rotulo" %in% names(dados_plot)) n_anos <- max(n_anos, length(unique(stats::na.omit(as.character(dados_plot$ANO_factor_rotulo)))))
  }

  titulo <- plot_obj$labels$title
  largura_titulo <- 78L
  if (n_categorias >= 10L || maior_categoria >= 30L) largura_titulo <- 68L
  if (!is.null(titulo) && nzchar(as.character(titulo))) {
    plot_obj <- plot_obj + ggplot2::labs(
      title = monitora_quebrar_linhas_publicavel(titulo, largura = largura_titulo, justificar = FALSE)
    )
  }

  width_adj <- width
  if (n_categorias >= 8L) width_adj <- max(width_adj, 12.0)
  if (n_categorias >= 12L || maior_categoria >= 34L) width_adj <- max(width_adj, 13.2)
  if (!is.null(titulo) && nchar(as.character(titulo), type = "width") >= 90L) width_adj <- max(width_adj, 12.6)

  caption <- plot_obj$labels$caption
  n_linhas_titulo <- if (is.null(plot_obj$labels$title)) 1L else length(strsplit(as.character(plot_obj$labels$title), "\n", fixed = TRUE)[[1]])
  height_adj <- height
  if (n_anos >= 6L) height_adj <- max(height_adj, 7.0 + 0.18 * (n_anos - 5L))
  if (n_linhas_titulo > 1L) height_adj <- max(height_adj, height + 0.28 * (n_linhas_titulo - 1L))

  plot_obj <- plot_obj + ggplot2::theme(
    plot.title = monitora_elemento_titulo_publicavel(size = if (n_linhas_titulo > 1L) 13.5 else 14),
    plot.title.position = "plot",
    plot.caption = ggplot2::element_blank(),
    plot.caption.position = "panel",
    plot.margin = ggplot2::margin(t = 18, r = 24, b = 22, l = 24)
  )

  caption_layout <- monitora_stat_adicionar_caption_painel(
    plot_obj,
    caption = caption,
    largura_total_in = width_adj,
    fontsize = 7.8,
    lineheight = 1.02
  )
  if (caption_layout$n_linhas_caption > 0L) {
    height_adj <- max(height_adj, 7.0 + 0.15 * caption_layout$n_linhas_caption)
  }

  list(plot = caption_layout$plot, width = width_adj, height = height_adj)
}

monitora_ggsave_publicavel <- function(filename, plot, width = 11, height = 7, dpi = 320, nome_plot = NULL) {
  tem_rotulo <- !is.null(nome_plot) && exists("monitora_stat_plot_com_rotulo", mode = "function") && monitora_stat_plot_com_rotulo(nome_plot)
  if (!is.null(nome_plot)) {
    plot <- monitora_stat_anotar_plot_exportacao(plot, nome_plot)
  }
  # Gráficos com rótulos estatísticos precisam de folga vertical para a legenda metodológica.
  if (isTRUE(tem_rotulo)) height <- max(height, 7.95)
  layout_exportacao <- monitora_ajustar_layout_exportacao(plot, nome_plot = nome_plot, width = width, height = height)
  ggplot2::ggsave(
    filename = filename,
    plot = layout_exportacao$plot,
    width = layout_exportacao$width,
    height = layout_exportacao$height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}


# Objetos de gráfico existentes.
if (isTRUE(MONITORA_EXPORTAR_GRAFICOS) && (!exists("registros_corrig") || nrow(registros_corrig) <= MONITORA_MAX_LINHAS_GRAFICOS_AUTO)) {
  existing_plots <- plot_list_names[plot_list_names %in% ls()]
  plot_list <- mget(existing_plots)
  dir.create("plots_png", showWarnings = FALSE)
  walk2(
    .x = plot_list,
    .y = existing_plots,
    .f = ~monitora_ggsave_publicavel(
      filename = file.path("plots_png", paste0(.y, ".png")),
      plot = .x,
      width = 11,
      height = 7,
      dpi = 320,
      nome_plot = .y
    )
  )
  rm(plot_list); monitora_gc("exportacao_graficos_png")
  if (exists("MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS")) {
    data.table::fwrite(
      MONITORA_STAT_AUDITORIA_SIMBOLOS_PLOTS,
      file.path(MONITORA_OUTPUT_DIR, "auditoria_simbolos_graficos.csv"),
      row.names = FALSE
    )
    monitora_stat_msg("auditoria de símbolos dos gráficos gravada em output/auditoria_simbolos_graficos.csv")
  }
  monitora_perf_checkpoint("exportacao_graficos_png", "gravação dos gráficos PNG")
} else {
  motivo <- if (!isTRUE(MONITORA_EXPORTAR_GRAFICOS)) "MONITORA_EXPORTAR_GRAFICOS=false" else paste0("nrow(registros_corrig)>MONITORA_MAX_LINHAS_GRAFICOS_AUTO=", MONITORA_MAX_LINHAS_GRAFICOS_AUTO)
  monitora_log("exportacao_graficos_png", "AVISO", NA_character_, paste0("Exportação de gráficos PNG ignorada: ", motivo), "para forçar, ajustar variáveis de ambiente")
  monitora_perf_checkpoint("exportacao_graficos_png_ignorada", motivo)
}

### Exportação dos arquivos KML.

if (isTRUE(MONITORA_EXPORTAR_KML) && exists("registros_corrig_stat") && nrow(registros_corrig_stat) <= MONITORA_MAX_UAS_KML_AUTO) {

# Converte colunas de coordenadas para numérico, quando necessário.
registros_corrig_stat$long_ini <- as.numeric(registros_corrig_stat$long_ini)
registros_corrig_stat$lat_ini <- as.numeric(registros_corrig_stat$lat_ini)
registros_corrig_stat$long_fin <- as.numeric(registros_corrig_stat$long_fin)
registros_corrig_stat$lat_fin <- as.numeric(registros_corrig_stat$lat_fin)

# Remove registros com coordenadas ausentes, quando houver.
registros_corrig_stat <- registros_corrig_stat[!is.na(registros_corrig_stat$long_ini) &
                                                 !is.na(registros_corrig_stat$lat_ini) &
                                                 !is.na(registros_corrig_stat$long_fin) &
                                                 !is.na(registros_corrig_stat$lat_fin), ]

# Garante unicidade na coluna 'name'.
registros_corrig_stat <- registros_corrig_stat %>%
  mutate(name = paste(UA, ANO, sep = "_")) %>%
  group_by(name) %>%
  mutate(name = paste(name, row_number(), sep = "_")) %>%
  ungroup()

# Cria uma geometria LineString para cada registro, combinando as coordenadas inicial e final.
geometry <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
  st_linestring(matrix(
    c(
      registros_corrig_stat$long_ini[i],
      registros_corrig_stat$lat_ini[i],
      registros_corrig_stat$long_fin[i],
      registros_corrig_stat$lat_fin[i]
    ),
    ncol = 2,
    byrow = TRUE
  ))
}))

# Cria geometrias de ponto para cada coordenada inicial e final.
points_ini <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
  st_point(c(
    registros_corrig_stat$long_ini[i],
    registros_corrig_stat$lat_ini[i]
  ))  # Pontos iniciais
}))

points_fin <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
  st_point(c(
    registros_corrig_stat$long_fin[i],
    registros_corrig_stat$lat_fin[i]
  ))
}))

# Cria uma nova tabela para os pontos inicial e final, preservando os atributos originais.
registros_corrig_stat_points_ini <- registros_corrig_stat %>%
  mutate(
    name = paste("Inicial: ", UA, "_", ANO, sep = ""),
    point_type = "start",
    geometry = points_ini
  )

registros_corrig_stat_points_fin <- registros_corrig_stat %>%
  mutate(
    name = paste("Final: ", UA, "_", ANO, sep = ""),
    point_type = "end",
    geometry = points_fin
  )

# Combina as tabelas de pontos e linhas.
combined_sf_object <- bind_rows(
  st_sf(registros_corrig_stat, geometry = geometry),
  registros_corrig_stat_points_ini,
  registros_corrig_stat_points_fin
)

# Define o sistema de referência WGS84 (EPSG:4326) no objeto sf.
combined_sf_object <- st_set_crs(combined_sf_object, 4326)

# Exporta o arquivo KML, sobrescrevendo arquivo anterior se existir.
st_write(combined_sf_object, "UAs_registros_corrig_stat.kml", driver = "KML", delete_dsn=TRUE)

# Exporta o arquivo KML, sobrescrevendo arquivo anterior se existir.
st_write(subset(combined_sf_object %>% select(c(".id", 
                                                "PROTOCOLO", 
                                                "UC", 
                                                "UA", 
                                                "ANO", 
                                                "lat_ini", 
                                                "long_ini", 
                                                "alt_ini", 
                                                "acc_ini", 
                                                "lat_fin", 
                                                "long_fin", 
                                                "alt_fin", 
                                                "acc_fin", 
                                                "name", 
                                                "geometry", 
                                                "point_type"))), 
         "UAs_verg_ini_verg_fin.kml", driver = "KML", delete_dsn=TRUE)

# Remove objetos temporários.
rm(combined_sf_object,
   geometry,
   points_ini,
   points_fin,
   registros_corrig_stat_points_ini,
   registros_corrig_stat_points_fin)
monitora_gc("exportacao_kml")

} else {
  motivo_kml <- if (!isTRUE(MONITORA_EXPORTAR_KML)) "MONITORA_EXPORTAR_KML=false" else paste0("nrow(registros_corrig_stat)>MONITORA_MAX_UAS_KML_AUTO=", MONITORA_MAX_UAS_KML_AUTO)
  monitora_log("exportacao_kml", "AVISO", NA_character_, paste0("Exportação KML ignorada: ", motivo_kml), "para forçar, ajustar variáveis de ambiente")
}

### Relatório de performance da execução
### Grava o tempo incremental e acumulado por etapa em log/ e a última execução em output/.
monitora_perf_checkpoint("exportacao_kml", "exportação dos arquivos KML, quando coordenadas suficientes existirem", if (exists("registros_corrig_stat")) registros_corrig_stat else NULL)
monitora_resource_control_write()
monitora_perf_write()
monitora_log("performance", "INFO", file.path(MONITORA_LOG_DIR, paste0("performance_execucao_", MONITORA_EXEC_ID, ".csv")), "relatório de performance gerado", "usar para identificar gargalos por duração_seg")
MONITORA_RESUMO_ACHADOS_RELEVANTES <- monitora_criar_resumo_achados_validacao()
fwrite(MONITORA_REPORT, file.path(MONITORA_LOG_DIR, paste0("relatorio_execucao_", MONITORA_EXEC_ID, ".csv")))
fwrite(MONITORA_REPORT, file.path(MONITORA_OUTPUT_DIR, "relatorio_execucao_ultima_execucao.csv"))

monitora_limpar_ambiente_temporario()
monitora_gc("limpeza_final_ambiente")
