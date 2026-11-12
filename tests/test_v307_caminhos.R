#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
script <- if (length(args)) args[1L] else "monitora_campsav_alvo_global_v3.0.7.R"
modulo <- "--modulo" %in% args
source("tools/v307/carregar.R")
e <- monitora_v307_carregar(script)
if (modulo) {
  source("tools/v307/caminhos.R", local = e)
  patch <- function(nome, transformar) {
    f <- e[[nome]]; body(f) <- parse(text = transformar(paste(deparse(body(f), width.cutoff = 500L), collapse = "\n")))[[1L]]; e[[nome]] <- f
  }
  mover <- function(s) gsub("mov <- monitora_output_mover_arquivo(ff, destino)",
    paste0("mov <- monitora_v307_caminhos_mover(ff, destino_rel, output_dir, exec_id, contexto)\n",
      "destino_rel <- mov$destino_relativo\ndestino <- file.path(output_dir, destino_rel)"), s, fixed = TRUE)
  patch("monitora_output_migrar_diretorios_legados", mover)
  patch("monitora_output_organizar_produtos", function(s) {
    s <- mover(s)
    s <- sub("aud_dirs <- monitora_output_migrar_diretorios_legados(output_dir, exec_id, contexto)",
      paste0("aud_curto <- monitora_v307_caminhos_compactar_existentes(output_dir, exec_id, contexto)\n",
        "aud_dirs <- data.table::rbindlist(list(aud_curto, monitora_output_migrar_diretorios_legados(output_dir, exec_id, contexto)), fill=TRUE)"), s, fixed = TRUE)
    s <- sub("MONITORA_OUTPUT_MAPA_MOVIMENTACOES <<- data.table::rbindlist(list(mapa_anterior, mapa_atual), fill = TRUE, use.names = TRUE)",
      paste0("MONITORA_OUTPUT_MAPA_MOVIMENTACOES <<- monitora_v307_caminhos_mapa_atual(\n",
        "data.table::rbindlist(list(mapa_anterior,mapa_atual),fill=TRUE,use.names=TRUE),mapa_atual,output_dir,exec_id)"), s, fixed = TRUE)
    s <- sub("monitora_output_reescrever_referencias(output_dir, mapa_total)",
      paste0("mapa_total <- monitora_v307_caminhos_mapa_atual(mapa_total, mapa_atual)\n",
        "prova_caminhos <- monitora_v307_caminhos_preparar_edicao(output_dir)\n",
        "monitora_output_reescrever_referencias(output_dir, mapa_total)\nmonitora_v307_caminhos_finalizar(output_dir, prova_caminhos)"), s, fixed = TRUE)
    s
  })
  patch("monitora_output_reescrever_referencias", function(s) sub("candidatos <- monitora_v307_editorial_candidatos_mutaveis(candidatos)",
    "candidatos <- monitora_v307_editorial_candidatos_mutaveis(candidatos)\ncandidatos <- monitora_v307_caminhos_candidatos(candidatos, output_dir)", s, fixed = TRUE))
}
stopifnot(is.function(e$monitora_v307_caminhos_mover), grepl("monitora_v307_caminhos_compactar_existentes", paste(deparse(body(e$monitora_output_organizar_produtos)), collapse = ""), fixed = TRUE))
checks <- 0L
checar <- function(nome, ok) { if (!isTRUE(ok)) stop("FALHOU: ", nome, call. = FALSE); checks <<- checks + 1L; cat("OK:", nome, "\n") }
erro <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
hash <- function(p) digest::digest(file = p, algo = "sha256")
gravar <- function(p, x) { dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines(x, p, useBytes = TRUE) }
foto <- function(p) {
  fs <- sort(list.files(p, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)); fs <- fs[!dir.exists(fs)]
  setNames(vapply(fs, hash, character(1L), USE.NAMES = FALSE), substring(fs, nchar(p) + 2L))
}
diag <- Sys.getenv("MONITORA_V307_TEST_DIAG_DIR", unset = tempdir()); dir.create(diag, recursive = TRUE, showWarnings = FALSE)
contador <- 0L
nova <- function() {
  contador <<- contador + 1L; p <- tempfile("v307_paths_", tmpdir = diag); dir.create(p)
  e$MONITORA_BASE_DIR <- p; e$MONITORA_INPUT_DIR <- file.path(p, "input"); e$MONITORA_OUTPUT_DIR <- file.path(p, "output")
  e$MONITORA_LOG_DIR <- file.path(p, "log"); e$MONITORA_CORRECOES_DIR <- file.path(p, "output", "correcoes_campos")
  e$MONITORA_EXEC_ID <- paste0("20261111_160000_", sprintf("%03d", contador)); e$MONITORA_V307_TRANSACAO_FINAL_ATIVA <- NULL
  e$MONITORA_OUTPUT_MAPA_MOVIMENTACOES <- data.table(); e$MONITORA_LOG_EXECUCAO <- data.table()
  gravar(file.path(p, "input", "original.csv"), "input preservado")
  gravar(file.path(p, "dadosoriginais", "original.csv"), "originais preservados")
  p
}
rel <- "99_legacy_compat/div/correcoes_campos/relatorios_pos_painel/ocorrencias/indice_ocorrencias.csv"
mover <- function(origem, r = rel, ...) e$monitora_v307_caminhos_mover(origem, r, e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "teste_caminhos", ...)
mapa <- function() fread(file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat", "mapa.csv"), colClasses = "character", na.strings = NULL)

p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "conteudo original"); sha <- hash(origem)
checar("Reproduz destino real de 222 caracteres", nchar(e$monitora_caminho_windows_projetado(origem, e$MONITORA_OUTPUT_DIR)) == 222L)
curto <- file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat", "curto.csv"); gravar(curto, "curto preservado")
canon <- file.path(e$MONITORA_OUTPUT_DIR, "01_produtos_dados", paste0(strrep("a", 85L), ".csv")); gravar(canon, "canonico preservado")
sha_curto <- hash(curto); sha_canon <- hash(canon)
aud <- e$monitora_v307_caminhos_compactar_existentes(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "teste_existentes")
m <- mapa(); dest <- file.path(e$MONITORA_OUTPUT_DIR, m$destino_relativo); bruto <- file.path(e$MONITORA_OUTPUT_DIR, m$fonte_preservada_relativa)
checar("Só o legado excessivo foi compactado", nrow(aud) == 1L && !file.exists(origem) && identical(hash(curto), sha_curto) && identical(hash(canon), sha_canon))
checar("Fonte e derivado preservam SHA antes da edição", identical(hash(dest), sha) && identical(hash(bruto), sha) && identical(m$sha256, sha))
checar("Todos os novos caminhos cabem no orçamento", all(nchar(e$monitora_caminho_windows_projetado(c(dest, bruto, file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat/mapa.csv")), e$MONITORA_OUTPUT_DIR)) <= 210L))
checar("Mapa mantém rota lógica, origem e prova completa", identical(m$destino_legado_relativo, rel) && identical(m$caminho_origem_relativo, rel) && nchar(m$sha256) == 64L)

# Uma colisão prévia, mesmo com o mesmo prefixo de nome, nunca é sobrescrita.
p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "fonte para colisao"); sha <- hash(origem)
mov <- mover(origem); destino1 <- file.path(e$MONITORA_OUTPUT_DIR, mov$destino_relativo)
unlink(file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat/mapa.csv")); gravar(destino1, "historico conflitante"); conflito <- hash(destino1)
gravar(origem, "fonte para colisao"); mov2 <- mover(origem)
checar("Colisão com bytes distintos preserva histórico e cria outra rota", mov2$destino_relativo != mov$destino_relativo && identical(hash(destino1), conflito) && identical(hash(file.path(e$MONITORA_OUTPUT_DIR, mov2$destino_relativo)), sha))
gravar(origem, "fonte para colisao"); mov3 <- mover(origem)
checar("Repetição idêntica não duplica mapa nem sobrescreve conflito", identical(mov3$destino_relativo, mov2$destino_relativo) && nrow(mapa()) == 1L && identical(hash(destino1), conflito))

for (etapa in c("antes_copia", "apos_copia", "apos_mapa", "apos_retirada")) {
  p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "arquivo anterior"); antes <- foto(e$MONITORA_OUTPUT_DIR)
  msg <- erro(mover(origem, injetar_falha = function(atual, tx) if (atual == etapa) stop("falha injetada")))
  checar(paste("Rollback completo em", etapa), grepl("falha injetada", msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))
}
p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "arquivo anterior"); antes <- foto(e$MONITORA_OUTPUT_DIR)
msg <- erro(mover(origem, injetar_falha = function(atual, tx) if (atual == "apos_mapa") gravar(file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat/mapa.csv"), "mapa corrompido")))
checar("Mapa adulterado é recusado e revertido", nzchar(msg) && identical(foto(e$MONITORA_OUTPUT_DIR), antes))

# Prova com o escritor REAL antes de qualquer mover/rewriter. O índice bruto
# inclui base absoluta legada; a fonte imutável e o derivado editorial diferem.
p <- nova(); base <- file.path(e$MONITORA_CORRECOES_DIR, "relatorios_pos_painel", "ocorrencias")
dados <- data.table(COLETA = "C1", UC = "UC_TESTE", EA = "1", UA = "1", CICLO = "1", CAMPANHA = "1", ANO = "2026",
  PROTOCOLO = "PLANTASHERBACEASELENHOSAS_CAMPSAV_21FEV25", Encostam = "nativa", forma_vida_nativa = "graminoide")
res <- e$monitora_diag_rel_gerar_ocorrencias(dados, fase = "pos_painel", base_dir = base, dominios_precomputados = data.table())
indice <- file.path(base, "indice_ocorrencias.csv"); antes <- fread(indice, colClasses = "character"); sha <- hash(indice)
checar("Escritor real produz links absolutos legados antes do mover", nrow(antes) > 0L && all(startsWith(antes$caminho, base)) && all(file.exists(antes$caminho)))
canonical <- file.path(e$MONITORA_OUTPUT_DIR, "02_painel_correcoes", "ocorrencias", "pos_painel", "indice_ocorrencias.csv")
gravar(canonical, "canonico anterior preservado"); sha_canon <- hash(canonical)
link <- file.path(e$MONITORA_OUTPUT_DIR, "07_validacao", "link_teste.txt"); origem_rel <- substring(indice, nchar(e$MONITORA_OUTPUT_DIR) + 2L)
gravar(link, origem_rel)
in1 <- foto(e$MONITORA_INPUT_DIR); ori1 <- foto(file.path(p, "dadosoriginais"))
e$monitora_output_organizar_produtos(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "fechamento_final_teste")
m <- mapa(); linha <- m[caminho_origem_relativo == origem_rel]; checar("Organizador real publicou mapa cumulativo", nrow(linha) == 1L)
raw <- file.path(e$MONITORA_OUTPUT_DIR, linha$fonte_preservada_relativa); edit <- file.path(e$MONITORA_OUTPUT_DIR, linha$destino_relativo)
editado <- fread(edit, colClasses = "character")
checar("Índice original preserva bytes anteriores a mover e rewrite", identical(hash(raw), sha) && identical(linha$sha256, sha))
checar("Derivado editorial tem hash próprio e links internos válidos", identical(hash(edit), linha$sha256_atual) && hash(edit) != sha && all(file.exists(file.path(e$MONITORA_OUTPUT_DIR, editado$caminho))) && all(startsWith(editado$caminho, "02_painel_correcoes/ocorrencias/pos_painel/")))
checar("Link externo aponta para derivado curto", identical(readLines(link), linha$destino_relativo))
checar("Canônico e dados de entrada continuam intactos", identical(hash(canonical), sha_canon) && identical(foto(e$MONITORA_INPUT_DIR), in1) && identical(foto(file.path(p, "dadosoriginais")), ori1))
ind <- fread(file.path(e$MONITORA_OUTPUT_DIR, "indice_produtos.csv"))
checar("Inventário real não conserva excesso nos legados", !any(ind[startsWith(caminho_relativo, "99_legacy_compat/"), comprimento_caminho_caracteres] > 210L))

# Mesmo caminho de origem, outra geração: o vínculo atual deve ser o novo,
# enquanto a fonte e o derivado da primeira geração permanecem acessíveis.
primeiro <- linha; gravar(indice, c("arquivo,caminho", paste0("novo.csv,", file.path(base, "novo.csv"))))
gravar(file.path(base, "novo.csv"), "novo artefato"); gravar(link, origem_rel)
e$MONITORA_EXEC_ID <- "20261111_170000"
e$monitora_output_organizar_produtos(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "fechamento_final_teste_source2")
m <- mapa(); atual <- m[caminho_origem_relativo == origem_rel][.N]
checar("Novo vínculo externo segue movimento atual comprovado", identical(readLines(link), atual$destino_relativo) && atual$destino_relativo != primeiro$destino_relativo)
checar("Histórico cumulativo conserva ambas as gerações e SHA", nrow(m[caminho_origem_relativo == origem_rel]) == 2L && identical(hash(file.path(e$MONITORA_OUTPUT_DIR, primeiro$fonte_preservada_relativa)), primeiro$sha256) && identical(hash(file.path(e$MONITORA_OUTPUT_DIR, primeiro$destino_relativo)), primeiro$sha256_atual))
hist <- data.table(caminho_origem_relativo = c("mesmo.csv", "mesmo.csv"), destino_relativo = c(primeiro$destino_relativo, atual$destino_relativo), hash_verificado = TRUE)
repetido <- hist[1L]
vinculo <- e$monitora_v307_caminhos_mapa_atual(hist, repetido)[caminho_origem_relativo == "mesmo.csv"]
checar("Vínculo atual A→B→A usa prova atual apesar da deduplicação antiga", nrow(vinculo) == 1L && identical(vinculo$destino_relativo, primeiro$destino_relativo))
e$MONITORA_OUTPUT_MAPA_MOVIMENTACOES <- data.table(); e$MONITORA_EXEC_ID <- "20261111_180000"; gravar(link, origem_rel)
e$monitora_output_organizar_produtos(e$MONITORA_OUTPUT_DIR, e$MONITORA_EXEC_ID, "fechamento_final_nova_sessao")
checar("Nova sessão com RAM vazia recupera vínculo durável sem novo sourcefile", identical(readLines(link), atual$destino_relativo))
map_path <- file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat/mapa.csv")
corrompido <- copy(m); corrompido[1L, fonte_preservada_relativa := "../input/original.csv"]; fwrite(corrompido, map_path)
checar("Finalizador recusa mapa apontando para entrada", grepl("amb.gua", erro(e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR))))
fwrite(m, map_path); raw <- file.path(e$MONITORA_OUTPUT_DIR, primeiro$fonte_preservada_relativa); gravar(raw, "alteracao indevida de fonte")
checar("Finalizador bloqueia fonte original adulterada", grepl("originais legados foram alterados", erro(e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR)), fixed = TRUE))
p <- nova(); decisao_rel <- sub("indice_ocorrencias.csv", "justificativas_pendencias_consolidada.csv", rel, fixed = TRUE)
decisao <- file.path(e$MONITORA_OUTPUT_DIR, decisao_rel); gravar(decisao, "Decisão literal: consultar correcoes_campos/relatorios_pre_painel/ocorrencias/indice_ocorrencias.csv")
sha <- hash(decisao); mov <- mover(decisao, decisao_rel)
map_ref <- data.table(caminho_origem = file.path(e$MONITORA_OUTPUT_DIR, "correcoes_campos/relatorios_pre_painel/ocorrencias/indice_ocorrencias.csv"),
  caminho_origem_relativo = "correcoes_campos/relatorios_pre_painel/ocorrencias/indice_ocorrencias.csv", destino_relativo = "02_painel_correcoes/ocorrencias/pre_painel/indice_ocorrencias.csv", hash_verificado = TRUE)
e$monitora_output_reescrever_referencias(e$MONITORA_OUTPUT_DIR, map_ref); e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR)
m <- mapa()
checar("Nome compacto mantém proteção da decisão original", identical(m$conteudo_imutavel, "TRUE") && identical(hash(file.path(e$MONITORA_OUTPUT_DIR, mov$destino_relativo)), sha))
gravar(file.path(e$MONITORA_OUTPUT_DIR, mov$destino_relativo), "decisao alterada indevidamente")
checar("Gate acusa alteração indevida de decisão compactada", grepl("editorialmente imut.vel", erro(e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR))))
p <- nova(); canon <- file.path(e$MONITORA_OUTPUT_DIR, "01_produtos_dados/registros_corrig.csv"); gravar(canon, "produto canonico")
sha <- hash(canon); msg <- erro(mover(canon))
checar("API recusa origem canônica mesmo com destino legado longo", grepl("produtos can.nicos", msg) && file.exists(canon) && identical(hash(canon), sha))
for (tipo in c("fora_output", "canonico_interno")) {
  p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "arquivo legado"); sha <- hash(origem)
  alvo <- if (tipo == "fora_output") file.path(p, "externo") else file.path(e$MONITORA_OUTPUT_DIR, "01_produtos_dados")
  dir.create(alvo, recursive = TRUE); link <- file.path(e$MONITORA_OUTPUT_DIR, "99_legacy_compat/c")
  dir.create(dirname(link), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(suppressWarnings(file.symlink(alvo, link)))) {
    msg <- erro(mover(origem))
    checar(paste("Alias de diretório recusado:", tipo), nzchar(msg) && identical(hash(origem), sha) && !length(list.files(alvo)))
  } else cat("SKIP: criação de symlink indisponível nesta plataforma\n")
}
p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "legado sem alias"); mover(origem)
m <- mapa(); destino <- file.path(e$MONITORA_OUTPUT_DIR, m$destino_relativo); raw <- file.path(e$MONITORA_OUTPUT_DIR, m$fonte_preservada_relativa); sha <- hash(raw)
unlink(destino)
if (isTRUE(suppressWarnings(file.symlink(raw, destino)))) {
  gravar(origem, "legado sem alias"); msg <- erro(mover(origem))
  checar("Alias interno derivado→raw recusado antes de qualquer edição", grepl("Aliases", msg) && identical(hash(raw), sha) && file.exists(origem))
  checar("Finalizador também recusa alias interno sem certificar raw", grepl("Aliases", erro(e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR))))
} else cat("SKIP: criação de symlink indisponível nesta plataforma\n")
p <- nova(); origem <- file.path(e$MONITORA_OUTPUT_DIR, rel); gravar(origem, "fonte editorial"); mover(origem)
m <- mapa(); gravar(file.path(e$MONITORA_OUTPUT_DIR, m$destino_relativo), "adulteracao antes da reescrita")
checar("Preparação bloqueia adulteração anterior ao rewriter", grepl("hash registrado", erro(e$monitora_v307_caminhos_preparar_edicao(e$MONITORA_OUTPUT_DIR)), fixed = TRUE))
checar("Finalizador não aceita edição sem prova explícita", grepl("prova expl.cita", erro(e$monitora_v307_caminhos_finalizar(e$MONITORA_OUTPUT_DIR))))
cat("PASS:", checks, "checks;", if (modulo) "pré-integração explicitamente adaptada" else "helper/hooks embutidos reais", "\n")
