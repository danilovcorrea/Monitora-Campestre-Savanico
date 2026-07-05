#!/usr/bin/env Rscript
# Interage de verdade (Chrome headless via chromote) com o runApp() real
# iniciado por rodar_caso_app.R, executando a sequencia de cliques do caso
# A/B/C/D da matriz 2x2. Mesma familia de tecnica validada em
# diagnostics/validacao_runapp_sintetico_h2r_c_20260705_093608/chromote_fechar_gracioso.R
# e chromote_salvar.R.
#
# Uso: Rscript rodar_caso_chromote.R <caso A|B|C|D> <porta>
#
# So deve ser executado localmente pelo usuario (Fedora), nunca pelo motor
# de IA, e somente depois que rodar_caso_app.R ja estiver com o servidor
# Shiny real no ar na porta informada.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("uso: rodar_caso_chromote.R <caso> <porta>")
caso <- args[[1]]
porta <- args[[2]]
url <- paste0("http://127.0.0.1:", porta, "/")

suppressMessages(library(chromote))
b <- ChromoteSession$new()
b$Page$navigate(url)
Sys.sleep(3)

titulo <- b$Runtime$evaluate("document.title")
cat("titulo pagina:", titulo$result$value, "\n")

clicar <- function(id) {
  res <- b$Runtime$evaluate(sprintf('
    (function(){
      var el = document.getElementById("%s");
      if (!el) return "NAO_ENCONTRADO:%s";
      el.click();
      return "CLICADO:%s";
    })()
  ', id, id, id))
  cat("clique[", id, "]: ", res$result$value, "\n", sep = "")
  invisible(res$result$value)
}

## Para os casos B/D ("com operacoes no painel"): adiciona uma correcao real
## simples (botao real "add_corr") antes do fechar/salvar, usando o unico
## atributo editavel garantidamente presente no dataset sintetico
## (`forma_vida_nativa_arvore_abaixo`, ja validado na rodada anterior). Nao
## foi usado o fluxo "add_mv" (movimento de forma de vida) porque ele exige
## colunas-lista principais de origem/destino (ex.:
## amostragem/registro/forma_vida_exotica) que o dataset sintetico atual nao
## define; o clique falharia silenciosamente (script.R, monitora_correcao_coluna_forma_vida,
## linha ~3082, retornaria NA e a operacao seria bloqueada).
##
## Em vez de adivinhar o valor interno exato do atributo no selectizeInput
## "atributo" (a transformacao de nome de coluna passa pelo dicionario/
## contrato interno do painel e pode nao ser identica ao caminho XLSForm
## original), o valor real e DESCOBERTO em tempo de execucao consultando a
## instancia selectize.js viva do controle (`el.selectize.options`), e nao o
## <select> nativo subjacente. A tentativa anterior lia `sel.options`
## assumindo que o selectize.js mantinha todas as <option> originais no
## <select> oculto; na pratica o selectize.js faz `detach()` das <option> ao
## inicializar e so devolve ao <select> nativo a(s) opcao(oes) atualmente
## selecionada(s) (para submissao nativa de formulario), entao a busca por
## substring em `sel.options` so enxerga a opcao default (a primeira em
## ordem_template_sismonitora) e falha para qualquer outro atributo -- foi
## isso que causou "atributo real encontrado no DOM: (NENHUM)" nos casos B/D
## da primeira rodada (diagnostics/validacao_runapp_matriz_2x2_20260705_095601/
## RESULTADO_MATRIZ_2X2.json). A API `el.selectize.options` guarda todas as
## opcoes carregadas pelo widget independentemente da selecao atual, o que
## torna o driver robusto a variacoes de formatacao do nome, em vez de um
## valor fixo adivinhado. Os demais inputs (acao/valor_novo/motivo/
## confirmar_abrangencia) usam ids estaveis e confirmados no codigo-fonte da
## UI (script.R, linhas ~19022-19038), validos independentemente do tipo de
## controle renderizado dinamicamente para "valor_novo" (uiOutput
## ui_valor_novo_controle sempre vincula ao id "valor_novo", script.R linha
## ~19276).
##
## Se a precondicao falhar (atributo nao encontrado ou operacao nao aceita
## pelo contrato), o driver reporta isso explicitamente no log; o
## orquestrador (executar_matriz_2x2_local.R) distingue esse caso
## ("precondicao_operacao_ok = FALSE") de uma falha real da correcao testada.
adicionar_operacao_real <- function() {
  set_input <- function(id, valor, tipo = "auto") {
    js_valor <- if (identical(tipo, "bool")) tolower(as.character(valor)) else sprintf('"%s"', valor)
    b$Runtime$evaluate(sprintf('Shiny.setInputValue("%s", %s, {priority: "event"});', id, js_valor))
  }

  set_input("coleta", "SINTETICO-001")
  Sys.sleep(1.5)

  achar_atributo <- b$Runtime$evaluate('
    (function(){
      var sel = document.getElementById("atributo");
      if (!sel) return "";
      if (sel.selectize && sel.selectize.options) {
        var opts = sel.selectize.options;
        for (var k in opts) {
          if (Object.prototype.hasOwnProperty.call(opts, k) && k.indexOf("forma_vida_nativa_arvore_abaixo") !== -1) return k;
        }
        return "";
      }
      for (var i=0;i<sel.options.length;i++){
        if (sel.options[i].value.indexOf("forma_vida_nativa_arvore_abaixo") !== -1) return sel.options[i].value;
      }
      return "";
    })()
  ')
  valor_atributo <- achar_atributo$result$value
  cat("atributo real encontrado no DOM:", if (nzchar(valor_atributo)) valor_atributo else "(NENHUM)", "\n")
  if (!nzchar(valor_atributo)) {
    cat("PRECONDICAO_FALHOU: atributo forma_vida_nativa_arvore_abaixo nao encontrado no select real.\n")
    return(invisible("PRECONDICAO_FALHOU:ATRIBUTO_NAO_ENCONTRADO"))
  }

  set_input("atributo", valor_atributo)
  Sys.sleep(1.5)

  ## Iteracao 3 (log_caso_{B,D}_chromote.log, NOTIFICACAO_APOS_ADD_CORR):
  ## a selecao do atributo acima dispara observeEvent(input$atributo) no
  ## script de producao (~linha 21720-21747), que muda automaticamente o
  ## radio "escopo" para "ponto" porque o dicionario sugere escopo pontual
  ## para forma_vida_nativa_arvore_abaixo e nenhum "ponto" foi selecionado.
  ## O dataset sintetico usado aqui tem exatamente 1 linha por COLETA e o
  ## driver nao seleciona um ponto amostral real, entao manter escopo="ponto"
  ## faz o observeEvent(input$add_corr) rejeitar a operacao em
  ## "Selecione um ponto amostral ou use escopo de coleta inteira." (~linha
  ## 22274-22277) -- exatamente a mensagem capturada na notificacao. Forcar
  ## escopo="coleta_inteira" aqui e uma das duas opcoes aceitas pela propria
  ## mensagem de validacao; o observeEvent(input$escopo) correspondente
  ## (~linha 21775) recalcula escopo_efetivo e reajusta n_esperado para 101
  ## automaticamente, e confirmar_abrangencia=TRUE (setado abaixo) cobre o
  ## descompasso entre 101 esperado e a 1 linha real da COLETA sintetica.
  set_input("escopo", "coleta_inteira")
  Sys.sleep(1)

  set_input("acao", "update")
  Sys.sleep(1)
  set_input("valor_novo", "1")
  Sys.sleep(0.5)
  set_input("motivo", "harness matriz 2x2 - operacao sintetica de teste")
  Sys.sleep(0.5)
  set_input("confirmar_abrangencia", TRUE, tipo = "bool")
  Sys.sleep(1)

  r <- clicar("add_corr")
  Sys.sleep(2)

  ## Diagnostico adicional: varias validacoes do observeEvent(input$add_corr)
  ## no script de producao retornam cedo emitindo apenas
  ## monitora_painel_notificar(..., type = "error") (shiny::showNotification),
  ## sem nenhuma linha em monitora_correcao_console_msg. Isso torna
  ## precondicao_operacao_ok=FALSE indistinguivel, so pelo log do app, de uma
  ## rejeicao silenciosa vs. um problema no proprio driver. Como
  ## showNotification sempre renderiza ".shiny-notification-content" no DOM,
  ## capturamos aqui o texto de qualquer notificacao visivel logo apos o
  ## clique, para que a causa raiz fique registrada no log do chromote sem
  ## precisar adivinhar qual validacao no script de producao foi acionada.
  notificacoes <- b$Runtime$evaluate('
    (function(){
      var els = document.querySelectorAll(".shiny-notification-content");
      var textos = [];
      for (var i=0;i<els.length;i++){ textos.push(els[i].innerText || els[i].textContent || ""); }
      return textos.join(" || ");
    })()
  ')
  texto_notificacoes <- notificacoes$result$value
  cat("NOTIFICACAO_APOS_ADD_CORR:", if (nzchar(texto_notificacoes)) texto_notificacoes else "(NENHUMA)", "\n")

  invisible(r)
}

resultado_sequencia <- character(0)

if (identical(caso, "A")) {
  resultado_sequencia <- c(resultado_sequencia, "A: sem operacoes no painel; fechar sem salvar")
  resultado_sequencia <- c(resultado_sequencia, clicar("cancelar"))
  Sys.sleep(2)
  resultado_sequencia <- c(resultado_sequencia, clicar("confirmar_encerrar_sem_materializar"))
} else if (identical(caso, "B")) {
  resultado_sequencia <- c(resultado_sequencia, "B: com operacoes no painel; fechar sem salvar")
  resultado_sequencia <- c(resultado_sequencia, adicionar_operacao_real())
  resultado_sequencia <- c(resultado_sequencia, clicar("cancelar"))
  Sys.sleep(2)
  resultado_sequencia <- c(resultado_sequencia, clicar("confirmar_encerrar_sem_materializar"))
} else if (identical(caso, "C")) {
  resultado_sequencia <- c(resultado_sequencia, "C: sem operacoes no painel; salvar checkpoint com pendencias")
  resultado_sequencia <- c(resultado_sequencia, clicar("salvar"))
  Sys.sleep(2)
  resultado_sequencia <- c(resultado_sequencia, clicar("confirmar_salvar_checkpoint_pendente"))
} else if (identical(caso, "D")) {
  resultado_sequencia <- c(resultado_sequencia, "D: com operacoes no painel; salvar checkpoint com pendencias")
  resultado_sequencia <- c(resultado_sequencia, adicionar_operacao_real())
  resultado_sequencia <- c(resultado_sequencia, clicar("salvar"))
  Sys.sleep(2)
  resultado_sequencia <- c(resultado_sequencia, clicar("confirmar_salvar_checkpoint_pendente"))
} else {
  stop("caso invalido: ", caso, " (esperado A, B, C ou D)")
}

Sys.sleep(3)
cat("SEQUENCIA_CLIQUES:", paste(resultado_sequencia, collapse = " | "), "\n")

try(b$close(), silent = TRUE)
cat("FIM_CHROMOTE_CASO_", caso, "\n", sep = "")
