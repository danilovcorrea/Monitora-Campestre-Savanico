suppressMessages(library(chromote))
b <- ChromoteSession$new()
b$Page$navigate("http://127.0.0.1:38765/")
Sys.sleep(3)
res <- b$Runtime$evaluate('document.title')
cat("titulo pagina:", res$result$value, "\n")

# lista botoes disponiveis (id + texto) para confirmar antes de clicar
btns <- b$Runtime$evaluate('
  JSON.stringify(Array.from(document.querySelectorAll("button")).map(function(x){return {id:x.id, text:x.innerText};}))
')
cat("botoes encontrados:\n", btns$result$value, "\n")

# clica no botao real "cancelar" (rotulo "Fechar painel sem salvar e continuar script")
clique <- b$Runtime$evaluate('
  (function(){
    var el = document.getElementById("cancelar");
    if (!el) return "NAO_ENCONTRADO";
    el.click();
    return "CLICADO";
  })()
')
cat("resultado clique 1 (cancelar):", clique$result$value, "\n")
Sys.sleep(2)

# apos o clique em "cancelar", o codigo (linha ~23291) pode abrir modal de
# confirmacao se houver ocorrencias impeditivas pendentes. Verifica e clica
# no botao de confirmacao se existir.
btns2 <- b$Runtime$evaluate('
  JSON.stringify(Array.from(document.querySelectorAll("button")).map(function(x){return {id:x.id, text:x.innerText};}))
')
cat("botoes apos 1o clique:\n", btns2$result$value, "\n")

clique2 <- b$Runtime$evaluate('
  (function(){
    var el = document.getElementById("confirmar_encerrar_sem_materializar") || document.getElementById("confirmar_fechar_sem_salvar_checkpoint_pendente");
    if (!el) return "NAO_ENCONTRADO";
    el.click();
    return "CLICADO:" + el.id;
  })()
')
cat("resultado clique 2 (confirmacao):", clique2$result$value, "\n")
Sys.sleep(3)

b$close()
cat("FIM_CHROMOTE\n")
