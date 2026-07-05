suppressMessages(library(chromote))
b <- ChromoteSession$new()
b$Page$navigate("http://127.0.0.1:38765/")
Sys.sleep(3)
clique <- b$Runtime$evaluate('
  (function(){
    var el = document.getElementById("salvar");
    if (!el) return "NAO_ENCONTRADO";
    el.click();
    return "CLICADO";
  })()
')
cat("resultado clique salvar:", clique$result$value, "\n")
Sys.sleep(2)
btns2 <- b$Runtime$evaluate('
  JSON.stringify(Array.from(document.querySelectorAll("button")).map(function(x){return {id:x.id, text:x.innerText};}))
')
cat("botoes apos clique salvar:\n", btns2$result$value, "\n")
clique2 <- b$Runtime$evaluate('
  (function(){
    var candidatos = ["confirmar_salvar_checkpoint_pendente","confirmar_encerrar_sem_materializar","confirmar_fechar_sem_salvar_checkpoint_pendente"];
    for (var i=0;i<candidatos.length;i++){
      var el = document.getElementById(candidatos[i]);
      if (el) { el.click(); return "CLICADO:" + candidatos[i]; }
    }
    return "NENHUM_MODAL_CONFIRMACAO_ENCONTRADO";
  })()
')
cat("resultado clique confirmacao:", clique2$result$value, "\n")
Sys.sleep(3)
b$close()
cat("FIM_CHROMOTE_SALVAR\n")
