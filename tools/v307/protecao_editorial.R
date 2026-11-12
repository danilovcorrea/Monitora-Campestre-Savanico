# Decisões e componentes assinados são dados de origem, não referências
# editoriais. O organizador pode mover esses arquivos preservando seus bytes.
monitora_v307_editorial_imutavel <- function(caminhos) {
  p <- tolower(gsub("\\\\", "/", as.character(caminhos)))
  nome <- sub("^.*/", "", p)
  diretorio_protegido <- grepl("(^|/)(linhagem|justificativas|justificativas_fontes)(/|$)", p)
  justificativa <- grepl("justificativ|^pendencias_justificadas([_.]|$)", nome)
  decisao_operacional <- grepl(
    "^(correcoes_semanticas|correcoes_campos|correcoes_espaciais|aplicacoes_correcoes|metadados_sessao_painel)([_.]|$)",
    nome
  )
  resultado <- diretorio_protegido | justificativa | decisao_operacional
  resultado[is.na(p) | !nzchar(p)] <- TRUE
  resultado
}

monitora_v307_editorial_candidatos_mutaveis <- function(candidatos) {
  candidatos[!monitora_v307_editorial_imutavel(candidatos)]
}
