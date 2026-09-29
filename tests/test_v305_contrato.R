source('tools/v305/carregar.R')
a<-monitora_v305_carregar('monitora_campsav_alvo_global_v3.0.4.R');b<-monitora_v305_carregar()
nomes<-intersect(ls(a),ls(b));nomes<-nomes[vapply(nomes,function(n)is.function(a[[n]])&&is.function(b[[n]]),logical(1))]
mudaram<-nomes[!vapply(nomes,function(n)identical(body(a[[n]]),body(b[[n]]))&&identical(formals(a[[n]]),formals(b[[n]])),logical(1))]
# Alias preservado pelo wrapper cartográfico: mesma inserção do inventário de siglas.
permitidas<-c('monitora_v31_render_base','monitora_relatorios_analiticos_renderizar_sentinel2','monitora_stat_definir_iteracoes_efetivas','monitora_editorial_testar_pareado_periodo_categoria','monitora_clima_inferencia','monitora_fogo_gerar','monitora_relatorios_analiticos_graficos_editoriais','monitora_v31_bibliografia','monitora_v31_docx_capa_original','monitora_relatorios_analiticos_conteudo_docx','monitora_relatorios_analiticos_docx_adequar_capa','monitora_relatorios_analiticos_renderizar','monitora_relatorios_analiticos_gerar','monitora_relatorios_analiticos_kable','monitora_relatorios_analiticos_catalogo_tabelas','monitora_relatorios_analiticos_html_colunas','monitora_relatorios_analiticos_html_mesclar_contexto','monitora_relatorios_analiticos_docx_preservar_linhas_tabela')
print(mudaram)
stopifnot(all(mudaram%in%permitidas),identical(a$monitora_validados_schema_embutido(),b$monitora_validados_schema_embutido()))
s<-readBin('R_monitora_campsav_alvo_global.R','raw',n=file.info('R_monitora_campsav_alvo_global.R')$size)
stopifnot(length(s)<5000000,length(s)+sum(s==as.raw(10))<5000000,identical(s,readBin('monitora_campsav_alvo_global_v3.0.5-rc01.R','raw',n=length(s))))
cat('PASS: funções alteradas limitadas à lista editorial revisada; contrato de dados e alias preservados; LF/CRLF <5MB.\n')
