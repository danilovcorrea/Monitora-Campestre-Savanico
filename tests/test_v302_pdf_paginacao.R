source('tests/helpers_v302.R');e<-monitora_v302_carregar()$env
root<-normalizePath('artifacts/v302_numeracao')
for(tipo in c('sintetico','detalhado')) {
 dir<-file.path(root,paste0('pnb_',tipo));arq<-file.path(dir,paste0('pnb_',tipo,'_v302.html'))
 a<-data.table::fread(file.path(dir,paste0('auditoria_numeracao_pnb_',tipo,'_v302.csv')),colClasses=c(numero='character'),na.strings='')
 if(length(xml2::xml_find_all(xml2::read_html(arq),".//table/caption")))e$monitora_relatorios_analiticos_html_legendas_tabelas(arq)
 e$monitora_relatorios_analiticos_auditar_numeracao_formato(arq,a)
 pdf<-sub('[.]html$','.pdf',arq)
 r<-e$monitora_relatorios_analiticos_chrome_print_isolado(arq,pdf,browser='/usr/bin/google-chrome',timeout=300)
 print(r[c('ok','status_processo','tamanho_bytes')]);stopifnot(r$ok)
}
