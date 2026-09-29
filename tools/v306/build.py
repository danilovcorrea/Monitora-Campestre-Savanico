from pathlib import Path
import re,base64,gzip,lzma,textwrap,hashlib,json
root=Path(__file__).resolve().parents[2]
s=(root/'monitora_campsav_alvo_global_v3.0.5.R').read_text()
def replace(a,b):
 global s
 assert s.count(a)==1,(a[:90],s.count(a));s=s.replace(a,b)
replace('# Versão 3.0.5 —','# Versão 3.0.6-rc01 —')
replace('MONITORA_SCRIPT_VERSAO <- "3.0.5"','MONITORA_SCRIPT_VERSAO <- "3.0.6-rc01"')
replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.5-20260929-r01"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.6-rc01-20260929-r04"')
replace('  conteudo <- monitora_v305_editorial(conteudo, dir_relatorio, base_nome)','  conteudo <- monitora_v306_limpar_notas(conteudo, dir_relatorio)\n  conteudo <- monitora_v306_editorial(conteudo, dir_relatorio)\n  conteudo <- monitora_v306_amplo(conteudo, dir_relatorio)\n  conteudo <- monitora_v305_editorial(conteudo, dir_relatorio, base_nome)\n  conteudo <- monitora_v306_notas_finais(conteudo)')
replace('paste0("Clima e trajetória: ",complementos_obj$resumo)','paste0("Clima — transições entre coletas: ",complementos_obj$resumo)')
replace('  composicao_linha_base <- trocar_material(composicao_linha_base, material_documentado$comparacoes$composicao_base)', '  composicao_linha_base <- trocar_material(composicao_linha_base, material_documentado$comparacoes$composicao_base)\n  data.table::fwrite(mudanca_periodo, file.path(dir_relatorio, \"nar_contrastes.csv\"), bom=TRUE)\n  data.table::fwrite(composicao_periodo, file.path(dir_relatorio, \"nar_composicao.csv\"), bom=TRUE)\n  data.table::fwrite(data.table::data.table(fogo=fogo_obj$status, clima=clima_obj$status), file.path(dir_relatorio, \"nar_contexto.csv\"), bom=TRUE)')
replace('    p_mud <- ggplot2::ggplot(', '    data.table::fwrite(mud, file.path(dirname(dir_figuras), "nar_prioritarias.csv"), bom=TRUE)\n    p_mud <- ggplot2::ggplot(')
s=s.replace('Associação entre calendário e médias anuais', 'Associação entre época e médias anuais').replace('calendário concentrado\"', 'época concentrada\"')
a=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(');b=a+s[a:].index('\n')+1;pos=b;parts=[]
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 parts.append(m[1]);last=pos+m.end();pos=s.index('\n',pos)+1
module=gzip.decompress(base64.b64decode(''.join(parts))).decode()
def hook(a,b):
 global module
 assert module.count(a)==1,(a[:80],module.count(a));module=module.replace(a,b)
hook('  if(is.data.frame(x)) {x<-as.data.frame(x);names(x)<-monitora_v305_rotulo_coluna(names(x))}', '''  if(is.data.frame(x)) {
    x<-as.data.frame(x);names(x)<-monitora_v305_rotulo_coluna(names(x))
    for(j in which(names(x)%in%c("Formação","formacao"))) {
      v<-as.character(x[[j]]);ch<-tolower(iconv(trimws(v),from="",to="ASCII//TRANSLIT"))
      hit<-!is.na(ch)&ch%in%c("campestre","savanica")
      v[hit]<-monitora_relatorio_rotulo_formacao(v[hit],TRUE);x[[j]]<-v
    }
  }''')
hook('th{font-size:8.5pt!important}', 'th{font-size:8.5pt!important;vertical-align:middle!important}')
hook('  salvar(w,"painel_selecionado")','  salvar(w,"painel_selecionado")\n  monitora_v306_meta_painel(w,es,blocos,dir_relatorio)')
hook('    salvar(pontos,"ordenacao_exploratoria")','    salvar(pontos,"ordenacao_exploratoria")\n    monitora_v306_meta_ordenacao(s,Y,dir_relatorio)')
hook('        centro[,form_veg:=monitora_relatorio_rotulo_formacao(form_veg,TRUE)]','        monitora_v306_meta_trajetoria(pc,w,ww,centro,dir_relatorio)\n        centro[,form_veg:=monitora_relatorio_rotulo_formacao(form_veg,TRUE)]')
hook('  linhas<-c("# Análise multivariada integrada da cobertura vegetal","",','  unlink(list.files(dir_relatorio,pattern="^mv_(painel|mudancas|blocos|ord_).*csv$",full.names=TRUE))\n  linhas<-c("# Análise multivariada integrada da cobertura vegetal","",')
hook('monitora_v30_complementos <- function(dir_relatorio,dir_figuras) {','monitora_v30_complementos <- function(dir_relatorio,dir_figuras) {\n  unlink(list.files(dir_relatorio,pattern="^mv_traj_.*csv$",full.names=TRUE))')
hook('paste(tabela_defs,collapse="; ")', 'if(identical(tabela_id,"esforco-percentuais"))monitora_v306_nota_esforco() else paste(tabela_defs,collapse="; ")')
hook('paste(tabela_defs,collapse="; "),"."),"")', 'paste(tabela_defs,collapse="; "),". <!-- monitora-nota-automatica -->"),"")')
hook('nota_elemento<-paste0("Nota da [[figura:",id,"]]: ",defs,".")', 'nota_elemento<-paste0("Nota da [[figura:",id,"]]: ",defs,". <!-- monitora-nota-automatica -->")')
hook("        for(run in xml2::xml_find_all(c,'.//w:r',ns)){rp2<-prop(run,'rPr');", "        if(i==1L)for(hp in xml2::xml_find_all(c,'./w:p',ns))add(prop(hp,'pPr'),'<w:keepNext/>')\n        for(run in xml2::xml_find_all(c,'.//w:r',ns)){rp2<-prop(run,'rPr');")
module=module.replace('Calendário observado por coleta','Época de amostragem por coleta').replace('revisitas preservadas','reamostragens preservadas')
module+='\n'+(root/'tools/v306/narrativa.R').read_text()+'\n'+(root/'tools/v306/editorial_amplo.R').read_text()
enc=base64.b64encode(lzma.compress(module.encode(),preset=9)).decode()
s=s[:b]+',\n'.join('"'+x+'"'for x in textwrap.wrap(enc,12000))+s[last:].replace('type="gzip"','type="xz"',1)
n=len(s.encode());crlf=n+s.count('\n');assert crlf<5_000_000,(n,crlf)
p=root/'monitora_campsav_alvo_global_v3.0.6-rc01.R';p.write_text(s)
out=root/'artifacts/v306';out.mkdir(exist_ok=True,parents=True);(out/'modulo.R').write_text(module);(out/'BUILD.json').write_text(json.dumps(dict(bytes_lf=n,bytes_crlf=crlf,sha256=hashlib.sha256(s.encode()).hexdigest()),indent=2));print('PASS',n,crlf)
