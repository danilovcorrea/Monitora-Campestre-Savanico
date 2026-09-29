from pathlib import Path
import re,base64,gzip,textwrap,hashlib,json
root=Path(__file__).resolve().parents[1];out=root/'artifacts/v305';out.mkdir(parents=True,exist_ok=True)
s=(root/'monitora_campsav_alvo_global_v3.0.4.R').read_text()
def change(a,b,n=1):
 global s
 assert s.count(a)==n,(a[:100],s.count(a));s=s.replace(a,b)
change('# Versão 3.0.4 —','# Versão 3.0.5-rc01 —')
change('MONITORA_SCRIPT_VERSAO <- "3.0.4"','MONITORA_SCRIPT_VERSAO <- "3.0.5-rc01"')
change('MONITORA_SCRIPT_BUILD_ID <- "v3.0.4-20260925-r01"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.5-rc01-20260929-r10"')
# Manter matriz tabular e parágrafos reais no Word.
change('    if (length(cab) <= 6L) return(bloco)','    return(bloco)')
change('    return(c(paste0("> ", paragrafos), ""))','    return(c(paste(paragrafos, collapse="\\n\\n"), ""))')
change('  editorial <- monitora_relatorios_analiticos_numerar(conteudo, dir_relatorio)','  conteudo <- monitora_v305_editorial(conteudo, dir_relatorio, base_nome)\n  editorial <- monitora_relatorios_analiticos_numerar(conteudo, dir_relatorio)')
change('    if(nrow(visitas_obj$modelos))paste0("Calendário: ",paste(unique(visitas_obj$modelos$motivo),collapse="; "),"."))','    if(nrow(visitas_obj$modelos))monitora_v305_calendario(visitas_obj$modelos))')
# Métrica repetida por achado: evita depender do título de um bloco em outra página.
change('paste0("- ", vapply(seq_len(nrow(z)), function(ii) frase_achado_item(z[ii]), character(1L)))','paste0("- **", z$tipo_metrica_label, "** — ", vapply(seq_len(nrow(z)), function(ii) frase_achado_item(z[ii]), character(1L)))')
# Uma figura por tema/métrica, com todos os resultados na página em retrato.
a=s.index('    dados_integrais <- data.table::copy(dados_plot)');b=s.index('    audit <- data.table::copy(dados_plot)',a)
s=s[:a]+'''    arquivo <- monitora_relatorios_analiticos_caminho_figura(dir_figuras,paste0("evidencia_estatistica_",id,".png"))
    monitora_v305_figura_evidencia(dados_plot,titulo,arquivo,paleta,cfg_num)
'''+s[b:]
a=s.index('  painel_inferencial <- function(');b=s.index('  temas_inferenciais <-',a)
z=s[a:b];assert z.count('    }\n    invisible(NULL)')==1
z=z.replace('    }\n    invisible(NULL)','    invisible(NULL)');s=s[:a]+z+s[b:]
# A capa é a primeira seção; páginas paisagem não devem receber rodapé de capa.
a=s.index('monitora_relatorios_analiticos_docx_adequar_capa <- function(');b=s.index('\nmonitora_relatorios_analiticos_conteudo_docx <-',a)
z=s[a:b];assert 'for (secao in secoes)' in z;z=z.replace('for (secao in secoes)','for (secao in head(secoes,1L))');s=s[:a]+z+s[b:]
change('"Estado anual das categorias gerais"','"Cobertura vegetal anual das categorias gerais"')
change('"Linha de base das categorias gerais"','"Cobertura vegetal das categorias gerais na linha de base"')
# A comparação editorial por período também deve cumprir o contrato de Monte Carlo.
a=s.index('monitora_stat_definir_iteracoes_efetivas <- function(');b=s.index('\n`%||%` <-',a)
s=s[:a]+'''monitora_stat_definir_iteracoes_efetivas <- function(n_solicitado, tipo = c("perm", "boot"), etapa = "estatistica", risco = "normal", objeto = NULL) {
  tipo<-match.arg(tipo)
  n_solicitado<-as.integer(n_solicitado)
  if(isTRUE(MONITORA_STAT_RECURSOS_ADAPTATIVO))monitora_stat_controlar_recursos_execucao(etapa,risco=risco,objeto=objeto,force_log=FALSE)
  # O perfil de recursos regula a execução; não altera a precisão científica solicitada.
  n_solicitado
}'''+s[b:]
change('message("[estatistica_mudanca] ", MONITORA_STAT_SEMENTE_MSG)','message("[estatistica_mudanca] ", MONITORA_STAT_SEMENTE_MSG)\nmessage("[estatistica_mudanca] Reamostragens configuradas preservadas em todos os perfis de recursos; testes exatos e casos degenerados mantêm seus tratamentos próprios.")')
change('monitora_editorial_testar_pareado_periodo_categoria <- function(long_dt) {', '''monitora_editorial_testar_pareado_periodo_categoria <- function(long_dt) {
  if(isTRUE(MONITORA_STAT_REPRODUTIBILIDADE_ATIVA)) {
    seed_antes<-get0(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
    on.exit({if(is.null(seed_antes)){if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",seed_antes,envir=.GlobalEnv)},add=TRUE)
  }''')
change('''    ci <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_calcular_ic_bootstrap(dif) else c(NA_real_, NA_real_)
    p_val <- if (n_pares >= MONITORA_STAT_MIN_PARES) monitora_stat_calcular_p_permutacao_pareada(dif) else NA_real_''','''    stat_id<-paste(g$grupo_grafico,g$tipo_metrica,g$form_veg,g$categoria,y1,y2,sep="|")
    ci <- if (n_pares >= MONITORA_STAT_MIN_PARES) {
      monitora_stat_ativar_semente("categoria_periodo_editorial_boot",stat_id)
      monitora_stat_calcular_ic_bootstrap(dif)
    } else c(NA_real_, NA_real_)
    p_val <- if (n_pares >= MONITORA_STAT_MIN_PARES) {
      monitora_stat_ativar_semente("categoria_periodo_editorial_perm",stat_id)
      monitora_stat_calcular_p_permutacao_pareada(dif)
    } else NA_real_''')
change('    titulo_atual <- p$labels$title\n    subtitulo_atual <- p$labels$subtitle', '''    titulo_atual <- p$labels$title
    eixo <- as.character(p$labels$y)
    if(length(eixo)==1L && length(titulo_atual)==1L) {
      metrica <- if(grepl("Proporção",eixo,ignore.case=TRUE)) "Proporção relativa" else if(grepl("Cobertura",eixo,ignore.case=TRUE)) "Cobertura vegetal" else ""
      if(nzchar(metrica) && !grepl(metrica,titulo_atual,ignore.case=TRUE,fixed=FALSE))titulo_atual<-paste0(metrica," — ",titulo_atual)
    }
    subtitulo_atual <- p$labels$subtitle''')

a=s.index('monitora_relatorios_analiticos_renderizar_sentinel2 <- function(')
b=s.index('\nmonitora_relatorios_analiticos_selecionar_candidato_sentinel <- function(',a)
z=s[a:b]
old='  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)'
assert z.count(old)==1
z=z.replace(old,'  siglas_uf_emitidas <- character()\n'+old)
old='      xy_estados <- terra::crds(centros_estados)'
assert z.count(old)==1
z=z.replace(old,old+'\n      siglas_uf_emitidas <- as.character(centros_estados$SIGLA_UF)')
old='  attr(ok, "metadados_cartograficos") <- metadados'
assert z.count(old)==1
z=z.replace(old,'''  if (isTRUE(ok)) monitora_v305_registrar_siglas_figura(
    destino,
    monitora_v305_inventariar_siglas_mapa(
      paragrafos_quadro, siglas_uf_emitidas, logos = "MMA"
    )
  )
'''+old)
s=s[:a]+z+s[b:]

# Definições do módulo público, com sobreposições editoriais ao final.
start=s.index('eval(parse(text=rawToChar(memDecompress(jsonlite::base64_dec(paste0(');pos=s.index('\n',start)+1;chunks=[]
while True:
 m=re.match(r'"([A-Za-z0-9+/=]+)"',s[pos:])
 if not m:break
 chunks.append(m[1]);last=pos+m.end();pos=s.index('\n',pos)+1
module=gzip.decompress(base64.b64decode(''.join(chunks))).decode();original=module
assert module.count('  xml2::write_xml(doc,arq)')==1
module=module.replace('  xml2::write_xml(doc,arq)','  monitora_v305_ordenar_ooxml(doc)\n  xml2::write_xml(doc,arq)')
old='paste(unique(c(motivos_global,if(nrow(dg))dg$motivo)),collapse="; ")';assert old in module;module=module.replace(old,'monitora_v305_motivos(c(motivos_global,if(nrow(dg))dg$motivo))')
old='"A tabela descreve campanhas';assert old in module;module=module.replace(old,'if(any(por_modalidade$indicador %in% c("nativa","seca_morta"))) "A [[tabela:fogo-cobertura]] descreve campanhas')
end='As demais categorias e as linhas individuais permanecem nos CSVs editáveis.",';assert end in module;module=module.replace(end,end[:-1]+' else "Não foi gerada a tabela de cobertura por modalidade: não há coocorrências elegíveis para os indicadores apresentados.",')
module=module.replace('if (nrow(por_modalidade)) monitora_relatorios_analiticos_kable','if (any(por_modalidade$indicador %in% c("nativa","seca_morta"))) monitora_relatorios_analiticos_kable')
# URL codificada, rótulo com colchetes balanceados sem ativar TeX.
module=module.replace('87\\\\[2614:VPOSDM\\\\]2.0.CO;2','87%5B2614:VPOSDM%5D2.0.CO;2')
module=module.replace('[https://doi.org/10.1890/0012-9658(2006)87%5B2614:VPOSDM%5D2.0.CO;2]', '[10.1890/0012-9658(2006)87[2614:VPOSDM]2.0.CO;2]')
a=module.index('        pp<-ggplot2::ggplot(centro,ggplot2::aes(Eixo1,Eixo2')
b=module.index('        mv<-c("## Trajetória',a)
module=module[:a]+'''        f2<-file.path(dir_figuras,"multivariada_trajetoria_eixos_comuns.png")
        monitora_v305_figura_trajetoria(centro,100*pc$sdev[1:2]^2/sum(pc$sdev^2),f2)
'''+module[b:]
for name in ['siglas_figuras.R','editorial_aux.R','editorial.R','layout.R','evidencia.R','trajetoria.R']:
 module+='\n'+(root/'tools/v305'/name).read_text()
enc=base64.b64encode(gzip.compress(module.encode(),9,mtime=0)).decode()
s=s[:s.index('\n',start)+1]+',\n'.join('"'+v+'"' for v in textwrap.wrap(enc,12000))+s[last:]
for name in ['R_monitora_campsav_alvo_global.R','monitora_campsav_alvo_global_v3.0.5-rc01.R']:(root/name).write_text(s)
(out/'modulos_base.R').write_text(original);(out/'modulos_final.R').write_text(module)
n=len(s.encode());crlf=n+s.count('\n');assert crlf<5000000,(n,crlf)
(out/'BUILD.json').write_text(json.dumps({'bytes_lf':n,'bytes_crlf':crlf,'sha256':hashlib.sha256(s.encode()).hexdigest(),'modulo_sha256':hashlib.sha256(module.encode()).hexdigest()},indent=2));print('PASS build',n,crlf)
