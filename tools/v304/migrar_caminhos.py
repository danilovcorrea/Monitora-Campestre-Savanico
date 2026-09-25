"""Migração de produtos existentes; sem executar R, renderizar ou recalcular análises.
Origem sempre preservada. A lista completa e todos os hashes ficam no manifesto.
"""
from pathlib import Path
import re,sys,json,hashlib,shutil,csv,io,zipfile,urllib.parse
DIRS={}
NOMES={'aud_pipes_registros_importados_operacional_':'aud_pipes_importados_','aud_pipe_registros_importados_operacional_':'aud_pipe_importados_','p_pos_export_pre_analises_registros_corrig_':'p_pre_analises_','ordem_contrato_checkpoint2_pos_tokenizacao_':'ordem_contrato_pos_tokens_'}

NOMES.update({'resumo_formas_vida_desconhecida_por_unidade':'desconhecida_por_unidade','correcoes_semanticas_sessao_painel_incremental_':'correcoes_painel_incremental_'})
NOMES['resumo_pipes_contrato_pos_export_registros_corrig_']='resumo_pipes_pos_export_'
NOMES.update({'resumo_ordem_checkpoint2_pos_tokenizacao_': 'resumo_ordem_pos_tokens_', 'auditoria_resolucao_colunas_estruturais_estatisticas': 'aud_resolucao_colunas_estatisticas', 'material_botanico_comparacoes_elegiveis_composicao_base': 'material_botanico_comp_elegiveis_base'})
def sha(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for b in iter(lambda:f.read(4194304),b''):h.update(b)
 return h.hexdigest()
def protegido(rel):
 return any(rel.startswith(p) for p in ('input/','qfield_input/','cache_fogo_icmbio/','output/01_produtos_dados/','output/02_painel_correcoes/linhagem/','output/90_cache/'))
def limite(p):
 return 210 if p.suffix.lower() in ('.csv','.xls','.xlsx') else 240 if p.suffix.lower() in ('.pdf','.doc','.docx','.html','.htm') else 259

def plano(src):
 result={}
 for p in sorted(src.rglob('*')):
  if not p.is_file():continue
  rel=p.relative_to(src).as_posix();name=p.name
  if not protegido(rel) and rel.startswith(('output/','log/')):
   for old,new in NOMES.items():
    if name.startswith(old):name=new+name[len(old):];break
  result[rel]=(Path(rel).parent/name).as_posix()
 assert len(set(x.casefold() for x in result.values()))==len(result),'Colisão de nomes Windows'
 return result

def migrar(src,dst,windows,aplicar=False):
 mapping=plano(src);excess=[]
 for a,b in mapping.items():
  n=len(windows.rstrip('/'))+1+len(b)
  if n>limite(Path(b)):excess.append({'arquivo':b,'caracteres':n,'limite':limite(Path(b))})
 print(json.dumps({'arquivos':len(mapping),'renomeados':sum(a!=b for a,b in mapping.items()),'maximo':max(len(windows)+1+len(x) for x in mapping.values()),'excessos':excess},ensure_ascii=False),flush=True)
 if not aplicar:return
 assert not excess,'Excessos: migração não aplicada'
 if dst.exists():
  mf=dst/'CAMINHOS.json'
  assert mf.exists(),'Destino já existe sem manifesto: nada sobrescrito'
  previous=json.loads(mf.read_text())
  assert previous['origem']==str(src) and previous['windows']==windows
  for e in previous['arquivos']:
   assert sha(src/e['origem'])==e['sha256_origem'] and sha(dst/e['destino'])==e['sha256_destino'],'Destino/origem alterado: revisão necessária'
  print('Migração já conferida; nenhuma alteração.');return previous
 # Relações relativas e nomes exatos. Nenhuma substituição de valores biológicos.
 renames={a:b for a,b in mapping.items() if a!=b}
 replacements={}
 aliases=[str(src)]
 if (src/'ORIGEM_CONSOLIDACAO.json').exists():aliases+=json.loads((src/'ORIGEM_CONSOLIDACAO.json').read_text())['raizes_referencias']
 for a,b in renames.items():
  replacements[a]=b;replacements[a.removeprefix('output/')]=b.removeprefix('output/')

  for alias in aliases:replacements[alias+"/"+a]=windows.rstrip("/")+"/"+b
  if Path(a).name!=Path(b).name:
   na,nb=Path(a).name,Path(b).name
   assert na not in replacements or replacements[na]==nb,'Nome isolado ambíguo'
   replacements[na]=nb
 for a,b in mapping.items():
  for alias in aliases:replacements[alias+'/'+a]=windows.rstrip('/')+'/'+b
 # Diretórios movidos são referências operacionais nos documentos/manuais.
 for a,b in renames.items():
  pa,pb=Path(a).parent.as_posix(),Path(b).parent.as_posix()
  if pa!=pb:
   replacements[pa+'/']=pb+'/'
   replacements[pa.removeprefix('output/')+'/']=pb.removeprefix('output/')+'/'
   # Recursos HTML usam diretórios relativos ao próprio documento.
   for oa,nb in zip(Path(pa).parts,Path(pb).parts):
    if oa!=nb and ('_files' in oa or oa in DIRS):replacements[oa+'/']=nb+'/'
 for a,b in list(replacements.items()):
  if a.startswith('/') and re.match(r'^[A-Za-z]:/',b):replacements['file://'+a]='file:///'+b
 for a,b in list(replacements.items()):
  replacements[urllib.parse.quote(a,safe='/:')]=urllib.parse.quote(b,safe='/:')
 rx=re.compile('|'.join(re.escape(x) for x in sorted(replacements,key=len,reverse=True)))
 aliases_codificados=set(aliases+[urllib.parse.quote(a,safe="/:") for a in aliases])
 sem_raiz=[k for k in replacements if not any(a in k for a in aliases_codificados)]
 def atualizar(t):
  chaves=replacements if any(a in t for a in aliases_codificados) else sem_raiz
  presentes=[k for k in chaves if k in t]
  if not presentes:return t
  local=re.compile('|'.join(re.escape(k) for k in sorted(presentes,key=len,reverse=True)))
  return local.sub(lambda m:replacements[m[0]],t)
 entries=[];dst.mkdir(parents=True)
 for a,b in mapping.items():
  source=src/a;target=dst/b;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(source,target)
  old=sha(source);mode='bytes_preservados'
  # Pacote interno QField/ZIP intactos; apenas auditoria externa pode mudar.
  pacote=a.startswith('output/09_qfield/') and len(Path(a).parts)>3
  docs=(a.startswith(('output/','manual_usuario/')) and not protegido(a) and not pacote)
  operacional=source.name == 'resumo_formas_vida_desconhecida.csv' or source.name.startswith(('indice','metadados_','referencias_','auditoria_geracao_','auditoria_renderizacao_relatorios_analiticos')) or source.suffix.lower() not in ('.csv','.json')
  if docs and operacional and source.suffix.lower() in ('.csv','.json','.txt','.md','.rmd','.html','.htm'):
   data=source.read_bytes()
   try:
    t=data.decode('utf-8');new=atualizar(t).encode('utf-8')
   except UnicodeDecodeError:new=data
   if new!=data:target.write_bytes(new);mode='referencias_atualizadas'
  elif docs and source.suffix.lower() in ('.docx','.xlsx'):
   # Somente relações de hiperlinks e textos de caminhos; conteúdo/gráficos originais.
   with zipfile.ZipFile(source) as z:
    members={n:z.read(n) for n in z.namelist()};changed=False
    for n,v in list(members.items()):
     if n.endswith(('.xml','.rels')):
      new=atualizar(v.decode('utf-8')).encode('utf-8')
      if v!=new:members[n]=new;changed=True
    if changed:
     with zipfile.ZipFile(target,'w',compression=zipfile.ZIP_DEFLATED) as out:
      for info in z.infolist():out.writestr(info,members[info.filename])
     mode='referencias_documento_atualizadas'
  entries.append({'origem':a,'destino':b,'sha256_origem':old,'sha256_destino':sha(target),'acao':mode})
 # Metadados operacionais de relatórios: hashes de documentos pós-atualização.
 for entry in entries:
  p=dst/entry['destino']
  if p.name not in ('indice_relatorios_analiticos.csv','auditoria_renderizacao_relatorios_analiticos.csv'):continue
  with p.open(encoding='utf-8-sig',newline='') as f:reader=csv.DictReader(f);fields=reader.fieldnames;rows=list(reader)
  assert 'sha256' in fields
  for row in rows:
   matches=[]
   for value in row.values():
    if not value:continue
    for candidate in (p.parent/value,dst/value,dst/'output'/value):
     if candidate.is_file() and candidate.suffix.lower() in ('.html','.docx','.pdf','.md','.rmd'):matches.append(candidate)
   matches=list(set(matches))
   if len(matches)==1:row['sha256']=sha(matches[0])
   elif row['sha256']:
    # Formato pode vir em coluna própria e arquivo como caminho Windows absoluto.
    for v in row.values():
     if isinstance(v,str) and v.startswith(windows.rstrip('/')+'/'):
      candidate=dst/v[len(windows.rstrip('/'))+1:]
      if candidate.is_file():row['sha256']=sha(candidate);break
    else:raise AssertionError(('Hash sem referência resolvível',str(p),row))
  with p.open('w',encoding='utf-8',newline='') as f:w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)
  entry['acao']='indice_hashes_atualizados';entry['sha256_destino']=sha(p)
 # Metadados com hashes de arquivos que mudaram: atualizar a relação exata arquivo/hash.
 # Índice de output será refeito pelo helper do script após este passo, sem organizer.
 manifest={'origem':str(src),'destino':str(dst),'windows':windows,'recalculo':False,'arquivos':entries}
 (dst/'CAMINHOS.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2))
 return manifest
if __name__=='__main__':
 migrar(Path(sys.argv[1]),Path(sys.argv[2]),sys.argv[3], '--aplicar' in sys.argv)
