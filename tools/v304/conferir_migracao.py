from pathlib import Path
import csv,json,sys,re,hashlib,subprocess,urllib.parse,zipfile,html
from migrar_caminhos import plano,sha,protegido,limite
repo=Path(__file__).resolve().parents[2]
dst=Path(sys.argv[1]);mf=dst/'CAMINHOS.json';m=json.loads(mf.read_text());src=Path(m['origem']);expected=plano(src)
# Ajustes finais do plano apenas na cópia; ausência/colisão bloqueiam, sem sobrescrita.
renames={}
for e in m['arquivos']:
 old=e['destino'];new=expected[e['origem']]
 if old!=new:
  if (dst/old).exists():
   assert not (dst/new).exists()
   (dst/new).parent.mkdir(parents=True,exist_ok=True);(dst/old).rename(dst/new)
  else:assert (dst/new).is_file() and sha(dst/new)==sha(src/e['origem']),'Migração parcial ambígua'
  renames[old]=new;renames[Path(old).name]=Path(new).name;e['destino']=new
for e in m['arquivos']:
 p=dst/e['destino']
 if renames and e['acao']!='bytes_preservados' and p.suffix.lower() in ('.csv','.json','.txt','.md','.rmd','.html'):
  t=p.read_text()
  for a,b in renames.items():t=t.replace(a,b)
  p.write_text(t)
# Normalizar URIs de arquivo Windows após substituir caminhos Linux absolutos.
for e in m['arquivos']:
 p=dst/e['destino']
 if e['acao']=='bytes_preservados':continue
 if p.suffix.lower() in ('.html','.rmd','.md','.txt','.csv','.json'):
  raw=p.read_bytes();new=re.sub(rb'file://([A-Za-z]:/)',rb'file:///\1',raw)
  if new!=raw:p.write_bytes(new)
 elif p.suffix.lower() in ('.docx','.xlsx'):
  with zipfile.ZipFile(p) as z:
   info=z.infolist();parts={i.filename:z.read(i.filename) for i in info}
  changed=False
  for n,v in list(parts.items()):
   if n.endswith(('.xml','.rels')):
    new=re.sub(rb'file://([A-Za-z]:/)',rb'file:///\1',v)
    if new!=v:parts[n]=new;changed=True
  if changed:
   with zipfile.ZipFile(p,'w') as z:
    for i in info:z.writestr(i,parts[i.filename])
# Nomes lógicos históricos conservados; inventários operacionais com tamanho/hash atuais.
for e in m['arquivos']:
 p=dst/e['destino']
 if p.name not in ('indice_relatorios_analiticos.csv','auditoria_renderizacao_relatorios_analiticos.csv'):continue
 with p.open(encoding='utf-8-sig',newline='') as f:r=csv.DictReader(f);fields=r.fieldnames;rows=list(r)
 with (src/e['origem']).open(encoding='utf-8-sig',newline='') as f:orig=list(csv.DictReader(f))
 assert len(orig)==len(rows)
 for row,old in zip(rows,orig):
  rel=row['caminho_relativo'];f=dst/'output'/rel
  if not f.is_file() and rel.startswith(m['windows']+'/'):f=dst/rel[len(m['windows'])+1:]
  assert f.is_file(),(p,rel)
  row['sha256']=sha(f);row['tamanho_bytes']=str(f.stat().st_size)
  if 'nome_logico' in row:row['nome_logico']=old['nome_logico']
  if 'diretorio_contexto' in row:row['diretorio_contexto']=p.parent.name
  if 'caminho_fisico_compactado' in row:row['caminho_fisico_compactado']='TRUE'
 with p.open('w',encoding='utf-8',newline='') as f:w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)
# Recriar somente índice: não executar organizer, curadoria, relatórios ou análises.
r=dst/'REINDEXAR.R';r.write_text('''args<-commandArgs(TRUE)
source(args[1]);e<-monitora_test_funcoes(args[2])$env
Sys.setenv(MONITORA_DESTINO_COMPARTILHAMENTO=args[4])
x<-e$monitora_output_escrever_indice_produtos(file.path(args[3],"output"),"migracao_caminhos","revisao_caminhos_sem_recalculo")
stopifnot(all(x$situacao_caminho_office=="apto_abertura_windows"))
''')
subprocess.run(['Rscript',str(r),str(repo/'tests/helpers_test_funcoes.R'),str(repo/'R_monitora_campsav_alvo_global.R'),str(dst),m['windows']],check=True)
r.unlink()
# Byte-identidade estrita dos dados, linhagem, cache e todo o pacote QField.
protected=0;changed=[]
for e in m['arquivos']:
 assert sha(src/e['origem'])==e['sha256_origem'],'Origem mudou'
 e['sha256_destino']=sha(dst/e['destino'])
 pack=e['origem'].startswith('output/09_qfield/') and len(Path(e['origem']).parts)>3
 if protegido(e['origem']) or pack:
  assert e['sha256_destino']==e['sha256_origem'],e['origem'];protected+=1
 elif e['sha256_destino']!=e['sha256_origem']:changed.append(e['destino'])
 if e['destino']=='output/indice_produtos.csv':e['acao']='indice_recriado'
# Vínculos HTML que existiam antes devem continuar resolvíveis após mudar os nomes.
rx=re.compile(r'(?:href|src)=[\"\']([^\"\']+)[\"\']',re.I)
def resolver(value,base,novo=False):
 value=html.unescape(urllib.parse.unquote(value)).split('#')[0].split('?')[0]
 if not value or value.startswith(('data:','http:','https:','mailto:','javascript:')):return None
 value=value.removeprefix('file://')
 if re.match(r'^/[A-Za-z]:/',value):value=value[1:]
 if novo and value.startswith(m['windows']+'/'):return dst/value[len(m['windows'])+1:]
 return Path(value) if value.startswith('/') else base/value
links=0
for e in m['arquivos']:
 a=src/e['origem'];b=dst/e['destino']
 if a.suffix.lower() not in ('.html','.htm'):continue
 before=rx.findall(a.read_text(encoding='utf-8',errors='replace'));after=rx.findall(b.read_text(encoding='utf-8',errors='replace'))
 assert len(before)==len(after)
 for old,new in zip(before,after):
  p=resolver(old,a.parent);q=resolver(new,b.parent,True)
  if p is not None and p.exists():assert q is not None and q.exists(),(str(b),old,new);links+=1
# Relações OOXML locais: links existentes antes da migração continuam válidos.
import xml.etree.ElementTree as ET
ooxml=0
for e in m['arquivos']:
 a=src/e['origem'];b=dst/e['destino']
 if a.suffix.lower() not in ('.docx','.xlsx'):continue
 with zipfile.ZipFile(a) as za,zipfile.ZipFile(b) as zb:
  assert zb.testzip() is None
  for n in za.namelist():
   if not n.endswith('.rels'):continue
   old={x.attrib.get('Id'):x.attrib for x in ET.fromstring(za.read(n))}
   new={x.attrib.get('Id'):x.attrib for x in ET.fromstring(zb.read(n))}
   for ident,row in old.items():
    if row.get('TargetMode')!='External':continue
    p=resolver(row.get('Target',''),a.parent);q=resolver(new[ident].get('Target',''),b.parent,True)
    if p is not None and p.exists():assert q is not None and q.exists(),(b,row,new[ident]);ooxml+=1
# Índice atual confere com bytes entregues.
with (dst/'output/indice_produtos.csv').open() as f:rows=list(csv.DictReader(f))
for row in rows:
 if row['hash_verificavel']=='TRUE':assert hashlib.md5((dst/'output'/row['caminho_relativo']).read_bytes()).hexdigest()==row['md5']
excess=[]
for p in dst.rglob('*'):
 if p.is_file():
  rel=p.relative_to(dst).as_posix();n=len(m['windows'])+1+len(rel)
  if n>limite(p):excess.append((rel,n,limite(p)))
assert not excess,excess
m['gates']={'status':'PASS','protegidos_identicos':protected,'links_html_conferidos':links,'links_ooxml_conferidos':ooxml,'arquivos_atualizados':changed,'excessos':0,'limites':{'csv_xlsx':210,'documentos':240,'outros':259}}
mf.write_text(json.dumps(m,ensure_ascii=False,indent=2))
print(json.dumps(m['gates'],ensure_ascii=False),flush=True)
