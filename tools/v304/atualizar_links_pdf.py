"""Atualiza apenas ações de hyperlinks PDF. Não renderiza nem altera fluxos de página."""
from pathlib import Path,PurePosixPath
import sys,json,urllib.parse,hashlib
from pypdf import PdfReader,PdfWriter
from pypdf.generic import NameObject,TextStringObject
sys.stdout.reconfigure(encoding='utf-8')
dst=Path(sys.argv[1]);m=json.loads((dst/'CAMINHOS.json').read_text(encoding='utf-8'));aliases=[m['origem']]
if (dst/'ORIGEM_CONSOLIDACAO.json').exists():aliases+=json.loads((dst/'ORIGEM_CONSOLIDACAO.json').read_text(encoding='utf-8'))['raizes_referencias']
paths={}
for e in m['arquivos']:
 for alias in aliases:paths[alias+'/'+e['origem']]=m['windows']+'/'+e['destino']
records=[]
for e in m['arquivos']:
 f=dst/e['destino']
 if f.suffix.lower()!='.pdf' or e['destino'].startswith('output/09_qfield/'):continue
 reader=PdfReader(f);changes=[]
 content=lambda r:hashlib.sha256(b''.join(p.get_contents().get_data() if p.get_contents() else b'' for p in r.pages)).hexdigest()
 before=content(reader)
 for p in reader.pages:
  for a in p.get('/Annots',[]):
   action=a.get_object().get('/A')
   if not action or '/URI' not in action:continue
   uri=str(action['/URI']);parsed=urllib.parse.urlsplit(uri)
   if parsed.scheme not in ('file',''):continue
   target=urllib.parse.unquote(parsed.path)
   new=paths.get(target)
   if new:
    value='file:///'+urllib.parse.quote(new,safe='/:')
    if parsed.fragment:value+='#'+parsed.fragment
    action[NameObject('/URI')]=TextStringObject(value);changes.append((uri,value))
 if changes:
  writer=PdfWriter();writer.clone_document_from_reader(reader)
  tmp=f.with_suffix('.pdf.tmp')
  with tmp.open('wb') as out:writer.write(out)
  assert content(PdfReader(tmp))==before,'Fluxo gráfico de página alterado'
  tmp.replace(f)
 records.append({'arquivo':e['destino'],'hyperlinks_atualizados':len(changes),'paginas_preservadas':True})
(dst/'LINKS_PDF.json').write_text(json.dumps(records,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps(records,ensure_ascii=False))
