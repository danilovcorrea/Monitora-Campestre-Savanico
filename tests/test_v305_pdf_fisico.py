from pathlib import Path
from pypdf import PdfReader
import json,sys,re,unicodedata
sys.stdout.reconfigure(encoding='utf-8');d=Path(sys.argv[1]);out=d/'output/08_analises';audit=json.loads((out/'AUDITORIA_PAGINACAO.json').read_text(encoding='utf-8'));results=[]
def norm(s):return re.sub(r'\s+','',unicodedata.normalize('NFKC',s)).replace('−','-').replace('\xad','')
for name,a in audit.items():
 p=next(out.rglob(name.replace('.html','.pdf')));reader=PdfReader(p);assert len(reader.pages)==len(a['folhas']);errors=[];cells=0
 for page,m in zip(reader.pages,a['folhas']):
  w,h=float(page.mediabox.width),float(page.mediabox.height)
  assert abs(w-m['largura'])<1 and abs(h-m['altura'])<1,(p.name,m['numero'],w,h,m)
  text=norm(page.extract_text()or'')
  for c in m['celulas']:
   cells+=1
   if norm(c)not in text:errors.append({'pagina':m['numero'],'celula':c})
 results.append({'arquivo':p.name,'paginas':len(reader.pages),'celulas_conferidas':cells,'falhas':errors,'status':'FAIL'if errors else'PASS'})
(d/'AUDITORIA_PDF_FISICO.json').write_text(json.dumps(results,ensure_ascii=False,indent=2),encoding='utf-8');print(results)
assert all(x['status']=='PASS'for x in results)
