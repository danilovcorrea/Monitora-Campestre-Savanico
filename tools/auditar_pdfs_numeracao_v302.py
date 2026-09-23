from pathlib import Path
import sys,re,json,csv
from pypdf import PdfReader
import pypdfium2 as pdfium
root=Path(sys.argv[1]);dest=Path(sys.argv[2]);dest.mkdir(parents=True,exist_ok=True);result=[]
for tipo,nt,nf in [('sintetico',7,8),('detalhado',32,76)]:
 f=root/('pnb_'+tipo)/('pnb_'+tipo+'_v302.pdf');r=PdfReader(f)
 pages=[p.extract_text() or '' for p in r.pages];text=' '.join(pages)
 tab=[int(n) for n in re.findall(r'Tabela\s+(\d+)\s*—',text)]
 figs=[int(n) for n in re.findall(r'Figura\s+(\d+)\.',text)]
 assert tab==list(range(1,nt+1)),(tipo,'tabelas',tab)
 assert figs==list(range(1,nf+1)),(tipo,'figuras',figs)
 examples=[i for i,s in enumerate(pages) if re.search(r'Tabela\s+(1|13|19|31)\s*—',s)]
 doc=pdfium.PdfDocument(str(f))
 for i in examples[:3]:doc[i].render(scale=1.35).to_pil().save(dest/('pdf_'+tipo+'_pagina_'+str(i+1)+'.png'))
 result.append(dict(tipo=tipo,paginas=len(pages),tabelas=len(tab),figuras=len(figs),sequencias='PASS',paginas_inspecao=[i+1 for i in examples[:3]]))
(dest/'PDFS_CONFERIDOS.json').write_text(json.dumps(result,indent=2,ensure_ascii=False),encoding='utf-8')
print(json.dumps(result,ensure_ascii=False))
