"""Detecta páginas sem conteúdo, ignorando o fólio e ornamentos de cabeçalho/rodapé."""
from pathlib import Path
import json,sys
import pypdfium2 as pdfium
from PIL import ImageChops,Image

def audit(path):
 doc=pdfium.PdfDocument(str(path));empty=[]
 for i,page in enumerate(doc):
  w,h=page.get_size();text=page.get_textpage();body=[]
  for j in range(text.count_chars()):
   c=text.get_text_range(j,1)
   if not c.strip():continue
   l,b,r,t=text.get_charbox(j)
   if b>54 and t<h-45:body.append(c)
  # Uma imagem sem texto também constitui conteúdo; excluir apenas as margens.
  if not ''.join(body).strip():
   im=page.render(scale=.75).to_pil().convert('RGB');crop=im.crop((25,34,im.width-25,im.height-41))
   hist=ImageChops.difference(crop,Image.new('RGB',crop.size,'white')).convert('L').histogram()
   ink=sum(hist[30:])/max(1,crop.width*crop.height)
   if ink<.0005:empty.append(i+1)
  text.close();page.close()
 return {'arquivo':str(path),'paginas':len(doc),'paginas_vazias':empty,'status':'FAIL'if empty else'PASS'}
if __name__=='__main__':
 result=[audit(Path(p))for p in sys.argv[1:]];print(json.dumps(result,ensure_ascii=False,indent=2));sys.exit(any(r['paginas_vazias']for r in result))
