from http.server import ThreadingHTTPServer,BaseHTTPRequestHandler
from threading import Thread
from pathlib import Path
import subprocess,time,json
counts={}
class Handler(BaseHTTPRequestHandler):
 def log_message(self,*args):pass
 def do_POST(self):
  self.rfile.read(int(self.headers.get('Content-Length',0))); n=counts.get(self.path,0)+1;counts[self.path]=n
  status=200;body=b'{"count":347}'
  if self.path=='/404':status=404
  if self.path in ['/429','/500'] and n==1:status=int(self.path[1:])
  if self.path=='/timeout':time.sleep(2)
  if self.path=='/heartbeat':time.sleep(16)
  if self.path=='/json' and n==1:body=b'{'
  if self.path=='/truncado':body=b'{"exceededTransferLimit":true}'
  if self.path=='/arc404':body=b'{"error":{"code":404,"message":"Not Found"}}'
  try:
   self.send_response(status);self.send_header('Content-Type','application/json');self.send_header('Content-Length',str(len(body)));self.end_headers();self.wfile.write(body)
  except BrokenPipeError:pass
server=ThreadingHTTPServer(('127.0.0.1',0),Handler);Thread(target=server.serve_forever,daemon=True).start()
out=Path('artifacts/v303');out.mkdir(exist_ok=True)
script='''source('tools/v303/observabilidade.R');source('tools/v303/fogo_api.R')
MONITORA_LOG_DIR<-'artifacts/v303/rede';dir.create(MONITORA_LOG_DIR,showWarnings=FALSE)
MONITORA_EXEC_ID<-'mock';Sys.setenv(MONITORA_FOGO_TENTATIVAS='2',MONITORA_FOGO_TIMEOUT_SEG='1')
base<-commandArgs(TRUE)[1]
for(path in c('/ok','/429','/500','/json'))stopifnot(monitora_fogo_api(paste0(base,path),etapa=path)$obj$count==347)
for(path in c('/404','/arc404','/truncado','/timeout')) {
 err<-tryCatch(monitora_fogo_api(paste0(base,path),etapa=path),error=conditionMessage)
 stopifnot(is.character(err),grepl('auditoria:',err),grepl(path,err,fixed=TRUE))
}
Sys.setenv(MONITORA_FOGO_TIMEOUT_SEG='20')
stopifnot(monitora_fogo_api(paste0(base,'/heartbeat'),etapa='pulso')$obj$count==347)
cat('PASS rede: recuperação, rejeições e prazo; nenhuma falha convertida em zero fogo.\\n')
'''
(out/'rede_mock.R').write_text(script)
with (out/'test_rede.log').open('w') as log:
 p=subprocess.run(['Rscript','--vanilla',str(out/'rede_mock.R'),f'http://127.0.0.1:{server.server_port}'],stdout=log,stderr=subprocess.STDOUT)
server.shutdown();assert p.returncode==0
assert counts=={'/ok':1,'/429':2,'/500':2,'/json':2,'/404':1,'/arc404':1,'/truncado':1,'/timeout':2,'/heartbeat':1},counts
assert 'aguardando conexão/resposta' in (out/'test_rede.log').read_text()
(out/'test_rede_contagens.json').write_text(json.dumps(counts,indent=2));print('PASS rede real contra servidor local: 9 cenários; pulso de espera confirmado.')
