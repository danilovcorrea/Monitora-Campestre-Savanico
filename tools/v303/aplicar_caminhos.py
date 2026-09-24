# Executado pelo builder; s e module são os fontes em memória.
from caminhos import codigo, DIRS, NOMES, TOKEN, dirs
# Manter uma referência da revisão que concluiu os testes de cálculo.
(root/'artifacts/v303/antes_caminhos.R').write_text(s)
# Nomes compactos desde a criação, inclusive em Linux e antes de compartilhar.
a=s.index('  caminhos_docx <-',s.index('monitora_relatorios_analiticos_destino_fisico <-'))
b=s.index('  chave_uc <-',a)
s=s[:a]+'  # Layout portátil permanente: o destino de compartilhamento pode ser maior.\n'+s[b:]
s=s.replace('pasta_uc <- file.path(output_dir, "09_qfield", slug)', 'pasta_uc <- file.path(output_dir, "09_qfield", paste0("u_", substr(digest::digest(uc, algo = "sha256", serialize = FALSE), 1, 8)))')
s=s.replace('versao <- paste0("projeto_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", substr(digest::digest(man), 1, 8))','versao <- paste0("p_", substr(digest::digest(list(man, Sys.time(), tempfile())), 1, 12))')
# Somente literais de caminho; funções, variáveis e categorias de dados preservadas.
s=codigo(s)
module=codigo(module)
# Consumidor de oráculos anteriores reconhece ambos layouts físicos explicitamente.
a=s.index('monitora_oraculo_resumo_ocorrencias_localizar <-');b=s.index('monitora_oraculo_comparar_resumo_ocorrencias <-',a)
p=s[a:b].replace('  monitora_oraculo_localizar(c(', '  candidatos <- c(')
p=p.replace('  ))\n}', '  )\n  monitora_oraculo_localizar(unique(c(candidatos, gsub("/oc/", "/ocorrencias_diagnosticas/", candidatos, fixed = TRUE))))\n}')
s=s[:a]+p+s[b:]
# O comparador aceita tanto a pasta temática nova quanto a anterior.
s=s.replace('file.path(output_dir, "02_painel_correcoes", "apoio", fase)', 'file.path(output_dir, "02_painel_correcoes", "apoio", fase)')
# Índice avalia o Windows real ou a reserva portátil de 120 caracteres para a raiz da rodada.
s=s.replace('240L, NA_integer_)', '240L, 259L)')
a=s.index('  comprimento_caminho <- nchar(',s.index('monitora_output_escrever_indice_produtos <-'));b=s.index('  situacao_caminho_office <-',a)
s=s[:a]+'''  caminho_windows <- monitora_caminho_windows_projetado(todos, output_dir)
  comprimento_caminho <- nchar(caminho_windows, type = "chars")
''' +s[b:]
s=s.replace('comprimento_caminho_caracteres = nchar(indice_path, type = "chars"),','comprimento_caminho_caracteres = nchar(monitora_caminho_windows_projetado(indice_path, output_dir), type = "chars"),')
s=s.replace('nchar(indice_path, type = "chars") <= 210L','nchar(monitora_caminho_windows_projetado(indice_path, output_dir), type = "chars") <= 210L')
s=s.replace('  data.table::fwrite(ind, indice_path, na = "")', '''  data.table::fwrite(ind, indice_path, na = "")
  excessos <- sum(ind$situacao_caminho_office == "revisar_caminho_windows")
  if (excessos) message("[CAMINHOS][AVISO] ", excessos, " produtos excedem o orçamento Windows/OneDrive. Consulte ", indice_path,
    ". Informe MONITORA_DESTINO_COMPARTILHAMENTO com a raiz final e revise os caminhos; não homologar a entrega com excessos.")
  else message("[CAMINHOS] Inventário dentro do orçamento Windows/OneDrive (CSV 210, documentos 240, demais 259).")''')
s=s.replace('MONITORA_SCRIPT_BUILD_ID <- "v3.0.3-rc01-20260924-r02"','MONITORA_SCRIPT_BUILD_ID <- "v3.0.3-rc01-20260924-r03"')
# Pequeno helper somente para auditoria de destino; não intercepta I/O nem reescreve dados.
helper='''monitora_caminho_windows_projetado <- function(arquivos, output_dir) {
  destino <- trimws(Sys.getenv("MONITORA_DESTINO_COMPARTILHAMENTO", ""))
  destino <- chartr(intToUtf8(92L), "/", destino)
  raiz <- dirname(normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  if (!nzchar(destino)) destino <- sub("^/mnt/([a-zA-Z])/", "\\\\U\\\\1:/", raiz, perl = TRUE)
  # Reserva portátil: todos os testes e índices usam ao menos 120 caracteres de raiz.
  if (!grepl("^[A-Za-z]:/|^//", destino)) destino <- strrep("x", 120L)
  destino <- sub("[/\\\\\\\\]+$", "", destino)
  if (nchar(destino) < 120L) destino <- paste0(destino, strrep("x", 120L - nchar(destino)))
  rel <- substring(arquivos, nchar(sub("[/\\\\\\\\]+$", "", output_dir)) + 2L)
  paste0(destino, "/", basename(output_dir), "/", gsub("\\\\\\\\", "/", rel))
}
'''
s=s.replace('monitora_output_escrever_indice_produtos <-',helper+'monitora_output_escrever_indice_produtos <-',1)
# Classificação mantém a semântica dos nomes lógicos anteriores, emitindo nomes físicos novos.
import json
inverso='monitora_nome_arquivo_logico <- function(x) {\n'
for velho,novo in sorted(NOMES.items(),key=lambda kv:-len(kv[1])):
 inverso+='  if (startsWith(x, '+json.dumps(novo)+')) return(paste0('+json.dumps(velho)+', substring(x, '+str(len(novo)+1)+')))\n'
inverso+='  x\n}\n'
s=s.replace('monitora_output_classificar_arquivo_raiz <-',inverso+'monitora_output_classificar_arquivo_raiz <-',1)
original=(root/'artifacts/v303/antes_caminhos.R').read_text()
for nome,seguinte in [('monitora_output_classificar_arquivo_raiz','monitora_output_mover_arquivo'),('monitora_output_destino_correcao','monitora_output_podar_diretorios_vazios')]:
 a=original.index(nome+' <-');b=original.index(seguinte+' <-',a);p=original[a:b]
 # aplicar somente diretórios, conservando os padrões lógicos de classificação
 p=TOKEN.sub(lambda m: m[0] if m[0].startswith('#') else m[0][0]+dirs(m[0][1:-1])+m[0][-1],p)
 if nome.endswith('arquivo_raiz'):p=p.replace('  bn <- basename(as.character(bn)[1L])','  bn <- monitora_nome_arquivo_logico(basename(as.character(bn)[1L]))')
 else:
  p=p.replace('  bn <- basename(rel)','  bn <- basename(rel)\n  bn_logico <- monitora_nome_arquivo_logico(bn)')
  p=p.replace(', bn, ignore.case',', bn_logico, ignore.case')
 a=s.index(nome+' <-');b=s.index(seguinte+' <-',a);s=s[:a]+p+s[b:]
# Leitura do comparador mantém compatibilidade com as rodadas organizadas anteriores.
s=s.replace('  caminhos <- unique(c(legado, organizado))','  organizado_anterior <- file.path(raiz, "02_painel_correcoes", "relatorios_apoio_tematicos", fase)\n  caminhos <- unique(c(legado, organizado, organizado_anterior))')
s=s.replace('    1L,\n    10L)', '    1L,\n    10L)')
# Pastas analíticas curtas com identidade estável, independente da grafia e do período.
s=s.replace('"uc-",\n    substr(digest::digest(chave_uc, algo = "sha256", serialize = FALSE), 1L, 10L)', '"u_",\n    substr(digest::digest(chave_uc, algo = "sha256", serialize = FALSE), 1L, 10L)')
s=s.replace('Os relatórios analíticos usam caminho físico compacto apenas quando necessário', 'Os relatórios analíticos usam sempre caminho físico compacto')
(root/'artifacts/v303/regras_caminhos.json').write_text(json.dumps({'diretorios':DIRS,'nomes':NOMES},ensure_ascii=False,indent=2))

s=s.replace('Nomes físicos de relatórios e planilhas podem ser compactados automaticamente somente quando necessário para abertura no Windows;', 'Relatórios analíticos e diretórios auxiliares usam nomes físicos curtos desde a criação;')
s=s.replace('As colunas comprimento_caminho_caracteres, limite_recomendado_windows e situacao_caminho_office do índice permitem identificar outros arquivos que dependam de uma pasta de execução mais curta.', 'O índice avalia o destino Windows/OneDrive e reserva ao menos 120 caracteres para a raiz da rodada. MONITORA_DESTINO_COMPARTILHAMENTO permite informar um destino maior antes da execução; qualquer excesso aparece no console e deve ser resolvido antes da entrega.')
# Outros formatos já tinham compactação reativa: torná-la independente do host.
s=s.replace('if (any(nchar(caminhos_documentais_logicos, type = "chars") > 240L)) {','if (TRUE) {')
s=s.replace('compactar_nome <- nchar(caminhos_logicos, type = "chars") > 210L','compactar_nome <- rep(TRUE, nrow(contextos))')
s=s.replace('compactar_mais <- nchar(caminhos_compactos, type = "chars") > 210L','compactar_mais <- rep(TRUE, nrow(contextos))')
a=s.index('monitora_relatorios_analiticos_caminho_figura <-');b=s.index('monitora_relatorios_analiticos_catalogo_tabelas <-',a)
p=s[a:b];x=p.index('  if (!identical(.Platform$OS.type');y=p.index('  extensao <-',x)
p=p[:x]+'  if (nchar(nome_arquivo, type = "chars") <= 64L) return(caminho)\n'+p[y:]
x=p.index('  orcamento <-');y=p.index('  prefixo_max <-',x)
p=p[:x]+'  orcamento <- 64L - nchar(sufixo_ext, type = "chars")\n'+p[y:]
s=s[:a]+p+s[b:]

s=s.replace("v3.0.3-rc01-20260924-r03", "v3.0.3-rc01-20260924-r04")
# Comparações entre layouts antigo/novo usam identidade lógica do arquivo, não sua abreviação física.
s=s.replace('  nomes <- basename(arqs)\n  for (n in unique(nomes[duplicated(nomes)]))', '  nomes <- vapply(basename(arqs), monitora_nome_arquivo_logico, character(1L))\n  for (n in unique(nomes[duplicated(nomes)]))')
