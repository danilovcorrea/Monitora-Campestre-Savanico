# Candidata 3.0.3-rc01 — recuperação PNM e EET

Em desenvolvimento e homologação local; não publicada. Base: tag pública v3.0.2 (d8df53d). Dados institucionais ficam somente nas áreas locais de homologação/entrega.

## Correções

- Comparação pré/pós localiza arquivos nas pastas transitórias e organizadas, inclusive após movimentação parcial. Duplicatas só são aceitas se idênticas; versões divergentes bloqueiam a comparação com causa explícita. Ausência das fases deixa aviso. O pós é recomputado quando solicitado, inclusive para alterações espaciais; a solicitação de relatórios de apoio é respeitada com o painel desligado.
- Consultas de fogo informam URL, etapa, tentativa, HTTP, duração, limites e espera. Persistem auditoria por tentativa. Há até quatro tentativas para falhas transitórias; HTTP permanente e resposta explicitamente truncada não são repetidos como se fossem recuperáveis. JSON inválido/transferência incompleta podem ser repetidos dentro do limite. Nenhuma falha vira zero fogo ou autoriza cache inválido/antigo.
- PDF de validação/manual usa o renderizador em processo R isolado, com padrão de 180 s e margem externa de 30 s. O produto é publicado a partir de arquivo temporário validado, sem considerar PDF antigo como resultado atual. HTML/PDF apresentam a causa real de falha. O console informa início, espera e conclusão.
- A existência de `input_qfield` é explicitamente sinalizada; a entrada canônica continua `qfield_input`. Arquivos não são movidos automaticamente. O recorte de imagens informa tiles processados, percentual e tempo decorrido; a conclusão identifica o pacote ou o bloqueio.
- Avisos R são preservados e persistidos por etapa/mensagem/chamada, com contagem e horários. O primeiro de cada grupo aparece imediatamente no console, sem remover o aviso original. O handler é local à execução e não altera handlers globais nem opções do RStudio.

- Medição de legendas gráficas usa dispositivo com suporte a Unicode, com restauração do dispositivo anterior. Isso elimina avisos de conversão de setas/letras gregas durante o cálculo da largura, sem trocar símbolos nem alterar os dados.

## Configuração de conexão

- `MONITORA_FOGO_TIMEOUT_SEG`: 90 s por requisição; aceita 1–600 s.
- `MONITORA_FOGO_CONEXAO_TIMEOUT_SEG`: 30 s para conexão, limitado pelo prazo da requisição; aceita 1–120 s.
- `MONITORA_FOGO_TENTATIVAS`: 4; aceita 1–6.
- `MONITORA_PDF_CHROME_TIMEOUT_SEG`: padrão 180 s para validação/manual.

Arquivos de diagnóstico ficam em `log/consultas_fogo_<execucao>.csv`, `log/avisos_execucao_<execucao>.csv` e logs documentais. Estimativas globais de duração não garantem o prazo dos serviços externos; os pulsos informam tempo efetivamente decorrido.

## Homologação

Status e resultados finais serão consolidados ao encerrar as rodadas. Área local: `/home/dlinux/Monitora_Dev_20260712/atualizacoes_v303_20260924`.

Testes focalizados incluem comparação em três layouts, mudança exclusivamente espacial, arquivos duplicados divergentes, ausência explícita, avisos sem supressão, pasta QField invertida, nove cenários HTTP locais, regressão editorial de 256 combinações e renderização documental em Linux/R Windows. Cálculos e contrato são comparados à v3.0.2; tamanho máximo obrigatório é 5.000.000 bytes em LF e CRLF.

A primeira execução integral usa aquisição de fogo sem cache. A rodada final usa o código consolidado e pode reutilizar somente os snapshots completos e íntegros adquiridos no ensaio anterior. Inputs e linhagem são copiados das saídas corrigidas das bolsistas; MBTiles são copiados do acervo indicado para `qfield_input`, por UC. Não há nova decisão de curadoria ecológica.

## Revisão de caminhos para OneDrive — build r04

A compactação anterior media a pasta de execução e podia deixar de ser aplicada em Linux, produzindo nomes inadequados depois da transferência para o OneDrive. O índice apenas sinalizava parte dos excessos. A candidata agora usa nomes físicos curtos desde a criação, conserva a identificação lógica nos metadados e audita todos os formatos contra o destino Windows (CSV/XLSX 210, documentos 240, demais 259). Reserva mínima da raiz: 120 caracteres; `MONITORA_DESTINO_COMPARTILHAMENTO` informa um destino maior e aceita contrabarras.

Relatórios analíticos: `08_analises/u_<hash>/analitico_sintetico` e `analitico_detalhado`. Validação: `07_validacao`. Auditorias: `03_aud`. Pastas auxiliares do painel e nomes longos de auditorias também foram reduzidos. `01_produtos_dados` e `02_painel_correcoes/linhagem` continuam canônicos. Nomes lógicos são usados para comparar produtos de layouts antigo/novo. Duplicatas conflitantes continuam bloqueadas. O QField encurta apenas os ancestrais externos; pacote, ZIP e conteúdo interno permanecem idênticos na migração.

Os limites de entrega foram verificados após renomear referências, metadados, índices e hashes. O migrador recusa colisões e sobrescrita de destinos sem manifesto, preserva a origem e é idempotente. Os caches de clima adicionais acompanham as entregas em ZIP, com bytes/estrutura preservados; o iniciador os extrai em uma pasta temporária curta. Não houve redução de chaves SHA256 nem alteração de contratos de cache.

## Resultado consolidado em 24/09/2026

| UC | Produtos esperados | Linhas / campos validados | Sintético: tabelas / figuras | Detalhado: tabelas / figuras | Maior documento | Maior CSV/XLSX | Maior caminho geral |
|---|---:|---:|---:|---:|---:|---:|---:|
| EET | 18/18 | 12.726 / 129 | 5 / 7 | 28 / 34 | 191 | 208 | 249 |
| PNM | 22/22 | 31.209 / 129 | 7 / 8 | 31 / 50 | 191 | 208 | 248 |

Os dados corrigidos são idênticos às fontes por SHA256. Durante a migração foram preservados integralmente 142 arquivos protegidos na EET e 157 no PNM, abrangendo dados, linhagem, cache e interior QField. Por UC: 34 vínculos HTML e 26 relações OOXML locais conferidos. PDFs analíticos mantêm sequência contínua, inclusive quando tabelas/figuras/seções são omitidas. Inspeção visual por amostragem documentada na área local.

A EET conserva a rodada r01 completa e incorpora somente relatórios de apoio/comparação já gerados na r02 sobre dados idênticos, conforme ORIGEM_CONSOLIDACAO.json. O PNM concluiu em r03 a homologação interrompida externamente em r02 (saída 0, 2.033 s). O build r04 posterior ajusta caminhos e seus leitores; foi testado por funções, migração e conferência de vínculos, sem repetir as análises. CODIGO.json distingue a fonte executada da candidata entregue. Os manuais PDF foram atualizados exclusivamente a partir do HTML com caminhos revisados.

Gates: comparação pré/pós em layouts antigo/organizado/misto e nomes longos/curtos; ausência e ambiguidade; nove cenários HTTP; PDF isolado e rejeição de resultado antigo; cache íntegro/adulterado/incompleto; 256 combinações editoriais; contrato 129; classificação de produtos; índice e destino Windows com contrabarras; Unicode/dispositivo gráfico; análise sintática e limite LF/CRLF. Testes focalizados também executados no R Windows. Execuções integrais realizadas em Linux. Interface interativa do RStudio e navegação no aparelho QField não ensaiadas nesta etapa. Candidata não publicada.

Avisos: os avisos Unicode da rodada histórica EET r01 permanecem nos logs; a correção passou nos testes e o PNM r03 não os reproduziu. GDAL QUALITY refere-se ao driver intermediário: formato, integridade e cobertura das imagens finais passaram. A reprojeção de vetores durante a extração é registrada; não muda os dados biológicos. Avisos geom_text/geom_col/geom_label de elementos ausentes ou fora da escala e tabelas auxiliares sem colunas ficam preservados para rastreabilidade, sem convertê-los em dados inventados. O alerta estimativo de corte do PNG 45 PNM foi inspecionado visualmente: não foi observado corte na imagem exportada. Isso não equivale a uma revisão científica independente de todas as figuras.

### Reprodução dos testes locais

Executar `python3 tools/dev_v303.py` antes dos testes; ele deriva a candidata da v3.0.2 e prepara os módulos/fixtures auxiliares ignorados pelo Git. Depois executar os testes `tests/test_v303_*.R` e `tests/test_v303_rede.py` conforme seus argumentos. O teste de cache recebe o diretório de um snapshot real de fogo já adquirido; não consulta a rede. Scripts em `tools/v303` permitem migrar e conferir produtos existentes sem executar o pipeline.

Tamanho final: 4,868,787 bytes LF; **4,953,005 bytes CRLF**, abaixo de 5.000.000. SHA256 LF: `00c9eb58900655de09fb4786374acf1bf25270923ab776368f3519cd535d92de`.
