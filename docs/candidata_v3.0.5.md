# Desenvolvimento e homologação da v3.0.5

A candidata final é `v3.0.5-rc01-20260929-r12`. As revisões intermediárias r07–r11 ficam documentadas como histórico; seus leiautes e contagens não descrevem a entrega final.

A versão final mantém todas as páginas em retrato e cada figura de evidência temporal inteira em uma página. Ajusta larguras e bordas das tabelas, centraliza valores e cabeçalhos numéricos, preserva conteúdos equivalentes em PDF e Word, expande siglas na primeira menção e organiza o glossário por tema e ordem alfabética. A r12 mantém títulos junto ao elemento seguinte mesmo quando há marcador vazio no Word.

A r11 corrigiu o uso do protocolo histórico como barreira nas categorias gerais harmonizadas. Mantém o histórico como proveniência e na elegibilidade do detalhamento do material botânico; preserva formação, continuidade espacial e requisitos próprios dos modelos. Ver `REVISAO_v305_r11.md`.

A homologação final executa integralmente FNB, PNI e PNM, reaproveitando dados corrigidos e caches. PDF e Word são conferidos quanto a conteúdo, matrizes de células, imagens, numeração, índices, geometria e páginas vazias. A revisão visual dirigida complementa os testes; não representa leitura individual de todas as páginas. As versões/builds efetivamente executados e a reaplicação da correção Word são rastreados nas entregas.

Dois defeitos prévios de reprodutibilidade foram corrigidos: dependência do estado aleatório anterior na comparação editorial por período e redução silenciosa de reamostragens pelo perfil de recursos. Comparações antigas podem mudar intervalos e classificações após essas correções, sem alteração dos dados ou das estimativas centrais. A homologação de PNM inclui reprodução controlada das configurações antiga e atual.

A promoção depende de `GATES_FINAIS.json` aprovado para cada UC, correspondente ao hash da candidata. A ferramenta `tools/promover_v305.py` altera somente três identificadores de versão/build, verifica reversibilidade byte a byte e o limite de 5.000.000 bytes em LF/CRLF. O estado da publicação e os resultados finais ficam nas notas da versão e no registro final de homologação.

Bases e relatórios institucionais não são incorporados ao repositório público.
