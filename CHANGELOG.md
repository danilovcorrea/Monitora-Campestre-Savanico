# Changelog

Todas as mudanças relevantes deste projeto são documentadas neste arquivo.

## [v2.3.0] - 2026-06-18

### Destaques

- Publica a versão `v2.3.0` após a `v2.2.2`.
- Amplia o Painel de validação - correções assistidas de `registros_corrig`.
- Introduz correção em lote por múltiplas COLETAS com filtros superiores hierárquicos.
- Introduz exclusão auditável de COLETAS filtradas ou selecionadas.
- Altera a deduplicação para preservar COLETAS distintas com mesma UC + UA + ANO para curadoria manual.
- Adiciona trava pós-correções para impedir análise estatística quando restarem conflitos de UAs duplicadas no mesmo ano.
- Inclui `COLETA` em `registros_corrig_stat`, antes de `UC`.

### Adicionado

- Filtros superiores hierárquicos e multisseleção no painel: UC(s), EA(s), ano(s), ciclo(s), campanha(s), UA(s) e COLETAS.
- Checkbox para usar todas as COLETAS resultantes dos filtros superiores.
- Prévia auditável por COLETA antes de gerar operações em lote.
- Operações de correção em lote com uma operação por COLETA por atributo.
- Botão para adicionar exclusão auditável de COLETAS filtradas/selecionadas.
- Filtro de triagem para UAs duplicadas no mesmo ano.
- Auditorias pré e pós-correções de COLETAS com UAs duplicadas por ano.
- Cache de localização de linhas por COLETA, coleta_uuid, uuid_registro e linha_indice.
- Coluna `COLETA` em `registros_corrig_stat`, posicionada antes de `UC`.

### Alterado

- A deduplicação automática foi ajustada para manter registros genuinamente idênticos, mas preservar COLETAS distintas envolvidas em conflitos UC + UA + ANO.
- Casos de múltiplas COLETAS para a mesma UC + UA + ANO deixam de ser resolvidos automaticamente e passam a ser encaminhados para triagem/correção no painel.
- A etapa pós-correções passa a bloquear a continuidade da execução quando conflitos de UAs duplicadas no mesmo ano permanecem não resolvidos.
- O README foi atualizado para refletir os novos fluxos de curadoria, mantendo as seções públicas anteriores, incluindo uso auxiliar de IA.
- Os links do README foram atualizados para as cinco cópias públicas da `v2.3.0`.

### Corrigido

- Redução de risco operacional em edições de atributos uniformes por COLETA.
- Evita que conflitos reais de campo sejam mascarados por deduplicação automática.
- Reforça a rastreabilidade entre `registros_corrig.csv` e `registros_corrig_stat.csv` por meio do atributo `COLETA`.

### Validação recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"`.
- Testar correção em lote por múltiplas COLETAS.
- Testar seleção automática de COLETAS pelos filtros superiores.
- Testar exclusão auditável de COLETAS.
- Testar caso com múltiplas COLETAS na mesma UC + UA + ANO e confirmar interrupção pós-correções quando o conflito não for resolvido.
- Conferir `output/registros_corrig.csv`, `output/registros_corrig_stat.csv` e auditorias de COLETAS com UAs duplicadas.
- Conferir que as cinco cópias públicas do script têm SHA256 idêntico.



## [v2.2.2] - 2026-06-18

### Destaques

- Publica a versão `v2.2.2` após a `v2.2.1`.
- Estabiliza o Painel de validação - correções assistidas de `registros_corrig`.
- Mantém o relatório de ocorrência de formas de vida exóticas, o painel assistido e o fluxo analítico, estatístico e gráfico consolidado.

### Adicionado

- Pré-validação transacional de grupos de correção.
- Auditoria semântica pré/pós-correção.
- Tabela unificada de triagem do painel.
- Localização acelerada por `linha_indice`.
- Deduplicação defensiva por assinatura semântica.

### Alterado

- Triagem de exóticas passa a exigir vínculo operacional estrito entre `Encostam`, forma de vida e espécie.
- Hábito passa a ser aceito apenas para formas condicionais.
- Mapa canônico estrutural de colunas passa a ser cacheado.
- Comentários do script foram revisados para remover menções a versões internas e comentários interlocutórios.

### Corrigido

- Bloqueio de correções parciais em movimentos assistidos.
- Redução de reintrodução de tokens residuais após movimentos exótica → nativa.
- Tratamento mais seguro de CSVs vazios, warnings de exportação e objetos temporários.
- Melhoria de checkpoints, progresso textual e controle de recursos.

## [v2.2.1] - 2026-06-16

### Adicionado
- Relatório de ocorrência de formas de vida exóticas em output/.
- Produtos CSV auxiliares com registros de formas de vida exóticas com e sem espécie vinculada.
- Resumos por unidade, forma de vida e campo de espécie exótica.

### Corrigido
- Triagem do relatório restrita a registros com token exotica em **Encostam** na vareta.
- Correção da contagem de espécies exóticas vinculadas.
- Exclusão de campos auxiliares indevidos, como .id, da detecção de espécies.
- Tratamento de NA, campos vazios e --- como ausência nos produtos exportados.
- Reconhecimento de campos textuais abertos do tipo Outra espécie ... exótica como espécie vinculada quando associados à forma de vida exótica correspondente.
# Changelog

Todas as mudanÃ§as relevantes deste projeto sÃ£o documentadas neste arquivo.

## [v2.2.0] - 2026-06-16

### Destaques

- Consolida a evoluÃ§Ã£o posterior Ã  Ãºltima versÃ£o pÃºblica `v2.1.3`.
- Introduz o **Painel de validaÃ§Ã£o - correÃ§Ãµes assistidas de `registros_corrig`**.
- MantÃ©m o fluxo analÃ­tico principal da sÃ©rie `v2.1.x`, incluindo estatÃ­sticas temporais, grÃ¡ficos publicÃ¡veis seriados, painÃ©is amostrais por ano inicial e relatÃ³rio textual estatÃ­stico.

### Adicionado

- Painel Shiny opcional para validaÃ§Ã£o e correÃ§Ã£o assistida de registros consolidados.
- VariÃ¡vel explÃ­cita no inÃ­cio do script para abertura do painel:
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` abre o painel;
  - `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` executa sem painel.
- CSV longo de correÃ§Ãµes assistidas em `input/correcoes_campos.csv`.
- Auditoria de correÃ§Ãµes em `log/auditoria_correcoes_campos_*.csv` e cÃ³pia da Ãºltima execuÃ§Ã£o em `output/correcoes_campos/`.
- Metadados embutidos dos XLSForms 2022, 2023, 2024 e 2025 para regras de validaÃ§Ã£o do painel, sem dependÃªncia de leitura de XLSForms externos.
- DicionÃ¡rios auxiliares e diagnÃ³stico adaptÃ¡vel aos atributos reais presentes em `registros_corrig`.
- Triagem de formas de vida exÃ³ticas nos registros selecionados, com exibiÃ§Ã£o de coleta, ponto amostral, ponto metro, forma exÃ³tica e UUID do registro.
- Movimento assistido de forma de vida entre `exotica`, `nativa` e `seca_morta`.
- Controle de campos condicionais para formas como `samambaia`, `orquidea`, `cactacea` e `bromelioide`, incluindo hÃ¡bito `terrestre`, `epifita` ou `rupicola` quando aplicÃ¡vel.
- HarmonizaÃ§Ã£o auditÃ¡vel de campos superiores e inferiores vinculados pelo XLSForm.

### Alterado

- O painel de validaÃ§Ã£o Ã© opcional e nÃ£o altera a execuÃ§Ã£o analÃ­tica padrÃ£o quando desativado.
- O valor padrÃ£o da opÃ§Ã£o do painel foi mantido como `"N"` para preservar a execuÃ§Ã£o normal em produÃ§Ã£o.
- A aplicaÃ§Ã£o de correÃ§Ãµes passou a usar resoluÃ§Ã£o defensiva de nomes de colunas, considerando variaÃ§Ãµes de labels, HTML, aspas escapadas, acentos e pontuaÃ§Ã£o.
- O recÃ¡lculo de `**Encostam** na vareta: (amostragem/registro)` passou a ser feito a partir das categorias de forma de vida apÃ³s as correÃ§Ãµes assistidas.
- A documentaÃ§Ã£o pÃºblica foi atualizada para incluir o painel, mantendo as seÃ§Ãµes institucionais, tÃ©cnicas, de versionamento e de uso auxiliar de IA.

### Corrigido

- RemoÃ§Ã£o de riscos associados Ã  ediÃ§Ã£o direta de `registros_corrig.csv` em planilhas, por meio de correÃ§Ãµes guiadas e auditadas.
- Tratamento de movimentos exÃ³tica â†’ nativa para limpar/atualizar campos inferiores e superiores vinculados.
- Compatibilidade entre campos do XLSForm e colunas consolidadas com labels ou HTML representados de formas distintas.
- ProteÃ§Ã£o contra correÃ§Ãµes malformadas geradas por seleÃ§Ã£o de linhas prÃ©-triadas.

### ValidaÃ§Ã£o recomendada

- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"` para validar o fluxo analÃ­tico padrÃ£o.
- Executar o script com `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "S"` para validar o painel.
- Testar ao menos:
  - uma correÃ§Ã£o simples/lote por coleta;
  - um movimento assistido exÃ³tica â†’ nativa;
  - conferÃªncia de `output/registros_corrig.csv`;
  - conferÃªncia de `output/correcoes_campos/auditoria_correcoes_campos_ultima_execucao.csv`.
- Conferir que os cinco scripts pÃºblicos tÃªm o mesmo SHA256.

## [v2.1.3] - 2026-06-12

### Destaques

- Ãšltima versÃ£o pÃºblica da sÃ©rie `v2.1.x` antes da publicaÃ§Ã£o da `v2.2.0`.
- MantÃ©m os produtos editoriais, estatÃ­sticos e grÃ¡ficos consolidados nas versÃµes `v2.1.0` a `v2.1.2`.

### Corrigido

- Tratamento defensivo para exportaÃ§Ã£o de grÃ¡ficos em subconjuntos vazios.
- Ajustes em parÃ¢metros de rÃ³tulos e exportaÃ§Ã£o para reduzir falhas em cenÃ¡rios de dados filtrados ou incompletos.
- Compatibilidade com arquivos manipulados em editores de planilha, preservando as correÃ§Ãµes jÃ¡ consolidadas na sÃ©rie `v2.1.x`.

## [v2.1.2] - 2026-06-12

### Alterado

- RefatoraÃ§Ã£o nominal interna e revisÃ£o editorial da documentaÃ§Ã£o.
- PadronizaÃ§Ã£o de nomes internos de funÃ§Ãµes utilitÃ¡rias, objetos globais, rotinas de recursos, auditoria, grÃ¡ficos, layout, estatÃ­stica e relatÃ³rio textual.
- ConsolidaÃ§Ã£o da nomenclatura pÃºblica baseada em **painÃ©is amostrais por ano inicial**.
- AtualizaÃ§Ã£o editorial do `README.md`, com recuperaÃ§Ã£o das informaÃ§Ãµes sobre uso auxiliar de IA.

### ValidaÃ§Ã£o

- PreservaÃ§Ã£o dos produtos analÃ­ticos principais em relaÃ§Ã£o ao baseline de refatoraÃ§Ã£o.
- Scripts principais sincronizados.

## [v2.1.1] - 2026-06-12

### Destaques

- RevisÃ£o editorial dos produtos pÃºblicos apÃ³s a publicaÃ§Ã£o da `v2.1.0`.
- AtualizaÃ§Ã£o da terminologia pÃºblica de â€œcoortesâ€ para **painÃ©is amostrais por ano inicial**.
- PadronizaÃ§Ã£o da nomenclatura pÃºblica dos grÃ¡ficos com serial global `fig_001_...png` a `fig_156_...png`.
- AmpliaÃ§Ã£o do relatÃ³rio textual estatÃ­stico.

### Adicionado

- Ãndice mestre de grÃ¡ficos `output/indice_graficos.csv`.
- GrÃ¡ficos publicÃ¡veis seriados em `output/plots_png/`.
- Tabelas estatÃ­sticas dos painÃ©is amostrais por ano inicial.

### Corrigido

- ClassificaÃ§Ã£o pÃºblica da `fig_036`.
- AusÃªncia da exportaÃ§Ã£o de composiÃ§Ã£o geral contra linha de base dos painÃ©is amostrais por ano inicial.
- Defasagem conceitual do relatÃ³rio textual estatÃ­stico.

## [v2.1.0] - 2026-06-11

### Destaques

- Consolida a evoluÃ§Ã£o desde a Ãºltima versÃ£o pÃºblica `v2.0.2`.
- Inclui novos produtos analÃ­ticos, grÃ¡ficos editoriais, anÃ¡lises longitudinais por ano inicial e auditorias.

### Adicionado

- GrÃ¡ficos temporais editoriais com escopo amostral explÃ­cito.
- PainÃ©is editoriais para amostra total por ano, UAs presentes em todos os anos avaliados e comparaÃ§Ãµes pareadas por perÃ­odo consecutivo.
- EstatÃ­stica pareada especÃ­fica para grÃ¡ficos editoriais perÃ­odo a perÃ­odo.
- RelatÃ³rio textual estatÃ­stico em `output/relatorio_textual_estatistico.txt`.
- Auditorias de layout de rÃ³tulos, sÃ­mbolos estatÃ­sticos, esforÃ§o amostral, performance e memÃ³ria.

### Corrigido

- RÃ³tulos de ano e `n UA` embaralhados em facetas.
- Linhas ou rÃ³tulos duplicados em painÃ©is temporais.
- SobreposiÃ§Ã£o de sÃ­mbolos estatÃ­sticos e rÃ³tulos.
- Legendas inferiores que ultrapassavam os limites dos painÃ©is.
- Compatibilidade entre fontes com sobreposiÃ§Ã£o de exportaÃ§Ãµes.

## [v2.0.2] - 2026-06-10

### Destaques

- Ãšltima versÃ£o pÃºblica antes da consolidaÃ§Ã£o `v2.1.0`.
- Continha o nÃºcleo de tratamento, padronizaÃ§Ã£o, deduplicaÃ§Ã£o, estatÃ­stica, auditoria e relatÃ³rio textual.

### Alterado

- Ajustes de consistÃªncia entre cÃ³pias pÃºblicas do script.
- Ajustes editoriais e de documentaÃ§Ã£o da sÃ©rie `v2.0.x`.

## [v2.0.1] - 2026-06-10

### Alterado

- Ajustes incrementais de publicaÃ§Ã£o.
- OrganizaÃ§Ã£o de arquivos.
- ConsistÃªncia entre cÃ³pias do script.
- PreparaÃ§Ã£o da linha pÃºblica para revisÃ£o documental e tagueamento estÃ¡vel.

## [v2.0.0] - 2026-06-10

### Destaques

- Primeira versÃ£o pÃºblica com adoÃ§Ã£o de versionamento semÃ¢ntico.
- ConsolidaÃ§Ã£o estatÃ­stica, auditoria e relatÃ³rio textual.
- OrganizaÃ§Ã£o pÃºblica do repositÃ³rio com cÃ³pia congelada em `releases/v2.0.0/`.
- Registro explÃ­cito do uso auxiliar de IA generativa a partir da fase de consolidaÃ§Ã£o pÃºblica.

### Adicionado

- ImportaÃ§Ã£o de mÃºltiplos tipos de entrada.
- ExtraÃ§Ã£o recursiva de ZIPs do SISMONITORA.
- Auditoria de arquivos candidatos Ã  importaÃ§Ã£o.
- DeduplicaÃ§Ã£o semÃ¢ntica de registros equivalentes.
- VerificaÃ§Ã£o de integridade dos dados.
- Tratamento defensivo de colunas, datas, coordenadas e aliases.
- Controle de performance, memÃ³ria e recursos computacionais.
- AnÃ¡lise estatÃ­stica inferencial pareada por unidade amostral.
- ComparaÃ§Ãµes ano a ano e contra linha de base acumulada.
- Teste de permutaÃ§Ã£o pareado.
- Intervalo de confianÃ§a por bootstrap.
- CorreÃ§Ã£o de mÃºltiplas comparaÃ§Ãµes por FDR.
- AnÃ¡lise de mudanÃ§a na composiÃ§Ã£o geral com distÃ¢ncia de Bray-Curtis.
