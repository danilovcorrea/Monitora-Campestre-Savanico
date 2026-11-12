---
title: 'Manual do usuário — Script monitora_campsav_alvo_global'
subtitle: 'Versão 3.0.7'
version: '3.0.7'
build: 'v3.0.7-20261112-r01'
output:
  html_document:
    toc: true
    toc_depth: 3
    number_sections: true
  pdf_document:
    toc: true
    number_sections: true
header-includes:
  - \usepackage{longtable}
  - \usepackage{booktabs}
geometry: margin=1.2cm
fontsize: 10pt
---

<style>table.monitora-tabela,table{width:100%;table-layout:fixed;border-collapse:collapse;font-size:11px;}td,th{white-space:normal;word-break:break-word;overflow-wrap:anywhere;vertical-align:top;padding:4px;}pre,code{white-space:pre-wrap;word-break:break-word;}</style>

<style>
:root{--verde:#24543c;--verde-claro:#eaf3ed;--ouro:#c89b3c;--cinza:#4b5563;}
body{font-family:'Segoe UI',Arial,sans-serif;color:#24312b;line-height:1.55;max-width:1120px;margin:auto;padding:18px 28px;text-align:justify;}
h1{color:var(--verde);border-bottom:2px solid var(--ouro);padding-bottom:6px;margin-top:1.8em;text-align:left;}
h2,h3{color:#35664d;text-align:left;} a{color:#236b4b;} code{color:#334155;background:#f3f6f4;}
.monitora-capa{border-left:8px solid var(--verde);background:linear-gradient(120deg,var(--verde-claro),#fff);padding:22px 26px;margin:18px 0 30px;}
.monitora-capa .kicker{color:var(--verde);font-weight:700;text-transform:uppercase;letter-spacing:.08em;}
.monitora-resumo{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:10px;margin:16px 0 22px;}
.monitora-card{border:1px solid #d7e2da;border-top:4px solid var(--verde);border-radius:5px;padding:10px 12px;background:#fff;text-align:left;}
.monitora-alerta{border-left:5px solid var(--ouro);background:#fff8e8;padding:10px 14px;margin:12px 0;}
.monitora-ok{border-left:5px solid var(--verde);background:var(--verde-claro);padding:10px 14px;margin:12px 0;}
table.monitora-tabela,table{border:1px solid #d7e2da;} th{background:var(--verde);color:#fff;} tr:nth-child(even){background:#f7faf8;}
@media print{body{font-size:9.5pt;max-width:none;padding:0;} #TOC,.tocify,#header,.title,.subtitle{display:none!important;} h1{page-break-after:avoid;} table{page-break-inside:auto;} .monitora-card,.monitora-capa{break-inside:avoid;} p:has(code){text-align:left;} a[href]:after{content:''!important;}}
</style>

<div class="monitora-capa">

<div class="kicker">

Programa Monitora · CBC/ICMBio

</div>

<h1>

Manual operacional
</h1>

<p>

Tratamento, validação e análise dos dados do Alvo Global Plantas
Herbáceas e Lenhosas — Componente Campestre Savânico.
</p>

</div>

# Finalidade

Este manual descreve a operação do script do Alvo Global Plantas
Herbáceas e Lenhosas do Componente Campestre Savânico. O foco é permitir
que uma pessoa recém-chegada à equipe prepare a entrada, execute o
fluxo, opere o painel, interprete diagnósticos e confira todos os
produtos sem editar manualmente as bases.

O manual é materializado antes da abertura do painel sempre que o modo
de execução for orientado a painel ou quando
`MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES = 'S'`. Assim, o usuário pode
consultar o documento durante a curadoria.

## O que pode e o que não pode ser editado

O usuário deve editar somente as variáveis documentadas no bloco
operacional inicial e os dados por meio dos controles do painel.
Preserve o arquivo baixado, o `registros_corrig.csv`, a pasta
`linhagem/`, os manifestos, as auditorias e os produtos canônicos. Não
edite o corpo do script, a inicialização do RStudio, o contrato único, o
XLSForm 2025, domínios de choices, regras de relevance, arquivos de
linhagem ou CSVs do output para fazer uma ocorrência desaparecer. O
contrato valida os registros; ele nunca deve ser alterado ou
flexibilizado para acomodar erro do conjunto de dados. Quando a decisão
biológica não for inequívoca, a ocorrência deve permanecer para
curadoria ou justificativa, nunca ser inferida pelo código.

Trabalhe preferencialmente em uma cópia local curta, fora da
sincronização ativa do OneDrive. Depois de concluir e conferir, copie a
pasta final para o diretório institucional. Não execute duas instâncias
sobre a mesma pasta, não mova arquivos enquanto o R estiver escrevendo e
não use **Stop** durante salvamento, promoção de produtos ou criação de
ZIP.

## Projeto QField de navegação

Ative `MONITORA_OPCAO_GERAR_PROJETO_QFIELD = 'S'` para criar
`output/09_qfield/` após a preparação espacial, sem alterar dados. O
padrão S solicita o módulo; bloqueios são registrados no console sem
interromper os demais produtos. Com
`MONITORA_OPCAO_IMPORTAR_CAMADAS_QFIELD = 'S'`, uma execução contendo
uma UC aceita diretamente em `qfield_input/` arquivos MBTiles e vetores
KML, KMZ, GPKG ou ZIP de shapefile. Não crie subpasta com o nome da UC,
não imponha nome ao MBTiles e não coloque esses arquivos no `input/`
biológico.

Cada MBTiles direto é inspecionado como banco SQLite/MBTiles; metadados
e tiles materializados precisam concordar. A classificação usa
`zoom_max_real`: 18 ou mais é **detalhe**, 15–17 é **operacional** e 14
ou menos é **regional**. Nome, pasta e extensão textual não substituem
essa inspeção. Quando há detalhe, o script usa os consensos aceitos pela
validação espacial para obter o ponto médio e produzir um círculo de 500
m por posição de referência (ou por alternativa temporal observada);
grava `recorte_imagens_500m.gpkg`, descarta tiles sem interseção e
mascara a borda em WebP transparente. O MBTiles em `qfield_input/`
permanece intocado. Um recorte anterior somente é reutilizado quando UC,
UAs, raio, formato e impressão digital dos buffers coincidem.

Nomes exibidos das camadas observadas: `verg_ini_YYYY` e
`verg_fin_YYYY`; nomes físicos preservam a UC. As posições mantêm UA,
ano e COLETA nas coordenadas literais. Referência temporal ambígua ou
insuficiente é alerta não impeditivo quando as coordenadas são
utilizáveis; preserve as posições e confira os vergalhões em campo, sem
correção artificial nem justificativa obrigatória. Divergências dos
extremos, possível troca de UA, inversão e coordenadas inválidas ou
conflitantes continuam bloqueando o projeto da UC. Nas UAs com alerta
temporal, as alternativas aparecem nas camadas e no KML, com status e
situação nos atributos; o recorte reúne círculos de 500 m em torno de
cada posição observada. O centro de cada círculo é apenas o ponto médio
do respectivo transecto, sem escolher uma posição como verdadeira. A
auditoria preserva a rastreabilidade e as análises científicas mantêm
seus critérios de elegibilidade. Nas demais UAs, o KML continua usando o
último ano como referência operacional.

Vetores comuns são importados como camadas adicionais somente leitura e
conservam arquivo, camada e atributos; o script não presume que toda
linha é estrada nem que todo ponto é PA. Para papel especial, use
`camadas_qfield.csv` com `arquivo`, `camada` e `papel`:
`pa_priorit_ini`, `pa_altern_ini` ou `acesso`; `sigla` é opcional. Os
nomes padronizados dos PAs são `UC_PA_priorit_verg_ini` e
`UC_PA_altern_verg_ini`, ambos referentes ao vergalhão inicial previsto.
`apoio_campo.gpkg` contém `pontos_interesse` e `trajeto` editáveis;
preserve uma cópia preenchida antes de substituir o projeto.
`projeto_qfield.csv` pode declarar `UC,sigla`. Não importar projetos
QGIS antigos, macros, ações ou links externos como se fossem dados.

### Passo a passo do QField

1.  Execute uma UC por vez e mantenha a validação espacial
    habilitada. 2. Se houver imagem detalhada, copie o MBTiles original
    diretamente para `qfield_input/`; copie também estradas, trilhas,
    acessos, PAs ou pontos de interesse nos formatos aceitos. 3. Ative
    as duas opções QField quando houver arquivos externos; para projeto
    apenas com Sentinel gerado pelo script, a importação pode ficar
    em N. 4. Execute normalmente e aguarde a promoção do pacote: falha
    QField preserva os demais produtos e grava o motivo. 5. Abra
    `auditoria_imagens.csv`, `auditoria_recorte_circular.csv`,
    `auditoria_camadas.csv` e `auditoria_cobertura.csv`. Todos os
    extremos anuais precisam ter pixel válido. 6. Importe o ZIP numa
    pasta nova no aplicativo e confira nomes, símbolos, coordenadas em
    graus decimais, imagens, acessos e camadas editáveis.

### Teste obrigatório do projeto QField

Faça dois testes antes do campo. **Online:** habilite
`Google Satellite`, confirme que é apenas visualização sob demanda e que
não substitui os fundos locais. **Modo avião:** confirme que MBTiles
detalhado/regional, extremos, escala, orientação e vetores essenciais
continuam disponíveis. A camada Google inicia desativada, exige internet
e nunca é baixada nem empacotada. Sentinel-2 possui informação nativa de
10 m e serve para contexto regional; zoom adicional não o transforma em
imagem detalhada. Fonte, licença, data e resolução de uma imagem
fornecida permanecem não declaradas quando o usuário não as informa.

## Três percursos de uso

### Primeira validação a partir de dados originais

Prepare uma pasta exclusiva, preserve o input original, escolha
`painel_e_parar` para a curadoria e, depois, continue com o
`registros_corrig.csv` e a linhagem produzidos juntos.

### Continuação de uma validação

Copie para o novo `input/` o `registros_corrig.csv` e toda a pasta
`linhagem/` da mesma rodada. Use `painel_incremental_*` e mantenha a
reaplicação de correções anteriores em N.

### Geração de produtos finais

Quando a base já estiver corrigida e a linhagem íntegra, use
`registros_corrig_completo` ou a variante sem PNG/gráficos. Ative
somente os produtos opcionais necessários.

## Decisão rápida

- **Há input original e é preciso corrigir?** Use `painel_e_parar`.
- **Há registros_corrig e linhagem da mesma rodada e é preciso
  continuar?** Use `painel_incremental_*`.
- **Há registros_corrig final e é preciso gerar todos os produtos?** Use
  `registros_corrig_completo`.
- **Há somente um cache compatível da mesma base?** Use
  `abrir_painel_cache`.
- **Há dúvida sobre a correspondência entre base e linhagem?** Não
  prossiga; verifique manifesto, hashes e inventário de sessões.

## Estrutura mínima da pasta

``` text
pasta_da_rodada/
├── monitora_campsav_alvo_global_*.R
├── input/
│   ├── arquivos brutos OU registros_corrig.csv
│   ├── linhagem/                   # da mesma rodada do checkpoint
│   └── novas_coletas/              # somente quando a opção estiver S
├── qfield_input/                   # opcional; arquivos espaciais de uma UC
├── output/                         # criado/atualizado pelo script
├── log/                            # registros técnicos
└── manual_usuario/                 # manual e dados de apoio
```

# Início rápido

1.  Crie uma pasta exclusiva para a rodada, coloque o `.R` na raiz e os
    arquivos de entrada dentro de `input/`. Não misture campanhas ou
    arquivos auxiliares de linhagem de rodadas diferentes sem seguir o
    fluxo incremental descrito neste manual.
2.  No bloco inicial do script, escolha `MONITORA_MODO_EXECUCAO`,
    informe o responsável e ative somente os produtos necessários. Para
    uma primeira curadoria, use `painel_e_parar`; para gerar tudo após a
    curadoria, use um modo `painel_incremental_*` com o
    `registros_corrig.csv` e a pasta de linhagem correspondentes.
3.  Execute o arquivo inteiro. No RStudio, confirme a execução do script
    grande quando solicitado. Aguarde os checkpoints no console; não
    clique repetidamente nos botões do painel.
4.  No painel, filtre a COLETA, confira a prévia, registre motivo e
    abrangência e só então adicione a operação. Use **Salvar e fechar**
    para materializar a sessão.
5.  Confira a auditoria de produtos finais, as ocorrências pós-painel, a
    linhagem, as justificativas remanescentes e os produtos opcionais
    habilitados.

## Preparação da entrada

O script aceita downloads diretos, CSV, XLSX/XLS, ZIP e lotes
compatíveis. Preserve os arquivos originais sem edição. Uma rodada
incremental deve receber exatamente o checkpoint e a linhagem da mesma
execução. O script diferencia ausência de evidência de ausência de
correção e bloqueia combinações incompatíveis.

### Planilhas XLSX com abas adicionais

Exportações recentes do SISMONITORA podem conter novas abas. Não apague,
mova ou renomeie abas para ajudar o script. Para cada XLSX, o leitor
examina o cabeçalho de todas as abas e exige **uma única aba biológica**
reconhecida pelo esquema contratual; abas administrativas ou auxiliares
permanecem apenas inventariadas. Zero ou mais de uma candidata bloqueiam
a leitura com a lista `aba=tipo`, porque escolher pela posição seria
inseguro. Consulte a auditoria de seleção de aba e, diante de falha,
teste novamente com o download original intacto antes de atribuir o
problema ao código.

## Roteiros integrais por finalidade

A tabela resume as rotinas completas que uma pessoa recém-chegada pode
executar. Leia também a seção específica antes de usar opções
excepcionais.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

rotina
</th>

<th style="text-align:left;">

preparar
</th>

<th style="text-align:left;">

configurar
</th>

<th style="text-align:left;">

conferir
</th>

<th style="text-align:left;">

nunca_fazer
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Primeira curadoria a partir do download original
</td>

<td style="text-align:left;">

Criar pasta local exclusiva; copiar o script e os downloads intactos
para input/.
</td>

<td style="text-align:left;">

MONITORA_MODO_EXECUCAO=‘painel_e_parar’; painel S; produtos finais
opcionais conforme necessidade.
</td>

<td style="text-align:left;">

Painel salvo, registros_corrig.csv, ocorrências pós-painel e linhagem
completa.
</td>

<td style="text-align:left;">

Não editar o download para retirar abas/colunas e não alterar o contrato
para acomodar valores inválidos.
</td>

</tr>

<tr>

<td style="text-align:left;">

Continuar uma curadoria existente
</td>

<td style="text-align:left;">

Copiar juntos registros_corrig.csv e toda a pasta linhagem/ da mesma
execução para input/.
</td>

<td style="text-align:left;">

Usar painel_incremental_registros_corrig para novo checkpoint ou
painel_incremental_completo para seguir aos produtos; reaplicação N.
</td>

<td style="text-align:left;">

Novo checkpoint e novos eventos anexados sem regravar os eventos
herdados.
</td>

<td style="text-align:left;">

Não separar o CSV da linhagem, reaplicar novamente as mesmas decisões
nem copiar arquivos de rodadas diferentes.
</td>

</tr>

<tr>

<td style="text-align:left;">

Gerar produtos finais sem nova edição
</td>

<td style="text-align:left;">

Copiar registros_corrig.csv e a linhagem correspondente para input/; não
misturar sessões.
</td>

<td style="text-align:left;">

Usar registros_corrig_completo, registros_corrig_sem_png ou
registros_corrig_estatisticas_sem_graficos; painel N se não houver nova
curadoria.
</td>

<td style="text-align:left;">

registros_validados.csv quando solicitado, estatísticas, relatórios e
auditoria final sem impedimentos.
</td>

<td style="text-align:left;">

Não chamar checkpoint antigo de validado sem conferir os impedimentos da
execução atual.
</td>

</tr>

<tr>

<td style="text-align:left;">

Incorporar COLETAs omitidas
</td>

<td style="text-align:left;">

Preparar a continuidade normal e colocar somente os arquivos brutos
adicionais em input/novas_coletas/.
</td>

<td style="text-align:left;">

Usar painel_incremental\_\* e
MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS=‘S’.
</td>

<td style="text-align:left;">

Auditoria de incorporação, 101 pontos por COLETA e ausência de
sobreposição divergente.
</td>

<td style="text-align:left;">

Não concatenar CSVs manualmente nem aceitar parcialmente um lote
bloqueado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Reaplicar decisões sobre os mesmos arquivos originais
</td>

<td style="text-align:left;">

Usar cópia idêntica dos arquivos brutos originais e apenas
correcoes_semanticas_consolidada.csv em input/linhagem/.
</td>

<td style="text-align:left;">

Usar modo baseado em input bruto e
MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES=‘S’; não usar modo
incremental.
</td>

<td style="text-align:left;">

Auditorias da reaplicação: toda decisão aplicável ou já satisfeita;
nenhum evento ambíguo.
</td>

<td style="text-align:left;">

Não levar registros_corrig.csv para o replay e não modificar o histórico
consolidado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Comparar a reaplicação com uma rodada de referência
</td>

<td style="text-align:left;">

Preparar a reaplicação e copiar a rodada correta para
input/oraculo_replay/.
</td>

<td style="text-align:left;">

Ativar COMPARAR_REPLAY_COM_ORACULO e ABORTAR_DIVERGENCIA; manter o
diagnóstico permissivo em N.
</td>

<td style="text-align:left;">

Selo replay_equivalente_ao_oraculo=SIM; qualquer diferença impede a
promoção.
</td>

<td style="text-align:left;">

Não usar a rodada de referência como entrada nem ignorar divergência
para publicar resultado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Gerar planilhas de importação do SISMONITORA
</td>

<td style="text-align:left;">

Usar uma execução capaz de materializar registros_validados.csv sem
impedimentos.
</td>

<td style="text-align:left;">

Ativar GERAR_REGISTROS_VALIDADOS e
GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA; escolher
conscientemente a política de UUID.
</td>

<td style="text-align:left;">

Manifesto, hashes, três abas, contexto UC+ciclo+campanha e política de
UUID registrada.
</td>

<td style="text-align:left;">

Não remover UUID para atualização nem preservá-lo para inclusão sem a
finalidade correta e homologada.
</td>

</tr>

<tr>

<td style="text-align:left;">

Reabrir uma sessão interrompida pelo cache
</td>

<td style="text-align:left;">

Confirmar que o cache pertence ao mesmo input e à mesma cadeia de
curadoria.
</td>

<td style="text-align:left;">

Usar abrir_painel_cache ou o caminho explícito somente quando a
correspondência estiver comprovada.
</td>

<td style="text-align:left;">

Identidade/hash do input e checkpoint recuperado antes de continuar;
nunca escolher cache apenas pela data.
</td>

<td style="text-align:left;">

Não reutilizar cache de outro conjunto de arquivos, UC ou versão
incompatível.
</td>

</tr>

<tr>

<td style="text-align:left;">

Gerar relatórios de validação e analíticos
</td>

<td style="text-align:left;">

Executar uma UC por vez para relatórios analíticos; manter a linhagem
junto da base.
</td>

<td style="text-align:left;">

Ativar os módulos e formatos necessários; o mapa orbital acrescenta
rede, tempo e dependências.
</td>

<td style="text-align:left;">

07_relatorio_validacao, 08_relatorios_analiticos, tabelas editáveis e
avisos de renderização.
</td>

<td style="text-align:left;">

Não interpretar associação temporal como causa, nem pares insuficientes
como ausência de mudança.
</td>

</tr>

<tr>

<td style="text-align:left;">

Gerar projeto QField com imagem detalhada
</td>

<td style="text-align:left;">

Em execução de uma UC, copiar MBTiles e vetores diretamente para
qfield_input/; não criar subpasta da UC nem renomear o MBTiles.
</td>

<td style="text-align:left;">

Ativar GERAR_PROJETO_QFIELD e IMPORTAR_CAMADAS_QFIELD; manter a
validação espacial ligada.
</td>

<td style="text-align:left;">

auditoria_imagens.csv, auditoria_recorte_circular.csv, cobertura de
todos os extremos, ZIP e teste no QField.
</td>

<td style="text-align:left;">

Não colocar arquivos espaciais no input/, não editar a fonte e não supor
fonte/licença/data pelo nome.
</td>

</tr>

<tr>

<td style="text-align:left;">

Gerar projeto QField sem imagem fornecida
</td>

<td style="text-align:left;">

Não criar qfield_input artificial. A rede e as coordenadas precisam
permitir a obtenção do Sentinel de contexto.
</td>

<td style="text-align:left;">

Ativar GERAR_PROJETO_QFIELD; IMPORTAR_CAMADAS_QFIELD pode ficar N se não
houver vetores externos.
</td>

<td style="text-align:left;">

Sentinel regional identificado como contexto de 10 m; ele não substitui
imagem detalhada para localizar vergalhões.
</td>

<td style="text-align:left;">

Não interpretar zoom maior como ganho de resolução nativa nem dispensar
a conferência de campo.
</td>

</tr>

</tbody>

</table>

## Incorporação de novas COLETAs

Se COLETAs foram omitidas do input original, não reinicie a curadoria
nem misture manualmente CSVs. Copie o `registros_corrig.csv` e a pasta
`linhagem/` da mesma rodada para `input/`, escolha um modo
`painel_incremental_*`, crie `input/novas_coletas/`, coloque ali somente
os arquivos brutos adicionais e defina
`MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS = 'S'`. O script prepara as
novas entradas pelo mesmo contrato em processo isolado, exige 101 pontos
por COLETA, ignora repetições factualmente idênticas e bloqueia o lote
inteiro diante de qualquer sobreposição divergente, UUID repetido ou
incompletude. A operação é idempotente e fica assinada em
`output/02_painel_correcoes/linhagem/incorporacoes_novas_coletas.csv`.
As correções manuais históricas não são aplicadas por hipótese aos
registros novos; eles entram no painel para a revisão que lhes for
pertinente.

# Configuração inicial

As variáveis abaixo ficam no bloco operacional inicial do script e devem
ser conferidas antes de cada execução. Elas também podem ser
sobrescritas por variáveis de ambiente. O padrão
`painel_incremental_completo` exige exatamente um
`registros_corrig*.csv` e a pasta de linhagem integral da mesma rodadas;
para input bruto, escolha um modo compatível. Relatórios analíticos,
Sentinel, produtos espaciais e planilhas de integração acrescentam
processamento e devem ser desativados quando não forem necessários. Em
caso de conflito, os modos orientados a painel prevalecem sobre a chave
manual de abertura do painel.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

variavel
</th>

<th style="text-align:left;">

valores
</th>

<th style="text-align:left;">

finalidade
</th>

<th style="text-align:left;">

cuidados
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

MONITORA_MODO_EXECUCAO
</td>

<td style="text-align:left;">

padrão painel_incremental_completo; completo; sem_png;
estatisticas_sem_graficos; ate_registros_corrig; painel_e_parar;
abrir_painel_cache; registros_corrig_completo; registros_corrig_sem_png;
registros_corrig_estatisticas_sem_graficos;
painel_incremental_registros_corrig; painel_incremental_completo;
painel_incremental_sem_png; painel_incremental_estatisticas_sem_graficos
</td>

<td style="text-align:left;">

Controla o fluxo principal e define se o script parte da entrada bruta,
de cache ou de registros_corrig já existente.
</td>

<td style="text-align:left;">

É a variável mais importante da execução. Modos de painel prevalecem
sobre MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Solicita abertura do painel no modo completo; nos modos orientados a
painel, o script força S independentemente desta opção.
</td>

<td style="text-align:left;">

No modo completo, N impede o painel. Em painel_e_parar,
abrir_painel_cache e painel_incremental\_\*, o painel abre mesmo se
estiver N.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_REGISTROS_IMPORTADOS
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Materializa registros_importados.csv como snapshot saneado da entrada
antes das correções finais.
</td>

<td style="text-align:left;">

Produto sensível; não publicar. Em modos com painel a geração
bruta/saneada pode ser obrigatória para auditoria pré-painel.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Gera registros_validados.csv compatível com contrato XLSForm/template
SISMONITORA quando o produto corrigido está apto.
</td>

<td style="text-align:left;">

Não é substituto de revisão humana; depende de registros_corrig
consistente e sem bloqueios impeditivos.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_VALIDAR_ESPACIAL_COLETAS
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Ativa validação espacial de COLETAS, relatórios espaciais, mapa e,
quando aplicável, aba espacial do painel.
</td>

<td style="text-align:left;">

A validação espacial depende de coordenadas detectáveis e deve ser
interpretada junto com relatórios específicos.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_ABRIR_ABA_VALIDACAO_ESPACIAL
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Abre a aba espacial dentro do painel quando a validação espacial está
habilitada e há suporte aos dados necessários.
</td>

<td style="text-align:left;">

Se leaflet não estiver instalado, o script mantém tabela e relatórios,
mas pode omitir mapa interativo.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_MANUAL_USUARIO
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Gera o manual do usuário em manual_usuario/. Em modos com painel, HTML e
Rmd ficam disponíveis antes da abertura; o PDF depende de opção própria.
</td>

<td style="text-align:left;">

Não depende de dados reais. O PDF permanece fora do caminho crítico e só
é gerado ao término quando solicitado explicitamente.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_RELATORIO_VALIDACAO_CONSOLIDADO
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Gera o relatório executivo no estado terminal da execução em
output/07_validacao/.
</td>

<td style="text-align:left;">

Produto documental local. Uma falha de renderização é auditada, mas não
interrompe nem invalida os produtos de dados já materializados.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_FORMATOS_RELATORIO_VALIDACAO
</td>

<td style="text-align:left;">

html, pdf, md, rmd
</td>

<td style="text-align:left;">

Define os formatos dos produtos documentais gerados por R Markdown.
</td>

<td style="text-align:left;">

HTML é o formato técnico principal; PDF é preferencialmente gerado via
HTML/pagedown para evitar tabelas largas.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_RESPONSAVEL_CORRECAO
</td>

<td style="text-align:left;">

texto livre ou variável de ambiente
</td>

<td style="text-align:left;">

Define responsável padrão pela correção; o relatório prioriza o
responsável salvo no painel quando houver.
</td>

<td style="text-align:left;">

Preencha no painel ou por variável de ambiente. O valor digitado no
painel tem prioridade.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_INSTITUICAO_RESPONSAVEL
</td>

<td style="text-align:left;">

texto livre; padrão ICMBio
</td>

<td style="text-align:left;">

Define instituição/equipe responsável; para uso institucional, o padrão
automático é ICMBio.
</td>

<td style="text-align:left;">

Evite deixar vazio em execução institucional.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES
</td>

<td style="text-align:left;">

S ou N
</td>

<td style="text-align:left;">

Reaplica operações semânticas de uma rodada anterior antes dos
diagnósticos pré-painel e antes da abertura do Shiny.
</td>

<td style="text-align:left;">

Use deliberadamente: replay muda o estado da base antes do painel. O
relatório audita o arquivo reaplicado.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_ARQUIVO_CORRECOES_ANTERIORES
</td>

<td style="text-align:left;">

caminho para correcoes_semanticas.csv ou correcoes_campos.csv
</td>

<td style="text-align:left;">

Define o ledger a reaplicar; se vazio, procura a trilha consolidada em
input/linhagem/ e input/, depois os nomes legados aceitos.
</td>

<td style="text-align:left;">

Prefira correcoes_semanticas_consolidada.csv exportado pelo script;
correcoes_campos.csv legado é aceito, mas pode depender mais da
estrutura da rodada original.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GRAVAR_TRILHA_SEMANTICA_CORRECOES
</td>

<td style="text-align:left;">

S ou N
</td>

<td style="text-align:left;">

Grava uma trilha semântica consolidada das operações do painel para
replay em versões futuras.
</td>

<td style="text-align:left;">

Mantenha ligado para preservar a intenção das operações
independentemente da evolução posterior do script.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_REABRIR_PAINEL_DO_CACHE
</td>

<td style="text-align:left;">

S ou N; compatibilidade histórica com reabertura de cache
</td>

<td style="text-align:left;">

Força reabertura de painel por cache em instalações antigas; na prática,
prefira MONITORA_MODO_EXECUCAO = abrir_painel_cache.
</td>

<td style="text-align:left;">

Mantida para compatibilidade; o modo explícito é mais claro e auditável.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_CACHE_PAINEL_DIR
</td>

<td style="text-align:left;">

caminho opcional para pasta de cache
</td>

<td style="text-align:left;">

Permite apontar manualmente a pasta de cache quando o script não deve
usar o local padrão.
</td>

<td style="text-align:left;">

Só use se souber qual cache pertence ao input atual.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_CACHE_PAINEL_ARQUIVO
</td>

<td style="text-align:left;">

arquivo opcional de cache
</td>

<td style="text-align:left;">

Permite apontar um cache específico para reabrir painel. Use com cautela
para não reabrir cache de outro input.
</td>

<td style="text-align:left;">

O uso de cache incorreto pode aplicar correções fora de contexto.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_INCORPORAR_NOVAS_COLETAS
</td>

<td style="text-align:left;">

padrão N; S ou N; padrão N
</td>

<td style="text-align:left;">

Permite incorporar arquivos brutos com COLETAs omitidas a uma
continuidade painel_incremental\_\* sem perder o checkpoint nem a
linhagem já materializados.
</td>

<td style="text-align:left;">

Com N, input/novas_coletas/ não é consultado. Com S, cada COLETA precisa
ter exatamente 101 pontos; duplicata idêntica é ignorada, e sobreposição
divergente, UUID repetido ou incompletude bloqueia atomicamente o lote
inteiro.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Gera uma planilha de importação para cada contexto UC + ciclo + campanha
a partir do registros_validados.csv materializado e validado na mesma
execução. O nome lógico permanece canônico; somente o nome físico pode
ser compactado para respeitar o limite do Excel no Windows, sempre com
correspondência no manifesto.
</td>

<td style="text-align:left;">

Exige MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = S; cada arquivo
preserva as três abas do modelo 21FEV25, inclui a coluna obrigatória uc
em Preenchimento e mantém campos comuns apenas na primeira linha de cada
bloco de 101 pontos. observacoes_gerais é omitido somente das planilhas
porque o importador não suporta a função XPath regex do formulário; o
valor original permanece na fonte e na auditoria. Pode conter dados
pessoais e referências a fotos.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_REMOVER_UUID_IMPORTACAO_SISMONITORA
</td>

<td style="text-align:left;">

padrão N; S ou N
</td>

<td style="text-align:left;">

Controla somente os valores das colunas uuid e amostragem/registro/uuid
no XLSX: S os remove para inclusão de registros novos; N preserva os
valores da fonte para eventual atualização por UUID.
</td>

<td style="text-align:left;">

A opção só é lida quando a planilha está ativa. N preserva o UUID para
eventual atualização, comportamento que depende de confirmação e
homologação no SISMONITORA; S o remove para inclusão de registros novos.
registros_validados.csv permanece intocado em ambos os modos.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_MANUAL_USUARIO_PDF
</td>

<td style="text-align:left;">

padrão N; S ou N; padrão N
</td>

<td style="text-align:left;">

Gera a versão PDF do manual somente ao término do fluxo principal. O
HTML continua disponível antes do painel quando o manual estiver ativo.
</td>

<td style="text-align:left;">

Mantenha N quando o PDF não for necessário. Com N, nenhum renderizador
de PDF é consultado e não existe custo adicional de execução. Uma falha
do PDF não bloqueia produtos de dados.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_RELATORIOS_ANALITICOS
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Gera relatórios analíticos sintético e detalhado por UC em
output/08_analises/, com tabelas editáveis e numeradas, esforço real,
continuidade, séries anuais por UA e painéis inferenciais auditáveis.
Seções, tabelas e figuras recebem números após a seleção do conteúdo,
por UC e por relatório; itens omitidos não reservam números. O arquivo
auditoria_numeracao registra os elementos incluídos e as tabelas não
selecionadas. No Word, tabelas largas podem apresentar identificação e
resultados em duas colunas, preservando os valores.
</td>

<td style="text-align:left;">

Exige exatamente uma UC na execução. Quando N, não executa cálculos,
leitura de produtos nem renderização do módulo. Quando S, reutiliza as
estatísticas já calculadas e acrescenta somente a agregação anual por UA
e a renderização dos relatórios.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_FORMATOS_RELATORIOS_ANALITICOS
</td>

<td style="text-align:left;">

rmd, md, html, docx, pdf
</td>

<td style="text-align:left;">

Define os formatos documentais. Rmd e Markdown preservam fontes
editáveis; DOCX é editável em processadores de texto; HTML e PDF são
produtos de leitura/publicação.
</td>

<td style="text-align:left;">

PDF requer pagedown e navegador Chrome/Chromium. Os produtos
estatísticos e de dados não são modificados.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_RELATORIOS_ANALITICOS_MAPA_SATELITE
</td>

<td style="text-align:left;">

padrão S; S ou N
</td>

<td style="text-align:left;">

Acrescenta ao relatório um mapa de continuidade sobre imagem orbital em
cor natural.
</td>

<td style="text-align:left;">

Quando N, nenhuma consulta remota, leitura orbital, criação de cache ou
renderização adicional é executada.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_FONTE_RELATORIOS_ANALITICOS_MAPA_SATELITE
</td>

<td style="text-align:left;">

SENTINEL2_PUBLICO ou GOOGLE_MAPS; padrão SENTINEL2_PUBLICO
</td>

<td style="text-align:left;">

Seleciona a fonte do fundo orbital. SENTINEL2_PUBLICO consulta imagens
Sentinel-2 L2A recentes, sem chave, conta ou cobrança; GOOGLE_MAPS
mantém a alternativa autenticada.
</td>

<td style="text-align:left;">

SENTINEL2_PUBLICO seleciona a aquisição mais recente com cobertura
adequada e avalia nuvens/sombras na extensão do mapa; depende de
internet. GOOGLE_MAPS exige MONITORA_GOOGLE_MAPS_API_KEY, API habilitada
e faturamento. Credenciais nunca são gravadas e a atribuição da fonte
deve ser preservada.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_PROJETO_QFIELD
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Gera projeto local de navegação QField a partir da projeção espacial
aceita, sem editar dados.
</td>

<td style="text-align:left;">

N não acessa dependências do módulo. S exige uma imagem offline ativa,
cobertura dos extremos e validação espacial coerente; não realiza
upload.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_IMPORTAR_CAMADAS_QFIELD
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Importa de qfield_input/ MBTiles e vetores KML, KMZ, GPKG ou
ZIP-shapefile, fora do input biológico.
</td>

<td style="text-align:left;">

Em execução com uma UC, coloque os arquivos diretamente na pasta, sem
subpasta ou nome obrigatório. MBTiles é classificado pelo zoom real:
zoom_max_real \>= 18 é detalhe. O detalhe é recortado internamente a um
círculo de 500 m por UA; a fonte permanece intacta. Vetores genéricos
são importados sem inferir significado; papéis PA/acesso exigem nome
padronizado ou camadas_qfield.csv.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_AUDITORIA_PRETOKENIZACAO_DETALHADA
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Preserva a auditoria detalhada célula a célula anterior à tokenização.
</td>

<td style="text-align:left;">

Mantenha S para rastreabilidade; desligar reduz evidência disponível.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_COMPRIMIR_AUDITORIAS_VOLUMOSAS
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Compacta auditorias volumosas sem reduzir sua cobertura.
</td>

<td style="text-align:left;">

Mantenha S para reduzir I/O sem perder conteúdo.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_DUPLICAR_AUDITORIAS_VOLUMOSAS_LOG
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Controla se auditorias volumosas também são duplicadas no log.
</td>

<td style="text-align:left;">

Mantenha N; duplicação aumenta espaço e tempo sem criar nova evidência.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_CAMINHO_NAVEGADOR_PDF
</td>

<td style="text-align:left;">

vazio para autodetecção; caminho/comando opcional
</td>

<td style="text-align:left;">

Indica Chrome, Chromium ou Edge somente para renderização de PDF.
</td>

<td style="text-align:left;">

Deixe vazio normalmente. Não confundir navegador com fonte do mapa
orbital.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_DISPENSAR_LINHAGEM_HISTORICA_INCOMPLETA
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Dispensa institucional excepcional do histórico ausente em checkpoint
legado.
</td>

<td style="text-align:left;">

Não usar sem autorização documentada; a opção não reconstrói linhagem.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_REPLAY_DIAGNOSTICO_NAO_ABORTAR
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Permite produzir diagnóstico de reaplicação mesmo quando há operação
inválida.
</td>

<td style="text-align:left;">

Uso exclusivamente investigativo; S não autoriza promover o produto.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Compara o estado final reaplicado com uma rodada considerada correta.
</td>

<td style="text-align:left;">

A rodada-oráculo não vira entrada e precisa ter identidade inequívoca.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_DIR_RUN_ORACULO
</td>

<td style="text-align:left;">

input/oraculo_replay
</td>

<td style="text-align:left;">

Aponta a pasta da rodada-oráculo usada somente para comparação.
</td>

<td style="text-align:left;">

Não misturar seus arquivos com o input operacional.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_REPLAY_ORACULO_ABORTAR_DIVERGENCIA
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Transforma qualquer divergência em bloqueio antes da exportação.
</td>

<td style="text-align:left;">

Use S em homologações/reaplicações que exigem equivalência estrita.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_IMPORTADOS
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Gera comparação diagnóstica dos cabeçalhos importados com o contrato
único.
</td>

<td style="text-align:left;">

Não renomeia, reordena nem corrige produtos.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_DIAGNOSTICO_PIPES_CONTRATO
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Gera diagnóstico das colunas físicas concorrentes separadas por pipe.
</td>

<td style="text-align:left;">

Não reconcilia valores nem substitui a validação normal.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_DIAGNOSTICO_CONTRATO_UNICO_REGISTROS_VALIDADOS
</td>

<td style="text-align:left;">

S ou N; padrão N
</td>

<td style="text-align:left;">

Enriquece a auditoria contratual final com severidade diagnóstica.
</td>

<td style="text-align:left;">

Não altera o critério bloqueante nem flexibiliza o XLSForm.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_RAIO_VALIDACAO_ESPACIAL_M
</td>

<td style="text-align:left;">

20 m
</td>

<td style="text-align:left;">

Limite metodológico da validação de posição.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_RAIO_ALERTA_ESPACIAL_M
</td>

<td style="text-align:left;">

10 m
</td>

<td style="text-align:left;">

Faixa mais rigorosa que gera alerta.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_MIN_COLETAS_CONSENSO_ESPACIAL
</td>

<td style="text-align:left;">

2 coletas
</td>

<td style="text-align:left;">

Mínimo para consenso temporal.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_VALIDACAO_ESPACIAL_LEAVE_ONE_OUT
</td>

<td style="text-align:left;">

S
</td>

<td style="text-align:left;">

Testa cada coleta contra consenso construído sem ela.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_USAR_ACURACIA_GPS_NA_TRIAGEM
</td>

<td style="text-align:left;">

S
</td>

<td style="text-align:left;">

Usa a acurácia declarada como contexto de triagem.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_ACURACIA_GPS_MAX_ALERTA_M
</td>

<td style="text-align:left;">

10 m
</td>

<td style="text-align:left;">

Limiar de alerta da acurácia informada.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_VALIDAR_COMPRIMENTO_TRANSECTO
</td>

<td style="text-align:left;">

S
</td>

<td style="text-align:left;">

Verifica o comprimento entre os extremos.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_COMPRIMENTO_TRANSECTO_ESPERADO_M
</td>

<td style="text-align:left;">

50 m
</td>

<td style="text-align:left;">

Comprimento esperado da transecção.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_TOLERANCIA_COMPRIMENTO_TRANSECTO_M
</td>

<td style="text-align:left;">

20 m
</td>

<td style="text-align:left;">

Tolerância ao redor do comprimento esperado.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_APLICAR_CORRECOES_ESPACIAIS
</td>

<td style="text-align:left;">

TRUE
</td>

<td style="text-align:left;">

Habilita a aplicação das operações espaciais confirmadas no painel.
</td>

<td style="text-align:left;">

Parâmetro metodológico: não alterar para fazer uma pendência
desaparecer; qualquer revisão exige decisão técnica, teste e registro de
governança.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_SALVAR_CACHE_PAINEL
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Salva cache recuperável do painel.
</td>

<td style="text-align:left;">

Não confundir cache com produto final.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_OPCAO_USAR_CACHE_PAINEL
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Permite reutilizar cache compatível.
</td>

<td style="text-align:left;">

O cache precisa pertencer à mesma base; prefira o fluxo incremental com
CSV+linhagem quando já houver checkpoint.
</td>

</tr>

<tr>

<td style="text-align:left;">

MONITORA_CACHE_PAINEL_HASH_INPUTS
</td>

<td style="text-align:left;">

S ou N; padrão S
</td>

<td style="text-align:left;">

Exige correspondência dos arquivos de entrada ao selecionar cache.
</td>

<td style="text-align:left;">

Mantenha S; a verificação por hash reduz o risco de abrir cache de outro
input.
</td>

</tr>

</tbody>

</table>

## Variáveis internas que não habilitam rotinas de usuário

Algumas constantes visíveis no início do arquivo existem para governança
ou desempenho e não devem ser ajustadas por uma pessoa não treinada:
versão do contrato semântico, colunas excepcionalmente ignoradas pelo
oráculo, granularidade de checkpoints, otimizações, backend/layout de
progresso, limites de lote espacial e persistência interna. Elas não são
atalhos para liberar dados. Em caso de necessidade metodológica ou de
performance, registre a demanda para revisão do código e homologação
multiplataforma; não mude o valor localmente para concluir uma rodada.

# Modos de operação

A escolha do modo define a origem dos dados, se o painel será aberto,
onde o script para e quais produtos finais serão gerados. Os modos
`painel_e_parar`, `abrir_painel_cache` e todos os `painel_incremental_*`
forçam a abertura do painel mesmo quando
`MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES = 'N'`.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

modo
</th>

<th style="text-align:left;">

finalidade
</th>

<th style="text-align:left;">

entrada_principal
</th>

<th style="text-align:left;">

painel
</th>

<th style="text-align:left;">

saida_esperada
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

completo
</td>

<td style="text-align:left;">

Executa o fluxo integral desde a entrada bruta até bases, relatórios,
estatísticas, gráficos e validações finais.
</td>

<td style="text-align:left;">

download bruto, ZIP, CSV, XLSX/XLS ou lote em input/
</td>

<td style="text-align:left;">

opcional por MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES
</td>

<td style="text-align:left;">

produtos finais completos
</td>

</tr>

<tr>

<td style="text-align:left;">

sem_png
</td>

<td style="text-align:left;">

Executa o fluxo integral, mas suprime exportação de PNGs para reduzir
tempo e volume de saída.
</td>

<td style="text-align:left;">

download bruto, ZIP, CSV, XLSX/XLS ou lote em input/
</td>

<td style="text-align:left;">

opcional
</td>

<td style="text-align:left;">

produtos finais sem PNG
</td>

</tr>

<tr>

<td style="text-align:left;">

estatisticas_sem_graficos
</td>

<td style="text-align:left;">

Executa leitura, correções e estatísticas tabulares/textuais, omitindo
objetos gráficos e PNGs.
</td>

<td style="text-align:left;">

download bruto, ZIP, CSV, XLSX/XLS ou lote em input/
</td>

<td style="text-align:left;">

opcional
</td>

<td style="text-align:left;">

bases e estatísticas sem gráficos
</td>

</tr>

<tr>

<td style="text-align:left;">

ate_registros_corrig
</td>

<td style="text-align:left;">

Executa até materializar registros_corrig.csv e encerra de forma
controlada, sem abrir painel automaticamente.
</td>

<td style="text-align:left;">

download bruto, ZIP, CSV, XLSX/XLS ou lote em input/
</td>

<td style="text-align:left;">

não forçado
</td>

<td style="text-align:left;">

checkpoint registros_corrig.csv
</td>

</tr>

<tr>

<td style="text-align:left;">

painel_e_parar
</td>

<td style="text-align:left;">

Executa pré-processamento, materializa manual antes do Shiny, abre
painel obrigatoriamente e para após registros_corrig.csv.
</td>

<td style="text-align:left;">

download bruto, ZIP, CSV, XLSX/XLS ou lote em input/
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

manual + painel + checkpoint registros_corrig.csv
</td>

</tr>

<tr>

<td style="text-align:left;">

abrir_painel_cache
</td>

<td style="text-align:left;">

Reabre painel a partir de cache pré-painel válido, aplica correções
novas e para após registros_corrig.csv.
</td>

<td style="text-align:left;">

cache_painel\_\*.rds gerado em execução anterior compatível
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

painel reaberto + checkpoint registros_corrig.csv
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_corrig_completo
</td>

<td style="text-align:left;">

Retoma de registros_corrig\*.csv em input/ e segue para produtos finais
completos sem reconstruir a entrada bruta.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

não abre automaticamente
</td>

<td style="text-align:left;">

produtos finais a partir de registros_corrig
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_corrig_sem_png
</td>

<td style="text-align:left;">

Retoma de registros_corrig\*.csv em input/ e segue sem PNGs.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

não abre automaticamente
</td>

<td style="text-align:left;">

produtos sem PNG a partir de registros_corrig
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_corrig_estatisticas_sem_graficos
</td>

<td style="text-align:left;">

Retoma de registros_corrig\*.csv em input/ e gera estatísticas sem
gráficos.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

não abre automaticamente
</td>

<td style="text-align:left;">

estatísticas sem gráficos a partir de registros_corrig
</td>

</tr>

<tr>

<td style="text-align:left;">

painel_incremental_registros_corrig
</td>

<td style="text-align:left;">

Abre painel a partir de registros_corrig\*.csv em input/ e para após
novo registros_corrig.csv.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

novo checkpoint corrigido
</td>

</tr>

<tr>

<td style="text-align:left;">

painel_incremental_completo
</td>

<td style="text-align:left;">

Abre painel a partir de registros_corrig\*.csv, aplica correções e segue
para fluxo completo.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

produtos finais pós-painel incremental
</td>

</tr>

<tr>

<td style="text-align:left;">

painel_incremental_sem_png
</td>

<td style="text-align:left;">

Abre painel a partir de registros_corrig\*.csv, aplica correções e segue
sem PNGs.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

produtos sem PNG pós-painel incremental
</td>

</tr>

<tr>

<td style="text-align:left;">

painel_incremental_estatisticas_sem_graficos
</td>

<td style="text-align:left;">

Abre painel a partir de registros_corrig\*.csv, aplica correções e segue
apenas para estatísticas tabulares/textuais.
</td>

<td style="text-align:left;">

exatamente um registros_corrig\*.csv em input/
</td>

<td style="text-align:left;">

forçado
</td>

<td style="text-align:left;">

estatísticas sem gráficos pós-painel incremental
</td>

</tr>

</tbody>

</table>

## Passo a passo detalhado dos modos

A tabela abaixo orienta a equipe sobre quando usar cada modo, quais
passos executar e quais produtos verificar ao final. O objetivo é
reduzir ambiguidade operacional durante a produção e durante transições
de versão do script.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

modo
</th>

<th style="text-align:left;">

quando_usar
</th>

<th style="text-align:left;">

passo_a_passo
</th>

<th style="text-align:left;">

conferir
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

completo
</td>

<td style="text-align:left;">

Rodada integral de tratamento, validação, painel opcional, estatísticas
e gráficos.
</td>

<td style="text-align:left;">

1)  Conferir input bruto. 2) Ajustar variáveis iniciais. 3) Rodar
    script. 4) Revisar painel se aberto. 5) Conferir
    registros_corrig/registros_validados, relatórios e gráficos.
    </td>

    <td style="text-align:left;">

    output/01_produtos_dados/, output/02_painel_correcoes/,
    output/07_validacao/, output/05_estatisticas/ e output/06_graficos/
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    sem_png
    </td>

    <td style="text-align:left;">

    Fluxo integral sem exportar os PNGs opcionais.
    </td>

    <td style="text-align:left;">

    1)  Conferir input bruto. 2) Ajustar variáveis. 3) Rodar. 4)
        Conferir produtos tabulares e relatórios. 5) Confirmar que PNGs
        não eram esperados.
        </td>

        <td style="text-align:left;">

        produtos tabulares e output/07_validacao/; PNGs não são
        esperados
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        estatisticas_sem_graficos
        </td>

        <td style="text-align:left;">

        Fluxo integral até estatísticas tabulares, sem gráficos.
        </td>

        <td style="text-align:left;">

        1)  Conferir input bruto. 2) Ajustar variáveis. 3) Rodar. 4)
            Conferir estatísticas e relatórios. 5) Confirmar que
            gráficos não eram esperados.
            </td>

            <td style="text-align:left;">

            estatísticas e output/07_validacao/; gráficos não são
            esperados
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            ate_registros_corrig
            </td>

            <td style="text-align:left;">

            Fluxo controlado até registros_corrig.csv, sem forçar o
            painel.
            </td>

            <td style="text-align:left;">

            1)  Conferir input. 2) Rodar. 3) Conferir o checkpoint
                registros_corrig.csv. 4) Copiar a linhagem
                correspondente antes da próxima rodada.
                </td>

                <td style="text-align:left;">

                output/01_produtos_dados/registros_corrig.csv e
                output/02_painel_correcoes/linhagem/
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                painel_e_parar
                </td>

                <td style="text-align:left;">

                Rodada focada em curadoria; abre painel, grava
                registros_corrig.csv e encerra de forma controlada.
                </td>

                <td style="text-align:left;">

                1)  Conferir input bruto e histórico de correções quando
                    houver. 2) Rodar. 3) Corrigir no painel. 4)
                    Salvar/fechar. 5) Conferir relatórios pós-painel e
                    registros_corrig.csv.
                    </td>

                    <td style="text-align:left;">

                    output/01_produtos_dados/registros_corrig.csv,
                    output/02_painel_correcoes/ocorrencias/pos_painel/ e
                    output/07_validacao/
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    abrir_painel_cache
                    </td>

                    <td style="text-align:left;">

                    Retomar uma sessão de painel a partir de cache
                    compatível com o mesmo input.
                    </td>

                    <td style="text-align:left;">

                    1)  Confirmar cache correto. 2) Abrir painel. 3)
                        Continuar curadoria. 4) Salvar. 5) Conferir
                        auditorias de sessão.
                        </td>

                        <td style="text-align:left;">

                        output/02_painel_correcoes/operacoes/ e
                        relatórios pós-painel
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        registros_corrig_completo
                        </td>

                        <td style="text-align:left;">

                        Prosseguir a partir de registros_corrig.csv já
                        materializado e gerar produtos finais.
                        </td>

                        <td style="text-align:left;">

                        1)  Colocar registros_corrig.csv validado em
                            input. 2) Rodar. 3) Conferir contrato e
                            registros_validados. 4) Conferir
                            estatísticas/relatórios.
                            </td>

                            <td style="text-align:left;">

                            output/01_produtos_dados/registros_validados.csv,
                            auditorias XLSForm e estatísticas
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            registros_corrig_sem_png
                            </td>

                            <td style="text-align:left;">

                            Prosseguir de registros_corrig.csv sem gerar
                            PNGs pesados.
                            </td>

                            <td style="text-align:left;">

                            1)  Usar registros_corrig.csv validado. 2)
                                Rodar sem PNG. 3) Conferir tabelas e
                                relatórios essenciais.
                                </td>

                                <td style="text-align:left;">

                                produtos tabulares e relatório
                                consolidado; PNGs não são esperados
                                </td>

                                </tr>

                                <tr>

                                <td style="text-align:left;">

                                registros_corrig_estatisticas_sem_graficos
                                </td>

                                <td style="text-align:left;">

                                Prosseguir de registros_corrig.csv
                                apenas para estatísticas/tabelas, sem
                                gráficos.
                                </td>

                                <td style="text-align:left;">

                                1)  Usar registros_corrig.csv
                                    validado. 2) Rodar estatísticas sem
                                    gráficos. 3) Conferir saídas
                                    tabulares.
                                    </td>

                                    <td style="text-align:left;">

                                    estatísticas e relatórios; gráficos
                                    não são esperados
                                    </td>

                                    </tr>

                                    <tr>

                                    <td style="text-align:left;">

                                    painel_incremental_registros_corrig
                                    </td>

                                    <td style="text-align:left;">

                                    Reabrir painel sobre
                                    registros_corrig.csv e parar no
                                    checkpoint corrigido.
                                    </td>

                                    <td style="text-align:left;">

                                    1)  Usar registros_corrig.csv. 2)
                                        Reabrir painel. 3) Aplicar
                                        ajustes incrementais. 4)
                                        Conferir novo
                                        registros_corrig.csv.
                                        </td>

                                        <td style="text-align:left;">

                                        novo registros_corrig.csv e
                                        trilhas semânticas da sessão
                                        </td>

                                        </tr>

                                        <tr>

                                        <td style="text-align:left;">

                                        painel_incremental_completo
                                        </td>

                                        <td style="text-align:left;">

                                        Reabrir painel sobre
                                        registros_corrig.csv e depois
                                        executar fluxo completo.
                                        </td>

                                        <td style="text-align:left;">

                                        1)  Usar
                                            registros_corrig.csv. 2)
                                            Reabrir painel. 3) Aplicar
                                            ajustes. 4) Seguir para
                                            produtos finais.
                                            </td>

                                            <td style="text-align:left;">

                                            registros_corrig/registros_validados,
                                            relatórios, estatísticas e
                                            gráficos
                                            </td>

                                            </tr>

                                            <tr>

                                            <td style="text-align:left;">

                                            painel_incremental_sem_png
                                            </td>

                                            <td style="text-align:left;">

                                            Reabrir painel sobre
                                            registros_corrig.csv e
                                            depois executar sem PNGs.
                                            </td>

                                            <td style="text-align:left;">

                                            1)  Usar
                                                registros_corrig.csv. 2)
                                                Reabrir painel. 3)
                                                Aplicar ajustes. 4)
                                                Gerar produtos finais
                                                sem PNG.
                                                </td>

                                                <td style="text-align:left;">

                                                produtos finais sem PNG
                                                e relatório consolidado
                                                </td>

                                                </tr>

                                                <tr>

                                                <td style="text-align:left;">

                                                painel_incremental_estatisticas_sem_graficos
                                                </td>

                                                <td style="text-align:left;">

                                                Reabrir painel sobre
                                                registros_corrig.csv e
                                                depois executar
                                                estatísticas sem
                                                gráficos.
                                                </td>

                                                <td style="text-align:left;">

                                                1)  Usar
                                                    registros_corrig.csv. 2)
                                                    Reabrir painel. 3)
                                                    Aplicar ajustes. 4)
                                                    Gerar
                                                    estatísticas/tabelas
                                                    sem gráficos.
                                                    </td>

                                                    <td style="text-align:left;">

                                                    estatísticas/tabelas
                                                    sem gráficos e
                                                    relatório
                                                    consolidado
                                                    </td>

                                                    </tr>

                                                    </tbody>

                                                    </table>

## Combinações recomendadas

- Use `completo` + `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES = 'S'` quando
  quiser executar tudo e revisar pendências no painel durante a rodada.
- Use `painel_e_parar` quando quiser dedicar a rodada à curadoria e só
  depois rodar estatísticas/produtos finais.
- Use `abrir_painel_cache` para continuar uma curadoria sem repetir
  etapas pesadas de pré-processamento, desde que o cache pertença ao
  mesmo input.
- Use `registros_corrig_completo`, `registros_corrig_sem_png` ou
  `registros_corrig_estatisticas_sem_graficos` quando já existir um
  `registros_corrig*.csv` validado em `input/` e não for necessário
  reconstruir a entrada bruta.
- Use `painel_incremental_*` quando precisar reabrir o painel sobre um
  `registros_corrig*.csv` e depois seguir para checkpoint ou produtos
  finais.
- Habilite `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = 'S'` apenas
  quando o objetivo incluir o produto contratual final e a base
  corrigida estiver sem bloqueios impeditivos.

# Reaplicação semântica e curadoria continuada

`correcoes_campos.csv` é o arquivo operacional de uma sessão do painel.
A trilha durável é `correcoes_semanticas_consolidada.csv`: ela preserva
decisões semânticas, escopo, alvos, ação, tokens, justificativa,
autoria, versão do contrato e identificadores estáveis dos eventos. O
contrato público atual é `correcoes_semanticas_v2` /
`replay_semantico_v2`; versões futuras devem migrar explicitamente
formatos anteriores antes de qualquer mutação.

Há dois fluxos válidos e mutuamente exclusivos. (1) REPLAY: parta de uma
cópia idêntica dos arquivos brutos/originais, leve somente
`correcoes_semanticas_consolidada.csv` para `input/linhagem/`, mantenha
`registros_corrig.csv` e os demais arquivos auxiliares de linhagem fora
do input e use `MONITORA_OPCAO_REAPLICAR_CORRECOES_ANTERIORES = 'S'`. Os
modos compatíveis são `completo`, `sem_png`,
`estatisticas_sem_graficos`, `ate_registros_corrig` e `painel_e_parar`.
(2) CONTINUIDADE: leve `registros_corrig.csv` e a pasta `linhagem`
inteira da rodadas anterior para `input/`, escolha um modo
`painel_incremental_*` e mantenha o replay em `N`, porque o checkpoint
já contém os efeitos materiais das decisões.

Nunca combine replay ligado com `registros_corrig.csv`,
`abrir_painel_cache` ou modo incremental. O script bloqueia essas
combinações antes de alterar dados. Replay sem histórico de correções,
histórico de correções vazio ou caminho explícito inexistente também
interrompe a execução. Mantenha
`MONITORA_OPCAO_REPLAY_DIAGNOSTICO_NAO_ABORTAR = 'N'` para uso normal;
`S` serve apenas para investigação e não autoriza promover os produtos.

Para validar uma transição contra uma rodadas-oráculo, copie a pasta
dessa rodadas para `input/oraculo_replay/`, use
`MONITORA_OPCAO_COMPARAR_REPLAY_COM_ORACULO = 'S'` e
`MONITORA_OPCAO_REPLAY_ORACULO_ABORTAR_DIVERGENCIA = 'S'`. A comparação
ocorre sobre `registros_corrig` final reconciliado, antes de sua
exportação. Oráculo ausente, identidade não única ou qualquer diferença
impedem a materialização; o oráculo nunca é usado como dado de entrada.

No console, confirme `Reaplicação semântica solicitado: SIM`,
`Replay concluído` e, quando houver oráculo,
`critérios de validação final do oráculo de replay: convergente_com_oraculo`.
Confira
`output/02_painel_correcoes/auditorias/auditoria_validacao_replay_v2_ultima_execucao.csv`,
`auditoria_verificação prévia_replay_v2_ultima_execucao.csv`,
`output/02_painel_correcoes/linhagem/aplicacoes_correcoes.csv`,
`resumo_linhagem.csv` e
`output/03_auditorias/replay_semantico/oraculo_replay_selo_convergencia_pos_replay_final_reconciliado.csv`.
Todas as operações devem estar aplicáveis ou já satisfeitas; todas as
aplicações, aplicadas ou já satisfeitas; o selo estrito deve registrar
`replay_equivalente_ao_oraculo = SIM`.

Na continuidade incremental, o histórico herdado não é recanonizado nem
regravado: seus bytes e hashes são preservados. Novas decisões são
anexadas, e `aplicacoes_correcoes.csv` acumula tanto as aplicações
históricas quanto a aplicação bem-sucedida da sessão atual. O manifesto
liga criptograficamente o `registros_corrig.csv` ao histórico de
correções. Para publicar `registros_validados.csv`, histórico ausente,
manifesto ausente ou hash legado ausente exigem a dispensa institucional
explícita; a dispensa não reconstrói o histórico.

`inventario_sessoes_linhagem.csv` distingue execuções/rodadas, sessões
que criaram decisões semânticas e execuções sem novas decisões; também
informa eventos herdados, reaplicados, atuais e acumulados. O relatório
consolidado apresenta a mesma cronologia e os controles de integridade.
A v2.9.7 preserva os metadados das sessões em
`metadados_sessoes_painel_consolidado.csv`, assinado pelo manifesto; na
primeira continuidade de uma cadeia legada, a recuperação histórica deve
ser feita uma única vez a partir dos arquivos auxiliares de linhagem da
cadeia canônica. Depois de consumida, a recuperação não é propagada: as
rodadas seguintes copiam `registros_corrig.csv` e a pasta
`output/02_painel_correcoes/linhagem/` completa para o novo `input/`.
Ausências verdadeiras continuam declaradas, nunca inferidas.

Copie sempre a pasta `output/02_painel_correcoes/linhagem/` junto com o
`registros_corrig.csv` correspondente. Não edite manualmente o histórico
de correções, o manifesto ou `aplicacoes_correcoes.csv`, nem misture
arquivos de rodadas diferentes.

# Mapa dos diretórios de saída

A organização abaixo separa produtos canônicos, evidências de auditoria,
relatórios e artefatos recuperáveis. Use `README_OUTPUT.txt` e
`indice_produtos.csv` como inventário da execução; não procure a versão
mais recente apenas pela data da pasta.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

diretorio
</th>

<th style="text-align:left;">

conteudo
</th>

<th style="text-align:left;">

acao_usuario
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

00_manifesto_execucao
</td>

<td style="text-align:left;">

Manifestos, hashes e inventários que identificam a execução e seus
produtos.
</td>

<td style="text-align:left;">

Preservar com a rodada e usar para conferir identidade/integridade.
</td>

</tr>

<tr>

<td style="text-align:left;">

01_produtos_dados
</td>

<td style="text-align:left;">

Bases canônicas: registros importados, corrigidos, validados e derivados
de integração.
</td>

<td style="text-align:left;">

Usar o produto correspondente à finalidade; não editar manualmente.
</td>

</tr>

<tr>

<td style="text-align:left;">

02_painel_correcoes
</td>

<td style="text-align:left;">

Ocorrências, operações da sessão, relatórios de apoio e linhagem
acumulada do painel.
</td>

<td style="text-align:left;">

Copiar a pasta linhagem inteira junto com registros_corrig.csv numa
continuidade.
</td>

</tr>

<tr>

<td style="text-align:left;">

03_auditorias
</td>

<td style="text-align:left;">

Evidências técnicas da importação, contrato, sanitizações, execução e
produtos finais.
</td>

<td style="text-align:left;">

Consultar diante de aviso, bloqueio ou dúvida sobre transformação.
</td>

</tr>

<tr>

<td style="text-align:left;">

04_validacao_espacial
</td>

<td style="text-align:left;">

Coletas, consensos, alertas, pendências, planos e mapas da validação
espacial.
</td>

<td style="text-align:left;">

Resolver pendências no painel com evidência; não alterar CSVs
diretamente.
</td>

</tr>

<tr>

<td style="text-align:left;">

05_estatisticas
</td>

<td style="text-align:left;">

Tabelas estatísticas e objetos editáveis utilizados nos relatórios e
gráficos.
</td>

<td style="text-align:left;">

Usar para auditoria e análises complementares, mantendo vínculo com a
rodada.
</td>

</tr>

<tr>

<td style="text-align:left;">

06_graficos
</td>

<td style="text-align:left;">

Figuras e respectivos índices/auditorias quando a geração gráfica está
habilitada.
</td>

<td style="text-align:left;">

Conferir legibilidade, escalas e correspondência com as tabelas.
</td>

</tr>

<tr>

<td style="text-align:left;">

07_validacao
</td>

<td style="text-align:left;">

Relatório de validação e dados de apoio sobre toda a cadeia de
transformações.
</td>

<td style="text-align:left;">

Entregar à gestão somente após conferir quantitativos herdados/atuais e
impedimentos.
</td>

</tr>

<tr>

<td style="text-align:left;">

08_analises
</td>

<td style="text-align:left;">

Relatórios analíticos por UC, figuras e CSVs que sustentam cada
resultado.
</td>

<td style="text-align:left;">

Ler limitações, população, pares, calendário e fonte de cada indicador.
</td>

</tr>

<tr>

<td style="text-align:left;">

09_qfield
</td>

<td style="text-align:left;">

Projeto de navegação QField, imagens, camadas, intercâmbio e auditorias
próprias.
</td>

<td style="text-align:left;">

Importar o ZIP em pasta nova e testar online/offline antes do campo.
</td>

</tr>

<tr>

<td style="text-align:left;">

90_cache
</td>

<td style="text-align:left;">

Caches recuperáveis; não são produtos finais nem devem ser escolhidos
sem verificar a identidade.
</td>

<td style="text-align:left;">

Preservar para recuperação; descartar somente pelo fluxo controlado.
</td>

</tr>

<tr>

<td style="text-align:left;">

99_legacy_compat
</td>

<td style="text-align:left;">

Cópias de compatibilidade. Não substituir os produtos canônicos pelas
versões legadas.
</td>

<td style="text-align:left;">

Usar apenas quando uma integração antiga exigir explicitamente.
</td>

</tr>

</tbody>

</table>

# Produtos de dados

Os produtos abaixo representam estágios diferentes da mesma cadeia de
processamento. Eles não devem ser confundidos: cada um tem escopo,
pré-requisitos e finalidade próprios.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

produto
</th>

<th style="text-align:left;">

como_e_criado
</th>

<th style="text-align:left;">

escopo
</th>

<th style="text-align:left;">

pre_requisitos
</th>

<th style="text-align:left;">

finalidade
</th>

<th style="text-align:left;">

subsidia
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

registros_importados_bruto.csv
</td>

<td style="text-align:left;">

Criado como snapshot técnico da montagem dos arquivos lidos, preservando
leitura bruta/operacional antes das sanitizações principais.
</td>

<td style="text-align:left;">

Entrada bruta da execução e montagem técnica.
</td>

<td style="text-align:left;">

Input válido e modo que demande snapshot bruto, especialmente painel ou
auditoria de importação.
</td>

<td style="text-align:left;">

Auditar importação, diagnosticar problemas de ZIP, codificação,
cabeçalhos e montagem.
</td>

<td style="text-align:left;">

Depuração do input e descrição dos arquivos de entrada.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_importados.csv
</td>

<td style="text-align:left;">

Criado após normalização inicial de cabeçalhos, aliases e saneamento
preliminar da entrada.
</td>

<td style="text-align:left;">

Entrada saneada pré-correção final.
</td>

<td style="text-align:left;">

Leitura inicial concluída e mapeamento mínimo de atributos.
</td>

<td style="text-align:left;">

Comparar a entrada saneada com registros_corrig e apoiar o relatório
consolidado.
</td>

<td style="text-align:left;">

Painel, comparação pré/pós e rastreabilidade documental.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_importados_operacional_pre_painel.csv
</td>

<td style="text-align:left;">

Criado após tokenização operacional e antes do painel, com identidade
própria e sem substituir registros_importados.csv.
</td>

<td style="text-align:left;">

Camada operacional pós-tokenização/pré-painel.
</td>

<td style="text-align:left;">

registros_importados.csv materializado e tokenização operacional
concluída antes do painel.
</td>

<td style="text-align:left;">

Auditar a base operacional pré-painel e permitir comparação sem
semântica híbrida.
</td>

<td style="text-align:left;">

Diagnósticos pré-painel e comparação com registros_importados.csv.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_corrig.csv
</td>

<td style="text-align:left;">

Criado após harmonização, deduplicação, sanitizações automáticas,
aplicação das receitas do painel e validações de consistência.
</td>

<td style="text-align:left;">

Base corrigida central e auditável.
</td>

<td style="text-align:left;">

Schema operacional resolvido, input processável e ausência de falhas
críticas impeditivas.
</td>

<td style="text-align:left;">

Subsidiar estatísticas, relatórios específicos, painel incremental,
validação espacial e registros_validados.
</td>

<td style="text-align:left;">

Toda a etapa pós-correção: relatórios, estatísticas, mapas, KML,
auditorias e contrato final.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_validados.csv
</td>

<td style="text-align:left;">

Criado opcionalmente pela projeção de registros_corrig.csv no contrato
do XLSForm/template de destino.
</td>

<td style="text-align:left;">

Produto contratual final, quando habilitado.
</td>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = S, contrato resolvido e
registros_corrig sem bloqueios impeditivos.
</td>

<td style="text-align:left;">

Disponibilizar uma base compatível com estrutura, ordem, domínios e
formatos esperados pelo contrato de destino.
</td>

<td style="text-align:left;">

Auditoria final, integração/devolutiva e conferência formal de
schema/formato.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_corrig_stat.csv
</td>

<td style="text-align:left;">

Criado a partir de registros_corrig.csv, com sumarizações e contagens
por COLETA/UC/UA/ano usadas diretamente pelas estatísticas e pelos
gráficos PNG.
</td>

<td style="text-align:left;">

Base estatística final, derivada de registros_corrig.csv.
</td>

<td style="text-align:left;">

registros_corrig.csv materializado sem pendências impeditivas.
</td>

<td style="text-align:left;">

Subsidiar diretamente as estatísticas tabulares/textuais e os gráficos
PNG da execução.
</td>

<td style="text-align:left;">

Estatísticas, gráficos e KMLs de UAs.
</td>

</tr>

<tr>

<td style="text-align:left;">

registros_validados_importacao_sismonitora\*.xlsx
</td>

<td style="text-align:left;">

Criado opcionalmente por projeção estrita do registros_validados.csv
materializado e validado na mesma execução, com um XLSX por contexto
UC + ciclo + campanha, preservando o modelo oficial 21FEV25,
acrescentando a coluna obrigatória uc na aba Preenchimento, aplicando a
política configurada de remoção ou preservação dos UUIDs e omitindo
observacoes_gerais, cujo XPath regex não é suportado pelo importador.
</td>

<td style="text-align:left;">

Produto derivado de integração, sem alterar registros_validados.csv.
</td>

<td style="text-align:left;">

MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA = S;
MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = S; chaves UC, ciclo e
campanha preenchidas; coletas com exatamente 101 pontos.
</td>

<td style="text-align:left;">

Homologação e importação de registros novos ou, após confirmação
funcional do SISMONITORA, eventual atualização por UUID.
</td>

<td style="text-align:left;">

Inclusão de registros provenientes de formulários em papel, eventual
atualização controlada e auditoria da carga.
</td>

</tr>

<tr>

<td style="text-align:left;">

09_qfield/<uc>/projeto\_\*/
</td>

<td style="text-align:left;">

Exportação opcional da projeção espacial aceita; cópias dos vetores
adicionais; Sentinel regional; e MBTiles fornecido, classificado pelo
zoom real e recortado internamente quando detalhado.
</td>

<td style="text-align:left;">

Navegação offline, produto espacial derivado; nunca fonte de dados
biológicos ou prova de validação.
</td>

<td style="text-align:left;">

Opção QField S, uma UC para qfield_input plano, geometria válida,
validação espacial sem pendência e cobertura de todos os extremos por
imagem ativa.
</td>

<td style="text-align:left;">

Localizar extremos observados das transecções; apoiar deslocamento com
camadas adicionais.
</td>

<td style="text-align:left;">

QGS, GPKG, MBTiles, KML/KMZ, recorte_imagens_500m.gpkg, LEIA_ME,
auditorias e ZIP; testar no QField online e em modo avião antes do
campo.
</td>

</tr>

</tbody>

</table>

## Relação entre os produtos

`registros_importados_bruto.csv` documenta a leitura/montagem da
entrada. `registros_importados.csv` documenta a entrada já saneada.
`registros_importados_operacional_pre_painel.csv` documenta a camada
operacional pós-tokenização/pré-painel e não substitui
`registros_importados.csv`, `registros_corrig.csv` nem
`registros_validados.csv`. `registros_corrig.csv` é a base operacional
corrigida e auditável. `registros_validados.csv`, quando habilitado, é a
projeção contratual final para integração/devolutiva.

## Planilha para o SISMONITORA

`registros_validados_importacao_sismonitora*.xlsx` só pode ser criado
quando `registros_validados.csv` foi gerado e aprovado na mesma
execução. O script produz um arquivo por contexto UC + ciclo + campanha,
preserva o modelo 21FEV25 e representa atributos superiores apenas na
primeira linha do bloco quando o template assim exige. A opção de UUID
afeta somente o XLSX: use remoção para inclusão de registros novos;
preserve UUID apenas após homologação da atualização pela equipe do
SISMONITORA.

# Painel de correções assistidas

O painel permite corrigir a base sem edição manual de CSV. As operações
devem registrar responsável, escopo, atributo, valor novo e
justificativa. Ao salvar, o script aplica as operações, recalcula campos
superiores quando necessário, atualiza diagnósticos e materializa os
produtos pós-painel.

A ordem das abas é: **Correções de registros**, **Equipe da COLETA**,
**Validação espacial** e **Justificar pendências**. Essa ordem acompanha
o fluxo operacional recomendado.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

controle
</th>

<th style="text-align:left;">

funcao
</th>

<th style="text-align:left;">

quando_usar
</th>

<th style="text-align:left;">

auditoria_efeito
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Responsável
</td>

<td style="text-align:left;">

Identifica a pessoa responsável pela sessão de curadoria.
</td>

<td style="text-align:left;">

Sempre antes de salvar operações.
</td>

<td style="text-align:left;">

Valor resgatado no relatório consolidado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Filtros UC/EA/UA/Ciclo/Campanha
</td>

<td style="text-align:left;">

Restringem o universo de linhas analisadas e editadas.
</td>

<td style="text-align:left;">

Antes de qualquer operação que possa afetar mais de uma linha.
</td>

<td style="text-align:left;">

Escopo fica registrado nas operações e reduz risco de alteração
indevida.
</td>

</tr>

<tr>

<td style="text-align:left;">

COLETA
</td>

<td style="text-align:left;">

Define foco de diagnóstico por coleta, reduzindo custo e risco de
escopo.
</td>

<td style="text-align:left;">

Quando a curadoria for por coleta ou quando a pré-visualização global
estiver pesada.
</td>

<td style="text-align:left;">

Opera sobre subconjunto auditável.
</td>

</tr>

<tr>

<td style="text-align:left;">

Atributo
</td>

<td style="text-align:left;">

Define qual campo será corrigido.
</td>

<td style="text-align:left;">

Em correções simples ou assistidas.
</td>

<td style="text-align:left;">

Atributo/documento aparece nos relatórios de modificação.
</td>

</tr>

<tr>

<td style="text-align:left;">

Valor novo
</td>

<td style="text-align:left;">

Define o valor a ser aplicado no campo selecionado.
</td>

<td style="text-align:left;">

Quando houver valor correto definido tecnicamente.
</td>

<td style="text-align:left;">

Valor anterior e novo compõem a trilha de auditoria quando disponíveis.
</td>

</tr>

<tr>

<td style="text-align:left;">

Justificativa
</td>

<td style="text-align:left;">

Registra motivação técnica da correção.
</td>

<td style="text-align:left;">

Sempre que uma operação alterar dados.
</td>

<td style="text-align:left;">

Justificativa acompanha CSVs e relatório consolidado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Aplicar correção
</td>

<td style="text-align:left;">

Aplica a correção à linha ou escopo selecionado conforme o estado do
painel.
</td>

<td style="text-align:left;">

Correções pontuais.
</td>

<td style="text-align:left;">

Gera receita/registro de correção.
</td>

</tr>

<tr>

<td style="text-align:left;">

Aplicar em lote
</td>

<td style="text-align:left;">

Aplica a mesma regra a múltiplas linhas selecionadas ou filtradas.
</td>

<td style="text-align:left;">

Correções repetitivas com mesmo critério e escopo seguro.
</td>

<td style="text-align:left;">

Gera múltiplas operações rastreáveis.
</td>

</tr>

<tr>

<td style="text-align:left;">

Mover exótica para nativa
</td>

<td style="text-align:left;">

Move marcações e campos dependentes de exótica para nativa quando a
triagem indicar erro de classificação.
</td>

<td style="text-align:left;">

Exótica marcada por engano como nativa.
</td>

<td style="text-align:left;">

Atualiza superiores/inferiores relacionados conforme regras do script.
</td>

</tr>

<tr>

<td style="text-align:left;">

Substituir desconhecida
</td>

<td style="text-align:left;">

Reclassifica forma de vida desconhecida ou registra decisão de
tratamento.
</td>

<td style="text-align:left;">

Pendências de desconhecida em Encostam ou campos inferiores.
</td>

<td style="text-align:left;">

Atualiza pendências e relatórios pré/pós-painel.
</td>

</tr>

<tr>

<td style="text-align:left;">

Limpar outras formas
</td>

<td style="text-align:left;">

Remove resíduos indevidos de outras formas de vida e atualiza
diagnósticos.
</td>

<td style="text-align:left;">

Pendências de outra forma de vida sem compatibilidade com contrato.
</td>

<td style="text-align:left;">

Reduz pendências contratuais e registra operação.
</td>

</tr>

<tr>

<td style="text-align:left;">

Operação espacial
</td>

<td style="text-align:left;">

Corrige, copia, inverte ou valida coordenadas conforme alertas
espaciais.
</td>

<td style="text-align:left;">

Alertas de coordenada, duplicidade espacial ou inconsistência de coleta.
</td>

<td style="text-align:left;">

Alimenta output/validacao_espacial e mapa quando possível.
</td>

</tr>

<tr>

<td style="text-align:left;">

Atualizar diagnóstico
</td>

<td style="text-align:left;">

Recalcula pendências, contadores e pré-visualizações após mudanças.
</td>

<td style="text-align:left;">

Após filtros, mudanças manuais ou operações em lote.
</td>

<td style="text-align:left;">

Não deve alterar dados sozinho; atualiza visualização/diagnóstico.
</td>

</tr>

<tr>

<td style="text-align:left;">

Salvar e fechar
</td>

<td style="text-align:left;">

Encerra a sessão, devolve controle ao script e aplica/grava as
correções.
</td>

<td style="text-align:left;">

Encerramento normal da curadoria.
</td>

<td style="text-align:left;">

Materializa registros_corrig.csv e auditorias.
</td>

</tr>

<tr>

<td style="text-align:left;">

Fechar sem salvar
</td>

<td style="text-align:left;">

Sai do painel sem aplicar operações pendentes da sessão.
</td>

<td style="text-align:left;">

Sessão exploratória ou erro de seleção.
</td>

<td style="text-align:left;">

Não deve materializar operações novas não salvas.
</td>

</tr>

<tr>

<td style="text-align:left;">

Equipe da COLETA
</td>

<td style="text-align:left;">

Inclui, edita ou exclui um integrante específico do repeat de coletores,
sem replicar a alteração nas 101 linhas.
</td>

<td style="text-align:left;">

Quando a composição da equipe estiver incorreta; selecione o integrante,
não um ponto amostral.
</td>

<td style="text-align:left;">

Gera operações atômicas por linha do repeat e preserva os demais
integrantes.
</td>

</tr>

<tr>

<td style="text-align:left;">

CPF do coletor
</td>

<td style="text-align:left;">

Mantém a associação posicional com o nome quando o CPF é conhecido e
válido.
</td>

<td style="text-align:left;">

Somente quando houver CPF confiável. CPF é opcional; nome é obrigatório
para cada integrante.
</td>

<td style="text-align:left;">

CPF inválido, parcial ou ambiguamente associado é removido sem
inferência e documentado.
</td>

</tr>

<tr>

<td style="text-align:left;">

Impactos de manejo e uso
</td>

<td style="text-align:left;">

Edita a resposta superior e os tokens da lista condicional de tipos de
impacto.
</td>

<td style="text-align:left;">

Use Sim/Não no campo pai e adicionar/remover/substituir token em Quais?.
</td>

<td style="text-align:left;">

Registra operações de tokens e limpa campos filhos somente quando o pai
passa para Não.
</td>

</tr>

<tr>

<td style="text-align:left;">

Justificar pendências
</td>

<td style="text-align:left;">

Registra explicação auditável para uma pendência que permanece sem
alterar, ocultar ou liberar a ocorrência.
</td>

<td style="text-align:left;">

Quando a pendência for legítima, não puder ser resolvida nesta rodada ou
depender de evidência externa.
</td>

<td style="text-align:left;">

Alimenta justificativas_pendencias_consolidada.csv e
pendencias_justificadas.csv.
</td>

</tr>

<tr>

<td style="text-align:left;">

Selecionar pendências filtradas
</td>

<td style="text-align:left;">

Seleciona de uma só vez as ocorrências visíveis após os filtros da
tabela.
</td>

<td style="text-align:left;">

Quando várias ocorrências filtradas tiverem a mesma justificativa.
</td>

<td style="text-align:left;">

Não altera dados; apenas define a seleção corrente.
</td>

</tr>

<tr>

<td style="text-align:left;">

Confirmar justificativa em lote
</td>

<td style="text-align:left;">

Autoriza conscientemente a mesma classificação e justificativa para
todas as pendências selecionadas.
</td>

<td style="text-align:left;">

Obrigatório para lotes com mais de uma pendência; revise a abrangência
antes de confirmar.
</td>

<td style="text-align:left;">

Cada ocorrência recebe evento próprio; o lote inteiro só entra na sessão
após validação transacional completa.
</td>

</tr>

<tr>

<td style="text-align:left;">

Excluir justificativas da sessão
</td>

<td style="text-align:left;">

Remove atomicamente uma, várias, todas as filtradas ou todas as
justificativas ainda não persistidas.
</td>

<td style="text-align:left;">

Quando uma justificativa ainda não salva foi adicionada à ocorrência ou
ao lote errado.
</td>

<td style="text-align:left;">

A exclusão só substitui o estado da sessão após reconstrução e validação
integral dos lotes remanescentes.
</td>

</tr>

<tr>

<td style="text-align:left;">

Mudança de formação vegetacional
</td>

<td style="text-align:left;">

Exibe a ocorrência diagnóstica não impeditiva quando uma UA apresenta
formações diferentes entre anos.
</td>

<td style="text-align:left;">

Revisar possível erro de classificação ou mudança ecológica legítima de
longo prazo/impacto severo.
</td>

<td style="text-align:left;">

Permanece no relatório específico e pode receber justificativa
individual ou em lote.
</td>

</tr>

<tr>

<td style="text-align:left;">

Limpar filtros espaciais
</td>

<td style="text-align:left;">

Reinicia todas as escolhas, coordenadas e COLETAS de origem/destino do
módulo espacial.
</td>

<td style="text-align:left;">

Antes de iniciar outra correção espacial ou se restarem artefatos de uma
seleção anterior.
</td>

<td style="text-align:left;">

Não altera dados nem receitas já adicionadas; limpa somente o estado de
filtros espaciais.
</td>

</tr>

</tbody>

</table>

## Equipe da COLETA

Coletores formam um repeat: cada integrante ocupa uma linha, com o CPF
correspondente na mesma posição. A edição é feita no controle **Equipe
da COLETA**, e não pela seleção de ponto amostral nem pela edição
genérica de atributo superior. Nome é obrigatório para o integrante; CPF
é opcional. Para excluir uma pessoa, selecione somente a linha dessa
pessoa. O script não deve replicar um nome nas 101 linhas nem deslocar o
CPF de outro integrante.

## Impactos de manejo e uso

`impact_manejo_uso` é a pergunta superior Sim/Não.
`tipos_impacto_manejo_uso` é a lista condicional **Quais?** e deve ser
editada por operações de tokens: adicionar, remover ou substituir. Os
seletores mostram `label — name`, mas os arquivos armazenam os names do
contrato único. Ao mudar o pai para Não, os tipos e descritores filhos
tornam-se irrelevantes e são limpos de forma auditável; ao mudar para
Sim, o usuário escolhe quantos tokens forem necessários.

## Justificar pendências

A aba **Justificar pendências** documenta ocorrências que permanecem
após a revisão por motivo legítimo. A justificativa não corrige, não
oculta e não libera uma pendência impeditiva. Selecione um ou mais
rótulos para que a tabela exiba somente as ocorrências correspondentes e
use **Selecionar todas as pendências filtradas**; a seleção abrange
todas as páginas e permanece vinculada a `ocorrencia_id`. Também é
possível adicionar o conjunto filtrado à seleção atual. Revise os
totais, marque a confirmação e aplique a classificação e a justificativa
em lote. A inclusão é atômica: todas as ocorrências e os metadados do
lote são validados antes de uma única atualização da sessão; qualquer
falha rejeita o lote completo. Em **Justificativas adicionadas nesta
sessão**, filtre e selecione uma, várias, todas as filtradas ou todas e
use **Excluir justificativas selecionadas**. A exclusão também é atômica
e reconstrói lotes parcialmente mantidos. Eventos já persistidos nunca
são apagados; eventual revogação deve ser um novo evento auditável. Se
uma falha recuperável ocorrer ao salvar, o painel permanece aberto, os
arquivos anteriores são restaurados e um checkpoint recuperável integral
preserva as filas de correções de campos, operações espaciais,
justificativas e a auditoria exata da falha em
`output/02_painel_correcoes/operacoes/cache/` após a organização do
output. Na reabertura sobre a mesma base, esse checkpoint é restaurado
automaticamente e só é removido após salvamento concluído ou descarte
explícito. Use texto específico, verificável e com pelo menos 20
caracteres.

## Validação espacial

A aba de Validação espacial trabalha sempre no escopo integral da
COLETA. Os filtros identificam origem e destino; quando o conjunto de
filtros define uma única COLETA destino, ela é preenchida
automaticamente. A prévia mostra somente as coordenadas realmente
afetadas pela operação. **Limpar filtros** reinicia COLETAS, listas,
coordenadas e seleções do módulo. Confira os CSVs e mapas em
`output/04_validacao_espacial/` antes de salvar.

## Situação dos dados e diagnósticos

O status apresentado ao usuário é **não validado**, **em validação** ou
**validado**. Ocorrências impeditivas precisam ser corrigidas antes de
`registros_validados.csv`. Ocorrências de revisão podem permanecer,
desde que avaliadas e justificadas. A mudança de formação vegetacional
entre anos na mesma UA é uma ocorrência diagnóstica não impeditiva: pode
decorrer de classificação inconsistente ou ser compatível com mudança
ecológica, como adensamento lenhoso ou supressão, mas o diagnóstico
isolado não demonstra causa e deve orientar verificação de campo e
análise temporal. A vegetação seca ou morta também é registrada como
revisão não impeditiva, por linha e forma de vida. O apoio à triagem
inclui `relatorio_seca_morta_*`, sínteses por ano, por UA e ano e por
forma de vida, além de trajetórias herbáceas e lenhosas entre campanhas
amostradas; os produtos editáveis identificam as COLETAS amostradas e as
COLETAS com ocorrência. `criterios_atendidos` registra todas as razões
da triagem e `criterio_principal` preserva apenas a precedência
operacional. `classificacao_triagem` distingue suspeita de falso
positivo, ocorrência biologicamente plausível a revisar, falta de
contexto, padrão persistente e revisão rotineira. Nenhuma classe
confirma erro ou causa ecológica. Percentuais usam todos os pontos da
COLETA ou da UA/ano como denominador. Registros estruturados e texto
livre de fogo, impacto e manejo são exibidos somente como contexto
associado: fenologia, seca, fogo, herbivoria e outros processos
permanecem hipóteses, nunca causas atribuídas pelo diagnóstico.

# Sanitizações automáticas

As sanitizações automáticas ocorrem antes do painel e são repetidas como
critério idempotente antes dos produtos finais. Elas não substituem
decisões ecológicas. Para coletores, o script reconhece somente formatos
legados comprovados, transforma a equipe em repeat esparso e preserva
CPF apenas quando um CPF válido está inequivocamente associado a um
único nome. CPF parcial, inválido ou único para vários nomes é removido
sem tentativa de adivinhação. A auditoria fica em
`output/03_auditorias/cadastro/auditoria_sanitizacao_coletores.csv`; ela
registra contagens e motivos, sem expor nomes ou CPFs.

O reparo histórico do token órfão `outros` em exótica é deliberadamente
estreito. Ele só migra o texto para o ramo de outra espécie
correspondente quando existe exatamente uma forma exótica válida, o
destino contratual é único e não há valor conflitante; o texto informado
é preservado. Qualquer ambiguidade permanece bloqueada para decisão
humana. Isso não é conversão de **outra forma de vida** e não adiciona
alias ao XLSForm atual. O contrato único continua inalterado.

# Relatórios e auditorias

## Relatório de validação

O relatório de validação descreve a cadeia desde o input original até o
produto final. O resumo separa arquivos da sessão atual e herdados,
número de sessões e modificações do usuário, automáticas e técnicas.
**Tratamentos herdados e atuais** precisam aparecer com sua origem:
correção do usuário, transformação automática, auditoria, recuperação ou
conciliação não são somadas como se fossem o mesmo tipo de
inconsistência. As seções detalhadas apresentam histórico por ano,
equipe da COLETA, exclusões e validação espacial quando existe
evidência. Zero significa zero comprovado; ausência ou parcialidade
documental deve ser apresentada como não quantificável/parcial, nunca
convertida em zero. Antes de entregar, confira se os totais do resumo
são compatíveis com as tabelas e se a linhagem informa cada sessão.

## Como ler contagens

**Nº de UAs** é o número de unidades amostrais distintas que
contribuíram para a célula. **Nº de registros** é o número de
toques/linhas registrados para a categoria e pode superar o número de
pontos com presença quando um ponto contém mais de uma ocorrência
relevante. **Cobertura** usa os pontos amostrados como denominador;
**proporção relativa** reparte a cobertura entre as categorias incluídas
no respectivo universo. Categorias com zero registros no recorte exibido
são omitidas das tabelas do documento para evitar sugerir ocorrência,
mas permanecem nos CSVs completos e na matriz analítica quando
necessárias aos cálculos.

## Relatórios analíticos

Os relatórios analíticos são opcionais, exigem uma UC e não executam
quando a opção está em N. Eles apresentam categorias gerais, contraste
herbácea × lenhosa, formas nativas/exóticas, vegetação seca ou morta,
material botânico e evidências temporais. A série principal usa todas as
UAs observadas em cada ano; cada contraste temporal utiliza as UAs
comuns ao par de anos. Painéis por ano de entrada são sensibilidades da
composição do esforço, não substitutos da evidência principal.
`pares insuficientes` significa que o desenho pareado não alcançou o
mínimo exigido; não significa estabilidade, ausência da categoria ou
inexistência de mudança.

### Época das campanhas e linha de base

A seção **Época das campanhas e linha de base** mostra o calendário
observado, avalia o suporte para associações da época com os indicadores
e quantifica quanto a escolha da referência altera o contraste do
ano-alvo. O recorte exploratório usa os meses observados e não substitui
a janela oficial do projeto de amostragem da UC. A análise não prova
causalidade sazonal nem valida campanha fora de época. A recomendação
após a figura informa se há suporte para um corte de linha de base; sem
suporte suficiente, anos antigos permanecem como histórico e as
comparações são condicionais. Para avaliar conformidade, consulte o
projeto da UC e considere especialmente seca/morta, cobertura
herbácea/lenhosa, solo/rocha, material botânico e composição, sempre nas
mesmas UAs quando possível.

### Mudança do instrumento de material botânico

A leitura do material botânico antes de 2025 exige cautela: todo o
material era registrado como **serrapilheira**. A partir do XLSForm
2025, o atributo passou a separar serrapilheira, fragmentos botânicos e
material inundável. Assim, o total de material pode apoiar uma série
mais longa quando a definição agregada for compatível, mas sua
composição por tipo só é diretamente observada a partir de 2025. O
relatório deve explicitar essa mudança e não interpretar ausência de
fragmentos/material inundável antes de 2025 como zero ecológico.

Os painéis inferenciais usam UAs pareadas, efeito com IC95% bootstrap e
q ajustado por FDR-BH. Aumento, redução, estabilidade/equivalência,
inconclusão e insuficiência amostral são resultados diferentes; ausência
de significância não é estabilidade. Hipóteses e recomendações só devem
acompanhar evidência de mudança pertinente: estabilidade não é motivo,
por si só, para propor causa ou manejo. O histórico cartográfico de fogo
complementa os relatos de manejo e uso, com fonte e período explícitos;
nenhum deles, isoladamente, prova causalidade. Preserve os CSVs
editáveis, `auditoria_integracao_estatistica_graficos_relatorio.csv` e
`auditoria_robustez_inferencial_relatorio.csv` junto com o documento.

## Preparar uma atualização completa da UC

1.  Crie uma pasta local curta e exclusiva, por exemplo
    `C:/Monitora/APAI_rodada_nova`. Coloque nela apenas o arquivo `.R`
    da versão atual. Não execute simultaneamente duas instâncias na
    mesma pasta.
2.  Na rodada anterior concluída, localize
    `output/01_produtos_dados/registros_corrig.csv` e a linhagem
    correspondente. Copie a base para `input/` e preserve o histórico em
    `input/linhagem/`, conforme o roteiro de continuidade deste manual.
    Não use o arquivo estatístico como entrada nem misture bases de
    rodadas diferentes.
3.  Para somente atualizar produtos de uma base já conferida, escolha
    `MONITORA_MODO_EXECUCAO = "registros_corrig_completo"` e abertura do
    painel em `"N"`. Para revisar registros, use o percurso com painel
    descrito anteriormente. A atualização não autoriza novas correções
    ecológicas.
4.  Ative com `"S"` os produtos desejados: registros validados,
    planilhas SISMONITORA, relatório de validação, relatórios analíticos
    e QField. Para QField com imagens/camadas externas, ative também
    `MONITORA_OPCAO_IMPORTAR_CAMADAS_QFIELD` e coloque os arquivos em
    `qfield_input/`. Para gerar o PDF deste manual, ative a opção
    específica do PDF.
5.  Mantenha as análises de fogo e clima em `"S"` para incluí-las. A
    primeira consulta exige internet e pode demorar. O próprio `.R`
    obtém as dependências e os dados públicos necessários, conserva os
    arquivos de apoio e produz as análises. Não é necessário executar
    módulos separados.
6.  Abra o `.R` no RStudio, confira o bloco operacional e clique em
    **Source**. Aguarde a finalização registrada no console. Confira
    `indice_produtos.csv`, as auditorias e os documentos; a existência
    de um PDF antigo ou de uma pasta não comprova que a rodada atual
    terminou.

O script deve conter menos de 5.000.000 bytes, inclusive quando salvo
com terminações de linha do Windows. Use o arquivo distribuído completo;
não apague funções ou cargas incorporadas. Um bloqueio de cadastro da
equipe pode impedir a planilha de importação sem impedir os demais
produtos: registre a mensagem, confira a COLETA em **Equipe da COLETA**
e solicite o cadastro correto, sem inventar nomes ou CPFs.

## Navegar e conferir os novos relatórios

O relatório sintético favorece a leitura dos resultados principais; o
detalhado reúne resultados, auditorias e limites de interpretação. A
capa não exibe número. O índice, na página 2, liga cada seção à página
de destino por hiperlink e pontilhado. Clique no título para navegar. No
Word, aguarde a paginação terminar, pressione Ctrl+A e depois F9 para
atualizar o índice antes de imprimir ou exportar, inclusive na primeira
abertura. Se a paginação ainda mudar, repita a atualização. O PDF
fornecido já está paginado. É normal o PDF e o Word terem números de
páginas diferentes; confira o índice no próprio formato consultado.

Comece por **Escopo e método** e pelo esforço amostral: confirme UC,
anos, UAs e formações. O LPI registra presenças em 101 pontos ao longo
de 50 m, com intervalos de 50 cm. Várias categorias podem ocorrer no
mesmo ponto; suas coberturas não precisam somar 100%. Consulte o projeto
amostral da UC para áreas elegíveis, PAs previstos, prioridades,
alternativas e época de campo. O PA planejado, a UA efetivamente
instalada e cada ponto de leitura são unidades diferentes. As
recomendações de esforço não garantem, sozinhas, poder estatístico.

Nas seções de resultados, leia primeiro a figura/tabela e depois a
discussão específica da UC. **NE** significa não estimável no desenho
disponível, com motivo informado: não é resultado nulo. Diferencie
descrição, teste efetivamente executado e hipótese ainda não testada.
Métodos e condições de inferência estão em **Escopo e método** e
**Robustez inferencial**; as fontes e referências estão ao final, no
padrão ABNT.

## Fogo e cobertura vegetal: ativar, ler e verificar

Use `MONITORA_OPCAO_ANALISE_FOGO = "S"`. Deixe
`MONITORA_PASTA_DADOS_FOGO` vazio para obtenção automática ou informe
uma pasta de acervo compatível já auditado. O script consulta as Áreas
Atingidas por Fogo do ICMBio, grava um conjunto datado e registra a
fonte. Para solicitar atualização deliberada, use
`MONITORA_OPCAO_ATUALIZAR_DADOS_FOGO = "S"`; a versão anterior é
preservada. Retorne a `"N"` nas execuções seguintes para reutilizar o
conjunto e manter comparabilidade. Com a análise em `"N"`, o módulo não
consulta nem analisa essa fonte.

Confira os anos disponíveis, o ano parcial excluído e as UAs com posição
espacial elegível. **Sem registro de fogo**, **fonte indisponível** e
**histórico de queima conhecido** são situações distintas. Em UCs sem
interseções registradas, o texto e as análises são condicionais: não se
deve concluir ausência histórica de fogo nem comparar modalidades sem
observações.

Os mapas apresentam o histórico por UA sobre a base orbital disponível,
com norte, escala, coordenadas, localizador, legenda, informações
específicas e logos. Círculo indica histórico geral; triângulo,
incêndio; quadrado, queima prescrita. A cor representa **anos distintos
com registro**, não número de episódios. Leia o período de cada mapa: o
histórico geral e o de modalidades têm coberturas temporais diferentes.
Uma UA pode ter ambas as modalidades; não some suas contagens. Cinza
indica indeterminação. Use os recortes de detalhe quando disponíveis;
ampliar a imagem além de sua resolução não acrescenta informação.
Sentinel é contexto da paisagem, não observação de alta resolução de
cada toque do LPI.

A hipótese de manutenção pela queima prescrita e a de alteração pelo
incêndio exigem comparação adequada entre campanhas e controles. O
arquivo opcional `input/fogo_desenho_validado.csv` registra blocos,
eventos, controles, fases e janelas previamente conferidos; um modelo de
cabeçalho é gerado nos produtos. Seu preenchimento exige documentação de
campo e avaliação técnica. Não atribua datas de ignição, controles ou
independência apenas pela sobreposição de polígonos anuais. Sem desenho
suficiente, os mapas e a descrição permanecem, mas o teste fica como NE
com justificativa.

Nos contrastes efetivamente testados, **D** indica diferença detectada
após ajuste; **E5** e **E10**, equivalência demonstrada dentro das
margens exploratórias de ±5 e ±10 pontos percentuais; **?**, resultado
inconclusivo; **NE**, contraste não estimável. Esses símbolos acompanham
as tabelas/figuras pertinentes; não são letras de agrupamento universal
de médias. D pode coexistir com equivalência: uma diferença pequena pode
ser detectada e ainda caber na margem. Resultado não significativo não
demonstra manutenção. Regeneração requer observações pós-fogo
comparáveis, intervalos documentados e referência; aumento isolado de
cobertura não demonstra recuperação do estado anterior.

Ao interpretar a discussão final, associe a conclusão à modalidade e ao
suporte realmente disponível. Vegetação seca/morta e material botânico
podem indicar locais para investigar combustível, mas não medem sua
massa, umidade, continuidade ou inflamabilidade. A análise não prescreve
queima automaticamente. Preserve os arquivos `fogo_*.csv`, a auditoria
espacial e a referência do conjunto público junto dos relatórios.

## Clima e variáveis bioclimáticas: ativar, ler e verificar

Use `MONITORA_OPCAO_ANALISE_CLIMA = "S"`. A pasta
`MONITORA_PASTA_DADOS_CLIMA` pode ficar vazia para cache automático;
`MONITORA_OPCAO_ATUALIZAR_DADOS_CLIMA = "S"` solicita nova consulta.
Guarde a auditoria da fonte e reutilize o mesmo conjunto ao comparar
execuções.

O script obtém dados diários NASA POWER, baseados em reanálise, para
precipitação, temperatura média/mínima/máxima, umidade relativa e vento.
Deriva métricas e anomalias em relação a 1991–2020, com janelas
antecedentes de 7, 30, 90 e 180 dias. As janelas terminam na véspera da
coleta. Confira `clima_fontes.csv`, `clima_grade.csv`, as datas de campo
e a completude de cada métrica. Lacunas não são chuva zero. O déficit de
pressão de vapor é uma aproximação derivada, não uma medida de campo.

UAs na mesma célula compartilham a informação meteorológica. Centenas de
UAs não equivalem a centenas de estações independentes. A resolução
regional permite contextualizar as campanhas, mas não resolver
microclimas de encosta, solo ou borda. As associações descritivas e os
modelos condicionais devem ser lidos com os diagnósticos de variação dos
preditores e dependência espacial/temporal.

A análise inferencial pré-especifica chuva, temperatura e umidade nos 90
dias anteriores. O script verifica a possibilidade de separar seus
efeitos dos efeitos de UA e ano; suporte insuficiente produz NE. Uma
série curta pode apresentar descrições e, quando elegível, avaliação
preditiva por anos, sem sustentar um teste global robusto. Consulte os
motivos e os anos realmente usados, sem escolher a janela que produziu o
maior efeito. Correlação, coeficiente e capacidade preditiva não
comprovam causa. Preserve as séries, janelas, associações e diagnósticos
`clima_*.csv`.

## Calendário, série curta e esforço amostral

Confira a tabela de datas antes da figura **Influência do calendário e
sensibilidade da linha de base**. Se todas as campanhas ocorreram
praticamente na mesma época, pode faltar variação para estimar uma
associação global. Isso é diferente de demonstrar ausência de influência
sazonal. Se épocas e anos mudaram juntos, os efeitos podem estar
confundidos. Recortes e comparações pareadas só devem ser interpretados
onde há suporte observado.

Uma série curta, meses distintos com poucas UAs comuns e mudanças do
esforço exigem cautelas diferentes. O relatório apresenta diagnósticos,
comparações elegíveis e limitações; não estende um resultado de um
recorte para todos os anos. Consulte as UAs de cada contraste, os
intervalos de confiança e a sensibilidade à linha de base. Quando não
houver teste estimável, use as datas para planejar repetição nas mesmas
UAs e épocas, mantendo o histórico disponível. Não exclua uma campanha
somente para obter significância.

## Análise multivariada integrada e gestão

A seção integrada reúne o suporte disponível para época, clima,
histórico de fogo e demais indicadores. Leia primeiro as tabelas de
elegibilidade, colinearidade e representatividade da série. Uma variável
constante ou associada exclusivamente a um ano não permite separar seu
efeito. Mais variáveis em uma série pequena podem piorar a identificação
do modelo.

A candidata apresenta integração e ordenação exploratórias quando
possíveis. **PCA** resume padrões de covariação; suas cargas não
classificam causas da cobertura. A apresentação de métodos possíveis,
como RDA, modelos mistos ou GAMM, não significa que todos tenham sido
executados. Consulte o status de cada análise: o modelo longitudinal
conjunto não deve ser tratado como resultado disponível quando o
relatório registra que não foi implementado ou que o desenho é
insuficiente.

Em **Evidências, hipóteses e gestão**, separe o que foi observado, o que
foi testado e o que precisa de nova investigação. Procure direção e
magnitude dos efeitos, incerteza, abrangência do recorte e medidas de
campo necessárias. As recomendações devem ser compatíveis com a
evidência da UC, inclusive quando não houver fogo registrado, variação
de época ou série suficiente para inferência.

## Conferência do QField e entrega dos produtos

Depois da execução, abra as auditorias do QField. Quando tiver fornecido
um MBTiles detalhado, confirme sua identificação pelo zoom real, o
recorte sem alterar a fonte e a incorporação ao projeto. Confira as
camadas anuais em `dados/monitora.gpkg`, referências e acessos em
`dados/camadas_adicionais.gpkg`, quando presentes, e as camadas
editáveis **pontos_interesse** e **trajeto** em
`dados/apoio_campo.gpkg`. Para uma camada de acesso com papel especial,
use o manifesto `camadas_qfield.csv` descrito neste manual; apenas
renomear a camada não substitui a conferência do conteúdo.

Transfira o ZIP completo ao celular, extraia-o preservando as subpastas
e abra o projeto no QField. Se houver imagem detalhada, ligue essa
camada; localize as UAs e, quando houver mais de um ano, confira camadas
anuais distintas. Confira a legenda e teste em modo avião. Um projeto
apenas com Sentinel é válido como contexto regional e não substitui a
imagem detalhada. Verifique a visibilidade de pontos de interesse e,
numa cópia de teste, a criação e o salvamento de uma feição. Uma camada
vazia pode estar disponível para registro sem mostrar pontos no mapa.
Preserve o projeto que já contém observações de campo antes de
substituí-lo.

Na entrega, acompanhe os relatórios com um inventário por UC: rodada de
origem, versão usada, produtos presentes e impedimentos específicos.
Planilha bloqueada por cadastro de coletor precisa ser identificada por
campanha, sem declarar importação concluída. Arquivos produzidos não
comprovam importação no SISMONITORA nem teste físico no celular: essas
conferências devem ser registradas separadamente.

# Roteiro operacional do usuário

A tabela abaixo indica quais arquivos consultar antes de cada operação,
como operar no painel e quais produtos verificar depois para confirmar o
efeito da correção. Os links são relativos à pasta `manual_usuario/`; se
o manual for movido, abra-o a partir da pasta da execução para preservar
os hiperlinks.

<table class="monitora-tabela">

<thead>

<tr>

<th style="text-align:left;">

situacao
</th>

<th style="text-align:left;">

consultar_antes
</th>

<th style="text-align:left;">

operacao_no_painel
</th>

<th style="text-align:left;">

conferir_depois
</th>

<th style="text-align:left;">

criterio_sucesso
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Forma de vida desconhecida
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pre_painel'>Ocorrências
diagnósticas
pré-painel</a><br><code>output/02_painel_correcoes/ocorrencias/pre_painel/</code>
</td>

<td style="text-align:left;">

Selecionar a coleta/linha indicada, mover desconhecida para a categoria
e forma tecnicamente correta ou aplicar operação em lote quando a
decisão for comum ao escopo.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pos_painel'>Ocorrências
diagnósticas
pós-painel</a><br><code>output/02_painel_correcoes/ocorrencias/pos_painel/</code>
</td>

<td style="text-align:left;">

contador de forma de vida desconhecida = 0 e ausência de bloqueio em
registros_corrig/registros_validados
</td>

</tr>

<tr>

<td style="text-align:left;">

Ponto amostral sem interceptação
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias/completude/coletas_quarentenadas_por_incompletude_pre_painel.csv'>Auditoria
de completude 101
pontos</a><br><code>output/03_auditorias/completude/coletas_quarentenadas_por_incompletude_pre_painel.csv</code>
</td>

<td style="text-align:left;">

Verificar se a coleta deve permanecer quarentenada, ser excluída do
painel ou demandar correção do ponto/interceptação; não forçar
preenchimento sem evidência.
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias/completude'>Auditorias de completude
pós-processamento</a><br><code>output/03_auditorias/completude/</code>
</td>

<td style="text-align:left;">

coletas incompletas explicadas por quarentena ou saneadas com evidência;
painel sem ponto amostral sem interceptação impeditivo
</td>

</tr>

<tr>

<td style="text-align:left;">

Nativa sem forma de vida
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pre_painel'>Ocorrências
de nativa sem forma de
vida</a><br><code>output/02_painel_correcoes/ocorrencias/pre_painel/</code>
</td>

<td style="text-align:left;">

Preencher a forma de vida inferior compatível com o token superior em
Encostam.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pos_painel'>Relatórios
pós-painel</a><br><code>output/02_painel_correcoes/ocorrencias/pos_painel/</code>
</td>

<td style="text-align:left;">

contador de nativa sem forma de vida = 0
</td>

</tr>

<tr>

<td style="text-align:left;">

Exótica sem forma de vida
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pre_painel'>Ocorrências
de exótica sem forma de
vida</a><br><code>output/02_painel_correcoes/ocorrencias/pre_painel/</code>
</td>

<td style="text-align:left;">

Preencher a forma de vida exótica inferior compatível; se houver espécie
conhecida, preencher também o vínculo de espécie.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pos_painel'>Relatórios
pós-painel</a><br><code>output/02_painel_correcoes/ocorrencias/pos_painel/</code>
</td>

<td style="text-align:left;">

contador de exótica sem forma de vida = 0
</td>

</tr>

<tr>

<td style="text-align:left;">

Seca ou morta sem forma de vida
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pre_painel'>Ocorrências
de seca/morta sem forma de
vida</a><br><code>output/02_painel_correcoes/ocorrencias/pre_painel/</code>
</td>

<td style="text-align:left;">

Preencher a forma de vida de seca/morta inferior compatível com a
interceptação.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pos_painel'>Relatórios
pós-painel</a><br><code>output/02_painel_correcoes/ocorrencias/pos_painel/</code>
</td>

<td style="text-align:left;">

contador de seca ou morta sem forma de vida = 0
</td>

</tr>

<tr>

<td style="text-align:left;">

Outra forma de vida
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pre_painel'>Relatórios
de outras formas de
vida</a><br><code>output/02_painel_correcoes/ocorrencias/pre_painel/</code>
</td>

<td style="text-align:left;">

Mover ou limpar o conteúdo de outra forma de vida conforme contrato
XLSForm; evitar manter texto livre quando houver categoria padronizada.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/ocorrencias/pos_painel'>Relatórios
pós-painel de outras
formas</a><br><code>output/02_painel_correcoes/ocorrencias/pos_painel/</code>
</td>

<td style="text-align:left;">

contador de outra forma de vida impeditiva = 0
</td>

</tr>

<tr>

<td style="text-align:left;">

Revisão: forma de vida exótica sem espécie
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/apoio/pre_painel'>Relatório de
exóticas
pré-painel</a><br><code>output/02_painel_correcoes/apoio/pre_painel/</code>
</td>

<td style="text-align:left;">

Revisar se a ausência de espécie é aceitável. Quando houver
identificação, preencher espécie exótica ou outra espécie exótica.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/apoio/pos_painel'>Relatório de
exóticas
pós-correções</a><br><code>output/02_painel_correcoes/apoio/pos_painel/</code>
</td>

<td style="text-align:left;">

lista revisada; pendência não impeditiva documentada ou espécie
preenchida quando houver evidência
</td>

</tr>

<tr>

<td style="text-align:left;">

Revisão: forma de vida exótica com espécie
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/apoio/pre_painel'>Relatório de
exóticas com espécie
vinculada</a><br><code>output/02_painel_correcoes/apoio/pre_painel/</code>
</td>

<td style="text-align:left;">

Revisar coerência entre forma de vida exótica e espécie vinculada;
confirmar se a espécie corresponde ao ponto/coleta.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/apoio/pos_painel'>Relatório de
exóticas
pós-correções</a><br><code>output/02_painel_correcoes/apoio/pos_painel/</code>
</td>

<td style="text-align:left;">

lista revisada; vínculo forma/espécie coerente
</td>

</tr>

<tr>

<td style="text-align:left;">

Replay de correções anteriores
</td>

<td style="text-align:left;">

<a href='../input/linhagem/correcoes_semanticas_consolidada.csv'>Ledger
semântico consolidado
reaplicado</a><br><code>input/linhagem/correcoes_semanticas_consolidada.csv</code>
</td>

<td style="text-align:left;">

Não editar como dado bruto. Conferir se o replay foi aplicado antes do
painel e se não há duplicação de operações.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/auditorias/auditoria_reaplicacao_correcoes_anteriores_ultima_execucao.csv'>Auditoria
de
reaplicação</a><br><code>output/02_painel_correcoes/auditorias/auditoria_reaplicacao_correcoes_anteriores_ultima_execucao.csv</code>
</td>

<td style="text-align:left;">

operações anteriores aparecem em auditoria e os contadores impeditivos
esperados não reaparecem
</td>

</tr>

<tr>

<td style="text-align:left;">

Comparação contra oráculo
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias/replay_semantico'>Diagnóstico de
convergência com
oráculo</a><br><code>output/03_auditorias/replay_semantico/</code>
</td>

<td style="text-align:left;">

Não editar como entrada. Usar apenas para verificar se a run atual
convergiu com a run de referência.
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias/replay_semantico/oraculo_replay_selo_convergencia_pos_replay_final_reconciliado.csv'>Selo
final de convergência com
oráculo</a><br><code>output/03_auditorias/replay_semantico/oraculo_replay_selo_convergencia_pos_replay_final_reconciliado.csv</code>
</td>

<td style="text-align:left;">

em validação estrita, replay_equivalente_ao_oraculo = SIM; qualquer
divergência bloqueia a promoção
</td>

</tr>

<tr>

<td style="text-align:left;">

Token fora do domínio do XLSForm 2025
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias'>Auditorias do contrato e
domínio</a><br><code>output/03_auditorias/</code>
</td>

<td style="text-align:left;">

Corrigir o valor do registro para um name permitido e coerente com os
campos condicionais. Não criar alias, choice ou relevance local.
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias'>Domínio contratual
pós-correção</a><br><code>output/03_auditorias/</code>
</td>

<td style="text-align:left;">

token aceito no ramo correto e nenhuma ocorrência impeditiva
equivalente; contrato permanece inalterado
</td>

</tr>

<tr>

<td style="text-align:left;">

Token órfão outros em exótica histórica
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias'>Auditoria da migração histórica de
outros</a><br><code>output/03_auditorias/</code>
</td>

<td style="text-align:left;">

Revisar a auditoria. A migração automática só ocorre quando forma, texto
e destino são unívocos; se bloqueada, decidir o ramo biológico no painel
com evidência.
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias'>Células migradas e bloqueios
remanescentes</a><br><code>output/03_auditorias/</code>
</td>

<td style="text-align:left;">

texto preservado no campo condicional correto ou bloqueio humano
explicitamente resolvido
</td>

</tr>

<tr>

<td style="text-align:left;">

Equipe ou CPF da COLETA incorreto
</td>

<td style="text-align:left;">

<a href='../output/03_auditorias/cadastro/auditoria_sanitizacao_coletores.csv'>Auditoria
da equipe da
COLETA</a><br><code>output/03_auditorias/cadastro/auditoria_sanitizacao_coletores.csv</code>
</td>

<td style="text-align:left;">

Usar a aba Equipe da COLETA; editar uma pessoa do repeat por vez e
manter o CPF na mesma posição do nome.
</td>

<td style="text-align:left;">

<a href='../output/07_validacao'>Padronização da equipe herdada e
atual</a><br><code>output/07_validacao/</code>
</td>

<td style="text-align:left;">

nomes obrigatórios, CPFs opcionais válidos/inequívocos e modificações
visíveis no relatório
</td>

</tr>

<tr>

<td style="text-align:left;">

Coordenadas ou posição anual divergentes
</td>

<td style="text-align:left;">

<a href='../output/04_validacao_espacial'>Validação espacial
pré/pós-painel</a><br><code>output/04_validacao_espacial/</code>
</td>

<td style="text-align:left;">

Usar a aba espacial no escopo integral da COLETA; conferir origem,
destino e prévia antes de copiar, inverter, corrigir ou validar.
</td>

<td style="text-align:left;">

<a href='../output/04_validacao_espacial/pos_painel'>Status espacial
pós-painel</a><br><code>output/04_validacao_espacial/pos_painel/</code>
</td>

<td style="text-align:left;">

nenhuma pendência espacial; alertas aceitos permanecem documentados e
coordenadas anuais preservadas
</td>

</tr>

<tr>

<td style="text-align:left;">

Falha recuperável ao salvar o painel
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/operacoes/cache'>Checkpoint
recuperável da
sessão</a><br><code>output/02_painel_correcoes/operacoes/cache/</code>
</td>

<td style="text-align:left;">

Não fechar à força. Registrar a mensagem; o painel restaura os arquivos
anteriores e preserva filas/justificativas no checkpoint.
</td>

<td style="text-align:left;">

<a href='../output/02_painel_correcoes/operacoes'>Auditoria da falha e
restauração</a><br><code>output/02_painel_correcoes/operacoes/</code>
</td>

<td style="text-align:left;">

reabertura sobre a mesma base restaura exatamente a sessão; novo
salvamento conclui sem perder operações
</td>

</tr>

</tbody>

</table>

# Solução de problemas

Ao usar o botão **Source**, confirme a execução do arquivo grande na
caixa exibida pelo RStudio e execute o arquivo inteiro, nunca apenas um
trecho. O encapsulamento homologado mantém uma única expressão externa,
executa o corpo sem eco e desativa **Source with Echo** para as próximas
chamadas quando necessário. O script não relê o próprio arquivo nem
depende de iniciador auxiliar. Se o console parecer parado, verifique
barra/etapa e atividade de disco antes de interromper.

Se ocorrer erro, copie a mensagem integral e consulte primeiro o log,
`auditoria_produtos_finais_ultima_execucao.csv` e a auditoria específica
da etapa. Uma falha opcional de PDF, mapa orbital ou QField não autoriza
declarar esse produto concluído, mas os produtos canônicos anteriores
podem permanecer válidos quando a auditoria assim registrar. Não execute
novamente por cima da mesma pasta sem entender o ponto de parada; faça
uma cópia local e preserve o estado falho para diagnóstico.

Falha no XLSX: confira a auditoria de abas e volte ao download original
intacto. Falha do painel: preserve o checkpoint recuperável e não edite
as filas manualmente. Falha de registros_validados: corrija o registro,
não o contrato. Falha QField: confirme uma UC, as duas opções,
`qfield_input/`, zoom real, cobertura, validação espacial e permissão da
imagem. Falha de importação SISMONITORA: confira UC/ciclo/campanha, 101
pontos e política de UUID. Sanitização de coletores bloqueada: não
associe CPF por hipótese. Avisos de pacote compilado em outra versão do
R não equivalem, por si só, a erro do produto.

# Proteção de dados

Nome, CPF, UUID, caminhos locais e referências a fotografias podem ser
dados sensíveis ou operacionais. Não publique `input/`, produtos de
dados, caches, relatórios de validação ou logs sem triagem. As
auditorias de sanitização de coletores não registram os valores
pessoais. Compartilhe somente pelo diretório institucional autorizado e
preserve a rastreabilidade da rodadas.

# Conferência final

Após uma execução com painel, conferir:
`output/01_produtos_dados/registros_corrig.csv`; ocorrências pós-painel;
`pendencias_justificadas.csv`; auditoria de sanitização de coletores;
produtos espaciais quando habilitados; `registros_validados.csv` e seus
XLSX opcionais; `registros_corrig_stat.csv`; toda a linhagem; o
relatório consolidado em `output/07_validacao/`; relatórios analíticos;
projeto QField quando solicitado; e
`auditoria_produtos_finais_ultima_execucao.csv`. Todos os produtos
obrigatórios devem constar como presentes, não vazios e completos.

# Lista de conferência e glossário

## Antes de executar

- Trabalhar em cópia local exclusiva e confirmar que não há outra
  instância usando a pasta.
- Confirmar modo, input e linhagem correspondentes.
- Confirmar opções de produtos; deixar o PDF do manual em N quando não
  for necessário.
- Preservar uma cópia do input e não editar CSVs de produto manualmente.
- Se houver QField, separar arquivos espaciais em `qfield_input/` e
  confirmar que a execução contém uma UC.

## Antes de encerrar a rodada

- Clicar em **Atualizar prévia integral** e revisar o resultado.
- Conferir pendências impeditivas e justificativas das ocorrências de
  revisão.
- Salvar e fechar o painel; confirmar `registros_corrig.csv` e a pasta
  `linhagem/`.
- Consultar `output/07_validacao/`, `README_OUTPUT.txt` e
  `indice_produtos.csv`.
- Quando solicitado, conferir relatórios analíticos e testar o ZIP
  QField online e em modo avião.

## Glossário essencial

- **COLETA:** conjunto integral de linhas de uma unidade amostral em uma
  campanha.
- **Ocorrência impeditiva:** inconsistência que impede a geração de um
  produto validado.
- **Ocorrência em revisão:** situação a conferir que pode permanecer
  quando avaliada e justificada.
- **Linhagem:** histórico auditável que conecta decisões, aplicações e
  produtos entre rodadas.
- **Prévia integral:** simulação atômica das operações pendentes antes
  do salvamento.
- **Produto canônico:** cópia oficial do produto no diretório definido
  pelo contrato de saída.
