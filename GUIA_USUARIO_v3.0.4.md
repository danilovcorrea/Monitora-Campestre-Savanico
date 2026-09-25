# Guia do usuário — v3.0.4

O arquivo `monitora_campsav_alvo_global_v3.0.4.R` executa a curadoria e gera os
produtos. Os módulos necessários estão incorporados; não execute arquivos de
apoio de desenvolvimento. Na primeira utilização, mantenha conexão à internet
para instalar dependências e consultar as fontes públicas habilitadas.

O [manual completo em PDF](manual_usuario/manual_usuario_v3.0.4.pdf) e a
[versão navegável em HTML](manual_usuario/manual_usuario_v3.0.4.html) explicam
as operações para usuários iniciantes, com percursos de execução, controles do
painel, arquivos de entrada e conferência dos resultados.

## Começar

1. Crie uma pasta local curta, exclusiva para a rodada, fora da sincronização
   ativa do OneDrive. Copie o `.R` para ela e crie `input/`.
2. Para uma primeira rodada, coloque as exportações brutas do SISMONITORA em
   `input/`. Para continuidade, use a base `registros_corrig.csv` da rodada
   conferida e sua linhagem correspondente. Siga o percurso específico do manual.
3. Abra o `.R` no RStudio em UTF-8. Edite somente as opções do bloco operacional.
   O arquivo fica abaixo de 5.000.000 bytes, inclusive com terminações CRLF.
4. Para curadoria, escolha o modo com painel. Revise as abas de registros,
   equipe da coleta, validação espacial e justificativas; registre responsável
   e justificativa nas operações. Não edite diretamente os CSVs gerados.
5. Para atualizar produtos de uma base conferida, escolha
   `registros_corrig_completo`, com abertura do painel em `N`.
6. Clique em **Source** e execute o arquivo inteiro. Ao final, confira a
   auditoria de produtos e o índice do `output/`. Depois copie a rodada
   concluída para o diretório institucional, preservando a rastreabilidade.

Uma justificativa documenta a decisão, mas não elimina uma inconsistência
impeditiva. O script preserva um checkpoint quando a base ainda exige curadoria;
não declare os produtos bloqueados como concluídos.

## Produtos e análises

| Produto | Configuração e conferência |
|---|---|
| Base validada | Ative a geração de registros validados; confira os impedimentos antes da entrega. |
| Importação SISMONITORA | Ative a opção específica. As planilhas são separadas por UC, ciclo e campanha. Cadastros de coletores podem bloquear o produto; não invente nomes/CPFs. A política de UUID depende da finalidade da importação. |
| Relatório de validação | Confira alterações herdadas/atuais, pendências, exclusões e linhagem. |
| Relatórios analíticos | Confira esforço, datas, comparabilidade, resultados, incerteza e discussão. Use o índice com páginas e links. |
| Fogo | Ative `MONITORA_OPCAO_ANALISE_FOGO`. O próprio R obtém/cacheia a fonte ICMBio. Leia período, modalidade, exposição espacial e condições dos testes. |
| Clima | Ative `MONITORA_OPCAO_ANALISE_CLIMA`. O próprio R consulta/cacheia NASA POWER. UAs na mesma célula compartilham os dados meteorológicos. |
| QField | Geração e importação de camadas têm padrão S. Coloque MBTiles e vetores em `qfield_input/`. Ausência de fontes válidas e bloqueios são informados no console, sem interromper os demais produtos. Confira auditorias e teste o ZIP no celular, inclusive em modo avião. |
| Manual | Ative sua geração; o PDF tem opção própria. O manual fica em `manual_usuario/`, ao lado de `input/` e `output/`. |

**NE** significa que o teste não pôde ser estimado sob o desenho disponível;
não demonstra ausência de efeito. No fogo, D indica diferença detectada;
E5/E10 indicam equivalência demonstrada nas margens exploratórias de ±5/±10
pontos percentuais. Ausência de significância não é equivalência. A ordenação
multivariada exploratória não identifica causas nem substitui o modelo
longitudinal conjunto. Consulte o status de cada análise antes de interpretar.

## Alertas temporais no QField

Referência temporal ambígua ou insuficiente é alerta quando as coordenadas são
utilizáveis. Preserve as observações e confirme os vergalhões em campo, sem
correção artificial nem justificativa obrigatória para a limitação temporal.
As camadas mantêm as alternativas por campanha e os rótulos indicam incerteza.
Divergências, possível troca/inversão, coordenadas inválidas e conflitos continuam
impedindo o projeto integral. Consulte as pendências e alertas do painel antes
da geração; aceitar uma posição para navegação não a torna automaticamente
elegível para análises científicas.

## Preservação e suporte

O contrato XLSForm e a execução por um único arquivo R permanecem preservados.
Guarde a rodada anterior, o arquivo executado, sua versão/build e os logs.
Quando houver falha, registre a etapa e a mensagem integral. Não contorne o
bloqueio alterando o contrato ou eliminando observações sem decisão documentada.

As planilhas, coordenadas, fotografias, nomes de coletores e relatórios locais
podem conter informações institucionais ou pessoais. O pacote público da versão
contém software e documentação; as rodadas das UCs têm entrega institucional
separada.


## Destinos espaciais e nomes QField

A UA de destino acompanha os filtros gerais e espaciais escolhidos. Selecionar
uma coleta para inspeção lateral não reduz a lista de destinos nem substitui o
destino escolhido. Limpar filtros restaura o domínio disponível; seleções que
ficam fora de um filtro deliberado são removidas. Confira coleta, UA e ano no
preview antes de aplicar a operação.

No QField, os extremos anuais aparecem como `verg_ini_YYYY` e `verg_fin_YYYY`.
Os nomes físicos das camadas e a identificação da UC nos dados são preservados.


## Numeração e elementos ausentes nos relatórios

Cada relatório reinicia as sequências de tabelas e figuras. A numeração é
atribuída depois de selecionar e ordenar o conteúdo: tabelas vazias, figuras
não selecionadas e análises inaplicáveis não reservam números. As seções têm
numeração hierárquica, refletida no índice. Consulte a auditoria de numeração
junto aos arquivos do relatório para conhecer os objetos incluídos e omitidos.

As tabelas do Word são editáveis. Nas tabelas com mais de seis colunas, os campos
são apresentados em identificação e resultados/contexto, preservando os valores.
No HTML/PDF, a legenda fica associada à tabela e é conferida antes do fechamento.
Não interprete a ausência de uma análise como evidência de ausência de efeito.


## Caminhos, conexões e diagnóstico

Os relatórios analíticos ficam em `output/08_analises/u_<hash>/`, com nomes
`analitico_sintetico` e `analitico_detalhado`. Validação fica em `07_validacao`
e auditorias em `03_auditorias`; a identificação da UC permanece nos documentos e
metadados. Dados e linhagem preservam suas pastas canônicas. Use uma pasta de
execução curta e confira o índice antes de compartilhar. Para um destino maior
que a reserva padrão de 120 caracteres, informe o caminho completo em
`MONITORA_DESTINO_COMPARTILHAMENTO` antes de executar o script. O índice considera
210 caracteres para CSV/XLSX, 240 para documentos e 259 para os demais arquivos;
um destino arbitrariamente longo ainda pode ultrapassar esses limites.

Imagens e camadas adicionais entram em `qfield_input/`. A pasta invertida
`input_qfield/` gera aviso e não é importada automaticamente. O console informa
progresso dos recortes, tentativas de conexão, espera, duração e motivo das falhas.
Consulte `log/` para auditorias de consultas de fogo e avisos agrupados. Uma falha
na fonte não significa ausência de fogo; não descarte uma pendência com base nela.
A geração de PDF tem limite de tempo e preserva a causa do erro; um PDF antigo
não é aceito como resultado novo. Confira os produtos ao encerrar a execução.

## Registros históricos e relatórios

A exigência de hábito segue o protocolo do registro. No protocolo de 2023, a ausência de hábito da samambaia não impede a mudança de categoria. Não invente um hábito para contornar a validação. Em lotes mistos, preencha somente o que o protocolo exigir. Confira a prévia e a trilha da operação.

As tabelas de estado indicam a campanha utilizada; figuras temporais podem abranger períodos distintos. Painéis extensos são divididos sem excluir resultados. Tabelas e figuras indisponíveis não reservam números na sequência final.
