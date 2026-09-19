# Guia do usuário — v3.0.0

O arquivo `monitora_campsav_alvo_global_v3.0.0.R` executa a curadoria e gera os
produtos. Os módulos necessários estão incorporados; não execute arquivos de
apoio de desenvolvimento. Na primeira utilização, mantenha conexão à internet
para instalar dependências e consultar as fontes públicas habilitadas.

O [manual completo em PDF](manual_usuario/manual_usuario_v3.0.0.pdf) e a
[versão navegável em HTML](manual_usuario/manual_usuario_v3.0.0.html) explicam
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
| QField | Ative geração e, para arquivos externos, importação de camadas. Coloque MBTiles e vetores em `qfield_input/`. Confira auditorias e teste o ZIP no celular, inclusive em modo avião. |
| Manual | Ative sua geração; o PDF tem opção própria. O manual fica em `manual_usuario/`, ao lado de `input/` e `output/`. |

**NE** significa que o teste não pôde ser estimado sob o desenho disponível;
não demonstra ausência de efeito. No fogo, D indica diferença detectada;
E5/E10 indicam equivalência demonstrada nas margens exploratórias de ±5/±10
pontos percentuais. Ausência de significância não é equivalência. A ordenação
multivariada exploratória não identifica causas nem substitui o modelo
longitudinal conjunto. Consulte o status de cada análise antes de interpretar.

## Preservação e suporte

O contrato XLSForm e a execução por um único arquivo R permanecem preservados.
Guarde a rodada anterior, o arquivo executado, sua versão/build e os logs.
Quando houver falha, registre a etapa e a mensagem integral. Não contorne o
bloqueio alterando o contrato ou eliminando observações sem decisão documentada.

As planilhas, coordenadas, fotografias, nomes de coletores e relatórios locais
podem conter informações institucionais ou pessoais. O pacote público da versão
contém software e documentação; as rodadas das UCs têm entrega institucional
separada.
