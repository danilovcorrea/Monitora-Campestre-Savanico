### Script de análise de dados do Alvo Global do Componente Campestre Savânico do Programa Monitora.

### O script CAMPSAV_Alvo_Global_form_novo.R analisa registros obtidos a partir do novo formulário a ser
### implementado no ambiente de produção do SISMONITORA (atualmente em desenvolvimento, previsto para
### lançamento em maio/2024).
### Todos os scripts estão em desenvolvimento, podendo ser atualizados a qualquer momento.
### Instruções para a utilização dos scripts encontram-se comentadas em cada arquivo.
### Todos os scritps estão disponíveis para utilização, modificação e compartilhamento, citada a fonte.

### Para citar este repositório:
###    CBC - ICMBio/MMA, 2024. Scripts de análise de dados do Alvo Global do Componente Campestre Savânico
###    do Programa Monitora. Desenvolvido por Danilo Correa - CBC/ICMBio.

### Disponível em https://github.com/danilovcorrea/Monitora-Campestre-Savanico

### Contato: danilo.correa@icmbio.gov.br

### set working directory as script source location

### INSTRUÇÕES PARA UTILIZAÇÃO DO SCRIPT

### 1 - Baixe os registros em formato .csv (contidos em arquivo .zip) do SISMONITORA (OBS: este script foi
###     desenvolvido analisar os registros obtidos a partir do formulário PLANTASHERBACEASELENHOSAS_CAMPSAV
###     _11AGO22 (Básico e Avançado).
###     Para analisar dados obtidos por outro formulário, baixe o script correspondente disponível no
###     repositório https://github.com/danilovcorrea/Monitora-Campestre-Savanico;
###     Não é necessário extrair os arquivos. O script fará a extração automaticamente.
### 2 - Cole o arquivo .R contendo este script no mesmo diretório onde estão os arquivos .zip baixados;
### 3 - Selecione todo o script (CTRL+A no Windows e Linux, CMD+A no macOS);
### 4 - Execute o script (CTRL+ENTER);
### 5 - Após a conclusão da execução, será gerado o arquivo "registros_corrig_stat.csv" contendo as estatís-
###     ticas necessárias para análise. O arquivo estará localizado no mesmo diretório do script;
### 6 - Os gráficos gerados podem ser exportados em formato .png ou .pdf no menu "Export" na aba "plots";


### Verificação e download dos pacotes necessários:

if (!require("dplyr"))
  install.packages("dplyr")
library("dplyr")
if (!require("data.table"))
  install.packages("data.table")
library("data.table")
if (!require("purrr"))
  install.packages("purrr")
library("purrr")
if (!require("stringr"))
  install.packages("stringr")
library("stringr")
if (!require("tidyverse"))
  install.packages("tidyverse")
library("tidyverse")
if (!require("ggplot2"))
  install.packages("ggplot2")
library("ggplot2")

### lista e extração de todos os arquivos .zip. OBS: necessário ter o app unzip instalado.

zipfiles <-
  list.files(
    path = setwd(dirname(
      rstudioapi::getActiveDocumentContext()$path
    )),
    pattern = "*.zip",
    recursive = T,
    full.names = TRUE
  )

purrr::map(.x = zipfiles, .f = unzip, exdir = "extracted")

rm(zipfiles)

### Leitura e concatenação (por linha) dos arquivos .csv


csvfiles <-
  list.files(
    path = setwd(dirname(
      rstudioapi::getActiveDocumentContext()$path
    )),
    pattern = "*.csv",
    recursive = T,
    full.names = TRUE
  ) %>%
  
  stringr::str_subset(., "registros_corrig.csv", negate = TRUE) %>%
  stringr::str_subset(., "registros_corrig_stat", negate = TRUE) %>%
  stringr::str_subset(., "sum_herbacea_sum_lenhosa.csv", negate = TRUE) %>%
  stringr::str_subset(., "sum_categorias.csv", negate = TRUE) %>%
  stringr::str_subset(., "sum_form_vida_nativas.csv", negate = TRUE) %>%
  stringr::str_subset(., "sum_form_vida_exoticas.csv", negate = TRUE) %>%
  stringr::str_subset(., "sum_form_vida_secas_mortas.csv", negate = TRUE)

registros <-
  data.table::rbindlist(
    lapply(csvfiles, fread, colClasses = "character"),
    idcol = TRUE,
    fill = TRUE,
    use.names = TRUE
  )
registros[, .id := factor(.id, labels = basename(csvfiles))]

setnames(registros, make.unique(names(registros)))

rm(csvfiles)

### criação do arquivo onde serão realizadas as correções, mantendo o arquivo original:

registros -> registros_corrig

### correção de labels para names (erro em uma versao do xlsform, presente em algumas campanhas amostrais)

registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida"
    )
  )


registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida"
    )
  )

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
      "Graminoide \\(gramíneas, ciperácease juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto abaixo de 0,5m de altura," = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura," = "arbusto_acima",
      "Arbusto abaixo de 0,5m de altura" = "arbusto_abaixo",
      "Arbusto acima de 0,5m de altura" = "arbusto_acima",
      "Árvore abaixo de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore acima de 5cm de diâmetro a 30 cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Bromelioide \\(bromélias e apiáceas\\)" = "bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida"
    )
  )

### extração do último token em colunas específicas (o SISMONITORA exporta a lista concatenada por "|")

## ponto amostral

registros_corrig$`ponto_amostral (amostragem/registro)` <-
  word(registros_corrig$`ponto_amostral (amostragem/registro)`,
       sep = fixed("|"),-1)

## metro

registros_corrig$`ponto_metro (amostragem/registro)` <-
  word(registros_corrig$`ponto_metro (amostragem/registro)`,
       sep = fixed("|"),-1)

## encostam na vareta

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  word(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
       sep = fixed("|"),-1)

## forma de vida de planta nativa

registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "nativa",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## erva bromelioide nativa

registros_corrig$`A erva bromelioide observada é: (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "bromelioide",
      negate = FALSE
    ),
    word(
      registros_corrig$`A erva bromelioide observada é: (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## cactacea nativa

registros_corrig$`A cactácea observada é: (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A cactácea observada é: (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## orquídea nativa

registros_corrig$`A orquídea observada é: (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "orquidea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A orquídea observada é: (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## forma de vida de planta exótica

registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "exot",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## erva bromelioide exótica

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "bromelioide",
      negate = FALSE
    ),
    word(
      registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## cactacea exótica

registros_corrig$`A cactácea observada é: (amostragem/registro).1` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A cactácea observada é: (amostragem/registro).1`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## orquídea exótica

registros_corrig$`A orquídea observada é: (amostragem/registro).1` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "orquidea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A orquídea observada é: (amostragem/registro).1`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## forma de vida de planta seca ou morta

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  fifelse(
    str_detect(
      registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
      "seca_morta",
      negate = FALSE
    ),
    word(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## erva bromelioide seca ou morta

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "bromelioide",
      negate = FALSE
    ),
    word(
      registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## cactacea seca ou morta

registros_corrig$`A cactácea observada é: (amostragem/registro).2` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A cactácea observada é: (amostragem/registro).2`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

## orquídea seca ou morta

registros_corrig$`A orquídea observada é: (amostragem/registro).2` <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "orquidea",
      negate = FALSE
    ),
    word(
      registros_corrig$`A orquídea observada é: (amostragem/registro).2`,
      sep = fixed("|"),-1
    ),
    NA_character_
  )

### extração do ANO a partir da DATA. O SISMONITORA exportou datas em diferentes formatos. A função
### a seguir faz a coerção dos formatos identificados

registros_corrig$`Data (data_hora)` <-
  fifelse(
    str_detect(registros_corrig$`Data (data_hora)`, "([0-9]+(/[0-9]+)+)"),
    as.Date(registros_corrig$`Data (data_hora)`, "%d/%m/%Y"),
    as.Date(registros_corrig$`Data (data_hora)`, "%Y-%m-%d")
  )

registros_corrig$ANO <-
  format(as.Date(registros_corrig$`Data (data_hora)`, "%Y-%m-%d"),
         "%Y")


### construção das tabelas estatísticas

## somatório de categorias por UC, UA, ANO

sum_categ_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`
  ) %>%
  separate_rows(`**Encostam** na vareta: (amostragem/registro)`, sep = "\\s+") %>%
  filter(nzchar(`**Encostam** na vareta: (amostragem/registro)`)) %>%
  dplyr::count(UA, `**Encostam** na vareta: (amostragem/registro)`) %>%
  spread(`**Encostam** na vareta: (amostragem/registro)`, n, fill = 0)

## somatório de formas de vida nativas por UC, UA, ANO

sum_form_vida_nat_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`
  ) %>%
  separate_rows(
    `Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
    sep = "\\s+"
  ) %>%
  filter(
    nzchar(
      `Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`
    )
  ) %>%
  dplyr::count(
    UA,
    `Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`
  ) %>%
  spread(
    `Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
    n,
    fill = 0
  ) %>%
  rename_with( ~ paste("nativa", ., sep = "_"),-(1:6))

## somatório de formas de vida exóticas por UC, UA, ANO

sum_form_vida_exot_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`
  ) %>%
  separate_rows(
    `Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
    sep = "\\s+"
  ) %>%
  filter(
    nzchar(
      `Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`
    )
  ) %>%
  dplyr::count(
    UA,
    `Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`
  ) %>%
  spread(
    `Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
    n,
    fill = 0
  ) %>%
  rename_with( ~ paste("exot", ., sep = "_"),-(1:6))

## somatório de formas de vida secas ou mortas por UC, UA, ANO

sum_form_vida_seca_morta_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`
  ) %>%
  separate_rows(
    `Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
    sep = "\\s+"
  ) %>%
  filter(
    nzchar(
      `Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`
    )
  ) %>%
  dplyr::count(
    UA,
    `Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`
  ) %>%
  spread(
    `Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
    n,
    fill = 0
  ) %>%
  rename_with( ~ paste("seca_morta", ., sep = "_"),-(1:6))

### formação vegetacional por UC, UA, ANO:

form_veg <- registros_corrig %>%
  group_by(
    .id,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`,
    form_veg = registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`
  ) %>%
  dplyr::summarise()

### merge data tables:

registros_corrig_stat <-
  Reduce(
    function(...)
      merge(
        ...,
        all = TRUE,
        by = c(
          ".id",
          "UC",
          "UA",
          "ANO",
          "Coordenada inicial da amostragem (amostragem)",
          "Coordenada final da amostragem (amostragem)"
        )
      ),
    list(
      form_veg,
      sum_categ_by_UC_UA_ANO,
      sum_form_vida_nat_by_UC_UA_ANO,
      sum_form_vida_exot_by_UC_UA_ANO,
      sum_form_vida_seca_morta_by_UC_UA_ANO
    )
  )

### criação de colunas "lat_ini", "long_ini", "alt_ini", "acc_ini" e
### "lat_fin", "long_fin", "alt_fin", "acc_fin"

registros_corrig_stat <- registros_corrig_stat %>%
  separate_wider_delim(
    cols = `Coordenada inicial da amostragem (amostragem)`,
    delim = " ",
    names = c("lat_ini", "long_ini", "alt_ini", "acc_ini")
  ) %>%
  separate_wider_delim(
    cols = `Coordenada final da amostragem (amostragem)`,
    delim = " ",
    names = c("lat_fin", "long_fin", "alt_fin", "acc_fin")
  )

### remove NA:

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::select(-any_of(c(
    "nativa_<NA>", "exot_<NA>", "seca_morta_<NA>"
  )))

### criação sum_nativa, sum_exotica, sum_seca_morta and remove nativa, exotica, seca_morta correction:

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(
    sum_nativa = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
    ))) %>% rowSums(na.rm = TRUE),
    .after = form_veg
  ) %>%
  dplyr::mutate(
    sum_exotica = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
    ))) %>% rowSums(na.rm = TRUE),
    .after = sum_nativa
  ) %>%
  dplyr::mutate(
    sum_seca_morta = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
    ))) %>% rowSums(na.rm = TRUE),
    .after = sum_exotica
  ) %>%
  dplyr::select(-any_of(c("exotica",
                          "nativa",
                          "seca_morta")))

### criação sum_herbacea, sum_lenhosa

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_herbacea = select(
    .,
    c(
      which(str_detect(
        colnames(registros_corrig_stat), "_graminoide", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_bambu", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_bromelioide", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_cactacea", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_passarinho", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_orquidea", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_samambaia", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_canela_de_ema", negate = FALSE
      ))
    )
  ) %>%
    rowSums(na.rm = TRUE),
  .after = form_veg)

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_lenhosa = select(., c(
    which(str_detect(
      colnames(registros_corrig_stat), "_arbusto_abaixo", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_arbusto_acima", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_arvore_abaixo", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_arvore_acima", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_lianas", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_palmeira", negate = FALSE
    ))
  )) %>%
    rowSums(na.rm = TRUE),
  .after = sum_herbacea)

### remoção de objetos não mais necessários:

rm(registros)
rm(sum_categ_by_UC_UA_ANO)
rm(sum_form_vida_nat_by_UC_UA_ANO)
rm(sum_form_vida_exot_by_UC_UA_ANO)
rm(sum_form_vida_seca_morta_by_UC_UA_ANO)
rm(form_veg)

### Análises

### Proporção relativa de plantas herbáceas e lenhosas

## plot herbacea x lenhosa

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "sum_herbacea", negate = FALSE)
)))) |
sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "sum_lenhosa", negate = FALSE)
))))
> 0) {
  reg_herb_lenh <- registros_corrig_stat %>%
    dplyr::select(UC, UA, ANO, form_veg, sum_herbacea, sum_lenhosa) %>%
    pivot_longer(!c(UC, UA, ANO, form_veg),
                 names_to = "categoria",
                 values_to = "n")
  
  library(scales)
  (
    p1 <- ggplot(reg_herb_lenh, aes(form_veg, n, fill = categoria)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle("Proporção relativa de plantas herbáceas e lenhosas") +
      theme_bw()
  )
}

## reg_corrig_stat_summarise_p1

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "sum_herbacea", negate = FALSE)
)))) |
sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "sum_lenhosa", negate = FALSE)
))))
> 0) {
  reg_corrig_stat_summarise_p1 <- registros_corrig_stat %>%
    dplyr::select(any_of(c(
      "UC", "ANO", "form_veg", "sum_herbacea", "sum_lenhosa"
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = c(sum_herbacea, sum_lenhosa)
    ) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p1 <- reg_corrig_stat_summarise_p1 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de plantas herbáceas e lenhosas") +
    theme_bw()
  
  p1
}

### Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira
### e solo nu

## plot nativa, exotica, seca_morta, serrapilheira, solo_nu

reg_categ_plantas_longer <- registros_corrig_stat %>%
  dplyr::select(any_of(
    c(
      "UC",
      "UA",
      "ANO",
      "form_veg",
      "sum_nativa",
      "sum_exotica",
      "sum_seca_morta",
      "serrapilheira",
      "solo_nu"
    )
  )) %>%
  pivot_longer(!c(UC, UA, ANO, form_veg),
               names_to = "categoria",
               values_to = "n") %>%
  filter(n > 0)

library(scales)
(
  p2 <-
    ggplot(reg_categ_plantas_longer, aes(form_veg, n, fill = categoria)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent) +
    ggtitle(
      "Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira e solo nu"
    ) +
    theme_bw()
)

## reg_corrig_stat_summarise_p2

reg_corrig_stat_summarise_p2 <- registros_corrig_stat %>%
  dplyr::select(any_of(
    c(
      "UC",
      "ANO",
      "form_veg",
      "sum_nativa",
      "sum_exotica",
      "sum_seca_morta",
      "serrapilheira",
      "solo_nu"
    )
  )) %>%
  pivot_longer(
    names_to = "categoria",
    values_to = "soma",
    cols = -c("UC", "ANO", "form_veg")
  ) %>%
  group_by(UC, ANO, form_veg, categoria) %>%
  dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
  dplyr::mutate(prop = prop.table(n)) %>%
  filter(n > 0)

p2 <- reg_corrig_stat_summarise_p2 %>%
  ggplot(aes(form_veg, prop, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = paste0(
    "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
  )),
  position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = percent) +
  ggtitle(
    "Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira e solo nu"
  ) +
  theme_bw()

p2

### Proporção relativa de formas de vida de plantas nativas

## plot formas vida nativa

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
)))) > 0) {
  reg_formas_vida_nat_longer <- registros_corrig_stat %>%
    dplyr::select(UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
    ))) %>%
    pivot_longer(!c(UC, UA, ANO, form_veg),
                 names_to = "categoria",
                 values_to = "n")
  
  library(scales)
  (
    p3 <-
      ggplot(
        reg_formas_vida_nat_longer,
        aes(form_veg, n, fill = categoria)
      ) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle(
        "Proporção relativa de formas de vida e outras categorias de plantas nativas"
      ) +
      theme_bw()
  )
}

## reg_corrig_stat_summarise_p3

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
)))) > 0) {
  reg_corrig_stat_summarise_p3 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p3 <- reg_corrig_stat_summarise_p3 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas nativas") +
    theme_bw()
  
  p3
}

### Proporção relativa de formas de vida de plantas exóticas

## plot formas vida exotica

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
)))) > 0) {
  reg_formas_vida_exot_longer <- registros_corrig_stat %>%
    dplyr::select(UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
    ))) %>%
    pivot_longer(!c(UC, UA, ANO, form_veg),
                 names_to = "categoria",
                 values_to = "n")
  
  library(scales)
  (
    p4 <-
      ggplot(
        reg_formas_vida_exot_longer,
        aes(form_veg, n, fill = categoria)
      ) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle(
        "Proporção relativa de formas de vida e outras categorias de plantas exóticas"
      ) +
      theme_bw()
  )
}

## reg_corrig_stat_summarise_p4

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
)))) > 0) {
  reg_corrig_stat_summarise_p4 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p4 <- reg_corrig_stat_summarise_p4 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas exóticas") +
    theme_bw()
  
  p4
}

### Proporção relativa de formas de vida de plantas secas ou mortas

### plot formas vida seca ou morta

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
)))) > 0) {
  reg_formas_vida_seca_morta_longer <- registros_corrig_stat %>%
    dplyr::select(UC, UA, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
    ))) %>%
    pivot_longer(!c(UC, UA, ANO, form_veg),
                 names_to = "categoria",
                 values_to = "n")
  
  library(scales)
  (
    p5 <-
      ggplot(
        reg_formas_vida_seca_morta_longer,
        aes(form_veg, n, fill = categoria)
      ) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle(
        "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas"
      ) +
      theme_bw()
  )
}

### reg_corrig_stat_summarise_p5

if (sum(select(.data = registros_corrig_stat, which((
  str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
)))) > 0) {
  reg_corrig_stat_summarise_p5 <- registros_corrig_stat %>%
    dplyr::select(., UC, ANO, form_veg, which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)
    ))) %>%
    pivot_longer(
      names_to = "categoria",
      values_to = "soma",
      cols = -c("UC", "ANO", "form_veg")
    ) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p5 <- reg_corrig_stat_summarise_p5 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas") +
    theme_bw()
  
  p5
}

### remoção de objetos não mais necessários:

rm(reg_herb_lenh)
rm(reg_categ_plantas_longer)
rm(reg_formas_vida_nat_longer)
rm(reg_formas_vida_exot_longer)
rm(reg_formas_vida_seca_morta_longer)

### exportação das tabelas estatísticas em .csv

fwrite(registros_corrig,
       file.path("registros_corrig.csv"),
       row.names = FALSE)

fwrite(registros_corrig_stat,
       file.path("registros_corrig_stat.csv"),
       row.names = FALSE)

fwrite(
  reg_corrig_stat_summarise_p1,
  file.path("sum_herbacea_sum_lenhosa.csv"),
  row.names = FALSE)

fwrite(reg_corrig_stat_summarise_p2,
       file.path("sum_categorias.csv"),
       row.names = FALSE)

fwrite(reg_corrig_stat_summarise_p3,
       file.path("sum_form_vida_nativas.csv"),
       row.names = FALSE)

fwrite(
  reg_corrig_stat_summarise_p4,
  file.path("sum_form_vida_exoticas.csv"),
  row.names = FALSE)

fwrite(
  reg_corrig_stat_summarise_p5,
  file.path("sum_form_vida_secas_mortas.csv"),
  row.names = FALSE)
