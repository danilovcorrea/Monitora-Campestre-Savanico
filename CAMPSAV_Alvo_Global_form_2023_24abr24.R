### set working directory as script source location

#rstudioapi::getActiveDocumentContext

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### csv batch import and read

#install.packages("dplyr")                           # Install dplyr package
#install.packages("plyr")                            # Install plyr package
#install.packages("readr")                           # Install readr package

#library("dplyr")                                    # Load dplyr package
#library("plyr")                                     # Load plyr package
#library("readr")   
library("data.table")
#library("tidyverse")

# get all the zip files, unzip and remove temp object

zipF <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), pattern = "*.zip", recursive = T, full.names = TRUE)

# lapply(.data = zipF, fun = unzip, exdir = "extracted")

library("purrr")
purrr::map(.x = zipF, .f = unzip, exdir = "extracted")
detach("package:purrr", unload=TRUE)

#library("plyr")
#llply(.data = zipF, .fun = unzip, exdir = "extracted")
#detach("package:plyr", unload=TRUE)

rm(zipF)

#zipped_csv_names <- grep('\\.csv$', unzip(zipF, list=TRUE)$Name, 
#                         ignore.case=TRUE, value=TRUE)

#install.packages("easycsv")
#library(easycsv)

#fread_zip(zipF, extension = "CSV", colClasses=list(character=1:95))

#csvnames <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
#                        pattern = "*.csv", recursive = T, full.names = TRUE)

#registros <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
#                        pattern = "*.csv", recursive = T, full.names = TRUE) %>% 
#  stringr::str_subset(., "registros_corrig_stat", negate = TRUE) %>%
#  lapply(fread,colClasses=list(character=1:95,extension = "CSV")) %>%                              # Store all files in list
#  #.[,csvname] <- csvnames %>%
#  bind_rows                                         # Combine data sets into one data set 
#registros


csvfiles <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
                       pattern = "*.csv", recursive = T, full.names = TRUE) %>%
  stringr::str_subset(., "registros_corrig_stat", negate = TRUE)

registros <- data.table::rbindlist(lapply(csvfiles, fread, colClasses = "character"), idcol = TRUE,fill=TRUE,use.names = TRUE)
registros[, .id := factor(.id, labels = basename(csvfiles))]

setnames(registros, make.unique(names(registros)))


###

#registros <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
#                        pattern = "*.csv", recursive = T, full.names = TRUE) %>%
#  stringr::str_subset(., "registros_corrig_stat", negate = TRUE) %>%
#  rbindlist((lapply(.,fread, colClasses = "character")), idcol = TRUE,fill=TRUE,use.names = TRUE)

### Column classes to fread

#reg_classes <- list("character", "integer", c("POSIXct", "POSIXt"), c("POSIXct", "POSIXt"), "character", 
#                "character", "character", "character", "character", "character", 
#                "character", "character", "character", "character", "character", 
#                c("IDate", "Date"), "character", "character", "character", "character",
#                "character", "character", "character", "character", "integer",
#                "character", "integer", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character",
#                "character", "character", "character", "character", "character")


#library("dplyr")                                    # Load dplyr package
#library("plyr")                                     # Load plyr package
#library("readr")   
#library("data.table")

#registros <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
#                        pattern = "*.csv", recursive = T, full.names = TRUE) %>% 
#                        stringr::str_subset(., "registros_corrig_stat", negate = TRUE) %>%
#                        lapply(fread, colClasses=list(character=1:95)) %>%                              # Store all files in list
#                        bind_rows                                         # Combine data sets into one data set 
#registros                                           # Print data to RStudio console

#detach("package:data.table", unload=TRUE)
#detach("package:plyr", unload=TRUE)
#detach("package:readr", unload=TRUE)

#library("dplyr")                                    # Load dplyr package
#library("plyr")                                     # Load plyr package
#library("readr") 
#library("data.table")

#registros <- list.files(path = setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),  # Identify all CSV files
#                        pattern = "*.csv", recursive = T, full.names = TRUE) %>% 
#  lapply(fread) %>%                              # Store all files in list
#  bind_rows                                         # Combine data sets into one data set 
#registros  

#detach("package:data.table", unload=TRUE)

#detach("package:plyr", unload=TRUE)
#detach("package:readr", unload=TRUE)

### extract last token (stringi, faster but not working)

#install.packages("stringi")
#library("stringi")

#stri_extract_last(seq2,coll="|")

#stri_extract_last_fixed(seq2,pattern = "|")


### extract last token (stringr)

#install.packages("stringr")
library("stringr")

#word(data_all$`Formas de vida de plantas <span style="color:red">nativas:</span> (amostragem/registro)`,sep = fixed("|"),-1)

#word(data_all$`ponto_metro (amostragem/registro)`,sep = fixed("|"),-1)

### registros_corrig

registros -> registros_corrig

### replace labels to names (erro em uma versao do xlsform, presente em algumas campanhas amostrais)

registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` %>%
  str_replace_all(.,c("Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
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
                      "Outra forma de vida" = "outra"))

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` %>%
  str_replace_all(.,c("Serrapilheira ou folhiço \\(partes de plantas em decomposição no solo\\)" = "serrapilheira",
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
                      "Outra forma de vida" = "outra"))

### atualizacao dos names para o xlsform atual (o name está o mesmo, não é necessário). O código está funcionando:

## graminoides nativas

#registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
#  str_replace_all(registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,c(
#    "[^\\w]graminoide" = " erva_graminoide",
#    "\\sgraminoide" = "erva_graminoide",
#    "(?<!\\w)graminoide" = "erva_graminoide",
#    "bromelioide" = "erva_bromelioide"
#))

## graminoides exoticas

#registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <-
#  str_replace_all(registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,c(
#    "[^\\w]graminoide" = " erva_graminoide",
#    "\\sgraminoide" = "erva_graminoide",
#    "(?<!\\w)graminoide" = "erva_graminoide"
#  ))

## graminoides secas ou mortas

#registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
#  str_replace_all(registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,c(
#    "[^\\w]graminoide" = " erva_graminoide",
#    "\\sgraminoide" = "erva_graminoide",
#    "(?<!\\w)graminoide" = "erva_graminoide"
#  ))

### registros_corrig (keep last token in specific columns)

## ponto amostral

registros_corrig$`ponto_amostral (amostragem/registro)` <- 
  word(registros_corrig$`ponto_amostral (amostragem/registro)`,sep = fixed("|"),-1)

## metro

registros_corrig$`ponto_metro (amostragem/registro)` <- 
  word(registros_corrig$`ponto_metro (amostragem/registro)`,sep = fixed("|"),-1)

## encostam na vareta

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <- 
  word(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,sep = fixed("|"),-1)

## forma de vida de planta nativa

registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <- 
  fifelse(str_detect(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`, "nativa", negate = FALSE),
          word(registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, sep = fixed("|"),-1),
          NA_character_)

## bromélia nativa

registros_corrig$`A bromélia observada é: (amostragem/registro)` <-
fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, "bromelioide", negate = FALSE),
        word(registros_corrig$`A bromélia observada é: (amostragem/registro)`,sep = fixed("|"),-1),
        NA_character_)

## cactacea nativa

registros_corrig$`A cactácea observada é: (amostragem/registro)` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, "cactacea", negate = FALSE),
          word(registros_corrig$`A cactácea observada é: (amostragem/registro)`,sep = fixed("|"),-1),
          NA_character_)

## orquidea nativa

registros_corrig$`A orquídea observada é: (amostragem/registro)` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, "orquidea", negate = FALSE),
          word(registros_corrig$`A orquídea observada é: (amostragem/registro)`,sep = fixed("|"),-1),
          NA_character_)

## forma de vida de planta exótica

registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)` <- 
  fifelse(str_detect(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`, "exot", negate = FALSE),
          word(registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, sep = fixed("|"),-1),
          NA_character_)

## bromélia exótica

registros_corrig$`A bromélia observada é: (amostragem/registro).1` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, "bromelioide", negate = FALSE),
          word(registros_corrig$`A bromélia observada é: (amostragem/registro).1`,sep = fixed("|"),-1),
          NA_character_)

## cactacea exótica

registros_corrig$`A cactácea observada é: (amostragem/registro).1` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, "cactacea", negate = FALSE),
          word(registros_corrig$`A cactácea observada é: (amostragem/registro).1`,sep = fixed("|"),-1),
          NA_character_)

## orquidea exótica

registros_corrig$`A orquídea observada é: (amostragem/registro).2` <-
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, "orquidea", negate = FALSE),
          word(registros_corrig$`A orquídea observada é: (amostragem/registro).2`,sep = fixed("|"),-1),
          NA_character_)

## forma de vida de planta seca ou morta

registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)` <-
  fifelse(str_detect(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`, "seca_morta", negate = FALSE),
          word(registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,sep = fixed("|"),-1),
          NA_character_)

## bromélia seca ou morta

registros_corrig$`A bromélia observada é: (amostragem/registro).2` <-
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`, "bromelioide", negate = FALSE),
          word(registros_corrig$`A bromélia observada é: (amostragem/registro).2`,sep = fixed("|"),-1),
          NA_character_)

## cactacea seca ou morta

registros_corrig$`A cactácea observada é: (amostragem/registro).2` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`, "cactacea", negate = FALSE),
          word(registros_corrig$`A cactácea observada é: (amostragem/registro).2`,sep = fixed("|"),-1),
          NA_character_)

## orquidea seca ou morta

registros_corrig$`A orquídea observada é: (amostragem/registro).2` <- 
  fifelse(str_detect(registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`, "orquidea", negate = FALSE),
          word(registros_corrig$`A orquídea observada é: (amostragem/registro).2`,sep = fixed("|"),-1),
          NA_character_)

## extract year from date

#install.packages("fasttime")
#library("fasttime")

#registros_corrig$ANO <- year(fastDate(registros_corrig$`Data (data_hora)`)) #não funciona com os dados de Taiamã, pois a data está num formato diferente das demais campanhas.
# em taiama ha dois formatos de datas nos csv: dd/mm/yyy e yyyy-mm-dd. Abaixo segue funcao para coercao:

registros_corrig$`Data (data_hora)` <-
  fifelse(str_detect(registros_corrig$`Data (data_hora)`,"([0-9]+(/[0-9]+)+)"),as.Date(registros_corrig$`Data (data_hora)`,"%d/%m/%Y"),as.Date(registros_corrig$`Data (data_hora)`,"%Y-%m-%d"))

registros_corrig$ANO <- format(as.Date(registros_corrig$`Data (data_hora)`,"%Y-%m-%d"),"%Y")

### stat count

library(dplyr)
#install.packages("tidyr")
library(tidyr)

### avançando a partir da solução A:

sum_categ_by_UC_UA_ANO <- registros_corrig %>%
  group_by(.id,UC,UA,ANO,`Coordenada inicial da amostragem (amostragem)`,`Coordenada final da amostragem (amostragem)`) %>% 
  separate_rows(`**Encostam** na vareta: (amostragem/registro)`, sep="\\s+") %>%
  filter(nzchar(`**Encostam** na vareta: (amostragem/registro)`)) %>% 
  dplyr::count(UA, `**Encostam** na vareta: (amostragem/registro)`) %>% 
  spread(`**Encostam** na vareta: (amostragem/registro)`, n, fill = 0)

sum_form_vida_nat_by_UC_UA_ANO <- registros_corrig %>%
  group_by(.id,UC,UA,ANO,`Coordenada inicial da amostragem (amostragem)`,`Coordenada final da amostragem (amostragem)`) %>% 
  separate_rows(`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, sep="\\s+") %>%
  filter(nzchar(`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`)) %>% 
  dplyr::count(UA, `Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`) %>% 
  spread(`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`, n, fill = 0) %>%
  rename_with(~ paste("nativa",., sep = "_"), -(1:6))

sum_form_vida_exot_by_UC_UA_ANO <- registros_corrig %>%
  group_by(.id,UC,UA,ANO,`Coordenada inicial da amostragem (amostragem)`,`Coordenada final da amostragem (amostragem)`) %>% 
  separate_rows(`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, sep="\\s+") %>%
  filter(nzchar(`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`)) %>% 
  dplyr::count(UA, `Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`) %>% 
  spread(`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`, n, fill = 0) %>%
  rename_with(~ paste("exot",., sep = "_"), -(1:6))

sum_form_vida_seca_morta_by_UC_UA_ANO <- registros_corrig %>%
  group_by(.id,UC,UA,ANO,`Coordenada inicial da amostragem (amostragem)`,`Coordenada final da amostragem (amostragem)`) %>% 
  separate_rows(`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`, sep="\\s+") %>%
  filter(nzchar(`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`)) %>% 
  dplyr::count(UA, `Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`) %>% 
  spread(`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`, n, fill = 0) %>%
  rename_with(~ paste("seca_morta",., sep = "_"), -(1:6))

### form_veg:

form_veg <- registros_corrig %>%
  group_by(.id,UC,UA,ANO,`Coordenada inicial da amostragem (amostragem)`,`Coordenada final da amostragem (amostragem)`, form_veg=registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`) %>%
  dplyr::summarise()

### merge data tables:

registros_corrig_stat <- Reduce(function(...) merge(..., all = TRUE, by = c(".id","UC","UA","ANO","Coordenada inicial da amostragem (amostragem)","Coordenada final da amostragem (amostragem)")),
                                list(form_veg, sum_categ_by_UC_UA_ANO,sum_form_vida_nat_by_UC_UA_ANO,sum_form_vida_exot_by_UC_UA_ANO,sum_form_vida_seca_morta_by_UC_UA_ANO))

### tidyr:

library(tidyr)

registros_corrig_stat <- registros_corrig_stat %>%
  separate_wider_delim(cols = `Coordenada inicial da amostragem (amostragem)`,delim=" ",names = c("lat_ini","long_ini","alt_ini","acc_ini")) %>% 
  separate_wider_delim(cols = `Coordenada final da amostragem (amostragem)`,delim=" ",names = c("lat_fin","long_fin","alt_fin","acc_fin"))

### remove NA:

#registros_corrig_stat <- subset(registros_corrig_stat,select=-c(`nativa_<NA>`,`exot_<NA>`,`seca_morta_<NA>`))

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::select(-any_of(c("nativa_<NA>","exot_<NA>","seca_morta_<NA>")))

### create sum_nativa, sum_exotica, sum_seca_morta and remove nativa, exotica, seca_morta correction:

library(tidyverse)

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_nativa = dplyr::select(., which((str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)))) %>% rowSums(na.rm = TRUE),.after=form_veg) %>%
  dplyr::mutate(sum_exotica = dplyr::select(., which((str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)))) %>% rowSums(na.rm = TRUE),.after=sum_nativa) %>%
  dplyr::mutate(sum_seca_morta = dplyr::select(., which((str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)))) %>% rowSums(na.rm = TRUE),.after=sum_exotica) %>%
  dplyr::mutate(serrapilheira = dplyr::select(., which((str_detect(colnames(registros_corrig_stat), "_serrapilheira", negate = FALSE)))) %>% rowSums(na.rm = TRUE),.after=sum_seca_morta) %>%
  dplyr::select(-any_of(c("exotica","nativa","seca_morta","nativa_serrapilheira","exot_serrapilheira","seca_morta_serrapilheira")))
#select_if(!colnames(.) %in% c("exotica","nativa","seca_morta","nativa_serrapilheira","exot_serrapilheira","seca_morta_serrapilheira")) #solucao qnd nao existe alguma coluna

### create sum_herbacea, sum_lenhosa

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_herbacea = select(., c(which(str_detect(colnames(registros_corrig_stat), "_graminoide", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_bambu", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_bromelioide", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_cactacea", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_passarinho", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_orquidea", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_samambaia", negate = FALSE)),
                                           which(str_detect(colnames(registros_corrig_stat), "_canela_de_ema", negate = FALSE)))) %>%
                  rowSums(na.rm = TRUE),.after=form_veg)

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_lenhosa = select(., c(which(str_detect(colnames(registros_corrig_stat), "_arbusto_abaixo", negate = FALSE)),
                                          which(str_detect(colnames(registros_corrig_stat), "_arbusto_acima", negate = FALSE)),
                                          which(str_detect(colnames(registros_corrig_stat), "_arvore_abaixo", negate = FALSE)),
                                          which(str_detect(colnames(registros_corrig_stat), "_arvore_acima", negate = FALSE)),
                                          which(str_detect(colnames(registros_corrig_stat), "_lianas", negate = FALSE)),
                                          which(str_detect(colnames(registros_corrig_stat), "_palmeira", negate = FALSE)))) %>%
                  rowSums(na.rm = TRUE),.after=sum_herbacea)

### remove unnecessary objects:

rm(registros)
rm(sum_categ_by_UC_UA_ANO)
rm(sum_form_vida_nat_by_UC_UA_ANO)
rm(sum_form_vida_exot_by_UC_UA_ANO)
rm(sum_form_vida_seca_morta_by_UC_UA_ANO)
rm(form_veg)

### analysis (dev)

#install.packages("tidyverse")

library("tidyverse")

library(ggplot2)

#ggplot(registros_corrig_stat, aes(x=form_veg, fill = sum_herbacea)) +
#  geom_bar(position = "fill") +
#  stat_count(geom = "text", 
#             aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
#             position=position_fill(vjust=0.5), colour="white")

#registros_corrig_stat_longer <- registros_corrig_stat %>%
#  pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "formas_vida", values_to = "n")

#ggplot(registros_corrig_stat_longer, aes(x=form_veg, fill = n)) +
#  geom_bar(position = "fill") +
#  stat_count(geom = "text", 
#             aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
#             position=position_fill(vjust=0.5), colour="white")

###

#library(scales)
#(p <- ggplot(registros_corrig_stat_longer, aes(form_veg, n, fill = formas_vida)) +
#    geom_bar(position = "fill", stat = "identity") +
#    scale_y_continuous(labels = percent)
#)

### plot herbacea x lenhosa

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "sum_herbacea", negate = FALSE))))) |
   sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "sum_lenhosa", negate = FALSE)))))
   >0) {
  
  reg_herb_lenh <- registros_corrig_stat %>%
    dplyr::select(UC,UA,ANO,form_veg,sum_herbacea,sum_lenhosa) %>%
    pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "categoria", values_to = "n")
  
  library(scales)
  (p1 <- ggplot(reg_herb_lenh, aes(form_veg, n, fill = categoria)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle("Proporção relativa de plantas herbáceas e lenhosas") +
      theme_bw()
  )
}

# reg_corrig_stat_summarise_p1

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "sum_herbacea", negate = FALSE))))) |
   sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "sum_lenhosa", negate = FALSE)))))
   >0) {
  
  reg_corrig_stat_summarise_p1 <- registros_corrig_stat %>%
    dplyr::select(any_of(c("UC","ANO","form_veg","sum_herbacea","sum_lenhosa"))) %>%
    pivot_longer(names_to = "categoria", values_to = "soma", cols = c(sum_herbacea,sum_lenhosa)) %>%
    group_by(UC,ANO,form_veg,categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p1 <- reg_corrig_stat_summarise_p1 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0("(n=",n," , ",scales::percent(prop, accuracy = .1),")")),
              position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de plantas herbáceas e lenhosas") +
    theme_bw()
  
  p1
}

### plot nativa, exotica, seca_morta, serrapilheira, solo_nu

reg_categ_plantas_longer <- registros_corrig_stat %>%
  dplyr::select(any_of(c("UC","UA","ANO","form_veg","sum_nativa","sum_exotica","sum_seca_morta","serrapilheira","solo_nu"))) %>%
  pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "categoria", values_to = "n") %>%
  filter(n>0)

library(scales)
(p2 <- ggplot(reg_categ_plantas_longer, aes(form_veg, n, fill = categoria)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira e solo nu") +
    theme_bw()
)

### reg_corrig_stat_summarise_p2

reg_corrig_stat_summarise_p2 <- registros_corrig_stat %>%
  dplyr::select(any_of(c("UC","ANO","form_veg","sum_nativa","sum_exotica","sum_seca_morta","serrapilheira","solo_nu"))) %>%
  pivot_longer(names_to = "categoria", values_to = "soma", cols = -c("UC","ANO","form_veg")) %>%
  group_by(UC,ANO,form_veg,categoria) %>%
  dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
  dplyr::mutate(prop = prop.table(n)) %>%
  filter(n>0)

p2 <- reg_corrig_stat_summarise_p2 %>%
  ggplot(aes(form_veg, prop, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = paste0("(n=",n," , ",scales::percent(prop, accuracy = .1),")")),
            position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira e solo nu") +
  theme_bw()

p2

### plot formas vida nativa

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)))))>0) {
  
  reg_formas_vida_nat_longer <- registros_corrig_stat %>%
    dplyr::select(UC,UA,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)))) %>%
    pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "categoria", values_to = "n")
  
  library(scales)
  (p3 <- ggplot(reg_formas_vida_nat_longer, aes(form_veg, n, fill = categoria)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle("Proporção relativa de formas de vida e outras categorias de plantas nativas") +
      theme_bw()
  )
}

### reg_corrig_stat_summarise_p3

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)))))>0) {
  
  reg_corrig_stat_summarise_p3 <- registros_corrig_stat %>%
    dplyr::select(.,UC,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE)))) %>%
    pivot_longer(names_to = "categoria", values_to = "soma", cols = -c("UC","ANO","form_veg")) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p3 <- reg_corrig_stat_summarise_p3 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0("(n=",n," , ",scales::percent(prop, accuracy = .1),")")),
              position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas nativas") +
    theme_bw()
  
  p3
}

### plot formas vida exotica

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)))))>0) {
  
  reg_formas_vida_exot_longer <- registros_corrig_stat %>%
    dplyr::select(UC,UA,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)))) %>%
    pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "categoria", values_to = "n")
  
  library(scales)
  (p4 <- ggplot(reg_formas_vida_exot_longer, aes(form_veg, n, fill = categoria)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle("Proporção relativa de formas de vida e outras categorias de plantas exóticas") +
      theme_bw()
  )
}

### reg_corrig_stat_summarise_p4

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)))))>0) {
  
  reg_corrig_stat_summarise_p4 <- registros_corrig_stat %>%
    dplyr::select(.,UC,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE)))) %>%
    pivot_longer(names_to = "categoria", values_to = "soma", cols = -c("UC","ANO","form_veg")) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p4 <- reg_corrig_stat_summarise_p4 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0("(n=",n," , ",scales::percent(prop, accuracy = .1),")")),
              position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas exóticas") +
    theme_bw()
  
  p4
}

### plot formas vida seca ou morta

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)))))>0) {
  
  reg_formas_vida_seca_morta_longer <- registros_corrig_stat %>%
    dplyr::select(UC,UA,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)))) %>%
    pivot_longer(!c(UC,UA,ANO,form_veg), names_to = "categoria", values_to = "n")
  
  library(scales)
  (p5 <- ggplot(reg_formas_vida_seca_morta_longer, aes(form_veg, n, fill = categoria)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent) +
      ggtitle("Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas") +
      theme_bw()
  )
}

### reg_corrig_stat_summarise_p5

if(sum(select(.data=registros_corrig_stat,which((str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)))))>0) {
  
  reg_corrig_stat_summarise_p5 <- registros_corrig_stat %>%
    dplyr::select(.,UC,ANO,form_veg,which((str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE)))) %>%
    pivot_longer(names_to = "categoria", values_to = "soma", cols = -c("UC","ANO","form_veg")) %>%
    group_by(UC, ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p5 <- reg_corrig_stat_summarise_p5 %>%
    ggplot(aes(form_veg, prop, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0("(n=",n," , ",scales::percent(prop, accuracy = .1),")")),
              position = position_stack(vjust = .5)) +
    scale_y_continuous(labels = percent) +
    ggtitle("Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas") +
    theme_bw()
  
  p5
}

### dev: percent calc


### exportar csv

#path <- choose.dir()
fwrite(registros_corrig_stat, file.path("registros_corrig_stat.csv"), row.names=FALSE)

