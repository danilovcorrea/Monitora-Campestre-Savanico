### Script de análise de dados do Alvo Global do Componente Campestre Savânico do Programa Monitora.

### O script CAMPSAV_Alvo_Global_all_forms.R analisa registros obtidos a partir de todos os formulários já
### utilizados no ambiente de produção do SISMONITORA: 11AGO22,05MAI23,03MAI24.
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
### 6 - Os gráficos gerados podem ser exportados em formato .png ou .pregistros_corrig_stat no menu "Export" na aba "plots";

### Verificação e download dos pacotes necessários:

if (!require("rstudioapi"))
  install.packages("rstudioapi")
library("rstudioapi")
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
# if (!require("ggtext"))
#   install.packages("ggtext")
# library("ggtext")
# if (!require("rio"))
#   install.packages("rio")
# library("rio")
if (!require("readxl"))
  install.packages("readxl")
library("readxl")
if (!require("openxlsx"))
  install.packages("openxlsx")
library("openxlsx")
if (!require("sf"))
  install.packages("sf")
library("sf")


### Especificação do diretório de trabalho como o diretório onde está o script

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

# Load necessary library
library(readxl)

# List all .xlsx files in the directory
xlsx_files <- list.files(pattern = "\\.xlsx$", recursive = T, full.names = TRUE)

# Loop through each .xlsx file
for (xlsx_file in xlsx_files) {
  # Create the corresponding .csv filename
  csv_file <- sub("\\.xlsx$", ".csv", xlsx_file)
  
  # Check if the .csv file already exists
  if (!file.exists(csv_file)) {
    # Read the .xlsx file into a data frame
    data <- read_excel(xlsx_file)
    
    # Write the data frame to a .csv file
    write.csv(data, file = csv_file, row.names = FALSE)
    cat("Converted:", xlsx_file, "to", csv_file, "\n")
  } else {
    cat("CSV file already exists:", csv_file, "\n")
  }
}

rm(data, csv_file, xlsx_file, xlsx_files)

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

# Check if '.id' exists in the 'registros' data.table

if (!".id" %in% colnames(registros)) {
  # If '.id' doesn't exist, create it and assign NA
  registros[, .id := NA]
}

registros[, .id := factor(.id, labels = basename(csvfiles))]

setnames(registros, make.unique(names(registros)))

rm(csvfiles)

### criação do arquivo onde serão realizadas as correções, mantendo o arquivo original:

registros -> registros_corrig

###

names(registros_corrig) <- str_replace_all(names(registros_corrig), '\\"{4}', '\\"\"')

###

### rename columns from script downloaded xlsx to directly downloaded SISMONITORA.csv

if (".id" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == ".id"] <- ".id"
}
if ("registro_uuid" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "registro_uuid"] <- "UUID"
}
if ("coleta" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "coleta"] <- "COLETA"
}
if ("data do registro" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "data do registro"] <- "DATA DO REGISTRO"
}
if ("data do recebimento" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "data do recebimento"] <- "DATA DO RECEBIMENTO"
}
if ("uc" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "uc"] <- "UC"
}
if ("ciclo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "ciclo"] <- "CICLO"
}
if ("campanha" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "campanha"] <- "CAMPANHA"
}
if ("protocolo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "protocolo"] <- "PROTOCOLO"
}
if ("ea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "ea"] <- "EA"
}
if ("ua" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "ua"] <- "UA"
}
if ("usuario" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "usuario"] <- "USUARIO"
}
if ("coletores" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "coletores"] <- "COLETORES"
}
if ("validado" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "validado"] <- "VALIDADO"
}
if ("validado por" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "validado por"] <- "VALIDADO POR"
}
if ("data validacao" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "data validacao"] <- "DATA VALIDAÇÃO"
}
if ("data_hora/data" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "data_hora/data"] <- "Data (data_hora)"
}
if ("data_hora/hora" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "data_hora/hora"] <- "Horário (data_hora)"
}
if ("form_veg" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "form_veg"] <- "Qual a formação vegetacional onde está situado o transecto?"
}
if ("impact_manejo_uso/impacto_manejo_uso" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "impact_manejo_uso/impacto_manejo_uso"] <- "Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)"
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "impact_manejo_uso/tipos_impacto_manejo_uso"] <- "Qual(is)? (impact_manejo_uso)"
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso_outro" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "impact_manejo_uso/tipos_impacto_manejo_uso_outro"] <- "Outros tipos de manejo ou uso: (impact_manejo_uso)"
}
if ("impact_manejo_uso/tipos_impacto_manejo_uso_descricao" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "impact_manejo_uso/tipos_impacto_manejo_uso_descricao"] <- "Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)"
}
if ("observacoes_gerais" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "observacoes_gerais"] <- "Descreva observações gerais do transecto, caso necessário:"
}
if ("amostragem/ponto_inicio_transecto" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/ponto_inicio_transecto"] <- "Coordenada inicial da amostragem (amostragem)"
}
if ("amostragem/foto_ponto_inicial" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/foto_ponto_inicial"] <- "Foto do ponto inicial do transecto (amostragem)"
}
if ("amostragem/num_placa" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/num_placa"] <- "Número da plaqueta (amostragem)"
}
if ("amostragem/modulo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/modulo"] <- "Módulo (amostragem)"
}
if ("amostragem/registro/ponto_amostral" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/ponto_amostral"] <- "ponto_amostral (amostragem/registro)"
}
if ("amostragem/registro/ponto_metro" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/ponto_metro"] <- "ponto_metro (amostragem/registro)"
}
if ("amostragem/registro/tipo_forma_vida" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/tipo_forma_vida"] <- "**Encostam** na vareta: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa"] <- "Formas de vida de plantas <span style=\"\"color:red\"\">nativas:</span> (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_bromelioide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_bromelioide"] <- "A erva bromelioide observada é: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_bromelioide_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_bromelioide_sp"] <- "Espécie ou nome popular (Erva bromelioide) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_cactacea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_cactacea"] <- "A cactácea observada é: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_cactacea_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_cactacea_sp"] <- "Espécie ou nome popular (Cactácea) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_orquidea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_orquidea"] <- "A orquídea observada é: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_orquidea_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_orquidea_sp"] <- "Espécie ou nome popular (Orquídea) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_outra"] <- "Outra forma de vida de planta nativa: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_nativa_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_nativa_outra"] <- "Foto de outra forma de vida de planta nativa: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_nativa_desconhecida" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_nativa_desconhecida"] <- "Foto da forma de vida desconhecida de planta nativa: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_graminoide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_graminoide"] <- "Espécie ou nome popular (Erva graminoide) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_erva_nao_graminoide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_erva_nao_graminoide"] <- "Espécie ou nome popular (Erva não graminoide) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_arbusto_abaixo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_arbusto_abaixo"] <- "Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_arbusto_acima" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_arbusto_acima"] <- "Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_arvore_abaixo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_arvore_abaixo"] <- "Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_arvore_acima" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_arvore_acima"] <- "Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_bambu" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_bambu"] <- "Espécie ou nome popular (Bambu) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_lianas" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_lianas"] <- "Espécie ou nome popular (Lianas) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_ervas_de_passarinho" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_ervas_de_passarinho"] <- "Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_palmeira" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_palmeira"] <- "Espécie ou nome popular (Palmeira) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_samambaia" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_samambaia"] <- "Espécie ou nome popular (Samambaia) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_nativa_canela_de_ema" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_nativa_canela_de_ema"] <- "Espécie ou nome popular (Velósia) (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_exotica" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_exotica"] <- "Formas de vida de plantas <span style=\"\"color:red\"\">exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_graminoide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_graminoide"] <- "**Espécies** de <span style=\"\"color:red\"\"> graminóides exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_erva_nao_graminoide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_erva_nao_graminoide"] <- "**Espécies** de <span style=\"\"color:red\"\"> ervas não graminóides exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_arbusto_abaixo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_arbusto_abaixo"] <- "**Espécies** de <span style=\"\"color:red\"\"> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_arbusto_acima" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_arbusto_acima"] <- "**Espécies** de <span style=\"\"color:red\"\"> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_arvore_abaixo" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_arvore_abaixo"] <- "**Espécies** de <span style=\"\"color:red\"\"> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_arvore_acima" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_arvore_acima"] <- "**Espécies** de <span style=\"\"color:red\"\"> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_bambu" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_bambu"] <- "**Espécies** de <span style=\"\"color:red\"\"> bambus exóticos:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_cactacea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_cactacea"] <- "**Espécies** de <span style=\"\"color:red\"\"> cactáceas exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_lianas" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_lianas"] <- "**Espécies** de <span style=\"\"color:red\"\"> lianas exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_orquidea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_orquidea"] <- "**Espécies** de <span style=\"\"color:red\"\"> orquídeas exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_palmeira" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_palmeira"] <- "**Espécies** de <span style=\"\"color:red\"\"> palmeiras exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_samambaia" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_samambaia"] <- "**Espécies** de <span style=\"\"color:red\"\"> samambaias exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/especies_exotica_outros" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/especies_exotica_outros"] <- "**Espécies** de <span style=\"\"color:red\"\"> outros exóticas:</span> (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_exotica_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_exotica_outra"] <- "Outra forma de vida de planta exótica: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_exotica_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_exotica_outra"] <- "Foto de outra forma de vida de planta exótica: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_exotica_desconhecida" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_exotica_desconhecida"] <- "Foto da forma de vida desconhecida de planta exótica: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_exotica_bromelioide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_exotica_bromelioide"] <- "A erva bromelioide observada é: (amostragem/registro).1"
}
if ("amostragem/registro/forma_vida_exotica_cactacea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_exotica_cactacea"] <- "A cactácea observada é: (amostragem/registro).1"
}
if ("amostragem/registro/forma_vida_exotica_orquidea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_exotica_orquidea"] <- "A orquídea observada é: (amostragem/registro).1"
}
if ("amostragem/registro/exotica_graminoide_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_graminoide_outra_sp"] <- "Outra espécie de erva graminoide exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_erva_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_erva_outra_sp"] <- "Outra espécie de erva não graminoide exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_arbusto_abaixo_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_arbusto_abaixo_outra_sp"] <- "Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)"
}
if ("amostragem/registro/exotica_arbusto_acima_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_arbusto_acima_outra_sp"] <- "Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)"
}
if ("amostragem/registro/exotica_arvore_abaixo_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_arvore_abaixo_outra_sp"] <- "Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)"
}
if ("amostragem/registro/exotica_arvore_acima_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_arvore_acima_outra_sp"] <- "Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)"
}
if ("amostragem/registro/exotica_bambu_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_bambu_outra_sp"] <- "Outra espécie de bambu exótico: (amostragem/registro)"
}
if ("amostragem/registro/exotica_cactacea_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_cactacea_outra_sp"] <- "Outra espécie de cactácea exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_orquidea_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_orquidea_outra_sp"] <- "Outra espécie de orquídea exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_palmeira_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_palmeira_outra_sp"] <- "Outra espécie de palmeira exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_samambaia_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_samambaia_outra_sp"] <- "Outra espécie de samambaia exótica: (amostragem/registro)"
}
if ("amostragem/registro/exotica_outros_outra_sp" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/exotica_outros_outra_sp"] <- "Outra espécie exótica: (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_seca_morta" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_seca_morta"] <- "Formas de vida de plantas <span style=\"\"color:red\"\">secas ou mortas:</span> (amostragem/registro)"
}
if ("amostragem/registro/forma_vida_seca_morta_bromelioide" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_seca_morta_bromelioide"] <- "A erva bromelioide observada é: (amostragem/registro).2"
}
if ("amostragem/registro/forma_vida_seca_morta_cactacea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_seca_morta_cactacea"] <- "A cactácea observada é: (amostragem/registro).2"
}
if ("amostragem/registro/forma_vida_seca_morta_orquidea" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_seca_morta_orquidea"] <- "A orquídea observada é: (amostragem/registro).2"
}
if ("amostragem/registro/forma_vida_seca_morta_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/forma_vida_seca_morta_outra"] <- "Outra forma de vida de planta seca e/ou morta: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_seca_morta_outra" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_seca_morta_outra"] <- "Foto de outra forma de vida de planta seca ou morta: (amostragem/registro)"
}
if ("amostragem/registro/foto_forma_vida_seca_morta_desconhecida" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/foto_forma_vida_seca_morta_desconhecida"] <- "Foto da forma de vida desconhecida de planta seca ou morta: (amostragem/registro)"
}
if ("amostragem/registro/observacao" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/observacao"] <- "Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)"
}
if ("amostragem/registro/uuid" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/registro/uuid"] <- "uuid (amostragem/registro)"
}
if ("amostragem/ponto_fim_transecto" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/ponto_fim_transecto"] <- "Coordenada final da amostragem (amostragem)"
}
if ("amostragem/foto_ponto_final" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "amostragem/foto_ponto_final"] <- "Foto do ponto final do transecto (amostragem)"
}
if ("uuid" %in% colnames(registros_corrig)) {
  colnames(registros_corrig)[colnames(registros_corrig) == "uuid"] <- "uuid"
}

### merge duplicated columns names:

# Identify duplicate column names
dups <- names(registros_corrig)[duplicated(names(registros_corrig))]

# Loop over each duplicated column name and merge them (concatenate non-NA values)
for (col in unique(dups)) {
  # Get indices of all columns named `col`
  cols <- which(names(registros_corrig) == col)
  
  # Merge non-NA values (concatenate with "_")
  registros_corrig[, (col) := apply(.SD, 1, function(x)
    paste(na.omit(x), collapse = "_")), .SDcols = cols]
  
  # Remove all duplicate columns (those with the same name)
  registros_corrig[, (cols[-1]) := NULL]
}

rm(col, cols, dups)

### rename columns values from script downloaded .xlsx to directly downloaded SISMONITORA.csv

if ("campestre" %in% registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`) {
  registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`[registros_corrig$`Qual a formação vegetacional onde está situado o transecto?` ==
                                                                                   "campestre"] <- "Campestre"
}
if ("savanica" %in% registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`) {
  registros_corrig$`Qual a formação vegetacional onde está situado o transecto?`[registros_corrig$`Qual a formação vegetacional onde está situado o transecto?` ==
                                                                                   "savanica"] <- "Savânica"
}

### labels to names correction

## 16mar25

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "Plantas nativas" = "nativa",
      "Plantas nativas," = "nativa",
      "Plantas exóticas" = "exotica",
      "Plantas exóticas," = "exotica",
      "Plantas secas ou mortas" = "seca_morta",
      "Plantas secas ou mortas," = "seca_morta",
      "Solo nu / rochas \\(sem plantas tocando a vareta\\)" = "solo_nu",
      "Solo nu / rochas \\(sem plantas tocando a vareta\\)," = "solo_nu",
      "Material botânico em decomposição no solo" = "serrapilheira",
      "Material botânico em decomposição no solo," = "serrapilheira",
      "Outras plantas terrestres, líquens e/ou fungos" = "solo_nu",
      "Outras plantas terrestres, líquens e/ou fungos," = "solo_nu"
      )
  )

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  registros_corrig$`**Encostam** na vareta: (amostragem/registro)` %>%
  str_replace_all(
    .,
    c(
      "nativa," = "nativa",
      "exotica," = "exotica",
      "seca_morta," = "seca_morta",
      "serrapilheira," = "serrapilheira",
      "solo_nu," = "solo_nu"
    )
  )

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
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##07mar25
      
      ##correção "label"
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##correção "label " (label seguido de espaço)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ##correção "label," (label seguido de vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ##correção "label ," (label seguido de espaço e vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ##correção name (16abr25)
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ##correção "name," (name seguido de vírgula)
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ##correção "name ," (name seguido de espaço e vírgula)
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
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
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##07mar25
      
      ##correção "label"
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##correção "label " (label seguido de espaço)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ##correção "label," (label seguido de vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ##correção "label ," (label seguido de espaço e vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ##correção name (16abr25)
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ##correção "name," (name seguido de vírgula)
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ##correção "name ," (name seguido de espaço e vírgula)
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
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
      "Bromelioide \\(bromélias e apiáceas\\)" = "erva_bromelioide",
      "Cactácea" = "cactacea",
      "Lianas \\(cipós, trepadeiras\\)" = "lianas",
      "Erva-de-passarinho \\(parasitas\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Canela-de-ema ou candombá\\)" = "canela_de_ema",
      "Outra forma de vida" = "outra",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##07mar25
      
      ##correção "label"
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)" = "graminoide",
      "Erva não graminoide" = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm" = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm" = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)" = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)" = "arvore_acima",
      "Bambu ou taquara" = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)" = "erva_bromelioide",
      "Cacto" = "cactacea",
      "Liana, cipó ou trepadeira" = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)" = "ervas_de_passarinho",
      "Orquídea" = "orquidea",
      "Palmeira" = "palmeira",
      "Samambaia" = "samambaia",
      "Velósia \\(Velloziaceae\\)" = "canela_de_ema",
      "Forma de vida desconhecida" = "desconhecida",
      
      ##correção "label " (label seguido de espaço)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) " = "graminoide",
      "Erva não graminoide " = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm " = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm " = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) " = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) " = "arvore_acima",
      "Bambu ou taquara " = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) " = "erva_bromelioide",
      "Cacto " = "cactacea",
      "Liana, cipó ou trepadeira " = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) " = "ervas_de_passarinho",
      "Orquídea " = "orquidea",
      "Palmeira " = "palmeira",
      "Samambaia " = "samambaia",
      "Velósia \\(Velloziaceae\\) " = "canela_de_ema",
      "Forma de vida desconhecida " = "desconhecida",
      
      ##correção "label," (label seguido de vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\)," = "graminoide",
      "Erva não graminoide," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\)," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\)," = "arvore_acima",
      "Bambu ou taquara," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\)," = "erva_bromelioide",
      "Cacto," = "cactacea",
      "Liana, cipó ou trepadeira," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\)," = "ervas_de_passarinho",
      "Orquídea," = "orquidea",
      "Palmeira," = "palmeira",
      "Samambaia," = "samambaia",
      "Velósia \\(Velloziaceae\\)," = "canela_de_ema",
      "Forma de vida desconhecida," = "desconhecida",
      
      ##correção "label ," (label seguido de espaço e vírgula)
      
      "Erva graminoide \\(gramíneas, ciperáceas e juncáceas\\) ," = "graminoide",
      "Erva não graminoide ," = "erva_nao_graminoide",
      "Arbusto tocando a vareta a uma altura inferior a 50cm ," = "arbusto_abaixo",
      "Arbusto tocando a vareta a uma altura igual ou superior a 50cm ," = "arbusto_acima",
      "Árvore com diâmetro do tronco menor que 5cm a 30cm do solo \\(D30\\) ," = "arvore_abaixo",
      "Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo \\(D30\\) ," = "arvore_acima",
      "Bambu ou taquara ," = "bambu",
      "Erva bromelioide \\(bromeliáceas, apiáceas, eriocauláceas\\) ," = "erva_bromelioide",
      "Cacto ," = "cactacea",
      "Liana, cipó ou trepadeira ," = "lianas",
      "Erva-de-passarinho \\(hemiparasita\\) ," = "ervas_de_passarinho",
      "Orquídea ," = "orquidea",
      "Palmeira ," = "palmeira",
      "Samambaia ," = "samambaia",
      "Velósia \\(Velloziaceae\\) ," = "canela_de_ema",
      "Forma de vida desconhecida ," = "desconhecida",
      
      ##correção name (16abr25)
      
      "(?<!erva_)bromelioide" = "erva_bromelioide",
      
      ##correção "name," (name seguido de vírgula)
      
      "graminoide," = "graminoide",
      "erva_nao_graminoide," = "erva_nao_graminoide",
      "arbusto_abaixo," = "arbusto_abaixo",
      "arbusto_acima," = "arbusto_acima",
      "arvore_abaixo," = "arvore_abaixo",
      "arvore_acima," = "arvore_acima",
      "bambu," = "bambu",
      "(?<!erva_)bromelioide," = "erva_bromelioide",
      "erva_bromelioide," = "erva_bromelioide",
      "cactacea," = "cactacea",
      "lianas," = "lianas",
      "ervas_de_passarinho," = "ervas_de_passarinho",
      "orquidea," = "orquidea",
      "palmeira," = "palmeira",
      "samambaia," = "samambaia",
      "canela_de_ema," = "canela_de_ema",
      "desconhecida," = "desconhecida",
      
      ##correção "name ," (name seguido de espaço e vírgula)
      
      "graminoide ," = "graminoide",
      "erva_nao_graminoide ," = "erva_nao_graminoide",
      "arbusto_abaixo ," = "arbusto_abaixo",
      "arbusto_acima ," = "arbusto_acima",
      "arvore_abaixo ," = "arvore_abaixo",
      "arvore_acima ," = "arvore_acima",
      "bambu ," = "bambu",
      "(?<!erva_)bromelioide ," = "erva_bromelioide",
      "erva_bromelioide ," = "erva_bromelioide",
      "cactacea ," = "cactacea",
      "lianas ," = "lianas",
      "ervas_de_passarinho ," = "ervas_de_passarinho",
      "orquidea ," = "orquidea",
      "palmeira ," = "palmeira",
      "samambaia ," = "samambaia",
      "canela_de_ema ," = "canela_de_ema",
      "desconhecida ," = "desconhecida"
    )
  )

# registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` <-
#   registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)` %>%
#   str_replace_all(

### extração do último token em colunas específicas (o SISMONITORA exporta a lista concatenada por "|")

## ponto amostral

registros_corrig$`ponto_amostral (amostragem/registro)` <-
  word(registros_corrig$`ponto_amostral (amostragem/registro)`,
       sep = fixed("|"),
       -1)

## metro

registros_corrig$`ponto_metro (amostragem/registro)` <-
  word(registros_corrig$`ponto_metro (amostragem/registro)`,
       sep = fixed("|"),
       -1)

## encostam na vareta

registros_corrig$`**Encostam** na vareta: (amostragem/registro)` <-
  word(registros_corrig$`**Encostam** na vareta: (amostragem/registro)`,
       sep = fixed("|"),
       -1)

### descrição do ponto amostral (corrigido)

if (is.null(registros_corrig[['Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)']]))
  set(registros_corrig, j = 'Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)', value = NA_character_)

# nº tokens e último token
obs_pa_tokens <- registros_corrig[, .(
  Tokens = lapply(strsplit(`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`, "\\|"), length),
  Last_token = lapply(strsplit(`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`, "\\|"), function(x) if(length(x) > 0) tail(x, 1) else NA)
), by = .(COLETA, `ponto_amostral (amostragem/registro)`)]

# nº max tokens
obs_pa_tokens[, MaxTokens_coleta := max(unlist(Tokens)), by = .(COLETA)]

# token válido
obs_pa_tokens[, `Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` := ifelse(Tokens == MaxTokens_coleta, Last_token, NA_character_)]

# atualização tokens válidos em registros_corrig
registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` <- obs_pa_tokens$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`

rm(obs_pa_tokens)

# remocao line breaks (\r , \n) nos campos descritivos AND COLETORES AND PUNCT IN PLAQUETA

registros_corrig$`Número da plaqueta (amostragem)` <-
  str_replace_all(
    registros_corrig$`Número da plaqueta (amostragem)`,
    "[[:punct:]]",
    " "
  )

registros_corrig$COLETORES <-
  str_replace_all(
    registros_corrig$COLETORES,
    "[\r\n]",
    " "
  )
    
registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)` <-
  str_replace_all(
    registros_corrig$`Descreva observações gerais do ponto amostral, caso necessário: (amostragem/registro)`,
    "[\r\n]",
    " "
  )

registros_corrig$`Descreva observações gerais do transecto, caso necessário:` <-
  str_replace_all(
    registros_corrig$`Descreva observações gerais do transecto, caso necessário:`,
    "[\r\n]",
    " "
  )

registros_corrig$`Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)` <-
  str_replace_all(
    registros_corrig$`Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)`,
    "[\r\n]",
    " "
  )

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
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

# ## remover após teste
# 
# registros_corrig$PROTOCOLO <- registros_corrig$PROTOCOLO %>% str_replace_all(c(
#   "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22_2 Básico e Avançado" = "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
#   "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23_2 Básico e Avançado" = "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
#   "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24_2 Básico e Avançado" = "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado"))

## erva bromelioide nativa

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      )
    ),default = NA_character_
  )

## cactacea nativa

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro)' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro)',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea nativa

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro)']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro)', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro)` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro)`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro)`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
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
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## erva bromelioide exótica

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro).1`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      )
    )
  )

## cactacea exótica

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro)' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro).1',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea exótica

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro).1', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro).1']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro).1', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro).1` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro).1`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro).1`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
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
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## erva bromelioide seca ou morta

if (is.null(registros_corrig[['Selecione se a bromélia observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'Selecione se a bromélia observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A bromélia observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A bromélia observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A erva bromelioide observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A erva bromelioide observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a bromélia observada é: (amostragem/registro).2`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A bromélia observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "bromelioide",
        negate = FALSE
      ),
      word(
        registros_corrig$`A erva bromelioide observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      )
    )
  )

## cactacea seca ou morta

if (is.null(registros_corrig[['A cactácea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A cactácea observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$'A cactácea observada é: (amostragem/registro).2' <-
  fifelse(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$'A cactácea observada é: (amostragem/registro).1',
      sep = fixed("|"),
      -1
    ),
    NA_character_
  )

## orquídea seca ou morta

if (is.null(registros_corrig[['Selecione se a orquidea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'Selecione se a orquidea observada é: (amostragem/registro).2', value = NA_character_)

if (is.null(registros_corrig[['A orquídea observada é: (amostragem/registro).2']]))
  set(registros_corrig, j = 'A orquídea observada é: (amostragem/registro).2', value = NA_character_)

registros_corrig$`A orquídea observada é: (amostragem/registro).2` <-
  fifelse(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`Selecione se a orquidea observada é: (amostragem/registro).2`,
        sep = fixed("|"),-1
      ),
      NA_character_
    ),
    fifelse(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
        "orquidea",
        negate = FALSE
      ),
      word(
        registros_corrig$`A orquídea observada é: (amostragem/registro).2`,
        sep = fixed("|"),
        -1
      ),
      NA_character_
    ),
    NA_character_
  )

### `Outra forma de vida de planta nativa: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta nativa: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta nativa: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta nativa: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta nativa: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### `Outra forma de vida de planta exótica: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### `Outra forma de vida de planta seca e/ou morta: (amostragem/registro)`

if (is.null(registros_corrig[['Outra forma de vida de planta seca e/ou morta: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra forma de vida de planta seca e/ou morta: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra forma de vida de planta seca e/ou morta: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">secas ou mortas:</span> (amostragem/registro)`,
      "outra",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra forma de vida de planta seca e/ou morta: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

### Protocolo avançado: extração do último token dos atributos de espécies

## Espécies nativas

# Nativa Espécie ou nome popular (Erva graminoide)

if (is.null(registros_corrig[['Espécie ou nome popular (Graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Graminoide) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva graminoide) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
      fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "graminoide",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Erva graminoide) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Erva não graminoide)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva não graminoide) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva não graminoide) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva não graminoide) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "erva_nao_graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Erva não graminoide) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto abaixo de 0,5m de altura) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura inferior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto acima de 0,5m de altura) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Arbusto tocando a vareta a uma altura igual ou superior a 50cm) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30))

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)` <- 
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore abaixo de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco menor que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30))

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore acima de 5cm de diâmetro a 30 cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`Espécie ou nome popular (Árvore com diâmetro do tronco igual ou maior que 5cm a 30cm do solo (D30)) (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# Nativa Espécie ou nome popular (Bambu)

if (is.null(registros_corrig[['Espécie ou nome popular (Bambu) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Bambu) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Bambu) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "bambu",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Bambu) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Lianas)

if (is.null(registros_corrig[['Espécie ou nome popular (Lianas) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Lianas) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Lianas) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "lianas",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Lianas) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Erva-de-passarinho)

if (is.null(registros_corrig[['Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "ervas_de_passarinho",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Erva-de-passarinho) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Espécie ou nome popular (Palmeira)

if (is.null(registros_corrig[['Espécie ou nome popular (Palmeira) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Palmeira) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Palmeira) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "palmeira",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Palmeira) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Samambaia)

if (is.null(registros_corrig[['Espécie ou nome popular (Samambaia) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Samambaia) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Samambaia) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "samambaia",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Samambaia) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# Nativa Espécie ou nome popular (Velósia)

if (is.null(registros_corrig[['Espécie ou nome popular (Velósia) (amostragem/registro)']]))
  set(registros_corrig, j = 'Espécie ou nome popular (Velósia) (amostragem/registro)', value = NA_character_)

registros_corrig$`Espécie ou nome popular (Velósia) (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">nativas:</span> (amostragem/registro)`,
      "canela_de_ema",
      negate = FALSE
    ),
    word(
      registros_corrig$`Espécie ou nome popular (Velósia) (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

## espécies exoticas

# especies_exotica_graminoide

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_erva_nao_graminoide

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "erva_nao_graminoide",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_arbusto_abaixo

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> abaixo de 0,5m de altura: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# especies_exotica_arbusto_acima

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> acima de 0,5m de altura: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arbusto_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# especies_exotica_arvore_abaixo

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> abaixo de 5cm diâmetro: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_abaixo",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# especies_exotica_arvore_acima

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)', value = NA_character_)

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)` <-
  fcase(
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_11AGO22 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> acima de 5cm diâmetro: (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_05MAI23 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    ),
    registros_corrig$PROTOCOLO == "PLANTASHERBACEASELENHOSAS_CAMPSAV_03MAI24 Básico e Avançado",
    fcase(
      str_detect(
        registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
        "arvore_acima",
        negate = FALSE
      ),
      word(
        registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
        sep = fixed("|"),-1
      )
    )
  )

# especies_exotica_bambu

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "bambu",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_cactacea

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "cactacea",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_lianas

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "lianas",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> lianas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_orquidea

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "orquidea",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_palmeira

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "palmeira",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# especies_exotica_samambaia

if (is.null(registros_corrig[['**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)']]))
  set(registros_corrig, j = '**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)', value = NA_character_)

registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`Formas de vida de plantas <span style=""color:red"">exóticas:</span> (amostragem/registro)`,
      "samambaia",
      negate = FALSE
    ),
    word(
      registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

## especies_exotica_outros

# exotica_graminoide_outra_sp

if (is.null(registros_corrig[['Outra espécie de erva graminoide exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de erva graminoide exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de erva graminoide exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> graminóides exóticas:</span> (amostragem/registro)`,
      "exotica_graminoide_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de erva graminoide exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_erva_nao_graminoide_outra_sp

if (is.null(registros_corrig[['Outra espécie de erva não graminoide exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de erva não graminoide exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de erva não graminoide exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> ervas não graminóides exóticas:</span> (amostragem/registro)`,
      "exotica_erva_nao_graminoide_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de erva não graminoide exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arbusto_abaixo_outra_sp

if (is.null(registros_corrig[['Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
      "exotica_arbusto_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma altura inferior a 50cm: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arbusto_acima_outra_sp

if (is.null(registros_corrig[['Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> arbustos exóticos</span> tocando a vareta a uma altura igual ou superior a 50cm: (amostragem/registro)`,
      "exotica_arbusto_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de arbusto exótico tocando a vareta a uma igual ou superior a 50cm: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arvore_abaixo_outra_sp

if (is.null(registros_corrig[['Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
      "exotica_arvore_abaixo_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco menor que 5cm a 30cm do solo (D30): (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_arvore_acima_outra_sp

if (is.null(registros_corrig[['Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> árvores exóticas</span> com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
      "exotica_arvore_acima_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de árvore exótica com diâmetro do tronco igual ou maior que 5cm a 30 cm do solo(D30): (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_bambu_outra_sp

if (is.null(registros_corrig[['Outra espécie de bambu exótico: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de bambu exótico: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de bambu exótico: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> bambus exóticos:</span> (amostragem/registro)`,
      "exotica_bambu_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de bambu exótico: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_cactacea_outra_sp

if (is.null(registros_corrig[['Outra espécie de cactácea exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de cactácea exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de cactácea exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> cactáceas exóticas:</span> (amostragem/registro)`,
      "exotica_cactacea_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de cactácea exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_orquidea_outra_sp

if (is.null(registros_corrig[['Outra espécie de orquídea exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de orquídea exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de orquídea exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> orquídeas exóticas:</span> (amostragem/registro)`,
      "exotica_orquidea_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de orquídea exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )


# exotica_palmeira_outra_sp

if (is.null(registros_corrig[['Outra espécie de palmeira exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de palmeira exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de palmeira exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> palmeiras exóticas:</span> (amostragem/registro)`,
      "exotica_palmeira_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de palmeira exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

# exotica_samambaia_outra_sp

if (is.null(registros_corrig[['Outra espécie de samambaia exótica: (amostragem/registro)']]))
  set(registros_corrig, j = 'Outra espécie de samambaia exótica: (amostragem/registro)', value = NA_character_)

registros_corrig$`Outra espécie de samambaia exótica: (amostragem/registro)` <-
  fcase(
    str_detect(
      registros_corrig$`**Espécies** de <span style=""color:red""> samambaias exóticas:</span> (amostragem/registro)`,
      "exotica_samambaia_outra_sp",
      negate = FALSE
    ),
    word(
      registros_corrig$`Outra espécie de samambaia exótica: (amostragem/registro)`,
      sep = fixed("|"),-1
    )
  )

##07mar25: comment exotica_outros_outra_sp

# # exotica_outros_outra_sp
# 
# if (is.null(registros_corrig[['Outra espécie exótica: (amostragem/registro)']]))
#   set(registros_corrig, j = 'Outra espécie exótica: (amostragem/registro)', value = NA_character_)
# 
# registros_corrig$`Outra espécie exótica: (amostragem/registro)` <-
#   fcase(
#     str_detect(
#       registros_corrig$`**Espécies** de <span style=""color:red""> outros exóticas:</span> (amostragem/registro)`,
#       "exotica_outros_outra_sp",
#       negate = FALSE
#     ),
#     word(
#       registros_corrig$`Outra espécie exótica: (amostragem/registro)`,
#       sep = fixed("|"),-1
#     )
#   )

### extração do ANO a partir da DATA. O SISMONITORA exportou datas em diferentes formatos. A função
### a seguir faz a coerção dos formatos identificados

registros_corrig[, ANO := {
  d1 <- as.IDate(`Data (data_hora)`, format = "%m/%d/%Y")
  d2 <- as.IDate(`Data (data_hora)`, format = "%d/%m/%Y")
  d3 <- as.IDate(`Data (data_hora)`, format = "%Y-%m-%d")
  as.character(year(fcoalesce(d1, d2, d3)))
}]

### correct incorrect coordinates delimiters

# Replace all incorrect delimiters with a single space
registros_corrig$`Coordenada inicial da amostragem (amostragem)` <- gsub(
  "\\s*,\\s*|\\s*,|,\\s*",
  " ",
  registros_corrig$`Coordenada inicial da amostragem (amostragem)`
)
registros_corrig$`Coordenada final da amostragem (amostragem)` <- gsub(
  "\\s*,\\s*|\\s*,|,\\s*",
  " ",
  registros_corrig$`Coordenada final da amostragem (amostragem)`
)

### construção das tabelas estatísticas

## somatório de categorias por UC, UA, ANO

sum_categ_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    PROTOCOLO,
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
    PROTOCOLO,
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
  rename_with(~ paste("nativa", ., sep = "_"), -(1:7))

## somatório de formas de vida exóticas por UC, UA, ANO

sum_form_vida_exot_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    PROTOCOLO,
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
  rename_with(~ paste("exot", ., sep = "_"), -(1:7))

## somatório de formas de vida secas ou mortas por UC, UA, ANO

sum_form_vida_seca_morta_by_UC_UA_ANO <- registros_corrig %>%
  group_by(
    .id,
    PROTOCOLO,
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
  rename_with(~ paste("seca_morta", ., sep = "_"), -(1:7))

### formação vegetacional por UC, UA, ANO:

form_veg <- registros_corrig %>%
  group_by(
    .id,
    PROTOCOLO,
    UC,
    UA,
    ANO,
    `Coordenada inicial da amostragem (amostragem)`,
    `Coordenada final da amostragem (amostragem)`,
    `Ocorreram impactos, ações de manejo ou uso no local onde está situado o transecto? (impact_manejo_uso)`,
    `Qual(is)? (impact_manejo_uso)`,
    `Descreva os impactos, ações de manejo ou uso ocorridos (data, método, severidade, quando for o caso), caso conhecidos: (impact_manejo_uso)`,
    `Descreva observações gerais do transecto, caso necessário:`,
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
          "PROTOCOLO",
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
    names = c("lat_ini", "long_ini", "alt_ini", "acc_ini"),
    too_few = "align_start",
  ) %>%
  separate_wider_delim(
    cols = `Coordenada final da amostragem (amostragem)`,
    delim = " ",
    names = c("lat_fin", "long_fin", "alt_fin", "acc_fin"),
    too_few = "align_start",
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
      str_detect(colnames(registros_corrig_stat), "nativa_", negate = FALSE) &
        str_detect(
          colnames(registros_corrig_stat),
          "^nativa_serrapilheira",
          negate = TRUE
        )
    ))) %>% rowSums(na.rm = TRUE),
    .after = form_veg
  )

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(
    sum_exotica = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "exot_", negate = FALSE) &
        str_detect(
          colnames(registros_corrig_stat),
          "^exot_serrapilheira",
          negate = TRUE
        )
    ))) %>% rowSums(na.rm = TRUE),
    .after = sum_nativa
  )

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(
    sum_seca_morta = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "seca_morta_", negate = FALSE) &
        str_detect(
          colnames(registros_corrig_stat),
          "^seca_morta_serrapilheira",
          negate = TRUE
        )
    ))) %>% rowSums(na.rm = TRUE),
    .after = sum_exotica
  )

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(
    serrapilheira = dplyr::select(., which((
      str_detect(colnames(registros_corrig_stat), "serrapilheira", negate = FALSE)
    ))) %>% rowSums(na.rm = TRUE),
    .after = sum_seca_morta
  ) %>%
  dplyr::select(-any_of(
    c(
      "exotica",
      "nativa",
      "seca_morta",
      "nativa_serrapilheira",
      "exot_serrapilheira",
      "seca_morta_serrapilheira"
    )
  ))

### criação sum_herbacea, sum_lenhosa

registros_corrig_stat <- registros_corrig_stat %>%
  dplyr::mutate(sum_herbacea = select(
    .,
    c(
      which(str_detect(
        colnames(registros_corrig_stat), "_graminoide", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_bromelioide", negate = FALSE
      )),
      which(str_detect(
        colnames(registros_corrig_stat), "_cactacea", negate = FALSE
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
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_passarinho", negate = FALSE
    )),
    which(str_detect(
      colnames(registros_corrig_stat), "_bambu", negate = FALSE
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
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p1.1.1 <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p1.1.2 <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p1.2.1 <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p1.2.2 <- reg_corrig_stat_summarise_p1 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de plantas herbáceas e lenhosas
            em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  list(p1.1.1, p1.1.2, p1.2.1, p1.2.2)
}

### Proporção relativa de plantas nativas, exóticas, secas ou mortas, serrapilheira
### e solo nu

## plot nativa, exotica, seca_morta, serrapilheira, solo_nu

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
  group_by(ANO, form_veg, categoria) %>%
  dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
  dplyr::mutate(prop = prop.table(n)) %>%
  filter(n > 0)

p2.1.1 <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Campestre") %>%
  ggplot(aes(prop, ANO, fill = categoria)) +
  geom_col() +
  # geom_text(aes(label = paste0(
  #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
  # )),
  # angle = 90,
  # position = position_stack(vjust = .5)) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
    serrapilheira e solo nu em formações campestres",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  theme_bw() # +
# theme(plot.title.position = "plot",
#   plot.title = ggtext::element_textbox_simple())


p2.1.2 <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Campestre") %>%
  ggplot(aes(prop, ANO, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = paste0(
    "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
  )),
  angle = 90,
  position = position_stack(vjust = .5)) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
    serrapilheira e solo nu em formações campestres",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  theme_bw() # +
# theme(plot.title.position = "plot",
#   plot.title = ggtext::element_textbox_simple())

p2.2.1 <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Savânica") %>%
  ggplot(aes(prop, ANO, fill = categoria)) +
  geom_col() +
  # geom_text(aes(label = paste0(
  #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
  # )),
  # angle = 90,
  # position = position_stack(vjust = .5)) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
    serrapilheira e solo nu em formações savânicas",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  theme_bw() # +
# theme(plot.title.position = "plot",
#   plot.title = ggtext::element_textbox_simple())

p2.2.2 <- reg_corrig_stat_summarise_p2 %>%
  subset(., form_veg == "Savânica") %>%
  ggplot(aes(prop, ANO, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = paste0(
    "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
  )),
  angle = 90,
  position = position_stack(vjust = .5)) +
  labs(
    title = "Proporção relativa de plantas nativas, exóticas, secas ou mortas,
    serrapilheira e solo nu em formações savânicas",
    x = "Proporção relativa",
    y = "ANO",
    fill = "Categoria"
  ) +
  theme_bw() # +
# theme(plot.title.position = "plot",
#   plot.title = ggtext::element_textbox_simple())

list(p2.1.1, p2.1.2, p2.2.1, p2.2.2)

### Proporção relativa de formas de vida de plantas nativas

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
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p3.1.1 <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p3.1.2 <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p3.2.1 <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p3.2.2 <- reg_corrig_stat_summarise_p3 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas nativas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  list(p3.1.1, p3.1.2, p3.2.1, p3.2.2)
}

### Proporção relativa de formas de vida de plantas exóticas

## plot formas vida exotica

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
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p4.1.1 <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p4.1.2 <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  #p4.1.2
  
  p4.2.1 <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p4.2.2 <- reg_corrig_stat_summarise_p4 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de
      plantas exóticas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  
  list(p4.1.1, p4.1.2, p4.2.1, p4.2.2)
}

### Proporção relativa de formas de vida de plantas secas ou mortas

### plot formas vida seca ou morta

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
    group_by(ANO, form_veg, categoria) %>%
    dplyr::summarise(n = sum(soma, na.rm = TRUE)) %>%
    dplyr::mutate(prop = prop.table(n))
  
  p5.1.1 <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p5.1.2 <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Campestre") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações campestres",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p5.2.1 <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    # geom_text(aes(label = paste0(
    #   "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    # )),
    # angle = 90,
    # position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  p5.2.2 <- reg_corrig_stat_summarise_p5 %>%
    subset(., form_veg == "Savânica") %>%
    ggplot(aes(prop, ANO, fill = categoria)) +
    geom_col() +
    geom_text(aes(label = paste0(
      "(n=", n, " , ", scales::percent(prop, accuracy = .1), ")"
    )),
    angle = 90,
    position = position_stack(vjust = .5)) +
    labs(
      title = "Proporção relativa de formas de vida e outras categorias de plantas secas ou mortas em formações savânicas",
      x = "Proporção relativa",
      y = "ANO",
      fill = "Categoria"
    ) +
    theme_bw() # +
  # theme(plot.title.position = "plot",
  #   plot.title = ggtext::element_textbox_simple())
  
  list(p5.1.1, p5.1.2, p5.2.1, p5.2.2)
  
}

### remoção de objetos não mais necessários:

if (exists("reg_herb_lenh"))
  rm(reg_herb_lenh)
if (exists("reg_categ_plantas_longer"))
  rm(reg_categ_plantas_longer)
if (exists("reg_formas_vida_nat_longer"))
  rm(reg_formas_vida_nat_longer)
if (exists("reg_formas_vida_exot_longer"))
  rm(reg_formas_vida_exot_longer)
if (exists("reg_formas_vida_seca_morta_longer"))
  rm(reg_formas_vida_seca_morta_longer)

### exportação das tabelas estatísticas em .csv

if (exists("registros_corrig"))
  fwrite(registros_corrig,
         file.path("registros_corrig.csv"),
         row.names = FALSE)

if (exists("registros_corrig_stat"))
  fwrite(registros_corrig_stat,
         file.path("registros_corrig_stat.csv"),
         row.names = FALSE)

if (exists("reg_corrig_stat_summarise_p1"))
  fwrite(
    reg_corrig_stat_summarise_p1,
    file.path("sum_herbacea_sum_lenhosa.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p2"))
  fwrite(reg_corrig_stat_summarise_p2,
         file.path("sum_categorias.csv"),
         row.names = FALSE)

if (exists("reg_corrig_stat_summarise_p3"))
  fwrite(reg_corrig_stat_summarise_p3,
         file.path("sum_form_vida_nativas.csv"),
         row.names = FALSE)

if (exists("reg_corrig_stat_summarise_p4"))
  fwrite(
    reg_corrig_stat_summarise_p4,
    file.path("sum_form_vida_exoticas.csv"),
    row.names = FALSE
  )

if (exists("reg_corrig_stat_summarise_p5"))
  fwrite(
    reg_corrig_stat_summarise_p5,
    file.path("sum_form_vida_secas_mortas.csv"),
    row.names = FALSE
  )

### kml export

  # Convert coordinate columns to numeric (if not already)
  registros_corrig_stat$long_ini <- as.numeric(registros_corrig_stat$long_ini)
  registros_corrig_stat$lat_ini <- as.numeric(registros_corrig_stat$lat_ini)
  registros_corrig_stat$long_fin <- as.numeric(registros_corrig_stat$long_fin)
  registros_corrig_stat$lat_fin <- as.numeric(registros_corrig_stat$lat_fin)
  
  # Remove rows with missing coordinates (if any)
  registros_corrig_stat <- registros_corrig_stat[!is.na(registros_corrig_stat$long_ini) &
                                                   !is.na(registros_corrig_stat$lat_ini) &
                                                   !is.na(registros_corrig_stat$long_fin) &
                                                   !is.na(registros_corrig_stat$lat_fin), ]
  
  # Ensure uniqueness in the 'name' column
  registros_corrig_stat <- registros_corrig_stat %>%
    mutate(name = paste(UA, ANO, sep = "_")) %>%
    group_by(name) %>%
    mutate(name = paste(name, row_number(), sep = "_")) %>%
    ungroup()
  
  # Create a 'LineString' for each row, combining the start and end coordinates
  geometry <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
    st_linestring(matrix(
      c(
        registros_corrig_stat$long_ini[i],
        registros_corrig_stat$lat_ini[i],
        registros_corrig_stat$long_fin[i],
        registros_corrig_stat$lat_fin[i]
      ),
      ncol = 2,
      byrow = TRUE
    ))
  }))
  
  # Create point geometries for each start and end coordinate
  points_ini <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
    st_point(c(
      registros_corrig_stat$long_ini[i],
      registros_corrig_stat$lat_ini[i]
    ))  # Start points
  }))
  
  points_fin <- st_sfc(lapply(1:nrow(registros_corrig_stat), function(i) {
    st_point(c(
      registros_corrig_stat$long_fin[i],
      registros_corrig_stat$lat_fin[i]
    ))
  }))
  
  # Create a new dataframe for points (start and end points) and keep all original attributes
  registros_corrig_stat_points_ini <- registros_corrig_stat %>%
    mutate(
      name = paste("Start: ", UA, "_", ANO, sep = ""),
      point_type = "start",
      geometry = points_ini
    )
  
  registros_corrig_stat_points_fin <- registros_corrig_stat %>%
    mutate(
      name = paste("End: ", UA, "_", ANO, sep = ""),
      point_type = "end",
      geometry = points_fin
    )
  
  # Combine the points and lines dataframes
  combined_sf_object <- bind_rows(
    st_sf(registros_corrig_stat, geometry = geometry),
    registros_corrig_stat_points_ini,
    registros_corrig_stat_points_fin
  )
  
  # Assign CRS (WGS84 - EPSG:4326) to the sf object
  combined_sf_object <- st_set_crs(combined_sf_object, 4326)
  
  # Export to KML file, overwriting if it already exists
  st_write(combined_sf_object, "output.kml", driver = "KML", delete_dsn=TRUE)
  
  rm(combined_sf_object,geometry,points_ini,points_fin,
     registros_corrig_stat_points_ini,registros_corrig_stat_points_fin)
  
