# Packages/Pacotes
library(dplyr)
library(rio)
library(shiny)
library(shinydashboard)
library(data.table)
library(dygraphs)
library(xts)
library(RColorBrewer)
library(leaflet)

# Set Working Directory
setwd("~/Git/shiny-gasolinas")

# #
# base <- data.table()
# 
# # 
# arquivos <- append(paste0("http://www.anp.gov.br/images/dadosabertos/precos/", 2013:2017, "-", "1_CA.csv"), paste0("http://www.anp.gov.br/images/dadosabertos/precos/", 2013:2017, "-", "2_CA.csv"))
# arquivos <- append(arquivos, "http://www.anp.gov.br/images/dadosabertos/precos/2018-1_CA.csv")
# 
# #
#  for (i in seq_along(arquivos)) {
#    aux <- rio::import(arquivos[i])
#    base <- data.table::rbindlist(list(base, aux))
#  }
# rm(aux)


base1 <- rio::import("http://www.anp.gov.br/images/dadosabertos/precos/2017-2_CA.csv")
base2 <- rio::import("http://www.anp.gov.br/images/dadosabertos/precos/2018-1_CA.csv")
base <- data.table::rbindlist(list(base1, base2))

# Reformata algumas vari?veis
# Checar se precisa mesmo de duas linhas
base$`Data da Coleta` <- as.Date(base$`Data da Coleta`, "%d/%m/%Y")
base$`Data da Coleta` <- as.Date(base$`Data da Coleta`, format = "%Y%m%d")
#base$`Data da Coleta` <- as.Date(paste0(sprintf("%.*s", 8, base$`Data da Coleta`), "01"))

base$`Valor de Compra` <- as.numeric(gsub(",", ".", base$`Valor de Compra`))
base$`Valor de Venda` <- as.numeric(gsub(",", ".", base$`Valor de Venda`))
base$`Valor de Lucro` <- base$`Valor de Venda` - base$`Valor de Compra`

#
data.table::setkey(base, `Data da Coleta`, `Estado - Sigla`, Bandeira, Produto)

base[, Ano := year(base$`Data da Coleta`)]

# Base para o Gr�fico de Linhas
base_media <- base[, .(
  Compra = mean(`Valor de Compra`, na.rm = T), 
  Venda = mean(`Valor de Venda`, na.rm = T), 
  Lucro = mean(`Valor de Lucro`, na.rm = T)
  ), 
  by = .(`Data da Coleta`, `Estado - Sigla`, Munic�pio, Bandeira, Produto)]

### Mapa
mapa <- rgdal::readOGR(dsn = paste0(getwd(), "/Brasil - Spacial Data/UFEBRASIL.shp"), layer = "UFEBRASIL", encoding = "UTF8")

# Simplifica o arquivo de Mapa, mas quebra o encode
#mapa <- rmapshaper::ms_simplify(mapa, keep_shapes = T)

#
mapa_data <- data.frame(mapa@data)
data.table::setDT(mapa_data)

de_para <- list(
  "ACRE" = "AC",
  "ALAGOAS" = "AL",
  "AMAP�" = "AP",
  "AMAZONAS" = "AM",
  "BAHIA" = "BA",
  "CEAR�" = "CE",
  "DISTRITO FEDERAL" = "DF",
  "ESP�RITO SANTO" = "ES",
  "GOI�S" = "GO",
  "MARANH�O" = "MA",
  "MATO GROSSO" = "MT",
  "MATO GROSSO DO SUL" = "MS",
  "MINAS GERAIS" = "MG",
  "PAR�" = "PA",
  "PARA�BA" = "PB",
  "PARAN�" = "PR",
  "PERNAMBUCO" = "PE",
  "PIAU�" = "PI",
  "RORAIMA" = "RR",
  "ROND�NIA" = "RO",
  "RIO DE JANEIRO" = "RJ",
  "RIO GRANDE DO NORTE" = "RN",
  "RIO GRANDE DO SUL" = "RS",
  "SANTA CATARINA" = "SC",
  "S�O PAULO" = "SP",
  "SERGIPE" = "SE",
  "TOCANTINS" = "TO"
)

#
mapa_data[order(NM_ESTADO), SIGLA_ESTADO := as.character(de_para)]

# App Shiny
runApp(appDir = paste0(getwd(), "/Shiny"))
