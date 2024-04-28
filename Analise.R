#Atividade: 1) Número de lançamentos a cada década por formato de lançamento

library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)

setwd("C:/Users/andre/OneDrive/Documentos/Documents/Trabalho/Analise_Estatistica_Warner")
banco <- read.csv("banco_final.csv")
'banco
banco_df <- data.frame(banco)
banco_df

banco_df$data_mes <- year(banco$date_aired)


data_de_lançamentos <- banco_df %>%
  group_by(date_aired) %>%
  count()
data_de_lançamentos
numero_de_lançamentos <- banco_df %>%
  group_by(format) %>%
  count()
table(banco_df$format)'


data_e_lançamentos <- banco %>% select(date_aired,format )
data_em_ano <- data.frame(year(data_e_lançamentos$date_aired))
data_e_lançamentos <- data_e_lançamentos %>% mutate(data_em_ano)
names(data_e_lançamentos)[names(data_e_lançamentos) == 'year.data_e_lançamentos.date_aired.'] <- 'Launch_Year'
data_e_lançamentos <- subset(data_e_lançamentos, select = -c(date_aired))

## gráfico: tenho que olhar na padronização algum que traga uma ideia temporal!
classes <- c( 1960, 1970, 1980, 1990, 2000, 2010, 2020)
labels <- c("1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2010", "2010-2020", "2020")

#cut(
#  x = data_e_lançamentos$Launch_Year,
#  breaks = classes,
#  labels = labels,
#  include.lowest = TRUE
#)

data_e_lançamentos <- data_e_lançamentos %>% mutate(Decada = case_when(
  Launch_Year >= 1960 & Launch_Year <1970 ~ "1960-1970",
  Launch_Year >= 1970 & Launch_Year <1980 ~ "1970-1980",
  Launch_Year >= 1980 & Launch_Year <1990 ~ "1980-1990",
  Launch_Year >= 1990 & Launch_Year <2000 ~ "1990-2000",
  Launch_Year >= 2000 & Launch_Year <2010 ~ "2000-2010",
  Launch_Year >= 2010 & Launch_Year <2020 ~ "2010-2020",
  Launch_Year >= 2020 ~ "2020-",
))

data_e_lançamentos <- subset(data_e_lançamentos, select = -c(Launch_Year))
data_e_lançamentos_Tbl <- table(data_e_lançamentos)
                                                    


