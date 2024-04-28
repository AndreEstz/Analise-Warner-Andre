#Atividade: 1) Número de lançamentos a cada década por formato de lançamento

library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)


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
#classes <- c( 1960, 1970, 1980, 1990, 2000, 2010, 2020)
#labels <- c("1960-1970", "1970-1980", "1980-1990", "1990-2000", "2000-2010", "2010-2020", "2020")

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

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


df_series <- data_e_lançamentos[data_e_lançamentos$format == "Serie",]
df_series <- group_by(df_series, Decada) %>%
  summarise(quantidade_lançamento = n())

df_movie <- data_e_lançamentos[data_e_lançamentos$format == "Movie",]
df_movie <- group_by(df_movie, Decada) %>%
  summarise(quantidade_lançamento = n())

df_crossover <- data_e_lançamentos[data_e_lançamentos$format == "CrossOver",]
df_crossover <- group_by(df_crossover, Decada) %>%
  summarise(quantidade_lançamento = n())


plot_serie <- ggplot(df_series) +
  aes(x= Decada, y= quantidade_lançamento, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Preço") +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")

plot_movie <- ggplot(df_movie) +
  aes(x= Decada, y= quantidade_lançamento, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Preço") +
  theme_estat()

data_e_lançamentos$Valor <- 1 


ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

data_e_lançamentos_resumo <- data_e_lançamentos %>%
  group_by(format, Decada) %>%
  summarise(n = n())

names(data_e_lançamentos_resumo)[names(data_e_lançamentos_resumo) == 'format'] <- 'Formatos'

ggplot(data_e_lançamentos_resumo) +
  aes(x = Decada, y = n , group = Formatos, colour = Formatos) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Decadas", y = "Quantidade de Lançamentos") +
  theme_estat()
       


       





                                          
