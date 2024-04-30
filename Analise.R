#_______________________ Análise de Desempenho Scooby-Doo _________________________#
#_______________________ André Rodrigues ___________________________________#

#____ Analises: ----
#' 1) Número de lançamentos a cada década por formato de lançamento;
#' 2) Variação da nota IMDB por temporada dos episódios;
#' 3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#' 4) Relação entre as notas IMDB e engajamento;
#' 5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
 
#____ Pacotes ----
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#____ Bancos ----
banco <- read.csv("banco_final.csv")

#____ Tema Estat ----
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
### 1) Numero de Lançamentos ----
# Filtrando o data set
data_e_lançamentos <- banco %>% select(date_aired,format )
data_em_ano <- data.frame(year(data_e_lançamentos$date_aired))
data_e_lançamentos <- data_e_lançamentos %>% mutate(data_em_ano)

#Renomeando A coluna para tornar mais facil
names(data_e_lançamentos)[names(data_e_lançamentos) == 'year.data_e_lançamentos.date_aired.'] <- 'Launch_Year'
data_e_lançamentos <- subset(data_e_lançamentos, select = -c(date_aired))
names(data_e_lançamentos)[names(data_e_lançamentos) == 'format'] <- 'Formatos'

#Definindo as décadas 
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

# Recolhendo dados iniciais da quantidade de lançamentos
data_e_lançamentos_Tbl <- table(data_e_lançamentos)

# Agrupando dados iguais para o gráfico
data_e_lançamentos <- data_e_lançamentos %>%
  group_by(Formatos, Decada) %>%
  summarise(n = n())

#Mudando nome das variáveis para português
data_e_lançamentos <- data_e_lançamentos %>%
  mutate(Formatos = recode(Formatos, Movie = 'Filme', CrossOver = 'Crossover', Serie = 'Série'))

#Gerando o gráfico final
ggplot(data_e_lançamentos) +
  aes(x = Decada, y = n , group = Formatos, colour = Formatos) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Quantidade de Lançamentos") +
  theme_estat()
ggsave("numero_de_lançamentos.pdf", width = 158, height = 93, units = "mm")     

### 2) Variação IMDB ----
#Variação da nota IMDB por temporada dos episódios;
#ideia inicial : Precisa de coluna IMDB SEASON EPISODIOS
# Tira a média do imdb da temporada e compara??????
#Colocar eixo x episodios y imdb e colocar em grupos na tag das temporadas

df_imdbeTemp <- banco %>% select(season, imdb)
excludente1 <- filter(df_imdbeTemp, season == 'Movie')
excluddente2 <- filter(df_imdbeTemp, season == 'Crossover')
df_imdbeTemp <- anti_join(df_imdbeTemp, excludente1)
df_imdbeTemp <- anti_join(df_imdbeTemp, excluddente2)
#Esse codigo precisa de otimização 

#dispersão?
df_imdbeTemp %>%
  mutate(Temporada = case_when(
    season %>% str_detect("1") ~ "Aumático",
    season %>% str_detect("2") ~ "Manual"
  )) %>%
  ggplot() +
  aes(x=1:553, y=imdb) +
  geom_point(aes(colour=season)) +
  labs(
    x="Episódios", y="Nota Imdb"
  ) +
  theme_estat()
ggsave("disp_bi.pdf", width = 158, height = 93, units = "mm")
#Eu posso tirar aqui a concentração dos dados em nota imdb medianas 

#possivel blox pot
df_imdbeTemp %>%
  mutate(Temporada = case_when(
    season %>% str_detect("1") ~ "Aumático",
    season %>% str_detect("2") ~ "Manual"
  )) %>%
  ggplot() +
  aes(x=season, y=imdb) +
  geom_point(aes(colour=season)) +
  labs(
    x="Temporada", y="Nota Imdb"
  ) +
  theme_estat()
ggsave("futuro_box.pdf", width = 158, height = 93, units = "mm")
25
#Eu posso tirar daqui (em formato box plot de verdade, so n faço agora pq eu tenho reuniao) medidas resumo de cada temporada para a analise
