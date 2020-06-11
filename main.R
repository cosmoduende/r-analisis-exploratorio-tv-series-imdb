
library(tidyverse)
library(plotly)
library(kableExtra)
library(knitr)

theme_set(theme_bw())

# LECTURA DE CSV GENERADO POR WEB SCRAPING IMDB 
series_imdb <- read_csv("series_imdb.csv")

# EN CASO DE HABER GENERADO UN SET DE DATOS MÁS GRANDE, FILTRAR SÓLO ALGUNAS SERIES DEL SET COMPLETO
mis_series <- series_imdb %>% filter(series_name %in% c("Black Mirror", "Game of Thrones", "Rick & Morty", "Stranger Things", "Westworld")) %>%
  mutate(series_name = factor(series_name)) %>%
  mutate(season = factor(season))

#PREVIEW SERIES Y COLUMNAS
levels(mis_series$series_name)
colnames(mis_series)

# HISTOGRAMA USER RATING GLOBAL
mis_series %>%
  ggplot(aes(UserRating)) +
  geom_histogram(binwidth = .1, fill = "#0196d7") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(y = "Número de ocurrencias", x = "Calificación de episodio") + 
  ggtitle("UserRating general de las 5 series", "Black Mirror, Westworld, Game of Thrones, Rick and Morty y Stranger Things.")
ggplotly()



# RELACIÓN ENTRE CALIFICACIÓN Y NÚMERO DE VOTOS POR SERIE
series_votos <- mis_series %>% group_by(series_name) %>% summarise(rating = median(UserRating), totalvotos = sum(UserVotes))
series_votos %>% 
  ggplot(aes(x = reorder(series_name, totalvotos), y = totalvotos, fill = rating)) +
  geom_histogram(stat = "identity") +
  scale_fill_distiller(name="Calificación", palette = "Blues") +
  scale_y_continuous(labels = scales::comma) + 
  coord_flip() +
  xlab("Serie") + ylab("Número total de votos") + 
  ggtitle("Calificación de cada serie", "Relación entre calificación y número de votos")
ggplotly()


# EVOLUCIÓN DE LA ACEPTACIÓN DE CADA SERIE CON BASE EN SU CALIFICACIÓN POR TEMPORADA
series_acep <- mis_series %>% group_by(series_name, season) %>% summarise(rating = median(UserRating))
series_acep %>% 
  ggplot(aes(x = as.numeric(season), y = rating, color = series_name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  scale_color_brewer(name = "Serie", palette = "Set1") +
  xlab("Temporada") + ylab("Calificación") + 
  ggtitle("Evolución de la aceptación de cada serie", "Calificación por temporada")
ggplotly()


# BOX PLOT CALIFICACIÓN TEMPORADA POR EPISODIOS
series_temp <- mis_series %>%
  ggplot(aes(season, UserRating, color = season)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  facet_wrap(~series_name) +
  labs(x = "Temporada", y = "Calificación de episodio", color = "Temporada") + 
  ggtitle("Evolución de cada temporada por capítulo", "Calificación por episodio")
ggplotly()



# TOP MEJORES Y PEORES EPISODIOS CON BASE EN SU CALIFICACIÓN
mejores_ep <- knitr::kable(x = head(arrange(mis_series, desc(UserRating)) %>%
      select(series_name, Episode, ,season, UserRating), 7),
      col.names = c('Serie', 'Episodio', 'Temporada', 'Calificación'))
kable_styling(mejores_ep, "striped", position = "left", font_size = 12)

peores_ep <- knitr::kable(x = head(arrange(mis_series, (UserRating)) %>%
       select(series_name, Episode, ,season, UserRating), 7),
      col.names = c('Serie', 'Episodio', 'Temporada', 'Calificación'))
kable_styling(peores_ep, "striped", position = "left", font_size = 12)


