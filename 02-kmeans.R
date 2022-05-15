# Carrega biblioteca para leitura de arquivos
library(readr)

#leitura da base
spotify <- read.csv("data/Spotify 2010 - 2019 Top 100.csv",stringsAsFactors=TRUE)

summary(spotify)

# visualizando gêneros
barplot(table(spotify$top.genre))
utils::View(table(spotify$top.genre))

########################################################

# melhorando a classificação de generos agrupando eles
# visualizando gêneros
barplot(table(spotify$top.genre))
spotify_genre1 <- as.data.frame(table(spotify$top.genre))

library(dplyr)
library(stringr)

spotify <- spotify %>% 
  mutate(
    new.genre = case_when(
      str_detect(top.genre, 'pop')  ~ "pop",
      str_detect(top.genre, 'rock')  ~ "rock",
      str_detect(top.genre, 'hip hop')  ~ "hip hop",
      str_detect(top.genre, 'country')  ~ "country",
      str_detect(top.genre, 'indie')  ~ "indie",
      str_detect(top.genre, 'rap')  ~ "rap",
      str_detect(top.genre, 'r&b')  ~ "r&b",
      str_detect(top.genre, 'house')  ~ "house",
      str_detect(top.genre, 'edm')  ~ "edm",
      str_detect(top.genre, 'soul')  ~ "soul",
      str_detect(top.genre, 'alternative')  ~ "alternative",
      TRUE ~ "unknown"
    )
  )

barplot(table(spotify$new.genre))
spotify_genre2 <- as.data.frame(table(spotify$new.genre))
utils::View(table(spotify$new.genre))

########################################################


#criando um dataset somente com as colunas que vão ser analisadas
# "bpm", "nrgy", "dnce", "val", "acous", "live", "spch", "dB"
# "nrgy", "dnce", "dB"

spotify1 <- spotify[, c("bpm", "nrgy", "dnce", "dB", "spch")]
summary(spotify1)


# Visualizando as distancias entre os objetos
#
library(factoextra)
distance <- get_dist(spotify1)

# gráfico de similariade 
fviz_dist(distance, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

?fviz_dist
#
# Usando o K-means
#

# rodando o kmeans com 3 grupos, o melhor resultado de 10 e visualizando o resultado
spotify_kmi1 <- kmeans(spotify1, centers=3, nstart = 10)
spotify_kmi1
spotify_kmi1$centers
spotify_kmi1$size
spotify_kmi1$tot.withinss
spotify_kmi1$cluster

# cluster plot
fviz_cluster(spotify_kmi1, data = spotify1)


# plotando os agrupamentos de objetos, no caso de mais de duas variaveis, de acordo com seus componentes principais
plot(spotify1, col=spotify_kmi1$cluster, 
     main = paste("k-means clustering com 3 grupos; Total do SSE = ",spotify_kmi1$tot.withinss))


# gerando uma matriz de contingencia e avaliando o SSE intra-clusters e total
table(spotify$top.genre, spotify_kmi1$cluster)
table(spotify$new.genre, spotify_kmi1$cluster)


## gerando agrupamentos com diferentes numeros de clusters
spotify_kmi3 <- kmeans(spotify1, centers=3, nstart = 10)
spotify_kmi5 <- kmeans(spotify1, centers=5, nstart = 10)
spotify_kmi10 <- kmeans(spotify1, centers=10, nstart = 10)
spotify_kmi20 <- kmeans(spotify1, centers=20, nstart = 10)
spotify_kmi50 <- kmeans(spotify1, centers=50, nstart = 10)

# visualizando os resultados 
p3 <- fviz_cluster(spotify_kmi3, geom = "point", data = spotify1) + ggtitle("k = 3")
p5 <- fviz_cluster(spotify_kmi5, geom = "point",  data = spotify1) + ggtitle("k = 5")
p10 <- fviz_cluster(spotify_kmi10, geom = "point",  data = spotify1) + ggtitle("k = 10")
fviz_cluster(spotify_kmi10, geom = "point",  data = spotify1) + ggtitle("k = 10")
#p20 <- fviz_cluster(spotify_kmi20, geom = "point",  data = spotify1) + ggtitle("k = 20")
#p50 <- fviz_cluster(spotify_kmi50, geom = "point",  data = spotify1) + ggtitle("k = 50")

library(gridExtra)
grid.arrange(p3, p5, p10, nrow = 1)


plot(spotify1, col=spotify_kmi3$cluster, 
     main = paste("k-means clustering com 3 grupos; Total do SSE = ",spotify_kmi1$tot.withinss))

plot(spotify1, col=spotify_kmi5$cluster, 
              main = paste("k-means clustering com 5 grupos; Total do SSE = ",spotify_kmi1$tot.withinss))

plot(spotify1, col=spotify_kmi10$cluster, 
              main = paste("k-means clustering com 10 grupos"))

#comparando as matrizes de contingencia e relacinando com os SSE dos clusters
table(spotify$top.genre, spotify_kmi3$cluster)
table(spotify$top.genre, spotify_kmi5$cluster)
table(spotify$top.genre, spotify_kmi10$cluster)
table(spotify$top.genre, spotify_kmi20$cluster)
table(spotify$top.genre, spotify_kmi50$cluster)

table(spotify$artist.type, spotify_kmi3$cluster)
table(spotify$artist.type, spotify_kmi5$cluster)
table(spotify$artist.type, spotify_kmi10$cluster)
table(spotify$artist.type, spotify_kmi20$cluster)
table(spotify$artist.type, spotify_kmi50$cluster)



table(spotify$new.genre, spotify_kmi3$cluster)
table(spotify$new.genre, spotify_kmi5$cluster)
table(spotify$new.genre, spotify_kmi10$cluster)
table(spotify$new.genre, spotify_kmi20$cluster)
table(spotify$new.genre, spotify_kmi50$cluster)

table(spotify$artist.type, spotify_kmi3$cluster)
table(spotify$artist.type, spotify_kmi5$cluster)
table(spotify$artist.type, spotify_kmi10$cluster)
table(spotify$artist.type, spotify_kmi20$cluster)
table(spotify$artist.type, spotify_kmi50$cluster)

spotify_kmi3$withinss
spotify_kmi3$tot.withinss

# Elbow method
set.seed(123)
fviz_nbclust(spotify1 , kmeans, method = "wss", k.max=25)
?fviz_nbclust



