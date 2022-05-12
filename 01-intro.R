#Análise geral da base de dados (sumarização, histogramas, correlações, etc)

#instala pacotes necessarios (psych para usar funcao describe)
install.packages("psych")
install.packages("ggplot")

#pacotes necessarios
library(readr)
library(psych)
#library(tidyverse)
library(ggplot2)

#leitura da base
spotify <- read.csv("data/Spotify 2010 - 2019 Top 100.csv",stringsAsFactors=TRUE)

#exploracao da base
View(spotify)
str(spotify)

#sumarizacao das variáveis numéricas
#summary(spotify)
#mean | standard deviation | min | max | range | standard error
describe(spotify[ , c('dnce', 'nrgy', 'dB', 'spch', 'acous', 'live', 'bpm', 'dur', 'pop')], fast=TRUE)


#Histogramas das variáveis numéricas
ggplot(data = spotify, aes(x = dnce)) + geom_histogram() + ggtitle('Dançabilidade - Quão fácil é dançar a música')
ggplot(data = spotify, aes(x = nrgy)) + geom_histogram() + ggtitle('Energia - Quão energética é a música')
ggplot(data = spotify, aes(x = dB)) + geom_histogram() + ggtitle('Decibel - Quão alta é a música')
ggplot(data = spotify, aes(x = spch)) + geom_histogram() + ggtitle('Cantado - Quão a música é focada na palavra falada')
ggplot(data = spotify, aes(x = acous)) + geom_histogram() + ggtitle('Acústica - Quão acústica é a música')
ggplot(data = spotify, aes(x = live)) + geom_histogram() + ggtitle('Ao Vivo - O quanto se parece com uma gravação ao vivo')
ggplot(data = spotify, aes(x = bpm)) + geom_histogram() + ggtitle('Beats Per Minute - Ritmo da música')
ggplot(data = spotify, aes(x = dur)) + geom_histogram() + ggtitle('Duração da música em segundos')
ggplot(data = spotify, aes(x = pop)) + geom_histogram() + ggtitle('Popularidade da música (não é um ranking)')

# EXEMPLO DE  MULTIPLOS MINI HISTOGRAM COM BASE EM UMA VARIÁVEL CATEGÓRICA
#ggplot(data = txhousing, aes(x = median)) + geom_histogram() + ggtitle('Preço Mediano das Vendas')+facet_wrap(~city)

spotify1 <- spotify[, c('dnce', 'nrgy', 'dB', 'spch', 'acous', 'live', 'bpm', 'dur', 'pop')]

#grafico de pairs
pairs(spotify1)

#gr?fico de correla??o
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
ggcorr(spotify1)
ggcorr(spotify1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")


