# Para criar o agrupamento hierárquico, de forma a visualizar de forma aninhada 
# os clusters encontrados, precisaremos trabalhar com variáveis numéricas.
# Por isso, as colunas título, artista, top.genre e added foram excluídas.
# O campo added também foi excluído, pois não agrega na tarefa de agrupamento,
# não há relação com quando a música foi lançada (year.released), por fim, seus valores
# estão concentrados em datas específicas.
spotify_numeric <- spotify[-c(1:3, 5)]
spotify_numeric <- dummy_cols(spotify_numeric, select_columns = 'artist.type')
spotify_numeric <- spotify_numeric[-c(13)]
spotify_numeric <- spotify_numeric[-c(13)]
str(spotify_numeric)


# Outra tarefa bastante importante é a normalização, pois para o cálculo das distâncias
# temos que minimizar as distorções decorrentes de unidades ou dispersões distintas 
# entre as variáveis, deixando-as com o mesmo peso.
z <- spotify_numeric
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
spotify_numeric <- scale(z,center=means,scale=sds)


# Dendograma incial. 
# Depois da criação da matriz de distâncias criamos um dendograma inicial.
# Contudo, como se trata de uma base com 1000 registros, a identificação de
# cada observação individualmente fica confusa.
dista=dist(spotify_numeric, method="euclidean")
dista.hc=hclust(d=dista, method="ward.D")
fviz_dend(dista.hc, cex=0.5)


# Uma forma de contornar esse problema é visualizar apenas um trecho da árvore
# criada, com isso podemos analisar um pedaço do gráfico de acordo com um corte.
# Abaixo criamos um corte de altura 7, onde podemos reparar a parte superior dessa altura.
hc <- hclust(dist(spotify_numeric))
hcd <- as.dendrogram(hc)
plot(cut(hcd, h=7)$upper, 
     main="Partde de cima do corte h=7")

# De forma análoga ao diagrama anterior, também podemos focar nossa atenção na
# parte de baixo de alguma altura específica.
# Abaixo criamos uma visualização da parte inferior ao corte de altura 10.
plot(cut(hcd, h=10)$lower[[2]], 
     main="Second branch of lower tree with cut at h=10")

# Outra forma de melhorar o entendimento dos dados é demarcar os clusters de
# interesse em um diagrama hierárquico.
# Como nossas classes de interesse são os valores encontradas no campo new.genre,
# iremos realizar o agrupamento em 12 clusters, que é justamente o valor de
# classes do campo.
# As áreas demarcadas, cada uma com uma cor diferente é a representação de onde
# cada observação foi agrupada (de acordo com a distância calculada).
plot(dista.hc, labels=FALSE)
rect.hclust(dista.hc, k = 12, border = 2:6)
abline(h = 50, col = 'red')


# Com o mesmo raciocínio do gráfico anterior, podemos, ao invés de demarcar os
# clusters em áreas, colorir os agrupamentos diferentes, colocando um rótulo
# acima para facilitar a visualização. Desta forma fica um pouco mais claro
# como cada cluster foi criado.
avg_dend_obj <- as.dendrogram(dista.hc)
avg_col_dend <- color_branches(avg_dend_obj, h = 30, k = 12, groupLabels=TRUE)
plot(avg_col_dend)


# De acordo com nossa análise do gráfico hierárquico, onde dividimos a base
# em 12 clusters, esperamos que haja uma correlação entre os agrupamentos e
# os valores do campo new.genre.
# Abaixo segue a tabela que relaciona os clusters encontrados e os ritmos (que são
# os valores encontrados na base inicial).
gruposavg <-cutree(dista.hc,12)
table(spotify$new.genre, gruposavg)
# Apenas pelos números dessa tabela podemos afirmar que não houve uma boa
# divisão de clusters, de forma que os nossos grupos encontrados não correspondem
# aos ritmos esperados. Cada aglomeração que identificamos ficou claramente
# espalhada pelas classes originais da base, indicando que não houve uma boa
# correlação entre os grupos identificados e os ritmos que pretendíamos encontrar.

#Através dos números da tabela acima, apenas um ritmo apresentou maior correlação com um cluster. Trata-se da classe indie (campo new.genre) e o cluster número 5. Ressalta-se que o contrário não é verdadeiro, pois o cluster 5 possui seus elementos divididos, quase que de forma aleatória, por todos os ritmos. 
#Para nos certificar disso vamos analisar os números. Para isso vamos agregar à uma cópia da base original os nossos clusters encontrados, depois iremos comparar o resultado obtido.
#Abaixo segue a tabela composta com as observações da classe indie e sua identificação do cluster que foi atribuída pelo nosso modelo:
spotify2 <- spotify
spotify2$predicted <- gruposavg
spotify2 <- spotify2[c(18,19)]

indie <- filter(spotify2, new.genre == "indie")
indie
# Aparentemente apenas a  classe indie teve uma taxa de acerto muito mais alta que as demais,
# 72,7% das observações foram agrupadas corretamente, o que ainda é insuficiente
# para dizermos que encontramos um modelo adequado de agrupamento de acordo
# com os gêneros musicais.

