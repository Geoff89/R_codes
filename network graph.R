#pltting the graph network
library(igraph)
igraph::
#plotting the directed graph
gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)

#undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed = FALSE)
#No labels
plot(gu, vertex.label = NA)

set.seed(229)
set.seed(123)
set.seed(245)
plot(gu,vertex.label = NA)

#structure of gd
str(gd)
str(gu)

library(gcookbook)
madmen2
head(madmen2)
head(madmen)

#create a graph object from the data set
g <- graph.data.frame(madmen2, directed = TRUE)
#remove unneneces =sary margins
par(mar = c(0,0,0,0))
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8,
     edge.arrow.size = 0.5, vertex.label = NA)

#for an undirected graph
g <- graph.data.frame(madmen, directed = FALSE)
par(mar = c(0,0,0,0))
plot(g, layout = layout.circle, vertex.size = 8, vertex.label = NA)


#adding labels to the nodes
m <- madmen[1: nrow(madmen) %% 2 == 1,] #pick rows tha are odd in number
m
g <- graph.data.frame(m, directed = FALSE)
g1 <- graph.data.frame(m, directed = TRUE)

#print the names of the labels
V(g)$name

plot(g, layout = layout.fruchterman.reingold, 
     vertex.size = 4, #make smaller nodes
     vertex.label = V(g)$name, #setting the labels of nodes
     vertex.label.cex = 0.8, #slightly smaller font
     vertex.label.dist = 0.4, #offset the labels
     vertex.label.color = "black")

plot(g1, layout = layout.fruchterman.reingold, 
     vertex.size = 4, #make smaller nodes
     vertex.label = V(g)$name, #setting the labels of nodes
     vertex.label.cex = 0.8, #slightly smaller font
     vertex.label.dist = 0.4, #offset the labels
     vertex.label.color = "black")
