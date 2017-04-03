library(igraph)
library(RColorBrewer)
library(extrafont)
loadfonts()

## CMV NETS
edges1 = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/edgelist_subset.csv')
nodes1 = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/nodelist_subset.csv')

net1 = 
  graph_from_data_frame(d=edges1, vertices=nodes1, directed = TRUE) %>%
  simplify(remove.loops = T)

V(net1)$type = nodes1$type # make bipartite
pr1=bipartite.projection(net1)
subs1 = pr1$proj2
mods1 = pr1$proj1
V(subs1)$degree = degree(subs1)
V(mods1)$degree = degree(mods1)

## TD NETS
edges2 = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/edgelist_subset.csv')
nodes2 = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/nodelist_subset.csv')

net2 = 
  graph_from_data_frame(d=edges2, vertices=nodes2) %>%
  simplify(remove.loops = T)

V(net2)$type = nodes2$type # make bipartite
pr2=bipartite.projection(net2)
subs2 = pr2$proj2
mods2 = pr2$proj1
V(subs2)$degree = degree(subs2)
V(mods2)$degree = degree(mods2)

## PLOT MODERATOR HISTOGRAMS

png(filename='/Users/emg/Google Drive/PhD/presenting/ss_phd_seminar_april_17/visuals/mod-degree-dist.png',
    width=680,height=512)

{
  par(mfrow=c(2,2))
  
deg <- degree(mods1, mode="all")
hist(deg, breaks=20, xlab="Degree",
     cex.lab=1.2, cex.main=1.4,
     main="Histogram of CMV mod degree")

deg.dist <- degree_distribution(mods1, cumulative=T, mode="all")
plot(rev(deg.dist),  pch=19, cex=1.2, col="orange",
     cex.lab=1.2, cex.main=1.4,
     xlab="Degree", ylab="Cumulative Frequency",
     main='Cumulative Frequency of CMV mod degree')

deg <- degree(mods2, mode="all")
hist(deg, breaks=20, xlab="Degree",
     cex.lab=1.2, cex.main=1.4,
     main="Histogram of TD mod degree")

deg.dist <- degree_distribution(mods2, cumulative=T, mode="all")
plot(rev(deg.dist),  pch=19, cex=1.2, col="orange", 
     xlab="Degree", ylab="Cumulative Frequency",
     cex.lab=1.2, cex.main=1.4,
     main='Cumulative Frequency of TD mod degree')

dev.off()
}



## PLOT SUBREDDIT HISTOGRAMS

png(filename='/Users/emg/Google Drive/PhD/presenting/ss_phd_seminar_april_17/visuals/sub-degree-dist.png',
    width=680,height=512)

{
  par(mfrow=c(2,2))
  
  deg <- degree(subs1, mode="all")
  hist(deg, breaks=20, xlab="Degree",
       cex.lab=1.2, cex.main=1.4,
       main="Histogram of CMV sub degree")
  
  deg.dist <- degree_distribution(subs1, cumulative=T, mode="all")
  plot(rev(deg.dist),  pch=19, cex=1.2, col="orange",
       cex.lab=1.2, cex.main=1.4,
       xlab="Degree", ylab="Cumulative Frequency",
       main='Cumulative Frequency of CMV sub degree')
  
  deg <- degree(subs2, mode="all")
  hist(deg, breaks=20, xlab="Degree",
       cex.lab=1.2, cex.main=1.4,
       main="Histogram of TD sub degree")
  
  deg.dist <- degree_distribution(subs2, cumulative=T, mode="all")
  plot(rev(deg.dist),  pch=19, cex=1.2, col="orange", 
       xlab="Degree", ylab="Cumulative Frequency",
       cex.lab=1.2, cex.main=1.4,
       main='Cumulative Frequency of TD sub degree')
  
  dev.off()
}


## summary statistics

#cmv mods
vcount(mods1)
range(V(mods1)$degree)
mean(V(mods1)$degree)
sd(V(mods1)$degree)
ecount(mods1)
edge_density(mods1) #prop edges of all possible edges
transitivity(mods1) # ratio of triangles to connected triples
centr_degree(mods1)$centralization # centralisation measure

#cmv subs
vcount(subs1)
range(V(subs1)$degree)
mean(V(subs1)$degree)
sd(V(subs1)$degree)
range(E(subs1)$weight)
mean(E(subs1)$weight)
sd(E(subs1)$weight)
ecount(subs1)
edge_density(subs1) #prop edges of all possible edges
transitivity(subs1) # ratio of triangles to connected triples
centr_degree(subs1)$centralization # centralisation measure

#td mods
vcount(mods2)
range(V(mods2)$degree)
mean(V(mods2)$degree)
sd(V(mods2)$degree)
range(E(mods2)$weight)
mean(E(mods2)$weight)
sd(E(mods2)$weight)
ecount(mods2)
edge_density(mods2) #prop edges of all possible edges
transitivity(mods2) # ratio of triangles to connected triples
centr_degree(mods2)$centralization # centralisation measure

#td subs
vcount(subs2)
range(V(subs2)$degree)
mean(V(subs2)$degree)
sd(V(subs2)$degree)
range(E(subs2)$weight)
mean(E(subs2)$weight)
sd(E(subs2)$weight)
ecount(subs2)
edge_density(subs2) #prop edges of all possible edges
transitivity(subs2) # ratio of triangles to connected triples
centr_degree(subs2)$centralization # centralisation measure


