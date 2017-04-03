library(igraph)
library(RColorBrewer)
library(extrafont)
loadfonts()

edges = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/edgelist_subset.csv')
nodes = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/nodelist_subset.csv')

net = 
  graph_from_data_frame(d=edges, vertices=nodes) %>%
  simplify(remove.loops = T)

V(net)$type = nodes$type # make bipartite

pr=bipartite.projection(net)
subs = pr$proj2
mods = pr$proj1
V(subs)$degree = degree(subs)
V(mods)$degree = degree(mods)

edge_density(subs) #prop edges of all possible edges
count_triangles(subs)
transitivity(subs) # ratio of triangles to connected triples


diameter(net, directed=F, weights=NA) # longest geodesic distnace (shortest path)

#degree histograms
par(mfrow=c(2,2))

deg <- degree(subs, mode="all")
hist(deg, breaks=20, xlab="Degree",
     main="Histogram of Subreddit Degree")

deg.dist <- degree_distribution(subs, cumulative=T, mode="all")
plot(rev(deg.dist),  pch=19, cex=1.2, col="orange", 
     xlab="Degree", ylab="Cumulative Frequency",
     main='Cumulative Frequency of Subreddt Degree')

deg <- degree(mods, mode="all")
hist(deg, breaks=20, xlab="Degree",
     main="Histogram of Moderator Degree")

deg.dist <- degree_distribution(mods, cumulative=T, mode="all")
plot(rev(deg.dist),  pch=19, cex=1.2, col="orange", 
     xlab="Degree", ylab="Cumulative Frequency",
     main='Cumulative Frequency of Moderator Degree')

par(mfrow=c(1,1))

