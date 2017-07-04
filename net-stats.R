options(scipen = 999)

subs = giant

edge_density(subs)
transitivity(subs, type="global")
mean_distance(subs, directed=F)
diameter(subs, directed=F) # longest path


deg <- degree(subs)
hist(deg, breaks=1:vcount(subs)-1, main="Histogram of node degree")

deg.dist <- degree_distribution(subs, cumulative=T)

plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency", main = 'Cumulative frequency of node degree')

datatable = matrix(c(mean(V(subs)$degree),
                     sd(V(subs)$degree),
                     min(V(subs)$degree),
                     max(V(subs)$degree),
                     '-',
                     
                     mean(E(subs)$weight),
                     sd(E(subs)$weight),
                     min(E(subs)$weight),
                     max(E(subs)$weight),
                     '-',
              
                  mean(degree(subs)),
                  sd(degree(subs)),
                  min(degree(subs)),
                  max(degree(subs)),
                  centr_degree(subs, normalized=T)$centralization,
                  
                  mean(closeness(subs)),                  
                  sd(closeness(subs)),
                  min(closeness(subs)),
                  max(closeness(subs)),
                  centr_clo(subs, mode="all", normalized=T)$centralization,
                  
                  mean(betweenness(subs, directed = F)),
                  sd(betweenness(subs, directed = F)),
                  min(betweenness(subs, directed = F)),
                  max(betweenness(subs, directed = F)),
                  centr_betw(subs, directed = F, normalized=T)$centralization
                
                  ),ncol=5)

colnames(datatable) <- c("# ties", "edge weights", "degree","closeness", "betweenness")
rownames(datatable) <- c("Mean","SD","Min", "Max", "Centrality")
t(datatable)

write.csv(t(datatable), 'td-giant-datatable.csv')


