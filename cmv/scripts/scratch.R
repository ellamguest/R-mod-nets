library("igraph")

# IMPORT DATA
mat = read.csv('/Users/emg/Programmming/GitHub/the_donald_R/day_mod(10+days)_matrix.csv', header=T)
#mat = read.csv('/Users/emg/Programmming/GitHub/the_donald_R/mod(10+days)_matrix.csv', header=T)

#att = read.csv('default_attributes.csv', header=T)

# CREATE NETWORK OBJECT
mat = as.matrix(mat)
net = graph_from_adjacency_matrix(mat, weighted=T, directed=F)
net <- simplify(net, remove.loops = T)

# CHECK EDGE WEIGHTS ATTACHED
E(net)$weight
E(net)$arrow.size = 0

# ADD NODE ATTRIBUTES
V(net)$subscribers = att$subscribers
V(net)$subreddit = unlist(att$subreddit) # returns actually alphabetically ordered id numbers - curious!
V(net)$color = 'lightblue'
V(net)$size = 5
V(net)$label.cex = 1
V(net)$label.font= 1
V(net)$label.color = 'black'
V(net)$degree <- degree(net, mode="all")
E(net)$width = ifelse(E(net)$weight>=10, 9, E(net)$weight) # change sci-asksci weight from 92 to 9
E(net)$color = ifelse(E(net)$weight>=10, 'red', 'grey')

plot(net, layout=layout_with_fr, vertex.size = V(net)$subscribers / 4000000,
     vertex.frame.color=NA)

per = c(31, 27, 11, 2, 20, 47, 35, 8, 33, 49)
inc.edges = incident_edges(net, per, mode='all')

ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[per] <- "gold"
plot(net, vertex.color=vcol, edge.color=ecol)
