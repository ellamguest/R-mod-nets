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
V(net)$shape=V(net)$type
V(net)$color=V(net)$type
V(net)$shape=gsub("1","square",V(net)$shape)
V(net)$shape=gsub("0","circle",V(net)$shape)
V(net)$font=V(net)$type + 1
E(net)$arrow.size = 0

layout = layout.kamada.kawai(net)

# simple plot of two-mod
# plot(net,
#      layout = layout)

#
pr=bipartite.projection(net)
subs = pr$proj2
V(subs)$degree = degree(subs)


layout2 = layout.kamada.kawai(subs)
layout3 = layout.fruchterman.reingold(subs)

plot(subs.sp,
     layout = layout3,
     vertex.shape = 'none',
     edge.width=E(subs.sp)$weight,
     vertex.label.font=2,
     vertex.label.cex=(log(V(subs)$degree))/3)


# sparsifying network
hist(E(subs)$weight)
mean(E(subs)$weight)
sd(E(subs)$weight)

cut.off <- mean(E(subs)$weight)
subs.sp <- delete_edges(subs, E(subs)[weight<2])
plot(subs.sp) 

layout3 = layout.fruchterman.reingold(subs.sp)

plot(subs.sp,
     layout = layout3,
     vertex.shape = 'none',
     edge.width=E(subs.sp)$weight,
     vertex.label.font=2,
     vertex.label.cex=(log(V(subs)$degree))/3)

# 
# # w/ clustering
# cfg <- cluster_fast_greedy(as.undirected(net))
# V(net)$community <- cfg$membership
# colrs  = brewer.pal(length(cfg),"Dark2")
# V(net)$color=colrs[V(net)$community]
# V(net)$label.color= V(net)$color
# 

# 
# layout = layout.kamada.kawai(subs)
# colrs  = brewer.pal(length(cfg),"Dark2")
# clustering = cluster_leading_eigen(subs)
# V(subs)$community <- clustering$membership
# 
# plot(clustering, subs, edge.arrow.size = 0,
#      layout = layout,
#      edge.curved = 0.2,
#      vertex.shape='none',
#      vertex.label.cex=1,
#      vertex.label.color = 'black',
#      vertex.label.font=2,
#      edge.width=E(subs)$weight,
#      edge.lty='dotted')
# title('CMv mod net, cle clustering', cex.main=3)
# text(1,1,sprintf('# subs = %s',vcount(subs)),cex=2, font=3)
# theme(text=element_text(family="Garamond", size=14))
