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

# Set colors to plot the distances:
dist.from.CMV <- distances(net, v=V(net)[name=="changemyview"], 
                           to=V(net), weights=NA)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.CMV)+1)
col <- col[dist.from.CMV+1]
V(net)$dist = col

layout = layout.kamada.kawai(net)

# simple plot of two-mod
# plot(net,
#      layout = layout)

#
pr=bipartite.projection(net)
subs = pr$proj2
V(subs)$degree = degree(subs)

dist.from.CMV <- distances(subs, v=V(subs)[name=="changemyview"], 
                           to=V(subs), weights=NA)
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.CMV)+1)
col <- col[dist.from.CMV+1]
V(subs)$dist = col


layout2 = layout.kamada.kawai(subs)
layout3 = layout.fruchterman.reingold(subs)

plot(subs,
     layout = layout3,
     vertex.shape = 'none',
     edge.width=E(subs)$weight,
     vertex.label.font=2,
     vertex.label.cex=(log(V(subs)$degree))/3,
     vertex.label.color=V(subs)$dist)


# sparsifying network

layout3 = layout.fruchterman.reingold(subs)

plot(subs,
     layout = layout3,
     vertex.shape = 'none',
     edge.width=E(subs)$weight,
     vertex.label.font=2,
     vertex.label.cex=(log(V(subs)$degree))/3)

# colour distance from changemyview





plot(net, vertex.color=col, vertex.label=dist.from.CMV, edge.arrow.size=.6, 
     vertex.label.color="white")

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
