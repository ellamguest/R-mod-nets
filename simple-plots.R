library(igraph)
library(RColorBrewer)
library(extrafont)
loadfonts()

# CMV NET
edges1 = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/edgelist_subset.csv')
nodes1 = read.csv('/Users/emg/Programming/GitHub/cmv/tidy_data/nodelist_subset.csv')

net1 = 
  graph_from_data_frame(d=edges1, vertices=nodes1) %>%
  simplify(remove.loops = T)

V(net1)$type = nodes$type # make bipartite

# get one-mode net1work
pr1=bipartite.projection(net1)
subs1 = pr1$proj2
E(subs1)$arrow.size = 0
V(subs1)$degree = degree(subs1)
V(subs1)$cmv = as.integer(V(subs1)$name=='changemyview')
V(subs1)$color=V(subs1)$cmv
V(subs1)$color=gsub("1","red",V(subs1)$color)
V(subs1)$color=gsub("0","black",V(subs1)$color)
V(subs1)$color

## TD NETWORK
edges2 = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/edgelist_subset.csv')
nodes2 = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/nodelist_subset.csv')

net2 = 
  graph_from_data_frame(d=edges2, vertices=nodes2) %>%
  simplify(remove.loops = T)

# get one-mode net2work
V(net2)$type = nodes$type # make bipartite
pr2=bipartite.projection(net2)
subs2 = pr2$proj2
E(subs2)$arrow.size = 0
V(subs2)$degree = degree(subs2)

# highlight TD in red
V(subs2)$td = as.integer(V(subs2)$name=='The_Donald')
V(subs2)$color=V(subs2)$td
V(subs2)$color=gsub("1","red",V(subs2)$color)
V(subs2)$color=gsub("0","black",V(subs2)$color)
V(subs2)$color


## PLOTTING SUBREDDIT NETWORKS
png(filename='/Users/emg/Google Drive/PhD/presenting/ss_phd_seminar_april_17/visuals/cmv-sub-net.png',
    width=500,height=225)
layout1 = layout.fruchterman.reingold(subs1)
plot(subs1,
     layout = layout1,
     vertex.shape = 'none',
     edge.width=log(E(subs1)$weight),
     vertex.label.font=1,
     vertex.label.cex=0.85,
     vertex.label.color=V(subs1)$color,
     main = 'CMV subreddit network')
dev.off()

png(filename='/Users/emg/Google Drive/PhD/presenting/ss_phd_seminar_april_17/visuals/td-sub-net.png',
    width=1000,height=800)
layout2 = layout.fruchterman.reingold(subs2)
plot(subs2,
     layout = layout2,
     vertex.shape = 'none',
     edge.width=log(E(subs1)$weight),
     vertex.label.font=1,
     vertex.label.cex=1,
     vertex.label.color=V(subs2)$color,
     main = 'TD subreddit network')
dev.off()

