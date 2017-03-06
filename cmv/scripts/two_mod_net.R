library("igraph")

# IMPORT DATA
sub = 'cmv'
sub = 't_d'

edges = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/edgelist_shared.csv', sub), header=T)
nodes = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/nodelist_shared.csv', sub), header=T)

#edges = read.csv('/Users/emg/Programmming/GitHub/R-mod-nets/t_d/data/edgelist.csv', header=T)
#nodes = read.csv('/Users/emg/Programmming/GitHub/R-mod-nets/t_d/data/nodelist.csv', header=T)

net <- graph_from_data_frame(d=edges, vertices=nodes)
net <- simplify(net, remove.loops = T)

# CHECK EDGE WEIGHTS ATTACHED
#E(net)$weight
### ADDING NODE ATTIBUTES
V(net)$type = V(net)$mode # make bipartite
V(net)$color=V(net)$mode
V(net)$color=gsub("1","red",V(net)$color) #subs will be red
V(net)$color=gsub("0","blue",V(net)$color) #mods will be blue
V(net)$label.color = V(net)$color
V(net)$label.font = V(net)$type+1 # redditors = plain, subs = bold
V(net)$label.cex = 0.75
V(net)$size=3

### SELECTING FOR LARGE COMPONETS
clu = components(net, mode = c("weak", "strong"))

### PLOT GIANT COMPONENT ONLY
giant = mapply(c, groups(clu)[1], SIMPLIFY=FALSE)
sub = induced_subgraph(net, unlist(giant))

layout1=layout.kamada.kawai(net)
plot(net, vertex.frame.color=NA, edge.arrow.size=0,
     layout=layout1, vertex.label = ifelse(V(net)$type>0, V(net)$name, NA),
     vertex.shape=ifelse(V(net)$type<1, "circle", "none"))


