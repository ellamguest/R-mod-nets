library("igraph")
library(RColorBrewer)

# IMPORT DATA
sub = 'cmv'
version = 2
filename="visuals/cmv_two_mode_net.png"
filename2="visuals/cmv_two_mode_net(mods).png"
main='r/changemyview Co-moderation Network'
width=1300
height=1300

### CREATE NET FROM DATA
edges = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/edgelist_shared%s.csv', sub, version), header=T)
nodes = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/nodelist_shared%s.csv', sub, version), header=T)
net <- graph_from_data_frame(d=edges, vertices=nodes)
net <- simplify(net, remove.loops = T)
V(net)$type = V(net)$mode # make bipartite
V(net)$shape=V(net)$mode
V(net)$shape=gsub("1","square",V(net)$shape)
V(net)$shape=gsub("0","circle",V(net)$shape)
V(net)$font=V(net)$type + 1


clp <- cluster_label_prop(net)
plot(clp, net, edge.arrow.size=0, vertex.label = NA, vertex.color=V(net)$color)

cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net), vertex.label = NA)

V(net)$community <- cfg$membership
colrs <- adjustcolor(palette(rainbow(7)), alpha=1)
colrs  = brewer.pal(7,"Dark2")
V(net)$color=colrs[V(net)$community]
V(net)$label.color= V(net)$color


subs = sum(V(net)$mode)
mods = vcount(net)-subs

png(filename="visuals/cmv_two_mode_net(comms).png", width=1500,height=1500)
plot(net, edge.arrow.size = 0, 
     layout = layout,
     edge.curved = 0.2,
     #vertex.shape = ifelse(V(net)$type<1, "circle", "none"),
     vertex.shape='none',
     vertex.size=V(net)$type*2, 
     #vertex.label = ifelse(V(net)$type>0, V(net)$name, NA),
     vertex.label.family='Arial',
     vertex.label.color= V(net)$color,
     vertex.label.cex=(V(net)$type+1)*1.5,
     vertex.label.font=V(net)$font)
title(main, cex.main=3)
text(0,1,sprintf('# mods = %s, # subs = %s',mods,subs),cex=2, font=3)
dev.off()
