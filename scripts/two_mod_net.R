library("igraph")
library('extrafont')

# IMPORT DATA
sub = 't_d'
version = 3
filename="visuals/td_two_mode_net(d>2).png"
main='r/The_Donald Co-moderation Network (Sub d>2)'
width=1500
height=1500
# 
# sub = 't_d'
# version = 2
# filename="visuals/td_two_mode_net.png"
# main='r/The_Donald Co-moderation Network'
# width=1500
# height=1500
# 
# sub = 'cmv'
# version = 2
# filename="visuals/cmv_two_mode_net.png"
# main='r/changemyview Co-moderation Network'
# width=1300
# height=1300

### CREATE NET FROM DATA
edges = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/edgelist_shared%s.csv', sub, version), header=T)
nodes = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/nodelist_shared%s.csv', sub, version), header=T)
net <- graph_from_data_frame(d=edges, vertices=nodes)
net <- simplify(net, remove.loops = T)

### ADDING NODE ATTIBUTES
V(net)$type = V(net)$mode # make bipartite
V(net)$color=V(net)$mode
V(net)$color=gsub("1","black",V(net)$color) #subs will be red
V(net)$color=gsub("0","blue",V(net)$color) #mods will be blue
V(net)$label.color = V(net)$color
V(net)$label.font = V(net)$type+1 # redditors = plain, subs = bold
V(net)$label.cex = 2
V(net)$size=2

### SELECTING FOR LARGE COMPONETS
clu = components(net, mode = c("weak", "strong"))
giant = mapply(c, groups(clu)[1], SIMPLIFY=FALSE)
sub = induced_subgraph(net, unlist(giant))

### PLOT
layout=layout.kamada.kawai(net)

subs = sum(V(net)$mode)
mods = vcount(net)-subs

png(filename=filename, width=width,height=height)
plot(net, vertex.frame.color=NA, edge.arrow.size=0,
     layout=layout, vertex.label = ifelse(V(net)$type>0, V(net)$name, NA),
     vertex.shape=ifelse(V(net)$type<1, "circle", "none"),
     edge.curved=0.2, vertex.label.family='Arial')
title(main, cex.main=3)
#mtext(sprintf('# mods=%s, # subs = %s',mods,subs), line=-3, cex=3, font="AvantGarde")
text(0,1,sprintf('# mods = %s, # subs = %s',mods,subs),cex=2, font=3)
dev.off()

