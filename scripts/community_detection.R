library("igraph")
library(RColorBrewer)

### IMPORT DATA
sub = 't_d'
version = 3
main='r/The_Donald Co-moderation Network (Sub d>2)'
width=1500
height=1500

# sub = 't_d'
# version = 2
# main='r/The_Donald Co-moderation Network'
# width=1500
# height=1500
# 
# sub = 'cmv'
# version = 2
# main='r/changemyview Co-moderation Network'
# width=1300
# height=1300

### DEFINE NETWORK
edges = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/edgelist_shared%s.csv', sub, version), header=T)
nodes = read.csv(sprintf('/Users/emg/Programmming/GitHub/R-mod-nets/%s/data/nodelist_shared%s.csv', sub, version), header=T)
net <- graph_from_data_frame(d=edges, vertices=nodes)
net <- simplify(net, remove.loops = T)
V(net)$type = V(net)$mode # make bipartite
V(net)$shape=V(net)$mode
V(net)$shape=gsub("1","square",V(net)$shape)
V(net)$shape=gsub("0","circle",V(net)$shape)
V(net)$font=V(net)$type + 1

cfg <- cluster_fast_greedy(as.undirected(net))
V(net)$community <- cfg$membership
colrs  = brewer.pal(length(cfg),"Dark2")
V(net)$color=colrs[V(net)$community]
V(net)$label.color= V(net)$color

### PLOT TWO MODE NET
# subs = sum(V(net)$mode)
# mods = vcount(net)-subs
# layout=layout.kamada.kawai(net)

# png(filename="visuals/cmv_two_mode_net(comms).png", width=1500,height=1500)
# plot(net, edge.arrow.size = 0, 
#      #layout = l,
#      layout = layout,
#      edge.curved = 0.2,
#      #vertex.shape = ifelse(V(net)$type<1, "circle", "none"),
#      vertex.shape='none',
#      vertex.size=V(net)$type*2, 
#      #vertex.label = ifelse(V(net)$type>0, V(net)$name, NA),
#      vertex.label.family='Arial',
#      vertex.label.color= V(net)$color,
#      vertex.label.cex=(V(net)$type+1)*1.5,
#      vertex.label.font=V(net)$font)
# title(main, cex.main=3)
# text(0,1,sprintf('# mods = %s, # subs = %s',mods,subs),cex=2, font=3)
# dev.off()
# 
# tkid <- tkplot(net, edge.arrow.size = 0, 
#                layout = l,
#                edge.curved = 0.2,
#                #vertex.shape = ifelse(V(net)$type<1, "circle", "none"),
#                vertex.shape='none',
#                vertex.size=V(net)$type*2, 
#                #vertex.label = ifelse(V(net)$type>0, V(net)$name, NA),
#                vertex.label.family='Arial',
#                vertex.label.color= V(net)$color,
#                vertex.label.cex=(V(net)$type+1)*1.5,
#                vertex.label.font=V(net)$font)
# l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
# tk_close(tkid, window.close = T)


### MAKE ONE MODE SUBREDDIT AFFILIATION NETWORK
pr=bipartite.projection(net)
subs = pr$proj2
V(subs)$degree = degree(subs)

### PLOT NETWORK FOR EACH CLUSTERING SET (cle,ceb,cfg)
layout = layout.kamada.kawai(subs)
colrs  = brewer.pal(length(cfg),"Dark2")


clustering = cluster_leading_eigen(subs)
V(subs)$community <- clustering$membership
cluname = 'cle'
filename = sprintf("visuals/%s_sub_net%s(%s).png", sub, version, cluname) 
png(filename=filename, width=width,height=height)
plot(clustering, subs, edge.arrow.size = 0, 
     layout = layout,
     edge.curved = 0.2,
     vertex.shape='none',
     vertex.label.cex=2.5, 
     vertex.label.family='Arial',
     vertex.label.color = 'black',
     vertex.label.font=2,
     edge.width=E(subs)$weight,
     edge.lty='dotted')
title(main, cex.main=3)
text(1,1,sprintf('# subs = %s',vcount(subs)),cex=2, font=3)
dev.off()

clustering = cluster_edge_betweenness(subs)
cluname = 'ceb'
filename = sprintf("visuals/%s_sub_net%s(%s).png", sub, version, cluname) 
png(filename=filename, width=width,height=height)
plot(clustering, subs, edge.arrow.size = 0, 
     layout = layout,
     edge.curved = 0.2,
     vertex.shape='none',
     vertex.label.cex=2.5, 
     vertex.label.family='Arial',
     vertex.label.color = 'black',
     vertex.label.font=2,
     edge.width=E(subs)$weight,
     edge.lty='dotted')
title(main, cex.main=3)
text(1,1,sprintf('# subs = %s',vcount(subs)),cex=2, font=3)
dev.off()

clustering = cluster_fast_greedy(as.undirected(subs))
V(subs)$community <- clustering$membership
cluname = 'cfg'
filename = sprintf("visuals/%s_sub_net%s(%s).png", sub, version, cluname) 
png(filename=filename, width=width,height=height)
plot(clustering, subs, edge.arrow.size = 0, 
     layout = layout,
     edge.curved = 0.2,
     vertex.shape='none',
     vertex.label.cex=2.5, 
     vertex.label.family='Arial',
     vertex.label.color = 'black',
     vertex.label.font=2,
     edge.width=E(subs)$weight,
     edge.lty='dotted')
title(main, cex.main=3)
text(1,1,sprintf('# subs = %s',vcount(subs)),cex=2, font=3)
dev.off()


# 
# #### ADJUST INTERACTIVE PLOT
tkid <- tkplot(subs, edge.arrow.size = 0,
               layout = layout,
               edge.curved = 0.2,
               vertex.shape='none',
               vertex.label.cex=2,
               vertex.label.family='Arial',
               vertex.label.color = 'black',
               vertex.label.font=2)
layout <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)

# ### ADJUSTED PLOT
# png(filename=filename, width=width,height=height)
# plot(clustering, subs, edge.arrow.size = 0, 
#      layout = l,
#      edge.curved = 0.2,
#      vertex.shape='none',
#      vertex.label.cex=2, 
#      vertex.label.family='Arial',
#      vertex.label.color = 'black',
#      vertex.label.font=2)
# title(main, cex.main=3)
# text(0,1,sprintf('# subs = %s',vcount(subs)),cex=2, font=3)
# dev.off()
