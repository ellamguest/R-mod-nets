library(igraph)
library(RColorBrewer)
library(extrafont)
loadfonts()

subreddit = 'td'
subname = 'The_Donald'
date = Sys.Date()
# date = 2017-06-01

# subreddit = 'cmv'
# subname = 'changemyview'
# date = Sys.Date()

## TD NETWORK
edges = read.csv(
  sprintf('/Users/emg/Programming/GitHub/mod-timelines/moding-data/%s/%s/lists/edgelist.csv',
          subreddit,
          date))

nodes = read.csv(
  sprintf('/Users/emg/Programming/GitHub/mod-timelines/moding-data/%s/%s/lists/nodelist.csv',
          subreddit,
          date))

net = graph_from_data_frame(d=edges, vertices=nodes)
net = simplify(net, remove.loops = T)



#### ONE-MODE MOD NETWORK
V(net)$type = nodes$type # allows bipartite bipartite
V(net)$color = nodes$mod_type
net = delete_vertices(net, V(net)[V(net)$name==subname])
V(net)$degree = degree(net)
pr=bipartite.projection(net)
mods = pr$proj2





# ONE MODE SUBS NETWORK
subs=pr$proj1
subs = delete_vertices(subs, V(subs)[V(subs)$name=='r/0'])
subs = delete_vertices(subs, V(subs)[V(subs)$name=='r/Not Found'])

plot(subs, vertex.label.cex=0.5)

# remove isolate subs
subs2 = delete_vertices(subs, V(subs)[V(subs)$degree==1])
plot(subs2)

### components
clu = components(subs2)
g = groups(clu)
giant = induced.subgraph(subs2, which(clu$membership==1))
V(giant)$subdegree = degree(giant)

plot(giant, vertex.label=NA, vertex.size=log(V(giant)$subdegree), layout=layout_with_fr)




### PLOTTING mod network


cols = c('royalblue','midnightblue','indianred','darkred')
V(mods)$color = cols[V(mods)$color]

layout= layout_with_fr(net)

png(
  sprintf('%s-comod-net-%s.png',
          subreddit, date),
  width=960, height=(960/2))

layout= layout_with_fr(giant)
plot(giant, vertex.frame.color='black', edge.arrow.size=0,
     layout=layout,
     vertex.size = (V(giant)$degree),
     #vertex.size = 5,
     #vertex.color = adjustcolor(V(giant)$color, alpha.f = 0.85),
     vertex.label = NA)
title(sprintf(
  'r/%s Co-Moderation Network', subname)
  , cex.main=1)
legend(x=-1.75, y=1.4, c("Former non-top mod","Former top mod", "Current non-top mod", "Current top-mod"), pch=21,
       pt.bg=c('royalblue','midnightblue','indianred','darkred'), pt.cex=2, cex=.8, bty="n")

dev.off()
# 
# png(
#   sprintf('%s-comod-net-labelled-%s.png',
#           subreddit, date))

plot(giant, vertex.frame.color=NA, edge.arrow.size=0,
     vertex.frame.color='black',
     layout=layout,
     vertex.size = 7,
     vertex.color = NA,
     vertex.label.cex = 1,
     vertex.label.family = 'Arial',
     vertex.label.color=V(giant)$color)
title(sprintf('r/%s Co-Moderation Network', name),
      cex.main=1)
# legend(x=-1.5, y=1, c("Former non-top mod","Former top mod", "Current non-top mod", "Current top-mod"), pch=21,
#        pt.bg=c('royalblue','midnightblue','indianred','darkred'), pt.cex=2, cex=.8, bty="n")

dev.off()




#### ONE-MODE SUB NETWORK
subs = pr$proj1

### PLOTTING
layout2= layout_with_fr(subs)

png(
  sprintf('%s-sub-net-%s.png',
          subreddit, date))

plot(subs, vertex.frame.color='black', edge.arrow.size=0,
     layout = layout2,
     vertex.size = 5,
     vertex.color = 'red',
     vertex.label = NA)
title(sprintf('r/%s Co-Moderation Network',name), cex.main=1)

dev.off()

png(
  sprintf('%s-sub-net-labelled-%s.png',
          subreddit, date))

plot(subs, vertex.frame.color=NA, edge.arrow.size=0,
     vertex.frame.color='black',
     layout=layout2,
     vertex.size = 7,
     vertex.color = NA,
     vertex.label.cex = 0.75,
     vertex.label.family = 'Arial',
     vertex.label.color='black')
title(sprintf('r/%s Co-Moderation Network', name), cex.main=1)

dev.off()


## analyses
clu = components(tdsubs)
g = groups(clu)
giant = induced.subgraph(subs, which(clu$membership==1))
c3 = induced.subgraph(subs, which(clu$membership==3))
c4 = induced.subgraph(subs, which(clu$membership==4))
c5 = induced.subgraph(subs, which(clu$membership==5))

