library(igraph)
library(RColorBrewer)

subreddit = 'td'
subname = 'The_Donald'
#date = Sys.Date()
date = 2017-06-05

## create NETWORK
edges = read.csv('/Users/emg/Programming/GitHub/mod-timelines/moding-data/td/2017-06-06/lists/edgelist.csv')
nodes = read.csv('/Users/emg/Programming/GitHub/mod-timelines/moding-data/td/2017-06-06/lists/nodelist.csv')
net = graph_from_data_frame(d=edges, vertices=nodes)
net = simplify(net, remove.loops = T)

net
#### TWO-MODE NETWORK
V(net)$type = nodes$type # allows bipartite bipartite
shapes = c('square','circle')
V(net)$shape = shapes[V(net)$type + 1]
cols = c('black','royalblue','midnightblue','indianred','darkred')
V(net)$color = cols[nodes$mod_type + 1]
net = delete_vertices(net, V(net)[V(net)$name==subname])
V(net)$degree = degree(net)
#net = delete_vertices(net, V(net)[V(net)$degree==1])

layout = layout_with_fr(net)

plot(net, vertex.frame.color='black', edge.arrow.size=0,
     layout=layout,
     vertex.size = log10(V(net)$degree)*7,
     vertex.label = NA,
     vertex.label.color = V(net)$color,
     vertex.label.cex = 0.4,
     vertex.color = adjustcolor(V(net)$color, alpha.f = 0.5))
title(sprintf(
  'r/%s Full Two Mode Mod-Sub Network', subname)
  , cex.main=1)


net = delete_vertices(net, V(net)[V(net)$degree==1])

plot(net, vertex.frame.color='black', edge.arrow.size=0,
     layout=layout_with_fr,
     vertex.size = log10(V(net)$degree)*7,
     vertex.label = NA,
     vertex.label.color = V(net)$color,
     vertex.label.cex = 0.4,
     vertex.color = adjustcolor(V(net)$color, alpha.f = 0.5),
     edge.color=adjustcolor('black', alpha.f = 0.5))
title(sprintf(
  'r/%s Two Mode Mod-Sub Network (D>1)', subname)
  , cex.main=1)

pr=bipartite.projection(net)
subs=pr$proj1
V(subs)$subdegree = degree(subs)
subs = delete_vertices(subs, V(subs)[V(subs)$degree==1])

plot(subs, vertex.frame.color='black', edge.arrow.size=0,
     layout=layout_with_fr,
     vertex.size = log10(V(subs)$degree)*7,
     #vertex.label = NA,
     vertex.label.color = V(subs)$color,
     vertex.color = adjustcolor(V(subs)$color, alpha.f = 0.5),
     edge.color=adjustcolor('black', alpha.f = 0.1))
title(sprintf(
  'r/%s One Mode Sub Network (D>1)', subname)
  , cex.main=1)

### components
clu = components(subs)
g = groups(clu)
giant = induced.subgraph(subs, which(clu$membership==1))

plot(giant, vertex.label=NA, vertex.size=2, layout=layout_with_fr)

layout= layout_with_fr(giant)

plot(giant, vertex.frame.color='midnightblue', edge.arrow.size=0,
     layout=layout,
     vertex.size = coreness(giant),
     vertex.label = NA)
title(sprintf(
  'r/%s Subreddit Co-Moderation Network', subname)
  , cex.main=1)


