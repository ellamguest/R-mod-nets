library(igraph)
library(RColorBrewer)
library(extrafont)
loadfonts()


## TD NETWORK
edges = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/edgelist_subset.csv')
nodes = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/nodelist_subset.csv')

net = 
  graph_from_data_frame(d=edges, vertices=nodes) %>%
  simplify(remove.loops = T)

V(net)$type = nodes$type # make bipartite

# set colour to 'mod type' of node
V(net)$mod_type = nodes$mod_types
cols = c('black', 'green','royalblue','midnightblue','indianred','darkred')
V(net)$color = cols[V(net)$mod_type]

# remove non-td mods
net = delete_vertices(net, V(net)[V(net)$mod_type==2])
V(net)$degree = degree(net)
net = delete_vertices(net, V(net)[V(net)$degree==0])

# remove TD
net = delete_vertices(net, V(net)[V(net)$name=="The_Donald"])
net = delete_vertices(net, V(net)[V(net)$degree==0])

# Set visual parameters
E(net)$arrow.size = 0
V(net)$label.cex = 1
V(net)$shape=V(net)$type
V(net)$shape=gsub("1","none",V(net)$shape)
V(net)$shape=gsub("0","circle",V(net)$shape)


pr=bipartite.projection(net)
mods = pr$proj1
V(mods)$degree = degree(mods)
mods = delete_vertices(mods, V(mods)[V(mods)$degree==0]) #remove isolates
#mods = delete_vertices(mods, V(mods)[V(mods)$degree==1])
### PLOT
l=layout_with_fr(mods)

# tkid <- tkplot(mods, layout=l) #tkid is the id of the tkplot that will open
# l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
# plot(mods, layout=l)



g.components <- clusters(mods)
# which is the largest component
ix <- which.max(g.components$csize)
# get the subgraph correspondent to just the giant component
g.giant <- induced.subgraph(mods, which(g.components$membership == ix))

l=layout_with_fr(g.giant)

png('td-comod-net.png', width=15,height=9.27, units='in', res=100)
plot(g.giant, vertex.frame.color=NA, edge.arrow.size=0,
     layout=l,
     vertex.size = V(g.giant)$degree/2.5,
     vertex.color = adjustcolor(V(g.giant)$color, alpha.f = .9),
     vertex.label = NA)
#title('r/The_Donald Co-Moderation Network', cex.main=3, line=2)
legend(x=-2, y=0.3, c("Former non-top mod","Former top mod", "Current non-top mod", "Current top-mod"), pch=21,
       pt.bg=c('royalblue','midnightblue','indianred','darkred'), pt.cex=5, cex=2, bty="n")
dev.off()


png('td-comod-net-label.png')
plot(mods, vertex.frame.color=NA, edge.arrow.size=0,
     layout=layout,
     vertex.size = 10,
     vertex.color = adjustcolor(V(mods)$color, alpha.f = .8),
     vertex.label = ifelse(V(mods)$type==1, V(mods)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.family = 'Arial',
     vertex.label.color='black')
title('r/The_Donald Co-Moderation Network', cex.main=1)
legend(x=-1.5, y=1, c("Former non-top mod","Former top mod", "Current non-top mod", "Current top-mod"), pch=21,
        pt.bg=c('light blue','dark blue','pink','dark red'), pt.cex=2, cex=.8, bty="n")
dev.off()


#loking at top mod subreddits
e = unique(edges)
subset(e, name=='FlairYourPostBot') #
former = c('purpletricycle','GayLubeOil','TheGhostOfTzvika','NYPD-32', 'Ditario')
subset(e, name %in% former)

current = c('Treteste','Knollsit','OhSnapYouGotServed')
subset(e, name %in% current)
