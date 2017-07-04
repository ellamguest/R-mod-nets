library(sna)
install.packages('blockmodel')

g.p<-sapply(runif(20,0,1),rep,20)  #Create a matrix of edge 
#probabilities
g<-rgraph(20,tprob=g.p)            #Draw from a Bernoulli graph 
#distribution

#Cluster based on structural equivalence
eq<-equiv.clust(g)

#Form a blockmodel with distance relaxation of 10
b<-blockmodel(g,eq,h=10)
plot(b)                            #Plot it
