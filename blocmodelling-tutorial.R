library(blockmodeling)

n<-8 #if larger, the number of partitions increases dramaticaly,
# as does if we increase the number of clusters
net<-matrix(NA,ncol=n,nrow=n)
clu<-rep(1:2,times=c(3,5))
tclu<-table(clu)
net[clu==1,clu==1]<-rnorm(n=tclu[1]*tclu[1],mean=0,sd=1)
net[clu==1,clu==2]<-rnorm(n=tclu[1]*tclu[2],mean=4,sd=1)
net[clu==2,clu==1]<-rnorm(n=tclu[2]*tclu[1],mean=0,sd=1)
net[clu==2,clu==2]<-rnorm(n=tclu[2]*tclu[2],mean=0,sd=1)

#we select a random parition and then optimise it
all.par<-nkpartitions(n=n, k=length(tclu))

#forming the partitions
all.par<-lapply(apply(all.par,1,list),function(x)x[[1]])
# to make a list out of the matrix

#optimizing one partition
res<-opt.par(M=net,
             clu=all.par[[sample(1:length(all.par),size=1)]],
             approach="ss", blocks="com")
plot(res) #Hopefully we get the original partition

#optimizing 10 random partitions which with opt.these.par
res<-opt.these.par(M=net,
                   partitions=all.par[sample(1:length(all.par),size=10)],
                   approach="ss",blocks="com")
plot(res) #Hopefully we get the original partition

#optimizing 10 random partitions with opt.random.par
res<-opt.random.par(M=net,k=2,rep=10,approach="ss",blocks="com")
plot(res) #Hopefully we get the original partition

#Checking all possible partitions
nkpar(n=n, k=length(tclu)) #computing the number of partitions
all.par<-nkpartitions(n=n, k=length(tclu))

#forming the partitions
all.par<-lapply(apply(all.par,1,list),function(x)x[[1]])

# to make a list out of the matrix
res<-check.these.par(M=net,partitions=all.par,approach="ss",
                     blocks="com")
plot(res) #we get the original partition

#using indidect approach - structural equivalence
D<-sedist(M=net)

plot.mat(net, clu=cutree(hclust(d=D,method="ward.D"),k=2))
