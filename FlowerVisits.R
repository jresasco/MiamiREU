# Flower visits activity Miami University REU
# J. Resasco, July 2019


# 
library(bipartite) 
library(reshape2)
library(igraph)

# set working directory
setwd("/Users/julianresasco/Documents/Presentations/Miami University REU/Pollination Activity")

# load data into R
Visits <- read.csv("FlowerVisitsMRSJuly.csv")

# let's look at the column names and the first few rows of data
head(Visits)

# make a matrix of plants (rows) x pollinaotrs (columns)
# cast long data to interaction matrix (using 'reshape2' package)
NetMat <- acast(Visits, Plant_sp~Pol_sp, value.var="Frequency")
# let's take a peek at the matrix
head(NetMat)

# function to make quantitative matrices qualitative (i.e, binary, 1/0)
quant2bin <-function(matr)
{
    ij<-which(matr!=0,arr.ind=T)
    matrb<-matrix(0,dim(matr)[1],dim(matr)[2])
    matrb[ij]<-1
    return(matrb)
}

# NETWORK VISUALIZATION

# visualizing the nested matrix with visweb (using 'bipartite' package)
visweb(NetMat,labsize=3) # shades of gray indicate freq.
visweb(quant2bin(NetMat)) # as binary

# visualizing the nested matrix with plot (using 'bipartite' package)
plotweb(NetMat, method="normal",arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="tomato", col.low="yellow green", high.lablength=3, low.lablength=3)

#igraph plot
net <- graph_from_incidence_matrix(NetMat, weight = T)
polcol = rep("tomato",dim(NetMat)[2])
plantcol = rep("yellow green",dim(NetMat)[1])
clrs = rbind(as.matrix(plantcol),as.matrix(polcol))
V(net)$label = NA
V(net)$color = clrs
E(net)$width = sqrt(E(net)$weight) # adjust desired link weight
V(net)$size = 6 # adjust node size
l <- layout_in_circle(net)
plot(net, layout=l)


# Network structure
# Network structure has implications for the coexistence and stability of species and coevolutionary processes (Bascompte and Jordano 2007). Let's explore network structure.

# A node (here species) is characterized by its degree, number of links to  other nodes. A first measure of network structure is based on the  concept of degree distribution, i.e., the frequency distribution of the  number of links per node 

#plot degree distribution and exponential, power-law, truncated power-law (in black, dark grey and light grey, respectively).
degreedistr(NetMat)

# Connectance - of all the possible links that could be made between plants and pollinators, only a small proportion are actually realized
networklevel(NetMat,index="connectance")

#Nestedness – If we rank plants and pollinators by the number of partners they have, we find this phenomenon  - Specialist tend to interact with generalists and generalists tend to interact with other generalists.  
nested(NetMat,method="NODF")
nested(NetMat,method="weighted NODF")

#Modularity – Some pollination networks show some degree of modularity where there are subsets of pollinators that that tend to interact with subsets of plants
moduleWebObject = computeModules(NetMat)
plotModuleWeb(moduleWebObject)
printoutModuleInformation(moduleWebObject)
