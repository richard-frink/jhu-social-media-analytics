#########################################
#    Exponential Random Graph Model     #
#           Example Problem             #
#           Gray's Anatomy              #
#                                       #
#   Data: Garry Weissman, 2011          #
#                                       #
#########################################

#install.packages("statnet")
library("statnet")

#Simple example using a 3-node network
n<-network.initialize(3, directed=T) #generate an empty 3 node network
n[1,2]<-1  #assign a single link between node 1 and node 2
gplot(n)  #plot the network

e1<-ergm(n~edges)  #conduct an ergm using only the edges term (see lecture slides)
summary(e1)

# Load Data: Requires data sets 'gray-adj.csv' & 'gray-attr.csv'
ga.matrix<-as.matrix(read.table("twitter_relationships.csv", #the name of the adjacency matrix
                                sep=",", #the spreadsheet uses commas to separate cells
                                header=T, #because there is a header with node ID
                                row.names=1, #because the first column has node ID
                                quote="\""))

ga.attr<-read.csv("twitter_attributes.csv",  #the name of the attributes file
                  header=TRUE, 
                  sep=",", 
                  stringsAsFactors = FALSE)

#Convert the data into a network object in statnet
ga.net<-network(ga.matrix, 
                vertex.attr = ga.attr,
                vertex.attrnames = colnames(ga.attr),
                directed=T, 
                loops=F, 
                multiple=F, 
                bipartite=F, 
                hyper=F)

# Plot the network
plot(ga.net, 
     vertex.col=c("blue","pink")[1+(get.vertex.attribute(ga.net, "sex")=="F")],
     label=get.vertex.attribute(ga.net, "vertex.names"), 
     label.cex=.7)

# Conduct Exponential Random Graph Models (ERGM) Analysis

e2<-ergm(ga.net~edges)  #Create a restricted model with just edges term
summary(e2)

e3<-ergm(ga.net~edges+triangle)  #include a triadic effect in the model
summary(e3)

e4<-ergm(ga.net~edges+triangle+nodematch("sex"))  #Create an unrestricted model
summary(e4)

e5<-ergm(ga.net~edges+nodematch("race"))  #Testing a racial homophily hypothesis
summary(e5)

# Not covered: goodness of fit testing and dealing with degenerate models