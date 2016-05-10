library(igraph)
library(data.table)
library(ggmap)
library(maps)
library(ggplot2)
setwd("~/Desktop/dw/Google Drive/active/R_GEO_SNA/R-Islands")


############################
######## PART 1 ############
############################

# 1) DATA
# nodes and attributes
actors <- data.table(
  name = c("A","B","C","D","E","F","G","H"),
  type = c(1,0,0,1,0,1,0,0),
  group = c(1,1,1,1,2,2,2,2)
  )
# edge list 1
edges1 <- data.table(
  V1 = c("A","A","A","B","B","A","E","E"),
  V2 = c("B","C","D","C","D","E","F","G")
)
# edge list 2
edges2 <- data.table(
  V1 = c("A","A","A","B","B","A","E","E","C","A","E"),
  V2 = c("B","C","D","C","D","E","F","G","H","H","H")
)
# what do we have?
# a population
actors
# 2 different edge lists for that population
# note that you can also set edge attributes, give edges continuous values
edges1
edges2


# 2) create igraph objects
# many ways to do this, but I like this method best
# note that for g2 we don't specify vertices - that is OK if we have no isolates
# if we have no isolates and don't care about actor level attributes
g1 <- graph.data.frame(edges1, directed=FALSE, vertices=actors )
g2 <- graph.data.frame(edges2, directed=FALSE )
print(g1, e=TRUE, v=TRUE)
print(g2, e=TRUE, v=TRUE)
# you can think of the graph object as an adjacency matrix
# igraph handles indexing
# note that it is symmetrical, per the directed=FALSE option
# note that edges are binary and have no attributes
get.adjacency(g1)
get.adjacency(g2)
# you can think of this as a picture
# note that the layout has no intrinsic meaning, and will change each time you run the command
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, vertex.label.dist=3)
plot(g2,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, vertex.label.dist=3)


# 3) Measurements
# at the node level: variance in position
# degree, eigenvector, and constraint are measures of "centrality"
# note that eigenvector is not actually meaningful for isolates (H)
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, vertex.label.dist=3)
data.table(node=V(g1)$name,degree=degree(g1),evcent=evcent(g1)$vector,constraint=constraint(g1))
# dyad level - path lengths
shortest.paths(g1)
get.shortest.paths(g1,"F","B")$vpath[[1]]
# networks 
# density = ratio of connected actors
# transitivity ~= clustering
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, vertex.label.dist=3)
data.table(density=graph.density(g1),trans=transitivity(g1))
plot(g2,vertex.label=V(g2)$name, layout=layout.fruchterman.reingold, vertex.label.dist=3)
data.table(density=graph.density(g2),trans=transitivity(g2))


# 4) more elaborate pictures
# using inputted actor attribute coding
# you may need to adjust the scaling
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, 
     vertex.color=V(g1)$type+1, vertex.size=V(g1)$group*10, vertex.label.dist=3)
# using computed actor attribute coding - eigenvector centrality
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, 
     vertex.color=V(g1)$type+1, vertex.size=evcent(g1)$vector*20, vertex.label.dist=3)
# using computed actor attribute coding - constraint
# constraint is a measure of brokerage: lower scores = less constraint
# reverse it (it has a range of 0 to 1.125) so that higher score = greater brokerage
# set missing values to zero
con <- (1.125-constraint(g1))
con[is.na(con)] <- .125
plot(g1,vertex.label=V(g1)$name, layout=layout.fruchterman.reingold, 
     vertex.color=V(g1)$type+1, vertex.size=con*20, vertex.label.dist=3)

############################
######## PART 2 ############
############################

# 5) a real world example
rm(list=ls())
# data on academic articles about "Atlantic Cod" published 2012-2014
# 1117 Articles published in ISI indexed journals
articles <- fread("articles.csv", sep=",")
View(articles[1:100])
# 5842 author credit lines - one record per author or affiliation
authors <- fread("authors.csv", sep=",")
View(authors[1:100])
# I have already added latitude/longitude coding
# add geo-coding to author list with the commented code below
# note that Google allows 2500 queries per day
# addr_unique <- authors[,.SD[1],by=addr_short]
# addr_unique <- addr_unique[,list(addr_short),]
# addr_unique <- cbind(
#   addr_unique,
#   geocode(addr_unique$addr_short, source="google",output="latlon")
# )
# authors <- merge(authors,addr_unique,by="addr_short",allow.cartesian=TRUE)
# authors[,lon:=round(lon,digits=1)]
# authors[,lat:=round(lat,digits=1)]
# geocodeQueryCheck()

# make a node list of unique author names and attributes
# 3796 unique author names
# For those with multiple listing we are grabbing the first record
actors <- authors[,.SD[1],by=author]
# code academic = 1 if the address string contains "Univ" or "Inst"
# quick and dirty coding of affiliation type
actors[,academic:=as.integer(regexpr("Univ|Inst",actors$address)>0)]
actors[,.N,by=academic]
# code Iceland = 1 if the address string contains "Iceland"
actors[,iceland:=as.integer(regexpr("Iceland",actors$address)>0)]
actors[,.N,by=iceland]

# make an edge list based on article co-authorship
edges1 <- merge(
  authors[,list(ISIid,author)],
  authors[,list(ISIid,author)],
  by="ISIid",allow.cartesian=TRUE)
# reduce to unique pairs
edges1 <- edges1[,.SD[1],by=list(author.x,author.y)]
# drop self matches
edges1 <- edges1[author.x != author.y]
# drop residual article ID
edges1[,ISIid:=NULL]
# create and igraph object
g1 <- graph.data.frame(edges1, directed=FALSE, vertices=actors )
# 3796 nodes and 25394 edges
g1
# plot
plot(g1,vertex.label=NA, vertex.size=(V(g1)$iceland*3)+2, vertex.color=V(g1)$academic+1)


# SUBSETS
# only those with high degree
# they may not look well connected in the subset because we lose many of their connections
names2 <-  V(g1)[degree(g1) > 20]
names2
g1sub <- subgraph.edges(g1, names2)
plot(g1sub,vertex.label=NA, vertex.size=3, vertex.color=V(g1)$academic+1)


# choose the neighborhood around a person
# 3 = number of steps we are looking away from focal node 
g1sub <- make_ego_graph(g1, 3, nodes = (V(g1)$name == "Margeirsson, Sveinn"), mode = c("all"), mindist = 0)
g1sub
plot(g1sub[[1]], vertex.size=3,vertex.label=V(g1sub[[1]])$name,vertex.label.cex=.6,
     vertex.label.dist=.1, vertex.color= ( V(g1sub[[1]])$name == "Margeirsson, Sveinn")+1)


# choose the neighborhood around a set of people
# 3 = number of steps we are looking away from focal node 
g1sub <- induced.subgraph(graph=g1,vids=unlist(neighborhood(g1,2,nodes = (V(g1)$iceland == 1))))
g1sub
plot(g1sub, vertex.size=3,vertex.label=NA, vertex.color= ( V(g1sub)$iceland+1) )


############################
######## PART 3 ############
############################

# reconceptualize the network as geographic nodes
# no need to create an explicit graph object

# aggregate author date at lat/lon level
geo_nodes <- authors[,.N,by=list(lat,lon)]
View(geo_nodes[1:100])
# edges are connections between locations
geo_dyads <- merge(
  authors[,list(ISIid,lat,lon)],
  authors[,list(ISIid,lat,lon)],
  by="ISIid",allow.cartesian=TRUE)
geo_dyads <- geo_dyads[lat.x != lat.y & lon.x != lon.y]
geo_dyads <- geo_dyads[,list(tieN = .N),by=list(lat.y,lat.x,lon.y,lon.x)]
View(geo_dyads[1:100])

# overlay the visualization on a world map
world <- map_data('world')
world <- subset(world,region!="Antarctica") #Delete Antarctica

# plot the relative distribution of COD research using ggplot/ggmap
ggplot() +
  geom_polygon(dat=world, aes(long, lat,group=group ), color="grey", fill="grey80") +
  geom_point(data=geo_nodes, aes(x=lon, y=lat,size=N),color="dark blue")  

# plot links between locations using ggplot/ggmap
ggplot() +
  geom_polygon(dat=world, aes(long, lat,group=group ), color="grey", fill="grey80") +
  geom_segment(data=geo_dyads, 
               aes(x=lon.x, y=lat.x, xend=lon.y, yend=lat.y),
               alpha = 0.05,size=.25,color="blue") +
  scale_size_continuous(range=c(.001, .005), guide=FALSE) 

# combine locations and links
# ggplot visualization a little unpredictable here....
ggplot() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        legend.position=c(.1, .2)
  ) +
  scale_size_continuous(name="Author Count" )+
  ggtitle("Atlantic Cod articles - Author Locations and Links 2012-2014") +
  annotate("text",x=105, y=-55,
           label="David M Waguespack (dwaguesp@rhsmith.umd.edu)",size=2) + 
  geom_polygon(dat=world, aes(long, lat,group=group ), color="grey", fill="grey80") +
  geom_segment(data=geo_dyads, 
               aes(x=lon.x, y=lat.x, xend=lon.y, yend=lat.y),
               size=.025,color="blue",alpha = 0.05) +
  geom_point(data=geo_nodes, aes(x=lon, y=lat,size=N),color="dark blue",alpha = 0.5) +
  coord_fixed() 
ggsave("cod.pdf",width=12,height=8,units="in")

