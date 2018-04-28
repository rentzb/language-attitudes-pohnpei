
library(tidyverse)
library(igraph)

setwd("~/Documents/UH/dissertation/r_code")

##########################################################
########   Data Import ##################################
#########################################################
data <- read.csv("network.csv")
data.selected <- data %>% select(1,3)
data.selected2 <- data %>% select(3,2)
data.selected2 <- data.selected2[-c(5,9,10,18,19,21,22,26,27,29,30),]
#summary(as.factor(data.selected$activity))
#summary(as.factor(data.selected2$activity))

##########################################################
########  GGally plot  ##################################
#########################################################

library(ggnetwork)
library(GGally)

network.data<- network(data.selected,
       ignore.eval = FALSE,
       names.eval = "weights")

ggnet2(network.data,label=T)


network.data %v% "domain" = c("family","work","language","work","work","language","family","language","work","family","education","education","education","education","work","education","work")
ggnet2(network.data,label=T,
       color="domain",size="domain",palette="Set2",
       size.palette=c("language"=5,"education"=2,"work"=2,"family"=2),
       label.size=4)
ggsave("network.png",heigh=10,width=15,units="in")

##########################################################
########  igraph plot  ##################################
#########################################################


#data.selected.matrix <- as.matrix(data.selected)
igraph.data<-igraph::graph_from_data_frame(data.selected)
igraph.data <- simplify(igraph.data, remove.multiple = F, remove.loops = T)
clp<-igraph::cluster_label_prop(igraph.data)
node.size= c(10,10,10)
plot(clp,igraph.data,edge.arrow.size=.1,vertex.label.cex=.6,vertex.size=5,#node.size*0.25,
     margin=c(0,0,0,0),vertex.label.family="Helvetica",vertex.label.font= 1,asp=0,rescale=T)#,rescale = F, ylim=c(5,20),xlim=c(3,20))
#plot(igraph.data,layout=l)
#tkplot(clp,igraph.data)
#rglplot(igraph.data)
#ggsave("network2.png",heigh=10,width=15,units="in")

##########################################################
######## other domains  ##################################
#########################################################
domains.gathered <- survey_data_good %>% select(18:42) %>% gather(key="domain",value="language")

network.data2<- network(domains.gathered,
                       ignore.eval = FALSE,
                       names.eval = "weights")

ggnet2(network.data2,label=T)


igraph.data<-igraph::graph_from_data_frame(domains.gathered)
igraph.data <- simplify(igraph.data, remove.multiple = F, remove.loops = T)
clp<-igraph::cluster_label_prop(igraph.data)
plot(clp,igraph.data,edge.arrow.size=.1)

