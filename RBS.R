library(meetupapi)
library(RNeo4j)
library(httr)
library(jsonlite)
library(xml2)
library(magrittr)
library(dplyr)
library(ROAuth)
library(data.table)
library(purrr)
library(stringr)
library(qdapRegex)
library(visNetwork)
library(igraph)
library(tidyverse)

api_key <- "6248262546e24d37a4a4443d3039"


base <- "https://api.meetup.com/2/groups?&sign=true&photo-host=public&lat=28.7041&lon=77.1025&radius=20&key="
searchurl <- paste0(base,api_key)


groupsD <- GET(searchurl)
groupsD$headers$`content-type`

gd <- content(groupsD, as="text",encoding = "UTF-8")
gd_t <- gd %>% fromJSON()
groups <- gd_t$results

#Loop through the 'next' URLs
nexturl <- gd_t$meta$`next`
datalist <- list()
for(i in 1:9){
  groupsD <- GET(nexturl)
  gd <- content(groupsD, as="text",encoding = "UTF-8")
  gd_t <- gd %>% fromJSON()
  datalist[[i]] <- gd_t$results
  if(!is.null(gd_t$meta$`next`)){
    nexturl <- gd_t$meta$`next`
  } 
  else {break}
}

groups1 <- select(groups, id,name,urlname,link,rating,created,description)
members1 <- select(groups$organizer, member_id,mname=name)
G <- as.data.frame(cbind(groups1,members1))
x <- map(datalist, ~select(.x,id,name,urlname,link,rating,created,description))
x <- rbindlist(x)
members2 <- map(datalist, ~select(.x$organizer, member_id,mname=name))
members2 <- rbindlist(members2)
X <- as.data.frame(cbind(x,members2))
combined <- rbind(G,X)

#Get topics

topics1 <- rbindlist(groups$topics)
topics2 <- map(datalist, ~rbindlist(.x$topics))
topics2 <- rbindlist(topics2)


elength <- vector("numeric",200)
for(i in 1:200){
  elength[i] <- nrow(as.data.frame(groups$topics[i]))
}
gid1 <- rep(groups$id, elength)


elength2 <- unlist(map(datalist, ~ sapply(.x[["topics"]], NROW)))
gid2 <- rep(X$id,elength2)

topics1 <- cbind(topics1,gid1)
topics2 <- cbind(topics2,gid2)
colnames(topics2)[4] <- "gid1"
combined_topics <- rbind(topics1,topics2)
write_csv(combined,"groups.csv")
write_csv(combined_topics,"groups_topics.csv")

######################################################################
graph = startGraph("http://localhost:7474/db/data/",
                   username = "neo4j",
                   password = "dhiraj0401")
summary(graph)
#browse(graph)

query="
MATCH (topic:Topic)<-[:HAS_TOPIC]-()-[:HAS_TOPIC]->(other:Topic)
WHERE ID(topic) < ID(other)
RETURN topic.name, other.name, COUNT(*) AS weight
"

edges = cypher(graph, query)
head(edges)

nodes <- data.frame(id = unique(c(edges$topic.name, edges$other.name)))
head(nodes)
ig <- graph_from_data_frame(edges,directed=F)
clusters <- cluster_walktrap(ig)

#clusters <- cluster_edge_betweenness(ig)
nodes$label <- nodes$id
nodes$group = clusters$membership
#nodes$value <- betweenness(ig)
# visNetwork(nodes,edges)
visIgraph(ig)
toVisNetworkData(ig)

#Write the result back to neo4j
query="
UNWIND {params} AS p
MATCH (t:Topic {name: p.id})
MERGE (cluster:Cluster {name: p.group})
MERGE (t)-[:IN_CLUSTER]->(cluster)
"
cypher(graph,query,params=nodes)

######################################################
#Get memebers of Backpackers Club Of Delhi
combined <- read.csv("combined.csv")

members <- vector("list",length(combined$id))
for(i in seq_along(combined$id)){
  print(i)
  murl <- paste0("https://api.meetup.com/2/members?group_id=",paste0(combined$id[[i]],"&key=6248262546e24d37a4a4443d3039"))
  md <- GET(murl)
  mdc <- content(md, as="text",encoding = "UTF-8")
  mdt <- mdc %>% fromJSON()
  if(length(mdt$results)==0|is.null(mdt$results)) next
  members[i] <- mdt
}

mlength <- unlist(map(members, ~ length(.x[["id"]])))
groupID <- rep(combined$id,mlength)
mid <- unlist(map(members, ~.x$id))
mname <- unlist(map(members, ~.x$name))

mems <- as.data.frame(cbind(mid,mname,groupID))

write.csv(mems,"members.csv")
