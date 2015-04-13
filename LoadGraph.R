# Cargar grafos
library(igraph)
library(NetData)
data(kracknets, package = "NetData")

# Reduce to non-zero edges and build a graph object

krack_full <- graph.data.frame(subset(krack_full_data_frame, 
                                      (advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0)),
                               vertices = cbind(id=seq(1,nrow(attributes)),
                                               attributes)) 

# Create sub-graphs based on edge attributes
krack_advice <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "advice_tie")==0])

krack_friendship <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "friendship_tie")==0])

krack_reports_to <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "reports_to_tie")==0])

rm(attributes,friendship_data_frame,krack_full_data_frame,reports_to_data_frame,
   advice_data_frame)
