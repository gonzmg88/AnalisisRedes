
library(networkD3)
rm(list=ls())
data(kracknets, package = "NetData")
friendship_data_frame = subset(friendship_data_frame,friendship_tie > 0)
friendship_data_frame$ego = friendship_data_frame$ego -1
friendship_data_frame$alter = friendship_data_frame$alter -1
Nodos = cbind(id=0:(nrow(attributes)-1),
              id_real=1:nrow(attributes),
              attributes)
forceNetwork(Links = friendship_data_frame, Source = "ego",
             Target = "alter",Nodes=Nodos,
             NodeID = "id_real",Group = "DEPT",height=500,width = 500)

reports_to_data_frame = subset(reports_to_data_frame,reports_to_tie > 0)

reports_to_data_frame$ego = reports_to_data_frame$ego -1
reports_to_data_frame$alter = reports_to_data_frame$alter -1
forceNetwork(Links = reports_to_data_frame, Source = "ego",
             Target = "alter",Nodes=Nodos,
             NodeID = "id_real",Group = "DEPT",height=500,width = 500)
