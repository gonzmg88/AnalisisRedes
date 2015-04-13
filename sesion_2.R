
####################################################################
# LAB 2: Methodological beginnings - Density, Reciprocity, Triads, #
# Transitivity, and heterogeneity. Node and network statistics.    #
####################################################################


# NOTE: if you have trouble because some packages are not installed, 
# see lab 1 for instructions on how to install all necessary packages. 
# Also see Lab 1 for prior functions.


##############################################################
# 
# Lab 2 
#
# The purpose of this lab is to acquire basic cohesion 
# metrics of density, reciprocity, reach, path distance, 
# and transitivity. In addition, we'll develop triadic 
# analyses and a measure of ego-network heterogenity. 
#
##############################################################



### 
# 1. SET UP SESSION
###
install.packages("NetData")

library(igraph)
library(NetData)


### 
# 2. LOAD DATA
###

# We would ordinarily need to follow the same proceedure we did for the Krackhardt data
# as we did in lab 1; see that lab for detail.

rm(list=ls())

source("LoadGraph.R")
### 
# 3. NODE-LEVEL STATISTICS
###

# Compute the indegree and outdegree for each node, first in the 
# full graph (accounting for all tie types) and then in each 
# tie-specific sub-graph. 
deg_full_in <- degree(krack_full, mode="in") 
deg_full_out <- degree(krack_full, mode="out") 
deg_full_in
deg_full_out

deg_advice_in <- degree(krack_advice, mode="in") 
deg_advice_out <- degree(krack_advice, mode="out") 
deg_advice_in
deg_advice_out

deg_friendship_in <- degree(krack_friendship, mode="in") 
deg_friendship_out <- degree(krack_friendship, mode="out") 
deg_friendship_in
deg_friendship_out

deg_reports_to_in <- degree(krack_reports_to, mode="in") 
deg_reports_to_out <- degree(krack_reports_to, mode="out") 
deg_reports_to_in
deg_reports_to_out

# Reachability can only be computed on one vertex at a time. To
# get graph-wide statistics, change the value of "vertex"
# manually or write a for loop. (Remember that, unlike R objects,
# igraph objects are numbered from 0.)

reachability <- function(g, m) {
  
  return(t(vapply(V(g), function(vertice){
    alcanzables=rep(0,vcount(g))
    v_alcanzables=subcomponent(g,vertice,mode=m)
    alcanzables[v_alcanzables] = 1
    return(alcanzables)
  },numeric(vcount(g)))))
}

reach_full_in <- reachability(krack_full, 'in')
reach_full_out <- reachability(krack_full, 'out')
reach_full_in
reach_full_out

reach_advice_in <- reachability(krack_advice, 'in')
reach_advice_out <- reachability(krack_advice, 'out')
reach_advice_in
reach_advice_out

reach_friendship_in <- reachability(krack_friendship, 'in')
reach_friendship_out <- reachability(krack_friendship, 'out')
reach_friendship_in
reach_friendship_out

reach_reports_to_in <- reachability(krack_reports_to, 'in')
reach_reports_to_out <- reachability(krack_reports_to, 'out')
reach_reports_to_in
reach_reports_to_out


# Often we want to know path distances between individuals in a network. 
# This is often done by calculating geodesics, or shortest paths between
# each ij pair. One can symmetrize the data to do this (see lab 1), or 
# calculate it for outward and inward ties separately. Averaging geodesics 
# for the entire network provides an average distance or sort of cohesiveness
# score. Dichotomizing distances reveals reach, and an average of reach for 
# a network reveals what percent of a network is connected in some way.

# Compute shortest paths between each pair of nodes. 
sp_full_in <- shortest.paths(krack_full, mode='in')
sp_full_out <- shortest.paths(krack_full, mode='out')
sp_full_in
sp_full_out

sp_advice_in <- shortest.paths(krack_advice, mode='in')
sp_advice_out <- shortest.paths(krack_advice, mode='out')
sp_advice_in
sp_advice_out

sp_friendship_in <- shortest.paths(krack_friendship, mode='in')
sp_friendship_out <- shortest.paths(krack_friendship, mode='out')
sp_friendship_in
sp_friendship_out

sp_reports_to_in <- shortest.paths(krack_reports_to, mode='in')
sp_reports_to_out <- shortest.paths(krack_reports_to, mode='out')
sp_reports_to_in
sp_reports_to_out


# Assemble node-level stats into single data frame for export as CSV.

# First, we have to compute average values by node for reachability and
# shortest path. (We don't have to do this for degree because it is 
# already expressed as a node-level value.)
reach_full_in_vec <- apply(reach_full_in, 1,mean)
reach_full_out_vec <- apply(reach_full_out, 1,mean)
reach_advice_in_vec <- apply(reach_advice_in, 1,mean)
reach_advice_out_vec <- apply(reach_advice_out, 1,mean)
reach_friendship_in_vec <-  apply(reach_friendship_in, 1,mean)
reach_friendship_out_vec <- apply(reach_friendship_out, 1,mean)
reach_reports_to_in_vec <- apply(reach_reports_to_in, 1,mean)
reach_reports_to_out_vec <- apply(reach_reports_to_out, 1,mean)


# Next, we assemble all of the vectors of node-levelvalues into a 
# single data frame, which we can export as a CSV to our working
# directory.
node_stats_df <- cbind(deg_full_in,
                       deg_full_out,
                       deg_advice_in,
                       deg_advice_out,
                       deg_friendship_in,
                       deg_friendship_out,
                       deg_reports_to_in,
                       deg_reports_to_out, 

                       reach_full_in_vec, 
                       reach_full_out_vec, 
                       reach_advice_in_vec, 
                       reach_advice_out_vec, 
                       reach_friendship_in_vec, 
                       reach_friendship_out_vec, 
                       reach_reports_to_in_vec, 
                       reach_reports_to_out_vec, 

                       sp_full_in_vec, 
                       sp_full_out_vec, 
                       sp_advice_in_vec, 
                       sp_advice_out_vec, 
                       sp_friendship_in_vec, 
                       sp_friendship_out_vec, 
                       sp_reports_to_in_vec, 
                       sp_reports_to_out_vec)

write.csv(node_stats_df, 'krack_node_stats.csv')

# Question #1 - What do these statistics tell us about
# each network and its individuals in general? 

### 
# 3. NETWORK-LEVEL STATISTICS
###

# Many initial analyses of networks begin with distances and reach, 
# and then move towards global summary statistics of the network. 
#
# As a reminder, entering a question mark followed by a function 
# name (e.g., ?graph.density) pulls up the help file for that function.
# This can be helpful to understand how, exactly, stats are calculated.

# Degree
mean(deg_full_in)
sd(deg_full_in)
mean(deg_full_out)
sd(deg_full_out)

mean(deg_advice_in)
sd(deg_advice_in)
mean(deg_advice_out)
sd(deg_advice_out)

mean(deg_friendship_in)
sd(deg_friendship_in)
mean(deg_friendship_out)
sd(deg_friendship_out)

mean(deg_reports_to_in)
sd(deg_reports_to_in)
mean(deg_reports_to_out)
sd(deg_reports_to_out)


# Shortest paths
# ***Why do in and out come up with the same results?
# In and out shortest paths are simply transposes of one another; 
# thus, when we compute statistics across the whole network they have to be the same.

mean(sp_full_in[which(sp_full_in != Inf)])
sd(sp_full_in[which(sp_full_in != Inf)])
mean(sp_full_out[which(sp_full_out != Inf)])
sd(sp_full_out[which(sp_full_out != Inf)])

mean(sp_advice_in[which(sp_advice_in != Inf)])
sd(sp_advice_in[which(sp_advice_in != Inf)])
mean(sp_advice_out[which(sp_advice_out != Inf)])
sd(sp_advice_out[which(sp_advice_out != Inf)])

mean(sp_friendship_in[which(sp_friendship_in != Inf)])
sd(sp_friendship_in[which(sp_friendship_in != Inf)])
mean(sp_friendship_out[which(sp_friendship_out != Inf)])
sd(sp_friendship_out[which(sp_friendship_out != Inf)])

mean(sp_reports_to_in[which(sp_reports_to_in != Inf)])
sd(sp_reports_to_in[which(sp_reports_to_in != Inf)])
mean(sp_reports_to_out[which(sp_reports_to_out != Inf)])
sd(sp_reports_to_out[which(sp_reports_to_out != Inf)])

# Reachability
mean(reach_full_in[which(reach_full_in != Inf)])
sd(reach_full_in[which(reach_full_in != Inf)])
mean(reach_full_out[which(reach_full_out != Inf)])
sd(reach_full_out[which(reach_full_out != Inf)])

mean(reach_advice_in[which(reach_advice_in != Inf)])
sd(reach_advice_in[which(reach_advice_in != Inf)])
mean(reach_advice_out[which(reach_advice_out != Inf)])
sd(reach_advice_out[which(reach_advice_out != Inf)])

mean(reach_friendship_in[which(reach_friendship_in != Inf)])
sd(reach_friendship_in[which(reach_friendship_in != Inf)])
mean(reach_friendship_out[which(reach_friendship_out != Inf)])
sd(reach_friendship_out[which(reach_friendship_out != Inf)])

mean(reach_reports_to_in[which(reach_reports_to_in != Inf)])
sd(reach_reports_to_in[which(reach_reports_to_in != Inf)])
mean(reach_reports_to_out[which(reach_reports_to_out != Inf)])
sd(reach_reports_to_out[which(reach_reports_to_out != Inf)])

# Density 
n_aristas_dirigidas = (vcount(krack_full)*(vcount(krack_full)-1))
graph.density(krack_full)
length(E(krack_full))/n_aristas_dirigidas

graph.density(krack_advice)
length(E(krack_advice))/n_aristas_dirigidas
graph.density(krack_friendship)
length(E(krack_friendship))/n_aristas_dirigidas
graph.density(krack_reports_to)
length(E(krack_reports_to))/n_aristas_dirigidas

# Reciprocity
reciprocity(krack_full)
reciprocity(krack_advice)
reciprocity(krack_friendship)
reciprocity(krack_reports_to)

# Transitivity
transitivity(krack_full)
transitivity(krack_advice)
transitivity(krack_friendship)
transitivity(krack_reports_to)
transitivity(krack_reports_to,type="undirected")

# Triad census. Here we'll first build a vector of labels for 
# the different triad types. Then we'll combine this vector
# with the triad censuses for the different networks, which 
# we'll export as a CSV.

census_labels = c('003',
                  '012',
                  '102',
                  '021D',
                  '021U',
                  '021C',
                  '111D',
                  '111U',
                  '030T',
                  '030C',
                  '201',
                  '120D',
                  '120U',
                  '120C',
                  '210',
                  '300')
tc_full <- triad.census(krack_full)
tc_advice <- triad.census(krack_advice)
tc_friendship <- triad.census(krack_friendship)
tc_reports_to <- triad.census(krack_reports_to)

triad_df <- data.frame(census_labels,
                       tc_full, 
                       tc_advice, 
                       tc_friendship,
                       tc_reports_to)
triad_df

# Question #2 - (a) How do the three networks differ on network statictics? 
# (b) What does the triad census tell us? Can you calculate the likelihood of
# any triad's occurrence? (c) See the back of Wasserman and Faust and its section
# on triads. Calculate the degree of clustering and hierarchy in Excel. 
# What do we learn from that?




### 
# 4. HETEROGENEITY 
###

# Miller and McPherson write about processes of homophily and
# here we take a brief look at one version of this issue. 
# In particular, we look at the extent to which each actor's
# "associates" (friend, advisor, boos) are heterogenous or not.

# We'll use a statistic called the IQV, or Index of Qualitative
# Variation. This is just an implementation of Blau's Index of
# Heterogeneity (known to economists as the Herfindahl-Hirschman
# index), normalized so that perfect heterogeneity (i.e., equal 
# distribution across categories) equals 1.

# We are interested in many of the attributes of nodes.  To save 
# time and to make our lives better we are going to create a function
# that will provide an IQV statistic for any network and for
# any categorical variable.  A function is a simple way to
# create code that is both reusable and easier to edit.

# Functions have names and receive arguments.  For example,
# anytime you call table() you are calling the table function.
# We could write code to duplicate the table function for each
# of our variables, but it is faster to write a single general tool
# that will provide frequencies for any variable. If I have
# a dataframe with the variable gender and I want to see the
# split of males and females I would pass the argument
# "dataframe$gender" to the table function. We follow a
# similar model here. Understanding each step is less important
# than understanding the usefulness and power of functions.

get_iqvs <- function(graph, attribute) {
  
  if(!require(dplyr)){
    stop("dplyr library required to continue!")
  }
  
  edges<-get.data.frame(graph,"edges")
  
  edges %>%
    mutate(at=get.vertex.attribute(graph,attribute,to)) %>% 
    select(from,to,at) %>% 
    group_by(from,at) %>% 
    summarize(total_links_per_att=n())->edges_with_links_per_att
 
  nlevels=length(unique(edges_with_links_per_att$at)) 
  edges_with_links_per_att %>% 
    group_by(from) %>% 
    summarize(blau_index=1-sum((total_links_per_att/sum(total_links_per_att))**2)) %>% 
    mutate(vertice=from,
           iqvs=blau_index/(1-1/nlevels)) %>% 
    select(vertice,iqvs,blau_index)->heterogeneity_measure
  
		# and the ego's IQV, which is just a normalized blau index
  return(heterogeneity_measure)
}

# For this data set, we'll look at homophily across departments, 

advice_iqvs <- get_iqvs(krack_advice, 'DEPT')
advice_iqvs

friendship_iqvs <- get_iqvs(krack_friendship, 'DEPT')
friendship_iqvs

reports_to_iqvs <- get_iqvs(krack_reports_to, 'DEPT')
reports_to_iqvs

# Question #3 - What does the herfindahl index reveal about 
# attribute sorting in networks? What does it mean for each network?


#####
# Extra-credit: What might be a better way to test the occurrence 
# of homophily or segregation in a network? How might we code that in R?
#####

#####
# Tau statistic (code by Sam Pimentel)
#####


#R code for generating random graphs:
#requires packages ergm, intergraph

#install.packages(c("ergm","intergraph"))
library(ergm)
library(intergraph)
#set up weighting vectors for clustering and hierarchy
clust.mask <- rep(0,16)
clust.mask[c(1,3,16)] <- 1
hier.mask <- rep(1,16)
hier.mask[c(6:8,10:11)]   <- 0

#compute triad count and triad proportion for a given weighting vector
mask.stat <- function(my.graph, my.mask){
      n.nodes <- vcount(my.graph)
      n.edges <- ecount(my.graph)
      #set probability of edge formation in random graph to proportion of possible edges present in original
      p.edge <- n.edges/(n.nodes*(n.nodes +1)/2)
      r.graph <- as.network.numeric(n.nodes, density = p.edge)
      r.igraph <- as.igraph(r.graph)
      tc.graph <- triad.census(r.igraph)
      clust <- sum(tc.graph*my.mask)
      clust.norm <- clust/sum(tc.graph)
      return(c(clust,clust.norm))
}

#build 100 random graphs and compute their clustering and hierarchy measurements to create
# an empirical null distribution
emp.distro <- function(this.graph){
   clust <- matrix(rep(0,200), nrow=2)  
   hier <- matrix(rep(0,200),nrow=2)
   for(i in c(1:100)){
        clust[,i] <- mask.stat(this.graph, clust.mask)
        hier[,i] <- mask.stat(this.graph, hier.mask)
   }
   my.mat <- rbind(clust, hier)
   rownames(my.mat) <- c("clust.ct", "clust.norm", "hier.ct", "hier.ct.norm")
   return(my.mat)
}

#fix randomization if desired so results are replicable
set.seed(3123)
#compute empirical distributions for each network
hc_advice <- emp.distro(krack_advice)
hc_friend <- emp.distro(krack_friendship)
hc_report <- emp.distro(krack_reports_to)

#find empirical p-value
get.p <- function(val, distro)
{
	distro.n <- sort(distro)
	distro.n <- distro.n - median(distro.n)
	val.n <- val - median(distro.n)
	p.val <- sum(abs(distro.n) > abs(val.n))/100
	return(p.val)
}
get.p(198, hc_full[1,])
get.p(194, hc_advice[1,])
get.p(525, hc_friend[1,])
get.p(1003, hc_report[1,])
get.p(979, hc_full[3,])
get.p(1047, hc_advice[3,])
get.p(1135, hc_friend[3,])
get.p(1314, hc_report[3,])

#generate   95% empirical confidence intervals for triad counts

#clustering
c(sort(hc_advice[1,])[5], sort(hc_advice[1,])[95])
c(sort(hc_friend[1,])[5], sort(hc_friend[1,])[95])
c(sort(hc_report[1,])[5], sort(hc_report[1,])[95])

#hierarchy
c(sort(hc_advice[3,])[5], sort(hc_advice[3,])[95])
c(sort(hc_friend[3,])[5], sort(hc_friend[3,])[95])
c(sort(hc_report[3,])[5], sort(hc_report[3,])[95])
