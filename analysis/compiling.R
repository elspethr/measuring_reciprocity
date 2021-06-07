### Code to integrate all the datasets and calculate network measures ###

library(igraph)

source("processing/TN_networks_build.R")
source("processing/Microfinance_networks_build.R")
#source("processing/MJDenny_networks_build.R")
source("processing/YWB_networks_build.R")
source("processing/Mtakula_networks_build.R")
#source("processing/Nain_networks_build.R")
#source("processing/Isakov_networks_build.R")
#source("processing/StanfordDorm_networks_build.R")

TNpairs <- c("snSurv_LoanAsk", "snSurv_LoanReceive",
             "snSurv_ItemBorrow","snSurv_ItemReceive")

IMpairs <- c("snSurv_borrowmoney", "snSurv_receivemoney",
             "snSurv_helpdecision", "snSurv_receiveadvice",
             "snSurv_keroricego","snSurv_keroricecome",
             "snSurv_visitgo","snSurv_visitcome")

MTpairs <- c("sn_childcare.in","sn_childcare.out_rev",
             "sn_meal.in","sn_meal.out_rev",
             "sn_sm.loan.in","sn_sm.loan.out_rev",
             "sn_lg.loan.in","sn_lg.loan.out_rev",
             "sn_hospital.in","sn_hospital.out_rev",
             "sn_field.labor.in","sn_field.labor.out_rev",
             "sn_clothes.in","sn_clothes.out_rev",
             "sn_advice.in","sn_advice.out_rev", #note, sn_advice.in is the union of sn_advice1.in and sn_advice2.in
             "sn_bicycle.in","sn_bicycle.out_rev")

YWBpairs <- c("YWB_cfreceivingnet_sample", "YWB_cfgivingnet_sample",
             "YWB_mealreceivingnet_sample", "YWB_mealgivingnet_sample")

TNnets_atts <- rbind.data.frame(TNnets_atts[,1:13], TNnets_atts[,1:13]) #put in something to flag village later

TNnets_atts$keep <- TNnets_atts$Network_Name %in% TNpairs
TNnets_atts_sub <- TNnets_atts[which(TNnets_atts$Network_Name %in% TNpairs),]
#IMnets_atts[,1:12] repeat for all villages....
IMnets_atts_full <- do.call("rbind", replicate(75, IMnets_atts[,1:13], simplify = FALSE))
IMnets_atts_full$keep <- IMnets_atts_full$Network_Name %in% IMpairs
IMnets_atts_full_sub <- IMnets_atts_full[which(IMnets_atts_full$Network_Name %in% IMpairs),]
#HNnets_atts_full <- do.call("rbind", replicate(11, HNnets_atts[,1:13], simplify = FALSE))
#SDnets_atts_full <- do.call("rbind", replicate(4, SDnets_atts[,1:13], simplify = FALSE))
YWBnets_atts$keep <- YWBnets_atts$Network_Name %in% YWBpairs
YWBnets_atts_sub <- YWBnets_atts[which(YWBnets_atts$Network_Name %in% YWBpairs),]
MTnets_atts <- MTnets_atts[,1:13]
MTnets_atts$keep <- MTnets_atts$Network_Name %in% MTpairs
MTnets_atts_sub <- MTnets_atts[which(MTnets_atts$Network_Name %in% MTpairs),]
MTnets_atts_sub <- MTnets_atts_sub[match(MTpairs, MTnets_atts_sub$Network_Name),]

#rm(list=setdiff(ls(), c("Tenpatti_nets","Alakapuram_nets", "IMnets", "mjnets", "YWBnets", "Mtakula_nets", "Nainnets", "HNnets", "TNnets_atts", "IMnets_atts_full", "mjnets_atts", "YWBnets_atts", "MTnets_atts", "Nainnets_atts", "HNnets_atts","SDnets_atts_full")))


#combine all into networks list and overall metadata table
#note that MJDenny already excludes unwanted networks

net_atts <- rbind.data.frame(TNnets_atts_sub,
                             YWBnets_atts_sub,
                             MTnets_atts_sub,
                             IMnets_atts_full_sub)
net_atts$source <- c(rep("TN", length(TNnets_atts_sub$Network_Name)),
                     rep("WB", length(YWBnets_atts_sub$Network_Name)),
                     #rep("MJ", length(mjnet_atts$Network_Name)),
                     rep("MT", length(MTnets_atts_sub$Network_Name)),
                     #rep("NN", length(Nainnets_atts$Network_Name)),
                     #rep("HN", length(HNnets_atts_full$Network_Name)),
                     rep("IM", length(IMnets_atts_full_sub$Network_Name)))

Mtakula_nets1 <- Mtakula_nets[MTnets_atts$keep==TRUE]
Mtakula_nets1 <- Mtakula_nets1[match(MTpairs,names(Mtakula_nets1))]

networks <- c(Alakapuram_nets[TNnets_atts[1:(nrow(TNnets_atts)/2),]$keep==TRUE],
              Tenpatti_nets[TNnets_atts[1:(nrow(TNnets_atts)/2),]$keep==TRUE],
              YWBnets[YWBnets_atts$keep==TRUE],
              Mtakula_nets1,
              IMnets[IMnets_atts_full$keep==TRUE])


#### different ways of unifying double sampled questions

concord <- data.frame(present=numeric(), null=numeric(), net1=numeric(), net2=numeric(),
                      concord_prop=numeric(), net1_concord_prop=numeric(), net2_concord_prop=numeric(),
                      net1_prop=numeric(), net2_prop=numeric(), net1_mut=numeric(), net2_mut=numeric(), union_mut=numeric(), rep_prob=numeric(), pct_recip_bi=numeric())
network_unions <- network_intersections <- list()
union_net_atts <- intersection_net_atts <- data.frame(matrix(ncol = ncol(net_atts)))
colnames(union_net_atts) <- colnames(intersection_net_atts) <- colnames(net_atts)
counter <- 0
for(i in seq(from = 1, to = length(networks), by=2)){
  counter <- counter+1
  incoming <- networks[[i]]
  outgoing <- networks[[i+1]]
  if (strsplit(names(networks)[[i]], "_")[[1]][1] != strsplit(names(networks)[[i+1]], "_")[[1]][1]) {
    break
    print("Mismatch, indexing is off!")
  }
  #union
  network_unions[[counter]] <- incoming + outgoing
  names(network_unions)[counter] <- paste0(names(networks)[[i]], "_union")

  #intersection
  network_intersections[[counter]] <- incoming %s% outgoing
  names(network_intersections)[counter] <- paste0(names(networks)[[i]], "_intersection")

  #netatts for union/intersection
  union_net_atts[counter,] <- intersection_net_atts[counter,] <- net_atts[i,]
  union_net_atts$Network_Name[counter] <- paste0(names(networks)[[i]], "_union")
  intersection_net_atts$Network_Name[counter] <- paste0(names(networks)[[i]], "_intersection")
  union_net_atts$Sampling[counter] <- "Union"
  intersection_net_atts$Sampling[counter] <- "Intersection"

  #concordance
  summats <- get.adjacency(incoming)+get.adjacency(outgoing)
  present <- sum(summats==2) #ties reported by both
  null <- sum(summats==0)-vcount(incoming) #reported by no one, subtract the diagonal
  net1 <- sum(get.adjacency(incoming)-get.adjacency(network_intersections[[counter]]))
  net2 <- sum(get.adjacency(outgoing)-get.adjacency(network_intersections[[counter]]))
  concord_prop <- present/(present+net1+net2)
  net1_concord_prop <- present/(present+net1) ## proportion of net1 edges that have agreement
  net2_concord_prop <- present/(present+net2) ## proportion of net2 edges that have agreement
  net1_prop <- net1/(present+net1+net2) ## proportion of all edges that are only reported in net1
  net2_prop <- net2/(present+net1+net2) ## proportion of all edges that are only reported in net2
  net1_mut <- dyad.census(incoming)$mut
  net2_mut <- dyad.census(outgoing)$mut
  union_mut <- dyad.census(network_unions[[counter]])$mut
 
  # calculating the probability of a respondent repeating the same name across the two double-sampled questions
  combos <- paste0(as.matrix(get.adjacency(incoming)), "_", t(as.matrix(get.adjacency(outgoing))))
  combotab <- table(combos)
  rep_prob <- combotab[4] / (combotab[3] + combotab[4])

  # trying to get cody's number. note: using edgelists, which will ignore isolates (which def exist!)
  q1 <- cbind(get.edgelist(incoming), get.edgelist(incoming)[,1]) ## adding final column with the reporter
  q2 <- cbind(get.edgelist(outgoing)[,c(1,2)], get.edgelist(outgoing)[,2]) ## ditto for outgoing
  d3 <- rbind.data.frame(q1, q2)
  colnames(d3) <- c("giver", "receiver", "reporter")
  d3$transfer <- paste0(d3$giver, "-", d3$receiver)
  d3$transfer_recip <- NA
  for(i in 1:length(d3$transfer)) {
    d3$transfer_recip[i] <- ifelse(d3$transfer[i] %in% paste0(d3$receiver, "-", d3$giver), "yes", "no" )
  }
  d4 <- d3[which(d3$transfer_recip=="yes"),]
  if (dim(d4)[1]==0) pct_recip_bi <- NA
  else {
    d4$ordered_pair <- NA
    d4$reporter_ordered <- NA
    for(i in 1:length(d4$transfer)){
      d4$ordered_pair[i] <- paste0(min(c(d4$receiver[i], d4$giver[i])), "-", max(d4$receiver[i], d4$giver[i]))
      d4$reporter_ordered[i] <- ifelse((strsplit(d4$ordered_pair[i], "-")[[1]][1])==d4$reporter[i], "Person 1", "Person 2")
    }
      tab1 <- table(d4$ordered_pair, d4$reporter_ordered)
      if (dim(tab1)[1]==1 & dim(tab1)[2]==1) {
        tab1 <- cbind(tab1, 0)
        }
      #table(paste0(tab1[,1], "-", tab1[,2]))
      res <- ifelse(tab1[,1]*tab1[,2] >0 , 1,0)
      pct_recip_bi <- sum(res)/length(res) #pct of reciprocal ties nominated by both persons
    }
  ## NOTE: here counting each concordant relationship once (proportion of all ties)
  #In CA paper we report proportion of all reported ties (concordant edges counted twice)
  row <- c(present, null, net1, net2, concord_prop, net1_concord_prop, net2_concord_prop, net1_prop, net2_prop, net1_mut, net2_mut, union_mut, rep_prob, pct_recip_bi)
  concord[counter,] <- row
}

networks <- c(networks, network_unions, network_intersections)
net_atts <- rbind.data.frame(net_atts, union_net_atts, intersection_net_atts)


## process
net_atts$reciprocity <- NA
net_atts$meandegree_dir <- NA
net_atts$meandistance_dir <- NA
net_atts$avpl_weighted <- NA
net_atts$avpl_weighted_dir <- NA

node_atts<-list()
for(i in 1:length(networks)){
    # Retrieve the network
    net = networks[[i]]

    # Create data frame
    node_atts[[i]]<- data.frame("ID" = V(net)$name)

    # 1. Calculations for all networks

    # 1.1 node level
    node_atts[[i]]$degree = degree(net, mode="all")
    node_atts[[i]]$betweenness = betweenness(net, directed=FALSE, weights=NA)
    node_atts[[i]]$eigen = evcent(net, directed=FALSE, weights=NA)$vector
    node_atts[[i]]$pr = page_rank(net, directed=FALSE, weights=NA)$vector
    node_atts[[i]]$loctrans = transitivity(net, type="local")
    #node_atts[[i]]$closeness = closeness(net, mode="all", weights=NA)

    # 1.2 ego-network level
    #EgoNets = make_ego_graph(net, order=1, mode="out")
    #for(j in 1:length(EgoNets)){
    #  node_atts$egoden[j] = graph.density(delete_vertices(EgoNets[[j]],V(EgoNets[[j]])$name == V(net)$name[j]))
    #  node_atts$egotrans[j] = transitivity(delete_vertices(EgoNets[[j]],V(EgoNets[[j]])$name == V(net)$name[j]), type="global")
    #  node_atts$ego_size[j] = vcount(delete_vertices(EgoNets[[j]],V(EgoNets[[j]])$name == V(net)$name[j]))
    #}

    # 1.3 network-level
    net_atts$directed[i] <- is.directed(net)
    net_atts$weighted[i] <- is.weighted(net)
    net_atts$density[i] <- graph.density(net)
    net_atts$meandegree[i] <- mean(degree(net,  mode="all"))
    net_atts$diameter[i] <- diameter(net, weights=NA)
    net_atts$meandistance[i] <- mean_distance(net, directed=FALSE) ## NOTE: by default mean_distance considers the direction in directed networks, making the direct comparison of directed and undirected networks tricky; also, mean_distance() doesn't allow for weighted edges. If want to include edge weights: mean(distances(networks[[7]],mode="out",algorithm = "unweighted")[is.finite(distances(networks[[7]],mode="out")) & distances(networks[[7]])!=0]) ALSO WEIGHTS HERE ARE TREATED AS DISTANCES.
    net_atts$nettrans[i] <- transitivity(net, type="global", isolates="zero") #ignores direction for directed graphs
    net_atts$kcores[i] <- max(graph.coreness(net)) #ignoring direction
    net_atts$nedges[i] <- ecount(net)
    net_atts$nnodes[i] <- vcount(net)
    net_atts$in_central[i] <- centr_degree(net, mode = "in")$centralization
    net_atts$out_central[i] <- centr_degree(net, mode = "out")$centralization


    # 2. Calculations for directed networks
    if (is.directed(net)) {
      node_atts[[i]]$outdegree = degree(net, mode="out")
      node_atts[[i]]$indegree = degree(net, mode="in")
      node_atts[[i]]$betweenness_dir = betweenness(net, directed=TRUE, weights=NA)
      node_atts[[i]]$pr_dir = page_rank(net, directed=TRUE, weights=NA)$vector
      net_atts$reciprocity[i] <- reciprocity(net)
      net_atts$meandegree_dir[i] <- mean(degree(net, mode="in"))
      net_atts$meandistance_dir[i] <- mean_distance(net, directed=TRUE)
    }

    # 3. Calculations for weighted networks
    if (is.weighted(net)) {
      node_atts[[i]]$strength = graph.strength(net, mode="all", weights=E(net)$weight)
      node_atts[[i]]$wpr = page_rank(net, directed=TRUE, weights=E(net)$weight)$vector
      dist <- distances(net, mode="all", weights=1/E(net)$weight, algorithm="dijkstra")
      net_atts$avpl_weighted[i] = mean(dist[is.finite(dist) & dist!=0])
    }

    # 4. Calculations for directed, weighted networks
    if (is.weighted(net) & is.directed(net)) {
      node_atts[[i]]$outstrength = graph.strength(net,mode="out", weights=E(net)$weight)
      node_atts[[i]]$instrength = graph.strength(net,mode="in", weights=E(net)$weight)
      dist <- distances(net, mode="out", weights=1/E(net)$weight, algorithm="dijkstra")
      net_atts$avpl_weighted_dir[i] = mean(dist[is.finite(dist) & dist!=0])
    }
}

#append reciprocity from addHealth
#source("processing/AH_build.R")
#net_atts <- rbind.data.frame(AH_net_atts,net_atts)


#net_atts <- net_atts[which(net_atts$Include=="Y"),]

#GWESP/GWDSP
#dyadic measures (affiliation?)
#reciprocity across tie types (how does reciprocity increase as you concatenate network layers.)
#reciprocity for weighted networks
#for all dyads, X possible relationship types, what fraction were named for each dyad
#additional loop for statnet stuff
#library(statnet)
#library(intergraph)
#for (i in 1:length(networks)) {
#  stnet <- asNetwork(networks[[i]])
#  without_gwstat <- summary(newnet ~ gwesp(alpha,fixed=TRUE) + gwdsp(alpha,fixed=TRUE))
#  without_spcount <- summary(newnet ~ esp(0:(n-2)) + dsp(0:(n-2)))
#}