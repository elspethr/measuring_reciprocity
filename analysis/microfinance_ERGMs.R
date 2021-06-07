### Multilevel ERGMs on Karnataka data ###

source("compiling.R")

library(statnet)
library(reshape2)
library(vioplot)
library(intergraph)
library(mlergm)

IMnets <- networks[net_atts$source == "IM"] 
IMnet_atts <- subset(net_atts, net_atts$source == "IM")
netsizes <- unlist(lapply(IMnets, vcount))

IMnet_concord <- concord[16:nrow(concord), ]
IMnet_concord <- cbind(IMnet_atts[601:900,],IMnet_concord)

#ERGMS: generate networks of different size, artificially maintaining
#the average degree to see how reciprocity etc vary with network size.

library(scales)

addalpha <- function(colors, alpha = 1.0) {
  r <- col2rgb(colors, alpha = TRUE)
  # Apply alpha
  r[4, ] <- alpha * 255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

library(RColorBrewer)
#pal <- brewer.pal(7,"Dark2")[c(1,1,1,2,2,2,3,3,3,4,4,4,5,6,7)]
#pal <- brewer.pal(6,"Dark2")[c(rep(1:3,4),4,5,6)]
pal <- brewer.pal(3, "Dark2")
typecol <- addalpha(pal, alpha = 0.2)


type = c("borrowmoney","lendmoney","helpdecision","giveadvice","keroricego",
         "keroricecome","visitgo","visitcome")

### run MLERGM for mega networks ###

# net_est_mut <- net_est_gw <- net_est_both <- list()
# for (j in 1:length(typeindex)){
#   net <- make_empty_graph()
#   blocks <- vector()
#   count = 0
#   for (i in seq(from=typeindex[j],to=length(IMnets),by=76)){
#     net <- net + IMnets[[i]]
#     count <- count + 1
#     blocks <- c(blocks,rep(count,vcount(IMnets[[i]])))
#   }
#   net <- asNetwork(net)
#   net <- mlnet(net,node_memb = blocks)
#   net_est_mut[[j]] <- mlergm(net ~ edges + mutual + odegree(0:2) +idegree(0:2),
#                              verbose=1,
#                              parameterization = "offset",
#                              options = set_options(burnin=100000, interval=20000, sample_size=50000,
#                                                    number_cores = 20, MCMLE_max_iter=20)
#   )
#   saveRDS(net_est_mut[[j]], paste0("mlergm_", typeindex[j], "_mut_models.rds",sep=""))
#   #net_est_gw[[j]] <- mlergm(net ~ edges + odegree(0:2) +idegree(0:2) + gwesp,
#   #                       verbose=1,
#   #                       parameterization = "offset",
#   #                       options = set_options(burnin=50000, interval=10000, sample_size=10000,
#   #                                             number_cores = 20, MCMLE_max_iter=20))
#   #saveRDS(net_est_gw[[j]], paste0("mlergm_", typeindex[j], "_gw_models.rds",sep=""))
#   net_est_both[[j]] <- mlergm(net ~ edges + mutual + odegree(0:2) +idegree(0:2) + gwesp,
#                               verbose=1,
#                               parameterization = "offset",
#                               options = set_options(burnin=100000, interval=20000, sample_size=50000,
#                                                     number_cores = 20, MCMLE_max_iter=20)
#   )
#   saveRDS(net_est_both[[j]],paste0("mlergm_", typeindex[j],"_both_models.rds",sep=""))
# }

#saveRDS(net_est_mut,"net_est_mut.rds")
#saveRDS(net_est_gw,"net_est_gwesp.rds")
#saveRDS(net_est,"net_est_both.rds")

j <- 1
net_est_mut <- net_est_both <- list()
for (i in type) {
  #net_est_mut[[j]] <- readRDS(paste0("data/MLERGMs/mlergm_", i, "_mut_models.rds"))
  net_est_both[[j]] <- readRDS(paste0("data/MLERGMs/mlergm_", i, "_both_models.rds"))
  j <- j+1
}


#do some manual GOF for the mlergms
for (x in c("mut", "both")) {
  net_est <- get(paste0("net_est_", x))
  for (k in c(1:13,15,14)) { ## changing order here because templecompany both didn't converge; putting it at end to make sure error doesn't break loop
    start <- typeindex[k]
    netindexes <- seq(from=start, by=76, length.out=75)
    #rec <- numeric()
    #recsims <- list()
    netind <- k
    for (j in netindexes) {
      net <- asNetwork(IMnets[[j]])
      size <- network.size(net)
      ideg <- data.frame(table(degree(net, cmode="indegree")))
      odeg <- data.frame(table(degree(net, cmode="outdegree")))
      #rec <- append(rec, reciprocity(IMnets[[j]]))
      #recipobj <- numeric()
      #ask
      temp1 <- net_est[[netind]]$theta
      temp1[1] <- temp1[1] - log(size)
      temp1[2] <- temp1[2] + log(size)
      if (x == "mut") {
        ergm_sims1 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2), nsim=100, coef=temp1)
      }
      if (x == "both") {
        ergm_sims1 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2) + gwesp, nsim=100, coef=temp1)
      }
      imutstore <- omutstore <- data.frame(Var1=0:250)
      imutstore <- merge(imutstore, ideg, all.x=TRUE, all.y=TRUE)
      omutstore <- merge(omutstore, odeg, all.x=TRUE, all.y=TRUE)
      for (netsim in 1:length(ergm_sims1)) {
          tempi <- data.frame(table(degree(ergm_sims1[netsim], cmode="indegree")))
          tempo <- data.frame(table(degree(ergm_sims1[netsim], cmode="outdegree")))
          colnames(tempi)[2] <- paste0("sim_i", netsim)
          colnames(tempo)[2] <- paste0("sim_o", netsim)
          imutstore <- merge(imutstore, tempi, by="Var1", all.x=TRUE, all.y=TRUE, sort=FALSE)
          omutstore <- merge(omutstore, tempo, by="Var1", all.x=TRUE, all.y=TRUE, sort=FALSE)
          #recipobj <- append(recipobj, reciprocity(asIgraph(ergm_sims1[[netsim]])))
      }
      pdf(paste0("GOF/sim_", x , "_", type[k], "_", j, ".pdf"))
      par(mfrow=c(1,2))
      imutstore[is.na(imutstore)] <- 0
      boxplot(t(imutstore)[3:ncol(imutstore),], xlim=c(0,25), ylim=c(0,300),main=paste(x, "indeg",type[k],"net",j))
      lines(1:251, t(imutstore)[2,], lwd=2)
      omutstore[is.na(omutstore)] <- 0
      boxplot(t(omutstore)[3:ncol(omutstore),], xlim=c(0,25), ylim=c(0,300),main=paste(x, "outdeg",type[k],"net",j))
      lines(1:251, t(omutstore)[2,], lwd=2)
      dev.off()
      #recsims <- append(recsims, list(recipobj))
    }
  }
}

pdf("recip_GOF_example.pdf", height=4, width=8)
boxplot(recsims, ylim=c(0,1), ylab="Reciprocity", main="Reciprocity GOF (Union money)", xaxt="n", xlab="Village")
points(1:75, rec, pch=16, col="red")
dev.off()

par(mfrow=c(2,15))
coefmutdf <- coefbothdf <- data.frame()
for (i in 1:length(net_est_mut)) {
    mutmodel <- net_est_mut[[i]]$theta
    bothmodel <- net_est_both[[i]]$theta
    coefmutdf <- rbind.data.frame(coefmutdf, mutmodel)
    coefbothdf <- rbind.data.frame(coefbothdf, bothmodel)
    # coefdf <- data.frame(names=names(mutmodel$theta), theta=mutmodel$theta,
    #                      se=mutmodel$se, term=rep(i, length(mutmodel$theta)))
    # coefdf$lower95 <- coefdf$theta - (1.96 * coefdf$se)
    # coefdf$upper95 <- coefdf$theta + (1.96 * coefdf$se)
    # coefdf$or <- exp(coefdf$theta)
    # coefdf$orlower <- exp(coefdf$theta - (1.96 * coefdf$se))
    # coefdf$orupper <- exp(coefdf$theta + (1.96 * coefdf$se))
    # ggplot(coefdf,aes(y=theta,x=names)) +
    #   geom_hline(yintercept=0,color="black", lty=2, lwd=0.25) +
    #   geom_pointrange(aes(ymin = lower95, ymax = upper95), shape=19, size=0.5) +
    #   theme_bw() + coord_flip() +
    #   theme(axis.ticks.y=element_blank(), axis.text=element_text(size=11)) +
    #   ggtitle(type[i])
    # plot(NULL, )
}
rownames(coefbothdf) <- rownames(coefmutdf) <- type
colnames(coefmutdf) <- names(net_est_mut[[1]]$theta)
colnames(coefbothdf) <-  names(net_est_both[[1]]$theta)

#function to calculate intersection of statnet network objects
is_sna <- function(g1, g2) {
  smat <- as.sociomatrix(g1) + as.sociomatrix(g2)
  smat <- smat>=2
  return(network(smat))
}

subset_network <- function(g1, prop) {
  nedges <- network.edgecount(g1)
  todelete <- sample(1:nedges, nedges*(1-prop), replace=FALSE)
  fakeintersect <- delete.edges(g1, todelete)
  return(fakeintersect)
}

### Double sampling simulations ###
for (x in c("both")) { #"mut", 
  net_est <- get(paste0("net_est_", x))
  maintypes <- c(1, 3, 5, 7) #only for double sampled questions
  for (k in 1:4) {
    simdatglob <- data.frame(name=character(), recipout=numeric(), recipin=numeric(), recipunion=numeric(), recipinter=numeric(),
                             transout=numeric(), transin=numeric(), transunion=numeric(), transinter=numeric(), stringsAsFactors = FALSE)
    #start <- typeindex[maintypes][k]
    netind <- maintypes[k] #for each net type
    netindexes <- seq(from = netind, by = 8, length.out = 75)
    cprop <- IMnet_concord$concord_prop[seq(from = k, by = 4, length.out = 75)]
    l <- 0
    for (j in netindexes) { #for each village...
      l <- l + 1
      size <- vcount(IMnets[[j]])
      #dens <- concord[netindexes]
      #ask
      temp1 <- net_est[[netind]]$theta
      temp1[1] <- temp1[1] - log(size)
      temp1[2] <- temp1[2] + log(size)
      #if (x == "mut") {
      #  ergm_sims1 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2), nsim=100, coef=temp1)
      #}
      if (x == "both") {
        ergm_sims1 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2) + gwesp, nsim=100, coef=temp1)
      }
      #give
      temp2 <- net_est[[netind+1]]$theta
      temp2[1] <- temp2[1] - log(size)
      temp2[2] <- temp2[2] + log(size)
      #if (x == "mut") {
      #  ergm_sims2 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2), nsim=100, coef=temp2)
      #}
      if (x == "both") {
        ergm_sims2 <- simulate(network(size) ~ edges + mutual + odegree(0:2) + idegree(0:2) + gwesp, nsim=100, coef=temp2)
      }
      #combine
      united <- mapply("+", ergm_sims1, ergm_sims2, SIMPLIFY = FALSE)
      #intersected <- mapply(is_sna, ergm_sims1, ergm_sims2, SIMPLIFY = FALSE) #calculate intersection of simulated graphs
      intersected <- lapply(united, subset_network, prop = cprop[l]) # take graphs that are the same density as intersection graphsersection graphs
      #calculate stats in each direction and for union
      recipout <- unlist(lapply(ergm_sims1, grecip, measure = "edgewise"))
      recipin <- unlist(lapply(ergm_sims2, grecip, measure = "edgewise"))
      recipunion <- unlist(lapply(united, grecip, measure = "edgewise"))
      recipinter <- unlist(lapply(intersected, grecip, measure = "edgewise"))
      transout <- unlist(lapply(ergm_sims1, gtrans))
      transin <- unlist(lapply(ergm_sims2, gtrans))
      transunion <- unlist(lapply(united, gtrans))
      transinter <- unlist(lapply(intersected, gtrans))
      simdat <- cbind.data.frame(name=names(IMnets[j]), recipout, recipin, recipunion, recipinter, 
                                 transout, transin, transunion, transinter, stringsAsFactors = FALSE)
      simdatglob <- rbind.data.frame(simdatglob, simdat)
    }
    kind <- rev(unlist(strsplit(names(IMnets[netind]), "_")))[1]
    assign(paste0("simdatglob_", kind, "_", x), simdatglob)
    write.csv(simdatglob, paste0("data/MLERGMs/simdatglob_mlergm_double_", kind, "_", x, ".csv"), row.names=FALSE)
  }
}


### Plots for effect of double sampling on recip and trans ###
#note not working on latest version of vioplot
overalltype = c("Money", "Advice", "Household Items", "Visit")
maintypes <- c(1, 3, 5, 7)
pdf(paste0("IM_sim_mlergm_tietype_double_both.pdf"), width=12, height=4, pointsize=13)
par(mfrow = c(1, 4), mar = c(2, 4, 2, 1))
layout(matrix(c(1:4), nrow = 1))
for (tie in c(1:4)){
  tempglob <- read.csv(paste0("data/MLERGMs/simdatglob_mlergm_double_", type[maintypes][tie], "_", x, ".csv"), header=TRUE)[,2:5]
  df_recip <- data.frame(Incoming = IMnet_atts$reciprocity[seq(from=tie, by=8, length.out=75)], 
                         Outgoing = IMnet_atts$reciprocity[seq(from=tie+1, by=8, length.out=75)], 
                         Intersection=IMnet_atts$reciprocity[seq(from=900+tie, by=4, length.out=75)],
                         Union = IMnet_atts$reciprocity[seq(from=600+tie, by=4, length.out=75)])
  vioplot(tempglob[,c(1,2,4,3)], main=overalltype[tie], ylim=c(0,1), cex.axis=0.85,
          names=c("Incoming", "Outgoing", "Intersect", "Union"), 
          pchMed=3, colMed=brewer.pal(10,"Paired")[6], col=addalpha(brewer.pal(10,"Paired")[6],0.2), border=brewer.pal(10,"Paired")[6], rectCol = brewer.pal(10,"Paired")[6], lineCol=brewer.pal(10,"Paired")[6])
  stripchart(tempglob[seq(from=1, to=7500, by = 75),c(1,2,4,3)], method = "jitter", jitter = 0.3, pch = 19, add = TRUE, vertical = TRUE, col = addalpha(brewer.pal(10, "Paired")[6], 0.3))
  vioplot(df_recip, ylim=c(0,1), 
          pchMed=3, colMed=brewer.pal(10,"Paired")[2], col=addalpha(brewer.pal(10,"Paired")[2],0.2), border=brewer.pal(10,"Paired")[2], rectCol = brewer.pal(10,"Paired")[2], lineCol=brewer.pal(10,"Paired")[2],add=TRUE)
  stripchart(df_recip, method = "jitter", jitter = 0.3, pch = 19, add = TRUE, vertical = TRUE, col = addalpha(brewer.pal(10,"Paired")[2],0.3))
  if(tie==1){mtext("Reciprocity", side=2, line=2, cex=0.75)}
  if (tie == 1) {
    legend("topleft", legend = c("Real", "Simulated"), pch = 19, col = c(brewer.pal(10, "Paired")[c(2, 6)]))
  }
}
dev.off()
