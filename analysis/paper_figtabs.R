### Code to generate tables and figures for the reciprocity paper ###

library(xtable)
library(ggplot2)
library(ggbeeswarm)
library(kableExtra)
library(gridExtra)

# source data 
source("compiling.R")

IMnets <- networks[net_atts$source == "IM"]
IMnet_atts <- net_atts[net_atts$source=="IM",]
IMnet_atts$edges <- IMnet_atts$Edge_Type
IMnet_atts$edges[IMnet_atts$Edge_Type == "Get household items"] <- "Get HH items"
IMnet_atts$edges[IMnet_atts$Edge_Type == "Give household items"] <- "Give HH items"
IMnet_atts$edges <- factor(IMnet_atts$edges, levels = unique(IMnet_atts$edges))
IMnet_atts$edgessimp <- factor(IMnet_atts$edges, labels = c(1, 1, 2, 2, 3, 3, 4, 4))
IMnet_atts$edges1 <- dplyr::recode(IMnet_atts$edgessimp, "1" = "Money", "2" = "Advice", "3" = "Household Items", "4" = "Visit")

IMnet_concord <- concord[16:nrow(concord),]

originalIM <-IMnet_atts[1:(75*8),]
orig_names <- unique(originalIM$Network_Name)

### Single-layer summary table (in SI) ###
IMsummary <- data.frame(name=character(), edges_mn=numeric(), edges_std=numeric(),
                        mdeg_mn=numeric(), mdeg_std=numeric(),
                        recip_mn=numeric(), recip_std=numeric())
for (i in orig_names) {
  temp <- originalIM[originalIM$Network_Name==i,]
  edges_mn <- round(mean(temp$nedges), 0)
  edges_std <- round(sd(temp$nedges), 0)
  mdeg_mn <- round(mean(temp$meandegree_dir), 2)
  mdeg_std <- round(sd(temp$meandegree_dir), 2)
  recip_mn <- round(mean(temp$reciprocity), 2)
  recip_std <- round(sd(temp$reciprocity), 2)
  IMsummary <- rbind(IMsummary, data.frame(name=i, edges_mn=edges_mn,
    edges_std=edges_std, mdeg_mn=mdeg_mn, mdeg_std=mdeg_std,
    recip_m=recip_mn, recip_std=recip_std))
}

print("SI Table 2")
#karnataka
xtable(IMsummary, digits=c(0,0,0,0,2,2,2,2))
#other datasets
othersummary <- net_atts[1:30,c(1,29,17,16)]
othersummary$meandegree_dir <- round(othersummary$meandegree_dir, 3)
xtable(othersummary)

### Table 1, concordance ###
IMnet_concord <- cbind(IMnet_atts[601:900,],IMnet_concord)
IMnet_concord$Village <- rep(1:75,each=4)

other_net_atts <- net_atts[!(net_atts$source %in% c("IM", "AH")),]
other_concord <- cbind(other_net_atts[which(other_net_atts$Sampling=="Union"),], concord[1:15,])

IMconcsummary <- data.frame(name=character(), conc_mn=numeric(), conc_std=numeric(),
                        net1_mn=numeric(), net1_std=numeric(),
                        net2_mn=numeric(), net2_std=numeric())
types <- unique(IMnet_concord$Edge_Type)
for (i in types) {
  tempconc <- IMnet_concord[IMnet_concord$Edge_Type==i,]
  mean_concord <- mean(tempconc$concord_prop)
  std_concord <- sd(tempconc$concord_prop)
  net1_mean_concord <- mean(tempconc$net1_concord_prop)
  net1_std_concord <- sd(tempconc$net1_concord_prop)
  net2_mean_concord <- mean(tempconc$net2_concord_prop)
  net2_std_concord <- sd(tempconc$net2_concord_prop)
  IMconcsummary <- rbind(IMconcsummary, data.frame(name=i, conc_mn=mean_concord,
    conc_std=std_concord, net1_mn=net1_mean_concord, net1_std=net1_std_concord,
    net2_mn=net2_mean_concord, net2_std=net2_std_concord))
}
print("Table 1")
xtable(IMconcsummary, digits=2)
xtable(other_concord[, c(1, 37, 38, 39)], digits=2)

### Figure 2 concordance ###
avgprop <- aggregate(IMnet_concord$concord_prop, list(IMnet_concord$Village), mean)
IMnet_concord$VillRank <- rep(rank(avgprop$x),each = 4)
byadvice <- rep(rank(subset(IMnet_concord,IMnet_concord$Edge_Type=="Get advice")$concord_prop), each=4)

concord_plot <- ggplot(data = IMnet_concord, aes(x = edges1, y = concord_prop)) +
  geom_violin(scale = "count", draw_quantiles = 0.5) + geom_beeswarm(aes(colour = edgessimp), shape = 16, priority = "random", cex = 0.5, size = 0.75, alpha = 0.8) + theme_bw() +
  labs(title = "", x = "", y = "Proportion of Concordant Ties") + theme(legend.position = "none") + ylim(c(0, 0.3)) + scale_colour_brewer(palette = "Set2")

pdf("writing/concordance.pdf", height=4, width=5)
concord_plot
dev.off()

### Figure 3, centralization ###
incent <- ggplot(data = IMnet_atts[IMnet_atts$Sampling %in% c("Straight"),], aes(x = edges1, y = in_central)) +
  geom_violin(scale = "count", draw_quantiles = 0.5) + geom_beeswarm(aes(colour = edgessimp), shape = 16, priority = "random", cex = 0.5, size = 0.75, alpha = 0.8) + theme_bw() + theme(axis.text=element_text(size=8), axis.title=element_text(size=9), aspect.ratio=1) + scale_y_continuous(limits = c(0,0.21)) +
  labs(title = "Incoming", x = "", y = "In-degree centralization") + theme(legend.position = "none") + scale_colour_brewer(palette = "Set2")

outcent <- ggplot(data = IMnet_atts[IMnet_atts$Sampling %in% c("Flipped"),], aes(x = edges1, y = out_central)) +
  geom_violin(scale = "count", draw_quantiles = 0.5) + geom_beeswarm(aes(colour = edgessimp), shape = 16, priority = "random", cex = 0.5, size = 0.75, alpha = 0.8) + theme_bw() + theme(axis.text=element_text(size=8), axis.title=element_text(size=9), aspect.ratio=1) + scale_y_continuous(limits = c(0,0.21)) +
  labs(title = "Outgoing", x = "", y = "") + theme(legend.position = "none") + scale_colour_brewer(palette = "Set2")

pdf("writing/centralization.pdf", height=4, width=7)
grid.arrange(incent, outcent, ncol=2)
dev.off()


### Figure 4, reciprocity ###
net_atts$Sampling1 <- dplyr::recode(net_atts$Sampling, "Flipped" = "Outgoing", "Straight" = "Incoming")
## Because the WB networks are TH rather than HT in terms of the flow of support, need to alter which are "incoming" and "outgoing" as don't align with "flipped" versus "straight"
net_atts$Sampling1[net_atts$source == "WB" & net_atts$Sampling == "Flipped"] <- "Incoming"
net_atts$Sampling1[net_atts$source == "WB" & net_atts$Sampling == "Straight"] <- "Outgoing"

net_atts$Sampling2 <- factor(net_atts$Sampling1, levels = c("Incoming", "Outgoing", "Intersection", "Union"))

net_atts$other <- ifelse(net_atts$source == "IM", "IM", "Others")
net_atts$newcol <- net_atts$source
net_atts$newcol[net_atts$source == "IM"] <- net_atts$Edge_Type[net_atts$source == "IM"]
net_atts$newcol[net_atts$Edge_Type == "Get household items"] <- "HH Items"
net_atts$newcol[net_atts$Edge_Type == "Give household items"] <- "HH Items"
net_atts$newcol[net_atts$Edge_Type == "Borrow money"] <- "Money"
net_atts$newcol[net_atts$Edge_Type == "Lend money"] <- "Money"
net_atts$newcol[net_atts$Edge_Type == "Get visitors"] <- "Visit"
net_atts$newcol[net_atts$Edge_Type == "Go visit"] <- "Visit"
net_atts$newcol[net_atts$Edge_Type == "Give advice"] <- "Advice"
net_atts$newcol[net_atts$Edge_Type == "Get advice"] <- "Advice"
net_atts$newcol <- factor(net_atts$newcol, levels = unique(net_atts$newcol)[c(4:7, 1:3)])


recip_plot <- ggplot(data = net_atts, aes(x = Sampling2, y = reciprocity)) +
    geom_beeswarm(aes(color = newcol),
    shape = 16, priority = "random",
    cex = 0.5, alpha = 0.7, 
  ) +
  geom_violin(
    scale = "count",
    draw_quantiles = 0.5, alpha=0
  ) +
  theme_bw() +
  labs(title = "", x = "", y = "Reciprocity") +
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylim(c(0, 1)) + 
  scale_colour_brewer(palette = "Set2") 

pdf("writing/reciprocity_fig.pdf", height=4, width=6)
recip_plot + facet_grid(~ net_atts$other)
dev.off()


### Aggregate layer summary table, in SI ###
IMagg <- subset(IMnet_atts, IMnet_atts$Sampling %in% c("Intersection", "Union"))
IMaggsummary <- data.frame(name=character(), edges_mn=numeric(), edges_std=numeric(),
                        mdeg_mn=numeric(), mdeg_std=numeric(),
                        recip_mn=numeric(), recip_std=numeric())
for (i in unique(IMnet_atts$edges1)) {
  for (j in c("Intersection", "Union")) {
    temp <- IMnet_atts[which(IMnet_atts$edges1==i & IMnet_atts$Sampling==j),]
    edges_mn <- round(mean(temp$nedges), 0)
    edges_std <- round(sd(temp$nedges), 0)
    mdeg_mn <- round(mean(temp$meandegree_dir), 2)
    mdeg_std <- round(sd(temp$meandegree_dir), 2)
    recip_mn <- round(mean(temp$reciprocity), 2)
    recip_std <- round(sd(temp$reciprocity), 2)
    IMaggsummary <- rbind(IMaggsummary, data.frame(name=i, edges_mn=edges_mn,
       edges_std=edges_std, mdeg_mn=mdeg_mn, mdeg_std=mdeg_std,
       recip_m=recip_mn, recip_std=recip_std))
    }
}



otheragg <- subset(net_atts, net_atts$Sampling %in% c("Intersection", "Union") & net_atts$source != "IM")
aggorder <- as.numeric(rbind(c(16:30), c(1:15)))
otheragg_summary <- otheragg[aggorder,c(1,29,17,16)]
otheragg_summary$meandegree_dir <- round(otheragg_summary$meandegree_dir, 3)

print("SI Table 3")
xtable(IMaggsummary, digits=c(0,0,0,0,2,2,2,2))
xtable(otheragg_summary, digits = c(0,0,0,2,2))

### Table 2, reciprocity bi-reporting and repeat naming ###

mut_source <- data.frame(pct_recip_bi = concord$pct_recip_bi)#, rep_prob = concord$rep_prob)
mut_source$name <- c(net_atts$Network_Name[seq(1,30,by=2)], as.character(net_atts$newcol[(nrow(net_atts)-299):nrow(net_atts)]))
other_mut <- mut_source[1:15,]

IMmutsummary <- data.frame(name=character(), 
                       #mean_rep=numeric(), std_rep=numeric(),
                       mean_total=numeric(), std_total=numeric())
for (i in c("Money", "Advice", "HH Items", "Visit")) {
  temp <- mut_source[which(mut_source$name==i),]
  mean_total <- mean(temp$pct_recip_bi)
  std_total <- sd(temp$pct_recip_bi)
  IMmutsummary <- rbind(IMmutsummary, data.frame(name=i, 
    mean_total=mean_total,
    std_total=std_total))
}

print("Table 2")
xtable(IMmutsummary, digits = 2)
xtable(other_mut[,c(2,1)], digits = 2)

### Numbers cited in text ###

print(paste("Mean IM concord", round(mean(IMnet_concord$concord_prop)*100, 2)))
print(paste("YWB cf receiving centralization", round(net_atts$out_central[which(net_atts$Network_Name=="YWB_cfreceivingnet_sample")], 2)))
print(paste("YWB cf giving centralization", round(net_atts$in_central[which(net_atts$Network_Name=="YWB_cfgivingnet_sample")], 2)))
print(paste("IM single-layer densities", round(100*mean(IMnet_atts$density[IMnet_atts$Sampling=="Straight" | IMnet_atts$Sampling=="Flipped"]), 2)))
print(paste("IM union densities", round(100*mean(IMnet_atts$density[IMnet_atts$Sampling=="Union"]), 2)))
print(paste("IM intersection densities", round(100*mean(IMnet_atts$density[IMnet_atts$Sampling=="Intersection"]), 2)))
print(paste("Rep. prop. HH items", mean(IMnet_concord$rep_prob[IMnet_concord$Edge_Type=="Get household items"]))) 
print(paste("Rep. prop. advice", mean(IMnet_concord$rep_prob[IMnet_concord$Edge_Type=="Get advice"])))
print(paste("Rep. prop. visiting", mean(IMnet_concord$rep_prob[IMnet_concord$Edge_Type=="Go visit"])))
print(paste("Rep. prop. money", mean(IMnet_concord$rep_prob[IMnet_concord$Edge_Type=="Borrow money"])))