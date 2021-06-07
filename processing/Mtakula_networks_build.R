
library(igraph)
library(reshape2)

## questions

  # f.food.in: In the last seven days who did you ask or borrow salt, flour or vegetables from? [ASKED OF FEMALES]
  # f.food.out: In the last seven days who asked you for salt, flour or vegetables? [ASKED OF FEMALES]
  # f.water:  In the last seven days with whom did you go to collect water? [ASKED OF FEMALES]
  # m.tools.in: In the last seven days from whom did you borrow a panga or axe? [ASKED OF MALES]
  # m.tools.out: In the last seven days who borrowed either a panga or axe from you? [ASKED OF MALES]
  # f.childcare.in, .childcare.in: In the last seven days who looked after or watched your children for a short time when you had other work (such as going to the farm or collecting water)? [ASKED OF FEMALES AND MALES]
  # f.childcare.out, .childcare.out: In the last seven days whose children did you look after or watch for a short time when their mother was busy? [ASKED OF FEMALES AND MALES]
  # f.meal.out, m.meal.out: In the last seven days did anyone from a different household eat a meal in your house, if so who? [ASKED OF FEMALES AND MALES]
  # f.meal.in, m.meal.in: In the last seven days did you eat at anyone else’s house, if so who? [ASKED OF FEMALES AND MALES]
  # f.sm.loan.in, m.sm.loan.in: In the last seven days did anyone give or lend you less than 5000 shillings (without wanting interest)? If so who? [ASKED OF FEMALES AND MALES]
  # f.sm.loan.out, m.sm.loan.out: In the last seven days did you give or lend anyone less than 5000 shillings (without wanting interest in return), if so who? [ASKED OF FEMALES AND MALES]
  # f.lg.loan.in, m.lg.loan.in: Within the last three months did anyone give or lend you more than 5000 shillings without wanting you to return the money plus interest? If so who? [ASKED OF FEMALES AND MALES]
  # f.lg.loan.out, m.lg.loan.out: Within the last three months did you give or lend anyone more than 5000 shilling without demanding interests? If so who? [ASKED OF FEMALES AND MALES]
  # f.hospital.out, m.hospital.out: Within the last three months who have you escorted or taken to hospital? [ASKED OF FEMALES AND MALES]
  # f.hospital.in, m.hospital.in: Within the last three months who escorted or took you to hospital? [ASKED OF FEMALES AND MALES]
  # f.field.labor.out, m.field.labor.out: This year (the last agricultural year) who did you help to clear land, plant, weed or any farm work for which you were not paid i.e. you weren’t a farm laborer for? [ASKED OF FEMALES AND MALES]
  # f.field.labor.in, m.field.labor.in: This year (last agricultural year) who helped you to clear your land, plant, weed or any other farm work which out payment ie not people you hired to help? [ASKED OF FEMALES AND MALES]
  # f.clothes.in, m.clothes.in: Within the last three months who gave or bought you clothes? [ASKED OF FEMALES AND MALES]
  # f.clothes.out, m.clothes.out: Within the last three months to whom did you give or buy clothes? [ASKED OF FEMALES AND MALES]
  # f.advice1.in, m.advice1.in: If you have a dispute with your neighbors to whom would you turn for advice? [ASKED OF FEMALES AND MALES]
  # f.advice2.in, m.advice2.in: If you have a problem at home, for example you have a problem with your husband/wife, who will you ask for advice? [ASKED OF FEMALES AND MALES]
  # f.advice.in, m.advice.in: Who comes to you for advice? [ASKED OF FEMALES AND MALES]
  # f.bicycle.in, m.bicycle.in: Whose bicycle have you borrowed in the last seven days? [ASKED OF FEMALES AND MALES]
  # f.bicycle.out, m.bicycle.out: Who in the last seven days has asked to borrow your bike? [ASKED OF FEMALES AND MALES]


eldf <- read.csv("data/Mtakula/Mtakula_el.csv", header=TRUE, as.is=TRUE)

## getting indiv info into data frame so can load in properly
indiv <- data.frame(mapply(c, eldf[,c(1:3)], eldf[,c(4:6)], SIMPLIFY=FALSE))
colnames(indiv) <- c("id","gender","hh")
indiv <- subset(indiv,duplicated(indiv[,1])==FALSE)

## removing gender & hh info for indivs from eldf, as can get confused.
eldf <- eldf[-c(2,3,5,6)]


## data frame of just double sampled questions that need to be reversed for taking union
## flow in the end is meant to be from "head" of the arrow to "tail" of the arrow, so need to select those of respondent reporting people calling upon them, here called "out"
 eldf_torev <- subset(eldf,select=colnames(eldf)[grepl("out",colnames(eldf))==TRUE])
 colnames(eldf_torev) <- paste(colnames(eldf_torev),"rev",sep="_")

 doublelist <- substr(colnames(eldf_torev),1,nchar(colnames(eldf_torev))-8)

# switch ego and alter for the double sampled questions to be reversed
 eldf_torev$ego.id <- eldf$alter.id
 eldf_torev$alter.id <- eldf$respondent.id

## removing empty rows
 eldf_torev <- subset(eldf_torev,rowSums(eldf_torev[,1:(ncol(eldf_torev)-2)])>0)


## the advice questions are peculiar -- two "incoming" advice questions, one outgoing. for unions, creating new variable that takes the union of the two "incoming" advice questions for M and F
eldf$f.advice.in <- eldf$f.advice1.in + eldf$f.advice2.in
eldf$f.advice.in[eldf$f.advice.in==2]<-1

eldf$m.advice.in <- eldf$m.advice1.in + eldf$m.advice2.in
eldf$m.advice.in[eldf$m.advice.in==2]<-1

colnames(eldf)[1] <- "ego.id"

edgelistlist <- list()

## create new edgelists for each of the double sampled questions, including the advice questions
for (i in 1:length(doublelist)){
  el <- rbind.data.frame(
    subset(eldf[,c(1,2)],eldf[colnames(eldf) %in% paste(doublelist,".in",sep="")[i]]>0),
    subset(eldf_torev[,c(21,22)],eldf_torev[colnames(eldf_torev) %in% paste(doublelist,".out_rev",sep="")[i]]>0)
    )
  el <- subset(el,duplicated(el[,1:2])==FALSE)
  el[,3] <- 1
  colnames(el)[3] <- doublelist[i]
  edgelistlist[[i]] <- el
  names(edgelistlist)[i] <- doublelist[i]
}


double_eldf <- Reduce(function(x,y) merge(x, y, by=c("ego.id","alter.id"),all.x=TRUE,all.y=TRUE),
      edgelistlist)

eldf <- merge(eldf, eldf_torev,by=c("ego.id","alter.id"),all.x=TRUE,all.y=TRUE)


## removing the double sampled "out" questions so that only their reverses are in.
eldf <- eldf[!colnames(eldf) %in% paste(doublelist,".out",sep="")]

# eldf <- merge(eldf,double_eldf,by=c("ego.id","alter.id"),all.x=TRUE,all.y=TRUE)

eldf[is.na(eldf)] <- 0


## further combining the gender-specific prompts
# eldf_gen <- colnames(eldf)[c(16:24,27,25,39:47,59:67)] ## all of the male ties that have a female equivalent
eldf_gen <- colnames(eldf)[c(16:24,27,25,39:47)] ## above, minus union'd
eldf_gen <- substr(eldf_gen,3,nchar(eldf_gen))

start <- ncol(eldf)

for (i in 1:length(eldf_gen)){
  fm <- eldf[colnames(eldf) %in% paste("f.",eldf_gen,sep="")[i]] + eldf[colnames(eldf) %in% paste("m.",eldf_gen,sep="")[i]]
  fm[fm==2] <- 1 ## because of the double sampling and reverses, 2s are possible
  eldf[,(start + i)] <- fm
  colnames(eldf)[start+i] <- eldf_gen[i]
}


## making smaller DF without double sampled questions, for aggregate network
# eldf_sing <- eldf[,c(1:27)]
# eldf_sing <- subset(eldf_sing,rowSums(eldf_sing[,-c(1,2)])>0)


## aggregating to the household level, SIMPLIFYING
eldf_hh <- eldf

eldf_hh$ego.hh<-indiv[match(eldf_hh$ego.id, indiv$id),3]
eldf_hh$alter.hh<-indiv[match(eldf_hh$alter.id, indiv$id),3]
eldf_hh <- eldf_hh[,c(ncol(eldf_hh)-1,ncol(eldf_hh),3:(ncol(eldf_hh)-2))]

## alternatively, sum all rows with same ego and alter household pair
eldf_hh_simp <- aggregate(eldf_hh[c(3:ncol(eldf_hh))], by=eldf_hh[c(1,2)], sum)

## making smaller DF without double sampled questions, for aggregate network
# eldf_hh_sing <- eldf_hh[,c(1:27)]
# eldf_hh_sing <- subset(eldf_hh_sing,rowSums(eldf_hh_sing[,-c(1,2)])>0)

# eldf_hh_sing_simp <- eldf_hh_simp[,c(1:27)]
# eldf_hh_sing_simp <- subset(eldf_hh_sing_simp,rowSums(eldf_hh_sing_simp[,-c(1,2)])>0)


## generating the networks

sn <- graph.data.frame(eldf, directed=TRUE)
id <- data.frame("id" = V(sn)$name,stringsAsFactors = FALSE)

## append the individual metadata to each individual included in the network
att <- merge(id,indiv,by="id",sort=FALSE,all.x=TRUE)

sn <- graph.data.frame(d = eldf, vertices = att, directed=TRUE)
## all of the external ties seem to have already been excluded, since everyone here has a household? check back to paper and Nolin code

# sn_sing <- graph.data.frame(eldf_sing, directed=TRUE)
# id_sing <- data.frame("id" = V(sn_sing)$name,stringsAsFactors = FALSE)
# att_sing <- merge(id_sing,indiv,by="id",sort=FALSE,all.x=TRUE)
# sn_sing <- graph.data.frame(d = eldf_sing, vertices = att_sing, directed=TRUE)



sn_hh <- graph.data.frame(eldf_hh)
#sn_hh_simp <- simplify(sn_hh,remove.multiple = TRUE, remove.loops = TRUE)
sn_hh_simp <- graph.data.frame(eldf_hh_simp) ## note that this way, the matrices should be weighted.

# sn_hh_sing <- graph.data.frame(eldf_hh_sing)
# sn_hh_sing_simp <- graph.data.frame(eldf_hh_sing_simp)
## not appending metadata here, since little available.


## making networks for each particular support type

nets <- list("sn" = sn,
             #"sn_sing" = sn_sing,
             "sn_hh" = sn_hh,
             "sn_hh_simp" = sn_hh_simp#,
             #"sn_hh_sing" = sn_hh_sing,
             #"sn_hh_sing_simp" = sn_hh_sing_simp
             )


tietypes <- colnames(eldf)[-c(1,2)]

count = length(nets)+1

for (i in 1:length(tietypes)){
  type = tietypes[i]
  nets[[count]] <- graph.adjacency(as.matrix(get.adjacency(sn, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste("sn", type, sep = "_")
  count = count + 1
  nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(sn_hh, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste("sn_hh", type, sep = "_")
  count = count + 1
}

assign(paste("Mtakula_nets",sep="_"),nets)

MTnets_atts <- read.csv("data/Mtakula/Mtakula_net_atts.csv", header=TRUE, stringsAsFactors=FALSE)
