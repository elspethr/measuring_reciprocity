

setwd("data/ENDOW")

library(igraph)
library(reshape2)


## Read in the files that include details of each individual and of each sharing unit
indiv <- read.csv("TN_Indiv.csv", header=TRUE, as.is=TRUE)
su <- read.csv("TN_SharingUnit.csv", header=TRUE, as.is=TRUE)

for (CommunityName in c("Tenpatti","Alakapuram")){

## Read in the node list file
nl <- read.csv(paste("TN_Nodelist_",CommunityName,".csv",sep=""), na="", header=TRUE, as.is=TRUE)
indiv$DidSurvey <- indiv$IndivID %in% unique(nl$Ego)

## Create edgelists for each network question
names <- unique(nl$Question)

for(i in 1:length(names)){
  nameN<-paste("n",unique(nl$Question),sep="")
  assign(nameN[i],subset(nl, Question == names[i], select = -Question))
  nameM<-paste("m",unique(nl$Question),sep="")
  assign(nameM[i],melt(get(nameN[i]), id.vars = "Ego", na.rm = T, value.name = "Alter"))
  nameE<-paste("e",unique(nl$Question),sep="")
  assign(nameE[i],subset(get(nameM[i]), select = -variable))
}

## Reverse tie directionality for some edges (the double-sampled ones)
e2t<-as.data.frame(e2[,c(2,1)])
colnames(e2t)<-c("Ego","Alter")

e4t<-as.data.frame(e4[,c(2,1)])
colnames(e4t)<-c("Ego","Alter")


## Now we can make combined data frames of these in order to generate the multilayer network
elLoanAsk <- data.frame(e1,"LoanAsk" = 1)
elLoanReceive <- data.frame(e2t,"LoanReceive" = 1)
#elLoanUnion <- data.frame(rbind(e1,e2t),"LoanUnion" = 1)
#elLoanUnion <- subset(elLoanUnion,duplicated(elLoanUnion[,1:2])==FALSE)

elItemBorrow <- data.frame(e3,"ItemBorrow" = 1)
elItemReceive <- data.frame(e4t,"ItemReceive" = 1)
#elItemUnion <- data.frame(rbind(e3,e4t),"ItemUnion" = 1)
#elItemUnion <- subset(elItemUnion,duplicated(elItemUnion[,1:2])==FALSE)

elTasksF <- data.frame(e5[,1:2],"TasksF" = 1)
elTasksM <- data.frame(e6[,1:2],"TasksM" = 1)

## limiting the female/male directed question to include only those reported by females/males
e5$Gender <- indiv$Gender[match(e5$Ego,indiv$IndivID)]
e5f<-subset(e5,e5$Gender=="Female")

## and the same for males
e6$Gender <- indiv$Gender[match(e6$Ego,indiv$IndivID)]
e6m<-subset(e6,e6$Gender=="Male")
elTasksUnion <- data.frame(rbind(e5f[,1:2],e6m[,1:2]),"TasksUnion" = 1)
elTasksUnion <- subset(elTasksUnion,duplicated(elTasksUnion[,1:2])==FALSE)

e7$Gender <- indiv$Gender [match(e7$Ego,indiv$IndivID)]
e7f<-subset(e7,e7$Gender=="Female")

e8$Gender <- indiv$Gender [match(e8$Ego,indiv$IndivID)]
e8m<-subset(e8,e8$Gender=="Male")

elTalkF <- data.frame(e7[,1:2],"TalkF" = 1)
elTalkM <- data.frame(e8[,1:2],"TalkM" = 1)
elTalkUnion <- data.frame(rbind(e7f[,1:2],e8m[,1:2]),"TalkUnion" = 1)
elTalkUnion <- subset(elTalkUnion,duplicated(elTalkUnion[,1:2])==FALSE)

elHighPosition <- data.frame(e9, "HighPosition" = 1)
elOutside <- data.frame(e10, "Outside" = 1)
#elExternalUnion <- data.frame(rbind(e9,e10),"ExternalUnion" = 1)
#elExternalUnion <- subset(elExternalUnion,duplicated(elExternalUnion[,1:2])==FALSE)

## THESE ARE EXTRA PROMPTS -- other ENDOW data won't have these -- might have other prompts, though! Would want to generalize this.
elImpIss <- data.frame(e11, "ImpIss" = 1)
elWork <- data.frame(e12, "Work" = 1)
elGenerous <- data.frame(e13, "Generous" = 1)
elInfluence <- data.frame(e14, "Influence" = 1)
elDevout <- data.frame(e15, "Devout" = 1)
elRelations <- data.frame(eRelations, "Relations" = 1)

dfMult <- Reduce(function(df1, df2) merge(df1, df2, by = c("Ego","Alter"), all.x = TRUE,all.y=TRUE),list(elLoanAsk,elLoanReceive,elItemBorrow,elItemReceive,elTasksF,elTasksM,elTasksUnion,elTalkF,elTalkM,elTalkUnion,elHighPosition,elOutside,elImpIss,elWork,elGenerous,elInfluence,elDevout))
dfMult[is.na(dfMult)] <- 0

dfSup <- dfMult[,1:16] ## all support types (including unions) MOVING FORWARD WITH THIS.
dfSup <- subset(dfSup,rowSums(dfSup[,-c(1,2)])>0)
# dfSupNoU <- dfMult[,c(1:4,6,7,9,10,12,13,15,16,18,19)] ## no unions
# dfSupNoU <- subset(dfSupNoU,rowSums(dfSupNoU[,-c(1,2)])>0)
# dfSupVill <- dfMult[,c(1:14,18,19)] ## only support types that are likely to be within the village
# dfSupVill <- subset(dfSupVill,rowSums(dfSupVill[,-c(1,2)])>0)
# dfSupVillNoU <- dfMult[,c(1:4,6,7,9,10,12,13,18,19)] ## only village support & no unions
# dfSupVillNoU <- subset(dfSupVillNoU,rowSums(dfSupVillNoU[,-c(1,2)])>0)
# dfSupVillSing <- dfSupVillNoU[,c(1:3,5,7:12)] ## Removing the double sampled questions (so, ItemReceive and LoanGive)
# dfSupVillSing <- subset(dfSupVillSing,rowSums(dfSupVillSing[,-c(1,2)])>0)

# dfRep <- dfMult[,c(1,2,20:22)] ## only reputation questions -- RIGHT NOW, PUTTING THESE ASIDE
# dfRep <- subset(dfRep,rowSums(dfRep[,-c(1,2)])>0)

## Generating networks

## generate a network from the multilayer dataframe
sn <- graph.data.frame(dfSup)
id <- data.frame("IndivID" = V(sn)$name,stringsAsFactors = FALSE)

## append the individual metadata to each individual included in the network
att <- merge(id,indiv,by="IndivID",sort=FALSE,all.x=TRUE)

#att$SharingUnitID <- ifelse(is.na(att$SharingUnitID), att$IndivID, att$SharingUnitID) ## This doesn't mean anything for my nets, but could for other ENDOW data
#att$Age <- ifelse(is.na(att$Age), 2017-att$BirthYear, att$Age)

## now can add in the sharing unit data
#att <- merge(att,su,by="SharingUnitID",sort=FALSE,all.x=TRUE,all.y=FALSE)
#att <- att[,c(2,1,3:length(att[1,]))] ## The first column has to be the id variable

## generate the full network
snFull <- graph.data.frame(d = dfSup, vertices = att, directed=TRUE)

## reduce the network down to include only residents - NOTE! This doesn't include residents who weren't named (e.g., many people under 18)
snRes <- delete.vertices(snFull,V(snFull)$Location != CommunityName)

## reduce the network down to include only survey respondents
snSurv <- delete.vertices(snRes,V(snRes)$DidSurvey==FALSE)

## removing BOTH multiple edges and loops, and NOT saving any of the weight info that we could potentially be
snRes_simp <- simplify(snRes,remove.multiple = TRUE, remove.loops = TRUE)
snSurv_simp <- simplify(snSurv,remove.multiple = TRUE, remove.loops = TRUE)

## Limiting edge types considered to only those support types expected to be in the village (so no "high position," no "external alters")
#snFull_VillSup <- graph.data.frame(d = dfSupVill, directed=TRUE)
#att1 <- merge(data.frame("IndivID" = V(snFull_VillSup)$name,stringsAsFactors = FALSE),indiv,by="IndivID",sort=FALSE,all.x=TRUE)
#snFull_VillSup <- graph.data.frame(d = dfSupVill, vertices = att1, directed=TRUE)
#snRes_VillSup <- delete.vertices(snFull_VillSup,V(snFull_VillSup)$Location != CommunityName)
#snSurv_VillSup <- delete.vertices(snRes_VillSup,V(snRes_VillSup)$DidSurvey==FALSE)
#snRes_VillSup_simp <- simplify(snRes_VillSup,remove.multiple = TRUE, remove.loops = TRUE)
#snSurv_VillSup_simp <- simplify(snSurv_VillSup,remove.multiple = TRUE, remove.loops = TRUE)


## Further limiting to exclude the double sampled questions (i.e., now creating merged networks that only ask people to report on who they go to, not also who comes to them, so no LoanGive or ItemReceive)
#snFull_VillSupSing <- graph.data.frame(d = dfSupVillSing, directed=TRUE)
#att1 <- merge(data.frame("IndivID" = V(snFull_VillSupSing)$name,stringsAsFactors = FALSE),indiv,by="IndivID",sort=FALSE,all.x=TRUE)
#snFull_VillSupSing <- graph.data.frame(d = dfSupVillSing, vertices = att1, directed=TRUE)
#snRes_VillSupSing <- delete.vertices(snFull_VillSupSing,V(snFull_VillSupSing)$Location != CommunityName)
#snSurv_VillSupSing <- delete.vertices(snRes_VillSupSing,V(snRes_VillSupSing)$DidSurvey==FALSE)
#snRes_VillSupSing_simp <- simplify(snRes_VillSupSing,remove.multiple = TRUE, remove.loops = TRUE)
#snSurv_VillSupSing_simp <- simplify(snSurv_VillSupSing,remove.multiple = TRUE, remove.loops = TRUE)



## For many analyses, you may want to only have nodes be sharing units (and external alters)
## Now, here's code to aggregate individuals into their sharing units

## As a first step, going to let all external alters having a SUID which is actually just their IndivID
indiv$SharingUnitID <- ifelse(indiv$SharingUnitID == "", indiv$IndivID, indiv$SharingUnitID)

## Create a new object that we can recode
dfSupSU<-dfSup

## Now, recode dfSupSU to include only the SharingUnitIDs
dfSupSU$EgoSU<-indiv[match(dfSupSU$Ego, indiv$IndivID),2] ## NOTE: This means that the second column of the indiv DF *must* be the SharingUnitID column
## because of reversed ties, some Egos are here sharing units
dfSupSU$EgoSU <- ifelse(is.na(dfSupSU$EgoSU), dfSupSU$Ego, dfSupSU$EgoSU)

dfSupSU$AlterSU<-indiv[match(dfSupSU$Alter, indiv$IndivID),2] ## NOTE: This means that the second column of the indiv DF *must* be the SharingUnitID column
dfSupSU$AlterSU <- ifelse(is.na(dfSupSU$AlterSU), dfSupSU$Alter, dfSupSU$AlterSU)
dfSupSU<-dfSupSU[,c(ncol(dfSupSU)-1,ncol(dfSupSU),3:ncol(dfSupSU)-2)]


snSU <- graph.data.frame(dfSupSU,directed=TRUE)
idSU <- V(snSU)$name
idSU <- data.frame("SharingUnitID" = idSU, stringsAsFactors = FALSE)

## append the SU metadata to each node in the network
attSU <- merge(idSU,su,by="SharingUnitID",sort=FALSE,all.x=TRUE)
attSU <- merge(attSU,indiv,by.x="SharingUnitID",by.y="IndivID",all.x=TRUE,all.y=FALSE)

## generate the full network
snSUFull <- graph.data.frame(d = dfSupSU, vertices = attSU, directed=TRUE)

## making a version of this network that excludes external alters
snSURes <- delete.vertices(snSUFull,!V(snSUFull)$name %in% grep("SU",V(snSUFull)$name,value=TRUE))
snSURes <- delete.vertices(snSURes,V(snSURes)$AorT != CommunityName)

## removing BOTH multiple edges and loops, and NOT saving any of the weight info that we could potentially be
snSURes_simp <- simplify(snSURes,remove.multiple = TRUE, remove.loops = TRUE)


## with only support expected within the village
#dfSupVillSU <- dfSupSU[,c(1:14,18,19)]
#dfSupVillSU <- subset(dfSupVillSU,rowSums(dfSupVillSU[,-c(1,2)])>0)

#snSUFull_VillSup <- graph.data.frame(d = dfSupVillSU, directed=TRUE)
#attSU1 <- merge(data.frame("SharingUnitID" = V(snSUFull_VillSup)$name,stringsAsFactors = FALSE),su,by="SharingUnitID",sort=FALSE,all.x=TRUE)
#attSU1 <- merge(attSU1,indiv,by.x="SharingUnitID",by.y="IndivID",all.x=TRUE,all.y=FALSE)

#snSUFull_VillSup <- graph.data.frame(d = dfSupVillSU, vertices = attSU1, directed=TRUE)
#snSURes_VillSup <- delete.vertices(snSUFull_VillSup,!V(snSUFull_VillSup)$name %in% grep("SU",V(snSUFull_VillSup)$name,value=TRUE))
#snSURes_VillSup <- delete.vertices(snSURes_VillSup,V(snSURes_VillSup)$AorT != CommunityName)
#snSURes_VillSup_simp <- simplify(snSURes_VillSup,remove.multiple = TRUE, remove.loops = TRUE)


## without the double sampled questions
#dfSupVillSingSU <- dfSupVillSU[,c(1:3,6,9,10,12,13,15,16)]
#dfSupVillSingSU <- subset(dfSupVillSingSU,rowSums(dfSupVillSingSU[,-c(1,2)])>0)

#snSUFull_VillSupSing <- graph.data.frame(d = dfSupVillSingSU, directed=TRUE)
#attSU1 <- merge(data.frame("SharingUnitID" = V(snSUFull_VillSupSing)$name,stringsAsFactors = FALSE),su,by="SharingUnitID",sort=FALSE,all.x=TRUE)
#attSU1 <- merge(attSU1,indiv,by.x="SharingUnitID",by.y="IndivID",all.x=TRUE,all.y=FALSE)

#snSUFull_VillSupSing <- graph.data.frame(d = dfSupVillSingSU, vertices = attSU1, directed=TRUE)
#snSURes_VillSupSing <- delete.vertices(snSUFull_VillSupSing,!V(snSUFull_VillSupSing)$name %in% grep("SU",V(snSUFull_VillSupSing)$name,value=TRUE))
#snSURes_VillSupSing <- delete.vertices(snSURes_VillSupSing,V(snSURes_VillSupSing)$AorT != CommunityName)
#snSURes_VillSupSing_simp <- simplify(snSURes_VillSupSing,remove.multiple = TRUE, remove.loops = TRUE)



## making networks for each particular support type

nets <- list("snFull" = snFull,
             "snRes" = snRes,
             "snRes_simp" = snRes_simp,
             "snSurv" = snSurv,
             "snSurv_simp" = snSurv_simp,
             "snSUFull" = snSUFull,
             "snSURes" = snSURes,
             "snSURes_simp" = snSURes_simp#,
             #"snFull_VillSup" = snFull_VillSup,
             #"snRes_VillSup" = snRes_VillSup,
             #"snRes_VillSup_simp" = snRes_VillSup_simp,
             #"snSurv_VillSup" = snSurv_VillSup,
             #"snSurv_VillSup_simp" = snSurv_VillSup_simp,
             #"snSUFull_VillSup" = snSUFull_VillSup,
             #"snSURes_VillSup" = snSURes_VillSup,
             #"snSURes_VillSup_simp" = snSURes_VillSup_simp,
             #"snFull_VillSupSing" = snFull_VillSupSing,
             #"snRes_VillSupSing" = snRes_VillSupSing,
             #"snRes_VillSupSing_simp" = snRes_VillSupSing_simp,
             #"snSurv_VillSupSing" = snSurv_VillSupSing,
             #"snSurv_VillSupSing_simp" = snSurv_VillSupSing_simp,
             #"snSUFull_VillSupSing" = snSUFull_VillSupSing,
             #"snSURes_VillSupSing" = snSURes_VillSupSing,
             #"snSURes_VillSupSing_simp" = snSURes_VillSupSing_simp
             )

names(nets) = paste0(CommunityName,sep="_",names(nets))


tietypes <- c("LoanAsk","LoanReceive","ItemBorrow","ItemReceive","TasksF","TasksM","TasksUnion","TalkF","TalkM","TalkUnion","HighPosition","Outside","ImpIss","Work")

count = 9

for (i in 1:length(tietypes)){
  type = tietypes[i]
  nets[[count]] <- graph.adjacency(as.matrix(get.adjacency(snFull, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste(CommunityName,"snFull", type, sep = "_")
  count = count + 1
  nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snRes, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste(CommunityName,"snRes", type, sep = "_")
  count = count + 1
  nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snSurv, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste(CommunityName,"snSurv", type, sep = "_")
  count = count + 1
  nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snSUFull, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste(CommunityName,"snSUFull", type, sep = "_")
  count = count + 1
  nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snSURes, attr = type)))
  nets[[count]] <- simplify(nets[[count]],remove.loops = TRUE)
  names(nets)[[count]] <- paste(CommunityName,"snSURes", type, sep = "_")
  count = count + 1
}

assign(paste(CommunityName,"nets",sep="_"),nets)
}

TNnets_atts <- read.csv("TN_net_atts.csv", header=TRUE, stringsAsFactors=FALSE)

setwd("..")
setwd("..")
