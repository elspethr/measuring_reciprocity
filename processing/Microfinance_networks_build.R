setwd("data/IndiaMicrofinance/Data/Raw_csv/")

library(igraph)
library(reshape2)
#

# From the "Diffusion of Microfinance" supplementary material:
# The household census gathered demographic information, GPS coordinates and data on a variety of amenities (e.g., roofing material, type of latrine, quality of access to electric power) for every household in each village.
# The individual questionnaire was administered to a subsample of households in the village. A household in the village was considered eligible for administering the individual survey if the household had a woman aged 18-50 years living there. All the eligible households were sorted by  the  religion  of  the  household  head.   For  non-Hindu  (Muslim  and  Christian)  households, all households were selected wherever the group only formed a small fraction of the village. The individual survey was then administered to the head of the household, his or her spouse, other adult women between the age 18 and 50 years, and spouses of these women if available. Hindu households, on the other hand, were grouped based on geography obtained from our GPS data and then from each of these groups, 50% of were randomly selected for administering the individual survey.  The survey was again given to the same types of individuals – household head, spouse of household head, other women aged between 18 and 50 years and their spouses if available.  Individual surveys administered in the above fashion yielded a sample of about 46% of all households per village, and we correct some of our measures for missing data.

# Villages  that  BSS  entered  have  an  average  of  223 households and 166 in non-BSS villages.

## We recover this 46% below:

hhinfo <- foreign::read.dta("../../datav4.0/Data/2. Demographics and Outcomes/household_characteristics.dta")
#coverage <- as.data.frame(cbind(IMnet_atts$nnodes[IMnet_atts$Network_Name=="snHH_simp"],table(hhinfo$village)))
#coverage$prop <- coverage$V1/coverage$V2

## using the Harvard Dataverse files for individual-level info, as this is meant to be the most up-to-date
indivinfo <- foreign::read.dta("../../datav4.0/Data/2. Demographics and Outcomes/individual_characteristics.dta")
indivinfo <- indivinfo[!duplicated(indivinfo$pid)==TRUE,] ## one individual (6109803) is repeated twice.



for (i in c(1:12, 14:21, 23:77)){
  vilcount <- i
  ## Read in the files that include details of each individual and of each sharing unit
  resp <- subset(indivinfo, indivinfo$village==vilcount)
  resp$didsurv <- 1

  ## age (888-refuse to say; 999-do not know);
  ## work outside or not (1-yes, 2-no);

  indiv <- read.csv(paste("village", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE)
  colnames(indiv) <- c("hhid", "ppid", "gender", "age")
  ## gender (1-Male, 2-Female)
  indiv$gender <- dplyr::recode(indiv$gender, "Male", "Female")

  indiv$pid <- ifelse(nchar(indiv$ppid)==2, paste(indiv$hhid, indiv$ppid, sep = ""),
                      paste(indiv$hhid, 0, indiv$ppid, sep = ""))

  hh <- subset(hhinfo, hhinfo$village==vilcount)
  #print(table(hh$hhSurveyed)[2]/nrow(hh))

  indiv <- merge(indiv, hh[, 4:19], by = "hhid", all.x = TRUE, all.y = TRUE)

  indiv <- merge(indiv, resp[,-c(1,2,4,5,6,8)], by = "pid", all.x = TRUE, all.y = TRUE) ## excluding duplicated attributes
  #print(table(indiv$didsurv)[1]/nrow(indiv))

  ## Read in the files that include details on each relationship
  borrowmoney <- read.csv(paste("borrowmoney", vilcount, ".csv", sep=""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  receivemoney <- read.csv(paste("lendmoney", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  receiveadvice <- read.csv(paste("giveadvice", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  helpdecision <- read.csv(paste("helpdecision", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  keroricecome <- read.csv(paste("keroricecome", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  keroricego <- read.csv(paste("keroricego", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  visitcome <- read.csv(paste("visitcome", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  visitgo <- read.csv(paste("visitgo", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  medic <- read.csv(paste("medic", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  templecompany <- read.csv(paste("templecompany", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  nonrel <- read.csv(paste("nonrel", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  rel <- read.csv(paste("rel", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))
  locleader <- read.csv(paste("locleader", vilcount, ".csv", sep = ""), header = FALSE, as.is = TRUE, na = c("9999999", "5555555", "7777777", "0"))

  tietypes <- list("borrowmoney" = borrowmoney,
                   "receivemoney" = receivemoney,
                   "helpdecision" = helpdecision,
                   "receiveadvice" = receiveadvice,
                   "keroricego" = keroricego,
                   "keroricecome" = keroricecome,
                   "visitgo" = visitgo,
                   "visitcome" = visitcome,
                   "medic" = medic,
                   "templecompany" = templecompany,
                   "nonrel" = nonrel) ## excluding local leader and rel

  edges <- data.frame()

  for(j in 1:length(tietypes)){
    df <- melt(tietypes[j], id.vars = 1, na.rm = TRUE, measure.vars = c(2:9), factorsAsStrings = FALSE)
    df <- df[, -2]
    df[, 3] <- rep(paste(names(tietypes)[j]), length(df[, 1]))
    if (names(tietypes)[j] %in% c("receivemoney", "receiveadvice", "keroricecome", "visitcome")) {
      temp <- df[, 2]
      df[, 2] <- df[, 1]
      df[, 1] <- temp
    }
    colnames(df) <- c("Ego", "Alter", "TieType")
    edges <- rbind(edges, df)
  }

  edges_df <- dcast(edges, Ego + Alter ~ TieType, fun.aggregate = length, value.var = "TieType")
  #probably want to get union of double-sampled networks??
  #edges_df$money <- ifelse((edges_df$borrowmoney + edges_df$lendmoney) >= 1, 1, 0)
  #edges_df$kerorice <- ifelse((edges_df$keroricecome + edges_df$keroricego) >= 1, 1, 0)
  #edges_df$visit <- ifelse((edges_df$visitcome + edges_df$visitgo) >= 1, 1, 0)
  #edges_df$advice <- ifelse((edges_df$giveadvice + edges_df$helpdecision) >= 1, 1, 0)

  ## generate a network from the multilayer dataframe
  sn <- graph.data.frame(edges_df)
  id <- data.frame("pid" = V(sn)$name, stringsAsFactors = FALSE)

  ## append the individual metadata to each individual included in the network
  att <- merge(id, indiv, by = "pid", sort = FALSE, all.x = TRUE)


  snFull <- graph.data.frame(d = edges_df, vertices = att, directed = TRUE) ## all edges, all nodes

  snSurv <- delete.vertices(snFull, is.na(V(snFull)$didsurv)) ## removing individuals who didn't complete the survey themselves

  ## removing BOTH multiple edges and loops, and NOT saving any of the weight info that we could potentially be
  snFull_simp <- simplify(snFull, remove.multiple = TRUE, remove.loops = TRUE)
  snSurv_simp <- simplify(snSurv, remove.multiple = TRUE, remove.loops = TRUE)

  ## limiting edges to exclude double sample questions
  #edges_df_sing <- edges_df[, c(1:3, 5, 7, 9, 10, 11, 13)]
  #edges_df_sing <- subset(edges_df_sing, rowSums(edges_df_sing[,-c(1, 2)]) > 0)

  #sn_sing <- graph.data.frame(edges_df_sing)
  #id_sing <- data.frame("pid" = V(sn_sing)$name, stringsAsFactors = FALSE)
  #att_sing <- merge(id_sing, indiv, by = "pid", sort = FALSE, all.x = TRUE)
  #snFull_sing <- graph.data.frame(d = edges_df_sing, vertices = att_sing, directed = TRUE)
  #snSurv_sing <- delete.vertices(snFull_sing, is.na(V(snFull_sing)$didsurv))
  #snFull_sing_simp <- simplify(snFull_sing, remove.multiple = TRUE, remove.loops = TRUE)
  #snSurv_sing_simp <- simplify(snSurv_sing, remove.multiple = TRUE, remove.loops = TRUE)

  ## aggregating to household
  hh_df <- edges_df

  hh_df$EgoHH <- indiv[match(hh_df$Ego, indiv$pid), 2]
  hh_df$AlterHH <- indiv[match(hh_df$Alter, indiv$pid), 2]
  lens <- length(colnames(hh_df))
  hh_df <- hh_df[, c(lens - 1, lens, 3:(lens - 2))]
  hh_df <- hh_df[!(is.na(hh_df$EgoHH)), ]
  hh_df <- hh_df[!(is.na(hh_df$AlterHH)), ]

  snHH <- graph.data.frame(hh_df)
  idHH <- data.frame("hhid" = V(snHH)$name, stringsAsFactors = FALSE)

  attHH <- merge(idHH, hh, by="hhid", sort=FALSE, all.x = TRUE)


  snHHFull <- graph.data.frame(d = hh_df, vertices = attHH, directed = TRUE)

  snHHSurv <- delete.vertices(snHHFull, is.na(V(snHHFull)$didsurv))

  ## removing BOTH multiple edges and loops, and NOT saving any of the weight info that we could potentially be interested in.
  snHHFull_simp <- simplify(snHHFull, remove.multiple = TRUE, remove.loops = TRUE)
  snHHSurv_simp <- simplify(snHHSurv, remove.multiple = TRUE, remove.loops = TRUE)



  ## limiting edges to exclude double sample questions
  #hh_df_sing <- hh_df[,c(1:3, 5, 7, 9, 10, 11, 13)]
  #hh_df_sing <- subset(hh_df_sing, rowSums(hh_df_sing[, -c(1, 2)]) > 0)

  #snHH_sing <- graph.data.frame(hh_df_sing)
  #idHH_sing <- data.frame("hhid" = V(snHH_sing)$name, stringsAsFactors = FALSE)
  #attHH_sing <- merge(idHH_sing, hh, by="hhid", sort=FALSE, all.x = TRUE)
  #snHHFull_sing <- graph.data.frame(d = hh_df_sing, vertices = attHH_sing, directed = TRUE)
  #snHHSurv_sing <- delete.vertices(snHHFull_sing, is.na(V(snHHFull_sing)$didsurv))

  #snHHFull_sing_simp <- simplify(snHH_sing, remove.multiple = TRUE, remove.loops = TRUE)
  #snHHSurv_sing_simp <- simplify(snHHSurv_sing, remove.multiple = TRUE, remove.loops = TRUE)



  nets <- list("snFull" = snFull,
               "snFull_simp" = snFull_simp,
               "snSurv" = snSurv,
               "snSurv_simp" = snSurv_simp,
               #"snFull_sing" = snFull_sing,
               #"snFull_sing_simp" = snFull_sing_simp,
               #"snSurv_sing" = snSurv_sing,
               #"snSurv_sing_simp" = snSurv_sing_simp,
               "snHHFull" = snHHFull,
               "snHHFull_simp" = snHHFull_simp,
               "snHHSurv" = snHHSurv,
               "snHHSurv_simp" = snHHSurv_simp#,
               #"snHHFull_sing" = snHHFull_sing,
               #"snHHFull_sing_simp" = snHHFull_sing_simp,
               #"snHHSurv_sing" = snHHSurv_sing,
               #"snHHSurv_sing_simp" = snHHSurv_sing_simp
               )

  names(nets) <- paste0("vil", vilcount, "_", names(nets))

  count <- 9

  for (k in 1:(length(tietypes) )){
    #type <- c("borrowmoney", "lendmoney", "money", "helpdecision", "giveadvice", "advice", "keroricego", "keroricecome", "kerorice", "visitgo", "visitcome", "visit", "medic", "templecompany", "nonrel")[k]
    type <- names(tietypes)[k]
    nets[[count]] <- graph.adjacency(as.matrix(get.adjacency(snFull, attr = type)))
    V(nets[[count]])$hhid <- V(snFull)$hhid
    nets[[count]] <- simplify(nets[[count]], remove.loops = TRUE)
    names(nets)[[count]] <- paste0("vil", vilcount, "_snFull_", type)
    count <- count + 1
    nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snSurv, attr = type)))
    V(nets[[count]])$hhid <- V(snSurv)$hhid
    nets[[count]] <- simplify(nets[[count]], remove.loops = TRUE)
    names(nets)[[count]] <- paste0("vil", vilcount, "_snSurv_", type)
    count <- count + 1
    nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snHHFull, attr = type)))
    nets[[count]] <- simplify(nets[[count]], remove.loops = TRUE)
    names(nets)[[count]] <- paste0("vil", vilcount, "_snHHFull_", type)
    count <- count + 1
    nets[[count]]  <- graph.adjacency(as.matrix(get.adjacency(snHHSurv, attr = type)))
    nets[[count]] <- simplify(nets[[count]], remove.loops = TRUE)
    names(nets)[[count]] <- paste0("vil", vilcount, "_snHHSurv_", type)
    count <- count + 1
  }

  assign(paste0("vil", vilcount, "_nets"), nets)
}


IMnets <- list()
for (i in c(1:12, 14:21, 23:77)){
  nownets <- get(paste0("vil", i, "_nets"))
  for (k in 1:length(nownets)) {
    net_name <- names(nownets[k])
    IMnets[net_name] <- nownets[k]
  }
}

setwd("../..")

IMnets_atts <- read.csv("IM_net_atts.csv", header = TRUE, as.is=TRUE)

setwd("../..")
