#YWB cf and meal sharing networks

#load packages
#library(igraph)

cfreceiving <- read.csv("data/YWB/YWB_cfreceiving.csv", header=TRUE, row.names = 1)[,2:1]
cfgiving <- read.csv("data/YWB/YWB_cfgiving.csv", header=TRUE, row.names = 1)
mealreceiving <- read.csv("data/YWB/YWB_mealreceiving.csv", header=TRUE, row.names = 1)[,2:1]
mealgiving <- read.csv("data/YWB/YWB_mealgiving.csv", header=TRUE, row.names = 1)

#cffull <- rbind(cfgiving, cfreceiving)
#mealfull <- rbind(mealgiving, mealreceiving)

allfoodgiving <- rbind(cfgiving, mealgiving)
allfoodreceiving <-rbind(cfreceiving, mealreceiving)
#allfood <- rbind(cffull, mealfull)

#get list of in-sample Inuit
InHIDs <- c(2,   3,   5,   8,   9,   14,  16,  17,  18,  20,
            23,  24,  27,  30,  31,  32,  33,  34,  36,  37,
            38,  39,  40,  41,  42,  43,  45,  46,  47,  50,
            51,  52,  53,  54,  55,  56,  60,  61,  62,  63,
            64,  65,  66,  75,  79,  80,  83,  84,  87,  88,
            90,  93,  94,  95,  96,  98,  102, 103, 104, 106,
            108, 109, 110, 111, 112, 114, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 127, 128, 130, 131, 137,
            138, 139, 140, 142, 143, 144, 145, 149, 150, 151,
            152, 153, 154, 156, 158, 159, 160, 161, 162, 163,
            164, 166, 167, 171, 172, 173, 174, 177, 178, 179)
NomInHH <- c("888","999", "1", "15", "19", "22", "28", "29", "48", "57", "58",
             "67", "68", "69", "70", "71", "72", "73", "74", "76",  "77",
             "78", "132", "133", "134", "135", "136", "165", "169", "170")
full <- c(1:24, 27:48, 50:84, 86:156, 158:181)
allIn <- full[!(full %in% NomInHH)]

#get list of nets
nets <- c("cfreceiving", "cfgiving", #"cffull",
          "mealreceiving", "mealgiving", #"mealfull",
          "allfoodreceiving", "allfoodgiving")#, "allfood")
#note excluding vs including hhs listed outside of the village

YWBnets <- list()
i <- 1
for (net in nets) {
  temp <- get(net)
  #first networks with just those named (excluding outside the village)
  temp1 <- temp
  #remove NAs
  temp1 <- temp1[!is.na(temp1[,1]),]
  temp1 <- temp1[!is.na(temp1[,2]),]
  #remove non-beneficiaries
  temp1 <- temp1[!(temp1[,1] %in% NomInHH),]
  temp1 <- temp1[!(temp1[,2] %in% NomInHH),]
  #NAMED VS ALL HOUSEHOLDS IN THE VILLAGE?
  tempnet1 <- graph.data.frame(temp1, directed=TRUE)
  tempnet1 <- simplify(tempnet1, remove.multiple=TRUE)
  assign(paste0(net, "net_named"), tempnet1)
  name <- paste0("YWB_", net, "net_named")
  YWBnets[[name]] <- tempnet1
  i <- i+1
  #networks of survey with Inuit only
  temp2 <- temp
  temp2 <- temp2[(temp2[,1] %in% InHIDs),]
  temp2 <- temp2[(temp2[,2] %in% InHIDs),]
  tempnet2 <- make_empty_graph(n=110, directed=TRUE)
  V(tempnet2)$name <- as.character(InHIDs)
  edgeset <- as.numeric(as.vector(t(temp2)))
  b <- as.numeric(factor(edgeset, levels=V(tempnet2)$name))
  tempnet2 <- add_edges(tempnet2, b)
  tempnet2 <- simplify(tempnet2, remove.multiple=TRUE)
  assign(paste0(net, "net_sample"), tempnet2)
  name <- paste0("YWB_", net, "net_sample")
  YWBnets[[name]] <- tempnet2
  i <- i+1
}

YWBnets_atts <- read.csv("data/YWB/YWB_net_atts.csv", header=TRUE, row.names=1, stringsAsFactors=FALSE)
