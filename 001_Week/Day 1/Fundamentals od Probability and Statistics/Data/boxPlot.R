aa <- c(7,9,9,10,10,10,10,11,11,13)
bb <- c(7,8,9,9,10,10,11,11,12,13)
 
cc <- c(3,  3,  6,  7,  7,  10,  10,  10,  11,  13,  30 )

datalist<-list()
dataL <- list()
dataL[["player1"]] = aa
dataL[["player2"]] = bb
dataL[["player3"]] = cc

boxOut <- boxplot(dataL)