rm(list=ls())

library(Morpho)

# **** LOOP IT ****

#read in data 
tlf <- read.table(unz("~/TraceLab/ExpAssets/Data/p1_2016-07-18 14:26:08/testing/session_1/p1_s1_b1_t1_2016-07-18.zip", "p1_s1_b1_t1_2016-07-18.tlf"), stringsAsFactors=FALSE, sep=",")
tlt <- read.table(unz("~/TraceLab/ExpAssets/Data/p1_2016-07-18 14:26:08/testing/session_1/p1_s1_b1_t1_2016-07-18.zip", "p1_s1_b1_t1_2016-07-18.tlt"), stringsAsFactors=FALSE, sep=",")

#create data frames
data_stim <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
data_resp <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlt))),ncol=4,nrow=length(tlt)/4, byrow=TRUE))

#remove artifacts 
data_resp_rem <- data_resp[!(data_resp$X1=="1919"&data_resp$X2=="1079"),]
data_resp_rem <- data_resp_rem[!(data_resp_rem$X1=="119"&data_resp_rem$X2=="1079"),]

#normalize to shortest segement *****FIX FOR OPPOSITE CASE*****
data_resp_rem$trialnum <- seq(from=1,to=length(data_resp_rem$X1),by=1)
rem_seq <- round(seq(from=1, to=225, by=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))/ifelse(length(data_resp_rem$X1)<length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))),digits=0)
data_resp_rem_sub <- data_resp_rem[c(rem_seq),]

##### PLOTS #####

#adding colour to points
rbPalstim <- colorRampPalette(c("grey","black"))
data_stim$Col <- rbPalstim(120)[as.numeric(cut(data_stim$X3,breaks=120))]
rbPalresp <- colorRampPalette(c("cyan","blue"))
data_resp_rem$Col <- rbPalresp(225)[as.numeric(cut(data_resp_rem$X3,breaks=225))]
rbPalrespsub <- colorRampPalette(c("yellow","green"))
data_resp_rem_sub$Col <- rbPalrespsub(120)[as.numeric(cut(data_resp_rem_sub$X3,breaks=120))]

# direction of movement: lighter to darker
# stim = grey to black, resp = cyan to blue, resp_sub = yellow to green

#plot points 
plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20, col=data_stim$Col)
points(data_resp_rem$X1,data_resp_rem$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20 ,col=data_resp_rem$Col)
points(data_resp_rem_sub$X1,data_resp_rem_sub$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20 ,col=data_resp_rem_sub$Col)


##### PROCRUSTES ANALYSIS #####

#take (x,y) coordinates only
stim <- data_stim[,1:2]
resp <- data_resp_rem_sub[,1:2]

#procrustes transformation
trans <- rotonto(stim, resp, scale = TRUE, signref = FALSE, reflection = FALSE, weights = NULL, centerweight = FALSE)

#get angle from rotation matrix ***in radians***
#should be same number for each... so just pick one:
rotation <- acos(trans$gamm[1,1])
# asin(trans$gamm[2,1]) 
# -asin(trans$gamm[1,2])
# acos(trans$gamm[2,2])

# we're not gonna screw up that bad.. yet... 



#test jack