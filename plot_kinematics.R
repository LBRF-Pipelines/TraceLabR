#### plot kinematics ####
#### by Tony Ingram ####

# rm(list=setdiff(ls(), c())) # clear all but all_figs
# graphics.off() # clear figures
# cat("\014") # clear console

library(Morpho) # for procrustes analysis
library(plyr) # for control task analysis. *use plyr::count()
library(tidyverse) # arranging data at end & plotting

# DEFINE BLOCK OF INTEREST:

# Participant number, session number, block number:
p <- 42
s <- 1 # need two
b <- 1 # need two

# code for timing this script:
ptm <- proc.time()

# Find all .zip files
path <- "~/TraceLab/ExpAssets/Data"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

blockpath <- sprintf("p%s_s%s_b%s", p, s, b)

file.names <- unique(grep(blockpath,file.names,value=TRUE))
file.names <- c(file.names[1],file.names[12],file.names[14:20],file.names[2:11],file.names[13])

# Apply the function to all files.
for(i in 1:length(file.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        
        # read in data 
        tlf <- read.table(unz(file.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        
        # skip missed trials
        if (length(tlt)<15){
        }
        else{
                
                #create data frames
                data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
                data_resp <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlt))),ncol=3,nrow=length(tlt)/3, byrow=TRUE))
                
                ### Pre-processing Trajectories ###
                
                #remove artifacts (now built into data collection)
                data_resp_rem <- data_resp #[!(data_resp$X1=="1919"&data_resp$X2=="1079"),]
                data_resp_rem <- data_resp_rem #[!(data_resp_rem$X1=="119"&data_resp_rem$X2=="1079"),]
                
                #find repeated points (from when people miss target at end of trial, for example)
                clip_index <- rep(1, length(data_resp_rem$X1)) # get indices that need to be cut off
                #clip_index gives a vector of 1's and 0's where 0 means point 'i' has same [x,y] as point 'i-1'
                for(j in 2:(length(data_resp_rem$X1))){ #start at 2 as first point will never be same as previous
                        if(data_resp_rem[j,1]!=data_resp_rem[j-1,1] | data_resp_rem[j,2]!=data_resp_rem[j-1,2]){
                                clip_index[j] <- 1
                        }
                        else{
                                clip_index[j] <- 0
                        }
                }
                
                #decide minimum response length
                ratio <- nrow(data_resp_rem) / nrow(data_stim)
                
                if(sum(clip_index)<20 | (ratio < 0.5) | (ratio > 2.5)){
                        # do nothing — plot it all
                }
                else{
                        ### Pre-processing Trajectories Continued ###
                        
                        #remove all repeated response points (when person not moving)
                        data_resp_clip <- cbind(data_resp_rem,clip_index)
                        data_resp_clip <- data_resp_clip[!(data_resp_clip$clip_index==0),1:3]
                        data_resp_rem <- data_resp_clip
                        
                        #normalize to shortest trajectory
                        data_resp_rem$trialnum <- seq(from=1,to=length(data_resp_rem$X1),by=1)
                        data_stim$trialnum <- seq(from=1,to=length(data_stim$X1),by=1)
                        rem_seq <- round(seq(from=1, to=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1)), by=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))/ifelse(length(data_resp_rem$X1)<length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))),digits=0)
                        data_sub <- if(length(data_resp_rem$X1)==length(rem_seq)) {data_stim[c(rem_seq),]} else {data_resp_rem[c(rem_seq),]}
                        
                        ##### ERROR ANALYSIS - RAW #####
                        
                        #take (x,y) coordinates only
                        stim <- if(length(data_stim$X1)==length(data_sub$X1)) {data_stim[,c(1,2)]} else {data_sub[,c(1,2)]}
                        resp <- if(length(data_resp_rem$X1)==length(data_sub$X1)) {data_resp_rem[,c(1,2)]} else {data_sub[,c(1,2)]}
                        
                        # procrustes transformation
                        trans <- rotonto(stim, resp, 
                                         scale = TRUE, 
                                         signref = FALSE, 
                                         reflection = FALSE, 
                                         weights = NULL, 
                                         centerweight = FALSE
                        )
                        
                        ##### PLOTS #####
                        
                        #plot shapes pre transforms:
                        
                        #adding colour to points
                        #direction of movement: lighter to darker
                        #stim = grey to black, resp = cyan to blue, resp_sub = yellow to green
                        rbPalstim <- colorRampPalette(c("grey","black"))
                        data_stim$Col <- rbPalstim(length(data_stim$X3))[as.numeric(cut(data_stim$X3,breaks=length(data_stim$X3)))]
                        rbPalresp <- colorRampPalette(c("cyan","blue"))
                        data_resp_rem$Col <- rbPalresp(length(data_resp_rem$X3))[as.numeric(cut(data_resp_rem$X3,breaks=length(data_resp_rem$X3)))]
                        rbPalsub <- colorRampPalette(c("yellow","green"))
                        data_sub$Col <- rbPalsub(length(data_sub$X3))[as.numeric(cut(data_sub$X3,breaks=length(data_sub$X3)))]
                        
                        #plot points
                        plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20, col=data_stim$Col)
                        points(data_resp_rem$X1,data_resp_rem$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20 ,col=data_resp_rem$Col)
                        points(data_sub$X1,data_sub$X2, xlim=c(0,1920), ylim=c(1080,0),pch=20 ,col=data_sub$Col)
                        title(main = c(name.tlt, " raw"))
                        
                        #plot centroids (note that one of these is down sampled data)
                        points(trans$trans[1],trans$trans[2],pch=8,col="black")
                        points(trans$transy[1],trans$transy[2],pch=8,col="blue")
                        
                        #plot shapes post transforms:
                        
                        plot(trans$X, xlim=c(-960,960), ylim=c(540,-540))
                        points(trans$Y, col="red")
                        title(main = c(name.tlt, " proc"))
                }
        }
        
}

# determine script timing:
Rtime <- proc.time() - ptm
print(Rtime)

##### FIN #####