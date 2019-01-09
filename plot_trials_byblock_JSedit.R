##### PLOT TRIALS - BY BLOCK ##### 
## by Tony Ingram & Jack Solomon ##

rm(list=setdiff(ls(), c())) # clear all but all_figs
graphics.off() # clear figures
cat("\014") # clear console

library(Morpho) # for procrustes analysis
library(plyr) # for control task analysis. *use plyr::count()
library(tidyverse) # arranging data at end & plotting

# DEFINE BLOCK OF INTEREST:

# data path
path <- "/Users/jacksolomon/Documents/Dalhousie/PhD-Neuroscience/Dissertation/OnlineTMS/Tracelab_TMS/TraceLab/ExpAssets/Data"

#list participants in analysis
participants = sort(unique(do.call(rbind,lapply(dir(path, recursive = FALSE, full.names = TRUE,pattern="\\.txt$"), read.csv, skip=16, sep='\t', header=T, stringsAsFactors = F))$participant))

# Participant number, session number, block number:
p <- participants[11] # use line 35 for participant[9]
s <- 1
b <- 5

# code for timing this script:
# ptm <- proc.time()

# Find all .zip files
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

blockpath <- sprintf("p%s_s%s_b%s", p, s, b)

file.names <- unique(grep(blockpath,file.names,value=TRUE))
file.names <- c(file.names[1],file.names[12],file.names[14:20],file.names[2:11],file.names[13])
#file.names <- c(file.names[1],file.names[7:14],file.names[2:6])

# Apply the function to all files.
for(i in 1:length(file.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        
        # read in data & create data frames
        data_stim <- data.frame(matrix(as.numeric(strsplit(gsub("\\[|\\]|\\(|\\)", "", read_lines(unz(file.names[i], name.tlf))), ",")[[1]]),ncol=3, byrow=TRUE))
        data_resp <- data.frame(matrix(as.numeric(strsplit(gsub("\\[|\\]|\\(|\\)", "", read_lines(unz(file.names[i], name.tlt))),",")[[1]]),ncol=3, byrow=TRUE))
        
        # skip missed trials
        if (nrow(data_resp)<5){
        }
        else{
        
        ### Pre-processing Trajectories ###
        
        #remove artifacts ([1919,1079] & [119,1079] now built into data collection)
        data_resp_rem <- data_resp[!(data_resp$X1=="239"&data_resp$X2=="1079"),]
        
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
        if(sum(clip_index)<10){
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
                rbPalsub <- colorRampPalette(c("pink","red"))
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
                
                plot(trans$X, xlim=c(-960,960), ylim=c(540,-540), pch=20, col=rbPalstim(length(trans$X))[as.numeric(cut(trans$X,breaks=length(trans$X)))])
                points(trans$Y,pch=20, col=rbPalresp(length(trans$Y))[as.numeric(cut(trans$Y,breaks=length(trans$Y)))])
                title(main = c(name.tlt, " proc"))
                }
        }
        
}

# determine script timing:
Rtime <- proc.time() - ptm
print(Rtime)

##### FIN #####