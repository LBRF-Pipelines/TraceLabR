#### plot kinematics ####
#### by Jack Solomon ####

rm(list=setdiff(ls(), c())) # clear all
# graphics.off() # clear figures
cat("\014") # clear console

# code for timing this script:
ptm <- proc.time()

library(Morpho) # for procrustes analysis
library(tidyverse) # arranging data at end & plotting
library(plyr) #needed for rbind.fill
library(ggplot2)
library(ggthemes)

# Variables for Tony
path <- "~/TraceLab/ExpAssets/Data"
part_num <- 30
block_num <- 1:5
stim_speed <- 2500 # either 500,1000,1500,2000,2500
sess_num <- c(1,5)
# error <- 'less' # error indicates if you want the trial with more or less error for a block

#find trials of interest
load('all_data (.5 to 2.5).Rda')
part.dat <- all_data[all_data$participant_id==part_num,]
part.dat <- part.dat[part.dat$figure_type=='repeated',]
part.dat <- part.dat[part.dat$session_num==sess_num[1]|part.dat$session_num==sess_num[2],]
part.dat <- part.dat[part.dat$stimulus_gt==stim_speed,]
# {if (error == "less"){
#        good.trials <- {}
#        for (i in sess_num){
#                thissession <- part.dat[part.dat$session_num==i,]
#                errordif <- thissession[1,]$raw_error_tot-thissession[2,]$raw_error_tot
#                if (errordif >= 0){
#                        good.trials[i] <- 2+((i-1)*2)
#                }
#                else{
#                        good.trials[i] <- 1+((i-1)*2)
#                }
#        }
#        part.dat <- part.dat[good.trials,]
# }
# else{
#        bad.trials <- {}
#        for (i in sess_num){
#                thissession <- part.dat[part.dat$session_num==i,]
#                errordif <- thissession[1,]$raw_error_tot-thissession[2,]$raw_error_tot
#                if (errordif >= 0){
#                        bad.trials[i] <- 1+((i-1)*2)
#                }
#                else{
#                        bad.trials[i] <- 2+((i-1)*2)
#                }
#        }
#        part.dat <- part.dat[bad.trials,]
# }}

# Find corresponding .zip files
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")
trial.names <- {}
for (i in 1:length(part.dat[,1])){
        blockpath <- part.dat$figure_file[i]
        blockpath <- gsub(".tlf","",blockpath)
        trial.names[i] <- unique(grep(blockpath,file.names,value=TRUE))
}

# Apply the function to all files.
resp <- {}
stim <- {}
for(i in 1:length(trial.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(trial.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(trial.names[i]))
        
        # read in data 
        tlf <- read.table(unz(trial.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(trial.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        
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
                        
                        #identify session and block and trial number
                        
                        thisses <- as.numeric(substr(name.tlf, regexpr('_s', name.tlf)[[1]][1]+2, regexpr('_b', name.tlf)[[1]][1]-1))
                        thisblk <- as.numeric(substr(name.tlf, regexpr('_b', name.tlf)[[1]][1]+2, regexpr('_t', name.tlf)[[1]][1]-1))
                        thistrl <- as.numeric(substr(name.tlf, regexpr('_t', name.tlf)[[1]][1]+2, regexpr('_20', name.tlf)[[1]][1]-1))
                        
                        #take (x,y) coordinates only
                        thisstim <- if(length(data_stim$X1)==length(data_sub$X1)) {cbind(thisses,thisblk,thistrl,data_stim[,c(1,2)])} else {cbind(thisses,thisblk,thistrl,data_sub[,c(1,2)])}
                        thisresp <- if(length(data_resp_rem$X1)==length(data_sub$X1)) {cbind(thisses,thisblk,thistrl,data_resp_rem[,c(1,2)])} else {cbind(thisses,thisblk,thistrl,data_sub[,c(1,2)])}
                        #save responses for all trials in one matrix 
                        if(empty(resp)){
                                resp=thisresp
                                stim=thisstim
                        }
                        else{
                                resp=rbind(resp,thisresp)
                                stim=rbind(stim,thisstim)
                        }
                        # procrustes transformation
                        #trans <- rotonto(stim, resp, 
                        #                 scale = TRUE, 
                        #                 signref = FALSE, 
                        #                 reflection = FALSE, 
                        #                 weights = NULL, 
                        #                 centerweight = FALSE
                        #)
                }
        }
}

allresp=data.frame(resp)
rownames(allresp) <- NULL

allstim=data.frame(stim)
rownames(allstim) <- NULL
##### PLOTS #####

#plot shapes pre transforms:

# find longest stimulus & replicate for both sessions
longtrial <- {}
trials <- levels(with(allstim, interaction(thisses,thisblk,thistrl,drop=TRUE)))
for (i in 1:length(trials)){
        thisTrial=as.numeric(unlist(strsplit(trials[i], "[.]")))
        longtrial[i]=nrow(allstim[allstim$thisses==thisTrial[1]&allstim$thisblk==thisTrial[2]&allstim$thistrl==thisTrial[3],])
}
stimtrial <- as.numeric(unlist(strsplit(trials[which.max(longtrial)], "[.]")))
stim <- allstim[allstim$thisses==stimtrial[1]&allstim$thisblk==stimtrial[2]&allstim$thistrl==stimtrial[3],]
stim= rbind(stim,stim)
rownames(stim) <- NULL
{if(stim$thisses[1]==1){
        stim$thisses[(nrow(stim)/2+1):nrow(stim)]=5
} else {
        stim$thisses[(nrow(stim)/2+1):nrow(stim)]=1
}}

#organize data for plotting
stim$type <- "stim"
allresp$type <- "resp"
alld <- rbind(allresp,stim)

ggplot(alld, aes(x=X1,y=X2), group=type) +
        facet_grid(thisses ~ .) +
        geom_path(aes(linetype = type)
                  , alpha = 1) +
        scale_linetype_manual(values = c("dotted", "solid")
                              , labels = c("Response", "Stimulus")) +
        scale_y_reverse(lim=c(1080,0)) +
        scale_x_continuous(lim=c(0,1920)) +
        theme_tufte() +
        labs(# title = "Participant Responses" ,
                x = "x-axis (pixels)" ,
                y = "y-axis (pixels)" ,
                linetype = "")

# determine script timing:
Rtime <- proc.time() - ptm
print(Rtime)

##### FIN #####