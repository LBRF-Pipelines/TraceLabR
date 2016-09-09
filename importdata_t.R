##### TRACELAB ANALYSIS #####
##### AUTHORED BY JACK SOLOMON AND TONY INGRAM #####

rm(list = ls()) # clear work space
graphics.off() # clear figures
cat("\014") # clear console

library(Morpho)
library(plyr)

# Read in .db information
participants <- read.csv("~/RStudio/TraceLabDB/participants.csv")
trials <- read.csv("~/RStudio/TraceLabDB/trials.csv", stringsAsFactors = FALSE)

##### PUT SPECIFIC TRIAL HERE #####

# Find .zip file for trial you want:
#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p8_2016-08-30 13:59:06/training/session_3/p8_s3_b5_t20_2016-09-01.zip"
#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p4_2016-08-26 11:06:02/testing/session_1/p4_s1_b1_t10_2016-08-26.zip"
#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p5_2016-08-26 11:56:53/testing/session_1/p5_s1_b3_t10_2016-08-26.zip"
#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p4_2016-08-26 11:06:02/testing/session_1/p4_s1_b5_t15_2016-08-26.zip"
file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p8_2016-08-30 13:59:06/testing/session_1/p8_s1_b4_t5_2016-08-30.zip"

out.file <- ""
# Apply the function to all files.
        name.tlf <- gsub(".zip",".tlf",basename(file.name))
        name.tlt <- gsub(".zip",".tlt",basename(file.name))
        name.pts <- gsub(".zip","_points.txt",basename(file.name))
        # read in data 
        tlf <- read.table(unz(file.name, name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.name, name.tlt),stringsAsFactors=FALSE, sep=",")
        pts <- read.table(unz(file.name, name.pts),stringsAsFactors=FALSE, sep=",")
        # separate PP data from MI and CC data (remember, final session of MI and CC are also PP sessions)
        
                #create data frames
                data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
                data_resp <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlt))),ncol=3,nrow=length(tlt)/3, byrow=TRUE))
                points <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(pts)), ", "))),ncol=2,nrow=length(pts)/2, byrow=TRUE))
                
                #remove artifacts 
                data_resp_rem <- data_resp #[!(data_resp$X1=="1919"&data_resp$X2=="1079"),]
                data_resp_rem <- data_resp_rem #[!(data_resp_rem$X1=="119"&data_resp_rem$X2=="1079"),]
                
                #find repeated points (from when people miss green, for example)
                clip_index <- rep(1, length(data_resp_rem$X1))
                #clip_index gives a vector of 1's and 0's where 0 means point 'i' has same [x,y] as point 'i-1'
                for(k in 2:(length(data_resp_rem$X1))){ #start at 2 as first point will never be same as previous
                        if(data_resp_rem[k,1]!=data_resp_rem[k-1,1] | data_resp_rem[k,2]!=data_resp_rem[k-1,2]){
                                clip_index[k] <- 1
                        }
                        else{
                                clip_index[k] <- 0
                        }
                }
                data_resp_clip <- cbind(data_resp_rem,clip_index)
                data_resp_clip <- data_resp_clip[!(data_resp_clip$clip_index==0),1:3]
                mt_clip <- max(data_resp_clip$X3)
                data_resp_rem <- data_resp_clip
                
                #normalize to shortest segement
                data_resp_rem$trialnum <- seq(from=1,to=length(data_resp_rem$X1),by=1)
                data_stim$trialnum <- seq(from=1,to=length(data_stim$X1),by=1)
                rem_seq <- round(seq(from=1, to=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1)), by=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))/ifelse(length(data_resp_rem$X1)<length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))),digits=0)
                data_sub <- if(length(data_resp_rem$X1)==length(rem_seq)) {data_stim[c(rem_seq),]} else {data_resp_rem[c(rem_seq),]}
                
                ##### PROCRUSTES ANALYSIS #####
                
                #take (x,y) coordinates only
                stim <- if(length(data_stim$X1)==length(data_sub$X1)) {data_stim[,c(1,2)]} else {data_sub[,c(1,2)]}
                resp <- if(length(data_resp_rem$X1)==length(data_sub$X1)) {data_resp_rem[,c(1,2)]} else {data_sub[,c(1,2)]}
                
                #procrustes transformation
                trans <- rotonto(stim, resp, scale = TRUE, signref = FALSE, reflection = FALSE, weights = NULL, centerweight = FALSE)
                
                #get translation
                translation <- sqrt((trans$transy[,1] - trans$trans[,1])^2 + (trans$transy[,2] - trans$trans[,2])^2)
                
                #get scale factor
                scale <- trans$bet
                
                #get rotation angle *radians*
                #from rotation matrix — don't know what I mean? wiki: rotation matrix
                #should be same number for each case below... so just pick one:
                rotation <- acos(trans$gamm[1,1])
                # asin(trans$gamm[2,1]) 
                # -asin(trans$gamm[1,2])
                # acos(trans$gamm[2,2])
                
                #get shape error (i.e. Procrustes SS)
                
                ProcSS <- sum(((trans$Y-trans$X)-mean((trans$Y-trans$X)))^2)
                ProcVar <- ProcSS/(length(stim[,1])-1)
                ProcSD <- sqrt(ProcVar)
                
                #get raw error
                
                stimaray <- data.matrix(stim)
                resparay <- data.matrix(resp)
                RawSS <- sum(((stimaray-resparay)-mean((stimaray-resparay)))^2)
                RawVar <- RawSS/(length(stim[,1])-1)
                RawSD <- sqrt(RawVar)
                
                ##### path length #####
                
                #get pathlength of participant response 
                
                segs <- matrix()
                for (y in 1:NROW(data_resp_rem)) {
                        seg_leg <- sqrt((data_resp_rem[y+1,1]-data_resp_rem[y,1])^2 + (data_resp_rem[y+1,2]-data_resp_rem[y,2])^2)
                        segs <- rbind(segs, seg_leg)
                }
                PLresp <- sum(segs, na.rm = TRUE)
                
                #stimulus pathlength
                
                segs <- matrix()
                for (u in 1:NROW(data_stim)) {
                        seg_leg <- sqrt((data_stim[u+1,1]-data_stim[u,1])^2 + (data_stim[u+1,2]-data_stim[u,2])^2)
                        segs <- rbind(segs, seg_leg)
                }
                PLstim <- sum(segs, na.rm = TRUE)
                
                ##### COMPLEXITY MEASURES #####
                
                ## as measured by extent of curvature using a modified sinuosity calculation 
                ## complexity = (stimulus pathlength) / (perimeter of straight lines between segment points)
                
                segs <- matrix()
                for (j in 1:NROW(points)) {
                        seg_leg <- sqrt((points[j+1,1]-points[j,1])^2 + (points[j+1,2]-points[j,2])^2)
                        segs <- rbind(segs, seg_leg)
                }
                perimeter <- sum(segs, na.rm = TRUE)
                
                complexity <- PLstim/perimeter
                
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
                
                
                dist = rep(0, length(trans$Y[,1]))
                for (h in 1:length(trans$Y[,1])){
                dist[h] = as.numeric(sqrt(((trans$Y[h,1]-trans$X[h,1])^2)+((trans$Y[h,2]-trans$X[h,2])^2)))
                }
                SSdist <- sum(dist^2)
                