##### TRACELAB ANALYSIS #####
##### AUTHORED BY JACK SOLOMON AND TONY INGRAM #####

## TO DO ##
# 1. look through and find out what loops can actually be functions!
# 2. go through and change loop inits that are = "" to something else... like = matrix()

rm(list = ls()) # clear work space
#graphics.off() # clear figures
#cat("\014") # clear console

library(Morpho)
library(plyr)
library(ggplot2)

# Read in .db information
participants <- read.csv("~/RStudio/TraceLabDB/participants.csv")
trials <- read.csv("~/RStudio/TraceLabDB/trials.csv", stringsAsFactors = FALSE)

# Find all .zip files
path <- "~/TraceLab/ExpAssets/Data"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

out.file <- "" # note, using "" here is why everything is output as a character string at the end, and needs reverting... messy code. Gotta refine. This should be = matrix()
# Apply the function to all files.
for(i in 1:length(file.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        name.pts <- gsub(".zip","_points.txt",basename(file.names[i]))
        # read in data 
        tlf <- read.table(unz(file.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        pts <- read.table(unz(file.names[i], name.pts),stringsAsFactors=FALSE, sep=",")
        # separate PP data from MI and CC data (remember, final session of MI and CC are also PP sessions)
        if (length(tlt)<15){
                # disclude all groups except CC
                if(trials[trials$figure_file==name.tlf,5]!='CC-00-5'){datarow=c(name.tlf,rep(NA,times=18))}
                # if in CC group, runs control task
                else{
                        #loads stimulus data
                        data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
                        
                        #extracts corrdinates for shape vertices (corners)
                        points <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(pts)), ", "))),ncol=2,nrow=length(pts)/2, byrow=TRUE))
                        
                        #index closest sample in stimulus data to vertices
                        index_vec <- ""
                        for(t in 1:5){
                                x_index <- abs(data_stim[,1]-points[t,1])
                                y_index <- abs(data_stim[,2]-points[t,2])
                                index <- which.min(x_index+y_index)
                                index_vec <- rbind(index_vec,index)
                        }
                        index_vec <- index_vec[-1,]
                        index_vec <- as.numeric(index_vec)
                        
                        #determines the direction of the begging of each line segment
                        direction_mat <- ""
                        for(n in 1:5){
                                direction <- round(colMeans(data_stim[index_vec[n]+2:7,1:2]))-(data_stim[index_vec[n]+1,1:2])
                                direction_mat <- rbind(direction_mat,direction)
                        }
                        direction_mat <- direction_mat[-1,]
                        direction_mat <- matrix(as.numeric(unlist(direction_mat)),ncol=2,dimnames = list(c("Corner1","Corner2","Corner3","Corner4","Corner5"),c("X","Y")))
                        dir_sign <- sign(direction_mat)
                        
                        #loads question of control task (ex.How many segments went "LEFT"?)
                        direction <- trials[trials$figure_file==name.tlf,16]
                        
                        #counts number of times segments went in the direction specified
                        if (direction=='LEFT'){
                                out <- count(dir_sign[,1])
                                corr.resp <- as.numeric(out[out$x==-1,2])
                        }
                        
                        if (direction=="RIGHT"){
                                out <- count(dir_sign[,1])
                                corr.resp <- as.numeric(out[out$x==1,2])
                        }
                        
                        if (direction=="DOWN"){
                                out <- count(dir_sign[,2])
                                corr.resp <- as.numeric(out[out$x==1,2])
                        }
                        
                        if (direction=="UP"){
                                out <- count(dir_sign[,2])
                                corr.resp <- as.numeric(out[out$x==-1,2])
                        }
                        datarow =c(name.tlf,rep(NA,times=17),corr.resp)
                }
        }
        else{
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
                #decide minimum response length — if not reached, report NA's for trial
                if(sum(clip_index)<10){
                        datarow=c(name.tlf,rep(NA,times=18))
                }
                else{
                        #remove all repeated response points (when person not moving)
                        data_resp_clip <- cbind(data_resp_rem,clip_index)
                        data_resp_clip <- data_resp_clip[!(data_resp_clip$clip_index==0),1:3]
                        data_resp_rem <- data_resp_clip
                        
                        #get new MT 
                        mt_clip <- max(data_resp_rem$X3)
                        
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
                        
                        ## SHAPE ERROR ##
                        
                        # create vector of point by point distances (error) between stimulus and response:
                        shape_dist = rep(0, length(trans$Y[,1]))
                        for (h in 1:length(shape_dist)){
                                shape_dist[h] = as.numeric(sqrt(((trans$Y[h,1]-trans$X[h,1])^2)+((trans$Y[h,2]-trans$X[h,2])^2)))
                        }
                        # error throughout trial:
                        shape_error_tot <- sum(shape_dist)
                        shape_error_mean <- mean(shape_dist)
                        shape_error_SD <- sd(shape_dist)
                        
                        # "ordinary procrustes sum of squares" and SD:
                        shape_procSS <- sum(shape_dist^2)
                        shape_procSD <- sqrt(shape_procSS/(length(shape_dist)-1))
                        
                        # NOTE: SD of error is NOT same as procSD — in SD you're subtracting each data point from the mean 
                        # and squaring that. For ProcSD you just take the distance between points and square that. It just 
                        # happens that in our error SD, the error's ARE distances between points. 
                        
                        ## RAW ERROR ##
                        
                        # create vector of point by point distances (error) between stimulus and response:
                        raw_dist = rep(0, length(resp[,1]))
                        for (m in 1:length(raw_dist)){
                                raw_dist[m] = as.numeric(sqrt(((resp[m,1]-stim[m,1])^2)+((resp[m,2]-stim[m,2])^2)))
                        }
                        # error throughout trial:
                        raw_error_tot <- sum(raw_dist)
                        raw_error_mean <- mean(raw_dist)
                        raw_error_SD <- sd(raw_dist)
                        
                        # pre-procrustes transform sum of squares and SD:
                        raw_procSS <- sum(raw_dist^2)
                        raw_procSD <- sqrt(raw_procSS/(length(raw_dist)-1))
                        
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
                        pathlength <- trials[trials$figure_file==name.tlf,12]
                        sinuosity <- pathlength/perimeter
                        complexity <- sinuosity
                        
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
                        
                        ##### save variables to a row & subsequently a file #####
                        
                        datarow <- c(name.tlf,PLstim,complexity,mt_clip,PLresp,raw_error_tot,raw_error_mean,raw_error_SD,raw_procSS,raw_procSD,translation,scale,rotation,shape_error_tot,shape_error_mean,shape_error_SD,shape_procSS,shape_procSD,rep(NA,times=1))
                }
        }
        out.file <- rbind(out.file, datarow)
}

#change output to df
df.out.file <- data.frame(out.file[-1,],stringsAsFactors = FALSE)
colnames(df.out.file) <- c("figure_file","PLstim","complexity","mt_clip","PLresp","raw_error_tot","raw_error_mean","raw_error_SD","raw_procSS","raw_procSD","translation","scale","rotation","shape_error_tot","shape_error_mean","shape_error_SD","shape_procSS","shape_procSD","correct_response")

#combine proc_df with db
all_data <- merge(trials,df.out.file,by="figure_file")
colnames(participants)[1] <- paste("participant_id")
all_data <- merge(participants[,c(1,4:6)],all_data,by="participant_id")
all_data <- all_data[c("participant_id","sex","age","handedness","condition","session_num","block_num","trial_num","figure_type","figure_file","stimulus_gt","stimulus_mt","avg_velocity","path_length","PLstim","complexity","trace_file","rt","it","mt","mt_clip","PLresp","raw_error_tot","raw_error_mean","raw_error_SD","raw_procSS","raw_procSD","translation","scale","rotation","shape_error_tot","shape_error_mean","shape_error_SD","shape_procSS","shape_procSD","control_question","control_response","correct_response")]

#change data to numeric where appropriate
all_data$condition <- as.factor(all_data$condition)
all_data$figure_type <- as.factor(all_data$figure_type)
all_data$PLstim <- as.numeric(all_data$PLstim)
all_data$complexity <- as.numeric(all_data$complexity)
all_data$mt_clip <- as.numeric(all_data$mt_clip)
all_data$PLresp <- as.numeric(all_data$PLresp)
all_data$raw_error_tot <- as.numeric(all_data$raw_error_tot)
all_data$raw_error_mean <- as.numeric(all_data$raw_error_mean)
all_data$raw_error_SD <- as.numeric(all_data$raw_error_SD)
all_data$raw_procSS <- as.numeric(all_data$raw_procSS)
all_data$raw_procSD <- as.numeric(all_data$raw_procSD)
all_data$translation <- as.numeric(all_data$translation)
all_data$scale <- as.numeric(all_data$scale)
all_data$rotation <- as.numeric(all_data$rotation)
all_data$shape_error_tot <- as.numeric(all_data$shape_error_tot)
all_data$shape_error_mean <- as.numeric(all_data$shape_error_mean)
all_data$shape_error_SD <- as.numeric(all_data$shape_error_SD)
all_data$shape_procSS <- as.numeric(all_data$shape_procSS)
all_data$shape_procSD <- as.numeric(all_data$shape_procSD)
all_data$correct_response <- as.integer(all_data$correct_response)

all_data <- all_data[with(all_data, order(participant_id, session_num, block_num, trial_num)), ]

#save .txt file with all_data
write.table(all_data,"~/RStudio/TraceLabDB/all_data.txt", sep="\t")

##### FIN #####