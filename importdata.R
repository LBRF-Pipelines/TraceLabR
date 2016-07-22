##### TRACELAB ANALYSIS #####
##### AUTHORED BY JACK SOLOMON AND TONY INGRAM #####

rm(list = ls())

library(Morpho)

#read in .db information
participants <- read.csv("~/RStudio/TraceLabDB/participants.csv")
trials <- read.csv("~/RStudio/TraceLabDB/trials.csv")

# Find all .zip files
path <- "~/TraceLab/ExpAssets/Data"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

out.file <- ""
# Apply the function to all files.
for(i in 1:length(file.names)){
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        #read in data 
        tlf <- read.table(unz(file.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        
        #create data frames
        data_stim <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
        data_resp <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlt))),ncol=4,nrow=length(tlt)/4, byrow=TRUE))
        
        #remove artifacts 
        data_resp_rem <- data_resp[!(data_resp$X1=="1919"&data_resp$X2=="1079"),]
        data_resp_rem <- data_resp_rem[!(data_resp_rem$X1=="119"&data_resp_rem$X2=="1079"),]
        
        #normalize to shortest segement *****FIX FOR OPPOSITE CASE*****
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
        for (i in 1:NROW(resp)) {
                seg_leg <- sqrt((resp[i+1,1]-resp[i,1])^2 + (resp[i+1,2]-resp[i,2])^2)
                segs <- rbind(segs, seg_leg)
        }
        PLresp <- sum(segs, na.rm = TRUE)
        
        #stimulus pathlength
        
        segs <- matrix()
        for (i in 1:NROW(stim)) {
                seg_leg <- sqrt((stim[i+1,1]-stim[i,1])^2 + (stim[i+1,2]-stim[i,2])^2)
                segs <- rbind(segs, seg_leg)
        }
        PLstim <- sum(segs, na.rm = TRUE)
        
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
        
        #plot centroids (note that one of these is down sampled data)
        points(trans$trans[1],trans$trans[2],pch=8,col="black")
        points(trans$transy[1],trans$transy[2],pch=8,col="blue")
        
        #plot shapes post transforms: ******set limits to max/min+50******
        
        plot(trans$X, xlim=c(-1000,1000), ylim=c(-1000,1000))
        points(trans$Y, col="red")
        
        ##### save variables to a row & subsequently a file #####
        
        datarow <- c(name.tlt,PLstim,PLresp,RawSS,RawSD,translation,scale,rotation,ProcSS,ProcSD)
        
        out.file <- rbind(out.file, datarow)
}

#change output to df
df.out.file <- data.frame(out.file[-1,])
colnames(df.out.file) <- c("trace_file","PLstim","PLresp","RawSS","RawSD","translation","scale","rotation","ProcSS","ProcSD")

#combine proc_df with db
all_data <- merge(trials,df.out.file,by="trace_file")
colnames(participants)[1] <- paste("participant_id")
all_data <- merge(participants[,c(1,4:6)],all_data,by="participant_id")
all_data <-all_data[c("id","participant_id","sex","age","handedness","condition","session_num","block_num","trial_num","figure_file","stimulus_gt","stimulus_mt","avg_velocity","path_length","PLstim","trace_file","rt","seg_count","seg_estimate","mt","PLresp","RawSS","RawSD","translation","scale","rotation","ProcSS","ProcSD")]

#save .txt file with all_data
write.table(all_data,"~/RStudio/TraceLabDB/all_data.txt", sep="\t")

##### speed accuracy functions ##### 
# probably a different file... 