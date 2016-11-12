##### TRACELAB IMPORT AND MAIN ANALYSIS #####
### AUTHORED BY JACK SOLOMON AND TONY INGRAM ###

## TO DO ##
# 1. go through and change loop inits that are = "" to something else... like = matrix()
# 2. get write.table working so can use other scripts without having to re-run this...
# 3. use ggplot2 to make better plots
# 4. impliment DTW (with SD)

rm(list=setdiff(ls(), "all_figs")) # clear all but all_figs
# rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but all_figs & all_data
# graphics.off() # clear figures
# cat("\014") # clear console

library(Morpho) # for procrustes analysis
library(plyr) # for control task analysis. *use plyr::count()
library(dplyr) # arranging data at end
library(ggplot2) # plotting
library(pracma) # for ApEn and SampEn
library(bezier) # for bezier curve analysis
library(dtw) # for Dynamic Time Warping

# Curvature of Bezier Curve at t points:
bcurv = function(t, p){
        b1x = (2 * (1-t) * (p[2,1]-p[1,1])) + (2 * t * (p[3,1] - p[2,1])) #first derivative of x
        b1y = (2 * (1-t) * (p[2,2]-p[1,2])) + (2 * t * (p[3,2] - p[2,2])) #first derivative of y
        b2x = (2 * (p[3,1] - (2 * p[2,1]) + p[1,1])) #second derivative of x
        b2y = (2 * (p[3,2] - (2 * p[2,2]) + p[1,2])) #second derivative of y
        bez_curvature <- ((b1x * b2y) - (b1y * b2x))/(((b1x^2) + (b1y^2))^(3/2)) #signed curvature
        return(bez_curvature)
}

# Read in .db information
participants <- read.csv("~/Documents/RStudio/TraceLabDB/participants.csv")
trials <- read.csv("~/Documents/RStudio/TraceLabDB/trials.csv", stringsAsFactors = FALSE)

# Find all .zip files
path <- "~/TraceLab/ExpAssets/Data/"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

out.file <- "" # note, using "" here is why everything is output as a character string at the end, and needs reverting... messy code. Gotta refine. This should be = matrix()
# Apply the function to all files.
for(i in 1:length(file.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        name.tlfp <- gsub(".zip",".tlfp",basename(file.names[i]))
        name.tlfs <- gsub(".zip",".tlfs",basename(file.names[i]))
        # read in data 
        tlf <- read.table(unz(file.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        tlfp <- read.table(unz(file.names[i], name.tlfp),stringsAsFactors=FALSE, sep=",")
        tlfs <- read.table(unz(file.names[i], name.tlfs),stringsAsFactors=FALSE, sep=",")
        # separate PP data from MI and CC data (remember, final session of MI and CC are also PP sessions)
        if (length(tlt)<15){
                # disclude all groups except CC
                if(trials[trials$figure_file==name.tlf,5]!='CC-00-5'){datarow=c(name.tlf,rep(NA,times=25))}
                # if in CC group, runs control task
                else{
                        #loads stimulus data
                        data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
                        
                        #extracts coordinates for shape vertices (corners)
                        vertices <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlfp)), ", "))),ncol=2,nrow=length(tlfp)/2, byrow=TRUE))
                        
                        #index closest sample in stimulus data to vertices
                        index_vec <- ""
                        for(t in 1:5){
                                x_index <- abs(data_stim[,1]-vertices[t,1])
                                y_index <- abs(data_stim[,2]-vertices[t,2])
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
                                out <- plyr::count(dir_sign[,1])
                                corr.resp <- as.numeric(out[out$x==-1,2])
                        }
                        
                        if (direction=="RIGHT"){
                                out <- plyr::count(dir_sign[,1])
                                corr.resp <- as.numeric(out[out$x==1,2])
                        }
                        
                        if (direction=="DOWN"){
                                out <- plyr::count(dir_sign[,2])
                                corr.resp <- as.numeric(out[out$x==1,2])
                        }
                        
                        if (direction=="UP"){
                                out <- plyr::count(dir_sign[,2])
                                corr.resp <- as.numeric(out[out$x==-1,2])
                        }
                        datarow =c(name.tlf,rep(NA,times=24),corr.resp)
                }
        }
        else{
                #create data frames
                data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))
                data_resp <- data.frame(matrix(as.numeric(gsub("\\[|\\]|\\(|\\)", "", as.character(tlt))),ncol=3,nrow=length(tlt)/3, byrow=TRUE))
                vertices <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlfp)), ", "))),ncol=2,nrow=length(tlfp)/2, byrow=TRUE))
                ctrl_pts <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlfs)), ", "))),ncol=2,nrow=length(tlfs)/2, byrow=TRUE)) 
                
                ### Pre-processing Trajectories ###
                
                # rearrange control points — currently [point, point, ctrl]..., needs to be [point, ctrl, point]
                for(j in 1:nrow(ctrl_pts)){
                        if (j %% 3 == 0){
                                ctrl_pts[(j-1):j,] <- ctrl_pts[j:(j-1),]
                        }
                } # every 3rd row switched with previous row
                
                # rearrange ctrl_pts to get rid of repeated points — this is needed for the bezier package
                ctrl_pts_rm <- ctrl_pts[c(1:3,5:6,8:9,11:12,14:15),]
                
                #remove artifacts (now built into data collection)
                data_resp_rem <- data_resp #[!(data_resp$X1=="1919"&data_resp$X2=="1079"),]
                data_resp_rem <- data_resp_rem #[!(data_resp_rem$X1=="119"&data_resp_rem$X2=="1079"),]
                
                #find repeated points (from when people miss green, for example)
                clip_index <- rep(1, length(data_resp_rem$X1))
                #clip_index gives a vector of 1's and 0's where 0 means point 'i' has same [x,y] as point 'i-1'
                for(j in 2:(length(data_resp_rem$X1))){ #start at 2 as first point will never be same as previous
                        if(data_resp_rem[j,1]!=data_resp_rem[j-1,1] | data_resp_rem[j,2]!=data_resp_rem[j-1,2]){
                                clip_index[j] <- 1
                        }
                        else{
                                clip_index[j] <- 0
                        }
                }
                #decide minimum response length — if not reached, report NA's for trial
                if(sum(clip_index)<10){
                        datarow=c(name.tlf,rep(NA,times=25))
                }
                else{
                        ### Pre-processing Trajectories Continued ###
                        
                        #remove all repeated response points (when person not moving)
                        data_resp_clip <- cbind(data_resp_rem,clip_index)
                        data_resp_clip <- data_resp_clip[!(data_resp_clip$clip_index==0),1:3]
                        data_resp_rem <- data_resp_clip
                        
                        #get new MT 
                        mt_clip <- max(data_resp_rem$X3)
                        
                        #normalize to shortest trajectory
                        data_resp_rem$trialnum <- seq(from=1,to=length(data_resp_rem$X1),by=1)
                        data_stim$trialnum <- seq(from=1,to=length(data_stim$X1),by=1)
                        rem_seq <- round(seq(from=1, to=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1)), by=ifelse(length(data_resp_rem$X1)>length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))/ifelse(length(data_resp_rem$X1)<length(data_stim$X1),length(data_resp_rem$X1),length(data_stim$X1))),digits=0)
                        data_sub <- if(length(data_resp_rem$X1)==length(rem_seq)) {data_stim[c(rem_seq),]} else {data_resp_rem[c(rem_seq),]}
                        
                        
                        ##### ERROR ANALYSIS - RAW #####
                        
                        #take (x,y) coordinates only
                        stim <- if(length(data_stim$X1)==length(data_sub$X1)) {data_stim[,c(1,2)]} else {data_sub[,c(1,2)]}
                        resp <- if(length(data_resp_rem$X1)==length(data_sub$X1)) {data_resp_rem[,c(1,2)]} else {data_sub[,c(1,2)]}
                        
                        
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
                        # NOTE: SD of error is NOT same as procSD — in SD you're subtracting each data point from the mean 
                        # and squaring that. For ProcSD you just take the distance between points and square that. It just 
                        # happens that in our error SD, the error's ARE distances between points. 
                        
                        
                        ## SHAPE ERROR ##
                        
                        #procrustes transformation
                        trans <- rotonto(stim, resp, scale = TRUE, signref = FALSE, reflection = FALSE, weights = NULL, centerweight = FALSE)
                        
                        #get translation
                        translation <- sqrt((trans$transy[,1] - trans$trans[,1])^2 + (trans$transy[,2] - trans$trans[,2])^2)
                        
                        #get scale factor
                        scale <- trans$bet
                        
                        #get rotation angle *radians* from rotation matrix
                        rotation <- acos(trans$gamm[1,1])
                        
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
                        
                        
                        
                        
                        ## Dynamic Time Warping (Multivariate) ##
                        
                        dtw <- dtw(x = resp, y = stim,
                                   dist.method = "Euclidean",
                                   step.pattern = symmetric1,
                                   window.type = "none",
                                   keep.internals = FALSE,
                                   distance.only = TRUE,
                                   open.end = FALSE,
                                   open.begin = FALSE
                        )
                        
                        dtw_tot <- dtw$distance
                        dtw_mean <- dtw$distance/nrow(stim)
                        # dtw_SD <- ## impliment this
                        
                        
                        ##### path length #####
                        
                        # get path length of participant response 
                        segs <- matrix()
                        for (y in 1:NROW(data_resp_rem)) {
                                seg_leg <- sqrt((data_resp_rem[y+1,1]-data_resp_rem[y,1])^2 + (data_resp_rem[y+1,2]-data_resp_rem[y,2])^2)
                                segs <- rbind(segs, seg_leg)
                        }
                        PLresp <- sum(segs, na.rm = TRUE)
                        
                        # stimulus path length
                        segs <- matrix()
                        for (u in 1:NROW(data_stim)) {
                                seg_leg <- sqrt((data_stim[u+1,1]-data_stim[u,1])^2 + (data_stim[u+1,2]-data_stim[u,2])^2)
                                segs <- rbind(segs, seg_leg)
                        }
                        PLstim <- sum(segs, na.rm = TRUE)
                        
                        # get path length from bezier curves
                        bezfig_len <- bezier::bezierArcLength(
                                ctrl_pts_rm
                                , deg = 2
                        )$arc.length
                        
                        
                        ##### COMPLEXITY MEASURES #####
                        
                        ## as measured by extent of curvature using a modified sinuosity calculation 
                        ## sinuosity = (stimulus pathlength) / (perimeter of straight lines between segment points)
                        
                        segs <- matrix()
                        for (j in 1:NROW(vertices)) {
                                seg_leg <- sqrt((vertices[j+1,1]-vertices[j,1])^2 + (vertices[j+1,2]-vertices[j,2])^2)
                                segs <- rbind(segs, seg_leg)
                        }
                        perimeter <- sum(segs, na.rm = TRUE)
                        pathlength <- trials[trials$figure_file==name.tlf,12]
                        sinuosity <- pathlength/perimeter
                        
                        ## as measured by TOTAL CURVATURE: https://en.wikipedia.org/wiki/Total_curvature
                        figlength <- nrow(data_stim) # just to see if values change for same shape at diff sampling rates
                        
                        # create vector from 1 to 100, with length based on amount of points captured: 
                        s1 <- seq(from = 1, to = 100, length.out = nrow(data_stim))
                        
                        # make functions x(t) and y(t) from which you can take derivatives:
                        xt.spl <- splinefun(x = s1, y = data_stim$X1) 
                        yt.spl <- splinefun(x = s1, y = data_stim$X2)
                        
                        # create vector from 1 to 100, "s2" with 10000 interpolated points:
                        #s2 <- seq(min(data_stim$X3), max(data_stim$X3), length.out = 10000)
                        s2 <- seq(min(s1), max(s1), length.out = 10000)
                        
                        # calculate curvature:
                        curvature = (
                                (xt.spl(s2, deriv=1) * yt.spl(s2, deriv=2)) - (yt.spl(s2, deriv=1) * xt.spl(s2, deriv=2)))/
                                ((xt.spl(s2, deriv=1)^2 + yt.spl(s2, deriv=1)^2)^(3/2)) #signed curvature
                        
                        # remove extreme values due to differentiation, particularly at figure vertices:
                        remove_outliers <- function(curv_in, na.rm = TRUE, ...) {
                                x <- curv_in
                                qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
                                H <- 6 * IQR(x, na.rm = na.rm)
                                y <- x
                                y[x < (qnt[1] - H)] <- NA
                                y[x > (qnt[2] + H)] <- NA
                                y
                        }
                        curv_in <- curvature
                        curvature <- remove_outliers(curv_in)
                        
                        # total curvature
                        curvature.spl <- splinefun(s2, curvature)
                        totcurv <- tryCatch(
                                integrate(Vectorize(curvature.spl), lower = min(s2), upper = max(s2), abs.tol = 0, subdivisions=1000)$value
                                , error=function(err) NA
                        ) # tryCatch does a great job of preventing extreme values
                        
                        # total absolute curvature
                        abscurv <- abs(curvature) # unsigned curvature
                        abscurv.spl <- splinefun(s2, abscurv)
                        totabscurv <- tryCatch(
                                integrate(Vectorize(abscurv.spl), lower = min(s2), upper = max(s2), abs.tol = 0, subdivisions=1000)$value
                                , error=function(err) NA
                        )
                        
                        # tortuosity: the integral of the change on curvature
                        dcurvedt <- curvature.spl(s2, deriv=1) # note this introduces more variabilty in data, again.
                        abs.dcurvdt <- abs(dcurvedt)
                        abs.dcurvedt.spl <- splinefun(s2, abs.dcurvdt)
                        tortuosity <- tryCatch(
                                integrate(abs.dcurvedt.spl, lower = min(s2), upper = max(s2), abs.tol = 0, subdivisions=1000)$value
                                , error=function(err) NA
                        )
                        
                        # other meausres based on curvature
                        curvsum <- sum(abscurv, na.rm = TRUE) # sum of absolute curvature values for 10000 points
                        #curvmean <- mean(abscurv, na.rm = TRUE) # mean of absolute curvature
                        #curvSD <- sd(abscurv, na.rm = TRUE) # SD of absolute curvature
                        
                        ## approximate entropy based measures
                        
                        # first normalize trajectory to 100 evenly spaced points using splines:
                        # s1 <- seq(from = 1, to = 100, length.out = nrow(data_stim)) # reparameterize to arbitrary points (1 to 100) preserving number of points captured
                        # xt.spl <- splinefun(x = s1, y = data_stim$X1)
                        # yt.spl <- splinefun(x = s1, y = data_stim$X2)
                        s3 <- seq(min(s1), max(s1), length.out = 100) # interpolate to 100 points
                        datastim <- matrix(c(as.vector(xt.spl(s3)),as.vector(yt.spl(s3))),ncol=2)
                        
                        #create "turning angle" sequence, reducing 2D (x,y) to 1D (relative angle):
                        stim_theta <- rep(0, length(datastim[,1])-2) # note you always lose two points
                        for (a in 1:length(stim_theta)){
                                V1 = c(datastim[a+1,1],datastim[a+1,2]) - c(datastim[a,1],datastim[a,2])
                                V2 = c(datastim[a+2,1],datastim[a+2,2]) - c(datastim[a+1,1],datastim[a+1,2])
                                stim_theta[a] = atan2(V2[2],V2[1]) - atan2(V1[2],V1[1])
                                if (abs(stim_theta[a]) > pi){
                                        stim_theta[a] = stim_theta[a] - ((2*pi)*sign(stim_theta[a]))
                                }
                        }
                        
                        # # SMOOTH that sequence... 
                        # datastim_x_smooth <- smooth.spline(s3, datastim[,1], df = .5*length(datastim[,1]))
                        # datastim_y_smooth <- smooth.spline(s3, datastim[,2], df = .5*length(datastim[,2]))
                        # datasmooth <- matrix(c(as.vector(predict(datastim_x_smooth, x = s3, deriv = 0)$y), as.vector(predict(datastim_y_smooth, x = s3, deriv = 0)$y)), ncol=2)
                        # 
                        # stim_theta <- rep(0, length(datasmooth[,1])-2) # note you always lose two points
                        # for (a in 1:length(stim_theta)){
                        #         V1 = c(datasmooth[a+1,1],datasmooth[a+1,2]) - c(datasmooth[a,1],datasmooth[a,2])
                        #         V2 = c(datasmooth[a+2,1],datasmooth[a+2,2]) - c(datasmooth[a+1,1],datasmooth[a+1,2])
                        #         stim_theta[a] = atan2(V2[2],V2[1]) - atan2(V1[2],V1[1])
                        #         if (abs(stim_theta[a]) > pi){
                        #                 stim_theta[a] = stim_theta[a] - ((2*pi)*sign(stim_theta[a]))
                        #         }
                        # }
                        
                        # approximate entropy and sample entropy: 
                        ApEn_stim <- approx_entropy(stim_theta)
                        SaEn_stim <- sample_entropy(stim_theta)
                        # note that ApEn likely better since already standardized the sequence length.
                        
                        # old stuff that worked a lot better: s3 <- seq(min(s1), max(s1), length.out = 100)
                        # data_stim_x <- xt.spl(s3) # used splines so that's not it... 
                        # data_stim_y <- yt.spl(s3)
                        # ApEn_stim <- approx_entropy(c(data_stim_x,data_stim_y)) # it's that sampling rates affect angles a lot apparently... 
                        # SaEn_stim <- sample_entropy(c(data_stim_x,data_stim_y)) # perhaps try smoothing?
                        # works great! but treats x and y as one long vector
                        
                        
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
                        
                        datarow <- c(name.tlf,PLstim,figlength,sinuosity,totcurv,totabscurv,tortuosity,curvsum,ApEn_stim,SaEn_stim,mt_clip,PLresp,raw_error_tot,raw_error_mean,raw_error_SD,raw_procSS,raw_procSD,translation,scale,rotation,shape_error_tot,shape_error_mean,shape_error_SD,shape_procSS,shape_procSD,rep(NA,times=1))
                }
        }
        out.file <- rbind(out.file, datarow)
}

# change output to df
df.out.file <- data.frame(out.file[-1,],stringsAsFactors = FALSE)
colnames(df.out.file) <- c("figure_file","PLstim","figlength","sinuosity","totcurv","totabscurv","tortuosity","curvsum","ApEn_stim","SaEn_stim","mt_clip","PLresp","raw_error_tot","raw_error_mean","raw_error_SD","raw_procSS","raw_procSD","translation","scale","rotation","shape_error_tot","shape_error_mean","shape_error_SD","shape_procSS","shape_procSD","correct_response")

# combine proc_df with db
all_data <- merge(trials,df.out.file,by="figure_file")
colnames(participants)[1] <- paste("participant_id")
all_data <- merge(participants[,c(1,4:6)],all_data,by="participant_id")
all_data <- all_data[c("participant_id","sex","age","handedness","condition","session_num","block_num","trial_num","figure_type","figure_file","stimulus_gt","stimulus_mt","avg_velocity","path_length","PLstim","figlength","sinuosity","totcurv","totabscurv","tortuosity","curvsum","ApEn_stim","SaEn_stim","trace_file","rt","it","mt","mt_clip","PLresp","raw_error_tot","raw_error_mean","raw_error_SD","raw_procSS","raw_procSD","translation","scale","rotation","shape_error_tot","shape_error_mean","shape_error_SD","shape_procSS","shape_procSD","control_question","control_response","correct_response")]

# change data to numeric where appropriate
all_data$condition <- as.factor(all_data$condition)
all_data$figure_type <- as.factor(all_data$figure_type)
all_data$PLstim <- as.numeric(all_data$PLstim)
all_data$figlength <- as.numeric(all_data$figlength)
all_data$sinuosity <- as.numeric(all_data$sinuosity)
all_data$totcurv <- as.numeric(all_data$totcurv)
all_data$totabscurv <- as.numeric(all_data$totabscurv)
all_data$tortuosity <- as.numeric(all_data$tortuosity)
all_data$curvsum <- as.numeric(all_data$curvsum)
all_data$ApEn_stim <- as.numeric(all_data$ApEn_stim)
all_data$SaEn_stim <- as.numeric(all_data$SaEn_stim)
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

# arrange trials in chronological order
all_data <- dplyr::arrange(all_data, participant_id, session_num, block_num, trial_num)

# calculate average response velocity per trial
all_data <- dplyr::mutate(
        .data = all_data,
        vresp = PLresp / mt_clip 
) # and reorder one last time:
all_data <- all_data[c("participant_id","sex","age","handedness","condition","session_num","block_num","trial_num","figure_type","figure_file","stimulus_gt","stimulus_mt","avg_velocity","path_length","PLstim","figlength","sinuosity","totcurv","totabscurv","tortuosity","curvsum","ApEn_stim","SaEn_stim","trace_file","rt","it","mt","mt_clip","PLresp","vresp","raw_error_tot","raw_error_mean","raw_error_SD","raw_procSS","raw_procSD","translation","scale","rotation","shape_error_tot","shape_error_mean","shape_error_SD","shape_procSS","shape_procSD","control_question","control_response","correct_response")]


# save .txt file with all_data:
write.table(all_data,"~/Documents/RStudio/TraceLabDB/all_data.txt", sep="\t")
# this saves object to load in R quickly: load("all_data.Rda")
save(all_data, file = "all_data.Rda")

##### FIN #####