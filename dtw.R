#### DTW workspace ####

# use stim and resp

library(dtw)

x <- matrix(rnorm(8), ncol = 2)
## test inheritance of names
rownames(x) <- LETTERS[1:4] # points
colnames(x) <- letters[1:2] # x's and y's
proxy::dist(x)
proxy::dist(x, x)
# test: 
sqrt((x[1,1]-x[2,1])^2 + (x[1,2]-x[2,2])^2)

d <- proxy::dist(stim, resp)

dtw <- dtw(x = resp, y = stim,
    dist.method = "Euclidean",
    step.pattern = symmetric1,
    window.type = "none",
    keep.internals = TRUE,
    distance.only = FALSE,
    open.end = FALSE,
    open.begin = FALSE
)

# FIGURE OUT HOW TO GET COORDINATES TO DO RAW and PROCRUSTES ERROR 

resp_dtw <- matrix(rep(0, 2*length(dtw$index1)), ncol=2)
for (j in 1:nrow(resp_dtw)){
        resp_dtw[j,1] <- resp[dtw$index1[j], 1]
        resp_dtw[j,2] <- resp[dtw$index1[j], 2]
}
plot(resp_dtw[,1],-resp_dtw[,2])

stim_dtw <- matrix(rep(0, 2*length(dtw$index2)), ncol=2)
for (j in 1:nrow(stim_dtw)){
        stim_dtw[j,1] <- stim[dtw$index2[j], 1]
        stim_dtw[j,2] <- stim[dtw$index2[j], 2]
}
plot(stim_dtw[,1],-stim_dtw[,2])

# raw error
raw_dist = rep(0, length(resp[,1]))
for (j in 1:length(raw_dist)){
        raw_dist[j] = as.numeric(sqrt(((resp[j,1]-stim[j,1])^2)+((resp[j,2]-stim[j,2])^2)))
}
# error throughout trial:
sum(raw_dist)
mean(raw_dist)
sd(raw_dist)
# pre-procrustes transform sum of squares and SD:
raw_procSS <- sum(raw_dist^2)
raw_procSD <- sqrt(raw_procSS/(length(raw_dist)-1))

# raw error DTW
raw_dist_dtw = rep(0, length(resp_dtw[,1]))
for (j in 1:length(raw_dist_dtw)){
        raw_dist_dtw[j] = as.numeric(sqrt(((resp_dtw[j,1]-stim_dtw[j,1])^2)+((resp_dtw[j,2]-stim_dtw[j,2])^2)))
}
sum(raw_dist_dtw)
mean(raw_dist_dtw)
sd(raw_dist_dtw)
# pre-procrustes transform sum of squares and SD:
rawdtw_procSS <- sum(raw_dist_dtw^2)
rawdtw_procSD <- sqrt(rawdtw_procSS/(length(raw_dist_dtw)-1))

# DTW procrustes transformation
trans_dtw <- rotonto(stim_dtw, resp_dtw, scale = TRUE, signref = FALSE, reflection = FALSE, weights = NULL, centerweight = FALSE)

#get translation
translation_dtw <- sqrt((trans_dtw$transy[1] - trans_dtw$trans[1])^2 + (trans_dtw$transy[2] - trans_dtw$trans[2])^2)

#get scale factor
scale_dtw <- trans_dtw$bet

#get rotation angle *radians*
rotation_dtw <- acos(trans_dtw$gamm[1,1])


## SHAPE ERROR DTW ##

# create vector of point by point distances (error) between stimulus and response:
shape_dist_dtw = rep(0, length(trans_dtw$Y[,1]))
for (j in 1:length(shape_dist_dtw)){
        shape_dist_dtw[j] = as.numeric(sqrt(((trans_dtw$Y[j,1]-trans_dtw$X[j,1])^2)+((trans_dtw$Y[j,2]-trans_dtw$X[j,2])^2)))
}
# error throughout trial:
sum(shape_dist_dtw)
mean(shape_dist_dtw)
sd(shape_dist_dtw)

# "ordinary procrustes sum of squares" and SD:
shape_procSS <- sum(shape_dist^2)
shape_procSD <- sqrt(shape_procSS/(length(shape_dist)-1))


















dtwPlotDensity(dtw)

# x axis (but both axes used for the alignment)
dtwPlotThreeWay(dtw
                , xts = resp$X1
                , yts = stim$X1
                , type.align = "l"
                , type.ts = "l"
                , match.indices = NULL
                , margin = 4
                , inner.margin = 0.2
                , title.margin = 1.5
                , xlab = "Query index"
                , ylab = "Reference index"
                , main = "Timeseries alignment"
)
# y axis
dtwPlotThreeWay(dtw
                , xts = resp$X2
                , yts = stim$X2
                , type.align = "l"
                , type.ts = "l"
                , match.indices = NULL
                , margin = 4
                , inner.margin = 0.2
                , title.margin = 1.5
                , xlab = "Query index"
                , ylab = "Reference index"
                , main = "Timeseries alignment"
)




