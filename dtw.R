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
    step.pattern = symmetric2,
    window.type = "none",
    keep.internals = TRUE,
    distance.only = FALSE,
    open.end = FALSE,
    open.begin = FALSE
)

# FIGURE OUT HOW TO GET COORDINATES TO DO RAW and PROCRUSTES ERROR 



subset(dtw$localCostMatrix, ) 

costx <- dtw$localCostMatrix[dtw$index1,]
costy <- costx[dtw$index2,]

costmatrix <- dtw$costMatrix
dirmatrix <- dtw$directionMatrix
steppattern <- dtw$stepPattern


dtw_tot <- dtw$distance
dtw_mean <- dtw$distance/nrow(stim)
dtw_SD <- 

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




