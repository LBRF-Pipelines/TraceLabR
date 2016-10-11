# complexity workspace 

graphics.off() # clear figures
#cat("\014") # clear console

fig2lowestval <- min(dplyr::filter(all_data, figure_type == "fig2", stimulus_mt > 1.4, stimulus_mt < 1.6, complexity4 != "NA")$complexity4)
fig2highestval <- max(dplyr::filter(all_data, figure_type == "fig2", stimulus_mt > 1.4, stimulus_mt < 1.6, complexity4 != "NA")$complexity4)
fig2lowest <- subset(all_data, complexity4 == fig2lowestval)
fig2highest <- subset(all_data, complexity4 == fig2highestval)

# lowest tortuosity score for 1500ms gt = p16_s4_b1_t10_2016-09-28.tlf 
# highest tortuosity score for 1500ms gt = p15_s1_b1_t18_2016-09-19.tlf 
# arbitrary score but same fig at different animation time (500 ms): p13_s2_b4_t20_2016-09-16.tlf

#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p16_2016-09-19 13:23:10/training/session_4/p16_s4_b1_t10_2016-09-28.zip"
file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p15_2016-09-19 12:33:09/testing/session_1/p15_s1_b1_t18_2016-09-19.zip"
#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p13_2016-09-14 10:15:00/training/session_2/p13_s2_b4_t20_2016-09-16.zip"


name.tlf <- gsub(".zip",".tlf",basename(file.name))
# read in data 
tlf <- read.table(unz(file.name, name.tlf),stringsAsFactors=FALSE, sep=",")
#create data frames
data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))

#data_stim2 <- data_stim
# YEAH, a repeated figure captures literally the same points!

#plot figure:
#plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0)) 

### using parametric equation method ###

# first normalize to 10000 time points: 
# (because I don't know how to just get derivatives of splines themselves...)
figlength <- nrow(data_stim)

s <- seq(from = 1, to = 100, length.out = nrow(data_stim)) # ends at 99.888889

time <- seq(min(s), max(s), length.out = 10000) # ends at 99.888889
#arclength <- seq(0, pathlength, length.out = 5000)
# NOTE: normalizing time points to a particular number (e.g. 100) should be equivalent
# to normalizing by arclength, because the animation moves at a constant velocity. RIGHT?

# maybe need to interpolate the data points and always have same x values? because 
# finally I can get same values for same figure using a same time series... BUT it's still probably
# not very fucking accurate. Anyway, the randoms will have even more variability in their time series. 


# make two functions x(t) and y(t):

# plot(data_stim$X3,data_stim$X1) #x(t)
# xt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X1, df = 30) 
# lines(xt.spl)
# dxdt <- predict(xt.spl, x = time, deriv = 1) # first derivative of x
# plot(dxdt)
# d2xdt2 <- predict(xt.spl, x = time, deriv = 2) # second derivative of x
# plot(d2xdt2)
# 
# plot(data_stim$X3,data_stim$X2) #y(t)
# yt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X2, df = 30) #(.5*nrow(data_stim))
# lines(yt.spl)
# dydt <- predict(yt.spl, x = time, deriv = 1) # first derivative of y
# plot(dydt)
# d2ydt2 <- predict(yt.spl, x = time, deriv = 2) # second derivative of y
# plot(d2ydt2)

# calculate curvature:

# curvature = (dxdt$y*d2ydt2$y - dydt$y*d2xdt2$y)/((dxdt$y^2 + dydt$y^2)^(3/2)) #signed curvature
# plot(time, curvature, ylim = c(-.01,.01))

# confirm that you made a function that recreates figure:
# xnew <- predict(xt.spl, x = time, deriv = 0)
# ynew <- predict(yt.spl, x = time, deriv = 0)
# #NOTE: wow it's bad!

# xnew <- splinefun(data_stim$X3, data_stim$X1)
# ynew <- splinefun(data_stim$X3, data_stim$X2)
# time <- seq(min(data_stim$X3), max(data_stim$X3), length.out = 10000)
# plot(xnew(time), -ynew(time))
# points(data_stim$X1,-data_stim$X2, col="cyan")
#NOTE: this is much better! change from smoothspline with predicts to splinefun(time, deriv=?) for above... see if thath helps...

xt.spl <- splinefun(x = s, y = data_stim$X1) 
yt.spl <- splinefun(x = s, y = data_stim$X2)
curvature = (
        (xt.spl(time, deriv=1) * yt.spl(time, deriv=2)) - (yt.spl(time, deriv=1) * xt.spl(time, deriv=2)))/
        ((xt.spl(time, deriv=1)^2 + yt.spl(time, deriv=1)^2)^(3/2)) #signed curvature
plot(time, curvature)
plot(time, curvature, ylim = c(-.01,.01))

#curvature <- curvature[!curvature %in% boxplot.stats(curvature, coef = 3)$out] #doesn't replace with NA's

remove_outliers <- function(curv_in, na.rm = TRUE, ...) {
        x <- curv_in
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}
curv_in <- curvature
curvature <- remove_outliers(curv_in)

plot(time, curvature)
plot(time, curvature, ylim = c(-.01,.01))

# calculate total curvature:

curvature.spl <- splinefun(time, curvature)
plot(curvature.spl, xlim=c(min(s),max(s)))

totcurv <- tryCatch(
        integrate(Vectorize(curvature.spl), lower = min(time), upper = max(time), subdivisions = 2000, abs.tol = 0)$value
        , error=function(err) NA
        )
complexity2 <- totcurv

# calculate total absolute curvature:

abscurv <- abs(curvature) #unsigned curvature
plot(time, abscurv)
abscurv.spl <- splinefun(time, abscurv)
plot(abscurv.spl)
totabscurv <- integrate(Vectorize(abscurv.spl), lower = min(time), upper = max(time)) #subdivisions=1000, rel.tol=.Machine$double.eps^.05
complexity3 <- totabscurv$value

# should also try the derivative of curvature (how much is curvature changing?)

dcurvedt <- curvature.spl(time, deriv=1)
plot(dcurvedt)
abs.dcurvdt <- abs(dcurvedt)
plot(abs.dcurvdt)
abs.dcurvedt.spl <- splinefun(time, abs.dcurvdt)
plot(abs.dcurvedt.spl)
tortuosity <- integrate(abs.dcurvedt.spl, lower = min(time), upper = max(time), subdivisions=1000)
# IT WON'T CALCULATE... 

# more ideas:

sum(abscurv, na.rm = TRUE) # also consider using MEAN! that's what Krakauer seems to have done... or SD? 
mean(abscurv, na.rm = TRUE)
sd(abscurv, na.rm = TRUE) 



#### VALIDATION TESTING ####

# note: using this method, the magnitude of peaks and valleys in curvature 
# are a function of how smooth you make the splines, such that better fitting
# leads to bigger spikes in curvature at the vertices of the figure.
# suggestion: test the final answer (integral) using a few different df's.

# INDEED: once you get beyond a couple thousand points, the curvature doesn't change much, 
# and once you get to < 20 df, the curvature score doesn't change much.


# create a scaled figure to test whether the method below is independent of pathlength:

# data_stim2 <- matrix(c(data_stim$X1*1.2, data_stim$X2*1.2, data_stim$X3*1.2), ncol=3)
# 
# time <- seq(min(data_stim2$X3), max(data_stim2$X3), length.out = 5000)
# xt.spl <- smooth.spline(x = data_stim2$X3, y = data_stim2$X1, df = (.5*nrow(data_stim2)))
# dxdt <- predict(xt.spl, x = time, deriv = 1) # first derivative of x
# d2xdt2 <- predict(xt.spl, x = time, deriv = 2) # second derivative of x
# yt.spl <- smooth.spline(x = data_stim2$X3, y = data_stim2$X2, df = (.5*nrow(data_stim2)))
# dydt <- predict(yt.spl, x = time, deriv = 1) # first derivative of y
# d2ydt2 <- predict(yt.spl, x = time, deriv = 2) # second derivative of y
# curvature = abs((dxdt$y*d2ydt2$y - dydt$y*d2xdt2$y))/((dxdt$y^2 + dydt$y^2)^(3/2))
# curvature.spl <- splinefun(time, curvature)
# totcurv <- integrate(curvature.spl, lower = min(time), upper = max(time))
# complexity3 <- totcurv$value

# yes, scaling the whole figure and time keeps complexity same 
# yes, same with changing speed only, or size only

###### APPROXIMATE ENTROPY ######

plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0))

library(pracma)

approx_entropy(data_stim$X1)
plot(data_stim$X1)
approx_entropy(data_stim$X2)
plot(data_stim$X2)

s2 <- seq(min(s), max(s), length.out = 1000)
curv <- curvature.spl(s2, deriv=0)
plot(curv)
approx_entropy(curv)

datastim <- matrix(c(as.vector(data_stim$X1),as.vector(data_stim$X2)),ncol=2)
datastim2 <- matrix(c(as.vector(data_stim$X1),as.vector(data_stim$X2)),ncol=1)
# neato but not necessary yet: convert cartesian to polar:
# datastim.pol <- cart2pol(datastim)

approx_entropy(datastim)
approx_entropy(datastim2)
approx_entropy(c(data_stim$X1,data_stim$X2)) 
# this means it just sticks them together into a long sequence... gotta convert to planar coordinates... 

s2 <- seq(min(s), max(s), length.out = 100)
data_stim_x <- xt.spl(s2)
data_stim_y <- yt.spl(s2)
plot(data_stim_x,data_stim_y)
approx_entropy(data_stim_x)
sample_entropy(data_stim_x)

approx_entropy(data_stim_y)
sample_entropy(data_stim_y)

# works great! but treats x and y as one long vector
# so, next need to do with turning angle!

#turning angle

V1 <- c(1,0)
V2 <- c(0,1)

angle <- function(x,y){
        dot.prod <- x%*%y 
        norm.x <- norm(x,type="2")
        norm.y <- norm(y,type="2")
        theta <- acos(dot.prod / (norm.x * norm.y))
        as.numeric(theta)
}
V1 <- c(1,0)
V2 <- c(0,1)
angle(V1,V2)
angle(V2,V1)

angle2 <- function(M,N){
        atan2(N[2],N[1]) - atan2(M[2],M[1]) 
}
V1 <- c(1,0)
V2 <- c(0,1)
angle2(V1,V2)
angle2(V2,V1)

V1 <- c(1,1)
V2 <- c(2,2)
V3 = V2-V1
V3

V1 <- c(2,2)
V2 <- c(3,2)
V3 = V2-V1
V3


angle2 <- function(V1,V2){
        atan2(V2[2],V2[1]) - atan2(V1[2],V1[1]) 
}

# turning angle sequence:
stim_theta <- rep(0, length(data_stim[,1])-2) # note you always lose two points
for (a in 1:length(stim_theta)){
        V1 = c(data_stim[a+1,1],data_stim[a+1,2]) - c(data_stim[a,1],data_stim[a,2])
        V2 = c(data_stim[a+2,1],data_stim[a+2,2]) - c(data_stim[a+1,1],data_stim[a+1,2])
        stim_theta[a] = atan2(V2[2],V2[1]) - atan2(V1[2],V1[1])
        if (abs(stim_theta[a]) > pi){
                stim_theta[a] = stim_theta[a] - ((2*pi)*sign(stim_theta[a]))
                }
}
plot(stim_theta)

stim_theta_deg <- stim_theta * (180/pi)
plot(stim_theta_deg)

approx_entropy(stim_theta)
sample_entropy(stim_theta)

approx_entropy(stim_theta_deg)
sample_entropy(stim_theta_deg)

plot(data_stim$X1[1:5],data_stim$X2[1:5])

