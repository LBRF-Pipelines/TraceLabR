# complexity workspace 

graphics.off() # clear figures
#cat("\014") # clear console

fig2lowestval <- min(dplyr::filter(all_data, figure_type == "fig2", stimulus_mt > 1.4, stimulus_mt < 1.6, complexity4 != "NA")$complexity4)
fig2highestval <- max(dplyr::filter(all_data, figure_type == "fig2", stimulus_mt > 1.4, stimulus_mt < 1.6, complexity4 != "NA")$complexity4)
fig2lowest <- subset(all_data, complexity4 == fig2lowestval)
fig2highest <- subset(all_data, complexity4 == fig2highestval)

# lowest = p16_s4_b1_t10_2016-09-28.tlf 
# highest = p15_s1_b1_t18_2016-09-19.tlf 

#file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p16_2016-09-19 13:23:10/training/session_4/p16_s4_b1_t10_2016-09-28.zip"
file.name <- "/Users/tonyingram/TraceLab/ExpAssets/Data/p15_2016-09-19 12:33:09/testing/session_1/p15_s1_b1_t18_2016-09-19.zip"

name.tlf <- gsub(".zip",".tlf",basename(file.name))
# read in data 
tlf <- read.table(unz(file.name, name.tlf),stringsAsFactors=FALSE, sep=",")
#create data frames
data_stim <- data.frame(matrix(as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", as.character(tlf)), ", "))),ncol=3,nrow=length(tlf)/3, byrow=TRUE))


#plot figure:
#plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0)) 

### using parametric equation method ###

# first normalize to 10000 time points: 
# (because I don't know how to just get derivatives of splines themselves...)
time <- seq(min(data_stim$X3), max(data_stim$X3), length.out = 10000) #but this takes a long time
#arclength <- seq(0, pathlength, length.out = 5000)
# NOTE: normalizing time points to a particular number (e.g. 100) should be equivalent
# to normalizing by arclength, because the animation moves at a constant velocity. RIGHT?

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
# plot(xnew(time), -ynew(time))
# points(data_stim$X1,-data_stim$X2, col="cyan")
# #NOTE: this is much better! change from smoothspline with predicts to splinefun(time, deriv=?) for above... see if thath helps...

xt.spl <- splinefun(x = data_stim$X3, y = data_stim$X1) 
yt.spl <- splinefun(x = data_stim$X3, y = data_stim$X2)
curvature = (
        (xt.spl(time, deriv=1) * yt.spl(time, deriv=2)) - (yt.spl(time, deriv=1) * xt.spl(time, deriv=2)))/
        ((xt.spl(time, deriv=1)^2 + yt.spl(time, deriv=1)^2)^(3/2)) #signed curvature
plot(time, curvature)
plot(time, curvature, ylim = c(-.01,.01))

#curvature <- curvature[!curvature %in% boxplot.stats(curvature, coef = 3)$out] #doesn't replace with NA's

remove_outliers <- function(curv_in, na.rm = TRUE, ...) {
        x <- curv_in
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 3 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}
curv_in <- curvature
curvature <- remove_outliers(curv_in)

plot(time, curvature, na.rm=TRUE)
plot(time, curvature, na.rm=TRUE, ylim = c(-.01,.01))

# calculate total curvature:

curvature.spl <- splinefun(time, curvature)
plot(curvature.spl)

totcurv <- integrate(Vectorize(curvature.spl), lower = min(time), upper = max(time), abs.tol = 0, subdivisions=1000) # , stop.on.error = FALSE
complexity2 <- totcurv$value

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

sum(abscurv) # also consider using MEAN! that's what Krakauer seems to have done... or SD? 
mean(abscurv)
sd(abscurv) 



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
