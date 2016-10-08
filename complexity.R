# complexity workspace 

graphics.off() # clear figures
#cat("\014") # clear console

#plot figure:
#plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0)) 

### using parametric equation method ###

# first normalize to 5000 time points: 
# (because I don't know how to just get derivatives of splines themselves...)
time <- seq(min(data_stim$X3), max(data_stim$X3), length.out = 5000) #but this takes a long time
#arclength <- seq(0, pathlength, length.out = 5000)
# NOTE: normalizing time points to a particular number (e.g. 100) should be equivalent
# to normalizing by arclength, because the animation moves at a constant velocity. RIGHT?

# make two functions x(t) and y(t):

#plot(data_stim$X3,data_stim$X1) #x(t)
xt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X1, df = (.5*nrow(data_stim))) # here's the problem: the df is based on sampling rate! and there are fives speeds! also splines will be fit with a lot of variability because of the number of points varying, even in same figure, but especially random where sometimes there's only 10 points... this means the fit is gonna be really poor, which probably means the derivatives and integrals are bad! 
#lines(xt.spl)
dxdt <- predict(xt.spl, x = time, deriv = 1) # first derivative of x
#plot(dxdt)
d2xdt2 <- predict(xt.spl, x = time, deriv = 2) # second derivative of x
#plot(d2xdt2)

#plot(data_stim$X3,data_stim$X2) #y(t)
yt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X2, df = (.5*nrow(data_stim)))
#lines(yt.spl)
dydt <- predict(yt.spl, x = time, deriv = 1) # first derivative of y
#plot(dydt)
d2ydt2 <- predict(yt.spl, x = time, deriv = 2) # second derivative of y
#plot(d2ydt2)

# calculate curvature:

curvature = (dxdt$y*d2ydt2$y - dydt$y*d2xdt2$y)/((dxdt$y^2 + dydt$y^2)^(3/2)) #signed curvature
plot(time, curvature, ylim = c(-.01,.01))

# calculate total curvature:

curvature.spl <- splinefun(time, curvature)
plot(curvature.spl)

totcurv <- integrate(curvature.spl, lower = min(time), upper = max(time), subdivisions=1000, rel.tol=.Machine$double.eps^.05)
complexity2 <- totcurv$value

# calculate total absolute curvature:

abscurv <- abs(curvature) #unsigned curvature
plot(time, abscurv)
abscurv.spl <- splinefun(time, abscurv)
plot(abscurv.spl)
totabscurv <- integrate(abscurv.spl, lower = min(time), upper = max(time), subdivisions=1000, rel.tol=.Machine$double.eps^.05)
complexity3 <- totabscurv$value

# should also try the derivative of curvature (how much is curvature changing?)

dcurvedt <- curvature.spl(time, deriv=1)
plot(time, dcurvedt)
abs.dcurvedt <- abs(dcurvedt)
plot(time, abs.dcurvedt)

dcurvedt.spl <- splinefun(time, abs.dcurvedt)
plot(dcurvedt.spl)
tortuosity <- integrate(dcurvedt.spl, lower = min(time), upper = max(time), subdivisions=1000, rel.tol=.Machine$double.eps^.05)
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
