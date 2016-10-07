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
xt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X1, df = (.5*nrow(data_stim)))
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

curvature = (dxdt$y*d2ydt2$y - dydt$y*d2xdt2$y)/((dxdt$y^2 + dydt$y^2)^(3/2))
plot(time, curvature, ylim = c(-.01,.01))

# calculate total curvature:

curvature.spl <- splinefun(time, curvature)

totcurv <- integrate(curvature.spl, lower = min(time), upper = max(time))

complexity2 <- totcurv$value

# note: using this method, the magnitude of peaks and valleys in curvature 
# are a function of how smooth you make the splines, such that better fitting
# leads to bigger spikes in curvature at the vertices of the figure.
# suggestion: test the final answer (integral) using a few different df's.

# INDEED: once you get beyond a couple thousand points, the curvature doesn't change much, 
# and once you get to < 20 df, the curvature score doesn't change much.

