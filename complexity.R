# complexity workspace 

graphics.off() # clear figures
#cat("\014") # clear console

### create parametric equation for figure ###

# make two functions x(t) and y(t):

plot(data_stim$X3,data_stim$X1) #x(t)
xt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X1, df = (.5*nrow(data_stim)))
lines(xt.spl)

plot(data_stim$X3,data_stim$X2) #y(t)
yt.spl <- smooth.spline(x = data_stim$X3, y = data_stim$X2, df = (.5*nrow(data_stim)))
lines(yt.spl)

plot(data_stim$X1,data_stim$X2, xlim=c(0,1920), ylim=c(1080,0))
lines(path.spl)

predictx <- predict(path.spl, x = xy[,1])
predicty <- predict(path.spl, x = xy[,2])

plot(predictx$x, predictx$y)
plot(predicty$x, predicty$y)
plot(predictx$x, predicty$y)

plot(predictx$x,xy[,2])
plot(predictx$x,xy[,1])

plot(predicty$x,xy[,2])


points(predicty$x,xy[,2], col="red")


