# complexity workspace 

xy <- matrix(c(data_stim$X1,data_stim$X2),ncol=2)
path.spl <- smooth.spline(xy, y = NULL, df = 20)
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