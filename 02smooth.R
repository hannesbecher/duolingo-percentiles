
sspl <-smooth.spline(x=dat[!is.na(dat$perc),]$timeH,
              y=dat[!is.na(dat$perc),]$perc, spar=.4)
xvals <- seq(0, 12, 0.1)
plot(predict(sspl, xvals), type="l")
points(dat$timeH,
       dat$perc)
#abline(v=c(8,9))

plot(x=xvals[-1], 
     y=diff(predict(sspl, xvals)$y)/
       diff(predict(sspl, xvals)$x),
     xlab="h",
     ylab="Slope (percent/h)"
     )


