
sspl <-smooth.spline(x=dat[!is.na(dat$perc),]$timeH,
              y=dat[!is.na(dat$perc),]$perc, spar=.6)
xvals <- seq(0, 12, 0.1)
plot(predict(sspl, xvals), type="l")
points(dat$timeH,
       dat$perc)
#abline(v=c(8,9))
#png("rate.png", width=7, height=5, units="in", res=150)
plot(x=xvals[-1], 
     y=diff(predict(sspl, xvals)$y)/
       diff(predict(sspl, xvals)$x),
     xlab="h",
     ylab="Slope (percent/h)",
     main="Rate by time"
     )
grid()
abline(v=dat$timeH[!is.na(dat$perc)], 
       col="#FF000040",
       lty=2)
#dev.off()
