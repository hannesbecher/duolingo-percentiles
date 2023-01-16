setwd("~/git_repos/duolingo-percentiles//")

dat <- read.table("data",
                  sep=",",
                  header=T
                  )
head(dat)
str(dat)

# convert hh:mm to decimal hours
time2num <- function(ti){
  a <- as.numeric(strsplit(ti, ":")[[1]])
  print(a)
  a[1] + a[2]/60
}
dat$timeH <- sapply(dat$hhmm, time2num)
dat
#png("perc.png", width=7, height=5, units="in", res=150)
plot(perc ~ timeH,
     data=dat,
     xlab="Time (h)",
     ylab="Percentile",
     main="Percentile by time")
abline(v=dat$timeH, 
       col="#FF000040",
       lty=2)
abline(h=seq(0,100,by=5), col="lightgrey", lty=3)

abline(v=seq(0,24,by=1), col="lightgrey", lty=3)
lm01 <- lm(perc ~ timeH,
           data=dat)
#abline(lm01)
#dev.off()
# fit looks linear with these limited data, but
summary(lm01)
# intercept is at 125%

plot(lm01)


