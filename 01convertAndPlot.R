setwd("~/git_repos/duolingo-percentiles//")

#dat <- read.table("data221219",
dat <- read.table("data221229",
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
     ylab="Percentile")
abline(v=dat$timeH, 
       col="#FF000040",
       lty=2)
grid()
lm01 <- lm(perc ~ timeH,
           data=dat)
abline(lm01)
#dev.off()
# fit looks linear with these limited data, but
summary(lm01)
# intercept is at 125%

plot(perc ~ timeH,
     data=dat,
     xlim=c(0, 24), ylim=c(0, 100))
grid()
abline(v=dat$timeH, 
       col="#FF000040",
       lty=2)

abline(lm01)
plot(lm01)
