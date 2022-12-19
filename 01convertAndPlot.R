setwd("~/git_repos/duolingo-percentiles//")

dat <- read.table("data221219",
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
plot(perc ~ timeH,
     data=dat)
grid()
lm01 <- lm(perc ~ timeH,
           data=dat)
abline(lm01)
# fit looks linear with these limited data, but
summary(lm01)
# intercept is at 125%

plot(perc ~ timeH,
     data=dat,
     xlim=c(0, 24), ylim=c(0, 100))
grid()
abline(lm01)
plot(lm01)
