
# set working dir
setwd("~/git_repos/duolingo-percentiles//")

# read in data
dat <- read.table("data",
                  sep=",",
                  header=T
                  )

# explore data format
head(dat)
str(dat)

# New data ----------------------------------------------------------------

# package lubridata for time/date strings
library(lubridate)
dat <- read.table("extracted260731.csv",
                  sep=",",
                  header=TRUE)
head(dat)
# convert date/time string to lubridate format
ymd_hms(dat$metadata_creation_datetime)
# extract date string
date(ymd_hms(dat$metadata_creation_datetime))
# order by date of creation
order <- order(date(ymd_hms(dat$metadata_creation_datetime)))
dat <- dat[order, ]
head(dat)

# add data and time (in hours) cols to dataset
dat$date <- date(ymd_hms(dat$metadata_creation_datetime))
dat$hTime <- hour(ymd_hms(dat$metadata_creation_datetime)) + 
  minute(ymd_hms(dat$metadata_creation_datetime))/60 +
  second(ymd_hms(dat$metadata_creation_datetime))/60/60 
date(ymd_hms(dat$metadata_creation_datetime)) > 2025
plot(date(ymd_hms(dat$metadata_creation_datetime)))
# convert percentile string to numbers
dat$perc <- sapply(dat$image_percentage, function(x) as.numeric(substr(x, 1, nchar(x)-1)),
       USE.NAMES = FALSE)
# flip to new-style percentiles if required
dat$perc[dat$date < "2025-01-01"] <- 100 - dat$perc[dat$date < "2025-01-01"]
plot(dat$perc)
# add 'epoch' column to dataset denoting whether the percentile was recorded before or after the change in percentile calculation
dat$epoch <- factor(ifelse(dat$date > "2025-01-01", "old", "new"))

# 3 percentiles were recorded while in Germany
germ <- rep("no", nrow(dat))
germ[date(dat$metadata_creation_datetime) > "2022-12-20" &
date(dat$metadata_creation_datetime) < "2022-12-29"] <- "yes"
dat$germ <- factor(germ)
dat$wday <- wday(date(dat$metadata_creation_datetime), label = TRUE)

#png("perc.png", width=7, height=5, units="in", res=150)
plot(perc ~ hTime,
     data=dat,
     col=ifelse(dat$date > "2025-01-01", "#FF000040", "#0000FF40"),
     pch=ifelse(dat$germ=="no",19, 17),
     cex=1.5,
     xlab="Time (h)",
     ylab = "Finished earlier than (%)",
     main="Duolingo finishing time distribution")

abline(h=seq(0,100,by=5), col="lightgrey", lty=3)

abline(v=seq(0,24,by=1), col="lightgrey", lty=3)

legend("topleft", 
       legend=c("pre-2025", "post-2025", "UK", "Germany"), 
       col=c("#0000FF40", "#FF000040", 1, 1), 
       pch=c(15, 15, 19, 17), 
       pt.cex=1.5)
#dev.off()
# GAM ---------------------------------------------------------------------

# generalized additive model to smooth the data and estimate rates of change
library(mgcv)


gam1 <- gam(perc ~ s(hTime) + epoch,
            data=dat, method="ML")
# fitting separate slplines for each epoch
gam2 <- gam(perc ~ s(hTime) + epoch + s(hTime, by=epoch),
            data=dat, method="ML")
summary(gam1)
summary(gam2)
gam.check(gam1)
gam.check(gam2)
plot(gam1)
plot(gam2)

# more complex model does not give better fit
anova(gam1, gam2, test="Chisq")
AIC(gam1, gam2)


# predice values to generate rate plot
tt <- seq(0,12,by=0.1) # times
predVals <- predict(gam1, newdata=data.frame(hTime=tt, epoch="old"))
#predVals2 <- predict(gam1, newdata=data.frame(hTime=dat$hTime, epoch="new"))
rates <- diff(predVals)*10 # take time 10 as there are 10 time points per hour
sum(rates/10)

#png("rate.png", width=7, height=5, units="in", res=150)
plot(tt[-1], rates,
     xlab = "Time (h)",
     ylab="Rate (percent/h)",
     type="l",
     lwd=2,
     main="Duolingo finishing rate over time")

abline(h=seq(0,5,by=1), col="lightgrey", lty=3)

#abline(v=seq(0,24,by=1), col="lightgrey", lty=3)
abline(v=dat$hTime, col="darkgrey", lty=3)
#dev.off()

# function to get rates at any time point
afun <- approxfun(x=tt[-1], y=rates, method="linear", rule=2)
lm1 <- lm(residuals(gam1)~afun(dat$hTime))
# additional sanity check: no association between gam residuals and hourly rates, so the model is a good fit to the data
plot(afun(dat$hTime), residuals(gam1))
grid()
abline(lm1)
summary(lm1)
