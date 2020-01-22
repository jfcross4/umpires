library(mgcv)
library(parallel)
library(dplyr)
library(ggplot2)


if (detectCores()>1 & !is.na(detectCores())) {
  cl <- makeCluster(detectCores()-1)
} else cl <- NULL



fit <- bam(strike~s(plate_x, plate_z), data=umpire_full_data %>% filter(umpire_id=="427115"), family=binomial, cluster=cl)

myx.gam <- matrix(data=seq(from=-1.75, to=1.75, by=0.025), nrow=141, ncol=141)
myz.gam <- t(matrix(data=seq(from=0.75,to=4.25, by=0.025), nrow=141, ncol=141))
fitdata.gam <- data.frame(plate_x=as.vector(myx.gam), plate_z=as.vector(myz.gam))

temp <- fitdata.gam
pred.grid <- data.frame(plate_x=numeric(), plate_z=numeric(), strikes=numeric(), balls=numeric(), stand=factor())
for (strikes in 0:2){
  for (balls in 0:3){
    for (stand in c("R", "L")){
      temp$strikes <- strikes
      temp$balls <- balls
      temp$stand <- stand
      
      pred.grid <- rbind(pred.grid, temp)
    }
  }
}

fitdata <- pred.grid
fitdata$preds <- c(predict(fit, pred.grid, type="response"))
s <- 120/3
strikezone_sizes <- fitdata %>% group_by(stand, balls, strikes) %>% summarize(area=(144/s^2)*sum(preds))
ggplot(strikezone_sizes, aes(x=as.factor(strikes),y=as.factor(balls), fill=area))+geom_tile()+ 
  scale_fill_gradient(low="green", high="red")+facet_grid(~stand)+geom_text(aes(label=round(area,0)))+xlab("Strikes")+ylab("Balls")+
  ggtitle("Strike Zone Sizes by Count")

fitdata.R <- fitdata %>% filter(stand=="R")
fitdata.L <- fitdata %>% filter(stand=="L")

v <- ggplot(fitdata.R, aes(plate_x, plate_z, z = preds))

v2 <- v+ stat_contour(breaks=c(0.5), col="blue", size=1)+
  geom_rect(aes(xmin=-10/12, xmax=10/12, ymin=2.5-10/12, ymax=2.5+10/12), fill="transparent", color="black")+
  stat_contour(breaks=c(0.25, 0.75), col="red", linetype=2, size=0.5)+
  labs(colour = "size")+scale_x_continuous(limits=c(-1.5, 1.5))+scale_y_continuous(limits=c(1, 4))

v2+facet_grid(balls~strikes)+ggtitle("Strike Zones by Count, RHB (2015-2018)")
