library(dplyr)
library(ggplot2)
library(XML)
library(XML2R)
library(pitchRx)
library(mgcv)
library(baseballr)
library(readr)
library(parallel)

if (detectCores()>2 & !is.na(detectCores())) {
  cl <- makeCluster(detectCores()-2)
} else cl <- NULL

umpire_data_url <- "https://t.co/R17oTRDnUN"
umpires <- read.csv(umpire_data_url)
umpires <- umpires %>% filter(position=="HP")
gd_savant_new_slim <- read_csv("gd_savant_new_slim.csv")
umpire_full_data <- left_join(gd_savant_new_slim, umpires %>% dplyr::select(game_pk, umpire_id=id, umpire_name=name), "game_pk")


noswing <- umpire_full_data %>% filter(description=="called_strike" | description=="ball")
noswing$strike <- as.numeric(noswing$description %in% "called_strike")
noswing <- noswing %>% rename(px = plate_x, pz = plate_z)

noswing_2016_06_03 <- noswing %>% filter(game_date == "2016-06-03")
noswing_2016_06_03$umpire_name <- droplevels(noswing_2016_06_03$umpire_name)

# m <- bam(strike ~ s(px, pz, by = factor(umpire_name)) +
#            factor(umpire_name), data = noswing_2016_06_03, 
#          family = binomial(link = 'logit'), cluster=cl)

m <- bam(strike ~ s(px, pz, by = umpire_name,k=10) , data = noswing_2016_06_03, 
         family = binomial(link = 'logit'), cluster=cl)


x <- list(
  facet_grid(. ~  umpire_name),
  theme_bw(),
  coord_equal(),
  viridis::scale_fill_viridis(name = "Probability of Called Strike Based on Umpire")
)
strikeFX(noswing_2016_06_03, model = m, layer = x, draw_zones = TRUE)

#######
myx.gam <- matrix(data=seq(from=-1.75, to=1.75, by=0.025), nrow=141, ncol=141)
myz.gam <- t(matrix(data=seq(from=0.75,to=4.25, by=0.025), nrow=141, ncol=141))
fitdata.gam <- data.frame(px=as.vector(myx.gam), pz=as.vector(myz.gam))

temp <- fitdata.gam
pred.grid <- data.frame(px=numeric(), pz=numeric(), strikes=numeric(), balls=numeric(), stand=factor())

for (umpire in levels(noswing_2016_06_03$umpire_name)){
      temp$umpire_name <- umpire
      
      pred.grid <- rbind(pred.grid, temp)
}
fitdata <- pred.grid
fitdata$preds <- c(predict(m, pred.grid, type="response"))

v <- ggplot(fitdata, aes(px, pz, z = preds))+facet_wrap(.~umpire_name)
v2 <- v+ stat_contour(breaks=c(0.5), col="blue", size=1)+
  geom_rect(aes(xmin=-10/12, xmax=10/12, ymin=2.5-10/12, ymax=2.5+10/12), fill="transparent", color="black")+
  stat_contour(breaks=c(0.25, 0.75), col="red", linetype=2, size=0.5)+
  labs(colour = "size")+scale_x_continuous(limits=c(-1.5, 1.5))+scale_y_continuous(limits=c(1, 4))+
  ggtitle("Strike Zones by Umpire on 6/3/2016")


