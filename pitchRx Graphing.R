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

