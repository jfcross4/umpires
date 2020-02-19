library(readr)
gd_savant_new_slim <- read_csv("gd_savant_new_slim.csv")
gd_savant_new_slim <- left_join(gd_savant_new_slim, player_heights, by=c("batter"="mlbamid"))


player_heights <- read_csv("player_heights.csv")
avg_sz_top <- mean(gd_savant_new_slim$sz_top, na.rm=TRUE)
avg_player_height <- mean(gd_savant_new_slim$Height, na.rm=TRUE)
avg_sz_bot <- mean(gd_savant_new_slim$sz_bot, na.rm=TRUE)
sz_middle <- (avg_sz_top+avg_sz_bot)/2

percent_height_top <- avg_sz_top*12/avg_player_height

library(dplyr)
gd_savant_new_slim_top_middle <- gd_savant_new_slim %>% filter(plate_z > sz_middle,
                                                               abs(plate_x) < 0.5)
gd_savant_new_slim_top_middle <- left_join(gd_savant_new_slim_top_middle, player_heights, by=c("batter"="mlbamid"))

gd_savant_new_slim_top_middle <- gd_savant_new_slim_top_middle %>% mutate(z_above_sztop = plate_z - sz_top,
                                         z_above_avg_sztop = plate_z - avg_sz_top,
                                         z_above_height_top = plate_z - percent_height_top*Height/12
                                         )
gd_savant_new_slim_top_middle_noswing <- gd_savant_new_slim_top_middle %>% 
  filter(description=="called_strike" | description=="ball") %>% mutate(
    strike = as.numeric(description =="called_strike")
  ) %>% filter(!is.na(Height))

library()

summary(gd_savant_new_slim_top_middle_noswing)


m <- lm(strike ~ z_above_sztop+z_above_avg_sztop+z_above_height_top, data=gd_savant_new_slim_top_middle_noswing)
m_logistic <- glm(strike ~ z_above_sztop+z_above_avg_sztop+z_above_height_top, data=gd_savant_new_slim_top_middle_noswing,
                  family="binomial")
gd_savant_new_slim_top_middle_noswing$pred_strike_linear <- predict(m, gd_savant_new_slim_top_middle_noswing)
gd_savant_new_slim_top_middle_noswing$pred_strike_logistic <- predict(m_logistic, gd_savant_new_slim_top_middle_noswing,
                                                                    type="response")
intercept_coef <- coef(m_logistic)["(Intercept)"]
sztop_coef <- -1*coef(m_logistic)["z_above_sztop"]
avg_sztop_coef <- -1*coef(m_logistic)["z_above_avg_sztop"]
above_height_top_coef <- -1*coef(m_logistic)["z_above_height_top"]


umpire_full_data <- mutate(umpire_full_data, based_on_sztop = plate_z-sz_top,  based_on_avgsz_top = plate_z - mean(sz_top))

pred_top <- (intercept_coef + sztop_coef*sz_top )
