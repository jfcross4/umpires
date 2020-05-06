library(readr); library(dplyr)
gd_savant_new_slim <- read_csv("gd_savant_new_slim.csv")
player_heights <- read_csv("player_heights.csv")
gd_savant_new_slim <- left_join(gd_savant_new_slim, player_heights, by=c("batter"="mlbamid"))

avg_sz_top <- mean(gd_savant_new_slim$sz_top, na.rm=TRUE)
avg_player_height <- mean(gd_savant_new_slim$Height, na.rm=TRUE)
avg_sz_bot <- mean(gd_savant_new_slim$sz_bot, na.rm=TRUE)
sz_middle <- (avg_sz_top+avg_sz_bot)/2

percent_height_top <- avg_sz_top*12/avg_player_height

gd_savant_new_slim <- gd_savant_new_slim %>% mutate(z_above_sztop = plate_z - sz_top,
                                                                          z_above_avg_sztop = plate_z - avg_sz_top,
                                                                          z_above_height_top = plate_z - percent_height_top*Height/12
)

gd_savant_new_slim_noswing <- gd_savant_new_slim %>% 
  filter(description=="called_strike" | description=="ball") %>% mutate(
    strike = as.numeric(description =="called_strike")
  ) %>% filter(!is.na(Height))


gd_savant_new_slim_top_middle_noswing <- gd_savant_new_slim_noswing %>% filter(plate_z > sz_middle,
                                                               abs(plate_x) < 0.5)





#m <- lm(strike ~ z_above_sztop+z_above_avg_sztop+z_above_height_top, data=gd_savant_new_slim_top_middle_noswing)
m_logistic_top <- glm(strike ~ z_above_sztop+z_above_avg_sztop+z_above_height_top, data=gd_savant_new_slim_top_middle_noswing,
                  family="binomial")
#gd_savant_new_slim_top_middle_noswing$pred_strike_linear_top <- predict(m, gd_savant_new_slim_top_middle_noswing)
gd_savant_new_slim_top_middle_noswing$pred_strike_logistic_top <- predict(m_logistic_top, gd_savant_new_slim_top_middle_noswing,
                                                                    type="response")
intercept_coef_top <- -1*coef(m_logistic_top)["(Intercept)"]
sztop_coef <- coef(m_logistic_top)["z_above_sztop"]
avg_sztop_coef <- coef(m_logistic_top)["z_above_avg_sztop"]
above_height_top_coef <- coef(m_logistic_top)["z_above_height_top"]


#umpire_full_data <- umpire_full_data  %>% mutate(umpire_full_data, based_on_sztop = plate_z-sz_top,  based_on_avgsz_top = plate_z - mean(sz_top))

gd_savant_new_slim_noswing <- gd_savant_new_slim_noswing  %>% filter(sz_top!=0) %>%
  mutate(pred_top = (intercept_coef_top + sztop_coef*sz_top + avg_sztop_coef*avg_sz_top +above_height_top_coef*percent_height_top*Height/12)/
           (sztop_coef+avg_sztop_coef+above_height_top_coef))

gd_savant_new_slim_top_middle_noswing <- gd_savant_new_slim_top_middle_noswing  %>% filter(sz_top!=0) %>%
  mutate(pred_top = (intercept_coef_top + sztop_coef*sz_top + avg_sztop_coef*avg_sz_top +above_height_top_coef*percent_height_top*Height/12)/
           (sztop_coef+avg_sztop_coef+above_height_top_coef))

gd_savant_new_slim_top_middle_noswing %>% filter(plate_z > pred_top+0.2, plate_z < sz_top-0.2) %>% summarize(n=n(), mean(strike))
pred_top_avg <- gd_savant_new_slim_top_middle_noswing %>% summarise(mean(pred_top))
