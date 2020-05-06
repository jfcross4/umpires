library(readr); library(dplyr)
gd_savant_new_slim <- read_csv("gd_savant_new_slim.csv")
player_heights <- read_csv("player_heights.csv")
gd_savant_new_slim <- left_join(gd_savant_new_slim, player_heights, by=c("batter"="mlbamid"))

avg_sz_bot <- mean(gd_savant_new_slim$sz_bot, na.rm=TRUE)
avg_player_height <- mean(gd_savant_new_slim$Height, na.rm=TRUE)
avg_sz_top <- mean(gd_savant_new_slim$sz_top, na.rm=TRUE)
sz_middle <- (avg_sz_top+avg_sz_bot)/2

percent_height_bot <- avg_sz_bot*12/avg_player_height

gd_savant_new_slim <- gd_savant_new_slim %>% mutate(z_above_szbot = plate_z - sz_bot,
                                                    z_above_avg_szbot = plate_z - avg_sz_bot,
                                                    z_above_height_bot = plate_z - percent_height_bot*Height/12
)

gd_savant_new_slim_noswing <- gd_savant_new_slim %>% 
  filter(description=="called_strike" | description=="ball") %>% mutate(
    strike = as.numeric(description =="called_strike")
  ) %>% filter(!is.na(Height))


gd_savant_new_slim_bot_middle_noswing <- gd_savant_new_slim_noswing %>% filter(plate_z < sz_middle,
                                                                       abs(plate_x) < 0.5)





#m <- lm(strike ~ z_above_sztop+z_above_avg_sztop+z_above_height_top, data=gd_savant_new_slim_top_middle_noswing)
m_logistic_bot <- glm(strike ~ z_above_szbot+z_above_avg_szbot+z_above_height_bot, data=gd_savant_new_slim_bot_middle_noswing,
                  family="binomial")
#gd_savant_new_slim_bot_middle_noswing$pred_strike_linear <- predict(m, gd_savant_new_slim_bot_middle_noswing)
gd_savant_new_slim_bot_middle_noswing$pred_strike_logistic_bot <- predict(m_logistic_bot, gd_savant_new_slim_bot_middle_noswing,
                                                                      type="response")
intercept_coef_bot <- -1*coef(m_logistic_bot)["(Intercept)"]
szbot_coef <- coef(m_logistic_bot)["z_above_szbot"]
avg_szbot_coef <- coef(m_logistic_bot)["z_above_avg_szbot"]
above_height_bot_coef <- coef(m_logistic_bot)["z_above_height_bot"]


#umpire_full_data <- umpire_full_data  %>% mutate(umpire_full_data, based_on_szbot = plate_z-sz_bot,  based_on_avgsz_bot = plate_z - mean(sz_bot))

gd_savant_new_slim_noswing <- gd_savant_new_slim_noswing  %>% filter(sz_bot!=0) %>%
  mutate(pred_bot = (intercept_coef_bot + szbot_coef*sz_bot + avg_szbot_coef*avg_sz_bot +above_height_bot_coef*percent_height_bot*Height/12)/
           (szbot_coef+avg_szbot_coef+above_height_bot_coef))

gd_savant_new_slim_bot_middle_noswing <- gd_savant_new_slim_bot_middle_noswing  %>% filter(sz_bot!=0) %>%
  mutate(pred_bot = (intercept_coef_bot + szbot_coef*sz_bot + avg_szbot_coef*avg_sz_bot +above_height_bot_coef*percent_height_bot*Height/12)/
           (szbot_coef+avg_szbot_coef+above_height_bot_coef))

gd_savant_new_slim_bot_middle_noswing %>% filter(plate_z > pred_bot-0.1, plate_z < sz_bot+0.1) %>% summarize(n=n(), mean(strike))

pred_bot_avg <- gd_savant_new_slim_bot_middle_noswing %>% summarise(mean(pred_bot))
