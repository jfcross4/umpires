#Installing + Scraping
library(dplyr)
library(devtools)
install_github("BillPetti/baseballr")
library(baseballr)

get_probables_mlb(566001)

?scrape_statcast_savant
t <- Sys.time()
daily_data <- scrape_statcast_savant(start_date = "2016-04-03", end_date = "2016-6-03")
Sys.time()-t
View(daily_data)
save(daily_data, file = "daily_data2016.RData")

umpires <- get_probables_mlb(as.numeric(daily_data$game_pk[1]))


for (i in 2:length(unique(daily_data$game_pk))){
  print(i)
  print(unique(daily_data$game_pk)[i])
  temp <- get_probables_mlb(unique(daily_data$game_pk)[i])
  umpires <- rbind(umpires, temp)
  
}
gd_savant_data <- read.csv("gd_savant_new_slim.csv")
head(gd_savant_data)

library(readr)
gd_savant_new_slim <- read_csv("gd_savant_new_slim.csv")

umpire_full_data <- left_join(umpires, daily_data, "game_pk")
View(umpire_full_data)

umpires_slim <- umpires %>% dplyr::select(game_pk, home_plate.fullName, home_plate.id) %>% unique()

umpire_full_data <- left_join(umpires_slim, daily_data, "game_pk")
View(umpire_full_data)

unique_game_pks <- unique(gd_savant_new_slim$game_pk)

umpires <- get_probables_mlb(unique_game_pks[1])


for (i in 20:length(unique_game_pks)){
  print(i)
  print(unique_game_pks[i])
  temp <- get_probables_mlb(unique_game_pks[i])
  umpires <- rbind(umpires, temp)
  
}

already_scraped_game_pks <- unique(umpires$game_pk)

not_yet_scrape_game_pks <- setdiff(unique_game_pks, already_scraped_game_pks) 
not_yet_scrape_game_pks <- not_yet_scrape_game_pks[!is.na(not_yet_scrape_game_pks)]

for (i in 1:length(not_yet_scrape_game_pks)){
  temp <- get_probables_mlb(not_yet_scrape_game_pks[i])
  umpires <- rbind(umpires, temp)
  
}

t <- Sys.time()
for (i in 1:x){
  temp <- get_probables_mlb(not_yet_scrape_game_pks[i])
  umpires <- rbind(umpires, temp)
  
}
Sys.time() -t

umpire_data_url <- "https://t.co/R17oTRDnUN"
umpires <- read.csv(umpire_data_url)
library(dplyr)
umpires <- umpires %>% filter(position=="HP")
