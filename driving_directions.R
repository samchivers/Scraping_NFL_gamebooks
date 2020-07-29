#Load in Packages

pacman::p_load(rWind,RJSONIO,RCurl,XML,openair,circular,rayshader,concaveman,ggforce,corrplot,devtools,tidymodels,caTools,jpeg, png,grid,extrafont,ggbump,here, ggridges,plotly, glue, gganimate,googleVis, lubridate, rvest, stringr,tidyverse,nflfastR,ggrepel,ggimage,ggthemes,sf,RANN, magick)
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x %in% c('WAS','KC'),paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))

seasons <- 2016:2019
directions <- list()
for(season in seasons){
  data<-read.csv(paste0("https://raw.githubusercontent.com/adriancm93/Scraping_NFL_gamebooks/master/CSVs/output_direction",season,".csv"))
  directions[[season]]<-data
}
all_coin_tosses <- bind_rows(directions)
all_coin_tosses <- all_coin_tosses %>% mutate(def_team_q1 =as.character(def_team_q1),def_team_q3 =as.character(def_team_q3),home_team = as.character(home_team), away_team = as.character(away_team))
all_coin_tosses_mutates <- all_coin_tosses %>% 
  mutate(home_team = case_when(home_team == "HST"~"HOU",
                               home_team =='CLI'~'CLE',
                               home_team == 'ARZ'~'ARI',
                               home_team == 'OAK'~'LV',
                               home_team == 'BLT'~'BAL',
                               home_team == 'SD'~'LAC',
                               TRUE ~ home_team),
         away_team = case_when(away_team == "HST"~"HOU",
                               away_team =='CLI'~'CLE',
                               away_team == 'ARZ'~'ARI',
                               away_team == 'OAK'~'LV',
                               away_team == 'BLT'~'BAL',
                               away_team == 'SD'~'LAC',
                               TRUE ~ away_team),
         def_team_q1 = case_when(def_team_q1 == "HST"~"HOU",
                                 def_team_q1 =='CLI'~'CLE',
                                 def_team_q1 =='CLV'~'CLE',
                                 def_team_q1 == 'ARZ'~'ARI',
                                 def_team_q1 == 'OAK'~'LV',
                                 def_team_q1 == 'BLT'~'BAL',
                                 def_team_q1 == 'SD'~'LAC',
                                 def_team_q1 == 'wins' ~'ARI',
                                 TRUE ~ def_team_q1),
         def_team_q3 = case_when(def_team_q3 == "HST"~"HOU",
                                 def_team_q3 =='CLI'~'CLE',
                                 def_team_q3 =='CLV'~'CLE',
                                 def_team_q3 == 'ARZ'~'ARI',
                                 def_team_q3 == 'OAK'~'LV',
                                 def_team_q3 == 'BLT'~'BAL',
                                 def_team_q3 == 'SD'~'LAC',
                                 TRUE ~ def_team_q3))

all_coin_tosses_mutates <- all_coin_tosses_mutates %>% mutate(season = as.numeric(word(game_id,3,sep = '_')))
all_coin_tosses_mutates <- all_coin_tosses_mutates %>% 
  mutate(home_team_off_direction1 = case_when(home_team == def_team_q1 & def_direction_q1 == 'west'~'east',
                                              home_team == def_team_q1 & def_direction_q1 == 'east' ~ 'west',
                                              home_team == def_team_q1 & def_direction_q1 == 'north' ~ 'south',
                                              home_team == def_team_q1 & def_direction_q1 == 'south' ~ 'north',
                                              away_team == def_team_q1 & def_direction_q1 == 'south' ~ 'south',
                                              away_team == def_team_q1 & def_direction_q1 == 'east' ~ 'east',
                                              away_team == def_team_q1 & def_direction_q1 == 'west' ~ 'west',
                                              away_team == def_team_q1 & def_direction_q1 == 'north' ~ 'north'
  ),
  home_team_off_direction3 = case_when(home_team == def_team_q3 & def_direction_q3 == 'west'~'east',
                                       home_team == def_team_q3 & def_direction_q3 == 'east' ~ 'west',
                                       home_team == def_team_q3 & def_direction_q3 == 'north' ~ 'south',
                                       home_team == def_team_q3 & def_direction_q3 == 'south' ~ 'north',
                                       away_team == def_team_q3 & def_direction_q3 == 'south' ~ 'south',
                                       away_team == def_team_q3 & def_direction_q3 == 'east' ~ 'east',
                                       away_team == def_team_q3 & def_direction_q3 == 'west' ~ 'west',
                                       away_team == def_team_q3 & def_direction_q3 == 'north' ~ 'north'
  ),
  home_team_off_direction2 = case_when(home_team_off_direction1 == 'west'~'east',
                                       home_team_off_direction1 == 'east' ~ 'west',
                                       home_team_off_direction1 =='north' ~ 'south',
                                       home_team_off_direction1 == 'south' ~ 'north'
  ),
  home_team_off_direction4 = case_when(home_team_off_direction3 == 'west'~'east',
                                       home_team_off_direction3 == 'east' ~ 'west',
                                       home_team_off_direction3 =='north' ~ 'south',
                                       home_team_off_direction3 == 'south' ~ 'north'
  ),
  away_team_off_direction1 = case_when(home_team == def_team_q1 & def_direction_q1 == 'west'~'west',
                                       home_team == def_team_q1 & def_direction_q1 == 'east' ~ 'east',
                                       home_team == def_team_q1 & def_direction_q1 == 'north' ~ 'north',
                                       home_team == def_team_q1 & def_direction_q1 == 'south' ~ 'south',
                                       away_team == def_team_q1 & def_direction_q1 == 'south' ~ 'north',
                                       away_team == def_team_q1 & def_direction_q1 == 'east' ~ 'west',
                                       away_team == def_team_q1 & def_direction_q1 == 'west' ~ 'east',
                                       away_team == def_team_q1 & def_direction_q1 == 'north' ~ 'south'
  ),
  away_team_off_direction3 = case_when(home_team == def_team_q3 & def_direction_q3 == 'west'~'west',
                                       home_team == def_team_q3 & def_direction_q3 == 'east' ~ 'east',
                                       home_team == def_team_q3 & def_direction_q3 == 'north' ~ 'north',
                                       home_team == def_team_q3 & def_direction_q3 == 'south' ~ 'south',
                                       away_team == def_team_q3 & def_direction_q3 == 'south' ~ 'north',
                                       away_team == def_team_q3 & def_direction_q3 == 'east' ~ 'west',
                                       away_team == def_team_q3 & def_direction_q3 == 'west' ~ 'east',
                                       away_team == def_team_q3 & def_direction_q3 == 'north' ~ 'south'
  ),
  away_team_off_direction2 = case_when(away_team_off_direction1 == 'west'~'east',
                                       away_team_off_direction1 == 'east' ~ 'west',
                                       away_team_off_direction1 =='north' ~ 'south',
                                       away_team_off_direction1 == 'south' ~ 'north'
  ),
  away_team_off_direction4 = case_when(away_team_off_direction3 == 'west'~'east',
                                       away_team_off_direction3 == 'east' ~ 'west',
                                       away_team_off_direction3 =='north' ~ 'south',
                                       away_team_off_direction3 == 'south' ~ 'north'))

summary(as.factor(all_coin_tosses_mutates$home_team_off_direction2))
games <- read.csv('../../../../Downloads/games.csv')
games
join <- merge(all_coin_tosses_mutates, games, by =c('home_team','away_team', 'season'))
join <- join %>% filter(roof!='closed',roof!='dome')
ultimate <- inner_join(join, windy_outdoors_games, by = c('game_id.y'='game_id'))
ultimate <- ultimate %>% arrange(desc(ws))
write.csv(ultimate,'../../../../Documents/testingman.csv')