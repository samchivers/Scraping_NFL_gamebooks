#Load in Packages
installed <- as.data.frame(installed.packages())
write.csv(installed,'documents/installed.csv')
pacman::p_load(rWind,RJSONIO,RCurl,XML,openair,circular,rayshader,concaveman,ggforce,corrplot,devtools,tidymodels,caTools,jpeg, png,grid,extrafont,ggbump,here, ggridges,plotly, glue, gganimate,googleVis, lubridate, rvest, stringr,tidyverse,nflfastR,ggrepel,ggimage,ggthemes,sf,RANN, magick)
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x %in% c('WAS','KC'),paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))

seasons <- 2013:2019
directions <- list()
for(season in seasons){
  data<-read.csv(paste0("https://raw.githubusercontent.com/adriancm93/Scraping_NFL_gamebooks/master/CSVs/output_direction",season,".csv"))
  directions[[season]]<-data
}
all_coin_tosses <- bind_rows(directions)
all_coin_tosses <- all_coin_tosses %>% mutate(def_team_ot = as.character(def_team_ot),def_team_q1 =as.character(def_team_q1),def_team_q3 =as.character(def_team_q3),home_team = as.character(home_team), away_team = as.character(away_team))
all_coin_tosses_mutates <- all_coin_tosses %>% 
  mutate(home_team = case_when(home_team == "HST"~"HOU",
                               home_team =='CLV'~'CLE',
                               home_team == 'ARZ'~'ARI',
                               home_team == 'OAK'~'LV',
                               home_team == 'BLT'~'BAL',
                               home_team == 'SD'~'LAC',
                               TRUE ~ home_team),
         away_team = case_when(away_team == "HST"~"HOU",
                               away_team =='CLV'~'CLE',
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
                                 TRUE ~ def_team_q3),
         def_team_ot = case_when(def_team_q3 == "HST"~"HOU",
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
                                       away_team_off_direction3 == 'south' ~ 'north'),
  home_team_off_direction5 = case_when(home_team == def_team_ot & def_direction_ot == 'west'~'east',
                                       home_team == def_team_ot & def_direction_ot == 'east' ~ 'west',
                                       home_team == def_team_ot & def_direction_ot == 'north' ~ 'south',
                                       home_team == def_team_ot & def_direction_ot == 'south' ~ 'north',
                                       away_team == def_team_ot & def_direction_ot == 'south' ~ 'south',
                                       away_team == def_team_ot & def_direction_ot == 'east' ~ 'east',
                                       away_team == def_team_ot & def_direction_ot == 'west' ~ 'west',
                                       away_team == def_team_ot & def_direction_ot == 'north' ~ 'north'
  ),
  away_team_off_direction5 = case_when(home_team == def_team_ot & def_direction_ot == 'west'~'west',
                                       home_team == def_team_ot & def_direction_ot == 'east' ~ 'east',
                                       home_team == def_team_ot & def_direction_ot == 'north' ~ 'north',
                                       home_team == def_team_ot & def_direction_ot == 'south' ~ 'south',
                                       away_team == def_team_ot & def_direction_ot == 'south' ~ 'north',
                                       away_team == def_team_ot & def_direction_ot == 'east' ~ 'west',
                                       away_team == def_team_ot & def_direction_ot == 'west' ~ 'east',
                                       away_team == def_team_ot & def_direction_ot == 'north' ~ 'south'
  ))

all_coin_tosses_mutates
summary(as.factor(all_coin_tosses_mutates$home_team_off_direction1))
games <- read.csv('Downloads/games.csv')
games <- games %>% mutate(home_team = as.character(home_team),away_team=as.character(away_team),
                          home_team = case_when(home_team == "HST"~"HOU",
                                                home_team =='CLV'~'CLE',
                                                home_team == 'ARZ'~'ARI',
                                                home_team == 'OAK'~'LV',
                                                home_team == 'BLT'~'BAL',
                                                home_team == 'SD'~'LAC',
                                                TRUE ~ home_team),
                          away_team = case_when(away_team == "HST"~"HOU",
                                                away_team =='CLV'~'CLE',
                                                away_team == 'ARZ'~'ARI',
                                                away_team == 'OAK'~'LV',
                                                away_team == 'BLT'~'BAL',
                                                away_team == 'SD'~'LAC',
                                                TRUE ~ away_team)) %>% filter(game_type == 'REG')

join <- merge(games, all_coin_tosses_mutates, by =c('home_team','away_team', 'season'))

join_weather <- join %>% filter(roof!='closed',roof!='dome')

join_weather
names(join_weather)
ultimate <- inner_join(join_weather, windy_outdoors_games, by = c('game_id.x'='game_id'))
ultimate <- ultimate %>% arrange(desc(ws))
ultimate <- ultimate %>% filter(!is.na(home_team_off_direction1),!is.na(home_team_off_direction3))
ultimate

for_nfl_fast_r <- ultimate %>% select(game_id.x, home_team_off_direction1,home_team_off_direction2, home_team_off_direction3, home_team_off_direction4,away_team_off_direction1, away_team_off_direction2, away_team_off_direction3,away_team_off_direction4, ws,wd, wind_compass, roof,stadium.x,StadiumAzimuthAngle)
for_nfl_fast_r
ok <- inner_join(all_nfl_pbp,for_nfl_fast_r, by =c('game_id'='game_id.x')) 

unique(ok$posteam_type)
ok <- ok %>% filter(play ==1, pass ==1|rush ==1) %>% mutate(posteam_driving = case_when(posteam_type == 'home' & qtr ==1 ~ home_team_off_direction1,
                                                                                        posteam_type == 'home' & qtr ==2 ~ home_team_off_direction2,
                                                                                        posteam_type == 'home' & qtr ==3 ~ home_team_off_direction3,
                                                                                        posteam_type == 'home' & qtr ==4 ~ home_team_off_direction4,
                                                                                        posteam_type == 'away' & qtr ==1 ~ away_team_off_direction1,
                                                                                        posteam_type == 'away' & qtr ==2 ~ away_team_off_direction2,
                                                                                        posteam_type == 'away' & qtr ==3 ~ away_team_off_direction3,
                                                                                        posteam_type == 'away' & qtr ==4 ~ away_team_off_direction4,
                                                                                        TRUE ~ "None for Now"),
                                                            wind_speed_bins = case_when(ws>=0&ws<=4 ~ "Light Wind",
                                                                                        ws>4 &ws <= 9 ~ "Medium Wind",
                                                                                        ws>8&ws<=14 ~'High Wind',
                                                                                        ws>14 ~"Strong Wind"),
                                                            wind_blowing = case_when(wind_compass =='N'~'north',
                                                                                     wind_compass == 'S'~'south',
                                                                                     wind_compass == 'W'~'west',
                                                                                     wind_compass == 'E'~'east'),
                                                            early_trial = case_when(wind_blowing=='north' & posteam_driving == 'east'|posteam_driving =='west'~'cross-wind',
                                                                                    wind_blowing=='south' & posteam_driving == 'east'|posteam_driving =='west'~'cross-wind',
                                                                                    wind_blowing=='east' & posteam_driving == 'north'|posteam_driving =='south'~'cross-wind',
                                                                                    wind_blowing=='west' & posteam_driving == 'north'|posteam_driving =='south'~'cross-wind',
                                                                                    wind_blowing =='north' & posteam_driving =='south' ~'wind_in_face',
                                                                                    wind_blowing =='south' & posteam_driving =='north' ~'wind_in_face',
                                                                                    wind_blowing =='east' & posteam_driving =='west' ~'wind_in_face',
                                                                                    wind_blowing =='west' & posteam_driving =='east' ~'wind_in_face',
                                                                                    wind_blowing =='west' & posteam_driving =='west' ~'wind_at_back',
                                                                                    wind_blowing =='east' & posteam_driving =='east' ~'wind_at_back',
                                                                                    wind_blowing =='south' & posteam_driving =='south' ~'wind_at_back',
                                                                                    wind_blowing =='north' & posteam_driving =='north' ~'wind_at_back',
                                                                                    TRUE ~'None'),
                                                            air_yards_bins = case_when(air_yards <0 ~ 'Behind LOS',
                                                                                       air_yards >0 & air_yards <=5 ~'0-5 Yards',
                                                                                       air_yards >5 & air_yards <=10~'5-10 Yards',
                                                                                       air_yards >10 & air_yards<=15~'10-15 Yards',
                                                                                       air_yards>15&air_yards<=20~'15-20 Yards',
                                                                                       air_yards>20~'Greater than 20 Yards',
                                                                                       TRUE ~'0 Yards'),
                                                            stadium_wind_adj = 360 - StadiumAzimuthAngle,
                                                            actual_stadium_wind = wd-StadiumAzimuthAngle,
                                                            deep_pass = ifelse(air_yards >20, 1,0))


ok %>% select(game_id,play_id,posteam, wd,ws,posteam_driving,wind_speed_bins,wind_blowing,early_trial)
summary(as.factor(ok$early_trial))

team_rush <- ok %>% group_by(posteam,early_trial) %>% filter(early_trial!='None') %>% summarize(plays = n(), epa = mean(epa),rush_rate = mean(rush), pass_rate = mean(pass))
team_rush %>% ggplot(aes(x = rush_rate, y = epa , color = early_trial)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_fill_manual(values=c('gold','pink','green'))+
  #geom_smooth(se = FALSE, linetype = 'solid', size = 1.25) +
  theme_fivethirtyeight() +
  theme(text = element_text(),
        plot.title = element_text(size = 12, family = "Trebuchet MS", hjust =.5, color = 'gold'),
        plot.subtitle = element_text(size = 12,family = "Trebuchet MS", color = 'gold', hjust =.5),
        axis.title = element_text(size = 12,family = "Trebuchet MS", color = 'gold'),
        axis.text = element_text(size = 10, face = "bold", color = 'gold'),
        plot.caption = element_text(color = 'gold',family = "Trebuchet MS"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8, family = 'Trebuchet MS'),
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.background = element_rect(fill="grey", 
                                         size=0.5, linetype="dashed"),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#070B34"),
        plot.background = element_rect(fill = "#070B34"),
        axis.line.x = element_line(color = 'gold'),
        axis.line.y = element_line(color = 'gold')) +
  labs(title = "Weather Effect?",
       subtitle = "2016-2019",
       x = "Team Rush Rate",
       y = "EPA",
       caption = 'Data from @nflfastR | Plot by @Sam_S35')

pass <- ok %>% filter(pass ==1, early_trial!='None')
pass %>% select(play_id,posteam, wd,ws,posteam_driving,wind_speed_bins,wind_blowing,early_trial)
alright <- pass %>% filter(posteam_type == 'away') %>% group_by(wind_speed_bins,early_trial)  %>% 
  summarize(deep_pass_rate = mean(deep_pass,na.rm=T),plays = n(),epa = mean(epa), completion = mean(complete_pass),air_yards = mean(air_yards, na.rm =T)) %>%
  arrange(completion)
passers <- all_nfl_pbp %>% filter(season>= 2016, qb_dropback==1) %>% group_by(passer_player_name,posteam) %>% summarize(n = n()) %>% 
  filter(n>1000)
passers
hmm <- ok %>% group_by(game_id) %>% filter(wind_speed_bins == 'High Wind',early_trial == 'wind_in_face'|early_trial=='wind_at_back')
check <- hmm %>% group_by(game_id,posteam,early_trial,posteam_type) %>% summarize(plays = n(), ws = mean(ws), epa = mean(epa,na.rm=T),air_yards = mean(air_yards,na.rm=T)) %>% arrange(epa)
check
alright <- pass %>% filter(passer_player_name %in% passers$passer_player_name)
alright <- inner_join(alright,logos, by =c('posteam'='team_abbr'))
alright %>% filter(air_yards!=0, ws>10) %>% ggplot(aes(x =air_yards, y = cp, color = early_trial)) +
  geom_smooth(se= FALSE, linetype = 'dashed')+  
  scale_fill_manual(values=c('gold','pink','green'))+
  facet_wrap(~posteam_type) +
  #geom_smooth(se = FALSE, linetype = 'solid', size = 1.25) +
  theme_fivethirtyeight() +
  theme(text = element_text(),
        plot.title = element_text(size = 12, family = "Trebuchet MS", hjust =.5, color = 'gold'),
        plot.subtitle = element_text(size = 12,family = "Trebuchet MS", color = 'gold', hjust =.5),
        axis.title = element_text(size = 12,family = "Trebuchet MS", color = 'gold'),
        axis.text = element_text(size = 10, face = "bold", color = 'gold'),
        plot.caption = element_text(color = 'gold',family = "Trebuchet MS"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8, family = 'Trebuchet MS'),
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.background = element_rect(fill="grey", 
                                         color = 'gold',
                                         size=0.5, linetype="solid"),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#070B34"),
        plot.background = element_rect(fill = "#070B34"),
        axis.line.x = element_line(color = 'gold'),
        axis.line.y = element_line(color = 'gold')) +
  labs(title = "Weather Effect?",
       subtitle = "2013-2019",
       x = "Air Yards",
       y = "Completion Percentage",
       caption = 'Data from @nflfastR | Plot by @Sam_S35')



write.csv(ultimate,'Documents/testingman.csv')