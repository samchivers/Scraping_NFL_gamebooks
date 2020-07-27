library(readtext)
library(pdftools)
library(stringr)
library(dplyr)

# 2019 Season
i=12
#Get list of PDFs
lst_files<-list.files(path = "C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/")
           
#Create output list to save datasets
output <- list() 
#length(lst_files)
#For Loop thru files
for (i in 1:20){
  #Get file
  file_name<-lst_files[i]
  file<-pdf_text(paste0("C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/",file_name))
  
  #Get Teams and direction
  file1<- file%>%strsplit("[.]")%>% unlist()
  elects <- file1 %>%.[(str_detect(., "elects")==TRUE) & (str_detect(., "defer")!=TRUE) ]  
  Q1 <- elects[1] %>% str_trim()%>% sub('.*Quarter', '', .)%>% str_trim() %>%sub('.*\r\n', '', .)
  Q3 <- elects[2]%>% str_trim() %>% sub('.*Quarter', '', .)%>% str_trim() %>%sub('.*\r\n', '', .)
  
  #Get teams and directions by Quarter
  first_defending_team_q1 <- Q1 %>%strsplit("[,]") %>% unlist() %>% .[2]%>% str_trim()%>%strsplit(" ") %>% unlist() %>% .[2]
  first_defending_team_q1 <-Q1%>% sub('.*Receive,', '', .)%>% str_trim()%>%strsplit(" ") %>% unlist() %>% .[2]
  team_defending_string_q1 <- Q1 %>%strsplit("[,]") %>% unlist() %>% tail(., n=1) %>% str_trim()
  
  first_defending_team_q3 <-Q3 %>%strsplit("[,]") %>% unlist()  %>% .[2]%>% str_trim()%>%strsplit(" ") %>% unlist() %>% .[2]
  team_defending_string_q3 <- Q3 %>%strsplit("[,]") %>% unlist() %>% .[2]%>% str_trim()
  
  #Get home and away team
  file2 <- file[1]
  file2<- file2 %>%strsplit("[.]")%>% unlist()
  home_team <- file2 %>% .[(str_detect(., "HOME:")==TRUE)]%>% sub('.*HOME:', '', .)%>% str_trim() %>%sub("^(\\D+).*", "\\1", .) %>%str_trim() 
  away_team<-file2 %>% .[(str_detect(., "VISITOR:")==TRUE)]%>% sub('.*VISITOR:', '', .)%>% str_trim() %>%sub("^(\\D+).*", "\\1", .) %>%str_trim() 
  
  #Create DF
  teams <- data.frame('home_team'=home_team,'away_team'=away_team,'team_defending_str_q1'=team_defending_string_q1,
                      'def_team_q1'=first_defending_team_q1,'team_defending_str_q3'=team_defending_string_q3,
                      'def_team_q3'=first_defending_team_q3)  
  
  #Clean DF and obtain Def direction per Quarter
  teams<-teams %>% mutate(
    home_team = if_else(home_team == 'Chicago Bears','CHI',
                        if_else(home_team == 'Carolina Panthers','CAR',
                                if_else(home_team =='Green Bay Packers','GB',
                                        if_else(home_team == 'Cleveland Browns','CLI',
                                                if_else(home_team == 'Tennessee Titans','TEN',
                                                        if_else(home_team == 'Los Angeles Rams','LA',
                                                                if_else(home_team == 'Jacksonville Jaguars','JAX',
                                                                        if_else(home_team == 'Kansas City Chiefs','KC',
                                                                                if_else(home_team == 'Miami Dolphins','BLT',
                                                                                        if_else(home_team == 'Baltimore Ravens','MIA',
                                                                                                if_else(home_team == 'Minnesota Vikings','MIN',
                                                                                                        if_else(home_team == 'Atlanta Falcons','ATL',
                                                                                                                if_else(home_team == 'New York Jets','NYJ',
                                                                                                                        if_else(home_team == 'Buffalo Bills','BUF',
                                                                                                                                if_else(home_team == 'Philadelphia Eagles','PHI',
                                                                                                                                        if_else(home_team == 'Washington Redskins','WAS',
                                                                                                                                                if_else(home_team == 'Los Angeles Chargers','LAC',
                                                                                                                                                        if_else(home_team == 'Indianapolis Colts','IND',
                                                                                                                                                                if_else(home_team == 'Seattle Seahawks','SEA',
                                                                                                                                                                        if_else(home_team == 'Cincinnati Bengals','CIN',
                                                                                                                                                                                if_else(home_team == 'Arizona Cardinals','ARZ',
                                                                                                                                                                                        if_else(home_team == 'Detroit Lions','DET',
                                                                                                                                                                                                if_else(home_team == 'Dallas Cowboys','DAL',
                                                                                                                                                                                                        if_else(home_team == 'Tampa Bay Buccaneers','TB',
                                                                                                                                                                                                                if_else(home_team == 'New England Patriots','NE',
                                                                                                                                                                                                                        if_else(home_team == 'New Orleans Saints','NO',
                                                                                                                                                                                                                                if_else(home_team == 'New York Giants','NYG',
                                                                                                                                                                                                                                        if_else(home_team == 'San Francisco','SF',
                                                                                                                                                                                                                                                if_else(home_team == 'Pittsburgh Steelers','PIT',
                                                                                                                                                                                                                                                        if_else(home_team == 'Houston Texans','HOU',
                                                                                                                                        home_team))))))))))))))))))))))))))))))
    ,
    away_team = if_else(away_team == 'Chicago Bears','CHI',
                        if_else(away_team == 'Carolina Panthers','CAR',
                                if_else(away_team =='Green Bay Packers','GB',
                                        if_else(away_team == 'Cleveland Browns','CLI',
                                                if_else(away_team == 'Tennessee Titans','TEN',
                                                        if_else(away_team == 'Los Angeles Rams','LA',
                                                                if_else(away_team == 'Jacksonville Jaguars','JAX',
                                                                        if_else(away_team == 'Kansas City Chiefs','KC',
                                                                                if_else(away_team == 'Miami Dolphins','BLT',
                                                                                        if_else(away_team == 'Baltimore Ravens','MIA',
                                                                                                if_else(away_team == 'Minnesota Vikings','MIN',
                                                                                                        if_else(away_team == 'Atlanta Falcons','ATL',
                                                                                                                if_else(away_team == 'New York Jets','NYJ',
                                                                                                                        if_else(away_team == 'Buffalo Bills','BUF',
                                                                                                                                if_else(away_team == 'Philadelphia Eagles','PHI',
                                                                                                                                        if_else(away_team == 'Washington Redskins','WAS',
                                                                                                                                                if_else(away_team == 'Los Angeles Chargers','LAC',
                                                                                                                                                        if_else(away_team == 'Indianapolis Colts','IND',
                                                                                                                                                                if_else(away_team == 'Seattle Seahawks','SEA',
                                                                                                                                                                        if_else(away_team == 'Cincinnati Bengals','CIN',
                                                                                                                                                                                if_else(away_team == 'Arizona Cardinals','ARZ',
                                                                                                                                                                                        if_else(away_team == 'Detroit Lions','DET',
                                                                                                                                                                                                if_else(away_team == 'Dallas Cowboys','DAL',
                                                                                                                                                                                                        if_else(away_team == 'Tampa Bay Buccaneers','TB',
                                                                                                                                                                                                                if_else(away_team == 'New England Patriots','NE',
                                                                                                                                                                                                                        if_else(away_team == 'New Orleans Saints','NO',
                                                                                                                                                                                                                                if_else(away_team == 'New York Giants','NYG',
                                                                                                                                                                                                                                        if_else(away_team == 'San Francisco','SF',
                                                                                                                                                                                                                                                if_else(away_team == 'Pittsburgh Steelers','PIT',
                                                                                                                                                                                                                                                        if_else(away_team == 'Houston Texans','HOU',
                                                                                                                                                                                                                                                                away_team))))))))))))))))))))))))))))))
    ,
    def_direction_q1 = if_else(str_detect(tolower(team_defending_str_q1),'south'),'south',
                               if_else(str_detect(tolower(team_defending_str_q1),'north'),'north',
                                       if_else(str_detect(tolower(team_defending_str_q1),'east'),'east',
                                               if_else(str_detect(tolower(team_defending_str_q1),'west'),'west',''))))
    ,
    def_direction_q3 = if_else(str_detect(tolower(team_defending_str_q3),'south'),'south',
                               if_else(str_detect(tolower(team_defending_str_q3),'north'),'north',
                                       if_else(str_detect(tolower(team_defending_str_q3),'east'),'east',
                                               if_else(str_detect(tolower(team_defending_str_q3),'west'),'west',''))))
    ,
    game_id=paste(away_team,home_team,'2019',sep='_')
  ) %>%select(-team_defending_str_q1,-team_defending_str_q3)
  
  #Append to output list
  output[[teams$game_id[1]]] <- teams
}

#Create Output Dataframe
dplyr::bind_rows(output)

