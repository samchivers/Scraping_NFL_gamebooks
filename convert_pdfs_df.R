library(readtext)
library(pdftools)
library(stringr)
library(dplyr)
setwd("~/GitHub/Scraping_NFL_gamebooks")

#Pick year
yr=2013

#Double check length (must be 256)
list.files(path = paste0("C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/",toString(yr)))%>% length()

#Get list of PDFs
lst_files<-list.files(path = paste0("C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/",toString(yr),"/"))

#Create output list to save datasets
output <- list() 

#For Loop thru files
for (i in 1:length(lst_files)){
  #Get file
  file_name<-lst_files[i]
  file<-pdf_text(paste0("C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/",toString(yr),"/",file_name))
  #Get home and away team
  file2 <- file[1]
  file2<- file2 %>%strsplit("[.]")%>% unlist()
  home_team <- file2 %>% .[(str_detect(., "HOME:")==TRUE)]%>% sub('.*HOME:', '', .)%>% str_trim() %>%sub("^(\\D+).*", "\\1", .) %>%str_trim() 
  away_team<-file2 %>% .[(str_detect(., "VISITOR:")==TRUE)]%>% sub('.*VISITOR:', '', .)%>% str_trim() %>%sub("^(\\D+).*", "\\1", .) %>%str_trim() 
  
  home_team = if_else(home_team == 'Chicago Bears','CHI',
                      if_else(home_team == 'Carolina Panthers','CAR',
                              if_else(home_team =='Green Bay Packers','GB',
                                      if_else(home_team == 'Cleveland Browns','CLV',
                                              if_else(home_team == 'Tennessee Titans','TEN',
                                                      if_else(home_team == 'Los Angeles Rams','LA',
                                                              if_else(home_team == 'Jacksonville Jaguars','JAX',
                                                                      if_else(home_team == 'Kansas City Chiefs','KC',
                                                                              if_else(home_team == 'Miami Dolphins','MIA',
                                                                                      if_else(home_team == 'Baltimore Ravens','BLT',
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
                                                                                                                                                                                                                                                      if_else(home_team == 'Houston Texans','HST',
                                                                                                                                                                                                                                                              if_else(home_team == 'Denver Broncos','DEN',
                                                                                                                                                                                                                                                                      if_else(home_team == 'Oakland Raiders','OAK',
                                                                                                                                                                                                                                                                              if_else(home_team == 'San Diego Chargers','SD',
                                                                                                                                                                                                                                                                                      if_else(home_team == 'St','SL',
                                                                                                                                                                                                                                                                                      home_team))))))))))))))))))))))))))))))))))
  
  away_team = if_else(away_team == 'Chicago Bears','CHI',
                      if_else(away_team == 'Carolina Panthers','CAR',
                              if_else(away_team =='Green Bay Packers','GB',
                                      if_else(away_team == 'Cleveland Browns','CLV',
                                              if_else(away_team == 'Tennessee Titans','TEN',
                                                      if_else(away_team == 'Los Angeles Rams','LA',
                                                              if_else(away_team == 'Jacksonville Jaguars','JAX',
                                                                      if_else(away_team == 'Kansas City Chiefs','KC',
                                                                              if_else(away_team == 'Miami Dolphins','MIA',
                                                                                      if_else(away_team == 'Baltimore Ravens','BAL',
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
                                                                                                                                                                                                                                                      if_else(away_team == 'Houston Texans','HST',
                                                                                                                                                                                                                                                              if_else(away_team == 'Denver Broncos','DEN',
                                                                                                                                                                                                                                                                      if_else(away_team == 'Oakland Raiders','OAK',
                                                                                                                                                                                                                                                                              if_else(away_team == 'San Diego Chargers','SD',
                                                                                                                                                                                                                                                                                      if_else(away_team == 'St','SL',
                                                                                                                                                                                                                                                                                      away_team))))))))))))))))))))))))))))))))))
  # Q1
  #Get Teams and direction
  file1<- file%>%strsplit("[.]")%>% unlist()
  elects <- file1 %>% .[(str_detect(., "elects")==TRUE)]  

  # Won/lost toss 
  won_q1<- file1 %>% .[(str_detect(., "toss")==TRUE)] %>% str_trim()%>% sub('.*Quarter', '', .)%>% str_trim()%>%sub('.*\r\n', '', .) %>% str_trim() %>%strsplit(" ") %>% unlist() %>% .[1]
  lost_q1<- if_else(won_q1==home_team,away_team,home_team)
  
  # Winner decision
  string_decision <- file1 %>% .[(str_detect(., "toss")==TRUE)] %>% str_trim()%>% sub('.*Quarter', '', .)%>% str_trim()%>%sub('.*\r\n', '', .) %>% str_trim() %>%strsplit("[,]") %>% unlist()%>% .[(str_detect(., "elects")==TRUE)] %>% .[1] %>% str_trim()
  decided_q1<-case_when(str_detect(tolower(string_decision),"defer") ~ "defer",
            str_detect(tolower(string_decision),"receive") ~ "receive",
            str_detect(tolower(string_decision),"defend") ~ "defend")

  #Direction
  if(decided_q1=='defer'){
    first_direction_q1 = paste(elects[1],elects[2])
    }else{first_direction_q1 = elects[1]}
  first_def_direction_q1 = case_when(str_detect(tolower(first_direction_q1),"north") ~ "north",
                               str_detect(tolower(first_direction_q1),"south") ~ "south",
                               str_detect(tolower(first_direction_q1),"west") ~ "west",
                               str_detect(tolower(first_direction_q1),"east") ~ "east"
                               )
  #First defending team  
  if(decided_q1=='defer'){
    decision2_str_q1 <- elects  %>% .[(str_detect(., "Quarter")==FALSE)] %>%strsplit("elects") %>% unlist() %>% .[2]
    decision2_q1 = case_when(str_detect(tolower(decision2_str_q1),"receive") ~ "receive",
                             str_detect(tolower(decision2_str_q1),"defend") ~ "defend")
    if(decision2_q1 == 'receive'){
      first_defending_team_q1 <-  won_q1
    }else{
      first_defending_team_q1 <-  lost_q1
    }
  } else if(decided_q1=='receive'){
    first_defending_team_q1 <- lost_q1
  } else if(decided_q1=='defend'){
    first_defending_team_q1 <- won_q1
  }

  # Q3
  if(decided_q1=='defer'){
    elect3 <- elects[3]
  } else{elect3 <- elects[2]}

  # Get defending team and defending direction
  first_defending_team_q3<-elect3 %>% sub('.*Quarter', '', .) %>%strsplit("[,]") %>% unlist() %>% .[(str_detect(., "defend")==T)]%>%strsplit("elects") %>% unlist()%>% .[1] %>% str_trim()%>%strsplit(" ") %>% unlist()%>% tail(., n=1)
  first_defending_team_q3 <- ifelse(is.null(first_defending_team_q3),first_defending_team_q3<- NA , first_defending_team_q3<-first_defending_team_q3)
  first_def_direction_q3 = case_when(str_detect(tolower(elect3),"north") ~ "north",
                                     str_detect(tolower(elect3),"south") ~ "south",
                                     str_detect(tolower(elect3),"west") ~ "west",
                                     str_detect(tolower(elect3),"east") ~ "east")
  
  # OT 1
  ot <- elects%>% .[str_detect(., "Overtime")==TRUE |str_detect(., "overtime")==TRUE ] %>%sub('.*\r\n', '', .) %>% str_trim()  
  ot<-str_replace(ot,"wins toss,","wins toss")
  
  if (length(ot)>0){
    won_ot<-ot %>%strsplit("[,]")%>% unlist()%>%.[1] %>%strsplit(" ")%>% unlist()%>%.[1]
    lost_ot<-ot %>%strsplit("[,]")%>% unlist()%>% tail(., n=1) %>% str_trim()%>%strsplit(" ") %>% unlist() %>% .[2] 
    decided_ot<-ot %>%strsplit("[,]") %>% unlist() %>% .[1]  %>%strsplit(" ")%>% unlist() %>% tail(., n=1) %>% str_trim()
    first_defending_team_ot<-if_else(tolower(decided_ot)=='kick',won_ot,lost_ot)
    
    first_def_direction_ot <- if_else(str_detect(tolower(ot),'west'),'west',
                               if_else(str_detect(tolower(ot),'east'),'east',
                                       if_else(str_detect(tolower(ot),'south'),'south',
                                               if_else(str_detect(tolower(ot),'north'),'north',''))))
    
    first_def_direction_ot<-if_else(tolower(decided_ot) != 'kick',first_def_direction_ot,
                             if_else(first_def_direction_ot=='north','south',
                                     if_else(first_def_direction_ot=='south','north',
                                             if_else(first_def_direction_ot=='west','east',
                                                     if_else(first_def_direction_ot=='east','west',first_def_direction_ot)))))
  }else{
    first_def_direction_ot <-NA
    first_defending_team_ot<-NA
  }
  
  #Create DF
  teams <- data.frame('home_team'=home_team,'away_team'=away_team,
                      'def_team_q1'=first_defending_team_q1,
                      'def_team_q3'=first_defending_team_q3,
                      'def_team_ot'=first_defending_team_ot,
                      'def_direction_q1'= first_def_direction_q1,
                      'def_direction_q3'= first_def_direction_q3,
                      'def_direction_ot'=first_def_direction_ot)  
  
  # Get Game ID
  teams<-teams %>% mutate(
    game_id=paste(away_team,home_team,toString(yr),sep='_')
  ) 
  
  #Append to output list
  output[[teams$game_id[1]]] <- teams
}

#Create Output Dataframe
output_df <- dplyr::bind_rows(output)
#Write file
write.csv(output_df,paste0('C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/CSVs/output_direction',toString(yr),'.csv'),row.names = F)
