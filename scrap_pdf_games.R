library(readtext)
library(pdftools)
library(stringr)
library(dplyr)

#################################

home_team_loop<-read.csv('home_teams.csv')

############################################
# 2019
#########################
for (d in 57900:58155){
  boolFalse<-F
  i = 1
  while(boolFalse==F){
    ht = home_team_loop$x[i]
    if (is.na(ht)){
      break
    }
    url = paste0('https://nflcdns.nfl.com/liveupdate/gamecenter/',toString(d),'/',home_team_loop$x[i],'_Gamebook.pdf')
    tryCatch({
      download.file(url,paste0('C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/2019/',toString(d),'_',home_team_loop$x[i],'_Gamebook','_2019','.pdf'),mode="wb");
      boolFalse<-T
    },error=function(e){
    },finally={})
    i = i+1
  }
}

############################################
# 2018
#########################
for (d in 57567:57822){
  boolFalse<-F
  i = 1
  while(boolFalse==F){
    ht = home_team_loop$x[i]
    if (is.na(ht)){
      break
    }
    url = paste0('https://nflcdns.nfl.com/liveupdate/gamecenter/',toString(d),'/',home_team_loop$x[i],'_Gamebook.pdf')
    tryCatch({
      download.file(url,paste0('C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/2018/',toString(d),'_',home_team_loop$x[i],'_Gamebook','_2018','.pdf'),mode="wb");
      boolFalse<-T
    },error=function(e){
    },finally={})
    i = i+1
  }
}
