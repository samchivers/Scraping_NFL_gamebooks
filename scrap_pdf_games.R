library(readtext)
library(pdftools)
library(stringr)
library(dplyr)

#################################

home_team_loop<-read.csv('home_teams.csv')

#################################
#ids2013: 55837:56092
#ids2014: 56170:56425
#ids2015: 56503:56758
#ids2016: 56901:57156
#ids2017: 57234:57489
#ids2018: 57567:57822 
#ids2019: 57900:58155

yr = 2013
for (d in 55837:56092) {
  boolFalse<-F
  i = 1
  while(boolFalse==F){
    ht = home_team_loop$x[i]
    if (is.na(ht)){
      break
    }
    url = paste0('https://nflcdns.nfl.com/liveupdate/gamecenter/',toString(d),'/',home_team_loop$x[i],'_Gamebook.pdf')
    tryCatch({
      download.file(url,paste0('C:/Users/adrian-boss/Documents/GitHub/Scraping_NFL_gamebooks/pdfs/',toString(yr),'/',toString(d),'_',home_team_loop$x[i],'_Gamebook_',toString(yr),'.pdf'),mode="wb");
      boolFalse<-T
    },error=function(e){
    },finally={})
    i = i+1
  }
}

