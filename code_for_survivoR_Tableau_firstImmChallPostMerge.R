## Carly Levitz
## Written 2022-01-09
## Updated 2022-04-02
## Purpose: how far do the people go who won the first individual immunity after merge?

rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library");
savedir <- "H:/R/survivoR/02_cleaned_data/"

## Datasets needed
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
castawaydetails <- read.csv(paste(savedir,"survivoR_01_castawayDetails_cleaned.csv",sep=""),header=T)

## Need the day of the merge to know what was the first individual immunity challenge post-merge
  tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
  dayofmerge <- unique(tribemap[tribemap$tribe_status == "Merged",c("version","version_season","season_name","season","day")])
  for (vzn in unique(dayofmerge$version)) {
    for (sn in seq(1,max(dayofmerge$season[dayofmerge$version == vzn]),1)) {
      
      dayofmerge$merge[dayofmerge$season == sn & dayofmerge$version == vzn] <- min(dayofmerge$day[dayofmerge$season == sn & dayofmerge$version == vzn])
    }
  }
  dayofmerge <- dayofmerge[dayofmerge$day == dayofmerge$merge,]
  
## Analysis
    ## Keep just the individual challenges that were for immunity
    firstii <- challenges[challenges$outcome_type == "Individual" & challenges$challenge_type %in% c("Immunity","Reward and Immunity") & challenges$tribe == "(Individual challenge)",]
    
    # Season 41 is a little weird, just manually put that the merge was day 13
    firstii$day[firstii$season == 41 & firstii$day == 14 & firstii$version == "US"] <- 13
    #season 14, too
    firstii$day[firstii$season == 14 & firstii$day == 27 & firstii$version == "US"] <- 24
    
    # What was the earliest (first) individual immunity post-merge?
    firstii <- left_join(firstii,dayofmerge,by=c("version","version_season","season_name","season","day")) %>%
                filter(!(is.na(merge))) %>%
                select(c(version,version_season,season_name,season,castaway,castaway_id,day)) %>%
                rename(dayofchallenge=day)
    
    ## revert the challenge days that I manually edited
    firstii$dayofchallenge[firstii$season == 41 & firstii$dayofchallenge == 13 & firstii$version == "US"] <- 14
    firstii$dayofchallenge[firstii$season == 14 & firstii$dayofchallenge == 24 & firstii$version == "US"] <- 27    
    
    ## bring on additional information 
    firstii <- firstii %>%
                  # how long the player lasted in the game overall, their placement, and jury status
                  left_join(castaways %>% 
                              select(version,version_season,season_name,season,castaway,castaway_id,full_name,episode,day,order,result,jury_status) %>%
                              group_by(version,version_season,castaway_id) %>%
                              filter(day==max(day,na.rm=T))) %>%
                  mutate(dayslastedpostwin=day-dayofchallenge) %>%
                  rename(daylasteduntil=day) %>%
                  # number of days in the season
                  left_join(castaways %>% 
                    select(version,version_season,day) %>%
                    group_by(version,version_season) %>%
                    filter(day==max(day,na.rm=T)) %>% 
                    distinct() %>%
                    rename(daysinseason=day)) %>%
                  # gender and race 
                  left_join(castawaydetails %>%
                              select(short_name,castaway_id,gender,poc) %>%
                              rename(castaway=short_name)) %>%
                  # % of game that was post-merge, that they lasted
                  mutate(daysinpostmerge=daysinseason-dayofchallenge,percentofpostmergethattheylasted=dayslastedpostwin/daysinpostmerge)

write.csv(firstii,paste(savedir,"survivor_firstImmChallWinner.csv",sep=""),row.names=F)



## # of people of color that won indiv imm, # of people of color that the game started with

