## Prep the cleaned SurvivoR data for Tableau
## Written 2022-02-16
## Carly Levitz


rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library"); library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library")
savedir <- "H:/R/survivoR/02_cleaned_data/"


castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
votehx <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)


## Superlatives for an individual
## Challenge superlatives for the individual
      ## number of individual immunity wins in one season and overall
        indivimmwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
                                                filter(grepl("Winner",outcome_status)) %>%
                                                filter(grepl("Immunity",challenge_type)) %>%
                                                select(season,winner_id,day) %>%
                                                group_by(season,winner_id) %>%
                                                summarize(immwinsOneSeason=n_distinct(day))
        indivimmwins <- indivimmwinsoneseason %>% 
                              group_by(winner_id) %>%
                              summarize(immwins = sum(immwinsOneSeason))
                                              
      ## number of individual reward wins
        indivrewwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
                                                filter(grepl("Winner",outcome_status)) %>%
                                                filter(grepl("Reward",challenge_type)) %>%
                                                select(season,winner_id,day) %>%
                                                group_by(season,winner_id) %>%
                                                summarize(rewwinsOneSeason=n_distinct(day))
        indivrewwins <- indivrewwinsoneseason %>% 
                            group_by(winner_id) %>%
                            summarize(rewwins = sum(rewwinsOneSeason))  
        
      ## number of tribal immunity wins
        tribalimmwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
                            filter(grepl("Winner",outcome_status)) %>%
                            filter(grepl("Immunity",challenge_type)) %>%
                            select(season,winner_id,day) %>%
                            group_by(season,winner_id) %>%
                            summarize(tribalimmwinsOneSeason=n_distinct(day))
        
        tribalimmwins <- tribalimmwinsoneseason %>% 
                              group_by(winner_id) %>%
                              summarize(tribalimmwins = sum(tribalimmwinsOneSeason))
        
        
      ## number of tribal reward wins
        tribalrewwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
                                filter(grepl("Winner",outcome_status)) %>%
                                filter(grepl("Reward",challenge_type)) %>%
                                select(season,winner_id,day) %>%
                                group_by(season,winner_id) %>%
                                summarize(tribalrewwinsOneSeason=n_distinct(day))
        
        tribalrewwins <- tribalrewwinsoneseason %>% 
                          group_by(winner_id) %>%
                          summarize(tribalrewwins = sum(tribalrewwinsOneSeason))  
        
  individualchallsuperlatives <-full_join(
                                  full_join(
                                    full_join(
                                      full_join(
                                        full_join(
                                          full_join(full_join(indivimmwinsoneseason,indivimmwins),
                                                indivrewwinsoneseason),indivrewwins),
                                                tribalimmwinsoneseason),tribalimmwins),
                                                tribalrewwinsoneseason),tribalrewwins) %>%
                                  rename(castaway_id=winner_id)
  
## Tribal Council information  
    ## number of times went to tc before merge
      premerge <- votehx %>% filter(tribe_status != "Merged") %>%
                      select(season,castaway_id,day) %>%
                      group_by(season,castaway_id) %>%
                      summarize(premergeTC=n_distinct(day))
                    
    ## number of votes received
      votesreceivedinoneseason <- votehx %>% select(season,vote_id) %>%
                                    group_by(season,vote_id) %>%
                                    mutate(count=1) %>%
                                    summarize(votesreceivedoneseason=sum(count)) %>%
                                    rename(castaway_id=vote_id)
      votesreceived <- votesreceivedinoneseason %>%
                        group_by(castaway_id) %>%
                        summarise(votesreceived=sum(votesreceivedoneseason)) 
      
    ## number of votes nullified
      votesnullifiedoneseason <- votehx %>% 
                                  filter(nullified == TRUE) %>%
                                  select(season,vote_id) %>%
                                  group_by(season,vote_id) %>%
                                  mutate(count=1) %>%
                                  summarize(votesnullifiedoneseason=sum(count)) %>%
                                  rename(castaway_id=vote_id)  
      votesnullified <- votesnullifiedoneseason %>%
                          group_by(castaway_id) %>%
                          summarize(votesnullified=sum(votesnullifiedoneseason))
      
    ## number of votes cast per season
      votescast <- votehx %>% 
                    filter(!(vote %in% c("-","Black Rock","countback","Do or Die","Eliminated","Exiled","Immune","Kidnap","Lose","None","Purple Rock","Saved","Shot in the dark","White Rock","Yellow Rock"))) %>%
                    select(season,castaway_id) %>%            
                    mutate(count=1) %>%
                    group_by(season,castaway_id) %>%
                    summarize(votescast=sum(count))
      
    ## number of accurate votes cast per season  
      accuratevotescast <- votehx %>% 
                            filter(!(vote %in% c("-","Black Rock","countback","Do or Die","Eliminated","Exiled","Immune","Kidnap","Lose","None","Purple Rock","Saved","Shot in the dark","White Rock","Yellow Rock"))) %>%
                            select(season,castaway_id,vote_id,voted_out_id) %>%    
                            filter(vote_id == voted_out_id) %>%
                            mutate(count=1) %>%
                            group_by(season,castaway_id) %>%
                            summarize(accuratevotescast=sum(count))      
      
      
  tribalcouncilindivsuper <- full_join(
                              full_join(
                                full_join(
                                  full_join(
                                    full_join(
                                      full_join(premerge,votesreceivedinoneseason),votesreceived),
                                       votesnullifiedoneseason),votesnullified),
                                        votescast),accuratevotescast) %>%
                              group_by(season,castaway_id) %>%
                              mutate(percentaccuracy=accuratevotescast/votescast)
    
## Idol information  
  ## number of idols played
  
## confessionals
  indivconftotal <- confessionals %>% group_by(season,castaway_id) %>%
                            summarize(confessionaltotal=sum(confessional_count))
  
  avgperepi <- confessionals %>% group_by(season,castaway_id) %>%
                    summarize(appearin=max(episode)) %>%
                    right_join(indivconftotal) %>%
                    mutate(averageperepisode=confessionaltotal/appearin) %>%
                    select(season,castaway_id,averageperepisode)
  
## Overall information
      ## number of times played
        timesplayed<- castaways %>% select(season,castaway_id) %>% 
                        distinct() %>%
                        group_by(castaway_id) %>%
                        summarize(numberoftimesplayed=n_distinct(season))
      ## number of days played
        daysplayed <- castaways %>% select(season,castaway_id,day) %>% 
                      group_by(season,castaway_id) %>%
                      summarize(maxdayinseason=max(day)) %>%
                      group_by(castaway_id) %>%
                      summarize(totaldaysoverall=sum(maxdayinseason))

## Bring data together  
      individualsuperlatives <- full_join(full_join(full_join(
                                  full_join(
                                    full_join(individualchallsuperlatives,tribalcouncilindivsuper),
                                        timesplayed),daysplayed),
                                        indivconftotal),avgperepi) 
    # Have things be 0 instead of NA
      for (var in 3:dim(individualsuperlatives)[2]) {
        individualsuperlatives[is.na(individualsuperlatives[,var]),var] <- 0
      }
      individualsuperlatives <- individualsuperlatives %>% pivot_longer(!(c("season","castaway_id")),names_to="variable",values_to="value")
      
    # Add in additional variables
    # Add marker for it being US survivor
      finaldata <- castaways %>% group_by(season,castaway_id) %>%
                            filter(day==max(day)) %>% 
                            select(season,season_name,castaway_id,full_name,castaway,age,order,result,jury_status) %>%
                            full_join(individualsuperlatives) %>%
                            full_join(castaway_details %>% select(castaway_id,gender,poc)) %>%
                            mutate(season=paste("US0",as.character(season),sep=""))
      
      finaldata$season[finaldata$season %in% c("US01","US02","US03","US04","US05","US06","US07","US08","US09")] <- gsub("US0","US00",finaldata$season[finaldata$season %in% c("US01","US02","US03","US04","US05","US06","US07","US08","US09")])
    # Add marker as to if it is a season-specific statistic or cross-season
      finaldata$superlativetype <- "Individual by season"
      finaldata$superlativetype[finaldata$variable %in% c("immwins","numberoftimesplayed","rewwins","totaldaysoverall","tribalimmwins","tribalrewwins","votesreceived","votesnullified")] <- "Individual across seasons"
      
      
      
# save data
write.csv(finaldata,paste(savedir,"survivoR_supeRlatives.csv",sep=""),row.names=F)
