## Carly Levitz
## Written 2022-01-09
## Updated 2022-01-22
## Purpose: get season-specific information

rm(list=ls())
library(tidyverse); library(survivoR); library(ggpubr)
savedir <- "H:/R/survivoR/02_cleaned_data/"

###################
## Color schemes
  
  greens <- c("#9FB8A2","#3AA845","#305844","#2A3B32","#101C16")
  reds <- c("#F1E48A","#F58406","#932306","#4B0A06","#060304")

#######################################
## Bring in the data
    tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
      dayofmerge <- unique(tribemap[tribemap$tribe_status == "Merged",c("season_name","season","day")])
      for (sn in seq(1,max(dayofmerge$season),1)) {
        
        dayofmerge$merge[dayofmerge$season == sn] <- min(dayofmerge$day[dayofmerge$season == sn])
      }
      dayofmerge <- dayofmerge[dayofmerge$day == dayofmerge$merge,]
      
    challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
    castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
    
#######################################
## Data cleaning
    ## dataset of just challenge id and challenge characteristics
        characteristics <- unique(challenges[,c(9,18:29)])
    
    ## For each season-day-challenge - # of winners by gender
        winnerdetail <- challenges %>%
          group_by(season,day,challenge_name,challenge_type,outcome_type,challenge_id,winner_gender) %>%
          summarise(genderwinner = length(unique(winner_id)))     
        
    ## For each season-day-outcometype-challengetype-challenge - # of who participated by gender
        participationdetail <- tribemap %>%
          group_by(season,day,gender) %>%
          summarise(genderparticipation = length(unique(castaway_id)))
        names(participationdetail)[names(participationdetail) =="gender"] <- "winner_gender"
      
        ## Will need to drop the days when there weren't challenges     
        winnerdetail <- left_join(winnerdetail,participationdetail,by=c("season","day","winner_gender"))
        
        ## drop the auctions
        winnerdetail <- winnerdetail %>% drop_na(winner_gender)
        
        ## there are four challenges for which we don't have participation data; dropping them for now
        winnerdetail <- winnerdetail %>% drop_na(genderparticipation)

    ## Simplify the dataset
        winnerdetail <- winnerdetail %>%
                          group_by(challenge_name,challenge_type,outcome_type,challenge_id,winner_gender) %>%
                          summarise(winnercount=sum(genderwinner),participationcount=sum(genderparticipation))
        winnerdetail$ratio <- winnerdetail$winnercount/winnerdetail$participationcount
        winnerdetail$challengecategory <- paste(winnerdetail$outcome_type,winnerdetail$challenge_type,sep=" ")
        winnerdetail$outcome_type <- winnerdetail$challenge_type <- NULL
        winnerdetail <- winnerdetail %>% filter(challengecategory!="Team Reward and Immunity")
        winnerdetail$challengecategory[winnerdetail$challengecategory == "Tribal Reward and Immunity"] <- "Tribal Reward\nand Immunity"
        winnerdetail$challengecategory[winnerdetail$challengecategory == "Individual Reward and Immunity"] <- "Individual Reward\nand Immunity"
        
        ## what do we know about the challenges?
        winnerdetail <- left_join(winnerdetail,characteristics,by="challenge_id")

   ## challenge characteristics - y
       winnerchall <- winnerdetail %>% gather(key="characteristic",value="value",puzzle:water)
        winnerchall <- winnerchall %>% filter(value == TRUE)
        winnerchall$winner_gender[winnerchall$winner_gender == "Non-binary"] <- "Non-\nbinary"
          
        t.test(winnerchall$ratio[winnerchall$winner_gender=="Female" & winnerchall$characteristic == "memory"],
               winnerchall$ratio[winnerchall$winner_gender=="Male" & winnerchall$characteristic == "memory"],
               alternative="greater")
        
#######################################
## graphing
## High level: ratio of winners to participants by gender and outcome type
  highlevelallchallenges <-       
    winnerdetail %>%
        ggplot(aes(x=participationcount,y=ratio,color=winner_gender))+
            geom_jitter(aes(fill=winner_gender))+
            facet_grid(.~challengecategory) +
            scale_color_manual(values=c("#6A3A99","#918B76","#007EA7"))+
            labs(
              x="Number of castaways of gender participating",
              y="Ratio of winners:participants",
              title="Ratio of winners to participants by gender for each challenge that has ever been run",
              subtitle="Each challenge is a circle. Challenges that have been run more than once can have a large number of participants",
              color="Gender"
            ) +
            guides(fill="none")
        
## Differences by challenge characteristic
  winnerchall %>%      
  ggplot(aes(x=winner_gender,y=ratio,fill=winner_gender))  +
    geom_boxplot() +
    scale_fill_manual(values=c("#6A3A99","#007EA7","#918B76")) +
    facet_wrap(.~characteristic,nrow=2) +
    theme(
      plot.background = element_rect(fill = greens[1]),
      plot.title = element_text(color="black",size=15),
      plot.subtitle = element_text(color="black",size=12),
      panel.background = element_rect(fill = greens[1]),
      axis.ticks.x = element_blank(),
      axis.text = element_text(color="black",size=12),
      axis.title = element_text(color="black",size=12),
      strip.background = element_rect(fill="black"),
      strip.text = element_text(color="white",size=10)
    ) +
    labs(
      x="",
      y="Ratio of winners:participants",
      title="Ratio of winners to participants by gender and challenge characteristic",
      subtitle="Challenges that have been run multiple times have been aggregated in terms of number of participants and number of winners by gender"
    ) +
    guides(fill="none")


        

