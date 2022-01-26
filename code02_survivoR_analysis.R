## Carly Levitz
## Written 2022-01-09
## Updated 2022-01-22
## Purpose: get season-specific information

rm(list=ls())
library(tidyverse); library(survivoR); library(ggpubr)
savedir <- "H:/R/survivoR/02_cleaned_data/"

tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
  dayofmerge <- unique(tribemap[tribemap$tribe_status == "Merged",c("season_name","season","day")])
  for (sn in seq(1,max(dayofmerge$season),1)) {
    
    dayofmerge$merge[dayofmerge$season == sn] <- min(dayofmerge$day[dayofmerge$season == sn])
  }
  dayofmerge <- dayofmerge[dayofmerge$day == dayofmerge$merge,]
  
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
  confessionals <- confessionals[,c("season_name","season","episode","castaway_id","confessional_count")]
castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
votehx <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)

###################
## Color schemes
  # Greens
  greens <- c("#9FB8A2","#3AA845","#305844","#2A3B32","#101C16")
  reds <- c("#F1E48A","#F58406","#932306","#4B0A06","#060304")

#########################################################################################
## INformation about Season-specific phases
## By phase of game (original, swapped, swapped2, merge)
## number of days in that phase - season name, season, tribe status, max day, min day
  seasonphasedays <- data.frame(matrix(ncol = 5, nrow = 0))
  names(seasonphasedays) <- c("season_name","season","tribe_status","max_day","min_day")
  
  for (season in 1:41) {
    sn <- unique(tribemap$season_name[tribemap$season == season])
    for (ts in c(unique(tribemap$tribe_status[tribemap$season == season]))) {
      
      tempmax <- max(tribemap$day[tribemap$season == season & tribemap$tribe_status == ts])
      tempmin <- min(tribemap$day[tribemap$season == season & tribemap$tribe_status == ts])
      
      seasonphasedays <- rbind(seasonphasedays,c(sn,season,ts,tempmax,tempmin))
    }
    
  }
  names(seasonphasedays) <- c("season_name","season","tribe_status","max_day","min_day")
  seasonphasedays$max_day <- as.numeric(seasonphasedays$max_day)
  seasonphasedays$min_day <- as.numeric(seasonphasedays$min_day)
  seasonphasedays$length <- seasonphasedays$max_day - seasonphasedays$min_day + 1

## number of players
  seasonphaseplayers <- unique(tribemap[,c("season_name","season","castaway_id","tribe_status")])
  seasonphaseplayers$count <- 1
  seasonphaseplayers <- aggregate(seasonphaseplayers$count, 
                                  by=list(seasonphaseplayers$season_name,seasonphaseplayers$season,seasonphaseplayers$tribe_status),
                                  FUN=sum, na.rm=T)
  names(seasonphaseplayers) <- c("season_name","season","tribe_status","number_players")
  
## number of reward challenges
## number of immunity challenges
## Need to merge on information about tribe status
  challengedays <- unique(challenges[,c("season_name","season","day","challenge_type","outcome_type")])
  tribedays <- unique(tribemap[,c("season_name","season","day","tribe_status")])
  
  seasonchall <- left_join(challengedays, tribedays, by=c("season_name","season","day"))
  seasonchall$count <- 1
  seasonchall <- aggregate(seasonchall$count,
                           by=list(seasonchall$season_name,seasonchall$season,seasonchall$challenge_type,seasonchall$tribe_status),
                           FUN=sum, na.rm=T)
  names(seasonchall) <- c("season_name","season","challenge_type","tribe_status","numberchallenges")
  seasonchall$tribe_status <- gsub(" ","",seasonchall$tribe_status)
  seasonchall <- reshape(seasonchall,
                         direction = "wide",
                         timevar = "challenge_type",
                         idvar = c("season_name","season","tribe_status"))
  
# Bring together the information, then format it long for tableau
  seasonphasedays$season <- as.numeric(seasonphasedays$season)
  seasoninfo <- left_join(seasonphasedays,seasonphaseplayers,by=c("season_name","season","tribe_status"))
  seasoninfo <- left_join(seasoninfo,seasonchall,by=c("season_name","season","tribe_status"))
  
  seasoninfo <- reshape(seasoninfo, direction = "long", 
                        varying = c("max_day","min_day","length","number_players","numberchallenges.Duel","numberchallenges.Immunity","numberchallenges.Reward","numberchallenges.Reward and Immunity"), 
                        v.names = "value",
                        timevar = "variable", 
                        times = c("max_day","min_day","length","number_players","numberchallenges.Duel","numberchallenges.Immunity","numberchallenges.Reward","numberchallenges.Reward and Immunity"))
  seasoninfo$id <- row.names(seasoninfo) <- NULL
  
  seasoninfo$value[is.na(seasoninfo$value)] <- 0
  
  write.csv(seasoninfo,paste(savedir,"survivor_phaseinfo_A_season_info.csv",sep=""),row.names=F)
  
########################################################################################################
## Season-tribe-phase specific information
  #TRIBE NAME
  #X rewards won
  #X immunities won
    tribedays2 <- unique(tribemap[,c("season_name","season","day","tribe_status","tribe")])
    challengetribedays <- unique(challenges[,c("season_name","season","day","challenge_type","outcome_type","winning_tribe")])
    seasontribechall <- left_join(challengetribedays, tribedays2, by=c("season_name","season","day"))
    seasontribechall$winning_tribe[is.na(seasontribechall$winning_tribe)] <-  seasontribechall$tribe[is.na(seasontribechall$winning_tribe)] 
    seasontribechall$winning_tribe[seasontribechall$winning_tribe == "(Individual challenge)"] <- seasontribechall$tribe[seasontribechall$winning_tribe == "(Individual challenge)"]
    seasontribechall$tribe <- NULL
    
    seasontribechall$count <- 1
    seasontribechall <- aggregate(seasontribechall$count,
                             by=list(seasontribechall$season_name,seasontribechall$season,seasontribechall$challenge_type,seasontribechall$tribe_status,seasontribechall$winning_tribe),
                             FUN=sum, na.rm=T)
    names(seasontribechall) <- c("season_name","season","challenge_type","tribe_status","tribe","numberchallenges")
    seasontribechall <- reshape(seasontribechall,
                                direction = "wide",
                                timevar="challenge_type",
                                idvar = c("season_name","season","tribe_status","tribe"))
    
    
  #X Tribal Councils attended
  #Average confessionals per Episode
    tribeepidays <- unique(tribemap[,c("season_name","season","episode","day","tribe","tribe_status")])
    confessionaltribestatus1 <- left_join(confessionals,tribeepidays,by=c("season_name","season","episode"))
    
        # Total by tribe & tribe status
          confessionaltribestatus2 <- aggregate(confessionaltribestatus1$confessional_count,
                                                by=list(confessionaltribestatus1$season_name,confessionaltribestatus1$season,confessionaltribestatus1$tribe_status,confessionaltribestatus1$tribe),
                                                FUN=sum,na.rm=T)
          names(confessionaltribestatus2) <- c("season_name","season","tribe_status","tribe","confessional_total")
      
        # Median by tribe & tribe status
          # first need to get the total for each person while on each tribe & tribe status
              confessionaltribestatus3 <- aggregate(confessionaltribestatus1$confessional_count,
                                                    by=list(confessionaltribestatus1$season_name,confessionaltribestatus1$season,confessionaltribestatus1$castaway_id,confessionaltribestatus1$tribe_status,confessionaltribestatus1$tribe),
                                                    FUN=sum,na.rm=T)    
              names(confessionaltribestatus3) <- c("season_name","season","castaway_id","tribe_status","tribe","confessional_sumbyperson")
          # then get the median of that
              confessionaltribestatus3 <- aggregate(confessionaltribestatus3$confessional_sumbyperson,
                                                    by=list(confessionaltribestatus3$season_name,confessionaltribestatus3$season,confessionaltribestatus3$tribe_status,confessionaltribestatus3$tribe),
                                                    FUN=median,na.rm=T) 
              names(confessionaltribestatus3) <- c("season_name","season","tribe_status","tribe","confessional_median")
        
        # bring together
            confessionaltribestatus <- left_join(confessionaltribestatus2,confessionaltribestatus3,
                                           by=c("season_name","season","tribe_status","tribe"))
            
  tribespecific <- left_join(seasontribechall,confessionaltribestatus,by=c("season_name","season","tribe_status","tribe"))
  tribespecific <- reshape(tribespecific, direction = "long", 
                           varying = c("confessional_total","confessional_median","numberchallenges.Duel","numberchallenges.Immunity","numberchallenges.Reward","numberchallenges.Reward and Immunity"), 
                           v.names = "value",
                           timevar = "variable", 
                           times = c("confessional_total","confessional_median","numberchallenges.Duel","numberchallenges.Immunity","numberchallenges.Reward","numberchallenges.Reward and Immunity"))
  row.names(tribespecific) <- tribespecific$id <- NULL

  
  #X idols found; Y idols played
  #Strongest voting alliance (% of times voted together)
  
  tribespecific <- tribespecific[!(is.na(tribespecific$value)),]
  write.csv(tribespecific,paste(savedir,"survivor_phaseinfo_B_tribe_info.csv",sep=""),row.names=F)
  

########################################################################################################
## How far do the people who won the FIRST indiv imm after the merge go?
########################################################################################################
    firstii <- challenges[challenges$outcome_type == "Individual" & challenges$challenge_type %in% c("Immunity","Reward and Immunity") & challenges$winning_tribe == "(Individual challenge)",]
    # Season 41 is a little weird, just manually put that the merge was day 13
    firstii$day[firstii$season == 41 & firstii$day == 14] <- 13
    #season 14, too
    firstii$day[firstii$season == 14 & firstii$day == 27] <- 24
    
  # What was the earliest (first) individual immunity?
  firstii <- left_join(firstii,dayofmerge,by=c("season_name","season","day"))
  firstii <- firstii[!(is.na(firstii$merge)),c("season_name","season","winner","winner_id","day")]
  names(firstii) <- c("season_name","season","castaway","castaway_id","dayofchallenge")
  castawayslimited <- castaways[,c("season_name","season","castaway","castaway_id","day","order","result")]
  firstii <- left_join(firstii,castawayslimited,by=c("season_name","season","castaway","castaway_id"))
  
    # revert the challenge days
    firstii$dayofchallenge[firstii$season == 41 & firstii$dayofchallenge == 13] <- 14
    firstii$dayofchallenge[firstii$season == 14 & firstii$dayofchallenge == 24] <- 27
    
  firstii$dayslasted <- firstii$day-firstii$dayofchallenge  
  firstii <- firstii[firstii$dayslasted > 0,]  
  
  # want to add the # of players in the game
  numberofcastaways <- unique(castawayslimited[,c("season","castaway_id")])
  numberofcastaways$count <- 1
  numberofcastaways <- aggregate(numberofcastaways$count,by=list(numberofcastaways$season),FUN=sum,na.rm=T)
  names(numberofcastaways) <- c("season","numberofplayers")
  firstii <- left_join(firstii,numberofcastaways,by="season")
  
    
write.csv(firstii,paste(savedir,"survivor_firstImmChallWinner.csv",sep=""),row.names=F)
    

########################################################################################################
## How much of a hot mess are the first TCs of a tribe?
## I.e., how many castaways & what % of the tribe receive votes?
########################################################################################################
  ## want to know what NUMBER of the TC is
    for (sn in 1:max(votehx$season)) {
      tempcount <- 1
      for (d in sort(unique(votehx$day[votehx$season == sn]))) {
        votehx$TCorder[votehx$season == sn & votehx$day == d] <- paste("TC",tempcount,sep="")
        tempcount <- tempcount+1
      }
      
    }
    
  ## Need to merge on tribe name information
    hotmess <- votehx
    hotmess <- left_join(hotmess[,c("season_name","season","episode","day","tribe_status","castaway","castaway_id","vote","vote_id","TCorder")],
                         tribemap[,c("season_name","season","episode","day","tribe_status","tribe","castaway","castaway_id")],
                         by=c("season_name","season","episode","day","tribe_status","castaway","castaway_id"))
    
  # How many people from that tribe were at TC on that given day? how many received votes?
    for (sn in 1:max(hotmess$season)) {
      
      for (d in c(unique(hotmess$day[hotmess$season == sn]))) {
        
        for (tr in c(unique(hotmess$tribe[hotmess$season == sn & hotmess$day == d]))) {
          # number of attendees
          numberofattendees <- nrow(unique(hotmess[hotmess$season == sn & hotmess$day == d & hotmess$tribe == tr,c("season","day","tribe","castaway")]))
          hotmess$numberattendees[hotmess$season == sn & hotmess$day == d & hotmess$tribe == tr] <- numberofattendees
          # Get rid of rocks, ties, etc. by only looking at those with a vote id
          numberofpplreceivingvotes <- length(unique(hotmess$vote_id[hotmess$season == sn & hotmess$day == d & hotmess$tribe == tr & !(is.na(hotmess$vote_id))]))
          hotmess$numberreceivingvotes[hotmess$season == sn & hotmess$day == d & hotmess$tribe == tr] <- numberofpplreceivingvotes
        }
      }
    }
    
  # keep just the necessary information
    hotmess <- unique(hotmess[,c("season_name","season","day","TCorder","tribe_status","tribe","numberattendees","numberreceivingvotes")])
    
    # Keep just the first time a tribe went to TC
    for (sn in 1:max(hotmess$season)) {
      for (tr in c(unique(hotmess$tribe[hotmess$season == sn]))) {
        
        hotmess$minday[hotmess$season == sn & hotmess$tribe == tr] <- min(hotmess$day[hotmess$season == sn & hotmess$tribe == tr])
      }
      
    }
    hotmess <- hotmess[hotmess$day == hotmess$minday,]
    hotmess$minday <- NULL
    
  # create the variable 
    hotmess$hotmess <- hotmess$numberreceivingvotes/hotmess$numberattendees

write.csv(hotmess,paste(savedir,"survivor_HotmessOfAFirstTribal.csv",sep=""),row.names=F)
    
hotmess$tribeseason <- paste(hotmess$tribe,hotmess$season_name,sep="-")


########################################################################################################
## Tribe swaps: if there is a majority of one of the older tribes on a new tribe and they go to TC, 
## what is the old tribe of the person voted off?
########################################################################################################
  ## Looking at swapped tribes - what are the make-ups of them in terms of the original tribes
  ## For now, just looking at FIRST swap, since there are so few second swaps
    swapped <- unique(castaways[!(is.na(castaways$swapped_tribe)),c("season_name","season","castaway","castaway_id","original_tribe","swapped_tribe")])
    swapped$count <- 1
    
    # number of castaways from each original tribe that are on a swapped tribe
        swappedOrigSwapCount <- aggregate(swapped$count,
                             by=list(swapped$season_name,swapped$season,swapped$original_tribe,swapped$swapped_tribe),
                             FUN=sum,
                             na.rm=T)
        row.names(swappedOrigSwapCount) <- swappedOrigSwapCount$id <- NULL
        names(swappedOrigSwapCount) <- c("season_name","season","original_tribe","swapped_tribe","numberFromOrig")
        
    ## number of people involved in swap
        swappedInvolved <- aggregate(swapped$count,
                                     by=list(swapped$season_name,swapped$season),
                                     FUN=sum,
                                     na.rm=T)
        row.names(swappedInvolved) <- swappedInvolved$id <- NULL
        names(swappedInvolved) <- c("season_name","season","numberAtSwap")
    
    ## number of people in swapped tribe
        swappedToATribe <- aggregate(swapped$count,
                                     by=list(swapped$season_name,swapped$season,swapped$swapped_tribe),
                                     FUN=sum,
                                     na.rm=T)
        row.names(swappedToATribe) <- swappedToATribe$id <- NULL
        names(swappedToATribe) <- c("season_name","season","swapped_tribe","numberInTribe")        

    ## Bring Swap Summary data together
        swapdata <- merge(swappedOrigSwapCount,swappedInvolved,
                          by=c("season_name","season"),
                          all.x=T,all.y=T)
        swapdata <- merge(swapdata,swappedToATribe,
                          by=c("season_name","season","swapped_tribe"),
                          all.x=T,all.y=T)
        swapdata <- swapdata[order(swapdata$season,swapdata$swapped_tribe,swapdata$original_tribe),]
        swapdata$majority <- 0
        swapdata$majority[swapdata$numberFromOrig/swapdata$numberInTribe > .5] <- 1

  ## Voting history during swap
    ## Day of swap & day swap ends
        dayofswap <- unique(tribemap[tribemap$tribe_status == "Swapped",c("season_name","season","day","tribe")])
        dayofswap$dayswapstarts <- dayofswap$dayswapends <- 0
        
        for (sn in c(unique(dayofswap$season))) {
          
          dayofswap$dayswapstarts[dayofswap$day == min(dayofswap$day[dayofswap$season == sn]) & dayofswap$season == sn] <- 1
          dayofswap$dayswapends[dayofswap$day == max(dayofswap$day[dayofswap$season == sn]) & dayofswap$season == sn] <- 1
          
          dayofswap$SwapStart[dayofswap$season == sn] <- unique(dayofswap$day[dayofswap$dayswapstarts == 1 & dayofswap$season == sn])
          dayofswap$SwapEnd[dayofswap$season == sn]      <- unique(dayofswap$day[dayofswap$dayswapends == 1 & dayofswap$season == sn])
        }
        
        dayofswap <- unique(dayofswap[dayofswap$dayswapstarts == 1 | dayofswap$dayswapends == 1,c("season_name","season","day","dayswapends","dayswapstarts","SwapStart","SwapEnd")])
        

    ## Who was voted out during the time of that swap
        # what are all the days during the swap (not just start and end)
        # need a dataset that is season + each day that exists during the swap; regardless of Tribe
          specificdays <- data.frame(matrix(ncol = 2, nrow = 0))
          tempswap <- unique(dayofswap[,c("season","SwapStart","SwapEnd")])
          for (sn in unique(tempswap$season)) {
            days <- seq(tempswap$SwapStart[tempswap$season == sn],tempswap$SwapEnd[tempswap$season == sn],1)
            numberofdays <- length(days)
            
            specificdays <- rbind(specificdays,cbind(rep(sn,numberofdays),days))
            
          }
          names(specificdays) <- c("season","day")
          
        
        ## who are the people who were voted out on those days that the swap existed
        swapvotes <- left_join(specificdays,
                               unique(votehx[,c("season_name","season","day","voted_out","voted_out_id","tribe")]),
                               by=c("season","day"))
        swapvotes <- swapvotes[!(is.na(swapvotes$voted_out_id)),]
        names(swapvotes) <- c("season","day","season_name","castaway","castaway_id","swapped_tribe")
        
        ## what were their ORIGINAL tribes?
        swapvotes <- left_join(swapvotes,
                               castaways[,c("season_name","season","castaway","castaway_id","day","original_tribe")],
                               by=c("season_name","season","castaway","castaway_id","day"))
        names(swapvotes) <- c("season","dayOfTC","season_name","voted_off","voted_off_id","swapped_tribe","voted_off_firsts_original_tribe")
        
        ## Keep just the FIRST person voted off
        for (sn in unique(swapvotes$season)) {
          minday <- min(swapvotes$dayOfTC[swapvotes$season == sn])
          swapvotes$keep[swapvotes$season == sn & swapvotes$dayOfTC == minday] <- 1
        }
        swapvotes <- swapvotes[swapvotes$keep == 1 & !(is.na(swapvotes$keep)),c("season_name","season","swapped_tribe","voted_off_firsts_original_tribe")]
    
    ## Bring vote data together with swap summary info      
    swappy <- left_join(swapdata,
                        swapvotes,
                        by=c("season_name","season","swapped_tribe"))
    swappy <- swappy[,c("season_name","season","original_tribe","swapped_tribe","numberFromOrig","numberInTribe","numberAtSwap","majority","voted_off_firsts_original_tribe")]

      ## keep just those that went to tribal council
        swappy$wentToTribal <- 0
        swappy$wentToTribal[!(is.na(swappy$voted_off_firsts_original_tribe))] <- 1
    
      ## was the person who was voted off in the tribe that had the majority in that swapped tribe
        swappy$majoritycannibalized <- 0
        swappy$majoritycannibalized[swappy$voted_off_firsts_original_tribe == swappy$original_tribe & swappy$majority == 1] <- 1
        
      
        
write.csv(swappy,paste(savedir,"survivor_SwappedTribeMajorityAndVoteOffs.csv",sep=""),row.names=F)    
        
############################################################################################################################
## Superlatives: number of immunity wins
challsupPrep <- unique(challenges[,c("season","winner","winner_id","challenge_type","outcome_status","outcome_type","day")])
  # was the challenge pre or post merge?
  for (sn in 1:max(challsupPrep$season)) {
    mergeday <- dayofmerge[dayofmerge$season == sn,"day"]
    challsupPrep$mergestatus[challsupPrep$season == sn] <- "post-merge"
    challsupPrep$mergestatus[challsupPrep$season == sn & challsupPrep$day < mergeday] <- "pre-merge"
  }
  
challsupPrep <- challsupPrep[grepl("mmunity",challsupPrep$challenge_type) & challsupPrep$outcome_status == "Winner" & challsupPrep$outcome_type == "Individual",
                      c("season","winner","winner_id","mergestatus")]

  # Number of wins per season
    challsupPrep$count <- 1
    challsup <- aggregate(challsupPrep$count,by=list(challsupPrep$season,challsupPrep$winner,challsupPrep$winner_id,challsupPrep$mergestatus),FUN=sum,na.rm=T)
    names(challsup) <- c("season","winner","winner_id","mergestatus","numberofwinsbyseason")
    
  # Number of wins across seasons  
    challsupcrossseason <- aggregate(challsup$numberofwinsbyseason,by=list(challsup$winner,challsup$winner_id,challsup$mergestatus),FUN=sum,na.rm=T)
    names(challsupcrossseason) <- c("winner","winner_id","mergestatus","numberofwinsacrossseasons")
    challsupcrossseason <- reshape(challsupcrossseason,direction="wide",idvar=c("winner","winner_id"),timevar="mergestatus")
    challsupcrossseason[is.na(challsupcrossseason[,"numberofwinsacrossseasons.pre-merge"]),"numberofwinsacrossseasons.pre-merge"] <- 0
    challsupcrossseason$numberofwinsacrossseasons <- challsupcrossseason[,"numberofwinsacrossseasons.post-merge"]+challsupcrossseason[,"numberofwinsacrossseasons.pre-merge"]
    
  # Number of seasons in which they won immunity
    temp <- unique(challsup[,c("season","winner","winner_id")])
    temp$count <- 1
    challsupseasons <- aggregate(temp$count,by=list(temp$winner,temp$winner_id),FUN=sum,na.rm=T)
    names(challsupseasons)<-c("winner","winner_id","numberofseasonshadawin")
    
  # Number of seasons in which they PLAYED
    seasonsplayed <- unique(castaways[,c("castaway_id","season")])
    names(seasonsplayed) <- c("winner_id","season")
    seasonsplayed$count <- 1
    seasonsplayed <- aggregate(seasonsplayed$count,by=list(seasonsplayed$winner_id),FUN=sum,na.rm=T)
    names(seasonsplayed) <- c("winner_id","numberofseasonsplayed")
    
  # bring data together
    challsuperlatives <- merge(challsupcrossseason,challsupseasons,by=c("winner","winner_id"),all.x=T,all.y=T)
    challsuperlatives <- merge(challsuperlatives,seasonsplayed,by=c("winner_id"),all.x=T,all.y=F)

    challsuperlatives <- challsuperlatives[order(challsuperlatives$numberofwinsacrossseasons,decreasing=T),]
    challsuperlatives$numberofseasonsplayed <- as.character(challsuperlatives$numberofseasonsplayed)
    names(challsuperlatives)[names(challsuperlatives) %in% c("numberofwinsacrossseasons.post-merge","numberofwinsacrossseasons.pre-merge")] <- c("numberofwinsacrossseasonspostmerge","numberofwinsacrossseasonspremerge")
    
## Graph it    
    # Select data for plot
    data <- challsuperlatives[challsuperlatives$numberofwinsacrossseasons >=3 & !(is.na(challsuperlatives$numberofwinsacrossseasons)),] 
    # Re-order from most to least wins
    data$winner <- factor(data$winner,levels=unique(data$winner[order(data$numberofwinsacrossseasons,data$numberofseasonsplayed,decreasing=F)]))
    
    theme_set(theme_classic())
      # Main plot  
      immunitywinsgraph <- 
        data %>%
        ggplot(aes(x=numberofwinsacrossseasons,
                   y=winner,
                   fill=numberofseasonsplayed)) + 
          geom_bar(stat="identity") +

        # add borders around bars 
          geom_bar(stat="identity",color="gray20") +
        # add which were pre-merge (but don't have the colors be outlined in white in the legend)
          geom_bar(stat="identity",aes(x=numberofwinsacrossseasonspremerge,y=winner),color="white", show.legend=FALSE) + 
        # color scale
          scale_fill_manual(values=greens) + 
        # Theme elements
          theme(plot.background = element_rect(fill = "#C6D09E"),
                panel.background = element_rect(fill = "#C6D09E"),
                axis.title.y= element_text(angle = 0,color="black",face="bold"),
                axis.text.y = element_text(size=10,color="black"),
                axis.text.x = element_text(color="black"),
                legend.background = element_rect(fill="#C6D09E"),
                legend.title = element_text(face="bold"),
                legend.position = c(.8,.4),
                #legend.key = element_rect(color="black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
                ) + 
      # X axis
          scale_x_continuous("Number of individual immunity wins across all seasons",
                             breaks=seq(1,10,1),
                             labels=seq(1,10,1),expand=c(0,0)) +
      # Y axis, title, and legend title
          labs(y="",
               title="Castaways with the most individual immunity wins by the number of seasons they played",
               subtitle = "Wins outlined in white indicate they were wins pre-merge",
               fill = "Number of seasons\nplayed") 
        
    # Add another plot for number of seasons played vs number won in 
       numberofseasonsgraph <- 
            ggplot(data, aes(x=numberofseasonshadawin, y=winner,fill=numberofseasonsplayed))+
              geom_bar(stat='identity')+
              geom_bar(stat='identity',color="gray20")+
              scale_fill_manual(values=greens) +
              theme(plot.background = element_rect(fill = "#C6D09E"),
                    panel.background = element_rect(fill = "#C6D09E"),
                    axis.text.x = element_text(color="black"),
                    axis.text.y = element_text(color="black"),
                    axis.title.y = element_text(color="black",angle=0),
                    #legend.background = element_rect(fill="#C6D09E"),
                    #legend.title = element_text(face="bold")
                    legend.background=element_blank(),
                    legend.title=element_blank(),
                    legend.position="none"
                    ) +
              labs(x="# seasons in which player won individual immunity",
                   y="",
                   subtitle="Of these players, in how many seasons\ndid they win individual immunity?",
                   #fill = "Number of seasons played"
                   )+
              scale_x_continuous(expand=c(0,0))

      # Proportion of seasons played in which they won individual immunity.
        proportion <- data[,c("winner_id","winner","numberofseasonshadawin","numberofseasonsplayed")]
        proportion$numberofseasonswithoutawin <- (as.numeric(proportion$numberofseasonsplayed)-proportion$numberofseasonshadawin)/as.numeric(proportion$numberofseasonsplayed)
        proportion$numberofseasonshadawin <- proportion$numberofseasonshadawin/as.numeric(proportion$numberofseasonsplayed)
        proportion$numberofseasonsplayed <- NULL
        proportion <- reshape(proportion,
                              direction="long",
                              varying=c("numberofseasonshadawin","numberofseasonswithoutawin"),
                              timevar="variable",
                              v.names="value",
                              times=c("numberofseasonshadawin","numberofseasonswithoutawin"))
        proportion$id <- row.names(proportion) <- NULL
        proportion$variable[proportion$variable == "numberofseasonshadawin"] <- "% of Seasons\nwith a win"
        proportion$variable[proportion$variable == "numberofseasonswithoutawin"] <- "% of Seasons\nwithout a win"
        proportion$winner <- factor(proportion$winner,levels=unique(data$winner[order(data$numberofwinsacrossseasons,decreasing=T)]))
        proportion$variable <- factor(proportion$variable, levels=c("% of Seasons\nwithout a win","% of Seasons\nwith a win"))
        
        propgraphall <-
          ggplot(proportion,aes(x="",y=value,fill=variable)) + 
          facet_wrap(winner ~ .,ncol=3,strip.position = "left")+
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y") +
          scale_fill_manual(values=reds[c(1,2)]) + 
          theme(
            plot.background = element_rect(fill = "#C6D09E"),
            panel.background = element_rect(fill = "#C6D09E"),
            strip.text = element_text(size=7,angle=0),
            strip.background = element_blank(),
            strip.placement = "outside",
            axis.text = element_blank(),
            legend.background = element_rect(fill="#C6D09E"),
            legend.title = element_blank(),
            legend.text = element_text(size=8),
            legend.position = "top",
            legend.key.size = unit(4,"mm"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.margin=unit(c(1,1,1,1), "cm")
          ) +
          labs(x="",y="",title="",
               subtitle="See left for # of seasons played") 
          
        
      # Acknowledgments            
            text <- paste("Data: github doehm/survivoR (@danoehm) / Graphic @carlylevitz / More viz at https://public.tableau.com/app/profile/carly.levitz/viz/SurvivorCBSData-Acknowledgements/Acknkowledgements")
            text.p <- ggparagraph(text = text, size = 11, color = "black")
   # Bring figures together         
       # first version
           ggarrange( 
              ggarrange(immunitywinsgraph,numberofseasonsgraph,
                        nrow = 1,ncol=2,widths=c(2,1)),
              text.p,
           nrow=2,ncol=1,heights=c(13,1))
            
          ggsave(paste(savedir,"CastawaySuperlativesIndividualImmunity1.png",sep=""),width=14,height=8)
      # second version
        ggarrange(
          ggarrange(immunitywinsgraph,propgraphall,widths=c(2,1)),
          text.p,
          nrow=2,ncol=1,heights=c(13,1)
        )
        ggsave(paste(savedir,"CastawaySuperlativesIndividualImmunity2.png",sep=""),width=14,height=8)
    
        
      ggarrange(immunitywinsgraph,text.p,nrow=2,ncol=1,heights=c(13,1))  
      ggsave(paste(savedir,"CastawaySuperlativesIndividualImmunity3.png",sep=""),width=14,height=8)
        
        
        