## Author: Carly Levitz
## Date written: 2021-12-09
## Date updated: 2021-12-25
## Purpose: data cleaning of Survivor data  - prep the data from https://github.com/doehm/survivoR and then use these data in Tableau
##    https://public.tableau.com/app/profile/carly.levitz/viz/SurvivorCBSData-Acknowledgements/Acknkowledgements
## 12/18 - added gender and race data to all datasets.
## 12/25/21 - Dan updated the Challenges dataset to be two datasets - results and description. Additionally, the gender data is now in the Castaway dataset
## 12/29/21 - bringing together additional info to each output file, so I don't have to merge them together in Tableau
## 1/2/2022 - Need to modify things because the castaway DETAILS file now has the information on race and ethnicity.
##          - also adding additional details about the castaways to hidden idol and confessional data
## 1/3/2022 - updating comments

rm(list=ls())
library(devtools); library(ggplot2) ; library(tidyverse)
devtools::install_github("doehm/survivoR")
savedir <- "H:/R/survivoR/02_cleaned_data/"

castaways <- survivoR::castaways
castawaydetails <- survivoR::castaway_details
challenges <- survivoR::challenge_results
challengesdesc <- survivoR::challenge_description
vote_history <- survivoR::vote_history
confessionals <- survivoR::confessionals
season_summary <- survivoR::season_summary
hidden_idols <- survivoR::hidden_idols
jury_votes <- survivoR::jury_votes
tribe_mapping <- survivoR::tribe_mapping
viewers <- survivoR::viewers

## Make assumption about race; if it's not listed, then it's white
## This is problematic because it centers whiteness. My data source (survivor wiki fandom) only have pages for Asian, Latin American, and Black. 
## I found a reddit page that had Jewish players. 
## On Natalie Bolton's page, it mentioned that she was Native American, and the page called out 3 or 4 other Native American players.
castawaydetails$race[is.na(castawaydetails$race) | castawaydetails$race == ""] <- "White"

  # edit to gender, race, and ethnicity data
  # Peih-Gee in Season 31 is missing ethnicity data
    castawaydetails$ethnicity[castawaydetails$short_name == "Peih-Gee" & is.na(castawaydetails$ethnicity)] <- "Hong Konger American"
  
  # visually check that all of the people who show up more than once have the same data for 
      #tempcast <- castaways[,c(1:9)]
      #tempcast$countofrows <- NA
      
      #for (cc in 1:max(tempcast$castaway_id)) {
      #  tempcast$countofrows[tempcast$castaway_id == cc] <- nrow(tempcast[tempcast$castaway_id == cc,])
        
      #}
      #tempcast <- tempcast[tempcast$countofrows > 1,]
      #tempcast <- tempcast[order(tempcast$castaway_id),]
      #tempcast  %>% View()
  
## Have Castaway & Full Name consistent across Castaway ID
  ## Castaway dataset
    castaways$castaway[castaways$castaway_id == 9] <- "Jenna L."
    castaways$castaway[castaways$castaway_id == 13] <- "Sue"
    castaways$castaway[castaways$castaway_id == 45] <- "Big Tom"
    castaways$castaway[castaways$castaway_id == 59] <- "The General"
    castaways$castaway[castaways$castaway_id == 94] <- "Rob C."
    castaways$castaway[castaways$castaway_id == 96] <- "Jenna M."
    castaways$castaway[castaways$castaway_id == 111] <- "Jonny Fairplay"
    castaways$castaway[castaways$castaway_id == 118] <- "Bubba"
    castaways$castaway[castaways$castaway_id == 122] <- "Sarge"
    castaways$castaway[castaways$castaway_id == 190] <- "Flicka"
    castaways$castaway[castaways$castaway_id == 206] <- "Papa Smurf"
    castaways$castaway[castaways$castaway_id == 288] <- "Russell S."
    castaways$castaway[castaways$castaway_id == 292] <- "Laura M."
    castaways$castaway[castaways$castaway_id == 300] <- "Russell H."
    castaways$castaway[castaways$castaway_id == 314] <- "Purple Kelly"
    
  ## castaway details
    castawaydetails$short_name[castawaydetails$castaway_id == 9] <- "Jenna L."
    castawaydetails$short_name[castawaydetails$castaway_id == 13] <- "Sue"
    castawaydetails$short_name[castawaydetails$castaway_id == 45] <- "Big Tom"
    castawaydetails$short_name[castawaydetails$castaway_id == 59] <- "The General"
    castawaydetails$short_name[castawaydetails$castaway_id == 94] <- "Rob C."
    castawaydetails$short_name[castawaydetails$castaway_id == 96] <- "Jenna M."
    castawaydetails$short_name[castawaydetails$castaway_id == 111] <- "Jonny Fairplay"
    castawaydetails$short_name[castawaydetails$castaway_id == 118] <- "Bubba"
    castawaydetails$short_name[castawaydetails$castaway_id == 122] <- "Sarge"
    castawaydetails$short_name[castawaydetails$castaway_id == 190] <- "Flicka"
    castawaydetails$short_name[castawaydetails$castaway_id == 206] <- "Papa Smurf"
    castawaydetails$short_name[castawaydetails$castaway_id == 288] <- "Russell S."
    castawaydetails$short_name[castawaydetails$castaway_id == 292] <- "Laura M."
    castawaydetails$short_name[castawaydetails$castaway_id == 300] <- "Russell H."
    castawaydetails$short_name[castawaydetails$castaway_id == 314] <- "Purple Kelly"    
    
    
  ## Challenges
    for (i in 1:length(challenges$winners)) {
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 9] <- "Jenna L."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 13] <- "Sue"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 45] <- "Big Tom"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 59] <- "The General"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 94] <- "Rob C."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 96] <- "Jenna M."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 111] <- "Jonny Fairplay"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 118] <- "Bubba"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 122] <- "Sarge"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 190] <- "Flicka"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 206] <- "Papa Smurf"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 288] <- "Russell S."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 292] <- "Laura M."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 300] <- "Russell H."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == 314] <- "Purple Kelly"          
      
    }

  ## vote hx
    vote_history$castaway[vote_history$castaway_id == 9] <- "Jenna L."
    vote_history$castaway[vote_history$castaway_id == 13] <- "Sue"
    vote_history$castaway[vote_history$castaway_id == 45] <- "Big Tom"
    vote_history$castaway[vote_history$castaway_id == 59] <- "The General"
    vote_history$castaway[vote_history$castaway_id == 94] <- "Rob C."
    vote_history$castaway[vote_history$castaway_id == 96] <- "Jenna M."
    vote_history$castaway[vote_history$castaway_id == 111] <- "Jonny Fairplay"
    vote_history$castaway[vote_history$castaway_id == 118] <- "Bubba"
    vote_history$castaway[vote_history$castaway_id == 122] <- "Sarge"
    vote_history$castaway[vote_history$castaway_id == 190] <- "Flicka"
    vote_history$castaway[vote_history$castaway_id == 206] <- "Papa Smurf"
    vote_history$castaway[vote_history$castaway_id == 288] <- "Russell S."
    vote_history$castaway[vote_history$castaway_id == 292] <- "Laura M."
    vote_history$castaway[vote_history$castaway_id == 300] <- "Russell H."
    vote_history$castaway[vote_history$castaway_id == 314] <- "Purple Kelly"    
    
    vote_history$vote[vote_history$vote_id == 9] <- "Jenna L."
    vote_history$vote[vote_history$vote_id == 13] <- "Sue"
    vote_history$vote[vote_history$vote_id == 45] <- "Big Tom"
    vote_history$vote[vote_history$vote_id == 59] <- "The General"
    vote_history$vote[vote_history$vote_id == 94] <- "Rob C."
    vote_history$vote[vote_history$vote_id == 96] <- "Jenna M."
    vote_history$vote[vote_history$vote_id == 111] <- "Jonny Fairplay"
    vote_history$vote[vote_history$vote_id == 118] <- "Bubba"
    vote_history$vote[vote_history$vote_id == 122] <- "Sarge"
    vote_history$vote[vote_history$vote_id == 190] <- "Flicka"
    vote_history$vote[vote_history$vote_id == 206] <- "Papa Smurf"
    vote_history$vote[vote_history$vote_id == 288] <- "Russell S."
    vote_history$vote[vote_history$vote_id == 292] <- "Laura M."
    vote_history$vote[vote_history$vote_id == 300] <- "Russell H."
    vote_history$vote[vote_history$vote_id == 314] <- "Purple Kelly"        
    
    vote_history$voted_out[vote_history$voted_out_id == 9] <- "Jenna L."
    vote_history$voted_out[vote_history$voted_out_id == 13] <- "Sue"
    vote_history$voted_out[vote_history$voted_out_id == 45] <- "Big Tom"
    vote_history$voted_out[vote_history$voted_out_id == 59] <- "The General"
    vote_history$voted_out[vote_history$voted_out_id == 94] <- "Rob C."
    vote_history$voted_out[vote_history$voted_out_id == 96] <- "Jenna M."
    vote_history$voted_out[vote_history$voted_out_id == 111] <- "Jonny Fairplay"
    vote_history$voted_out[vote_history$voted_out_id == 118] <- "Bubba"
    vote_history$voted_out[vote_history$voted_out_id == 122] <- "Sarge"
    vote_history$voted_out[vote_history$voted_out_id == 190] <- "Flicka"
    vote_history$voted_out[vote_history$voted_out_id == 206] <- "Papa Smurf"
    vote_history$voted_out[vote_history$voted_out_id == 288] <- "Russell S."
    vote_history$voted_out[vote_history$voted_out_id == 292] <- "Laura M."
    vote_history$voted_out[vote_history$voted_out_id == 300] <- "Russell H."
    vote_history$voted_out[vote_history$voted_out_id == 314] <- "Purple Kelly"         
    
  ## confessionals
    confessionals$castaway[confessionals$castaway_id == 9] <- "Jenna L."
    confessionals$castaway[confessionals$castaway_id == 13] <- "Sue"
    confessionals$castaway[confessionals$castaway_id == 45] <- "Big Tom"
    confessionals$castaway[confessionals$castaway_id == 59] <- "The General"
    confessionals$castaway[confessionals$castaway_id == 94] <- "Rob C."
    confessionals$castaway[confessionals$castaway_id == 96] <- "Jenna M."
    confessionals$castaway[confessionals$castaway_id == 111] <- "Jonny Fairplay"
    confessionals$castaway[confessionals$castaway_id == 118] <- "Bubba"
    confessionals$castaway[confessionals$castaway_id == 122] <- "Sarge"
    confessionals$castaway[confessionals$castaway_id == 190] <- "Flicka"
    confessionals$castaway[confessionals$castaway_id == 206] <- "Papa Smurf"
    confessionals$castaway[confessionals$castaway_id == 288] <- "Russell S."
    confessionals$castaway[confessionals$castaway_id == 292] <- "Laura M."
    confessionals$castaway[confessionals$castaway_id == 300] <- "Russell H."
    confessionals$castaway[confessionals$castaway_id == 314] <- "Purple Kelly"   
    
  ## hidden idols
    hidden_idols$castaway[hidden_idols$castaway_id == 9] <- "Jenna L."
    hidden_idols$castaway[hidden_idols$castaway_id == 13] <- "Sue"
    hidden_idols$castaway[hidden_idols$castaway_id == 45] <- "Big Tom"
    hidden_idols$castaway[hidden_idols$castaway_id == 59] <- "The General"
    hidden_idols$castaway[hidden_idols$castaway_id == 94] <- "Rob C."
    hidden_idols$castaway[hidden_idols$castaway_id == 96] <- "Jenna M."
    hidden_idols$castaway[hidden_idols$castaway_id == 111] <- "Jonny Fairplay"
    hidden_idols$castaway[hidden_idols$castaway_id == 118] <- "Bubba"
    hidden_idols$castaway[hidden_idols$castaway_id == 122] <- "Sarge"
    hidden_idols$castaway[hidden_idols$castaway_id == 190] <- "Flicka"
    hidden_idols$castaway[hidden_idols$castaway_id == 206] <- "Papa Smurf"
    hidden_idols$castaway[hidden_idols$castaway_id == 288] <- "Russell S."
    hidden_idols$castaway[hidden_idols$castaway_id == 292] <- "Laura M."
    hidden_idols$castaway[hidden_idols$castaway_id == 300] <- "Russell H."
    hidden_idols$castaway[hidden_idols$castaway_id == 314] <- "Purple Kelly"   
    
  ## jury votes
    jury_votes$castaway[jury_votes$castaway_id == 9] <- "Jenna L."
    jury_votes$castaway[jury_votes$castaway_id == 13] <- "Sue"
    jury_votes$castaway[jury_votes$castaway_id == 45] <- "Big Tom"
    jury_votes$castaway[jury_votes$castaway_id == 59] <- "The General"
    jury_votes$castaway[jury_votes$castaway_id == 94] <- "Rob C."
    jury_votes$castaway[jury_votes$castaway_id == 96] <- "Jenna M."
    jury_votes$castaway[jury_votes$castaway_id == 111] <- "Jonny Fairplay"
    jury_votes$castaway[jury_votes$castaway_id == 118] <- "Bubba"
    jury_votes$castaway[jury_votes$castaway_id == 122] <- "Sarge"
    jury_votes$castaway[jury_votes$castaway_id == 190] <- "Flicka"
    jury_votes$castaway[jury_votes$castaway_id == 206] <- "Papa Smurf"
    jury_votes$castaway[jury_votes$castaway_id == 288] <- "Russell S."
    jury_votes$castaway[jury_votes$castaway_id == 292] <- "Laura M."
    jury_votes$castaway[jury_votes$castaway_id == 300] <- "Russell H."
    jury_votes$castaway[jury_votes$castaway_id == 314] <- "Purple Kelly"   
    
    jury_votes$finalist[jury_votes$finalist_id == 9] <- "Jenna L."
    jury_votes$finalist[jury_votes$finalist_id == 13] <- "Sue"
    jury_votes$finalist[jury_votes$finalist_id == 45] <- "Big Tom"
    jury_votes$finalist[jury_votes$finalist_id == 59] <- "The General"
    jury_votes$finalist[jury_votes$finalist_id == 94] <- "Rob C."
    jury_votes$finalist[jury_votes$finalist_id == 96] <- "Jenna M."
    jury_votes$finalist[jury_votes$finalist_id == 111] <- "Jonny Fairplay"
    jury_votes$finalist[jury_votes$finalist_id == 118] <- "Bubba"
    jury_votes$finalist[jury_votes$finalist_id == 122] <- "Sarge"
    jury_votes$finalist[jury_votes$finalist_id == 190] <- "Flicka"
    jury_votes$finalist[jury_votes$finalist_id == 206] <- "Papa Smurf"
    jury_votes$finalist[jury_votes$finalist_id == 288] <- "Russell S."
    jury_votes$finalist[jury_votes$finalist_id == 292] <- "Laura M."
    jury_votes$finalist[jury_votes$finalist_id == 300] <- "Russell H."
    jury_votes$finalist[jury_votes$finalist_id == 314] <- "Purple Kelly"   
    
    
  ## tribe mapping
    tribe_mapping$castaway[tribe_mapping$castaway_id == 9] <- "Jenna L."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 13] <- "Sue"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 45] <- "Big Tom"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 59] <- "The General"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 94] <- "Rob C."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 96] <- "Jenna M."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 111] <- "Jonny Fairplay"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 118] <- "Bubba"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 122] <- "Sarge"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 190] <- "Flicka"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 206] <- "Papa Smurf"
    tribe_mapping$castaway[tribe_mapping$castaway_id == 288] <- "Russell S."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 292] <- "Laura M."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 300] <- "Russell H."
    tribe_mapping$castaway[tribe_mapping$castaway_id == 314] <- "Purple Kelly"   
  

## Other data cleaning
    # Updating castaway full names to be the same across all their seasons
      castaways$full_name[castaways$full_name %in% c("Amber Mariano","Amber Brkich")] <- "Amber (Brkich) Mariano"
      castaways$full_name[castaways$full_name %in% c("Candice Cody","Candice Woodcock")] <- "Candice (Woodcock) Cody"
      castaways$full_name[castaways$full_name %in% c("Kim Spradlin","Kim Wolfe")] <- "Kim Spradlin-Wolfe"    
      
      castawaydetails$full_name[castawaydetails$full_name %in% c("Amber Mariano","Amber Brkich")] <- "Amber (Brkich) Mariano"
      castawaydetails$full_name[castawaydetails$full_name %in% c("Candice Cody","Candice Woodcock")] <- "Candice (Woodcock) Cody"
      castawaydetails$full_name[castawaydetails$full_name %in% c("Kim Spradlin","Kim Wolfe")] <- "Kim Spradlin-Wolfe"    
      
    # adding additional information to season summaries based on my thoughts on the season
      season_summary$tribe_setup[season_summary$season == 16] <- "Also known as 'Fans vs. Favorites.' Two tribes of ten: new players against past contestants"
      season_summary$tribe_setup[season_summary$season == 21] <- "Two tribes of ten new players divided by age. This was the only season to have the vastly over-powered Medallion of Power"
      season_summary$tribe_setup[season_summary$season == 26] <- "Also known as 'Fans vs. Favorites 2.' Two tribes of ten: new players against past contestants"
      season_summary$tribe_setup[season_summary$season == 29] <- "Also called 'Blood vs. Water 2.' Nine pairs of new players, each with a pre-existing relationship, divided into two tribes of nine"
      season_summary$tribe_setup[season_summary$season == 31] <- "Also called 'Second Chances.' Two tribes of ten returning players who only played once before, have not won, and were selected by public vote"
      season_summary$tribe_setup[season_summary$season == 41] <- "Three tribes of six new players. 'Drop the 4 and keep the 1; it's a whole new Survivor'"

    # Votes cast against Kelley Wentworth on Day 36 of Season 31 should be nullified
    # Day 36 of Season 31 looks messy and weird.
      vote_history$nullified[vote_history$season == 31 & vote_history$day == 36 & vote_history$vote == "Kelley"] <- TRUE
      
## Add full name, gender, race, ethnicity to all tibbles
  fullnames <- unique(castawaydetails[,c("full_name","castaway_id","short_name","gender","race","ethnicity")])
  names(fullnames) <- c("full_name","castaway_id","castaway","gender","race","ethnicity")
  fullnames <- fullnames[order(fullnames$castaway_id),]
  #fullnames %>% View()
  
  # Vote history
    votenames <- fullnames
    names(votenames) <- c("vote_full_name","vote_id","vote","vote_gender","vote_race","vote_ethnicity")
    
    votedout <- fullnames
    names(votedout) <- c("voted_out_full_name","voted_out_id","voted_out","voted_out_gender","voted_out_race","voted_out_ethnicity")
  
    
    vote_history2 <- left_join(vote_history,fullnames,by=c("castaway_id", "castaway"))
    vote_history2 <- left_join(vote_history2,votenames,by=c("vote_id","vote"))
    vote_history2 <- left_join(vote_history2,votedout,by=c("voted_out_id","voted_out"))
    
  # tribe mapping
    tribe_mapping2 <- left_join(tribe_mapping,fullnames,by=c("castaway_id", "castaway"))
    
  # Jury Votes
    jury_votes2 <- left_join(jury_votes,fullnames,by=c("castaway_id", "castaway"))
    
    finalistnames <- fullnames
    names(finalistnames) <- c("finalist_full_name","finalist_id","finalist","finalist_gender","finalist_race","finalist_ethnicity")
    jury_votes2 <- left_join(jury_votes2,finalistnames,by=c("finalist_id","finalist"))
    
  # Hidden idols
    hidden_idols2 <- left_join(hidden_idols,fullnames,by=c("castaway_id", "castaway"))
    
  # confessionals
    confessionals2 <- left_join(confessionals,fullnames,by=c("castaway_id", "castaway"))
    
  # Challenges
    winnersnames <- fullnames
    names(winnersnames) <- c("winner_fullname","winner_id","winner","winner_gender","winner_race","winner_ethnicity")
    
    for (i in 1:length(challenges$winners)) {
      challenges$winners[[i]] <- left_join(challenges$winners[[i]],winnersnames,by=c("winner_id","winner"))
      
    } 
    
## Add tribe name to vote history
    vote_history3 <- left_join(vote_history2,tribe_mapping2[,c("season_name","season","episode","day","tribe_status","castaway_id","castaway","tribe")],
                               by=c("season_name","season","episode","day","tribe_status","castaway_id","castaway"))
    
    # not all tribes are filled in; do that manually
      vote_history3$tribe[vote_history3$season == 2 & vote_history3$day == 41] <- "Barramundi"
      
      vote_history3$tribe[vote_history3$season == 10 & vote_history3$day %in% c(3,6,8,11,12,15,18,21) & vote_history3$castaway == "Bobby Jon"] <- "Ulong"
      
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 3 & vote_history3$castaway == "Bobb==y Jon"] <- "Nakúm"
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 14 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 15 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 18 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 21 & vote_history3$castaway == "Bobby Jon"] <- "Xhakúm"
      vote_history3$tribe[vote_history3$season == 11 & vote_history3$day == 24 & vote_history3$castaway == "Bobby Jon"] <- "Xhakúm"
      
      #vote_history3[vote_history3$season == 38 & vote_history3$day == 16,] %>% View()
    
      
## Add additional information to datasets, to be able to say more things about people
  # set up the data
      additionaldetails <- as.data.frame(castaways[,c("castaway_id","season","age","day","order","result","jury_status")])
      additionaldetails$keep <- 0
          # only keep the "best" result for each castaway (only relevant for those who were on extinction/redemption and then came back into the game)
          for (ci in 1:max(additionaldetails$castaway_id)) {
            for (s in c(unique(additionaldetails$season[additionaldetails$castaway_id == ci]))) {
                maxday <- max(additionaldetails$day[additionaldetails$castaway_id == ci & additionaldetails$season == s],na.rm=T) 
                additionaldetails$keep[additionaldetails$castaway_id == ci & additionaldetails$day == maxday & additionaldetails$season == s] <- 1
            }
          }
      additionaldetails <- additionaldetails[additionaldetails$keep ==1,]    
      additionaldetails$keep <- NULL
  
  ## add to hidden idols data
      hidden_idols3 <- left_join(hidden_idols2,additionaldetails,
                             by=c("season","castaway_id"))
  
  ## and then do the same for the confessional data    
      confessionals3 <- left_join(confessionals2, additionaldetails,by=c("season","castaway_id"))
      
## make the the challenges tibbles able to be exported to CSV
    # create a holding dataset for the challenges data
      #create data frame with 0 rows and 17 columns
       challenges2 <- data.frame(matrix(ncol = 17, nrow = 0))
       
      #provide column names
        colnames(challenges2) <- c(names(challenges)[1:9],names(challenges$winners[[1]]))
    
    # for each of the season-episode-day-challenge types, duplicate that information for each of the winners
        for (i in 1:dim(challenges)[1]) {
          temp <- cbind(as.data.frame(challenges[i,1:9]),as.data.frame(challenges$winners[[i]]))
          
          challenges2 <- rbind(challenges2,temp)
          #print(challenges2)
        }
        
    # Data cleaning
        # want Outcasts to be "The Outcasts"
          challenges2$winning_tribe[challenges2$winning_tribe == "OutCasts"] <- "The Outcasts"
        
    # add on challenge description
      challenges2 <- left_join(challenges2,challengesdesc,
                               by=c("challenge_id","challenge_name"))
      
    # if the challenge was post-merge, add in "individual challenge" for the tribe name
      challenges2$winning_tribe[is.na(challenges2$winning_tribe) & challenges2$outcome_type %in% c("Individual","Team")] <- "(Individual challenge)"
      
      
      
###############################################################
## save the data
write.csv(castawaydetails,paste(savedir,"survivoR_01_castawayDetails_cleaned.csv",sep=""),row.names=F)
write.csv(castaways,paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),row.names=F)
write.csv(season_summary,paste(savedir,"survivoR_03_seasonSummary_cleaned.csv",sep=""),row.names=F)
write.csv(vote_history3,paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),row.names=F)
write.csv(challenges2,paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),row.names=F)
write.csv(confessionals3,paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),row.names=F)
write.csv(hidden_idols3,paste(savedir,"survivoR_07_idols_cleaned.csv",sep=""),row.names=F)
write.csv(jury_votes2,paste(savedir,"survivoR_08_juryvotes_cleaned.csv",sep=""),row.names=F)
write.csv(viewers,paste(savedir,"survivoR_09_viewers_cleaned.csv",sep=""),row.names=F)
write.csv(tribe_mapping2,paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),row.names=F)
    

