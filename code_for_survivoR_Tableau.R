## Author: Carly Levitz
## Date written: 2021-12-09
## Date updated: 2022-03-14
## Purpose: data cleaning of Survivor data  - prep the data from https://github.com/doehm/survivoR and then use these data in Tableau
##    https://public.tableau.com/app/profile/carly.levitz/viz/SurvivorCBSData-Acknowledgements/Acknkowledgements
## File organization:
##    Section 1: Clean the data for how I want to use it in Tableau
##    Section 2: Create a dataset for the tribe mapping. It's pretty inelegant but does the trick.
##    Section 3: Calculate different types of superlatives for each castaway. Superlatives can be within a season (e.g., most votes received in one season) or across seasons (e.g., most individual immunity wins across seasons played)
## Note 4/13/2022: 
## -- added some stuff for those still in S42 so they would show up on dashboards (day & result)
## -- Removed the extra votes because they were added into the raw data.

rm(list=ls()); .libPaths("C:/Program Files/R/R-4.1.1/library")
library(devtools,lib="C:/Program Files/R/R-4.1.1/library"); library(ggplot2,lib="C:/Program Files/R/R-4.1.1/library") ; library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library")
devtools::install_github("doehm/survivoR")
savedir <- "H:/R/survivoR/02_cleaned_data/"

castaways <- survivoR::castaways
  ## For now, have the season 42 folks have "still in the game" as their "result"
  castaways$result[castaways$version_season == "US42" & is.na(castaways$result)] <- "Still in game"
  castaways$day[castaways$version_season == "US42" & is.na(castaways$day)] <- 26
  
castawaydetails <- survivoR::castaway_details
challenges <- survivoR::challenge_results
challengesdesc <- survivoR::challenge_description
vote_history <- survivoR::vote_history
confessionals <- survivoR::confessionals
  confessionals$version <- "US"
  confessionals$version_season <- paste("US",as.character(confessionals$season),sep="")
  for (sn in 1:9) { confessionals$version_season[confessionals$season == sn] <- paste("US0",as.character(confessionals$season[confessionals$season == sn]),sep="") }

season_summary <- survivoR::season_summary
  season_summary$version <- "US"
  season_summary$version_season <- paste("US",as.character(season_summary$season),sep="")
  for (sn in 1:9) { season_summary$version_season[season_summary$season == sn] <- paste("US0",as.character(season_summary$season[season_summary$season == sn]),sep="") }
  
  # manually add in data for S42 until it's updated in dev
  season_summary$premiered[season_summary$version_season == "US42"] <- "2022-03-09" 
  season_summary$timeslot[season_summary$version_season == "US42"] <- "Wednesday 8:00 pm"
  season_summary$filming_started[season_summary$version_season == "US42"] <- "2021-05-16"
  season_summary$filming_ended[season_summary$version_season == "US42"] <- "2021-06-10"
  
hidden_idols <- survivoR::hidden_idols
  hidden_idols$version <- "US"
  hidden_idols$version_season <- paste("US",as.character(hidden_idols$season),sep="")
  for (sn in 1:9) { hidden_idols$version_season[hidden_idols$season == sn] <- paste("US0",as.character(hidden_idols$season[hidden_idols$season == sn]),sep="") }

jury_votes <- survivoR::jury_votes
  jury_votes$version <- "US"
  jury_votes$version_season <- paste("US",as.character(jury_votes$season),sep="")
  for (sn in 1:9) { jury_votes$version_season[jury_votes$season == sn] <- paste("US0",as.character(jury_votes$season[jury_votes$season == sn]),sep="") }

tribe_mapping <- survivoR::tribe_mapping
viewers <- survivoR::viewers
  viewers$version <- "US"
  viewers$version_season <- paste("US",as.character(viewers$season),sep="")
  for (sn in 1:9) { viewers$version_season[viewers$season == sn] <- paste("US0",as.character(viewers$season[viewers$season == sn]),sep="") }

#############################################################################################################################
#############################################################################################################################
## Section 1: Clean the data for how I want to use it in Tableau
#############################################################################################################################
#############################################################################################################################  

  # clean the result data
    castaways$result[castaways$result %in% c("2nd Runner-up","2nd runner-up")] <- "2nd runner-up"

## My data source (survivor wiki fandom) only have pages for Asian, Latin American, and Black. 
## I found a reddit page that had Jewish players. 
## On Natalie Bolton's page, it mentioned that she was Native American, and the page called out 3 or 4 other Native American players.
castawaydetails$race[is.na(castawaydetails$race) | castawaydetails$race == ""] <- "Unknown"

  # edit to gender, race, and ethnicity data
  # Peih-Gee in Season 31 is missing ethnicity data
    castawaydetails$ethnicity[castawaydetails$short_name == "Peih-Gee" & is.na(castawaydetails$ethnicity)] <- "Hong Konger American"
  
  # Want to distinguish between Black players and other players of color
    castawaydetails$poc[grep("Black",castawaydetails$race)] <- "Black"
    
  # visually check that all of the people who show up more than once have the same data for 
      #tempcast <- castaways[,c(1:9)]
      #tempcast$countofrows <- NA
      
      #for (cc in 1:max(tempcast$castaway_id)) {
      #  tempcast$countofrows[tempcast$castaway_id == cc] <- nrow(tempcast[tempcast$castaway_id == cc,])
        
      #}
      #tempcast <- tempcast[tempcast$countofrows > 1,]
      #tempcast <- tempcast[order(tempcast$castaway_id),]
      #tempcast  %>% View()
  
  # For my own purposes, I want some people to consistently be known with their nickname or their name with their last initial.
  # I tend to use the short name more than the long name.  
    ## Have Castaway & Full Name consistent across Castaway ID
    ## Castaway dataset
    castaways$castaway[castaways$castaway_id == "US0009"] <- "Jenna L."
    castaways$castaway[castaways$castaway_id == "US0013"] <- "Sue"
    castaways$castaway[castaways$castaway_id == "US0045"] <- "Big Tom"
    castaways$castaway[castaways$castaway_id == "US0059"] <- "The General"
    castaways$castaway[castaways$castaway_id == "US0094"] <- "Rob C."
    castaways$castaway[castaways$castaway_id == "US0096"] <- "Jenna M."
    castaways$castaway[castaways$castaway_id == "US0111"] <- "Jonny Fairplay"
    castaways$castaway[castaways$castaway_id == "US0118"] <- "Bubba"
    castaways$castaway[castaways$castaway_id == "US0122"] <- "Sarge"
    castaways$castaway[castaways$castaway_id == "US0190"] <- "Flicka"
    castaways$castaway[castaways$castaway_id == "US0206"] <- "Papa Smurf"
    castaways$castaway[castaways$castaway_id == "US0288"] <- "Russell S."
    castaways$castaway[castaways$castaway_id == "US0292"] <- "Laura M."
    castaways$castaway[castaways$castaway_id == "US0300"] <- "Russell H."
    castaways$castaway[castaways$castaway_id == "US0314"] <- "Purple Kelly"
    
    ## castaway details
    castawaydetails$short_name[castawaydetails$castaway_id == "US0009"] <- "Jenna L."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0013"] <- "Sue"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0045"] <- "Big Tom"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0059"] <- "The General"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0094"] <- "Rob C."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0096"] <- "Jenna M."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0111"] <- "Jonny Fairplay"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0118"] <- "Bubba"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0122"] <- "Sarge"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0190"] <- "Flicka"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0206"] <- "Papa Smurf"
    castawaydetails$short_name[castawaydetails$castaway_id == "US0288"] <- "Russell S."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0292"] <- "Laura M."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0300"] <- "Russell H."
    castawaydetails$short_name[castawaydetails$castaway_id == "US0314"] <- "Purple Kelly"    
    
    
    ## Challenges
    for (i in 1:length(challenges$winners)) {
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0009"] <- "Jenna L."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0013"] <- "Sue"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0045"] <- "Big Tom"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0059"] <- "The General"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0094"] <- "Rob C."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0096"] <- "Jenna M."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0111"] <- "Jonny Fairplay"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0118"] <- "Bubba"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0122"] <- "Sarge"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0190"] <- "Flicka"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0206"] <- "Papa Smurf"
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0288"] <- "Russell S."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0292"] <- "Laura M."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0300"] <- "Russell H."
      challenges$winners[[i]]$winner[challenges$winners[[i]]$winner_id == "US0314"] <- "Purple Kelly"          
      
    }
    
    ## vote hx
    vote_history$castaway[vote_history$castaway_id == "US0009"] <- "Jenna L."
    vote_history$castaway[vote_history$castaway_id == "US0013"] <- "Sue"
    vote_history$castaway[vote_history$castaway_id == "US0045"] <- "Big Tom"
    vote_history$castaway[vote_history$castaway_id == "US0059"] <- "The General"
    vote_history$castaway[vote_history$castaway_id == "US0094"] <- "Rob C."
    vote_history$castaway[vote_history$castaway_id == "US0096"] <- "Jenna M."
    vote_history$castaway[vote_history$castaway_id == "US0111"] <- "Jonny Fairplay"
    vote_history$castaway[vote_history$castaway_id == "US0118"] <- "Bubba"
    vote_history$castaway[vote_history$castaway_id == "US0122"] <- "Sarge"
    vote_history$castaway[vote_history$castaway_id == "US0190"] <- "Flicka"
    vote_history$castaway[vote_history$castaway_id == "US0206"] <- "Papa Smurf"
    vote_history$castaway[vote_history$castaway_id == "US0288"] <- "Russell S."
    vote_history$castaway[vote_history$castaway_id == "US0292"] <- "Laura M."
    vote_history$castaway[vote_history$castaway_id == "US0300"] <- "Russell H."
    vote_history$castaway[vote_history$castaway_id == "US0314"] <- "Purple Kelly"    
    
    vote_history$vote[vote_history$vote_id == "US0009"] <- "Jenna L."
    vote_history$vote[vote_history$vote_id == "US0013"] <- "Sue"
    vote_history$vote[vote_history$vote_id == "US0045"] <- "Big Tom"
    vote_history$vote[vote_history$vote_id == "US0059"] <- "The General"
    vote_history$vote[vote_history$vote_id == "US0094"] <- "Rob C."
    vote_history$vote[vote_history$vote_id == "US0096"] <- "Jenna M."
    vote_history$vote[vote_history$vote_id == "US0111"] <- "Jonny Fairplay"
    vote_history$vote[vote_history$vote_id == "US0118"] <- "Bubba"
    vote_history$vote[vote_history$vote_id == "US0122"] <- "Sarge"
    vote_history$vote[vote_history$vote_id == "US0190"] <- "Flicka"
    vote_history$vote[vote_history$vote_id == "US0206"] <- "Papa Smurf"
    vote_history$vote[vote_history$vote_id == "US0288"] <- "Russell S."
    vote_history$vote[vote_history$vote_id == "US0292"] <- "Laura M."
    vote_history$vote[vote_history$vote_id == "US0300"] <- "Russell H."
    vote_history$vote[vote_history$vote_id == "US0314"] <- "Purple Kelly"        
    
    vote_history$voted_out[vote_history$voted_out_id == "US0009"] <- "Jenna L."
    vote_history$voted_out[vote_history$voted_out_id == "US0013"] <- "Sue"
    vote_history$voted_out[vote_history$voted_out_id == "US0045"] <- "Big Tom"
    vote_history$voted_out[vote_history$voted_out_id == "US0059"] <- "The General"
    vote_history$voted_out[vote_history$voted_out_id == "US0094"] <- "Rob C."
    vote_history$voted_out[vote_history$voted_out_id == "US0096"] <- "Jenna M."
    vote_history$voted_out[vote_history$voted_out_id == "US0111"] <- "Jonny Fairplay"
    vote_history$voted_out[vote_history$voted_out_id == "US0118"] <- "Bubba"
    vote_history$voted_out[vote_history$voted_out_id == "US0122"] <- "Sarge"
    vote_history$voted_out[vote_history$voted_out_id == "US0190"] <- "Flicka"
    vote_history$voted_out[vote_history$voted_out_id == "US0206"] <- "Papa Smurf"
    vote_history$voted_out[vote_history$voted_out_id == "US0288"] <- "Russell S."
    vote_history$voted_out[vote_history$voted_out_id == "US0292"] <- "Laura M."
    vote_history$voted_out[vote_history$voted_out_id == "US0300"] <- "Russell H."
    vote_history$voted_out[vote_history$voted_out_id == "US0314"] <- "Purple Kelly"         
    
    ## confessionals
    confessionals$castaway[confessionals$castaway_id == "US0009"] <- "Jenna L."
    confessionals$castaway[confessionals$castaway_id == "US0013"] <- "Sue"
    confessionals$castaway[confessionals$castaway_id == "US0045"] <- "Big Tom"
    confessionals$castaway[confessionals$castaway_id == "US0059"] <- "The General"
    confessionals$castaway[confessionals$castaway_id == "US0094"] <- "Rob C."
    confessionals$castaway[confessionals$castaway_id == "US0096"] <- "Jenna M."
    confessionals$castaway[confessionals$castaway_id == "US0111"] <- "Jonny Fairplay"
    confessionals$castaway[confessionals$castaway_id == "US0118"] <- "Bubba"
    confessionals$castaway[confessionals$castaway_id == "US0122"] <- "Sarge"
    confessionals$castaway[confessionals$castaway_id == "US0190"] <- "Flicka"
    confessionals$castaway[confessionals$castaway_id == "US0206"] <- "Papa Smurf"
    confessionals$castaway[confessionals$castaway_id == "US0288"] <- "Russell S."
    confessionals$castaway[confessionals$castaway_id == "US0292"] <- "Laura M."
    confessionals$castaway[confessionals$castaway_id == "US0300"] <- "Russell H."
    confessionals$castaway[confessionals$castaway_id == "US0314"] <- "Purple Kelly"   
    
    ## hidden idols
    hidden_idols$castaway[hidden_idols$castaway_id == "US0009"] <- "Jenna L."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0013"] <- "Sue"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0045"] <- "Big Tom"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0059"] <- "The General"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0094"] <- "Rob C."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0096"] <- "Jenna M."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0111"] <- "Jonny Fairplay"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0118"] <- "Bubba"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0122"] <- "Sarge"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0190"] <- "Flicka"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0206"] <- "Papa Smurf"
    hidden_idols$castaway[hidden_idols$castaway_id == "US0288"] <- "Russell S."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0292"] <- "Laura M."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0300"] <- "Russell H."
    hidden_idols$castaway[hidden_idols$castaway_id == "US0314"] <- "Purple Kelly"   
    
    ## jury votes
    jury_votes$castaway[jury_votes$castaway_id == "US0009"] <- "Jenna L."
    jury_votes$castaway[jury_votes$castaway_id == "US0013"] <- "Sue"
    jury_votes$castaway[jury_votes$castaway_id == "US0045"] <- "Big Tom"
    jury_votes$castaway[jury_votes$castaway_id == "US0059"] <- "The General"
    jury_votes$castaway[jury_votes$castaway_id == "US0094"] <- "Rob C."
    jury_votes$castaway[jury_votes$castaway_id == "US0096"] <- "Jenna M."
    jury_votes$castaway[jury_votes$castaway_id == "US0111"] <- "Jonny Fairplay"
    jury_votes$castaway[jury_votes$castaway_id == "US0118"] <- "Bubba"
    jury_votes$castaway[jury_votes$castaway_id == "US0122"] <- "Sarge"
    jury_votes$castaway[jury_votes$castaway_id == "US0190"] <- "Flicka"
    jury_votes$castaway[jury_votes$castaway_id == "US0206"] <- "Papa Smurf"
    jury_votes$castaway[jury_votes$castaway_id == "US0288"] <- "Russell S."
    jury_votes$castaway[jury_votes$castaway_id == "US0292"] <- "Laura M."
    jury_votes$castaway[jury_votes$castaway_id == "US0300"] <- "Russell H."
    jury_votes$castaway[jury_votes$castaway_id == "US0314"] <- "Purple Kelly"   
    
    jury_votes$finalist[jury_votes$finalist_id == "US0009"] <- "Jenna L."
    jury_votes$finalist[jury_votes$finalist_id == "US0013"] <- "Sue"
    jury_votes$finalist[jury_votes$finalist_id == "US0045"] <- "Big Tom"
    jury_votes$finalist[jury_votes$finalist_id == "US0059"] <- "The General"
    jury_votes$finalist[jury_votes$finalist_id == "US0094"] <- "Rob C."
    jury_votes$finalist[jury_votes$finalist_id == "US0096"] <- "Jenna M."
    jury_votes$finalist[jury_votes$finalist_id == "US0111"] <- "Jonny Fairplay"
    jury_votes$finalist[jury_votes$finalist_id == "US0118"] <- "Bubba"
    jury_votes$finalist[jury_votes$finalist_id == "US0122"] <- "Sarge"
    jury_votes$finalist[jury_votes$finalist_id == "US0190"] <- "Flicka"
    jury_votes$finalist[jury_votes$finalist_id == "US0206"] <- "Papa Smurf"
    jury_votes$finalist[jury_votes$finalist_id == "US0288"] <- "Russell S."
    jury_votes$finalist[jury_votes$finalist_id == "US0292"] <- "Laura M."
    jury_votes$finalist[jury_votes$finalist_id == "US0300"] <- "Russell H."
    jury_votes$finalist[jury_votes$finalist_id == "US0314"] <- "Purple Kelly"   
    
    
    ## tribe mapping
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0009"] <- "Jenna L."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0013"] <- "Sue"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0045"] <- "Big Tom"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0059"] <- "The General"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0094"] <- "Rob C."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0096"] <- "Jenna M."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0111"] <- "Jonny Fairplay"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0118"] <- "Bubba"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0122"] <- "Sarge"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0190"] <- "Flicka"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0206"] <- "Papa Smurf"
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0288"] <- "Russell S."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0292"] <- "Laura M."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0300"] <- "Russell H."
    tribe_mapping$castaway[tribe_mapping$castaway_id == "US0314"] <- "Purple Kelly"   
    
## Other data cleaning
    # Updating castaway full names to be the same across all their seasons
      castaways$full_name[castaways$full_name %in% c("Amber Mariano","Amber Brkich")] <- "Amber (Brkich) Mariano"
      castaways$full_name[castaways$full_name %in% c("Candice Cody","Candice Woodcock")] <- "Candice (Woodcock) Cody"
      castaways$full_name[castaways$full_name %in% c("Kim Spradlin","Kim Wolfe")] <- "Kim Spradlin-Wolfe"    
      
      castawaydetails$full_name[castawaydetails$full_name %in% c("Amber Mariano","Amber Brkich")] <- "Amber (Brkich) Mariano"
      castawaydetails$full_name[castawaydetails$full_name %in% c("Candice Cody","Candice Woodcock")] <- "Candice (Woodcock) Cody"
      castawaydetails$full_name[castawaydetails$full_name %in% c("Kim Spradlin","Kim Wolfe")] <- "Kim Spradlin-Wolfe"    
      
    # adding additional information to season summaries based on my thoughts on the season
      season_summary$tribe_setup[season_summary$version_season == "US16"] <- "Also known as 'Fans vs. Favorites.' Two tribes of ten: new players against past contestants"
      season_summary$tribe_setup[season_summary$version_season == "US21"] <- "Two tribes of ten new players divided by age. This was the only season to have the vastly over-powered Medallion of Power"
      season_summary$tribe_setup[season_summary$version_season == "US26"] <- "Also known as 'Fans vs. Favorites 2.' Two tribes of ten: new players against past contestants"
      season_summary$tribe_setup[season_summary$version_season == "US29"] <- "Also called 'Blood vs. Water 2.' Nine pairs of new players, each with a pre-existing relationship, divided into two tribes of nine"
      season_summary$tribe_setup[season_summary$version_season == "US31"] <- "Also called 'Second Chances.' Two tribes of ten returning players who only played once before, have not won, and were selected by public vote"
      season_summary$tribe_setup[season_summary$version_season == "US41"] <- "Three tribes of six new players. 'Drop the 4 and keep the 1; it's a whole new Survivor'"

    # Votes cast against Kelley Wentworth on Day 36 of Season 31 should be nullified
    # Day 36 of Season 31 looks messy and weird.
      vote_history$nullified[vote_history$version_season == "US31" & vote_history$day == 36 & vote_history$vote == "Kelley"] <- TRUE
      
    ## Season 42 - Zach's vote ID should be considered NA because he used a shot in the dark
      vote_history$vote_id[vote_history$vote == "Shot in the dark"] <- NA
      
    ## add extra votes in
		#	extravotes <- data.frame(
		#	                rbind(# Worlds Apart: Dan had an extra vote that he bought at the auction. He used it against Carolyn on Day 35, but both of his votes were negated by Carolyn's idol; he went home. He won it on Day 25.			                      
		#	                          c("US","US30","Survivor: Worlds Apart",30,13,35,"Merged","Dan",NA,"Carolyn",NA,NA,"TRUE","Dan",13,1,"US0455","US0459","US0455"),
    #                        #	Kaoh Rong: Tai had an extra vote. He put both votes on Michele on Day 32, as his alliance decided to vote out Jason. He found it on Day 25.
		#	                          c("US","US32","Survivor: Kaoh Rong",32,12,32,"Merged","Tai",NA,"Michele",NA,NA,"FALSE","Jason",13,1,"US0476","US0478","US0473"),
		#	                      #	Game Changers: Day 14 Debbie got it on Exile. "Advantage menu". Used it on Day 24, voted against Ozzy twice. 
		#	                          c("US","US34","Survivor: Game Changers",34,7,24,"Merged","Debbie",NA,"Ozzy",NA,NA,"FALSE","Ozzy",9,1,"US0470","US0201","US0201"),
    #                        # Ghost Island: Kellyn on Day 18 - played for reward and received the Vote Steal from Game Changers that turns into extra vote. Kellyn cast two votes on Day 29 (double elimination) for Laurel. After vote tied, Kellyn voted for Michael WITHOUT using extra vote.
		#	                      c("US","US36","Survivor: Ghost Island",36,10,29,"Merged","Kellyn",NA,"Laurel",NA,NA,"FALSE","Tie",12,1,"US0530","US0534",NA),
		#	                      # Edge of Extinction: Day 21 - another hidden on Extinction, Aubry found it. Gifted it to Aurora. Day 27 Aurora transferred it to Ron, who reclaimed it the following day. Day 28 gave it to Gavin. Gavin ended up using it against Ron
		#	                      c("US","US38","Survivor: Edge of Extinction",38,11,31,"Merged","Gavin","Individual","Ron",NA,NA,"FALSE","Ron",13,1,"US0570","US0565","US0565"))
		#	              )
    #  names(extravotes) <- names(vote_history)
		#	for (var in c("season","episode","day","order","vote_order")) {
		#	  extravotes[,var] <- as.numeric(extravotes[,var])
		#	}
      
		#	vote_history <- rbind(vote_history,extravotes)			
			
## Add full name, gender, race, ethnicity to all tibbles
  fullnames <- unique(castawaydetails[,c("full_name","castaway_id","short_name","gender","race","ethnicity","poc")])
  names(fullnames) <- c("full_name","castaway_id","castaway","gender","race","ethnicity","poc")
  fullnames <- fullnames[order(fullnames$castaway_id),]
  #fullnames %>% View()
  
  # Vote history
    votenames <- fullnames
    names(votenames) <- c("vote_full_name","vote_id","vote","vote_gender","vote_race","vote_ethnicity","vote_poc")
    
    votedout <- fullnames
    names(votedout) <- c("voted_out_full_name","voted_out_id","voted_out","voted_out_gender","voted_out_race","voted_out_ethnicity","voted_out_poc")
  
    vote_history2 <- left_join(vote_history,fullnames,by=c("castaway_id", "castaway"))
    vote_history2 <- left_join(vote_history2,votenames,by=c("vote_id","vote"))
    vote_history2 <- left_join(vote_history2,votedout,by=c("voted_out_id","voted_out"))
    
  # tribe mapping
    tribe_mapping2 <- left_join(tribe_mapping,fullnames,by=c("castaway_id", "castaway"))
    
  # Jury Votes
    jury_votes2 <- left_join(jury_votes,fullnames,by=c("castaway_id", "castaway"))
    
    finalistnames <- fullnames
    names(finalistnames) <- c("finalist_full_name","finalist_id","finalist","finalist_gender","finalist_race","finalist_ethnicity","finalist_poc")
    jury_votes2 <- left_join(jury_votes2,finalistnames,by=c("finalist_id","finalist"))
    
  # Hidden idols
    hidden_idols2 <- left_join(hidden_idols,fullnames,by=c("castaway_id", "castaway"))
    
  # confessionals
    confessionals2 <- left_join(confessionals,fullnames,by=c("castaway_id", "castaway"))
    
  # Challenges
    winnersnames <- fullnames
    names(winnersnames) <- c("winner_fullname","winner_id","winner","winner_gender","winner_race","winner_ethnicity","winner_poc")
    
    for (i in 1:length(challenges$winners)) {
      challenges$winners[[i]] <- left_join(challenges$winners[[i]],winnersnames,by=c("winner_id","winner"))
      
    } 
    
## Add tribe name to vote history
    vote_history3 <- left_join(vote_history2,tribe_mapping2[,c("version","version_season","season_name","season","episode","day","tribe_status","castaway_id","castaway","tribe")],
                               by=c("version","version_season","season_name","season","episode","day","tribe_status","castaway_id","castaway"))
    
    # not all tribes are filled in; do that manually
      vote_history3$tribe[vote_history3$version_season == "US02" & vote_history3$day == 41] <- "Barramundi"
      
      vote_history3$tribe[vote_history3$version_season == "US10" & vote_history3$day %in% c(3,6,8,11,12,15,18,21) & vote_history3$castaway == "Bobby Jon"] <- "Ulong"
      
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 3 & vote_history3$castaway == "Bobb==y Jon"] <- "Nakúm"
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 14 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 15 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 18 & vote_history3$castaway == "Bobby Jon"] <- "Yaxhá"
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 21 & vote_history3$castaway == "Bobby Jon"] <- "Xhakúm"
      vote_history3$tribe[vote_history3$version_season == "US11" & vote_history3$day == 24 & vote_history3$castaway == "Bobby Jon"] <- "Xhakúm"
      
      #vote_history3[vote_history3$season == 38 & vote_history3$day == 16,] %>% View()
    
      
## Add additional information to datasets, to be able to say more things about people
  # set up the data
      additionaldetails <- as.data.frame(castaways[,c("castaway_id","version_season","age","day","order","result","jury_status")])
      additionaldetails$keep <- 0
          # only keep the "best" result for each castaway (only relevant for those who were on extinction/redemption and then came back into the game)
          for (ci in unique(additionaldetails$castaway_id)) {
            for (s in c(unique(additionaldetails$version_season[additionaldetails$castaway_id == ci]))) {
                maxday <- max(additionaldetails$day[additionaldetails$castaway_id == ci & additionaldetails$version_season == s],na.rm=T) 
                additionaldetails$keep[additionaldetails$castaway_id == ci & additionaldetails$day == maxday & additionaldetails$version_season == s] <- 1
            }
          }
      additionaldetails <- additionaldetails[additionaldetails$keep ==1,]    
      additionaldetails$keep <- NULL
  
  ## add to hidden idols data
      hidden_idols3 <- left_join(hidden_idols2,additionaldetails,
                             by=c("version_season","castaway_id"))
  
  ## and then do the same for the confessional data    
      confessionals3 <- left_join(confessionals2, additionaldetails,by=c("version_season","castaway_id"))
      
## make the the challenges tibbles able to be exported to CSV
    # create a holding dataset for the challenges data
      #create data frame with 0 rows and 17 columns
       challenges2 <- data.frame(matrix(ncol = 20, nrow = 0))
       
      #provide column names
        colnames(challenges2) <- c(names(challenges)[c(1:11)],names(challenges$winners[[1]]))
    
    # for each of the season-episode-day-challenge types, duplicate that information for each of the winners
        for (i in 1:dim(challenges)[1]) {
          temp <- cbind(as.data.frame(challenges[i,c(1:11)]),as.data.frame(challenges$winners[[i]]))
          
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
    
# align variable names
  challenges2 <- challenges2 %>%
                  rename(racecharacteristic=race,castaway_id=winner_id,castaway=winner,full_name=winner_fullname,gender=winner_gender,race=winner_race,ethnicity=winner_ethnicity,poc=winner_poc,tribe=winning_tribe)
  
      
###############################################################
## save the data as separate datasets before using them to create additional datasets
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
    
#############################################################################################################################
#############################################################################################################################
## Section 2: Tribe Map: day by day
#############################################################################################################################
#############################################################################################################################
rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library");library(tidygraph,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library"); library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library"); library(ggrepel,lib="C:/Program Files/R/R-4.1.1/library");library(ggraph,lib="C:/Program Files/R/R-4.1.1/library");  
savedir <- "H:/R/survivoR/02_cleaned_data/"

tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
# get the tribe info
castaways <- castaways %>% filter(!(is.na(castaway_id))) %>% 
  group_by(version,version_season,season,castaway_id) %>%
  filter(day %in% max(day)) %>%
  select(1:7,17:20)

## Data set up:
    
    ## Let's get a simplified dataset that is just season, day, castaway_id, and tribe name & status
    tribesimpl <- unique(tribemap[!(is.na(tribemap$castaway_id)),c("version","version_season","season","day","castaway_id","tribe","tribe_status")])
    
    # for edge of extinction & redemption, want that to show up in tribe name
    tribesimpl$tribe[tribesimpl$tribe_status == "Edge of Extinction"] <- "Edge of Extinction"
    tribesimpl$tribe[tribesimpl$tribe_status == "Redemption Island"] <- "Redemption Island"
    
    
    ## Add on additional information that will help us order the data
    tribesimpl <- left_join(tribesimpl,
                            castaways,
                            by=c("version","version_season","season","castaway_id")) %>% distinct()
    ## Y values
    # Get Y values by tribe status
        arrangement <- function(tribestatuscateg) {
          # for now, just keep one stage/phase of the game
          tempdata <- tribesimpl[tribesimpl$tribe_status %in% tribestatuscateg,]
          
          # organize the Y values within a season
          for (sn in seq(1,max(tempdata$season),1)) {
            
            # This counter will tick up and will become the Y value.
            ## need it to be different for Extinction/Redemption
            tempcount <- 1
            
            # Want each castaway to be their own line, and organize the original Ys by their original tribe
            for (tr in unique(tempdata$tribe[tempdata$season == sn])) {
              for (c in rev(unique(tempdata$castaway_id[tempdata$tribe == tr & tempdata$season == sn]))) {
                
                tempdata$y[tempdata$tribe == tr & tempdata$season == sn & tempdata$castaway_id==c] <- tempcount
                # increase the y value for each subsequent castaway
                tempcount <- tempcount+1
              } # close castaway loop
              # Add a break between tribes so it's easier to read on the chart where the delineations between tribes are
              tempcount <- tempcount+2
            } # close tribe loop
          } # close season loop
          # To be able to append this tribe status data to full dataset, need to print it (for some reason)
          print(tempdata)
        } #close function loop
        
        # Use this function for all tribe statuses, and then append the data together
        tribemapreorg <- rbind(arrangement(tribestatuscateg="Original"),
                               arrangement(tribestatuscateg="Swapped"),
                               arrangement(tribestatuscateg="Swapped_2"),
                               # arrangement(tribestatuscateg="Swapped_3"),
                               arrangement(tribestatuscateg="Merged"))

  # for redemption & extinction, give each person their own row  
      seasonswithredex <- unique(tribesimpl$season_name[tribesimpl$tribe_status %in% c("Edge of Extinction","Redemption Island","Exile Island")])
      tempdata <- tribesimpl[tribesimpl$season_name %in% seasonswithredex,]
      for (sn in seasonswithredex) {
        
        # number of people who were on extinction/redemption
        numberonredex <- length(unique(tempdata$castaway_id[tempdata$season_name %in% sn & tempdata$tribe %in% c("Redemption Island","Edge of Extinction")]))
        # what's the maximum y value i have for the NON red/ex
        maxy <- max(tribemapreorg$y[tribemapreorg$season_name %in% sn])
        
        tempy <- 1
        for (c in rev(unique(tribesimpl$castaway_id[tribesimpl$season_name %in% sn & tribesimpl$tribe %in% c("Redemption Island","Edge of Extinction")]))) {
          tempdata$y[tempdata$castaway_id %in% c & tempdata$season_name %in% sn & tempdata$tribe %in% c("Redemption Island","Edge of Extinction")] <- maxy+tempy
          tempy <- tempy+1
        }
      }
      tempdata <- tempdata[!(is.na(tempdata$y)),]
      tribemapreorg <- rbind(tribemapreorg,tempdata) 
      tribemapreorg <- tribemapreorg[order(tribemapreorg$season,tribemapreorg$day),]

    ## Have a variable that will become the label for the "line" that is each castaway. 
    ## We only want to label the start and end point for each Castaway in a season    
        tribemapreorg$labeltext <- ""
        for (vzn in unique(tribemap$version)) {
          for (sn in 1:max(tribemapreorg$season[tribemapreorg$version == vzn])) {
            for (c in unique(tribemapreorg$castaway_id[tribemapreorg$season == sn & tribemapreorg$version == vzn])) {
              # What is the first day that the castaway shows up in the data?
              minday <- min(tribemapreorg$day[tribemapreorg$season == sn & tribemapreorg$castaway_id == c & tribemapreorg$version == vzn],na.rm=T)
              temptext <- tribemapreorg$castaway[tribemapreorg$day == minday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c & tribemapreorg$version == vzn]
              tribemapreorg$labeltext[tribemapreorg$day == minday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c & tribemapreorg$version == vzn] <- temptext
              
              # what is the last day the castaway shows up in the data?
              maxday <- max(tribemapreorg$day[tribemapreorg$season == sn & tribemapreorg$castaway_id == c & tribemapreorg$version == vzn],na.rm=T)
              tribemapreorg$labeltext[tribemapreorg$day == maxday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c & tribemapreorg$version == vzn] <- temptext
              
            } # Close castaway loop
          } # Close season loop
        } # close the country/version loop

  # Get names of each tribe on the first day of their existence
  # We'll print that tribe name above the Castaways on that tribe. So need to get the max y-value of the castaways on that tribe
      tribenames <- tribemapreorg[,c("version","version_season","season","day","tribe","tribe_status","y")]
  
  # get a unique tribe name, to distinguish original tribes from swapped:
      tribenames$tribename <- paste(tribenames$tribe,tribenames$tribe_status,sep="-")
  
      for (tr in unique(tribenames$tribename)) {
        # first day of tribe's existence:
        min_day <- min(tribenames$day[tribenames$tribename == tr])
        tribenames$keep[tribenames$tribename == tr & tribenames$day == min_day] <- 1
        
        # the y-value we'll use for graphing
        max_y <- max(tribenames$y[tribenames$tribename == tr])
        tribenames$YtoUse[tribenames$tribename == tr] <- max_y
      }
      tribenames <- unique(tribenames[tribenames$keep %in% 1,c("version","version_season","season","day","tribe","tribe_status","tribename","YtoUse")])

    # bring this back onto the original data
        tribemapreorg <- left_join(tribemapreorg,tribenames,by=c("version","version_season","season","day","tribe","tribe_status"))
        # We don't want the second swaps to show up as "Swapped_2", so make it a little easier to understand
        tribemapreorg$tribename <- gsub("Swapped_2","Second swap",tribemapreorg$tribename)
        tribemapreorg$tribename <- gsub("Swapped_3","Third swap",tribemapreorg$tribename)
        tribemapreorg$tribename <- gsub("-Edge of Extinction","",tribemapreorg$tribename)
        tribemapreorg$tribename <- gsub("-Redemption Island","",tribemapreorg$tribename)
        
    ## Add in edge of extinction, redemptoin to the start of all relevant seasons  
    # but only want it to show up once (if I leave all the rows filled in with the tribe name information, then it will print that over itself again and again)
        for (tr in unique(tribemapreorg$tribename[!(is.na(tribemapreorg$tribename))])) {
          miny <- min(tribemapreorg$y[tribemapreorg$tribename == tr],na.rm=T)
          tribemapreorg$YtoUse[tribemapreorg$tribename == tr & tribemapreorg$y != miny] <- NA
          
        }      

write.csv(tribemapreorg,paste(savedir,"survivoR_TribeMap_DayByDay.csv",sep=""),row.names=F)      


#############################################################################################################################
#############################################################################################################################
## Section 3: Individual superlatives
#############################################################################################################################
#############################################################################################################################

rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library"); library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library")
savedir <- "H:/R/survivoR/02_cleaned_data/"


castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
castawaydetails <- read.csv(paste(savedir,"survivoR_01_castawayDetails_cleaned.csv",sep=""),header=T)
votehx <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)


## Superlatives for an individual
    ## A. Challenge superlatives for the individual
          ## number of individual immunity wins in one season and overall
          indivimmwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
            filter(grepl("Winner",outcome_status)) %>%
            filter(grepl("Immunity",challenge_type)) %>%
            select(version,version_season,season,castaway_id,day) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(immwinsOneSeason=n_distinct(day))
          indivimmwins <- indivimmwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(immwins = sum(immwinsOneSeason))
          
          ## number of individual reward wins (in one season and across seasons)
          indivrewwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
            filter(grepl("Winner",outcome_status)) %>%
            filter(grepl("Reward",challenge_type)) %>%
            select(version,version_season,season,castaway_id,day) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(rewwinsOneSeason=n_distinct(day))
          indivrewwins <- indivrewwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(rewwins = sum(rewwinsOneSeason))  
          
          ## number of tribal immunity wins (in one season and across seasons)
          tribalimmwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
            filter(grepl("Winner",outcome_status)) %>%
            filter(grepl("Immunity",challenge_type)) %>%
            select(version,version_season,season,castaway_id,day) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(tribalimmwinsOneSeason=n_distinct(day))
          
          tribalimmwins <- tribalimmwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(tribalimmwins = sum(tribalimmwinsOneSeason))
          
          
          ## number of tribal reward wins (in one season and across seasons)
          tribalrewwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
            filter(grepl("Winner",outcome_status)) %>%
            filter(grepl("Reward",challenge_type)) %>%
            select(version,version_season,season,castaway_id,day) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(tribalrewwinsOneSeason=n_distinct(day))
          
          tribalrewwins <- tribalrewwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(tribalrewwins = sum(tribalrewwinsOneSeason))  
          
          # Bring together challenge superlatives
          individualchallsuperlatives <-full_join(
            full_join(
              full_join(
                full_join(
                  full_join(
                    full_join(full_join(indivimmwinsoneseason,indivimmwins),
                              indivrewwinsoneseason),indivrewwins),
                  tribalimmwinsoneseason),tribalimmwins),
              tribalrewwinsoneseason),tribalrewwins) %>%
              # drop the Auction "wins"
              filter(castaway_id != "USNA")
    
    ## B. Tribal Council information  
        ## number of times went to tc before merge
        premerge <- votehx %>% filter(tribe_status != "Merged") %>%
          select(version,version_season,season,castaway_id,day) %>%
          group_by(version,version_season,season,castaway_id) %>%
          summarize(premergeTC=n_distinct(day))
        
        ## total TCs attended
        totaltcs <- votehx %>% 
          select(version,version_season,season,castaway_id,day) %>%
          group_by(version,version_season,season,castaway_id) %>%
          summarize(totalTCattended=n_distinct(day))
        
        ## Number of TCs at which received votes
        ## Filter out the non-person votes
        receivedvotesat <- votehx %>% select(version,version_season,vote_id,day) %>%
                          filter(!(is.na(vote_id))) %>%
                          rename(castaway_id=vote_id) %>%
                          group_by(version,version_season,castaway_id) %>%
                          summarise(tcsatwhichreceivedvotes=n_distinct(day))
        
              ## double checking by looking pre and post merge
              receivedvotespremerge <- votehx %>% select(version,version_season,vote_id,day,tribe_status) %>%
                filter(!(is.na(vote_id)) & tribe_status != "Merged") %>%
                rename(castaway_id=vote_id) %>%
                group_by(version,version_season,castaway_id) %>%
                summarise(tcsatwhichreceivedvotespremerge=n_distinct(day))
              
              receivedvotespostmerge <- votehx %>% select(version,version_season,vote_id,day,tribe_status) %>%
                filter(!(is.na(vote_id)) & tribe_status == "Merged") %>%
                rename(castaway_id=vote_id) %>%
                group_by(version,version_season,castaway_id) %>%
                summarise(tcsatwhichreceivedvotespostmerge=n_distinct(day))
              
              receivedvotesatTCs <- full_join(full_join(receivedvotesat,receivedvotespremerge),receivedvotespostmerge)
              
        ## Days until first vote was received      
          dayoffirstvotereceived <- votehx %>% select(version,version_season,day,vote_id) %>%
                                      filter(!(is.na(vote_id))) %>%
                                      group_by(version,version_season,vote_id) %>%
                                      summarize(dayoffirstvote=min(day)) %>%
                                      rename(castaway_id=vote_id)
        
        ## number of votes received (in one season and across seasons)
        votesreceivedinoneseason <- votehx %>% select(version,version_season,season,vote_id) %>%
          group_by(version,version_season,season,vote_id) %>%
          mutate(count=1) %>%
          summarize(votesreceivedoneseason=sum(count)) %>%
          rename(castaway_id=vote_id)
        votesreceived <- votesreceivedinoneseason %>%
          group_by(castaway_id) %>%
          summarise(votesreceived=sum(votesreceivedoneseason)) 
        
        ## number of votes nullified (in one season and across seasons)
        votesnullifiedoneseason <- votehx %>% 
          filter(nullified == TRUE) %>%
          select(version,version_season,season,vote_id) %>%
          group_by(version,version_season,season,vote_id) %>%
          mutate(count=1) %>%
          summarize(votesnullifiedoneseason=sum(count)) %>%
          rename(castaway_id=vote_id)  
        votesnullified <- votesnullifiedoneseason %>%
          group_by(castaway_id) %>%
          summarize(votesnullified=sum(votesnullifiedoneseason))
        
        ## number of votes cast per season
        votescast <- votehx %>% 
          filter(!(is.na(vote_id))) %>%
          select(version,version_season,season,castaway_id) %>%            
          mutate(count=1) %>%
          group_by(version,version_season,season,castaway_id) %>%
          summarize(votescast=sum(count))
        
        ## number of accurate votes cast per season  
        accuratevotescast <- votehx %>% 
          filter(!(is.na(vote_id))) %>%
          select(version,version_season,season,castaway_id,vote_id,voted_out_id) %>%    
          filter(vote_id == voted_out_id) %>%
          mutate(count=1) %>%
          group_by(version,version_season,season,castaway_id) %>%
          summarize(accuratevotescast=sum(count))      
        
        # Bring tribal council superlatives together
          tribalcouncilindivsuper <- full_join(full_join(full_join(full_join(
            full_join(
              full_join(
                full_join(
                  full_join(
                    full_join(premerge,votesreceivedinoneseason),votesreceived),
                  votesnullifiedoneseason),votesnullified),
              votescast),accuratevotescast),totaltcs),receivedvotesatTCs),dayoffirstvotereceived) %>%
            group_by(version,version_season,season,castaway_id) %>%
            mutate(percentaccuracy=accuratevotescast/votescast)
    
    ## C. Idol information  
    ## number of idols played
    ## number of idols found
    ## Etc.
    ## This section is not yet built out because Dan and I are modifying the hidden idol data.
    
    ## D. Confessionals
        # Total confessionals in a season
        indivconftotal <- confessionals %>% group_by(version,version_season,season,castaway_id) %>%
          summarize(confessionaltotal=sum(confessional_count))
        
        # Average confessionals per episode they were in
        avgperepi <- confessionals %>% group_by(version,version_season,season,castaway_id) %>%
          summarize(appearin=max(episode)) %>%
          right_join(indivconftotal) %>%
          mutate(averageperepisode=confessionaltotal/appearin) %>%
          select(version,version_season,season,castaway_id,averageperepisode)
        
        # confessional average for the same result
        confessionalavgbyresult <- confessionals %>% group_by(version,result) %>% 
          summarize(totalconfessionals=sum(confessional_count),totalpeople=n_distinct(castaway_id)) %>%
          mutate(resultavgconf=totalconfessionals/totalpeople) %>%
          select(version,result,resultavgconf)
        
        # confessional average for the same result by gender and race
        confessionalavgbyresultGenPoc <- confessionals %>% group_by(version,result,gender,poc) %>% 
          summarize(totalconfessionals=sum(confessional_count),totalpeople=n_distinct(castaway_id)) %>%
          mutate(resultgenderpocavgconf=totalconfessionals/totalpeople) %>%
          select(version,result,gender,poc,resultgenderpocavgconf)  
        
        ## Bring the two averages about result together
        confessionalaverages <- full_join(confessionalavgbyresultGenPoc,confessionalavgbyresult)
    
    ## E. Overall information
        ## number of times played
        timesplayed<- castaways %>% select(version,version_season,season,castaway_id) %>% 
          distinct() %>%
          group_by(version,castaway_id) %>%
          summarize(numberoftimesplayed=n_distinct(season))
        
        ## number of days played
        daysplayed <- castaways %>% select(version,version_season,season,castaway_id,day) %>% 
          group_by(version,version_season,season,castaway_id) %>%
          summarize(maxdayinseason=max(day)) %>%
          group_by(version,castaway_id) %>%
          summarize(totaldaysoverall=sum(maxdayinseason))
        
        ## Made case to jury
        timesmadecasetojury <- castaways %>% select(version,version_season,season,castaway_id,result) %>%
          filter(result %in% c("Co-runner-up","Runner-up","2nd Runner-up","2nd runner-up","Sole Survivor")) %>%
          mutate(count=1) %>% select(!result) %>% distinct() %>%
          group_by(version,castaway_id) %>% 
          summarize(madecase=sum(count))
        
        ## Made merge
        timesmademerge <- castaways %>% select(version,version_season,season,castaway_id,merged_tribe) %>%
          filter(!(is.na(merged_tribe))) %>%
          mutate(count=1) %>% select(!merged_tribe) %>% distinct() %>%
          group_by(version,castaway_id) %>% 
          summarize(mademerge=sum(count))
        
        ## Sat on jury
        timessatonjury <- castaways %>% select(version,version_season,season,castaway_id,jury_status) %>%
          filter(!(is.na(jury_status))) %>%
          mutate(count=1) %>% select(!jury_status) %>% distinct() %>%
          group_by(version,castaway_id) %>% 
          summarize(satonjury=sum(count))        

## Data cleaning    
    ## Bring data together  
        individualsuperlatives <- full_join(full_join(full_join(full_join(full_join(full_join(
          full_join(
            full_join(individualchallsuperlatives,tribalcouncilindivsuper),
            timesplayed),daysplayed),
          indivconftotal),avgperepi),
          timesmadecasetojury),timesmademerge),timessatonjury) %>%
          filter(!(is.na(season)))
    
    # Have things be 0 instead of NA
        for (var in 5:dim(individualsuperlatives)[2]) {
          individualsuperlatives[is.na(individualsuperlatives[,var]),var] <- 0
        }
        #individualsuperlatives <- individualsuperlatives %>% pivot_longer(!(c("version","version_season","season","castaway_id")),names_to="variable",values_to="value")
    
    # Add in additional variables
        finaldata <- castaways %>% group_by(version,version_season,season,castaway_id) %>%
          filter(day==max(day)) %>% 
          select(version,version_season,season,season_name,castaway_id,full_name,castaway,age,day,order,result,jury_status) %>%
          full_join(individualsuperlatives) %>%
          full_join(castawaydetails %>% select(castaway_id,gender,poc)) 
        finaldata <- finaldata %>% filter(castaway_id != "USNA")
        
        ## Add on result and gender-race-result specific confessional data   
        finaldata <- full_join(finaldata,confessionalaverages)
        
        # Add marker as to if it is a season-specific statistic or cross-season
        #finaldata$superlativetype <- "Individual by season"
        #finaldata$superlativetype[finaldata$variable %in% c("immwins","numberoftimesplayed","rewwins","totaldaysoverall","tribalimmwins","tribalrewwins","votesreceived","votesnullified")] <- "Individual across seasons"
        
    ## for days until first vote, if they never received a vote, replace the 0 with the day they lasted in the game (should only be relevant for people who made it to final tribal council, people who were medi-vac'd, elimiinated by rocks or idoled out)
      finaldata$dayoffirstvote[finaldata$dayoffirstvote %in% 0 & !(is.na(finaldata$day))] <- finaldata$day[finaldata$dayoffirstvote %in% 0 & !(is.na(finaldata$day))]
      
# save data
write.csv(finaldata,paste(savedir,"survivoR_supeRlatives.csv",sep=""),row.names=F)

