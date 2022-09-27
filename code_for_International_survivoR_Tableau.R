## Author: Carly Levitz
## Date written: 2021-12-09
## Date updated: 2022-09-27
## Purpose: data cleaning of Survivor data  - prep the data from https://github.com/doehm/survivoR and then use these data in Tableau
##    https://public.tableau.com/app/profile/carly.levitz/viz/SurvivorCBSData-Acknowledgements/Acknkowledgements
## File organization:
##    Section 1: Clean the data for how I want to use it in Tableau
##    Section 2: Create a dataset for the tribe mapping. It's pretty inelegant but does the trick.
##    Section 3: Calculate different types of superlatives for each castaway. Superlatives can be within a season (e.g., most votes received in one season) or across seasons (e.g., most individual immunity wins across seasons played)

rm(list=ls()); .libPaths("C:/Program Files/R/R-4.1.1/library")
#install.packages("rlang", .libPaths("C:/Program Files/R/R-4.1.1/library"))

library(devtools,lib="C:/Program Files/R/R-4.1.1/library"); library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library"); library(janitor,lib="C:/Program Files/R/R-4.1.1/library")
devtools::install_github("doehm/survivoR")



#dat <- import_non_us_data()
library(survivoR,lib="C:/Program Files/R/R-4.1.1/library")
savedir <- "H:/R/survivoR/02_cleaned_data/"


#advdetail <- survivoR::advantage_details
advdetail <- advantage_details 
advmvmt <- advantage_movement 
castaways <- castaways 
castawaydetails <- castaway_details 
challenges <- challenge_results
challengesdesc <- challenge_description 
vote_history <- vote_history 
confessionals <- confessionals 
season_summary <- season_summary 
jury_votes <- jury_votes 
tribe_mapping <- tribe_mapping 
bootmapping <- boot_mapping 
viewers <- viewers 
tribe_palettes <- tribe_colours 

## Have raw international data for review

      ###############################################################
    ## save the data as separate datasets before using them to create additional datasets
    #write.csv(castawaydetails,paste(savedir,"RawNonUSsurvivoR_01_castawayDetails_cleaned.csv",sep=""),row.names=F)
    #write.csv(castaways,paste(savedir,"RawNonUSsurvivoR_02_castaways_cleaned.csv",sep=""),row.names=F)
    #write.csv(season_summary,paste(savedir,"RawNonUSsurvivoR_03_seasonSummary_cleaned.csv",sep=""),row.names=F)
    #write.csv(vote_history,paste(savedir,"RawNonUSsurvivoR_04_votehx_cleaned.csv",sep=""),row.names=F)
    #write.csv(challenges,paste(savedir,"RawNonUSsurvivoR_05_challenges_cleaned.csv",sep=""),row.names=F)
    #write.csv(confessionals,paste(savedir,"RawNonUSsurvivoR_06_confessionals_cleaned.csv",sep=""),row.names=F)
    #write.csv(advmvmt,paste(savedir,"RawNonUSsurvivoR_07a_advantagesMvmt_cleaned.csv",sep=""),row.names=F)
    #write.csv(advdetail,paste(savedir,"RawNonUSsurvivoR_07b_advantagesDetail_cleaned.csv",sep=""),row.names=F)
    #write.csv(jury_votes,paste(savedir,"RawNonUSsurvivoR_08_juryvotes_cleaned.csv",sep=""),row.names=F)
    #write.csv(viewers,paste(savedir,"RawNonUSsurvivoR_09_viewers_cleaned.csv",sep=""),row.names=F)
    #write.csv(tribe_mapping,paste(savedir,"RawNonUSsurvivoR_10_tribemap_cleaned.csv",sep=""),row.names=F)
    #write.csv(bootmapping,paste(savedir,"RawNonUSsurvivoR_11_bootmap_cleaned.csv",sep=""),row.names=F)
    #write.csv(tribe_palettes,paste(savedir,"RawNonUSsurvivoR_12_tribecolors_cleaned.csv",sep=""),row.names=F)
    
#############################################################################################################################
#############################################################################################################################
## Section 1: Clean the data for how I want to use it in Tableau
#############################################################################################################################
#############################################################################################################################  

  # clean the result data
    castaways$result[castaways$result %in% c("2nd Runner-up","2nd runner-up")] <- "2nd runner-up"
    castaways$result[castaways$result %in% c("Runner up")] - "Runner-up"
    castaways$result[castaways$result %in% c("Sole survivor")] <- "Sole Survivor"
    castaways$day[castaways$version_season == "SA09" & castaways$castaway %in% c("Shane","Dino")] <- 39
    
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Andy")] <- 2
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Briana")] <- 5
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Kate")] <- 7
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Alex")] <- 12
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Jay")] <- 14
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Sandra")] <- 16
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Sophie")] <- 18
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Amy")] <- 20
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Nina")] <- 22
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Croc")] <- 23
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Ben")] <- 24
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Khanh")] <- 27
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Mel")] <- 28
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Jesse")] <- 30
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Michelle")] <- 36
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Jordan")] <- 39
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Sam")] <- 40
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("David")] <- 42
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Jordie")] <- 43
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("KJ")] <- 45
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Josh")] <- 46
    castaways$day[castaways$version_season == "AU07" & castaways$castaway %in% c("Mark","Shay","Chrissy")] <- 47

  ## My data source (survivor wiki fandom) only have pages for Asian, Latin American, and Black. 
  ## I found a reddit page that had Jewish players. 
  ## On Natalie Bolton's page, it mentioned that she was Native American, and the page called out 3 or 4 other Native American players.
  castawaydetails$race[is.na(castawaydetails$race) | castawaydetails$race == ""] <- "Unknown"
  
    # edit to gender, race, and ethnicity data
    # Peih-Gee in Season 31 is missing ethnicity data
      castawaydetails$ethnicity[castawaydetails$castaway == "Peih-Gee" & is.na(castawaydetails$ethnicity)] <- "Hong Konger American"
    
    # Want to distinguish between Black players and other players of color
      castawaydetails$poc[grep("Black",castawaydetails$race)] <- "Black"
  
    
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
    castawaydetails$castaway[castawaydetails$castaway_id == "US0009"] <- "Jenna L."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0013"] <- "Sue"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0045"] <- "Big Tom"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0059"] <- "The General"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0094"] <- "Rob C."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0096"] <- "Jenna M."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0111"] <- "Jonny Fairplay"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0118"] <- "Bubba"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0122"] <- "Sarge"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0190"] <- "Flicka"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0206"] <- "Papa Smurf"
    castawaydetails$castaway[castawaydetails$castaway_id == "US0288"] <- "Russell S."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0292"] <- "Laura M."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0300"] <- "Russell H."
    castawaydetails$castaway[castawaydetails$castaway_id == "US0314"] <- "Purple Kelly"    
    
    
    ## Challenges
    challenges$castaway[challenges$castaway_id == "US0009"] <- "Jenna L."
    challenges$castaway[challenges$castaway_id == "US0013"] <- "Sue"
    challenges$castaway[challenges$castaway_id == "US0045"] <- "Big Tom"
    challenges$castaway[challenges$castaway_id == "US0059"] <- "The General"
    challenges$castaway[challenges$castaway_id == "US0094"] <- "Rob C."
    challenges$castaway[challenges$castaway_id == "US0096"] <- "Jenna M."
    challenges$castaway[challenges$castaway_id == "US0111"] <- "Jonny Fairplay"
    challenges$castaway[challenges$castaway_id == "US0118"] <- "Bubba"
    challenges$castaway[challenges$castaway_id == "US0122"] <- "Sarge"
    challenges$castaway[challenges$castaway_id == "US0190"] <- "Flicka"
    challenges$castaway[challenges$castaway_id == "US0206"] <- "Papa Smurf"
    challenges$castaway[challenges$castaway_id == "US0288"] <- "Russell S."
    challenges$castaway[challenges$castaway_id == "US0292"] <- "Laura M."
    challenges$castaway[challenges$castaway_id == "US0300"] <- "Russell H."
    challenges$castaway[challenges$castaway_id == "US0314"] <- "Purple Kelly"    
    
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
    
    ## boot mapping
    bootmapping$castaway[bootmapping$castaway_id == "US0009"] <- "Jenna L."
    bootmapping$castaway[bootmapping$castaway_id == "US0013"] <- "Sue"
    bootmapping$castaway[bootmapping$castaway_id == "US0045"] <- "Big Tom"
    bootmapping$castaway[bootmapping$castaway_id == "US0059"] <- "The General"
    bootmapping$castaway[bootmapping$castaway_id == "US0094"] <- "Rob C."
    bootmapping$castaway[bootmapping$castaway_id == "US0096"] <- "Jenna M."
    bootmapping$castaway[bootmapping$castaway_id == "US0111"] <- "Jonny Fairplay"
    bootmapping$castaway[bootmapping$castaway_id == "US0118"] <- "Bubba"
    bootmapping$castaway[bootmapping$castaway_id == "US0122"] <- "Sarge"
    bootmapping$castaway[bootmapping$castaway_id == "US0190"] <- "Flicka"
    bootmapping$castaway[bootmapping$castaway_id == "US0206"] <- "Papa Smurf"
    bootmapping$castaway[bootmapping$castaway_id == "US0288"] <- "Russell S."
    bootmapping$castaway[bootmapping$castaway_id == "US0292"] <- "Laura M."
    bootmapping$castaway[bootmapping$castaway_id == "US0300"] <- "Russell H."
    bootmapping$castaway[bootmapping$castaway_id == "US0314"] <- "Purple Kelly"   
    
    # Idols and advantages
    advmvmt$castaway[advmvmt$castaway_id == "US0009"] <- "Jenna L."
    advmvmt$castaway[advmvmt$castaway_id == "US0013"] <- "Sue"
    advmvmt$castaway[advmvmt$castaway_id == "US0045"] <- "Big Tom"
    advmvmt$castaway[advmvmt$castaway_id == "US0059"] <- "The General"
    advmvmt$castaway[advmvmt$castaway_id == "US0094"] <- "Rob C."
    advmvmt$castaway[advmvmt$castaway_id == "US0096"] <- "Jenna M."
    advmvmt$castaway[advmvmt$castaway_id == "US0111"] <- "Jonny Fairplay"
    advmvmt$castaway[advmvmt$castaway_id == "US0118"] <- "Bubba"
    advmvmt$castaway[advmvmt$castaway_id == "US0122"] <- "Sarge"
    advmvmt$castaway[advmvmt$castaway_id == "US0190"] <- "Flicka"
    advmvmt$castaway[advmvmt$castaway_id == "US0206"] <- "Papa Smurf"
    advmvmt$castaway[advmvmt$castaway_id == "US0288"] <- "Russell S."
    advmvmt$castaway[advmvmt$castaway_id == "US0292"] <- "Laura M."
    advmvmt$castaway[advmvmt$castaway_id == "US0300"] <- "Russell H."
    advmvmt$castaway[advmvmt$castaway_id == "US0314"] <- "Purple Kelly"       
    
    advmvmt$played_for[advmvmt$played_for_id == "US0009"] <- "Jenna L."
    advmvmt$played_for[advmvmt$played_for_id == "US0013"] <- "Sue"
    advmvmt$played_for[advmvmt$played_for_id == "US0045"] <- "Big Tom"
    advmvmt$played_for[advmvmt$played_for_id == "US0059"] <- "The General"
    advmvmt$played_for[advmvmt$played_for_id == "US0094"] <- "Rob C."
    advmvmt$played_for[advmvmt$played_for_id == "US0096"] <- "Jenna M."
    advmvmt$played_for[advmvmt$played_for_id == "US0111"] <- "Jonny Fairplay"
    advmvmt$played_for[advmvmt$played_for_id == "US0118"] <- "Bubba"
    advmvmt$played_for[advmvmt$played_for_id == "US0122"] <- "Sarge"
    advmvmt$played_for[advmvmt$played_for_id == "US0190"] <- "Flicka"
    advmvmt$played_for[advmvmt$played_for_id == "US0206"] <- "Papa Smurf"
    advmvmt$played_for[advmvmt$played_for_id == "US0288"] <- "Russell S."
    advmvmt$played_for[advmvmt$played_for_id == "US0292"] <- "Laura M."
    advmvmt$played_for[advmvmt$played_for_id == "US0300"] <- "Russell H."
    advmvmt$played_for[advmvmt$played_for_id == "US0314"] <- "Purple Kelly"       
    
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
    
    ## bootmapping
    bootmapping$castaway[bootmapping$castaway_id == "US0009"] <- "Jenna L."
    bootmapping$castaway[bootmapping$castaway_id == "US0013"] <- "Sue"
    bootmapping$castaway[bootmapping$castaway_id == "US0045"] <- "Big Tom"
    bootmapping$castaway[bootmapping$castaway_id == "US0059"] <- "The General"
    bootmapping$castaway[bootmapping$castaway_id == "US0094"] <- "Rob C."
    bootmapping$castaway[bootmapping$castaway_id == "US0096"] <- "Jenna M."
    bootmapping$castaway[bootmapping$castaway_id == "US0111"] <- "Jonny Fairplay"
    bootmapping$castaway[bootmapping$castaway_id == "US0118"] <- "Bubba"
    bootmapping$castaway[bootmapping$castaway_id == "US0122"] <- "Sarge"
    bootmapping$castaway[bootmapping$castaway_id == "US0190"] <- "Flicka"
    bootmapping$castaway[bootmapping$castaway_id == "US0206"] <- "Papa Smurf"
    bootmapping$castaway[bootmapping$castaway_id == "US0288"] <- "Russell S."
    bootmapping$castaway[bootmapping$castaway_id == "US0292"] <- "Laura M."
    bootmapping$castaway[bootmapping$castaway_id == "US0300"] <- "Russell H."
    bootmapping$castaway[bootmapping$castaway_id == "US0314"] <- "Purple Kelly"
    
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

    # not all tribes are filled in; do that manually
      vote_history$tribe[vote_history$version_season == "US02" & vote_history$day == 41] <- "Barramundi"
      vote_history$tribe[vote_history$version_season == "US42" & vote_history$tribe == "Merged"] <- "Kula Kula"
      
    # want all tribe names in the castaway data set  
      castawaystribes <- tribe_mapping %>% 
                          select(version_season,castaway_id,tribe,tribe_status,day) %>%
                          filter(tribe != "Redemption Island" & tribe != "Edge of Extinction" & tribe != "Black" & tribe != "Exile Island" & tribe != "Redemption Rock" & tribe != "None" & tribe != "Dead Man's Island") %>%
                          group_by(version_season,castaway_id,tribe_status) %>%
                          filter(day==min(day)) %>%
                          select(!day) %>%
                          distinct() %>%
                          pivot_wider(values_from=tribe,names_from = tribe_status)
      
      castawaystribes$Merged[castawaystribes$version_season == "US41" & !(is.na(castawaystribes$Merged))] <- "Via Kana"
      castawaystribes$Merged[castawaystribes$version_season == "US42" & !(is.na(castawaystribes$Merged))] <- "Kula Kula"
      
      castaways <- left_join(castaways %>% select(!original_tribe),castawaystribes)
      
## Add Additional Information to all tables
  fullnames <- castawaydetails %>% select(full_name,castaway_id,castaway,gender,race,ethnicity,poc) %>%
                  distinct() %>%
               left_join(  
                  castaways %>% select(castaway_id,version_season,age,day,order,result,episode,jury_status) %>%
                    group_by(version_season,castaway_id) %>%
                    mutate(maxday=max(day)) %>%
                    filter(day==maxday) %>% select(!maxday)
               )
  
  
  advmvmt2 <- left_join(advmvmt,fullnames %>% rename(daylasteduntil=day,episodelastedthrough=episode),
                        by=c("version_season","castaway","castaway_id")) %>%
              left_join(fullnames %>% select(version_season,full_name,castaway,castaway_id,gender,poc,order,result,jury_status) %>%
                          rename(played_for=castaway,played_for_id=castaway_id,played_for_full_name=full_name,played_for_gender=gender,played_for_poc=poc,played_for_order=order,played_for_result=result,played_for_jury_status=jury_status))
  
  bootmapping2 <- left_join(bootmapping,fullnames %>% rename(daylasteduntil=day,episodelastedthrough=episode,ordervotedoff=order),
                            by=c("version_season","castaway","castaway_id")) 
  
  confessionals2 <- left_join(confessionals,fullnames %>% rename(daylasteduntil=day,episodelastedthrough=episode),
                              by=c("version_season","castaway","castaway_id"))
  
  jury_votes2 <- left_join(jury_votes,fullnames %>% rename(daylasteduntil=day,episodelastedthrough=episode),
                              by=c("version_season","castaway","castaway_id")) %>%
                      left_join(fullnames %>% 
                                  select(version_season,castaway_id,castaway,full_name,gender,poc,age,order,result,episode) %>%
                                  rename(finalist_id=castaway_id,finalist=castaway,finalist_full_name = full_name,finalist_gender=gender,finalist_poc=poc,finalist_age=age,finalist_order=order,finalist_result=result,finalist_episode =episode))
                    
  tribe_mapping2 <- left_join(tribe_mapping,fullnames %>% rename(daylasteduntil=day,episodelastedthrough=episode),
                            by=c("version_season","castaway","castaway_id")) 
  
  vote_history2 <- left_join(vote_history,
                             fullnames %>% 
                               rename(episodelasteduntil=episode,daylasteduntil=day,ordervotedoff=order)) %>%
                  left_join(fullnames %>%
                              rename_with(~paste(.x,"vote",sep="")) %>%
                              rename(vote_id=castaway_idvote,vote=castawayvote,version_season=version_seasonvote)) %>%
                  left_join(fullnames %>%
                              rename_with(~paste(.x,"voted_out",sep="")) %>%
                              rename(voted_out_id=castaway_idvoted_out,voted_out=castawayvoted_out,version_season=version_seasonvoted_out)) 
  

      
  challenges2 <- left_join(challenges,
                           fullnames %>% 
                             rename(daylasteduntil=day,episodelastedthrough=episode,bootorder=order)) 
  
      
      

###############################################################
## save the data as separate datasets before using them to create additional datasets
write.csv(castawaydetails,paste(savedir,"survivoR_01_castawayDetails_cleaned.csv",sep=""),row.names=F)
write.csv(castaways,paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),row.names=F)
write.csv(season_summary,paste(savedir,"survivoR_03_seasonSummary_cleaned.csv",sep=""),row.names=F)
write.csv(vote_history2,paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),row.names=F)
write.csv(challenges2,paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),row.names=F)
write.csv(confessionals2,paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),row.names=F)
write.csv(advmvmt2,paste(savedir,"survivoR_07a_advantagesMvmt_cleaned.csv",sep=""),row.names=F)
write.csv(advdetail,paste(savedir,"survivoR_07b_advantagesDetail_cleaned.csv",sep=""),row.names=F)
write.csv(jury_votes2,paste(savedir,"survivoR_08_juryvotes_cleaned.csv",sep=""),row.names=F)
write.csv(viewers,paste(savedir,"survivoR_09_viewers_cleaned.csv",sep=""),row.names=F)
write.csv(tribe_mapping2,paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),row.names=F)
write.csv(bootmapping2,paste(savedir,"survivoR_11_bootmap_cleaned.csv",sep=""),row.names=F)
    
#############################################################################################################################
#############################################################################################################################
## Section 2: Individual superlatives
#############################################################################################################################
#############################################################################################################################

rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library");  library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library")
savedir <- "H:/R/survivoR/02_cleaned_data/"


castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
castawaydetails <- read.csv(paste(savedir,"survivoR_01_castawayDetails_cleaned.csv",sep=""),header=T)
votehx <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
mvmt <- read.csv(paste(savedir,"survivoR_07a_advantagesMvmt_cleaned.csv",sep=""),header=T) %>%
          mutate(advtype = substr(advantage_id,3,4)) 

## Superlatives for an individual
    ## A. Challenge superlatives for the individual
          ## number of individual immunity wins in one season and overall
          indivimmwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
            filter(result == "Won") %>%
            filter(grepl("Immunity",challenge_type)) %>%
            select(version,version_season,season,castaway_id,challenge_name) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(immwinsOneSeason=n_distinct(challenge_name))
          indivimmwins <- indivimmwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(immwins = sum(immwinsOneSeason))
          
          ## number of individual reward wins (in one season and across seasons)
          indivrewwinsoneseason <- challenges %>% filter(outcome_type == "Individual") %>% 
            filter(result == "Won") %>%
            filter(grepl("Reward",challenge_type)) %>%
            select(version,version_season,season,castaway_id,challenge_name) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(rewwinsOneSeason=n_distinct(challenge_name))
          indivrewwins <- indivrewwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(rewwins = sum(rewwinsOneSeason))  
          
          ## number of tribal immunity wins (in one season and across seasons)
          tribalimmwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
            filter(result == "Won") %>%
            filter(grepl("Immunity",challenge_type)) %>%
            select(version,version_season,season,castaway_id,challenge_name) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(tribalimmwinsOneSeason=n_distinct(challenge_name))
          
          tribalimmwins <- tribalimmwinsoneseason %>% 
            group_by(castaway_id) %>%
            summarize(tribalimmwins = sum(tribalimmwinsOneSeason))
          
          
          ## number of tribal reward wins (in one season and across seasons)
          tribalrewwinsoneseason <- challenges %>% filter(outcome_type == "Tribal") %>% 
            filter(result == "Won") %>%
            filter(grepl("Reward",challenge_type)) %>%
            select(version,version_season,season,castaway_id,challenge_name) %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(tribalrewwinsOneSeason=n_distinct(challenge_name))
          
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
          summarize(votescast=sum(count)) %>%
          full_join(
            ## number of votes excluding ties
              votehx %>% 
                filter(!(is.na(vote_id)) & tie == FALSE) %>%
                select(version,version_season,season,castaway_id) %>%            
                mutate(count=1) %>%
                group_by(version,version_season,season,castaway_id) %>%
                summarize(votescastNonTie=sum(count))
            ) 
        
        ## number of accurate votes cast per season 
        ## take only the 
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
            mutate(percentaccuracyIncludingTies=accuratevotescast/votescast,
                   percentaccuracy=accuratevotescast/votescastNonTie)
    
    ## C. Idol information  
      ## IDOLS!!!!!!!!!!!!!
      idols <- 
        # Idols found in one season
        mvmt %>% 
        filter(event == "Found" & advtype == "HI") %>%
        group_by(version,version_season,season,castaway_id) %>%
        summarize(idolsfoundOneSeason = n()) %>%
        # Idols found across all seasons
        full_join(
          mvmt %>% 
            filter(event == "Found" & advtype == "HI") %>%
            group_by(version,castaway_id) %>%
            summarize(idolsfoundAllSeasons = n())
        ) %>%
        # Idols held at any time in one season
        full_join(
          mvmt %>% 
            filter(advtype == "HI") %>%
            group_by(version,version_season,season,castaway_id) %>%
            select(version,version_season,season,castaway_id,advantage_id) %>% 
            distinct() %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(idolsheldOneSeason = n())
        )     %>%
        # Idols held at any time across all seasons
        full_join(
          mvmt %>% 
            filter(advtype == "HI") %>%
            select(version,castaway_id,advantage_id) %>% 
            distinct() %>%
            group_by(version,castaway_id) %>%
            summarize(idolsheldAcrossSeasons = n())
        ) %>%
        # Idols played in one season
        full_join(
          mvmt %>% 
            filter(event == "Played" & advtype == "HI") %>%
            group_by(version,version_season,season,castaway_id) %>%
            summarize(idolsPlayedOneSeason = n())        
        ) %>%
        # Idols played across seasons
        full_join(
          mvmt %>% 
            filter(event == "Played" & advtype == "HI") %>%
            group_by(version,castaway_id) %>%
            summarize(idolsPlayedAcrossSeasons = n())        
        )  
      
      
    
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
        #confessionalavgbyresultGenPoc <- confessionals %>% group_by(version,result,gender,poc) %>% 
        #  summarize(totalconfessionals=sum(confessional_count),totalpeople=n_distinct(castaway_id)) %>%
        #  mutate(resultgenderpocavgconf=totalconfessionals/totalpeople) %>%
        #  select(version,result,gender,poc,resultgenderpocavgconf)  
        
        ## Bring the two averages about result together
        #confessionalaverages <- full_join(confessionalavgbyresultGenPoc,confessionalavgbyresult)
    
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
        timesmademerge <- castaways %>% select(version,version_season,season,castaway_id,Merged) %>%
          filter(!(is.na(Merged))) %>%
          mutate(count=1) %>% select(!Merged) %>% distinct() %>%
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
          full_join(idols) %>%
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
        finaldata <- finaldata %>% filter(!(castaway_id %in% c("USNA","SACaptain01","SACaptain02")) & !(is.na(castaway_id)) & !(is.na(season_name)))
        
        ## Add on result and gender-race-result specific confessional data   
        finaldata <- left_join(finaldata,confessionalavgbyresult)
        
        # Add marker as to if it is a season-specific statistic or cross-season
        #finaldata$superlativetype <- "Individual by season"
        #finaldata$superlativetype[finaldata$variable %in% c("immwins","numberoftimesplayed","rewwins","totaldaysoverall","tribalimmwins","tribalrewwins","votesreceived","votesnullified")] <- "Individual across seasons"
        
    ## for days until first vote, if they never received a vote, replace the 0 with the day they lasted in the game (should only be relevant for people who made it to final tribal council, people who were medi-vac'd, elimiinated by rocks or idoled out)
      finaldata$dayoffirstvote[finaldata$dayoffirstvote %in% 0 & !(is.na(finaldata$day))] <- finaldata$day[finaldata$dayoffirstvote %in% 0 & !(is.na(finaldata$day))]
      
# save data
write.csv(finaldata,paste(savedir,"survivoR_supeRlatives.csv",sep=""),row.names=F)

