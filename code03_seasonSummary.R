## Carly Levitz
## Written 2022-01-09
## Updated 2022-01-22
## Purpose: get season-specific information

rm(list=ls())
library(tidyverse); library(survivoR); library(ggpubr)
savedir <- "H:/R/survivoR/02_cleaned_data/"

tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
  confessionals <- confessionals[,c("season_name","season","episode","castaway_id","confessional_count")]
castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)

###################
## Color schemes
greens <- c("#9FB8A2","#3AA845","#305844","#2A3B32","#101C16")
reds <- c("#F1E48A","#F58406","#932306","#4B0A06","#060304")
theme_set(theme_classic())

############################################################################################
## Data set up
  ## Let's get a simplified dataset that is just season, day, castaway_id, and tribe name & status
    tribesimpl <- unique(tribemap[!(is.na(tribemap$castaway_id)),c("season","day","castaway_id","tribe","tribe_status")])
    
    # for edge of extinction & redemption, want that to show up in tribe name
      tribesimpl$tribe[tribesimpl$tribe_status == "Edge of Extinction"] <- "Edge of Extinction"
      tribesimpl$tribe[tribesimpl$tribe_status == "Redemption Island"] <- "Redemption Island"
      

  ## Add on additional information that will help us order the data
    tribesimpl <- left_join(tribesimpl,
                            castaways[!(is.na(castaways$castaway_id)),c(1:5,15:18)],
                            by=c("season","castaway_id"))

  ## Arrange the y-values based on original tribe, then swapped tribe, then second swapped, then merged, then redemption, then EoE
      arrangement <- function(tribestatuscateg) {
        # for now, just keep one stage/phase of the game
        tempdata <- tribesimpl[tribesimpl$tribe_status == tribestatuscateg,]
        
        # organize the Y values within a season
        for (sn in seq(1,max(tempdata$season),1)) {
          
          # This counter will tick up and will become the Y value.
          tempcount <- 1
          
          # Want each castaway to be their own line, and organize the original Ys by their original tribe
          for (tr in unique(tempdata$tribe[tempdata$season == sn])) {
            for (c in unique(tempdata$castaway_id[tempdata$tribe == tr & tempdata$season == sn])) {
              
              tempdata$y[tempdata$tribe == tr & tempdata$season == sn & tempdata$castaway_id==c] <- tempcount
              # increase the y value for each subsequent castawway
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
                             arrangement(tribestatuscateg="Merged"),
                             arrangement(tribestatuscateg="Redemption Island"),
                             arrangement(tribestatuscateg="Edge of Extinction"))
      
###################################################################################################
## The graph will depend on the type of season
    seasons <- unique(tribemap[,c("season","tribe_status")])
    seasons$temp <- 1
    seasons <- reshape(seasons,
                       direction="wide",
                       timevar="tribe_status",
                       idvar="season")
    # Different types of seasons
      noswapsonlymerge <- seasons$season[is.na(seasons$temp.Swapped) & is.na(seasons$temp.Swapped_2) & is.na(seasons$`temp.Redemption Island`) & is.na(seasons$`temp.Edge of Extinction`)]
      oneswap  <- seasons$season[!(is.na(seasons$temp.Swapped)) & is.na(seasons$temp.Swapped_2) & is.na(seasons$`temp.Redemption Island`) & is.na(seasons$`temp.Edge of Extinction`)]
      twoswaps  <- seasons$season[!(is.na(seasons$temp.Swapped)) & !(is.na(seasons$temp.Swapped_2)) & is.na(seasons$`temp.Redemption Island`) & is.na(seasons$`temp.Edge of Extinction`)]
      onlyredemption <- seasons$season[seasons$`temp.Redemption Island` == 1 & !(is.na(seasons$`temp.Redemption Island`)) & is.na(seasons$temp.Swapped)]
      redemptionandswap <- seasons$season[seasons$`temp.Redemption Island` == 1 & !(is.na(seasons$`temp.Redemption Island`)) & !(is.na(seasons$temp.Swapped))]
      extinctionandswap <- seasons$season[seasons$`temp.Edge of Extinction` == 1 & !(is.na(seasons$`Edge of Extinction`)) & !(is.na(seasons$temp.Swapped))]

########################################################################      
## One Swap graph   

  graphOneSwapSeason <- function(selectedseason) {      
          
      ## Prep the data for graphing
        graphdata <- tribemapreorg[tribemapreorg$season == selectedseason,] 
        dayofswap <- min(graphdata$day[graphdata$tribe_status == "Swapped"])
        dayofmerge <- min(graphdata$day[graphdata$tribe_status == "Merged"])
        
        # Get label for just the first and last datapoint
          graphdata$labeltext <- ""
          for (c in unique(graphdata$castaway_id)) {
            minday <- min(graphdata$day[graphdata$castaway_id == c],na.rm=T)
            temptext <- graphdata$castaway[graphdata$day == minday & graphdata$castaway_id == c]
            graphdata$labeltext[graphdata$day == minday & graphdata$castaway_id == c] <- temptext
            
            maxday <- max(graphdata$day[graphdata$castaway_id == c],na.rm=T)
            
              graphdata$labeltext[graphdata$day == maxday & graphdata$castaway_id == c] <- temptext
            
          }
          
      ## Graph
        swapone <-   
        graphdata %>%
          ## Basic plot
              ggplot(aes(x=day,y=y,color=original_tribe,group=castaway_id)) +
                  geom_line(aes(color=original_tribe)) +
                  geom_point(aes(x=day,y=y,fill=original_tribe,group=castaway_id)) +
                  geom_text(aes(label=labeltext,y=y),nudge_x=-1,show.legend = FALSE,color="black") +
              scale_fill_tribes(selectedseason,tribe=tribesimpl$original_tribe) +
              #scale_y_continuous(breaks=unique(graphdata$y),
              #                 labels=unique(graphdata$castaway)) +
              scale_x_continuous(breaks=seq(1,max(graphdata$day),1),
                                 labels=seq(1,max(graphdata$day),1)) +      
              labs(
                x="Day of game",
                color = "Original tribe",
                title = paste("Tribe mapping for ",unique(graphdata$season_name),sep=""),
                subtitle = paste("(Season ",selectedseason,")",sep="")
              ) +
              theme(
                plot.background = element_blank(),
                plot.title = element_text(color="black",size=16,face="bold"),
                plot.subtitle = element_text(color="black",size=12),
    
                panel.grid = element_blank(),
                panel.background = element_blank(),
                panel.border = element_blank(),            
                
                axis.ticks.x = element_line(color="black"),
                axis.text.x = element_text(color="black",size=12),
                axis.title.x = element_text(color="black",size=12,face="bold"),
                axis.line.x = element_line(color="black"),
                
                axis.ticks.y = element_blank(),
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                
                legend.background = element_rect(color="white"),
                legend.title=element_text(face="bold")
              ) +
              guides(fill="none")+
          ## Swap day
              geom_vline(xintercept=dayofswap-.1, linetype="dashed", color = reds[3]) +
              #annotate("text",x=dayofswap,"Day of swap") +
          ## Merge day
              geom_vline(xintercept=dayofmerge-.1, linetype="dashed", color = reds[3]) 
              #annotate("text",x=dayofmerge,"Day of merge") 
        print(swapone)
        
    } # close One Swap graphing function

########################################################################      
## Two Swap graph   
  graphTwoSwapSeason <- function(selectedseason)   {
    dayofSecondSwap <- min(tribemapreorg$day[tribemapreorg$tribe_status == "Swapped_2" & tribemapreorg$season == selectedseason])
    
    secondswapgraph <- 
        graphOneSwapSeason(selectedseason) +
          ## Swap day
          geom_vline(xintercept=dayofSecondSwap-.1, linetype="dashed", color = reds[3])  
    print(secondswapgraph)
      
  } # closing Two Swap graphing function

## Things I want to do    
## Size of dot = confessionals?
## Can I make the text labels on the Right be to the Right of the dot?
## Bring on info about challenges
## Label names of tribes at swap?
## Add info about season (summary, etc.)
## Add info about who makes the jury      
      
      
selectedseason <- 14
graphOneSwapSeason(selectedseason)
selectedseason <- 13
graphTwoSwapSeason(selectedseason)

