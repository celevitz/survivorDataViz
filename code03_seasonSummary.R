## Carly Levitz
## Written 2022-01-09
## Updated 2022-01-22
## Purpose: get season-specific information
## This is part 3 of code, after I've already reformatted data from https://github.com/doehm/survivoR

rm(list=ls())
library(tidyverse); library(survivoR); library(ggpubr); library(ggrepel)
savedir <- "H:/R/survivoR/02_cleaned_data/"

tribemap <- read.csv(paste(savedir,"survivoR_10_tribemap_cleaned.csv",sep=""),header=T)
challenges <- read.csv(paste(savedir,"survivoR_05_challenges_cleaned.csv",sep=""),header=T)
  immune <- unique(challenges[grep("Immunity",challenges$challenge_type),c("season","day","winner_id")])
  names(immune) <- c("season","day","castaway_id")
  immune$immunity <- "Immune"
confessionals <- read.csv(paste(savedir,"survivoR_06_confessionals_cleaned.csv",sep=""),header=T)
  confessionals <- confessionals[,c("season_name","season","episode","castaway_id","confessional_count")]
castaways <- read.csv(paste(savedir,"survivoR_02_castaways_cleaned.csv",sep=""),header=T)
seasonsummary <- read.csv(paste(savedir,"survivoR_03_seasonSummary_cleaned.csv",sep=""),header=T)
vote_history <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)

###################
## Color schemes
greens <- c("#9FB8A2","#3AA845","#305844","#2A3B32","#101C16")
reds <- c("#F1E48A","#F58406","#932306","#4B0A06","#060304")
theme_set(theme_classic())

# for really long text, want to "wrap" it
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

############################################################################################
############################################################################################
## Data set up:
## Day by Day map ##########################################################################
############################################################################################
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
      # Who was immune on which day?
      tribemapreorg <- left_join(tribemapreorg,immune)
      tribemapreorg$immunity[is.na(tribemapreorg$immunity)] <- "Not immune"
      
  ## Have a variable that will become the label for the "line" that is each castaway. 
  ## We only want to label the start and end point for each Castaway in a season    
      tribemapreorg$labeltext <- ""
      for (sn in 1:max(tribemapreorg$season)) {
        for (c in unique(tribemapreorg$castaway_id[tribemapreorg$season == sn])) {
          # What is the first day that the castaway shows up in the data?
          minday <- min(tribemapreorg$day[tribemapreorg$season == sn & tribemapreorg$castaway_id == c],na.rm=T)
          temptext <- tribemapreorg$castaway[tribemapreorg$day == minday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c]
          tribemapreorg$labeltext[tribemapreorg$day == minday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c] <- temptext
          
          # what is the last day the castaway shows up in the data?
          maxday <- max(tribemapreorg$day[tribemapreorg$season == sn & tribemapreorg$castaway_id == c],na.rm=T)
          tribemapreorg$labeltext[tribemapreorg$day == maxday & tribemapreorg$season == sn & tribemapreorg$castaway_id == c] <- temptext
          
        } # Close castaway loop
      } # Close season loop
      
  # Get names of each tribe on the first day of their existence
  # We'll print that tribe name above the Castaways on that tribe. So need to get the max y-value of the castaways on that tribe
      tribenames <- tribemapreorg[,c("season","day","tribe","tribe_status","y")]
      
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
      tribenames <- unique(tribenames[tribenames$keep %in% 1,c("season","day","tribe","tribe_status","tribename","YtoUse")])
      
      # bring this back onto the original data
      tribemapreorg <- left_join(tribemapreorg,tribenames,by=c("season","day","tribe","tribe_status"))
          # We don't want the second swaps to show up as "Swapped_2", so make it a little easier to understand
            tribemapreorg$tribename <- gsub("Swapped_2","Second swap",tribemapreorg$tribename)
            
      # but only want it to show up once (if I leave all the rows filled in with the tribe name information, then it will print that over itself again and again)
      for (tr in unique(tribemapreorg$tribename[!(is.na(tribemapreorg$tribename))])) {
        miny <- min(tribemapreorg$y[tribemapreorg$tribename == tr],na.rm=T)
        tribemapreorg$YtoUse[tribemapreorg$tribename == tr & tribemapreorg$y != miny] <- NA
        
      }      
      
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

############################################################################################
############################################################################################
## Data set up: ############################################################################
## Vote Hx by day ##########################################################################
############################################################################################
      ## Prep data
      votehx <- vote_history
      # Call out the rock draws, etc -- they need to have their own "vote_id"
      vote_id_startingpoint <- max(vote_history$vote_id,na.rm=T)+200
      for (oddvote in unique(votehx$vote[is.na(votehx$vote_id)])) {
        votehx$vote_id[is.na(votehx$vote_id) & votehx$vote == oddvote] <- vote_id_startingpoint
        vote_id_startingpoint <- vote_id_startingpoint+1
      }
      
      # Create a variable that represents an OPPORTUNITY to vote (i.e., a specific vote order on a specific day in a specific season)
      # to ensure sort-order is appropriate, add a "0" in front of season 1-9 and days 1-9 and all vote orders (which are either a value of 1 or 2)
      for (varname in c("day","season","vote_order")) {
        votehx[,varname] <- as.character(votehx[,varname])
        
        votehx[str_length(votehx[,varname]) == 1,varname] <- paste("0",votehx[str_length(votehx[,varname]) == 1,varname],sep="")
        
      }
      votehx$opportunity <- paste(votehx$season,votehx$day,votehx$vote_order,sep="_")
      
      # Create a variable that represents the vote for a given opportunity to vote
      votehx$uniqueid <- paste(votehx$season,votehx$day,votehx$vote_order,votehx$vote_id,sep="_")
      voteblocs <- votehx
      votehx[votehx$season == "33" & votehx$day == "30",]   
      # create a y-value for each person who voted a specific way on a specific opportunity
      for (sn in 1:max(votehx$season)) {
        # what are the opportunities to vote?
        for (opp in unique(votehx$opportunity[votehx$season == sn])) {
          tempcount <- 1
          # within each specific vote, get the Y value by castaway
          for (v in unique(votehx$uniqueid[votehx$season == sn & votehx$opportunity == opp])) {
            for (ca in unique(votehx$castaway_id[votehx$season == sn & votehx$opportunity == opp & votehx$uniqueid == v])) {
              
              votehx$y[votehx$season == sn & votehx$opportunity == opp & votehx$uniqueid == v & votehx$castaway_id == ca] <- tempcount
              tempcount <- tempcount+1
            }
            # add an extra space between votes at the same opportunity
            tempcount <- tempcount+3                
          } # close specific vote loop
          
        } # close opportunity to vote loop
      } # close season loop
      
      # Get vote name
      # We'll print that tribe name above the Castaways who voted that way. 
      # So need to get the max y-value of the castaways who voted that way to be able to use that to plot above them
      votenames <- votehx %>% select(uniqueid,y,vote)
      
      for (v in unique(votenames$uniqueid)) {
        maxY <- max(votenames$y[votenames$uniqueid == v ],na.rm=T)
        votenames$votelabel[votenames$uniqueid == v & votenames$y == maxY] <- votenames$vote[votenames$uniqueid == v & votenames$y == maxY]
        
      } # close specific vote loop
      
      votenames <- votenames %>% filter(!(is.na(votelabel))) 
      # add to the y-value so that it doesn't get plotted over another castaway's name
      votenames$yUse <- votenames$y+1
      
      # bring this back onto the original data
      votehx <- left_join(votehx,votenames,by=c("uniqueid","y","vote"))
      
      # mark those who were immune
      votehx$castaway[votehx$immunity %in% "Individual"] <- paste(votehx$castaway[votehx$immunity %in% "Individual"]," (II)",sep="")
      votehx$castaway[votehx$immunity %in% "Hidden"] <- paste(votehx$castaway[votehx$immunity %in% "Hidden"]," (HI)",sep="")
      
################################################################################################################################################ 
#### Data set up ########################################################################################################################################### 
#### Voting blocs ########################################################################################################################################### 
################################################################################################################################################ 
################################################################################################################################################ 
# Ultimate goal: who voted the same? 
      # Create a dataset where each row is a castaway-castaway combination of a Specific Castaway with each of the Other Castaways that they COULD have voted with
      # Want it to contain: # Season. castaway. # of opps. row for each person they had oppty with. "Voted together/apart"
      # Creating the empty dataset that we'll fill in.
      votingblocs <- data.frame(matrix(ncol = 10, nrow = 0))
      names(votingblocs) <- c("season","castaway_id","castaway","orderOut","numberOfVotingOpps","potentialblocmember","potentialblocmember_id","potentialblocmember_orderOut","timescouldhavevotedtogether","timesvotedtogether")
      
      ## drop the rock draws etc.
      voteblocs <- voteblocs %>% filter(vote_id < max(vote_history$vote_id,na.rm=T)+200)
      
      for (sn in 1:max(voteblocs$season)) {
        for (c in unique(voteblocs$castaway_id[voteblocs$season == sn])) {
          # castaways name
          castawayName <- unique(voteblocs$castaway[voteblocs$castaway_id == c])
          
          # Number of opportunities to vote & list of opportunities
          castawayNumOfOpps <- length(unique(voteblocs$opportunity[voteblocs$season == sn & voteblocs$castaway_id == c]))
          castawayOppsList <- unique(voteblocs$opportunity[voteblocs$season == sn & voteblocs$castaway_id == c])
          
          # Who could have voted with them at any of their own opportunities?
          peoplewhohadoppwiththem <- unique(voteblocs$castaway[(voteblocs$opportunity %in% castawayOppsList) & voteblocs$castaway_id != c & voteblocs$season == sn])
          peoplewhohadoppwiththem_id <- unique(voteblocs$castaway_id[(voteblocs$opportunity %in% castawayOppsList) & voteblocs$castaway_id != c & voteblocs$season == sn])
          
          # Put those data together, and then count how many times the Other Castaway voted with the Selected Castaway
          # data together
          votingblocs <- rbind(votingblocs,cbind(season=rep(sn, length(peoplewhohadoppwiththem)),
                                                 castaway_id = rep(c, length(peoplewhohadoppwiththem)),
                                                 castaway = rep(castawayName, length(peoplewhohadoppwiththem)),
                                                 orderOut = rep(unique(max(voteblocs$order[voteblocs$castaway_id == c & voteblocs$season == sn])),length(peoplewhohadoppwiththem)),
                                                 numberOfVotingOpps = rep(castawayNumOfOpps, length(peoplewhohadoppwiththem)),
                                                 potentialblocmember = peoplewhohadoppwiththem,
                                                 potentialblocmember_id = peoplewhohadoppwiththem_id,
                                                 potentialblocmember_orderOut = NA,
                                                 timescouldhavevotedtogether = NA,
                                                 timesvotedtogether = NA))
          # for each of the Other Castaways, how many times did they vote the same?  
          # count how many of the Other Castaways' Votes overlapped with Selected Castaway's
          for (pbm in peoplewhohadoppwiththem_id) {
            # count how many of the Other Castaways' Opportunities overlapped with Selected Castaway's
            OthersOpps <- unique(voteblocs$opportunity[voteblocs$season == sn & voteblocs$castaway_id == pbm])
            votingblocs$timescouldhavevotedtogether[votingblocs$season == sn &
                                                      votingblocs$castaway_id == c &
                                                      votingblocs$potentialblocmember_id == pbm] <- length(intersect(OthersOpps,castawayOppsList))
            
            # count how many of the Other Castaways' Votes overlapped with Selected Castaway's
            votingblocs$timesvotedtogether[votingblocs$season == sn &
                                             votingblocs$castaway_id == c &
                                             votingblocs$potentialblocmember_id == pbm] <- length(intersect(unique(votehx$uniqueid[votehx$season == sn & votehx$castaway_id == pbm]),
                                                                                                            unique(votehx$uniqueid[votehx$season == sn & votehx$castaway_id == c])))
            # what was their order out? going to use this to drop the mirrored part of the data
            votingblocs$potentialblocmember_orderOut[votingblocs$season == sn &
                                                      votingblocs$castaway_id == c &
                                                      votingblocs$potentialblocmember_id == pbm] <- unique(max(voteblocs$order[voteblocs$castaway_id == pbm & voteblocs$season == sn]))
            
          } # close Other Castaway loop
          
        } # close Selected Castaway loop
        
      } # close season loop
      
      for (varname in c("numberOfVotingOpps","timescouldhavevotedtogether","timesvotedtogether"))  {
        votingblocs[,varname] <- as.numeric(votingblocs[,varname])
      }
      votingblocs$percenttogether <- votingblocs$timesvotedtogether/votingblocs$timescouldhavevotedtogether
      votingblocs <- votingblocs %>% mutate(strength=case_when(percenttogether < .34 ~ "Low (under 34%)",
                                                               percenttogether >= .34 & percenttogether <.67 ~ "Medium (34%-66.9%)",
                                                               percenttogether >=.67 ~ "High (67% and higher)"
      )
      ) %>%
        mutate(timescouldhave_cat=case_when(
          timescouldhavevotedtogether %in% c(1,2) ~ "01-02",
          timescouldhavevotedtogether %in% c(3,4) ~ "03-04",
          timescouldhavevotedtogether %in% c(5,6) ~ "05-06",
          timescouldhavevotedtogether %in% c(7,8,9,10) ~ "07-10",
          timescouldhavevotedtogether > 10 ~ "11+",
        )
        )
      votingblocs$strength <- factor(votingblocs$strength,levels=c("Low (under 34%)","Medium (34%-66.9%)","High (67% and higher)"))
      
      ## Because of the mirror image aspect of these data, keep only half of it
      ## (right now, it has info both Bobby Jon voting with Stephenie and Stephenie voting with Bobby Jon)
      #votingblocs <- votingblocs %>% filter(castaway_id > potentialblocmember_id)
      
      
################################################################################################################################################
################################################################################################################################################
## Graphing functions
################################################################################################################################################      
################################################################################################################################################

########################################################################      
## One Swap graph function
######################################################################## 
      
  graphOneSwapSeason <- function(selectedseason) {      
          
      ## Prep the data for graphing
          graphdata <- tribemapreorg[tribemapreorg$season == selectedseason,] 
          
      ## Get info about the season    
          dayofswap <- min(graphdata$day[graphdata$tribe_status == "Swapped"])
          dayofmerge <- min(graphdata$day[graphdata$tribe_status == "Merged"])
          tribesetup <- seasonsummary$tribe_setup[seasonsummary$season == selectedseason]
          filming <- paste("It was filmed from ",seasonsummary$filming_started[seasonsummary$season == selectedseason]," to ",seasonsummary$filming_ended[seasonsummary$season == selectedseason],".",sep="")
          aired <- paste("It aired from ",seasonsummary$premiered[seasonsummary$season == selectedseason]," to ",seasonsummary$ended[seasonsummary$season == selectedseason],".",sep="")          
          
      ## Graph
        swapone <-   
          graphdata %>%
            ## Basic plot: organize things by castaway's original tribe. Their shape indicates whether they won immune that day
                ggplot(aes(x=day,y=y,group=castaway_id,shape=immunity)) +
                    geom_line(aes(),color="black") +
                    geom_point(aes(x=day,y=y,fill=original_tribe,col=original_tribe,group=castaway_id,shape=immunity,size=immunity)) +
                    geom_text_repel(aes(label=labeltext,y=y),nudge_x=-1,show.legend = FALSE,color="black") +
            ## changing how things are displayed        
                scale_fill_tribes(selectedseason,tribe=graphdata$original_tribe) +
                scale_colour_tribes(selectedseason,tribe=graphdata$original_tribe) +
                scale_shape_manual(values=c(21,22))+
                scale_size_manual(values=c(5,2))+
                scale_x_continuous(breaks=seq(1,max(graphdata$day),1),
                                   labels=seq(1,max(graphdata$day),1),
                                   limits=c(0,max(graphdata$day)+1)) +      
                labs(
                  x="Day of game",
                  title = paste(unique(graphdata$season_name)," (Season ",selectedseason,")",sep=""),
                  subtitle = wrapper(paste(tribesetup,".  ",filming,"  ",aired,sep=""),width=90),
                  shape="Won immunity in challenge",
                  size="Won immunity in challenge",
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
                  legend.title=element_text(face="bold"),
                ) +
                guides(fill="none")+
                guides(color="none") +
                
            ## Swap day & Merge day
                geom_vline(xintercept=dayofswap-.1, linetype="dashed", color = reds[3]) +
                geom_vline(xintercept=dayofmerge-.1, linetype="dashed", color = reds[3]) +
                
            ## Add Tribe names, just at the start of the tribe's existence
                geom_text(aes(label=tribename,x=day,y=YtoUse+1,fontface=2),show.legend = FALSE,color="black")

        print(swapone)
        
    } # close One Swap graphing function

########################################################################      
## Two Swap graph function
########################################################################       
  graphTwoSwapSeason <- function(selectedseason)   {
    dayofSecondSwap <- min(tribemapreorg$day[tribemapreorg$tribe_status == "Swapped_2" & tribemapreorg$season == selectedseason])
    
    secondswapgraph <- 
        graphOneSwapSeason(selectedseason) +
          ## Swap day
          geom_vline(xintercept=dayofSecondSwap-.1, linetype="dashed", color = reds[3])  
    print(secondswapgraph)
      
  } # closing Two Swap graphing function

## Things I want to do    
## Size of dot = confessionals? Or put confessional number in the shape? But the confessionals are by episode and this season map is by day.
## Can I make the text labels on the Right be to the Right of the dot?
## Bring on info about challenges
## Add info about who makes the jury in cases where the first person voted off post-merge didn't make the jury? 
## Season 37 - the swaps are plotted much higher than the original tribes, and it looks bad. It has to do with a potential data issue on day 10, where it says its swapped tribes, but still have David and Goliath as tribe names.
      
      

    
  
###############################################
## who voted for whom on what day
###############################################          
seasonvotehxgraph <- function(selectedseason) {
    
    votegraph <- votehx %>% filter(season == selectedseason) 
    
    xaxislabels <- votegraph %>% 
                      select(season,day,vote_order) %>% 
                      distinct() %>% 
                      mutate(xlabel=paste("Day ",day,"\nVote #",vote_order,sep=""),.keep="none")
    
        votegraph %>%
            ggplot(aes(x=opportunity,y=y,group=castaway)) +
              geom_text(label=votegraph$castaway,size=3) +
              geom_text(aes(x=opportunity,y=yUse,label=paste("Voted for\n",votelabel,sep=""),fontface=2),size=3) +
              theme(
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x= element_text(color="black"),
                title = element_text(face="bold")
              ) +
              labs(
                title = "Vote history",
                subtitle = "(HI) = hidden immunity    (II) = individual immunity"
              ) +
              scale_x_discrete(position="top",
                               labels=c(xaxislabels$xlabel),
                               ) +
              guides(size="none")
  } # close function

###############################################    
## Visualize Voting Alliances
###############################################      
votingalliancestrength <- function(selectedseason) {
    
  graphdata <- votingblocs %>% filter(season == selectedseason) 
          
      ## color indicates strength
          
          graphdata %>%
            ggplot(aes(
              x = forcats::fct_reorder(factor(castaway), as.numeric(orderOut),.desc=TRUE),
              y=forcats::fct_reorder(factor(potentialblocmember), as.numeric(potentialblocmember_orderOut),.desc=FALSE),
              size=timescouldhavevotedtogether,
              color=strength)) +
            geom_point(shape=19) +
            scale_color_manual(values=greens[c(2,3,5)])+
            scale_x_discrete(position="top") + 
            labs(
              title="Strength of voting blocs",
              color = "Strength of voting alliance\n(Percent voted together)",
              size = "Opportunities to vote together"
            ) +
            theme(
              axis.title=element_blank(),
              axis.text = element_text(color="black"),
              axis.text.x = element_text(angle=90)
            )    
  } # close function 
      
##############################################################################################################################
## print the graphs!
## Because the day-by-day graph content varies by type of season, need if-else statements for that
      
selectedseason <- 31
      
if (selectedseason %in% oneswap)   {   
      
    ggarrange(graphOneSwapSeason(selectedseason),
              ggarrange(seasonvotehxgraph(selectedseason),votingalliancestrength(selectedseason),
              nrow=1,widths=c(2,1)),
              nrow=2)
} else if (selectedseason %in% twoswaps) { # close one swap season and open two swap season
  
    ggarrange(graphTwoSwapSeason(selectedseason),
              ggarrange(seasonvotehxgraph(selectedseason),votingalliancestrength(selectedseason),
                        nrow=1,widths=c(2,1)),
              nrow=2)      
} else { # close two swap seasons. For now, don't print day by day map for other seasons since the graphs aren't ready
  ggarrange(seasonvotehxgraph(selectedseason),votingalliancestrength(selectedseason),
            nrow=1,widths=c(2,1))
}
ggsave(paste(savedir,"/Seasonsummary_",selectedseason,".png",sep=""),width=18, height=10)


