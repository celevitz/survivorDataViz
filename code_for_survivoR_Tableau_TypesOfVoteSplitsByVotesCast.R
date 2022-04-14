## Carly Levitz
## 4/7/2022
## Purpose: what are the different ways votes are split when different numbers of votes are cast?
## Note - first need to run code_for_survivoR_Tableau.R to get the needed data file


rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library");library(tidygraph,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library"); library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library"); library(ggrepel,lib="C:/Program Files/R/R-4.1.1/library");library(ggraph,lib="C:/Program Files/R/R-4.1.1/library");  
savedir <- "H:/R/survivoR/02_cleaned_data/"

# Bring in the data

votehx <- read.csv(paste(savedir,"survivoR_04_votehx_cleaned.csv",sep=""),header=T)

## number of votes cast in a given TC vote
votescast <- votehx %>% select(version,version_season,day,order,vote_order,castaway_id,vote_id) %>%
          filter(!(is.na(vote_id))) %>%
          group_by(version,version_season,day,order,vote_order) %>%
          mutate(votescast=n()) %>%
          select(version,version_season,day,order,vote_order,votescast) %>%
          distinct()


## number of people receiving votes
numberreceiving <- votehx %>% 
  select(version,version_season,day,order,vote_order,vote_id) %>%
  filter(!(is.na(vote_id))) %>%
  group_by(version,version_season,day,order,vote_order) %>%
  mutate(numbergettingvotes=n_distinct(vote_id)) %>%
  select(version,version_season,day,order,vote_order,numbergettingvotes) %>%
  distinct()


## type of vote split
numbervotesreceivedbyperson <- votehx %>% 
            select(version,version_season,day,order,vote_order,vote_id) %>%
            filter(!(is.na(vote_id))) %>%
            group_by(version,version_season,day,order,vote_order,vote_id) %>%
            mutate(votesreceivedbyperson=n(),temp="x",uniquevoteid=paste(version_season,day,order,vote_order,sep="_"),votetype=NA,votesplitamong=NA) %>%
            distinct() %>%
            group_by(version,version_season,day,order,vote_order) %>%
            select(!vote_id)

    # for a given vote event, what was the split?
    for (u in unique(numbervotesreceivedbyperson$uniquevoteid)) {
      
      votetypeforuniqueid <- paste(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][1],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][2],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][3],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][4],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][5],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][6],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][7],sep="-")
      #votessplitamongst <- paste(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][1],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][2],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][3],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][4],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][5],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][6],numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u][7],sep="-")
      numbervotesreceivedbyperson$votetype[numbervotesreceivedbyperson$uniquevoteid == u] <- votetypeforuniqueid
    }
    numbervotesreceivedbyperson$votetype <- gsub("-NA","",numbervotesreceivedbyperson$votetype)

numbervotesreceivedbyperson <- numbervotesreceivedbyperson %>%
      select(version,version_season,day,order,vote_order,votetype) %>%
      distinct()

##############################
## Bring all together
votes <-full_join( full_join(votescast,numberreceiving),
             numbervotesreceivedbyperson)


## do a little clean-up
  # 3 votes
    votes$votetype[votes$votetype %in% c("1-2")] <- "2-1"
  # 4 votes
    votes$votetype[votes$votetype %in% c("1-3")] <- "3-1"
  # 5 votes
    votes$votetype[votes$votetype %in% c("1-3-1","1-1-3")] <- "3-1-1"
    votes$votetype[votes$votetype %in% c("2-3")] <- "3-2"
    votes$votetype[votes$votetype %in% c("1-4")] <- "4-1"
    votes$votetype[votes$votetype %in% c("1-2-2","2-1-2")] <- "2-2-1" 
  # 6 votes
    votes$votetype[votes$votetype %in% c("1-4-1","1-1-4")] <- "4-1-1"  
    votes$votetype[votes$votetype %in% c("1-5")] <- "5-1"
    votes$votetype[votes$votetype %in% c("1-3-2","1-2-3","2-3-1","2-1-3","3-1-2")] <- "3-2-1"
    votes$votetype[votes$votetype %in% c("2-4")] <- "4-2"
  # 7 votes
    votes$votetype[votes$votetype %in% c("1-4-2","1-2-4","2-1-4","2-4-1")] <- "4-2-1"  
    votes$votetype[votes$votetype %in% c("1-6")] <- "6-1"
    votes$votetype[votes$votetype %in% c("2-5")] <- "5-2"
    votes$votetype[votes$votetype %in% c("3-4")] <- "4-3"
    votes$votetype[votes$votetype %in% c("2-4-1","1-4-2","1-2-4","4-1-2")] <- "4-2-1"
    votes$votetype[votes$votetype %in% c("1-5-1","1-1-5")] <- "5-1-1"
    votes$votetype[votes$votetype %in% c("2-1-3-1")] <- "3-2-1-1"
    votes$votetype[votes$votetype %in% c("2-3-2")] <- "3-2-2"
    votes$votetype[votes$votetype %in% c("1-3-3","3-1-3")] <- "3-3-1"
  # 8 votes
    votes$votetype[votes$votetype %in% c("1-2-4-1")] <- "4-2-1-1"  
    votes$votetype[votes$votetype %in% c("1-2-5","1-5-2","2-1-5","2-5-1")] <- "5-2-1"  
    votes$votetype[votes$votetype %in% c("1-3-4","1-4-3","3-4-1","3-1-4","4-1-3")] <- "4-3-1"  
    votes$votetype[votes$votetype %in% c("1-5-1-1","1-1-5-1","1-1-1-5")] <- "5-1-1-1"  
    votes$votetype[votes$votetype %in% c("1-7")] <- "7-1"  
    votes$votetype[votes$votetype %in% c("2-3-3","3-2-3")] <- "3-3-2"  
    votes$votetype[votes$votetype %in% c("2-4-2","2-2-4")] <- "4-2-2"  
    votes$votetype[votes$votetype %in% c("2-6")] <- "6-2"  
    votes$votetype[votes$votetype %in% c("3-5")] <- "5-3"  
    votes$votetype[votes$votetype %in% c("4-1-2-1","4-1-1-2","2-1-1-4","2-1-4-1","2-4-1-1")] <- "4-2-1-1"  
    votes$votetype[votes$votetype %in% c("5-1-2","1-2-5","1-5-2","2-1-5","2-5-1")] <- "5-2-1"  
    votes$votetype[votes$votetype %in% c("1-1-6","1-6-1")] <- "6-1-1"  
  # 9 votes
    votes$votetype[votes$votetype %in% c("1-3-5","1-5-3","3-5-1","3-1-5","5-1-3")] <- "5-3-1" 
    votes$votetype[votes$votetype %in% c("1-5-1-2","1-1-5-2","1-1-2-5","2-1-1-5","2-1-5-1","2-5-1-1","5-1-2-1","5-1-1-2")] <- "5-2-1-1"  
    votes$votetype[votes$votetype %in% c("1-1-7","1-7-1")] <- "7-1-1"  
    votes$votetype[votes$votetype %in% c("2-4-3","2-3-4","3-2-4","3-4-2","4-2-3")] <- "4-3-2" 
    votes$votetype[votes$votetype %in% c("2-6-1","2-1-6","6-1-2","1-2-6","1-6-2")] <- "6-2-1" 
    votes$votetype[votes$votetype %in% c("2-7")] <- "7-2" 
    votes$votetype[votes$votetype %in% c("3-6")] <- "6-3" 
    votes$votetype[votes$votetype %in% c("1-4-1-3","1-1-4-3","1-1-3-4","3-1-1-4","3-1-4-1","3-4-1-1","4-1-3-1","4-1-1-3")] <- "4-3-1-1" 
    votes$votetype[votes$votetype %in% c("4-5")] <- "5-4"
    votes$votetype[votes$votetype %in% c("2-5-2","2-2-5")] <- "5-2-2" 
  #10 votes
    votes$votetype[votes$votetype %in% c("1-1-1-1-1-4-1")] <- "4-1-1-1-1-1-1" 
    votes$votetype[votes$votetype %in% c("5-1-3-1")] <- "5-3-1-1" 
    votes$votetype[votes$votetype %in% c("7-1-2")] <- "7-2-1" 
    votes$votetype[votes$votetype %in% c("5-2-3")] <- "5-3-2"
    votes$votetype[votes$votetype %in% c("4-5-1")] <- "5-4-1" 
    votes$votetype[votes$votetype %in% c("4-2-4")] <- "4-4-2"
    votes$votetype[votes$votetype %in% c("3-5-2","2-3-5")] <- "5-3-2"
    votes$votetype[votes$votetype %in% c("1-6-3")] <- "6-3-1"
    votes$votetype[votes$votetype %in% c("4-6")] <- "6-4"
    votes$votetype[votes$votetype %in% c("3-7")] <- "7-3"
    votes$votetype[votes$votetype %in% c("2-8")] <- "8-2"
    votes$votetype[votes$votetype %in% c("1-9")] <- "9-1" 
  # 11 votes
    votes$votetype[votes$votetype %in% c("4-7")] <- "7-4" 
    votes$votetype[votes$votetype %in% c("5-6")] <- "6-5" 
    votes$votetype[votes$votetype %in% c("1-8-2")] <- "8-2-1" 
    votes$votetype[votes$votetype %in% c("1-9-1")] <- "9-1-1" 
    votes$votetype[votes$votetype %in% c("2-7-2")] <- "7-2-2" 
    votes$votetype[votes$votetype %in% c("6-2-3")] <- "6-3-2"
    votes$votetype[votes$votetype %in% c("2-6-2-1","2-2-1-6","2-2-6-1")] <- "6-2-2-1" 
  # 12 votes
    votes$votetype[votes$votetype %in% c("2-10")] <- "10-2" 
    votes$votetype[votes$votetype %in% c("3-9")] <- "9-3"
    votes$votetype[votes$votetype %in% c("3-4-5","3-5-4","4-3-5","4-5-3","5-3-4")] <- "5-4-3" 
    votes$votetype[votes$votetype %in% c("1-1-4-6","1-4-1-6","1-4-6-1","4-1-1-6","4-1-6-1","4-6-1-1","6-1-4-1","6-1-1-4")] <- "6-4-1-1" 
    votes$votetype[votes$votetype %in% c("5-6-1","5-1-6","1-5-6","1-6-5","6-1-5")] <- "6-5-1"
    votes$votetype[votes$votetype %in% c("2-3-7","2-7-3","3-2-7","3-7-2","7-2-3")] <- "7-3-2" 
    votes$votetype[votes$votetype %in% c("1-4-7","1-7-4","4-1-7","4-7-1","7-1-4")] <- "7-4-1" 
    votes$votetype[votes$votetype %in% c("2-8-2","2-2-8")] <- "8-2-2" 
  # 13 votes
    votes$votetype[votes$votetype %in% c("1-12")] <- "12-1" 
    votes$votetype[votes$votetype %in% c("5-8")] <- "8-5" 
    votes$votetype[votes$votetype %in% c("4-9")] <- "9-4" 
    votes$votetype[votes$votetype %in% c("2-4-7","2-7-4","4-2-7","4-7-2","7-2-4")] <- "7-4-2"
    votes$votetype[votes$votetype %in% c("1-2-4-6","1-2-6-4","1-4-2-6","1-4-6-2","1-6-4-2","1-6-2-4","2-1-4-6","2-4-1-6","2-4-6-1","2-1-6-4","2-6-1-4","2-6-4-1","4-6-2-1","4-6-1-2","4-1-2-6","4-1-6-2","4-2-6-1","4-2-1-6","6-4-1-2","6-2-1-4","6-2-4-1","6-1-2-4","6-1-4-2")] <- "6-4-2-1"
    votes$votetype[votes$votetype %in% c("2-2-3-6","2-2-6-3","3-2-2-6","3-2-6-2","3-6-2-2","6-2-3-2","6-2-2-3")] <- "6-3-2-2"
  #16 votes
    votes$votetype[votes$votetype %in% c("5-3-5-3")] <- "5-5-3-3" 
    
    
    
    
    

votehxdetailed <- full_join(votehx,votes)    
write.csv(votehxdetailed,paste(savedir,"Vote_history_types_of_vote_splits.csv",sep=""))

