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
      
      votetypeforuniqueid <- paste(sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[1],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[2],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[3],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[4],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[5],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[6],
                                   sort(numbervotesreceivedbyperson$votesreceivedbyperson[numbervotesreceivedbyperson$uniquevoteid == u],decreasing=T)[7],sep="-")
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

votehxdetailed <- full_join(votehx,votes)    
write.csv(votehxdetailed,paste(savedir,"Vote_history_types_of_vote_splits.csv",sep=""))

