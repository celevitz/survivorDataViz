## Carly Levitz
## 2022-02-23
## SurvivoR Voting Alliances
## Want to show two ways: one as a sort of table/heat map, and the other as network analysis type thing.

rm(list=ls())
library(tidyverse,lib="C:/Program Files/R/R-4.1.1/library");library(tidygraph,lib="C:/Program Files/R/R-4.1.1/library"); library(survivoR,lib="C:/Program Files/R/R-4.1.1/library"); library(ggpubr,lib="C:/Program Files/R/R-4.1.1/library"); library(ggrepel,lib="C:/Program Files/R/R-4.1.1/library");library(ggraph,lib="C:/Program Files/R/R-4.1.1/library");  

devtools::install_github("doehm/survivoR")
savedir <- "H:/R/survivoR/02_cleaned_data/"

votehx <- survivoR::vote_history
  ## what were the opportunities to vote?
    votehx$opportunity <- paste(as.character(votehx$season),as.character(votehx$day),as.character(votehx$vote_order),sep="_")
  
  # Create a variable that represents the vote for a given opportunity to vote
    votehx$uniqueid <- paste(as.character(votehx$season),as.character(votehx$day),as.character(votehx$vote_order),as.character(votehx$vote_id),sep="_")


###################
## Color schemes
greens <- c("#9FB8A2","#3AA845","#305844","#2A3B32","#101C16")
reds <- c("#F1E48A","#F58406","#932306","#4B0A06","#060304")
theme_set(theme_classic())

# for really long text, want to "wrap" it
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

################################################################################################################################################ 
#### Data set up ########################################################################################################################################### 
#### Voting blocs - goal is to have like a heat map ########################################################################################################################################### 
################################################################################################################################################ 
################################################################################################################################################ 

# Ultimate goal: who voted the same? 
# Create a dataset where each row is a castaway-castaway combination of a Specific Castaway with each of the Other Castaways that they COULD have voted with
# Want it to contain: # Season. castaway. # of opps. row for each person they had oppty with. "Voted together/apart"
# Creating the empty dataset that we'll fill in.
  votingblocs <- data.frame(matrix(ncol = 10, nrow = 0))
  names(votingblocs) <- c("season","castaway_id","castaway","orderOut","numberOfVotingOpps","potentialblocmember","potentialblocmember_id","potentialblocmember_orderOut","timescouldhavevotedtogether","timesvotedtogether")

  ## drop the rock draws etc.
  voteblocs <- votehx %>% filter(!(is.na(vote_id)))
  
  ## get the rest of the data
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
      
    # Clean the data
      for (varname in c("numberOfVotingOpps","timescouldhavevotedtogether","timesvotedtogether"))  {
        votingblocs[,varname] <- as.numeric(votingblocs[,varname])
      }
    # get the % of times people voted together, and create categories of strength
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
## (right now, it has info on e.g. both Bobby Jon voting with Stephenie and Stephenie voting with Bobby Jon)
#votingblocs <- votingblocs %>% filter(castaway_id > potentialblocmember_id)
      
      
      
########################################################################################################### 
###########################################################################################################
## Clean the data - 
## Voting Blocs as Network analysis of votes
###########################################################################################################
###########################################################################################################      
      castaways <- survivoR::castaways
      
      # keep one row per season-castaway (get rid of redemption seasons)
      for (sn in 1:max(castaways$season)) {
        for (c in unique(castaways$castaway_id[castaways$season == sn])) {
          finalorder <- max(castaways$order[castaways$castaway_id == c & castaways$season == sn],na.rm=T)
          castaways$finalorder[castaways$castaway_id == c & castaways$season == sn & castaways$order == finalorder] <- "keep"
        }
      }
      castaways <- castaways %>% filter(!(is.na(castaways$finalorder)))
      
      
      # keep just the relevant variables
      castaways <- castaways %>% mutate(Node=paste(castaways$season,castaways$castaway_id,sep="_")) %>% select(castaway,Node,original_tribe,order) %>% distinct()
      names(castaways)[names(castaways) %in% "castaway"] <- "Label"
      
      alliances <- votehx
      
      # Want the castaways to be unique by season (i.e., be able to distinguish Tyson in each of his seasons)
      alliances$castaway_id_s <- paste(alliances$season,alliances$castaway_id,sep="_")
      
      # Drop non-person votes
      alliances <- alliances[!(alliances$vote %in% c("-","Black Rock","countback","Eliminated","Exiled","Immune","Kidnap","Lose","None","Purple Rock","Saved","Shot in the dark","White Rock","Yellow Rock")),]
      
      # for each of the castaways, create a mini-dataset (then append) who voted with them 
      # do if for just a season at a time (i.e., don't use castaway_id, use castaway_id_s)
      temp <- data.frame(matrix(ncol = 7, nrow = 0))
      names(temp) <- c("season_name","season","castaway2","castaway2_id","count","castaway1","castaway1_id")
      
      for (c in c(unique(alliances$castaway_id_s))) {
        # get Castaway's name
        cc <- unique(alliances$castaway[alliances$castaway_id_s == c])
        
        # keep just the votes that the castaway cast  
        votescastbySelected <- unique(alliances$uniqueid[alliances$castaway_id_s == c])
        temp2 <- alliances[alliances$uniqueid %in% c(votescastbySelected),c("season_name","season","castaway","castaway_id_s","uniqueid")]
        
        # drop the castaway themselves
        temp2 <- temp2[temp2$castaway_id_s != c,]
        
        # if someone had no one ever vote the same as them, we need to create a dataset with at least one row
        if (nrow(temp2) == 0) { 
          temp2 <- data.frame(matrix(ncol = 7, nrow = 1))
          names(temp2) <- c("season_name","season","castaway2","castaway2_id","count","castaway1","castaway1_id")
          temp2$season_name <- unique(alliances$season_name[alliances$castaway_id_s == c])
          temp2$season <- unique(alliances$season[alliances$castaway_id_s == c])
          
        } else {
          
          # count how many times each castaway voted with the selected castaway
          temp2$count <- 1
          temp2 <- aggregate(temp2$count,by=list(temp2$season_name,temp2$season,temp2$castaway,temp2$castaway_id_s),FUN=sum,na.rm=T)
          names(temp2) <- c("season_name","season","castaway2","castaway2_id","count")
        }
        
        # make this temp dataset same as the original one  
        temp2$castaway1 <- unique(cc)
        temp2$castaway1_id <- unique(c)
        
        temp <- rbind(temp,temp2)
        
      }
      
      # because this is not directional, there are duplicates - e.g., season 1, Dirk voted with Richard once, and Richard voted with Dirk once.
      # I need to get rid of duplicates
      # I can do this by dividing the count by 2
      #temp <- temp[order(temp$season,temp$castaway1_id,temp$castaway2_id),]
      #temp$count <- temp$count/2
      
      # Drop those that only voted together once for simplicity?
      #temp <- temp %>% filter(count != .5)
      
      # get the nodes and edges for the blocs ready
      # Nodes
      # get the unique Castaway IDs
      blocnodes1 <- temp[,c("castaway1","castaway1_id")]
      names(blocnodes1) <- c("Label","Node")
      blocnodes2 <- temp[,c("castaway2","castaway2_id")]
      names(blocnodes2) <- c("Label","Node")
      blocnodes <- unique(rbind(blocnodes1,blocnodes2))
      # Create id's
      row.names(blocnodes) <- NULL
      blocnodes <- blocnodes %>% rowid_to_column("id")
      
      # Edges across all seasons
      blocedges1 <- temp[,c("castaway2_id","castaway1_id","count")]
      names(blocedges1) <- c("source","destination","weight")
      blocedges <- blocedges1 %>% 
        left_join(blocnodes, by = c("source" = "Node")) %>% 
        rename(from = id)
      
      blocedges <- blocedges %>% 
        left_join(blocnodes, by = c("destination" = "Node")) %>% 
        rename(to = id)
      names(blocedges) <- c("source","destination","weight","from","LabelFrom","to","LabelTo")
      
      
##########################################################################################################
###########################################################################################################
## Graphing
##############################################################################################################      
      ###############################################    
      ## Visualize Voting Alliances as discrete shapes (heat map)
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
      
      ###############################################    
      ## Visualize Voting Alliances as a network
      ###############################################           
      votingalliancenetwork <- function(selectedseason) {
        
        # get the sort order for a how we'll plot the network
        # how many edges does a castaway have?
        sortorder <- temp %>% group_by(castaway2) %>% filter(season == selectedseason) %>% mutate(sortorder=sum(count)) %>% select(c(castaway2,sortorder)) %>% distinct()
        names(sortorder) <- c("Label","sortorder")
        
        # Nodes
        # get the unique Castaway IDs
        blocnodes1 <- temp[temp$season == selectedseason,c("castaway1","castaway1_id")]
        names(blocnodes1) <- c("Label","Node")
        blocnodes2 <- temp[temp$season == selectedseason,c("castaway2","castaway2_id")]
        names(blocnodes2) <- c("Label","Node")
        blocnodes <- unique(rbind(blocnodes1,blocnodes2))
        blocnodes <- blocnodes[!(is.na(blocnodes$Node)),]
        # Create id's 
        blocnodes <- left_join(blocnodes,sortorder,by="Label")
        blocnodes <- blocnodes[order(blocnodes$sortorder),]
        row.names(blocnodes) <- NULL
        blocnodes <- blocnodes %>% rowid_to_column("id") %>% select(!sortorder)
        
        # Add original tribe back on for the purpose of colors
        blocnodes <- left_join(blocnodes,castaways,by=c("Label","Node"))   
        
        # Edges in one season
        blocedgesSeason1 <- temp[temp$season == selectedseason,c("castaway2_id","castaway1_id","count")]
        names(blocedgesSeason1) <- c("source","destination","weight")
        blocedgesSeason <- blocedgesSeason1 %>% 
          left_join(blocnodes %>% select(!(c(original_tribe,order))), by = c("source" = "Node")) %>% 
          rename(from = id)
        
        blocedgesSeason <- blocedgesSeason %>% 
          left_join(blocnodes %>% select(!(c(original_tribe,order))), by = c("destination" = "Node")) %>% 
          rename(to = id)
        
        names(blocedgesSeason) <- c("source","destination","weight","from","LabelFrom","to","LabelTo")
        blocedgesSeason <- blocedgesSeason[!(is.na(blocedgesSeason$from)),]    
        
        votingblocs <- tbl_graph(nodes = blocnodes, edges = blocedgesSeason, directed = FALSE)
        # Graph it      
        ggraph(votingblocs, layout = "fr",) + 
          #ggraph(votingblocs, layout = "linear",circular = T) +
          geom_edge_arc(aes(width = weight,), 
                        edge_alpha = 0.3,
                        edge_colour= "black",
                        lineend = "round",
                        strength = .1) + 
          scale_edge_width(range = c(.5, 3)) +
          scale_fill_tribes(selectedseason,tribe=blocnodes$original_tribe) +
          scale_colour_tribes(selectedseason,tribe=blocnodes$original_tribe) +
          geom_node_label(aes(label = blocnodes$Label,
                              fill=blocnodes$original_tribe,
                              #size=blocnodes$order
                              size=3
          ),
          color="white",
          repel=TRUE,
          point.padding = unit(0.2, "lines")
          ) +
          geom_node_point(aes(fill = blocnodes$original_tribe,
                              colour=blocnodes$original_tribe,
                              size=blocnodes$order),
                          shape=21) +
          labs(
            title=paste("Voting alliances in Season ",selectedseason,sep=""),
            fill="Original tribe",
            width="Number of times voted together"
          ) +
          theme_graph(
            base_family = "Arial Narrow",
            base_size = 11,
            background = "white",
            foreground = NULL,
            border = TRUE,
            #text_colour = "black",
            #bg_text_colour = text_colour,
            #fg_text_colour = text_colour,
            #title_family = base_family,
            title_size = 18,
            title_face = "bold",
            title_margin = 10,
            #title_colour = bg_text_colour,
            #subtitle_family = base_family,
            subtitle_size = 12,
            subtitle_face = "plain",
            subtitle_margin = 15,
            #subtitle_colour = bg_text_colour,
            #strip_text_family = base_family,
            strip_text_size = 10,
            strip_text_face = "bold",
            #strip_text_colour = fg_text_colour,
            #caption_family = base_family,
            caption_size = 9,
            caption_face = "italic",
            caption_margin = 10,
            #caption_colour = bg_text_colour,
            plot_margin = margin(30, 30, 30, 30)
          ) 
        
      } # close voting alliance network function  
      
      
      
      selectedseason <- 38
      votingalliancestrength(selectedseason)
      votingalliancenetwork(selectedseason)
