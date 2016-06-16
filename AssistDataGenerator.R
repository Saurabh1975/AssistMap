#' Script to generate .csv files of Team 
#' Assist data, so the Shiny App won't
#' need to access the NBA API everytime it runs

library(rjson)
library(stringr)
library(igraph)
library(reshape)

#Load method for reading Pass/Assist data from NBA API
source('GetPassDashboard.r')

#Read Team IDs from csv. Team IDs csv generated via GetTeamIDs. Colors were manually
#entered into .csv to match team colors later
teamids <- read.csv("teamids.csv")
teamids$name <- str_trim(teamids$name, side = 'right')

#Create .csv Files for each team
for (i in 1:nrow(teamids)) {
  team.name<-teamids$name[i]
  team.id <- teamids$id[i]
  team.color <- teamids$teamcolor[i]
  #Get pass data for given team in order select the relevant players
  team.roster<-GetPassDashboard('Pass Made', 'team', team.id)
  team.roster<-team.roster[team.roster$G>20,]
  team.roster<-team.roster[,c('PASS_FROM','PASS_TEAMMATE_PLAYER_ID')]
  
  #Create data frame of individual player assist data
  pass.frame<-GetPassDashboard('Pass Made', 'player',team.roster$PASS_TEAMMATE_PLAYER_ID[1])
  for(i in 2:nrow(team.roster)){
    player.pass.frame<-GetPassDashboard('Pass Made', 'player',team.roster$PASS_TEAMMATE_PLAYER_ID[i])
    pass.frame<-rbind(pass.frame,player.pass.frame)
  }
  
  #Create assist frame
  pass.frame<-pass.frame[pass.frame$TEAM_ID==team.id,]
  pass.frame<-pass.frame[pass.frame$PASS_TEAMMATE_PLAYER_ID %in% team.roster$PASS_TEAMMATE_PLAYER_ID,]
  
  pass.frame<-pass.frame[,c('PLAYER_ID',"PLAYER_NAME_LAST_FIRST","PASS_TO","PASS_TEAMMATE_PLAYER_ID",'AST')]
  
  pass.frame.ids<-pass.frame[,c('PLAYER_ID',"PASS_TEAMMATE_PLAYER_ID",'AST')]
  pass.frame.names<-pass.frame[,c("PLAYER_NAME_LAST_FIRST","PASS_TO",'AST')]
  
  #Create information for net & heatmap
  pass.net<-graph.data.frame(pass.frame.names, directed=T) #pass.net
  get.edgelist(pass.net, names=TRUE)
  pass.adj<- get.adjacency(pass.net, attr="AST", sparse=F) #pass.adj
  
  dat<-pass.adj
  row.order <- hclust(dist(dat))$order
  col.order <- hclust(dist(t(dat)))$order
  
  pass.hm <- pass.frame[,c('PLAYER_NAME_LAST_FIRST','PASS_TO','AST')]
  pass.hm <- melt(dat)
  colnames(pass.hm)<-c('PassTo','PassFrom','Assists')
  
  pass.hm$PassFrom <- factor(pass.hm$PassFrom, levels = pass.hm$PassFrom)
  pass.hm$PassTo <- factor(pass.hm$PassTo, levels = pass.hm$PassTo)
  pass.hm$PassTo <- factor(pass.hm$PassTo , levels=rev(levels(pass.hm$PassTo )))
  
  #Write .csv of Team Assist Matrix
  write.csv(pass.hm,paste0(team.name,'.csv'),row.names=F)
  
}