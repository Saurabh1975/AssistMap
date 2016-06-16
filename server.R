
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

library(XML)
library(rjson)
library(igraph)
library(ggplot2)
library(stringr)
library(arcdiagram)
library(reshape)
library(IDPmisc)
library(grDevices)


options(stringsAsFactors=FALSE)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))+1)
}

shinyServer(function(input, output) {
  
  colData<-reactive({
    #Grab Team IDs, Select Team
    teamids<-read.csv("teamids.csv")
    teamids$name<-str_trim(teamids$name,side='right')
    team.name<-input$team.selection
    team.color <- teamids[teamids$name==team.name,'teamcolor']
    return(team.color)
    
  })
  
  hmData<-reactive({
    
    #Grab Team IDs, Select Team
    teamids<-read.csv("teamids.csv")
    teamids$name<-str_trim(teamids$name,side='right')
    team.name<-input$team.selection
    
    pass.hm<-read.csv(paste0(team.name,'.csv'))

    return(pass.hm)
    
  })
  

  
  output$heatmap <- renderPlot({
    
    
    pass.hm<-hmData()
    team.color<-colData()
    
    
    hm<-ggplot(pass.hm , aes(PassFrom,PassTo)) + geom_tile(aes(fill = Assists), colour = "white") +
      scale_fill_gradient(low = "white", high =team.color) + #,limits=c(0,223)
      labs(title='Assist Matrix',x='Assist To',y='Assist From')+ 
      theme(plot.title = element_text(color="#666666", face="bold", size=22))+
      theme(axis.text.x=element_text(angle=330,vjust=0.9,hjust=0)) +
      theme(axis.text.y=element_text(hjust=0.25))+
      theme(plot.title = element_text(color="black",size=22))+
      theme(axis.title = element_text(color="black",  size=14))  
    print(hm)
    
  })
  
})
