
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plyr)

teamids<-read.csv("teamids.csv")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualizing Team Assists"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("team.selection", "Choose a team:", 
                  choices = sort(as.character(teamids$name)),selected='Toronto')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("heatmap")
      #plotOutput("netmap")
    )
  )
))
