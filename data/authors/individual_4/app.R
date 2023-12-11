#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(conflicted)

setwd("/Users/zachstrennen/hw4app/")
posting_layoff <- read.csv("data/posting_layoff.csv",check.names=FALSE)
#posting_layoff <- read.csv("/Users/zachstrennen/hw4app/data/posting_layoff.csv")
#posting_layoff
names(posting_layoff) <- c("Sector", "LinkedIn Job Postings Present at the End of August", "Total Number of Layoffs in August (in thousands)", "Total Number of Quits in August (in thousands)", "Total Number of Separations in August (in thousands)", "Total Number of Hires in August (in thousands)","Total Number of Job Openings in August (in thousands)","Turnover Rate for August")


vars <- setdiff(names(posting_layoff), "Sector")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plotting and Clustering Exploratory Dataset Variables"),

    # Sidebar with a slider input for number of bins 
    pageWithSidebar(
      h1('Variables to be clustered and cluster count:', style = "font-size:20px;"),
      sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        numericInput('clusters', 'Cluster Count', 3, min = 1, max = 9)
      ),
      mainPanel(
        plotOutput('plot1')
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    posting_layoff[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = alpha(clusters()$cluster,0.65),
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    text(selectedData()[,1],selectedData()[,2], labels=posting_layoff$Sector,cex=.7,adj = c(1, 1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
