library(shiny)
library(tidyverse)

source <- dget("source.txt")
original <- read_csv(source)

preprocess <- dget("util/1_preprocess.R")
processed <- preprocess(original)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Workhours Dashboard"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    tabsetPanel(
      id = "display_choice",
      tabPanel(
        "Table",
        verticalLayout(
          radioButtons(
            "table_type",
            "Choose table:",
            choiceNames = c("Recent chunks",
                            "Chunks by project"),
            choiceValues = c("recent", "project")
          )
        ),
        conditionalPanel(
          condition = "input.table_type == 'project'",
          selectInput(
            "project_choice",
            label = "Select project:",
            choices = levels(processed$project)
          ),
          downloadButton("downloadData", "Download")
        )
      ),
      tabPanel(
        "Graphs",
        verticalLayout(
          radioButtons(
            "graph_selection",
            "Select graph",
            choiceNames = c("By time period",
                            "Project development"),
            choiceValues = c("time_graph",
                             "development_graph")
          ),
          h2("Plotting options"),
          conditionalPanel(
            condition = "input.graph_selection == 'time_graph'",
            dateRangeInput(
              "date_selection",
              "Date range:",
              start = Sys.Date() - 14,
              end = Sys.Date()
            )
          ),
          conditionalPanel(condition = "input.graph_selection == 'development_graph'",
                           "test2")
          
        )
      )
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(verticalLayout(
    uiOutput("ui"),
    textOutput("debug_txt")
  )))
))