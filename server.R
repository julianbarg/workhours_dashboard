library(shiny)
library(tidyverse)

source <- dget("source.txt")
original <- read_csv(source)

preprocess <- dget("util/1_preprocess.R")
processed <- preprocess(original)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$selection_table <- renderTable({
        head(processed)
    })
    
    output$work_plot <- renderPlot({
        if (input$graph_selection == "time_graph"){
            in_time <- processed %>%
                filter(start_time >= input$date_selection[1] &
                           end_time <= input$date_selection[2])
            in_time %>%
                group_by(project) %>%
                summarize(project_time = sum(time_duration)) %>%
                ungroup() %>%
                arrange(project_time) %>%
                mutate(project = fct_reorder(project, project_time, .desc=T)) %>%
                ggplot(aes(x=project, y=project_time)) + 
                    geom_col()
        } else if (input$graph_selection == "development_graph"){
            ggplot(processed, aes(x=project))
        }
    })
    
    output$ui <- renderUI({
        if (input$display_choice == "Table"){
            tableOutput("selection_table")
        } else if (input$display_choice == "Graphs"){
            plotOutput("work_plot")
        }
    })
    
    output$debug_txt <- renderText(input$graphselection)
})
