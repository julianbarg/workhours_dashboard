library(shiny)
library(tidyverse)

source <- dget("source.txt")
original <- read_csv(source)

preprocess <- dget("util/1_preprocess.R")
processed <- preprocess(original)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    recent_table <- reactive({
        processed %>%
            arrange(desc(start_time)) %>%
            mutate(time_duration = paste0(time_duration, " minute(s)")) %>%
            rename(
                "Project" = project,
                "Start" = start_time,
                "Duration" = time_duration
            ) %>%
            select(-end_time)
    })
    
    project_table <- reactive({
        total_time <-
            sum(subset(
                processed,
                project == input$project_choice,
                time_duration
            ))
        total_time <-
            paste0(total_time %/% 60,
                   " hours and ",
                   total_time %% 60,
                   " minutes")
        
        processed %>%
            filter(project == input$project_choice) %>%
            mutate(time_duration = paste0(time_duration, " minute(s)")) %>%
            add_row(
                project = "Sum",
                start_time = " ",
                time_duration = total_time
            ) %>%
            rename(
                "Project" = project,
                "Start" = start_time,
                "Duration" = time_duration
            ) %>%
            select(-end_time)
    })
    
    datasetInput <- reactive({
        switch(input$table_type,
               "recent" = recent_table(),
               "project" = project_table())
    })
    
    output$selection_table <- renderTable({
        datasetInput()
    })
    
    output$work_plot <- renderPlot({
        if (input$graph_selection == "time_graph") {
            in_time <- processed %>%
                filter(start_time >= input$date_selection[1] &
                           end_time <= input$date_selection[2])
            in_time %>%
                group_by(project) %>%
                summarize(project_time = sum(time_duration)) %>%
                ungroup() %>%
                arrange(project_time) %>%
                mutate(project = fct_reorder(project, project_time, .desc =
                                                 T)) %>%
                ggplot(aes(x = project, y = project_time)) +
                geom_col()
        } else if (input$graph_selection == "development_graph") {
            ggplot(processed, aes(x = project))
        }
    })
    
    output$ui <- renderUI({
        if (input$display_choice == "Table") {
            tableOutput("selection_table")
        } else if (input$display_choice == "Graphs") {
            plotOutput("work_plot")
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$project_choice, " hours as of ", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(project_table(), file, row.names = FALSE)
        }
    )
})
