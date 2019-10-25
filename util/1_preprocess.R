library(tidyverse)

drop_mistakes <- function(data){
  # When either a job start or job end appears twice in a row, I must have made a mistake.
  # In this case, we drop the first occurance for start entries, or the second for end entries.
  data <- data %>%
    mutate(next_start = lag(start),
           previous_start = lead(start))
  
  data$double_start <- ifelse(data$start == 1 & data$previous_start == 1, TRUE, FALSE)
  data$double_quit <- ifelse(data$start == 0 & data$next_start == 0, TRUE, FALSE)
  data$mistake <- data$double_start | data$double_quit
  
  data <- data %>%
    filter(mistake == FALSE) %>%
    select(-c(double_start, double_quit, mistake, next_start, previous_start))
  
  return(data)
}

drop_unfinished <- function(data){
  # Drop the last row, if it is a project start entry.
  if(data[nrow(data), ]$start == 1){
    data <- head(data, -1)
  }
  
  return(data)
}

cut_and_paste <- function(data){
  # Cut and paste start and end together. This step is the most likely to fail, if I make a 
  # wrong entry. If the app was running locally, I could send an email to myself to 
  # inform me that something is wrong with the data, but because it will be running as a 
  # remote shiny app, I cannot use the email protocoll. So I will just try my best not to
  # enter data wrong, and fix it if I do, and discover that somehting is out of whack with 
  # the shiny app.
  
  even <- seq(2, nrow(data), 2)
  odd <- seq(1, nrow(data), 2)
  
  even_rows <- data[even, ]
  odd_rows <- data[odd, ]
  stopifnot(all(odd_rows$start == 1), all(even_rows$start == 0))
  
  odd_rows <- select(odd_rows, -start)
  even_rows <- select(even_rows, -start)
  
  odd_rows <- rename(odd_rows, start_time = timestamp)
  even_rows <- rename(even_rows, end_time = timestamp, check_project = project)
  
  data <- cbind(odd_rows, even_rows)
  stopifnot(all(data$project == data$check_project))
  data <- select(data, -check_project)
  
  return(data)
}

convert_timezone <- function(df, time_col, tz, hours_difference){
  time_col <- enquo(time_col)
  
  df <- df %>%
    mutate(!! time_col := !! time_col + hours(hours_difference)) %>%
    mutate(!! time_col := with_tz(!! time_col, "EDT"))
  
  return(df)
}

preprocess <- function(data){
  data <- drop_mistakes(data = data)
  data <- drop_unfinished(data = data)
  data <- cut_and_paste(data)
  
  data$time_duration <- difftime(data$end_time, data$start_time)
  data$time_duration <- as.integer(data$time_duration)
  
  data <- convert_timezone(data, start_time, "EDT", -4)
  data <- convert_timezone(data, end_time, "EDT", -4)
  
  data$project <- map_chr(data$project, ~ gsub("_", " ", .))
  data$project <- tools::toTitleCase(data$project)
  
  data <- data  %>%
    mutate(project = fct_reorder(project, start_time, .desc=T))
  
  data$start_time <- as.character.Date(data$start_time)
  data$end_time <- as.character.Date(data$end_time)
  
  return(data)
}