library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(usmap)
library(sf)


#Import Data
df <-read_csv("data/week16_exercise.csv")
df <- df %>%
  mutate(state = tolower(state))%>%
  filter(state != "state_average")
states <-sort(unique(df$state))

# US map
states_map <- map_data("state") %>% as_tibble()
centroids <- states_map %>% group_by(region) %>% summarize(centroid_long = mean(long),
                                                           centroid_lat = mean(lat))
#Include Hawaii and Alaska centroids
Hawaii_Alaska <- data.frame(region = c("alaska","hawaii"),
                            centroid_long = c(-149.4937, -155.5828),
                            centroid_lat = c(64.2008, 19.8968))
centroids <- rbind(centroids, Hawaii_Alaska)

#Compute average exercise rate  by gender and work status
df_average <- df %>% filter(!is.na(exercise))
df1<- df_average %>% filter(work_status == "working") %>% summarise(average_work = mean(exercise))
df2<- df_average %>% filter(work_status == "non_working") %>% summarise(average_nonwork = mean(exercise))
df3 <- df_average %>% filter(sex== "male") %>% summarise(average_male = mean(exercise))
df4<- df_average %>% filter(sex== "female") %>% summarise(average_female = mean(exercise))
df_average <- bind_cols(df1, df2,df3,df4) %>% round(digits = 0)

#Compute average exercise rate by gender in each state
df_gender <- df %>% filter(work_status == "all") %>%
  select(state, sex,exercise) %>%
  pivot_wider(names_from = sex, values_from = exercise)%>%
  mutate(state = tolower(state)) %>%
  rename(region = state)

#Compute average exercise rate by work status in each state
exercise_work <- df %>%
  filter(work_status == "working") %>%
  group_by(state)%>%
  summarise(average_work = mean(exercise, na.rm = TRUE))

exercise_nonwork <- df %>%
  filter(work_status == "non_working") %>%
  group_by(state)%>%
  summarise(average_nonwork = mean(exercise, na.rm = TRUE))

df_work <- left_join(exercise_work, exercise_nonwork, by = "state") %>%
  mutate(state = tolower(state)) %>%
  rename(region = state)

# Combine dataframe with US map
df_gw <-left_join(df_gender, df_work, by = "region")
df_analysis <- left_join(df_gw, states_map, by = "region")


ui <- fluidPage(

  navbarPage("Average Exercise Rate in each state(s) in the United States by Gender or Work Status", theme = shinytheme("cerulean"),
             tabPanel("About", fluid = TRUE, icon = icon("question"),
                      titlePanel("Background"),
                      fluidRow(column(11,
                                      div(class = "about",
                                          uiOutput('about')))),
                      includeCSS("styles.css")),

             tabPanel("Analysis", fluid = TRUE, icon = icon("earth-americas"),
                      titlePanel("Physical Activity - Statistics in US"),
                      sidebarLayout(sidebarPanel(
                        fluidRow(column(10, div(radioButtons (inputId = "Guess1",
                                                              label = "On average, do working population or non-working population exercise more in the United States?  ",
                                                              choices = c("Working", "Non working"),
                                                              inline = TRUE ),
                                                radioButtons(inputId = "Guess2",
                                                             label = "On average, do male or female exercise more in the United States?  ",
                                                             choices = c("Male", "Female"),
                                                             inline = TRUE),
                                                class = "guess"),
                                        br(),
                                        actionButton("submit", "Submit your guess!", class = "btn-danger"))),
                      ),
                      mainPanel(
                        strong(textOutput("message1")),
                        br(),
                        strong(textOutput("message2")),
                        br(),
                        br(),
                        textOutput("message3")),

                      )),

             tabPanel("Demographics", fluid = TRUE, icon = icon("person-walking"),
                      titlePanel("Pick a topic of interest : Working or Gender which affected the average exercise rate in the particular state(s) in the United States?"),
                      sidebarLayout(
                        sidebarPanel(radioButtons(inputId = "topic", label = "Select topic of interest:", choices = c("Work Status", "Gender"), inline=TRUE),
                                     uiOutput("conditional")),

                        mainPanel(
                          plotOutput(outputId = "stateinfo", click = "plot_click"),
                          textOutput("info"),
                          br(),
                          fluidRow(column(width = 3, offset = 2, actionButton("click", "Click on the button to show the bar graph comparison",icon = icon("bar-chart-o"), class = "btn-lg btn-success"))),

                          fluidRow(plotOutput(outputId = "bargraph")),
                        )
                      )
             )))

server <- function(input, output, session) {

  #second tab
  output$message1 <- renderText({
    if(input$Guess1 == "Working"){
      paste("Correct!", "On average", "average exercise rate of working people in US is", df_average[1,1], "%  as compared to", df_average[1,2], "% of non working people")
    }else if(input$Guess1 == "Non working"){
      paste("Wrong guess!", "On average", "average exercise rate of working people in US is", df_average[1,1], "%  as compared to", df_average[1,2], "% of non working people")
    }
  }) %>% bindEvent(input$submit)

  output$message2 <- renderText({
    if(input$Guess2 == "Male"){
      paste("Correct!", "On average", "Male has a higher average exercise rate of", df_average[1,3], "%  as compared to female, which is at", df_average[1,4], "%.")
    }else if(input$Guess2 == "Female"){
      paste("Wrong guess!", "On average", "Male has a higher average exercise rate of", df_average[1,3], "%  as compared to female, which is at", df_average[1,4], "%.")
    }
  })%>%bindEvent(input$submit)

  output$message3 <- renderText({
    paste("Tips : Go to next tab for further analysis on the average exercise rate in each state(s) in the United States by Gender or Work Status")
  })%>%bindEvent(input$submit)

  # Third tab
  df_finder<-reactive({
    filter(df_analysis, region %in% input$State)
  }) #%>% bindEvent(input$State) #observeEvent , detailed master shiny...


  centroid_finder <- reactive ({
    filter(centroids, region %in% input$State)
  })

  df_table <- reactive({
    filter(df_gw, region %in% input$State)
  })

  output$conditional <- renderUI({
    if(input$topic == "Work Status"){
      tagList(
        h4("Has working affected the average exercise rate at state(s) level in the United States?"),
        fluidRow(column (4,
                         radioButtons (inputId = "Workstatus",
                                       label = "Select work status:",
                                       choices = c ("working", "non working"),
                                       selected = "working")
        ),
        column(5,
               selectizeInput(inputId = "State",
                              label = "Select state(s):",
                              choices = states,
                              selected = c("alabama", "florida"),
                              multiple=TRUE))
        ))} else if(input$topic == "Gender") {
          tagList(
            h4("Gender differences in response to average exercise rate at state(s) level in the United States"),
            fluidRow(column (4,
                             radioButtons (inputId = "Gender",
                                           label = "Select gender:",
                                           choices = c ("Male", "Female"),
                                           selected = "Female")
            ),
            column(5,
                   selectizeInput(inputId = "State",
                                  label = "Select state(s):",
                                  choices = states,
                                  selected = c("alabama", "florida"),
                                  multiple=TRUE))))
        }
  })


  output$stateinfo<- renderPlot({

    fill_finder <- function(fill_by){
      ggplot(states_map, aes(long, lat, group = region)) +
        geom_polygon(data= states_map, aes(long, lat, group = region), fill ="grey", color ="white")+
        geom_polygon(data = df_finder(), aes(long, lat, group = region, fill = get(fill_by)), color = "white")+
        geom_point(data = centroid_finder(), aes(x = centroid_long, y = centroid_lat))+
        geom_label(data = centroid_finder(), aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 5) +
        labs (fill = "Average Exercise Rate")+
        theme_void()+
        scale_fill_viridis_c(option = "C")}

    if(input$topic == "Work Status"){
      if(input$Workstatus == "working"){
        fill_finder("average_work")
      }else if (input$Workstatus == "non working"){
        fill_finder("average_nonwork")
      }
    }else if (input$topic == "Gender"){
      if(input$Gender == "Male"){
        fill_finder("male")
      }else if (input$Gender == "Female"){
        fill_finder("female")
      }
    }
  })

  output$info <- renderText({
    req(input$plot_click)
    y <- if(input$topic == "Work Status"){
      if(input$Workstatus == "working"){
        df_table()$average_work
      }else if (input$Workstatus == "non working"){
        df_table()$average_nonwork
      }
    }else if (input$topic == "Gender"){
      if(input$Gender == "Male"){
        df_table()$male
      }else if (input$Gender == "Female"){
        df_table()$female
      }
    }
    paste("On average, average exercise rate in", x[1,1], "is", y, "%.")
  })



  table_data <- reactive({
    df_wide <- df_gw %>% pivot_longer(cols = -region, names_to = "demographics", values_to = "avg_rate")
    filter(df_wide, region %in% input$State)
  }) %>% bindEvent(input$topic, input$State)


  output$bargraph<- renderPlot({

    if(input$click){

      ggplot(data = table_data(), aes(x = reorder(region, avg_rate), y = avg_rate, fill = demographics))+
        geom_col()+
        theme( axis.text.x = element_text(angle=45, hjust = 1)) + labs(x = "States", y = "Average Exercise Rate")+
        facet_wrap(~demographics)

    } else {
      ""
    }

  },  width = 600 , height = "auto", res = 96)%>% bindEvent(input$topic,input$click, input$Gender , input$Workstatus, input$State)

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })


  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })

}

shinyApp(ui, server)
