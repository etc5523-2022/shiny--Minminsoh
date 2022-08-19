library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
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
df_gender <- df %>% select(!work_status) %>% filter(sex!= "both")

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

df_work_states <-left_join(states_map, df_work, by = "region")


#plot us maps
ggplot(df_work_states, aes(long, lat, group = region)) +
  geom_polygon(data= states_map, aes(long, lat, group = region), fill ="grey", color ="white")+
  geom_polygon(aes(fill = average_work), colour = "white")+
  scale_fill_viridis_c(option = "C")+
  geom_point(data = centroids, aes(x = centroid_long, y = centroid_lat, alpha = 0.8))+
  geom_label(data = centroids, aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 2.5) +
  theme(plot.title = element_text(hjust=0.5, face = "bold")) +
  theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+theme_void()

ggplot() +
  geom_polygon(data = df_work_states , aes(x = long, y = lat, group = region, fill = average_work), color = "white", fill = "grey") +
  geom_point(data = centroids, aes(x = centroid_long, y = centroid_lat, alpha = 0.8))+
  geom_label(data = centroids, aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 2)




ui <- fluidPage(

  navbarPage("Average Exercise Rate in each state(s) in the United States by Gender or Work Status",
             tabPanel("About", fluid = TRUE, icon = icon("question"),
                      titlePanel("Background"),
                      fluidRow(column(11,
                                      div(class = "about",
                                          uiOutput('about')))),
                      includeCSS("styles.css")),

             tabPanel("Analysis", fluid = TRUE, icon = icon("earth-americas"),
                      titlePanel("Physical Activity - Statistics in US"),
                      sidebarLayout(sidebarPanel(
                        fluidRow(column(10, radioButtons (inputId = "Guess1",
                                                          label = "On average, do working population or non-working population exercise more in the United States?  ",
                                                          choices = c("Working", "Non working"),
                                                          inline = TRUE ),
                                        radioButtons(inputId = "Guess2",
                                                     label = "On average, do male or female exercise more in the United States?  ",
                                                     choices = c("Male", "Female"),
                                                     inline = TRUE),
                                        actionButton("submit", "Submit your guess!"))),
                      ),
                      mainPanel(textOutput("message1"),
                                br(),
                                textOutput("message2"))

                      )),

             tabPanel("Demographics", fluid = TRUE, icon = icon("person-walking"),
                      titlePanel("Has working affected the average exercise rate at state(s) level in the United States?"),
                      sidebarLayout(
                        sidebarPanel(
                          tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
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
                                                multiple=TRUE),
                                 helpText("Select more states which will show the entire map in US!"))
                          )),
                        mainPanel(
                          fluidRow(column(3,
                                          radioButtons(inputId = "statename",
                                                       label = "Display:",
                                                       choices = c("states","Neither"),
                                                       selected = "states"))),

                          plotOutput(outputId = "stateinfo"),
                          br(),
                          fluidRow(column(width = 3, offset = 2, actionButton("click", "Click on the button to show the bar graph comparison",icon = icon("bar-chart-o")))),
                          fluidRow(plotOutput(outputId = "bargraph")),
                        )
                      ))

  ))

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



  df_finder<-reactive({
    filter(df_work_states, region %in% input$State)
  }) #%>% bindEvent(input$State) #observeEvent , detailed master shiny...

  #states_name <- reactive({
  #  filter(states_map, region %in% input$RegionFinder)
  #})

  centroid_finder <- reactive ({
    filter(centroids, region %in% input$State)
  })

  output$stateinfo<- renderPlot({
    if(input$Workstatus == "working"){
      ggplot(states_map, aes(long, lat, group = region)) +
        geom_polygon(data= states_map, aes(long, lat, group = region), fill ="grey", color ="white")+
        geom_polygon(data = df_finder(), aes(long, lat, group = region, fill = average_work), color = "white") + #fill use case when
        scale_fill_viridis_c(option = "C")+
        geom_point(data = centroid_finder(), aes(x = centroid_long, y = centroid_lat))+
        geom_label(data = centroid_finder(), aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 5) +
        labs (fill = "Average Exercise Rate")+
        theme_void()
    } else if (input$Workstatus == "non working"){
      ggplot(states_map, aes(long, lat, group = region)) +
        geom_polygon(data= states_map, aes(long, lat, group = region), fill ="grey", color ="white")+
        geom_polygon(data = df_finder(), aes(long, lat, group = region, fill = average_nonwork), color = "white") +
        scale_fill_viridis_c(option = "C")+
        geom_point(data = centroid_finder(), aes(x = centroid_long, y = centroid_lat))+
        geom_label(data = centroid_finder(), aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 5)+
        labs (fill = "Average Exercise Rate")+
        theme_void()

    }

  })

  work_data <- reactive({
    df_work_wide <- df_work %>% pivot_longer(cols = -region, names_to = "work_status", values_to = "avg_rate")
    filter(df_work_wide, region %in% input$State)
  }) %>% bindEvent(input$click) #question - doesnt change afterwards ??


  output$bargraph<- renderPlot({

    if(input$click){
      ggplot(data = work_data(), aes(x = reorder(region, avg_rate), y = avg_rate ))+
        geom_col(fill = "lightblue")+
        theme( axis.text.x = element_text(angle=45, hjust = 1)) + labs(x = "States", y = "Average Exercise Rate")+
        facet_grid(~work_status)
    } else {
      ""
    }

  })%>% bindEvent(input$click)

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })

}

shinyApp(ui, server)
