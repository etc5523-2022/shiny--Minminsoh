library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(usmap)
library(sf)
library(patchwork)
library(colorspace)
library(scales)
library(viridis)
library(reactable)


#Import Data
df <-read_csv("data/week16_exercise.csv")
df <- df %>%
  mutate(state = tolower(state))%>%
  filter(state != "state_average") %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  filter(!state == "District of Columbia")
states <-sort(unique(df$state))

# US map
# Focus on the continental United States, excluding Alaska, Hawaii and the other Pacific Islands.
# Exclude District of Columbia as it is a district not a state
# There are 50 states in US - less Alaska and Hawaii equal to a total of 48 states.
states_map <- map_data("state") %>% as_tibble()
USstates <- bind_cols(state.name, state.abb)%>%
  rename(region =...1, abb = ...2)%>%
  mutate(region = tolower(region))
states_map <- map_data("state") %>% as_tibble()
states_map  <- left_join(USstates, states_map, by = "region")

#Compute centroids
centroids <- states_map %>%
  filter(!region%in% c("alaska", "hawaii")) %>%
  group_by(abb) %>%
  summarize(centroid_long = mean(long),
            centroid_lat = mean(lat))
centroids <- left_join(centroids, USstates, by = "abb")


#Compute average exercise rate  by gender and work status
df_average <- df %>% filter(!is.na(exercise))
df1<- df_average %>% filter(work_status == "working") %>% summarise(average_work = mean(exercise))
df2<- df_average %>% filter(work_status == "non_working") %>% summarise(average_nonwork = mean(exercise))
df3 <- df_average %>% filter(sex== "male") %>% summarise(average_male = mean(exercise))
df4<- df_average %>% filter(sex== "female") %>% summarise(average_female = mean(exercise))
df_average <- bind_cols(df1, df2,df3,df4) %>%
  round(digits = 0) %>%
  rename(`Working Adults` = average_work,
          `Non Working Adults` = average_nonwork,
           Men = average_male,
           Women = average_female)%>%
  pivot_longer(cols = `Working Adults`: Women,
               names_to = "Demographics",
               values_to = "Avg_exercise")

#Compute average exercise rate in each state
df_state <- df %>%
  filter(sex == "both")%>%
  select(state, exercise)%>%
  arrange(-exercise)%>%
  mutate(state = tolower(state),
         rank = row_number())%>%
  mutate(Category = case_when(
    exercise<22  ~ "Below Average",
    between(exercise, 22, 23)  ~ "Average",
    exercise>23  ~ "Above Average"
  ))%>%
  rename(region = state)

centroids_state <- left_join(centroids, df_state, by = "region")
df_state_plot <- left_join(df_state, states_map, by = "region")
my_palette <- rev(magma(8))

# Ranking in each state
df_table_ranking <- df_state %>% select(region:rank) %>%
  mutate(region = toupper(region))%>%
  rename(State = region,
         `Adults reaching the National Physical Guidelines (%)` = exercise,
         Ranking = rank)

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
df_gw <-left_join(df_gender, df_work, by = "region") %>% rename(working = average_work, non_working = average_nonwork)
df_analysis <- left_join(df_gw, states_map, by = "region")


ui <- fluidPage(

  navbarPage("Percentage of adults who met the federal physical activity guidelines in the United States", theme = shinytheme("cerulean"),
             tabPanel("About", fluid = TRUE, icon = icon("question"),
                      titlePanel("Background"),
                      fluidRow(column(11,
                                      div(class = "about",
                                          uiOutput('about')))),
                      includeCSS("styles.css")),

             tabPanel("Background",fluid = TRUE,
                      fluidRow(column(tags$img(src="download.jpeg",width="200px",height="260px"),width=2),
                               column(
                                 br(),
                                 p("Keeping active is essential for maintaining physical health and weight. Major benefits include preventing health problems such as stroke, heart disease, type 2 diabetes and certain types of cancer. Despite the benefits of being physically active, physical inactivity is identified as the",
                                   strong("fourth leading risk factor"), "for death globally.", style="text-align:justify; color:black; background-color:lavender; padding:15px; border-radius:10px"),
                                 br(),
                                 p("According to Centers for Disease Control and Prevention, the US obesity prevalence has increased from 30.5% to 41.9% from 2000 to 2020. Obesity also leads to a significant medical costs in the United States, where the estimated annual medical costs was around $173 billion in 2019.",
                                 style="text-align:justify; color:black; background-color:lavender; padding:15px; border-radius:10px"),
                                 br(),
                                 p("Our comparison is based on the Physical Activity Guidelines for Americans issued by the U.S.Department of Health and Human Service (DHHS).To meet the physical activity guidelines, adults should reach at least 150 minutes
                                   a week of moderate-intensity or 75 mins a week of vigorous-intensity aerobic activity, or an equivalent combination of moderate- and vigorous-intensity aerobic activity.",
                                   style="text-align:justify; color:black; background-color:lavender; padding:15px; border-radius:10px"),
                                 width = 8),
                               )),

             tabPanel("Fact questions", fluid = TRUE, icon = icon("earth-americas"),
                      titlePanel("Exercise Rate Comparison in the United States (US)"),
                      sidebarLayout(sidebarPanel(
                        fluidRow(column(12, div(radioButtons (inputId = "Guess1",
                                                              label = "On average, do working population or non-working population exercise more in the United States?  ",
                                                              choices = c("Working", "Non working"),
                                                              inline = TRUE ),
                                                radioButtons(inputId = "Guess2",
                                                             label = "On average, do male or female exercise more in the United States?  ",
                                                             choices = c("Male", "Female"),
                                                             inline = TRUE),
                                                class = "guess"),
                                        br(),
                                        actionButton("submit", "Submit your guess!", class = "btn-lg btn-success"))),
                      ),
                      mainPanel(
                        strong(textOutput("message1")),
                        br(),
                        strong(textOutput("message2")),
                        br(),
                        br(),
                        textOutput("message3"),
                        plotOutput(outputId = "genderworkgraph"))


                      )),
             tabPanel("State", fluid = TRUE,
                      titlePanel("Geographic Differences? - What about exercise rate in each state in US? "),
                      sidebarLayout(
                        sidebarPanel(
                          tags$head(tags$style("#AdjustedDetermination{color: black;
                                                                        text-align: center;
                                                                        }")),
                          checkboxGroupInput(inputId="category",
                                             label ="Comparing the State Physical Activity Rate to the National Average",
                                             choices = c("Above Average", "Average", "Below Average")),
                        ),
                        mainPanel(plotOutput(outputId = "stateComparison"),
                                  textOutput("stateComparisonMessage"),
                                  dataTableOutput("stateComparisonTable"))
                      ) ),

             tabPanel("Demographics", fluid = TRUE, icon = icon("person-walking"),
                      titlePanel("Pick a topic of interest : Working or Gender which affected the average exercise rate in the particular state(s) in the United States?"),
                      sidebarLayout(
                        sidebarPanel(div(radioButtons(inputId = "topic", label = "Select topic of interest:", choices = c("Work Status", "Gender"), inline=TRUE),
                                     uiOutput("conditional")), class = "guess" ),

                        mainPanel(
                          plotOutput(outputId = "stateinfo"),
                          textOutput("info"),
                          br(),
                          fluidRow(column(width = 3, offset = 2, actionButton("click", "Click on the button to show the bar graph comparison",icon = icon("bar-chart-o"), class = "btn-lg btn-success"))),

                          fluidRow(plotOutput(outputId = "bargraph")),
                        )
                      )
             )))

server <- function(input, output, session) {

#Second tab - Fact Questions - Render messages
  output$message1 <- renderText({
    if(input$Guess1 == "Working"){
      paste("Correct!","Working adults are more active in their lifestyles than non working adults.", "On average", "working people has a higher average exercise rate of", df_average[1,2], "% in the US as compared to non working people, which is at the rate of", df_average[2,2], "%.")
    }else if(input$Guess1 == "Non working"){
      paste("Wrong guess!", "Working adults are more active in their lifestyles than non working adults", "On average", "working people has a higher average exercise rate of", df_average[1,2], "% in the US as compared to non working people, which is at the rate of", df_average[2,2], "%.")
    }
  }) %>% bindEvent(input$submit)

  output$message2 <- renderText({
    if(input$Guess2 == "Male"){
      paste("Correct!", "Men tend to pursue more physical activities than women.", "On average", "male has a higher average exercise rate of", df_average[3,2], "%  as compared to female, which is at", df_average[4,2], "%.")
    }else if(input$Guess2 == "Female"){
      paste("Wrong guess!", "Men tend to pursue more physical activities than women.", "On average", "male has a higher average exercise rate of", df_average[3,2], "%  as compared to female, which is at", df_average[4,2], "%.")
    }
  })%>%bindEvent(input$submit)

  output$message3 <- renderText({
    paste("Tips : Go to next tab - Demographics for further analysis on the average exercise rate in selected state(s) in the United States by Gender or Work Status")
  })%>%bindEvent(input$submit)

  output$genderworkgraph<- renderPlot({

    g1<-df_average %>% filter(Demographics %in% c("Working Adults", "Non Working Adults"))%>%
      ggplot(aes(Demographics, Avg_exercise))+
      geom_col()+
      geom_col(data = ~filter(.x, Demographics == "Working Adults"),
               fill = "#6295ED")+
     geom_text(aes(label = paste(Avg_exercise, "%", sep = "")),
                position= position_stack(vjust = 0.5), color = "white",size = 5)+
      labs(y = "Percentage of Population in US (%)")+
      ggtitle("Working Adults VS Non Working Adults")+
      theme_classic()

    g2<- df_average %>% filter(Demographics %in% c("Men", "Women"))%>%
         ggplot(aes(Demographics, Avg_exercise))+
         geom_col()+
         geom_col(data = ~filter(.x, Demographics == "Men"),
               fill = "#6295ED")+
         geom_text(aes(label = paste(Avg_exercise, "%", sep = "")),
                position= position_stack(vjust = 0.5), color = "white",size = 5)+
         labs(y = "Percentage of Population in US (%)")+
         ggtitle("Men VS Women")+
         theme(axis.title = element_blank())+
         theme_classic()

  g1+g2

  }) %>%bindEvent(input$submit)

#Third tab

  output$stateComparison<- renderPlot({
    df_region <- reactive({
      filter(df_state_plot, Category%in%input$category )
    })

   ggplot(df_state_plot, aes(long, lat, group = region)) +
        geom_polygon(data = df_state_plot, aes(long, lat, group = region), fill = "grey", color ="white")+
        geom_polygon(data = df_region(), aes(long, lat, group = region, fill = Category )) +
        geom_text(data = centroids_state, aes (x = centroid_long, y = centroid_lat, label=abb), size  = 2.5, alpha = 2, colour = "black")+
        scale_fill_manual(values = my_palette,
                          name = "Exercise Rate compared to the National Average",
                          guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1))+
        ggtitle("Comparing the State Physical Activity Rate to the National Average")+
        theme_void()+
        theme(
          legend.position = "top",
          plot.background = element_rect(fill = "#f5f5f5", color = NA),
          panel.background = element_rect(fill = "#f5f5f5", color = NA),
          legend.background = element_rect(fill = "#f5f5f5", color = NA),
          plot.title = element_text(size= 13, hjust=0.5, color = "#4e4d47"))
  })

  output$stateComparisonMessage <- renderText({
   "Counties in the central such as Colorado and Utah as well as along the west coasts such as California are much more active where there are higher percentage of adults having more exercise than the national average.
    Most of the states in the southeast are below than the national average, such as Kentucky, Mississippi, West Virginia and South Carolina."
  })%>%bindEvent(input$category)

  output$stateComparisonTable <- renderDataTable(
    df_table_ranking, options = list(pageLength = 5)
    )

# Fourth tab - Demographics
# df analysis includes the latitude and longitude for plotting the US map

  df_finder<-reactive({
    filter(df_analysis, region %in% input$State)
  })

  centroid_finder <- reactive ({
    filter(centroids, region %in% input$State)
  })
#df table for plotting bar graph comparison
  df_table <- reactive({
    filter(df_gw, region %in% input$State)
  })

  output$conditional <- renderUI({
    if(input$topic == "Work Status"){
      tagList(
        h4(div("Has working affected the average exercise rate at state(s) level in the United States?"), class = "white"),
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
            h4(div("Gender differences in response to average exercise rate at state(s) level in the United States"), class = "white"),
            fluidRow(column (4,
                             radioButtons (inputId = "Gender",
                                           label = "Select gender:",
                                           choices = c ("Male", "Female"),
                                           selected = "Female")
            ),
            column(5,
                   selectizeInput(inputId = "State",
                                  label = "Select state(s) for comparison:",
                                  choices = states,
                                  selected = c("alabama", "florida"),
                                  multiple=TRUE))))
        }
  })

#plot us map
  output$stateinfo<- renderPlot({

    fill_finder <- function(fill_by){
      ggplot(states_map, aes(long, lat, group = region)) +
        geom_polygon(data= states_map, aes(long, lat, group = region), fill ="grey", color ="white")+
        geom_polygon(data = df_finder(), aes(long, lat, group = region, fill = get(fill_by)), color = "white")+
        geom_point(data = centroid_finder(), aes(x = centroid_long, y = centroid_lat))+
        geom_label(data = centroid_finder(), aes (x = centroid_long, y = centroid_lat, label = as.character(region)), size  = 5) +
        labs (fill = "Average Exercise Rate")+
        theme_void()+
        scale_fill_viridis_c()}

    if(input$topic == "Work Status"){
      if(input$Workstatus == "working"){
        fill_finder("working")
      }else if (input$Workstatus == "non working"){
        fill_finder("non_working")
      }
    }else if (input$topic == "Gender"){
      if(input$Gender == "Male"){
        fill_finder("male")
      }else if (input$Gender == "Female"){
        fill_finder("female")
      }
    }
  })

# pivot longer df for bar graph comparison using facet wrap
  table_data <- reactive({
    df_wide <- df_gw %>% pivot_longer(cols = -region, names_to = "demographics", values_to = "avg_rate")
    filter(df_wide, region %in% input$State)
  }) %>% bindEvent(input$topic, input$State)

# plot bar graph
  output$bargraph<- renderPlot({

    if(input$click){

      ggplot(data = table_data(), aes(x = reorder(region, avg_rate), y = avg_rate, fill = demographics))+
        geom_col()+
        theme( axis.text.x = element_text(angle=45, hjust = 1)) + labs(x = "States", y = "Average Exercise Rate")+
        facet_wrap(~demographics)+
        scale_fill_discrete(breaks = c("working", "non_working", "female", "male"),
                            labels = c ("Working" , "Non working", "Female", "Male"))+
        ggtitle("Average Exercise Rate by Gender and Work Status of the state(s) selected")


    } else {
      ""
    }

  },  width = 800 , height = "auto", res = 96)%>% bindEvent(input$topic,input$click, input$Gender , input$Workstatus, input$State)

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })



}

shinyApp(ui, server)
