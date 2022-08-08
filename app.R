library(shiny)
library(tidyverse)

ui <- fluidPage(

    titlePanel("Old Faithful Geyser Data"),

    h3("1. What number of bins do you stop seeing bimodality in the waiting time?"),
    fluidRow(
      sidebarLayout(
          sidebarPanel(
              sliderInput("bins",
                          "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30)
          ),

          mainPanel(
             plotOutput("distPlot")
          )
      )
    ),

    h3("2. How do the different geoms change the view of the data?"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          radioButtons("geom",
                      "Geom choice:",
                      choices = c("geom_point",
                                  "geom_density_2d",
                                  "geom_density_2d_filled",
                                  "geom_bin_2d",
                                  "geom_hex"))
        ),

        mainPanel(
          plotOutput("plot")
        )
      )
    ),

    h3("3. Is a mixture of two normal distribution good fit on eruption time?"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins2",
                      "Adjust the number of bins (if needed):",
                      min = 1,
                      max = 50,
                      value = 30),
          "Enter your guess for the:",
          numericInput("p", "Mixing probability:",
                       value = 0.35, min = 0, max = 1),
          numericInput("mean1", "Mean of the first group:",
                       value = 2.02),
          numericInput("mean2", "Mean of the second group:",
                       value = 4.27),
          numericInput("sd1", "Standard deviation of the first group:",
                       value = 0.24, min = 0),
          numericInput("sd2", "Standard deviation of the second group:",
                       value = 0.44, min = 0)
        ),

        mainPanel(
          plotOutput("mixDistFit")
        )
      )
    ),


    fluidRow(
      column(10,
             div(class = "about",
                 uiOutput('about'))
      )
    ),
    includeCSS("styles.css")
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(faithful, aes(waiting)) +
         geom_histogram(bins = input$bins, color = "white") +
         theme_bw(base_size = 14) +
         labs(x = "Waiting time", y = "Count")
    })

    output$plot <- renderPlot({
      ggplot(faithful, aes(waiting, eruptions)) +
        get(input$geom)() +
        theme_bw(base_size = 14) +
        labs(x = "Waiting time", y = "Eruption time")
    })

    output$mixDistFit <- renderPlot({
      df <- data.frame(x = seq(min(faithful$eruptions), max(faithful$eruptions), length = 1000)) %>%
        mutate(density = input$p * dnorm(x, input$mean1, input$sd1) +
                         (1 - input$p) * dnorm(x, input$mean2, input$sd2))

      ggplot(faithful, aes(eruptions)) +
        geom_histogram(aes(y = stat(density)), bins = input$bins2, color = "white") +
        geom_line(data = df, aes(x = x, y = density), color = "red", size = 2) +
        theme_bw(base_size = 14) +
        labs(x = "Eruption time", y = "Density")
    })

    output$about <- renderUI({
      knitr::knit("about.Rmd", quiet = TRUE) %>%
        markdown::markdownToHTML(fragment.only = TRUE) %>%
        HTML()
    })
}

shinyApp(ui = ui, server = server)
