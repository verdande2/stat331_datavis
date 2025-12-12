#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(bslib) # for tabset
library(gt)

# clear out cwd
rm(list = ls())

# load cars dataset in
data <- cars

ui <- fluidPage(
  titlePanel(title = "Cars Dataset Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "slider_speed",
        label = "Filter by Speed",
        min = min(data$speed) - 2,
        max = max(data$speed) + 2,
        value = c(10, 20) # using a range in a c() makes the slider select an interval
      ),
      checkboxInput(
        inputId = "checkbox_regression",
        label = "Fit Regression Line?"
      ),
      textOutput(outputId = "text_correlation_output")
    ),
    mainPanel(
      navset_tab(
        nav_panel(
          "Car Dataset Vis Dashboard",
          fluidRow(plotlyOutput(outputId = "scatter_1")),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(
                outputId = "histogram_1",
                height = "150px"
              )
            ),
            column(
              width = 6,
              plotlyOutput(
                outputId = "histogram_2",
                height = "150px"
              )
            )
          )
        ),
        nav_panel(
          "Second Tab",
          fluidRow(
            dataTableOutput("filtered_data_table")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  filtered_data <- reactive({
    data |>
      filter(speed >= input$slider_speed[1] & speed <= input$slider_speed[2]) # filter data in interval
  })

  output$scatter_1 <- renderPlotly({
    fil <- filtered_data() # alias filtered_data() to fil

    # plot the data speed vs dist, add labels and scatterplot points
    fig <- ggplot(fil, aes(x = speed, y = dist)) +
      labs(title = "Speed vs Distance", x = "Speed", y = "Distance") +
      geom_point()

    # if the "Fit Regression Line" checkbox is checked, draw the regression line
    if (input$checkbox_regression) {
      fig <- fig + geom_smooth(method = "loess", formula = "Speed ~ Dist")
    }

    # spit out the figure
    fig
  })

  output$histogram_1 <- renderPlotly({
    fil <- filtered_data()
    fig <- fil |> ggplot(aes(x = speed)) + geom_histogram()

    ggplotly(fig)
  })

  # output$histogram_2 <- renderPlotly({
  #   # fig = plot_ly(data = filtered_data, x = ~dist, type = "histogram", bingroup = 0.1) |>
  #   #   layout(
  #   #     title = "Distance Histogram",
  #   #     xaxis = list(title = "Distance"),
  #   #     yaxis = list(title = "Frequency")
  #   #   )
  #   # fig
  # })

  output$text_correlation_output <- renderText({
    fil <- filtered_data()
    x <- cor(fil) # calc corr coeff for speed/dist
    x <- round(x, 3) # truncate off the correlation coeff to 3 decimal places
    paste("The correlation between speed and distance is ", x[2], ".", sep = "") # slam together the string with data
  })

  # on second tab, for filtering debug
  output$filtered_data_table <- renderDataTable(expr = filtered_data())
}

# Run the application
shinyApp(ui = ui, server = server)
