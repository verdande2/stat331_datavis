#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
# ui <- fluidPage(
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
ui <- fluidPage(
  titlePanel(title = "Cars Dataset Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "slider_1",
        label = "label",
        min = 0,
        max = 100,
        value = 50
      ),
      checkboxInput(
        inputId = "checkbox_regression",
        label = "Regression"
      ),
      textOutput(outputId = "text_correlation_output")
    ),
    mainPanel(verticalLayout(
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
    ))
  )
)

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

server <- function(input, output, session) {
  output$scatter_1 <- renderPlotly({
    fig = plot_ly(
      data = iris, x = ~Sepal.Length, y = ~Petal.Length,
      type = "scatter", mode = "markers"
    ) %>%
      layout(
        title = "Sepal Length vs. Petal Length",
        xaxis = list(title = "Sepal Length"),
        yaxis = list(title = "Petal Length")
      )
    fig
  })
  
  output$histogram_1 <- renderPlotly({
    fig = plot_ly(data = diamonds, x = ~carat, type = "histogram", bingroup = 0.1) %>%
      layout(
        title = "Diamond weight",
        xaxis = list(title = "Diamond weight"),
        yaxis = list(title = "Frequency")
      )
    fig
  })
  
  output$histogram_2 <- renderPlotly({
    fig = plot_ly(data = diamonds, x = ~carat, type = "histogram", bingroup = 0.1) %>%
      layout(
        title = "Diamond weight",
        xaxis = list(title = "Diamond weight"),
        yaxis = list(title = "Frequency")
      )
    fig
  })
  
  output$text_correlation_output <- renderText({
    "This is an example for a textOutput."
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
