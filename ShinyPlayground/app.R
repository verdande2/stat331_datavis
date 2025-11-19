#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Shiny Playground"),
  dashboardSidebar(
    fluidRow(
      selectInput(
        inputId = "select_dataset",
        label = "Select a Dataset",
        choices = list("iris", "mtcars", "diamonds")
      ),
      selectInput(
        inputId = "select_col",
        label = "Select a Feature",
        choices = NULL,
        selectize = TRUE
      ),
      fluidRow(
        conditionalPanel(
          condition = "output.column_type == 'categorical'",
          checkboxGroupInput(
            inputId = "filter_categorical",
            label = "Select Levels:",
          )
        ),
        conditionalPanel(
          condition = "output.column_type == 'numeric'",
          sliderInput(
            inputId = "filter_numeric",
            label = "Select Value Filter:",
            value = 0,
            min = 0,
            max = 100
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  df <- reactive({
    req(input$select_dataset)


    switch(input$select_dataset,
      "diamonds" = diamonds,
      "mtcars" = mtcars,
      "iris" = iris
    )


    observeEvent(input$select_dataset, {
      req(df())
      updateSelectInput(
        session,
        inputId = "select_col",
        choices = names(df()),
        selected = NULL
      )
    })
  })

  output$column_type <- reactive({
    req(input$select_col)

    selected_col <- input$select_col

    col_data <- df() |>
      select(!!sym(selected_col)) |>
      na.omit() |>
      head(n = 100)

    val_classes <- sapply(col_data, class)
    common_class <- names(table(val_classes))[which.max(table(val_classes))]

    if (common_class %in% c("double", "integer", "numeric")) {
      return("numeric")
    } else if (common_class %in% c("character", "factor", "ordered", "logical")) {
      return("categorical")
    } else {
      return("special")
    }
  })

  outputOptions(output, "column_type", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
