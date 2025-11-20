library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Shiny Playground"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Columns",
        tabName = "select_variable",
        icon = icon("dashboard")
      ),
      fluidRow(
        selectInput(
          inputId = "select_dataset",
          label = "Select a Dataset",
          choices = c(
            "diamonds" = "diamonds",
            "mtcars" = "mtcars",
            "iris" = "iris"
          ),
        ),
        selectInput(
          inputId = "select_col",
          label = "Select a Feature",
          choices = NULL,
          selectize = TRUE
        ),
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
  ),
  dashboardBody(
    fluidRow(
      conditionalPanel(
        condition = "output.column_type == 'categorical'",
        uiOutput("dynamic_categorical_graphs")
      ),
      conditionalPanel(
        condition = "output.column_type == 'numeric'",
        uiOutput("dynamic_numeric_graphs")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  df <- reactive({
    req(input$select_dataset)


    print("in df reactive")
    switch(input$select_dataset,
      "diamonds" = diamonds,
      "mtcars" = mtcars,
      "iris" = iris
    )


    observeEvent(input$select_dataset, {
      req(df())
      freezeReactiveValue(input, "select_col")
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

    print("in column type reactive")
    selected_col <- input$select_col

    col_data <- df() |>
      select(!!sym(selected_col)) |>
      na.omit()
    # head(n = 100)
    common_class <- class(col_data[[1]])
    print(common_class)

    # val_classes <- sapply(col_data, class)
    # common_class <- names(table(val_classes))[which.max(table(val_classes))]

    if (common_class %in% c("double", "integer", "numeric")) {
      return("numeric")
    } else if (common_class %in% c("character", "factor", "ordered", "logical")) {
      return("categorical")
    } else {
      return("special")
    }
  })

  outputOptions(output, "column_type", suspendWhenHidden = FALSE)

  # update the inputs that are conditional
  observeEvent(input$select_col, {
    req(input$select_col)

    print("select col hit")

    selected_col <- input$select_col

    col_data <- df() |>
      select(!!sym(selected_col)) |>
      na.omit() |>
      unlist(use.names = FALSE)

    common_class <- max(class(col_data[[1]]))
    print(common_class)


    if (common_class %in% c("double", "integer", "numeric")) {
      updateSliderInput(
        session = session,
        inputId = "filter_numeric",
        min = min(col_data),
        max = max(col_data),
        value = c(quantile(col_data, 0.25)[[1]], quantile(col_data, 0.75)[[1]])
      )
    } else if (common_class %in% c("character", "factor", "ordered", "logical")) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "filter_categorical",
        choices = unique(df()[[selected_col]])
      )
    }
  })

  output$dynamic_categorical_graphs <- renderUI({
    req(input$select_col)

    fluidRow(
      box(
        title = "Bar Plot",
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("bar_plot")
      )
    )
  })


  output$dynamic_numeric_graphs <- renderUI({
    req(input$select_col)

    fluidRow(
      box(
        title = "Histogram",
        status = "success",
        solidHeader = TRUE,
      ),
      box(
        title = "Bar Plot",
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("bar_plot")
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
