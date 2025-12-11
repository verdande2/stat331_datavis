# Your final project for this course is to develop a fully implemented dashboard based on a dataset and topic of your choosing.
# This should show off your skills and be appropriate to put in a portfolio.  However, it must meet the following requirements.
#
# 1.)  It must include at least a few graphics (3 or more) of different types
# a.)  The graphs must adhere to best practices and have consistent theming.
# b.)  The graphs must be dynamic to inputs from the user.
#
# 2.)  It must have at least three different kinds of inputs (numerics, checkbox, etc...)
#
# 3.)  It must tell a story to the user.  Geographic, time series, data summary, something interesting and useful where the user can answer a question of interest.  This is what you will demo for the final presentation.
#
# 4.)  It must be able to produce a dynamic report. (i.e. the push of a button will generate a report
#                                                    for the user which is dynamic based on parameter(s) from at least one of the inputs.)
#
# You will submit the .R file for your app (named app.R) and the data file used to make it work.  Put these together in a zip folder
# and submit that.  If your file has a more complicated structure, zip all of the elements that will make it work into one file and submit that.

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)

# Load dataset

ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Ranking Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Settings",
        tabName = "main"
      ),
      menuItem(
        "Another tab",
        tabName = "second"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "main",
        fluidRow(
          fluidRow(
            plotlyOutput(
              outputId = "plotScatter",
              width = "100%",
              height = "400px"
            )
          ),
          fluidRow(
            plotlyOutput(
              outputId = "scatterGDP",
              width = "100%",
              height = "400px"
            ),
            plotlyOutput(
              outputId = "scatterCorruption",
              width = "100%",
              height = "400px"
            )
          ),
          fluidRow(
            plotlyOutput(
              outputId = "scatterLife",
              width = "100%",
              height = "400px"
            ),
            plotlyOutput(
              outputId = "scatterFreedom",
              width = "100%",
              height = "400px"
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Dynamically load and filter dataset
  df <- reactive({
    req(input$select_dataset)

    switch(input$select_dataset,
      "diamonds" = diamonds,
      "mtcars" = mtcars,
      "iris" = iris
    )
  })

  # Dynamically fill selectInput when dataset changes
  observeEvent(input$select_dataset, {
    req(df())
    freezeReactiveValue(input, "select_col")
    updateSelectInput(
      session,
      "select_col",
      choices = names(df()),
      selected = NULL
    )
  })

  # Identify and return column type information
  output$column_type <- reactive({
    req(input$select_col)

    selected_col <- input$select_col

    selected_col <- noquote(input$select_col)
    col_data <- df() %>%
      select(!!sym(selected_col)) %>%
      na.omit()
    common_class <- max(class(col_data[[1]]))

    # Class classification
    if (common_class %in% c("double", "integer", "numeric")) {
      # print("numeric")
      return("numeric")
    } else if (common_class %in% c("character", "factor", "ordered")) {
      # print("categorical")
      return("categorical")
    } else {
      # print("special")
      return("special")
    }
  })

  outputOptions(output, "column_type", suspendWhenHidden = FALSE)

  observeEvent(c(input$select_col), {
    req(input$select_col)

    selected_col <- input$select_col

    # Get column type
    col_type <- df()[[selected_col]]

    # Extract data
    data <- df() %>%
      select(!!sym(selected_col)) %>%
      unlist(use.names = FALSE)

    if (length(data) == 0) {
      return()
    }

    # Update based on column type
    if (is.numeric(col_type)) {
      updateSliderInput(
        session,
        inputId = "filter_numeric",
        min = min(data),
        max = max(data),
        value = c(quantile(data, 0.25)[[1]], quantile(data, 0.75)[[1]])
      )
    } else {
      updateCheckboxGroupInput(
        session,
        inputId = "filter_categorical",
        choices = unique(df()[[selected_col]]),
        selected = unique(df()[[selected_col]])
      )
    }
  })

  output$dynamic_categorical_graphs <- renderUI({
    req(input$select_col)

    fluidRow(
      column(
        6,
        box(
          title = "Bar Plot",
          status = "success",
          solidHeader = TRUE,
          plotlyOutput("bar_plot")
        )
      )
    )
  })
  output$dynamic_numeric_graphs <- renderUI({
    req(input$select_col)

    fluidRow(
      column(
        6,
        box(
          title = "Distribution",
          status = "success",
          solidHeader = TRUE,
          plotlyOutput("histogram")
        )
      ),
      column(
        6,
        box(
          title = "Box-plot",
          status = "success",
          solidHeader = TRUE,
          plotlyOutput("box_plot")
        )
      )
    )
  })

  output$box_plot <- renderPlotly({
    req(input$select_col)

    plt <- df() %>%
      select(!!sym(input$select_col)) %>%
      filter(
        !!sym(input$select_col) >= input$filter_numeric[1] &
          !!sym(input$select_col) <= input$filter_numeric[2]
      ) %>%
      ggplot(aes(y = !!sym(input$select_col), )) +
      geom_boxplot()

    ggplotly(plt)
  })

  output$histogram <- renderPlotly({
    req(input$select_col)

    plt <- df() %>%
      select(!!sym(input$select_col)) %>%
      filter(
        !!sym(input$select_col) >= input$filter_numeric[1] &
          !!sym(input$select_col) <= input$filter_numeric[2]
      ) %>%
      ggplot(aes(x = !!sym(input$select_col))) +
      geom_histogram()

    ggplotly(plt)
  })

  output$bar_plot <- renderPlotly({
    req(input$select_col)

    plt <- df() %>%
      select(!!sym(input$select_col)) %>%
      filter(!!sym(input$select_col) %in% input$filter_categorical) %>%
      ggplot(aes(x = !!sym(input$select_col))) +
      geom_bar()

    ggplotly(plt)
  })
}

shinyApp(ui, server)
