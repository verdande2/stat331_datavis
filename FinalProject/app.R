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
library(bslib)
library(bs4Dash)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)

#world <- ne_countries(scale = "medium", returnclass = "sf")

# Load dataset

ui <- dashboardPage(
  title = "World Happiness bs4Dash Page",
  fullscreen = TRUE,
  header = dashboardHeader(
    title = "World Happiness Ranking Dashboard"
  ),
  sidebar = dashboardSidebar(
    collapsed = FALSE,
    status = "primary",
    elevation = 5,
    sidebarUserPanel(
      name = "General Config",
      box()
    ),
    sidebarMenu(
      id = "sidebar",
      sidebarHeader("Main Config"),
      menuItem(
        "Settings",
        tabName = "main",
        icon = icon("home"),
        card(
          fileInput(
            inputId = "upload",
            label = "Select Data File",
            multiple = FALSE
          )
        )
      ),
      menuItem(
        "Reporting",
        tabName = "report",
        card(
          selectInput(
            inputId = "myCountry",
            label = "Choose Country",
            choices = ""
          ),
          downloadButton(
            outputId = "report",
            label = "Generate Report"
          )
        )
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "main",
        fluidRow(
          column(
            12,
            box(
              title = "Scatter Plot",
              plotlyOutput(
                outputId = "plot_scatter",
                width = "100%",
                height = "400px"
              )
            )
          )
        ),
        fluidRow(
          column(
            6,
          box(
            title = "Map",
            plotlyOutput(
              outputId = "plot_map",
              width = "100%",
              height = "400px"
            )
          )
          ),
          column(
            6,
          box(
            title = "Ranking",
            plotlyOutput(
              outputId = "plot_ranking",
              width = "100%",
              height = "400px"
            )
          )
          )
          )
        ),
        fluidRow(
          column(6,
                 plotlyOutput(
            outputId = "scatterLife",
            width = "100%",
            height = "400px"
          )
          ),
          column(6,
          plotlyOutput(
            outputId = "scatterFreedom",
            width = "100%",
            height = "400px"
          )
          )
        ),
        fluidRow(
          column(12,
                 box(
            title = "Data Table",
            DTOutput(outputId = "DT_alldata", width = "100%") # TODO figure out how to adjust overflow settings
          )
        )
        )
      )
    )
  ),
  footer = dashboardFooter(
    "STAT331 Data Visualization and Dashboards - Final Project - Andrew Sparkes"
  )
)


server <- function(input, output, session) {
  # Read Data from File based on user choice ----
  df <- reactive({
    req(input$upload) # required id upload

    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  ## Update data ----
  happy_data <- reactive({
    data <- df()

    data <- left_join(world, data, by = c("full" = "state.name")) |>
      rename(state.name = full)

    return(data)
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
