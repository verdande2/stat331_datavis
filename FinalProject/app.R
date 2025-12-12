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

world <- ne_countries(scale = "medium", returnclass = "sf")

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
              status = "success",
              solidHeader = TRUE,
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
              status = "success",
              solidHeader = TRUE,
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
              status = "success",
              solidHeader = TRUE,
              plotlyOutput(
                outputId = "plot_ranking",
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
              title = "Scatter 1",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput(
                outputId = "scatterLife",
                width = "100%",
                height = "400px"
              )
            )
          ),
          column(
            6,
            box(
              title = "Scatter 2",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput(
                outputId = "scatterFreedom",
                width = "100%",
                height = "400px"
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            box(
              title = "Data Table",
              status = "success",
              solidHeader = TRUE,
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

  ## Updated data ----
  happy_data <- reactive({
    # perform any relevant filtering, mutations, etc here
    data <- df()

    # joining our happy_data with our world data
    data <- left_join(world, data, by = c("full" = "state.name")) |> # TODO update me for country/country code
      rename(state.name = full)

    return(data)
  })

  # update the main scatter plot

  # update the DT
  output$DT_alldata <- renderDT({
    happy_data()
  })

  # update the world map plot # TODO update this to work with rnatural earth world map data, and get the join working above
  output$plot_worldmap <- renderPlotly({
    happy_data() %>%
      mutate(
        highlight_country = if_else(country.name == input$myCountry, "Y", "N")
      ) %>%
      mutate(country.name = fct_reorder(country.name, cases_per_100000)) %>%
      ggplot() +
      geom_sf(aes(fill = cases_per_100000, alpha = highlight_country)) +
      scale_fill_gradient(low = "white", high = "blue") +
      guides(alpha = "none")
  })

  # # TODO determine what exactly this code chunk does...  ----
  observeEvent(input$upload, {
    updateSelectInput(inputId = "rankInputSelect", choices = rank_vec()) # rank vec was defined in the other app.R file
    updateSelectInput(
      inputId = "myState",
      choices = unique(COVID_data()$state.name)
    )
  })

  # Generate Report Button Event ----
  # handling the generate report button press event
  observeEvent(input$report, {
    rmarkdown::render(
      input = "AutomatedReport.Rmd",
      output_file = paste0(input$myCountry, ".html"),
      params = list(country = input$myCountry)
    )
  })

  # handling the actual output of the file download
  output$report <- downloadHandler(
    filename = function() {
      paste0(input$myCountry, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "AutomatedReport.Rmd")
      file.copy("AutomatedReport.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        state = input$myCountry,
        data = happy_data(),
        filter = input$myCountry
      )
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
