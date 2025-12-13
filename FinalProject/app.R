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

## imports ----
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(bslib)
library(bs4Dash)
library(DT)
library(rworldmap) # TODO figure this world map shit out... urgh
library(rnaturalearth)
library(rnaturalearthdata)

## Global Variables ----
world <- ne_countries(scale = "medium", returnclass = "sf") # static/const vector of country shape data

## ui definition (dashboardPage) ----
ui <- dashboardPage(
  title = "World Happiness bs4Dash Page", # TODO where the hell is this title actually output?
  fullscreen = TRUE, # TODO why does this not work?!?!

  ## Header ----
  header = dashboardHeader(
    title = "World Happiness Dashboard",
    titleWidth = 400,
    compact = TRUE,
    border = TRUE,
    status = "purple"
  ),

  ## Sidebar ----
  sidebar = dashboardSidebar(
    # TODO figure out how to increase the width of the sidemenu
    # Custom CSS Tweaks TODO this custom CSS does not in fact help any of the issues I was hoping it would. Fuck.
    tags$head(
      tags$style(HTML(
        "
        /* Ensure the dropdown menu appears above other elements */
        .sidebar .bs4-dropdown-menu {
          position: absolute !important;
          border: 10px red solid !important;
          z-index: 1060 !important; /* Higher than typical content z-index */
        }
        /* If using shiny inputs like pickerInput or selectInput, target their dropdowns */
        .selectize-dropdown {
          z-index: 1060 !important;
        }
        .picker-menu {
          z-index: 1060 !important;
        }
        /* Target the main sidebar and its inner viewport containers */
        .main-sidebar,
        .main-sidebar .sidebar .os-viewport,
        .os-host-overflow,
        .os-host-overflow > .os-padding {
          overflow: visible !important;
        }
        /* Ensure the dropdown menu itself displays correctly */
        .dropdown-menu.show {
          display: block; /* Use 'block' or 'contents' depending on exact BS version */
        }
      "
      ))
    ),
    collapsed = FALSE, # TODO figure out how to either un-autohide the sidemenu, or adjust it so that it collapses fully, vs expands fully
    status = "primary",
    elevation = 5,
    sidebarUserPanel(
      name = "General Config" # TODO why does this show up as a link?
    ),

    ### Sidebar Menu ----
    sidebarMenu(
      id = "sidebar",

      menuItem(
        "Settings",
        tabName = "main",
        icon = icon("gear"),
        column(
          12, # TODO figure out wtf I'm doing wrong with the col widths. 12 should be full width.... I must be missing something, or I'm retarded. Who knows.
          card(
            fileInput(
              inputId = "file_upload",
              label = "Load Data File",
              multiple = FALSE
            )
          ),
          card(
            selectInput(
              inputId = "select_country",
              label = "Choose Country",
              choices = ""
            )
          )
        )
      ),

      menuItem(
        "Reporting",
        tabName = "report",
        icon = icon("bug"),
        card(
          checkboxInput(
            "show_linear_regression",
            label = "Show Linear Regression Info"
          ),
          downloadButton(
            outputId = "btn_report",
            label = "Generate Report"
          )
        )
      )
    )
  ),

  ## Body ----
  body = dashboardBody(
    tabItems(
      ### Main Tab ----
      tabItem(
        tabName = "main",
        conditionalPanel(
          condition = "input.select_country != ''",

          fluidRow(
            column(
              12,

              ### Scatter Plot ----
              box(
                id = "box_plot_scatter",
                title = "Scatter Plot",
                selectInput(
                  inputId = "select_factor",
                  label = "Selected Factor:",
                  multiple = FALSE,
                  choices = ""
                ),

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

              ### World Map ----
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

              ### World Ranking ----
              box(
                title = "World Ranking",
                plotlyOutput(
                  outputId = "plot_ranking",
                  width = "100%",
                  height = "400px"
                )
              )
            )
          ),

          ### Additional Plots ----
          # fluidRow(
          #   column(
          #     6,
          #     plotlyOutput(
          #       outputId = "scatterLife",
          #       width = "100%",
          #       height = "400px"
          #     )
          #   ),
          #   column(
          #     6,
          #     plotlyOutput(
          #       outputId = "scatterFreedom",
          #       width = "100%",
          #       height = "400px"
          #     )
          #   )
          # ),
          fluidRow(
            column(
              12,

              ### DataTable output ----
              box(
                title = "Data Table",
                width = 12,
                DTOutput(outputId = "DT_alldata", width = "100%") # TODO figure out how to adjust overflow settings
              )
            )
          )
        )
      ),

      ## Second Tab ----
      tabItem(
        tabName = "report",
        "test"
      )
    )
  ),

  ## Footer ----
  footer = dashboardFooter(
    "STAT331 Data Visualization and Dashboards - Fall '25 - Professor Joseph Reid - Oregon Institute of Technology - Final Project - Andrew Sparkes",
    fixed = TRUE
  )
)

## Server Function Definition ----
server <- function(input, output, session) {
  ### help dialog toggle ----
  observe({
    if (input$help_switch == TRUE) {
      if (input$sidebar == "main") {
        shiny::showModal(
          ui = shiny::modalDialog(
            title = "Help",
            "Help Contents Here"
          )
        )
      } else if (input$sidebar == "report") {
        shiny::showModal(
          ui = shiny::modalDialog(
            title = "Help: Reports",
            "Help info for report settings"
          )
        )
      }
    }
  })

  ## Handle file upload ----
  df <- reactive({
    message("Detected file upload...")
    req(input$file_upload) # required id upload

    ext <- tools::file_ext(input$file_upload$name)
    switch(
      ext,
      csv = vroom::vroom(
        input$file_upload$datapath,
        delim = ",",
        progress = FALSE,
        show_col_types = FALSE,
        col_types = cols()
      ),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  ## Mangle dataset into a dataframe ----
  happy_data <- reactive({
    message("Cleaning up dataset...")
    dat <- df() # TODO get this selected country part working
    # |>
    #   mutate( # selected country gets a "Y" entry for the highlight_country col
    #     highlight_country = if_else(Country == input$select_country, "Y", "N")
    #   )

    # TODO left_join world data with the dataset
    #data <- left_join(world, data, by = c("full" = "state.name")) # world is not defined atm

    # Mutate the data as needed (ensure country names are in agreement between dataset and world map country names ----
    # data |> # TODO mutate me !
    #   rename(state.name = full)

    return(dat)
  })

  ## Update static elements ----

  # no dynamic change for the Data Table, should be all records, barfed out as is
  # TODO reconsider if I want to filter this based on selected country TBA
  # TODO sort the datatable by happiness rank
  ### Update DataTable ----
  output$DT_alldata <- renderDT({
    message("Updating DataTable...")
    happy_data()
  })

  # get a vector of the available factors
  v <- c(
    "Economy (GDP per Capita)",
    "Family",
    "Health (Life Expectancy)",
    "Freedom",
    "Trust (Government Corruption)",
    "Generosity",
    "Dystopia Residual"
  )

  ### Populate the select_factor select with available factors ----
  message("Updating the choices in the select_factor dropdown...")
  updateSelectInput(
    inputId = "select_factor",
    choices = v
  )

  ## Update dynamic elements  ----

  ### Upload input onchange ----
  observeEvent(input$file_upload, {
    message("File Upload initiated...")

    # update the country selector with the choices from the dataset
    message("Updating the select_country element with the list of countries...")
    updateSelectInput(
      inputId = "select_country",
      choices = unique(happy_data()$Country) # unique to remove duplicates
    )
  })

  ### Select country onchange ----
  observeEvent(input$select_country, {
    message("select_country Triggered...")

    # TODO add a way to show the selected country in each plot
    # ie. a is_selected flag with conditional color change or something
    # could also do a mutate(is_selected = ...) kinda thing

    # Determine the selected country and add a bool col that represents that
    # this is the proper spot to do this mutate for selected country, as it is triggered when a new country is selected
    dat <- happy_data()
    # |>
    #   mutate(
    #     highlight_country = if_else(Country == input$select_country, "Y", "N")
    #   ) # TODO figure this shit out with selected country
  })

  ### Handling change to select_factor
  observeEvent(input$select_factor, {
    message("Handling onchange event for select_factor...")

    # Updating scatter plot ----
    output$plot_scatter <- renderPlotly({
      happy_data() |>
        #select(c(input$select_factor)) |> # TODO fix me! we use the chosen factor to filter out the extraneous data
        ggplot() +
        geom_point(
          aes(
            x = `Country`,
            y = `Happiness Score`
          )
        ) +
        guides()
    })

    # update the box title for the scatter plot as well
    if (input$select_factor != "") {
      # if something is actually selected
      updateBox(
        id = "box_plot_scatter",
        action = "update", # TODO figure out wtf is wrong with this. it updates the box title the first time, but then fails to update
        options = list(
          title = paste(
            "Scatter Plot of ",
            input$select_factor,
            " and the resulting Happiness Score"
          ) # TODO make this actually show a proper title
        )
      )
    }

    # update any additional plots that are based on selected factor
  })

  ### Update world map plot ----
  # TODO update this after I can successfully join the world map data with the dataset
  # output$plot_map <- renderPlotly({
  #   print("Updating Map Plot")
  #   happy_data() |>
  #     #mutate(Country = fct_reorder(Country, cases_per_100000)) |> # TODO what does this line do
  #     ggplot() +
  #     geom_sf(aes(fill = `Happiness Score`)) + #, alpha = highlight_country)) +
  #     scale_fill_gradient(low = "white", high = "blue") +
  #     guides(alpha = "none")
  # })

  ### Update World Ranking plot
  output$plot_ranking <- renderPlotly({
    selection <- input$select_country
    happy_data() |>
      mutate(
        highlight_country = if_else(Country == input$select_country, "Y", "N")
      ) |>
      # mutate(
      #   Country = fct_reorder(Country, `Happiness Rank`) # this appears to reorder factors based on the sort of country, then happiness rank? Google this
      # ) |>
      #
      # TODO add a limit 35 or so, or paginate
      ggplot(
        aes(
          x = `Happiness Rank`,
          y = Country,
          fill = highlight_country
        )
      ) +
      geom_col() +
      scale_x_continuous(
        labels = comma_format()
      ) +
      theme(
        legend.position = "none"
      ) +
      labs(
        y = NULL,
        x = "Happiness Rank"
      )
  })

  ### Report button handling ----
  observeEvent(input$btn_report, {
    message("Report generating...")

    # handling the report button press
    rmarkdown::render(
      input = "AutomatedReport.Rmd",
      output_file = paste0(input$select_country, ".html"),
      params = list(Country = input$select_country)
    )
  })

  ### Download handler ----
  output$btn_report <- downloadHandler(
    # example filename: United States_2025-12-12.html # TODO look into replacing spaces with underscores in filename
    filename = function() {
      paste0(input$select_country, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      message("Generating automated report...")

      # copy the local Rmd report to a temp file
      tempReport <- file.path(tempdir(), "AutomatedReport.Rmd")
      file.copy("AutomatedReport.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        country = input$select_country,
        dat = happy_data() # TODO determine if I should filter this down before sending to report rmd, answer: duh, dumb shit, do it
      )

      # barf out the rendered file
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params, # TODO Warning: Error in knit_params_get: render params not declared in YAML: cases, filter TODO WTF Tried putting them in yaml of the report.Rmd
        envir = new.env(parent = globalenv())
      )
    }
  )
}


### Shiny App Function call ----
shinyApp(ui, server)
