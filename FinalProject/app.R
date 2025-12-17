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
library(plotly)
library(ggplot2)
library(dplyr)
library(bslib)
library(bs4Dash)
library(DT)
library(readr)
library(scales)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)

## Global Variables ----

## ui definition (dashboardPage) ----
ui <- dashboardPage(
  title = "World Happiness bs4Dash Page", # TODO where the hell is this title actually output? browser title?

  ## Header ----
  header = dashboardHeader(
    title = "World Happiness Dashboard",
    titleWidth = 400, # TODO figure out how this relates to sidebar width
    compact = TRUE,
    border = TRUE,
    status = "purple"
  ),

  ## Sidebar ----
  sidebar = dashboardSidebar(
    # TODO figure out how to increase the width of the sidemenu
    collapsed = FALSE, # TODO figure out how to either un-autohide the sidemenu, or adjust it so that it collapses fully, vs expands fully
    status = "primary",
    elevation = 5,

    ### Sidebar Menu ----
    sidebarMenu(
      id = "sidebar",

      menuItem(
        "Settings",
        tabName = "main",
        icon = icon("gear"),
        card(
          fileInput(
            inputId = "file_upload",
            label = "Load Data File",
            multiple = FALSE
          ),
          selectInput(
            inputId = "select_country",
            label = "Choose Country",
            choices = "Select Country"
          ),
          sliderInput(
            inputId = "slider_year",
            min = 0,
            max = 0,
            step = 0,
            value = c(0, 0),
            label = "Year Filter",
            sep = "" # remove comma delimited numbers
          ),
          checkboxInput(
            inputId = "show_linear_regression",
            label = "Show Linear Regression Info"
          )
        )
      ),

      menuItem(
        "Reporting",
        tabName = "report",
        icon = icon("bug"),
        card(
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
          ),
          fluidRow(
            ### World Map ----
            box(
              title = "World Map",

              plotlyOutput(
                outputId = "plot_map",
                width = "100%",
                height = "400px"
              )
            ),

            ### World Ranking ----
            box(
              title = "World Ranking",

              plotlyOutput(
                outputId = "plot_ranking",
                width = "100%",
                height = "400px"
              )
            )
          ),

          ### Additional Plots ----
          fluidRow(
            column(
              6,
              plotlyOutput(
                outputId = "plot_1",
                width = "100%",
                height = "400px"
              )
            ),
            column(
              6,
              plotlyOutput(
                outputId = "plot_2",
                width = "100%",
                height = "400px"
              )
            )
          ),
          fluidRow(
            ### DataTable output ----
            box(
              title = "Data Table",
              width = 12,
              closable = TRUE,
              DTOutput(outputId = "DT_alldata", width = "100%") # TODO figure out how to adjust overflow settings
            )
          )
        )
      ),

      ## Second Tab ----
      tabItem(
        tabName = "report",
        "Any Report settings here. Country and year select mainly. Probably don't need this...."
      )
    )
  ),

  ## Footer ----
  footer = dashboardFooter(
    "STAT331 Data Visualization and Dashboards - Fall '25 - Professor Joseph Reid - Oregon Institute of Technology - Final Project - Andrew Sparkes",
    fixed = TRUE # stick to bottom of page
  )
)

## Server Function Definition ----
server <- function(input, output, session) {
  ## Handle file upload ----
  df <- reactive({
    # Keeping the full unmodified data frame in df
    message("Detected file upload...")

    req(input$file_upload) # required id upload

    ext <- tools::file_ext(input$file_upload$name)
    switch(
      ext,
      csv = suppressMessages(
        # make vroom stfu
        vroom::vroom(
          input$file_upload$datapath,
          delim = ",",
          progress = FALSE,
          show_col_types = FALSE,
          col_types = cols()
        )
      ),
      validate("Invalid file; Please upload a .csv file")
    )
  })

  ## Mangle dataset into a dataframe ----

  happy_data <- reactive({
    # happy_data will represent the filtered/mutated subset that will be used to plot
    message("Cleaning up dataset...")

    ### Mutate the data as needed ----

    dat <- df()
    #filter(year %in% c(2015, 2016)) |> # for now, looking at a subset of the data due to so much incompleteness for years past 2016

    # (ensure country names are in agreement between dataset and world map country names - standardized/normalized to best of ability
    # dat |> # TODO mutate me !
    #   rename()

    # TODO should I do all filter/mutating here, and only here? already have unmodified df for aggregate plots

    # TODO fix the descrepancies. United States =/= United States of America, etc

    #if (input$select_country != NULL) { # this if statement really pisses it off. Complains that input$select_country is of length zero... wtf
    # world_modified <- dat |>
    #   mutate( # this mutate consistently pissed off the interpretter, fuck my life
    #     selected_country = if_else(Country == input$select_country, 1, NA)
    #   )

    # ggplot(data = world_modified) +
    #   geom_sf(aes(fill = selected_country)) +
    #   theme_bw()
    #}
    #
    if (input$select_country != '') {
      dat <- dat |>
        mutate(
          # this mutate really pisses it off
          # selected country gets a "Y" entry for the highlight_country col
          highlight_country = if_else(Country == input$select_country, "Y", "N")
        )
    }
    # if (!is.na(input$slider_year)) {
    #   dat <- dat |>
    #     filter(year >= input$slider_year[1], year <= input$slider_year[2])
    # }

    return(dat)
  })

  ## make reactive variables to fuck with later EDIT apparently bad idea, not needed, makes interpretter complain
  # country <- reactive({
  #   input$select_country
  # })
  # factor <- reactive({
  #   input$select_factor
  # })
  # years <- reactive({
  #   input$slider_year
  # })
  # show_linear_regression <- reactive({
  #   input$show_linear_regression
  # })

  ## Update static elements ----

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
    session = session,
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
      session = session,
      inputId = "select_country",
      choices = sort(unique(happy_data()$Country)), # unique to remove duplicates
      selected = NULL
    )

    message("Updating the slider element with min/max and setting value...")
    updateSliderInput(
      session = session,
      inputId = "slider_year",
      min = min(happy_data()$year, na.rm = T),
      max = max(happy_data()$year, na.rm = T),
      value = c(
        min(happy_data()$year, na.rm = T),
        max(happy_data()$year, na.rm = T)
      ), # default to select full range
      step = 1
    )
  })

  ### Select country onchange ----
  observeEvent(input$select_country, {
    message("Handling onchange event for select_country... DO NOTHING")

    #freezeReactiveValue(input, "select_country")
  })

  ### Select country onchange ----
  observeEvent(
    input$slider_year,
    {
      message("Handling onchange event for slider_year...")

      #freezeReactiveValue(input, "slider_year")

      # filter down # TODO if happy data is reactive, it should automatically update when years changes, right?
    },
    ignoreInit = TRUE
  )

  ### Handling change to select_factor
  observeEvent(input$select_factor, {
    message("Handling onchange event for select_factor...")

    freezeReactiveValue(input, "select_factor")
    #browser()

    # Updating scatter plot ----
    if (input$select_factor != '') {
      output$plot_scatter <- renderPlotly({
        happy_data() |>
          ggplot() +
          geom_point(
            aes(
              x = !!sym(input$select_factor), # TODO Y U NO WORK U FUK
              y = `Happiness Score`
            )
          ) +
          guides()
      })

      # update the box title for the scatter plot as well
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

      # update any additional plots that are based on selected factor
    }

    # update anything else that doesn't depend on the value of select_factor. Should be nothing ...
  })

  # TODO reconsider if I want to filter this based on selected country, selected year
  # TODO sort the datatable by happiness rank
  # TODO determine if this renderDT call needs to be somewhere else to be dynamically updated
  ### Update DataTable ----
  output$DT_alldata <- renderDT({
    message("Updating DataTable...")

    # pass through the whole dataframe (only filtered by year, basic mutation of country names), but ordered by happiness rank
    happy_data() # |>
    # mutate(Country = fct_reorder(Country, `Happiness Rank`)) # this should sort the df by happiness rank ASC, # This mutate chokes the interpretter
  })

  ### Update world map plot ----
  output$plot_map <- renderPlotly({
    message("Updating World Map Plot...")

    world <- ne_countries(scale = "medium", returnclass = "sf") # static/const vector of country shape data, to be later joined with country happiness data

    # dataset_countries <- sort(unique(dat$Country))
    # world_data_countries <- sort(unique(world$name))

    # TODO something in here is throwing a "Warning: Error in <Anonymous>: number of columns of matrices must match (see arg 15)"
    #browser()
    # dat <- left_join(happy_data(), world, by = c("Country" = "name")) # TODO verify the join by condition (col names match) THIS LOC IS FROM SATAN HIMSELF FUCK THIS CODE
    # # the above join should take each record in happy_data(), match it's Country to the name in world, and join the shape/spatial data, then store back in dat to use later
    #
    # # dat is the filtered happy_data, with world shape data joined on
    # dat |>
    #   ggplot() + # TODO issues with below geom_sf, complaining about geometry, tried passing geometry=geometry and it hated that too
    #   geom_sf(aes(fill = `Happiness Score`, geometry = geometry)) + #, alpha = highlight_country)) + # at this point, happy data should have the highlight_country col created
    #   scale_fill_gradient(low = "white", high = "blue") +
    #   guides(alpha = "none")
  })

  ### Update World Ranking plot ----
  output$plot_ranking <- renderPlotly({
    # don't try to render plotly if country hasn't been selected yet
    if (input$select_country != '') {
      # using country() pisses it off, but using input$select_country ALSO pisses it off, fuck me
      #req(input$select_country)

      happy_data() |>

        # TODO add a limit 35 or so, or paginate, but somehow also show the selected country? idfk
        # possible idea: sort list by happiness rank, find idx of selected country, select where id >= idx-15 && id <= idx+15 or whatever, so it has the surrounding countries AND the selected one
        ggplot(
          aes(
            x = `Happiness Rank`,
            y = Country
            #fill = highlight_country # TODO Fix this ASAP, bullshit
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
    }
  })

  ### Report button handling ----
  observeEvent(input$btn_report, {
    message("Report generating...")

    # handling the report button press
    rmarkdown::render(
      input = "AutomatedReport.Rmd",
      output_file = paste0(input$select_country, ".html"), # TODO figure out where this file is actually rendered to. Assuming tmp folder, but there is no mention of a directory?
      params = list(country = input$select_country) # does this params list need the remainder of the params like linear_regression? TODO figure this out. Seems like a redundant call to render?
    )
  })

  ### Download handler ----
  output$btn_report <- downloadHandler(
    filename = function() {
      # example filename: United States_2025-12-12.html # TODO look into replacing spaces with underscores in filename
      paste0(input$select_country, "_", Sys.Date(), ".html") # TODO note difference between this filename and the above filename in the input$btn_report render call. hmmmm.
    },

    content = function(file) {
      message("Generating automated report...")

      # copy the local Rmd report to a temp file
      tempReport <- file.path(tempdir(), "AutomatedReport.Rmd") # makes an abs path to /tmp/AutomatedReport.Rmd (file does not yet exist)
      file.copy("AutomatedReport.Rmd", tempReport, overwrite = TRUE) # copy the Report.Rmd to the temp location

      params <- list(
        # prepare the report parameters
        country = input$select_country,
        linear_regression = input$show_linear_regression,
        year_start = input$slider_year[1],
        year_end = input$slider_year[2],
        dat = happy_data(), # already filtered by year, and should have highlight_country set
        fulldat = df() # also pass along the full unmodified df in case we want to compare specific country to all countries
      )

      # barf out the rendered file
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params, # TODO Warning: Error in knit_params_get: render params not declared in YAML: cases, filter TODO WTF Tried putting them in yaml of the report.Rmd
        envir = new.env(parent = globalenv()) # TODO google this line and learn what it does. environment variables copy to scope of report?
      )
    }
  )

  ### help dialog toggle ----
  observe({
    if (input$help_switch == TRUE) {
      if (input$sidebar == "main") {
        shiny::showModal(
          ui = shiny::modalDialog(
            title = "Help",
            "Help Contents Here" # TODO Doc me
          )
        )
      } else if (input$sidebar == "report") {
        shiny::showModal(
          ui = shiny::modalDialog(
            title = "Help: Reports",
            "Help info for report settings" # TODO Doc me
          )
        )
      }
    }
  })
}


options(shiny.reactlog = TRUE) # this seems to do jack all
### Shiny App Function call ----
shinyApp(
  ui,
  server
  #options = list(display.mode = "showcase")
)
