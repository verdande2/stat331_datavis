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
default_data_path <- "world_happiness_report.csv"


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
            value = 0,
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
        fluidRow(
          ### Scatter Plot ----
          box(
            id = "box_plot_scatter",
            title = "Scatter Plot",

            selectInput(
              inputId = "select_factor",
              label = "Selected Factor:",
              multiple = FALSE,
              choices = NULL
            ),

            plotlyOutput(
              outputId = "plot_scatter",
              width = "800px",
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
  # a function to encapsulate all the updates to the ui that need to be done once data is loaded
  # Note this function depends on happy_data() being defined, so has to be called /after/ that
  update_all_dynamic_elements <- function(session) {
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
      value = min(happy_data()$year, na.rm = T), # default to min year
      step = 1
    )

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

    v <- sort(v) # alphabetize just for presentation's sake

    ### Populate the select_factor select with available factors ----
    message(
      "Updating the choices in the select_factor dropdown for main scatter plot..."
    )
    updateSelectInput(
      session = session,
      inputId = "select_factor",
      choices = v
    )
  }

  ## Handle file upload/default dataset load ----
  df <- reactive({
    # Keeping the full unmodified data frame in df

    # if a file upload is detected...
    if (!is.null(input$file_upload)) {
      message("Detected file upload...")

      # switch based on extension, TODO could tidy this up and reduce duplicate code, on the todo list, as I'm only supporting csv officially
      ext <- tools::file_ext(input$file_upload$name)
      switch(
        ext,
        csv = suppressMessages(
          # make vroom stfu
          vroom::vroom(
            input$file_upload$datapath,
            delim = ",", # csv file, duh
            progress = FALSE, # stfu
            show_col_types = FALSE, # stfu
            col_types = cols() # stfu
          )
        ),
        validate("Invalid file; Please upload a .csv file")
      )
    } else {
      # no file upload detected, load default dataset
      message(paste0("Loading default dataset: ", default_data_path, "..."))

      suppressMessages(
        # make vroom stfu
        vroom::vroom(
          file = default_data_path,
          delim = ",", # csv file, duh
          progress = FALSE, # stfu
          show_col_types = FALSE, # stfu
          col_types = cols() # stfu
        )
      )

      # now, after loading into df, still need to call update_all_dynamic_elements() somewhere, but it depends on happy_data(), so it needs to be called /after/ happy_data() def, right?
    }

    # update elements after dataset loaded, that makes sense, right? NOOOOOOOOOOOOOOOOOOOO infinite recursive death spiral!
    # update_all_dynamic_elements(session = session)
  })

  ## make reactive variables out of our filters in the sidebar, as they will be used to update other reactive elements
  country <- reactive({
    input$select_country
  })
  factor <- reactive({
    input$select_factor
  })
  year <- reactive({
    input$slider_year
  })
  show_linear_regression <- reactive({
    input$show_linear_regression
  })

  ## Mangle dataset into a dataframe ----

  happy_data <- reactive({
    # happy_data will represent the filtered/mutated subset that will be used to plot

    # do I need these reqs? no, default for country is handled as an unintentional side effect, and I check if years()[] have been set below, so not needed
    #req(years())
    #req(country())
    # don't need linear regression checkbox technically, it's state just determines if we plot a geom_smooth/linregression etc
    message("Making happy_data() more happy...")

    ### Mutate the data as needed ----

    #filter(year %in% c(2015, 2016)) |> # for now, looking at a subset of the data due to so much incompleteness for years past 2016

    # (ensure country names are in agreement between dataset and world map country names - standardized/normalized to best of ability
    # dat |> # TODO mutate me !
    #   rename()
    # TODO fix the discrepancies. United States =/= United States of America, etc

    # starting with the full unmodified dataframe, df
    dat <- df() |>
      mutate(
        # add a col representing the selected country to highlight
        selected_country = if_else(Country == country(), "Y", "N") # this dngaf if country() is "Select Country" (the default), so no big deal
      )

    # only if the year slider has been changed, filter the dat further by the year slider criteria
    if (year() != 0) {
      dat <- dat |>
        filter(
          # filter down to the selected year
          df()$year == year()
        )
    }

    #browser()
    return(dat)
  })

  ## Update static elements ----
  # are there even any static elements that need updating? I'm going crazy...

  ## Update dynamic elements  ----

  ### file upload onchange ----
  observeEvent(input$file_upload, {
    message("File Upload initiated...")

    # after a file upload has been initiated, update elements. makes sense.
    update_all_dynamic_elements(session = session)
  })

  ### Select country onchange ----
  observeEvent(input$select_country, {
    message("Handling onchange event for select_country... DO NOTHING")

    #freezeReactiveValue(input, "select_country")
  })

  ### Slider year onchange ----
  observeEvent(
    input$slider_year,
    {
      message("Handling onchange event for slider_year... DO NOTHING")

      #freezeReactiveValue(input, "slider_year")
    }
  )

  ### select factor onchange
  observeEvent(input$select_factor, {
    message("Handling onchange event for select_factor...")

    freezeReactiveValue(input, "select_factor")
    #browser()

    # Updating scatter plot ----
    output$plot_scatter <- renderPlotly({
      happy_data() |>
        ggplot() +
        geom_point(
          aes(
            x = Country, #!!sym(factor()), # TODO Y U NO WORK U FUK
            y = `Happiness Score`
          )
        ) +
        guides()
    })

    # update the box title for the scatter plot as well
    updateBox(
      session = session,
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

    # update anything else that doesn't depend on the value of select_factor. Should be nothing ...
  })

  # TODO reconsider if I want to filter this based on selected country, selected year
  # TODO sort the datatable by happiness rank
  # TODO determine if this renderDT call needs to be somewhere else to be dynamically updated
  ### Update DataTable ----
  output$DT_alldata <- renderDT({
    message("Updating DataTable...")

    # pass through all countries, with the selected_country col, and order by happiness score DESC

    # This doesn't fucking work...
    # happy_data() |>
    #   mutate(
    #     Country = forcats::fct_reorder(
    #       Country,
    #       `Happiness Score`,
    #       .desc = TRUE,
    #       .na_rm = TRUE
    #     )
    #   ) # this should sort the df by happiness score

    # Nor this ...
    # ordered_happy_data <- happy_data()[order(happy_data()$`Happiness Score`), ]

    # ... nor this one ...
    # TODO WTF IS WRONG WITH YOU DEVIL CODE
    # ordered_happy_data <- happy_data() |>
    #   mutate(
    #     Country = forcats::fct_reorder(
    #       happy_data()$Country,
    #       happy_data()$`Happiness Score`,
    #       .desc = TRUE,
    #       .na_rm = TRUE
    #     )
    #   )

    # return(ordered_happy_data)
  })

  ### Update world map plot ----
  output$plot_map <- renderPlotly({
    message("Updating World Map Plot...")

    #world <- ne_countries(scale = "medium", returnclass = "sf") # static/const vector of country shape data, to be later joined with country happiness data

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
    req(country())

    happy_data() |>

      # TODO add a limit 35 or so, or paginate, but somehow also show the selected country? idfk
      # possible idea: sort list by happiness rank, find idx of selected country, select where id >= idx-15 && id <= idx+15 or whatever, so it has the surrounding countries AND the selected one (use min/max to prevent out of range indexes)
      ggplot(
        aes(
          x = `Happiness Score`,
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
        x = "Happiness Score"
      )
  })

  ### Report button handling ----
  # Wondering if I need this entire observeEvent at all... Technically there is no input$btn_report ... the download button is an OUTPUT id of btn_report...
  observeEvent(input$btn_report, {
    # how does this get called exactly?
    message("Report generating...") # I don't think I've ever seen this LOC run... hmmmmm....

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
        country = country(),
        show_linear_regression = show_linear_regression(),
        year = year(),
        happy_data = happy_data(), # already filtered by year, and should have highlight_country set
        # TODO should I pass world data, happy+world data, or just let the Rmd report figure it out?
        fulldata = df() # also pass along the full unmodified df in case we want to compare specific country to all countries
      )

      # barf out the rendered file
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params, # TODO verify YAML in report.Rmd file
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
