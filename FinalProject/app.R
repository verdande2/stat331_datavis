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
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(bslib)
library(bs4Dash)
library(DT)
library(readr)
library(scales)
library(waiter)
library(htmltools)
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
    titleWidth = "200px", # TODO figure out how this relates to sidebar width
    compact = TRUE,
    border = TRUE,
    status = "purple",
    fixed = TRUE,
    rightUi = tagList(
      dropdownMenu(
        icon = icon("tasks"),
        type = "tasks",
        badgeStatus = "primary",
        taskItem(
          # TODO see https://shiny.posit.co/r/components/display-messages/progress-bar/ for more info on how to update the progress on this taskItem, and this SO answer for a weird file read/write workaround ... https://stackoverflow.com/a/79118567
          # for progress bar async, try https://www.rdocumentation.org/packages/ipc/versions/0.1.4/topics/AsyncProgress
          inputId = "report_progress",
          value = 50,
          color = "primary",
          "Report Generating..."
        )
      )
    ),
    leftUi = tagList(
      dropdownBlock(
        id = "debug_dropdown",
        title = "Debug",
        icon = icon("bug"),
        actionButton(
          inputId = "browser",
          label = "Open Debug Browser() Now!"
        )
        # prettyToggle( # TODO may cannabalize this for later use
        #   inputId = "na",
        #   label_on = "NAs kept",
        #   label_off = "NAs removed",
        #   icon_on = icon("check"),
        #   icon_off = icon("trash-can")
        # )
      )
    )

    #tags$script("$('#browser').hide();") # default to hide the browser button, pffffft, gimme dev mode
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
            choices = NULL
          ),
          sliderInput(
            inputId = "slider_year",
            min = 0,
            max = 0,
            step = 0,
            value = 0,
            label = "Year Filter",
            sep = "", # remove comma delimited numbers
            ticks = TRUE
          ),

          card_body(
            "Note: Data is incomplete for years beyond 2015 and 2016. Proceed at your own risk."
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
    autoWaiter(), # hopefully this is a good spot for the autowaiter call? Seems so
    tabItems(
      ### Main Tab ----
      tabItem(
        tabName = "main",
        fluidRow(
          ### Scatter Plot ----
          box(
            width = 12,
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

        ### Additional Plots ---- Aren't being used rn
        # fluidRow(
        #   column(
        #     6,
        #     plotlyOutput(
        #       outputId = "plot_1",
        #       width = "100%",
        #       height = "400px"
        #     )
        #   ),
        #   column(
        #     6,
        #     plotlyOutput(
        #       outputId = "plot_2",
        #       width = "100%",
        #       height = "400px"
        #     )
        #   )
        # ),

        fluidRow(
          ### DataTable output ----
          box(
            title = "Data Table of filtered subset of dataset",
            width = 12,
            collapsible = TRUE,
            DTOutput(
              outputId = "DT_filtered",
              width = "100%" # TODO figure out how to adjust overflow settings
            )
          )
        )
      ),

      ## Second Tab ----
      tabItem(
        tabName = "report",
        "Any Report settings here. Country and year select mainly. Probably don't need this.... Not really using the tabpanel link to tabname feature in the sidebar, using them more like accordions, eh...."
      )
    )
  ),

  ## Footer ----
  footer = dashboardFooter(
    right = "STAT331 Data Visualization and Dashboards - Fall '25 - Professor Joseph Reid - Oregon Institute of Technology - Final Project - Andrew Sparkes",
    left = textOutput(outputId = "status", inline = TRUE),
    fixed = TRUE # stick to bottom of page
  )
)

## Server Function Definition ----
server <- function(input, output, session) {
  ## Magic Debug Browser button handler
  observeEvent(input$browser, {
    browser()
  })

  ## Task management section ----

  # helper function to add a new task to the task menu
  # TODO this function does jack all, probably due to a dumb mistake in my css selector sequence
  # potentially scan https://shiny.posit.co/r/components/inputs/task-button/ for more info?
  add_task <- function(inputId, value, color, text) {
    insertUI(
      selector = "ul.navbar-right > li.nav-item > div.dropdown-menu-right > div.dropdown-divider", # Specific selector for the task list # TODO may need to update? This is probably wrong and that's my problem
      where = "afterEnd",
      # Wrap the taskItem in tags$li as required by the AdminLTE template structure
      ui = tags$li(
        taskItem(
          inputId = inputId,
          value = value,
          color = color,
          text = text
        )
      )
    )
  }

  # example of new extended task, to be used for report generation later
  # in the ui somewhere:
  # input_task_button("button_fit", "Fit model")

  # in the server somewhere
  # fit <- ExtendedTask$new(function(predictors) {
  #   future_promise({
  #     # lengthy time operation here
  #     formula <-
  #       formula(paste0("log(price) ~ ", paste0(predictors, collapse = " + ")))
  #
  #     # do stuff
  #   })
  # }) |>
  #   bind_task_button("button_fit")
  #
  # observeEvent(input$button_fit, {
  #   fit$invoke(input$predictors)
  # })

  # for the above task stuff, see https://shiny.posit.co/r/articles/improve/nonblocking/

  #browser()

  # Logic to remove the most recently added task item
  # TODO adapt this to remove a named inputId task, ie. after report finishes generating
  # observeEvent(input$remove_task, {
  #   # Check if there are dynamic tasks to remove
  #   if (task_counter() > 1) {
  #     last_id <- paste0("task_", task_counter() - 1)
  #     removeUI(
  #       selector = paste0("#", last_id),
  #       multiple = FALSE
  #     )
  #   }
  # })

  # TODO possibly cannabalize this section to handle dynamically created/named tasks
  # for (i in 1:task_counter()) {
  #   observeEvent(!!sym(paste0("input$task_", task_counter())), {
  #     showModal(
  #       modalDialog(
  #         title = paste0("Task ", i, " Clicked"),
  #         paste0("You clicked task item ", i)
  #       )
  #     )
  #   })
  # }

  ## Handle file upload/default dataset load ----
  df <- reactive({
    # Keeping the full unmodified data frame in df
    req(input$file_upload) # do I still need to req this?

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
      # TODO fix this eventually for my own sanity's sake
      return() # return early and skip default loading stuff for now, only allow file upload for time being

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
    #req(year())
    #req(country())
    #req(df())

    # don't need linear regression checkbox technically, it's state just determines if we plot a geom_smooth/linregression etc and doesn't affect the df
    message("Making happy_data() more happy...")

    ### Mutate the data as needed ----

    # (ensure country names are in agreement between dataset and world map country names - standardized/normalized to best of ability
    # dat |> # TODO mutate me !
    #   rename()
    # TODO fix the discrepancies. United States =/= United States of America, etc

    # starting with the full unmodified dataframe, df
    dat <- df() # so, this should be evaluating the df() reactive, and assigning it to dat, a local function-scope var

    #browser()
    dat <- dat |>
      select(-`...1`) # remove bullshit id/pk/whatever the hell this was, artifact from read_csv TODO research why

    # for now, looking at a subset of the data due to so much incompleteness for years past 2016
    dat <- dat |>
      filter(year %in% c(2015, 2016))

    if (!is.null(country())) {
      # if a country has been selected, add the selected_country col to highlight it later
      dat <- dat |>
        mutate(
          # add a col representing the selected country to highlight
          highlight_country = if_else(Country == country(), "Y", "N") # this dngaf if country() is "" (the default), so no big deal
        )
    }

    # only if the year slider has been changed, filter the dat further by the year slider criteria
    if (year() != 0) {
      dat <- dat |>
        filter(
          # filter down to the selected year
          dat$year == year()
        )
    }

    # we should probably do a default sort on this reactive, sort by year asc, then happiness score desc
    dat <- dat |>
      relocate(year, `Happiness Score`, Country, highlight_country, Region) |> # reorder the cols so they're easier for me to debug
      arrange(year, desc(`Happiness Score`)) # sorts by year, then by happiness score highest to lowest, ie. same as rank, but grouped (not really) by year ASC 2015-2022

    return(dat) # should be the exact same as original df() if no filters have been selected. dat is a local copy of a modified reactive value
  })

  ## Update static elements ----
  # are there even any static elements that need updating? I'm going crazy...

  ## Update dynamic elements  ----

  ### file upload onchange ----
  observeEvent(input$file_upload, {
    message("File Upload initiated...")

    # don't need this req(happy_data()) # bail early if no happy data
    # so, at this point, df() returns the tibble as expected... ok...
    #browser()

    countries <- sort(unique(happy_data()$Country)) # rip out the Country col to populate select options below, unique removes dupes, and alphabetize for making it pretty

    # after a file upload has been initiated, update elements. makes sense.
    message("Updating the select_country element with the list of countries...")
    updateSelectInput(
      session = session,
      inputId = "select_country",
      choices = countries,
      selected = countries[1] # default to first entry in countries (Afghanistan)
    )

    message("Updating slider_year with min/max and set value...")
    updateSliderInput(
      session = session,
      inputId = "slider_year",
      min = min(happy_data()$year, na.rm = T),
      max = max(happy_data()$year, na.rm = T),
      value = min(happy_data()$year, na.rm = T), # default to min year
      step = 1
    )

    # make a vector of the available factors (alphabetized)
    v <- c(
      "Dystopia Residual",
      "Economy (GDP per Capita)",
      "Family",
      "Freedom",
      "Generosity",
      "Health (Life Expectancy)",
      "Trust (Government Corruption)"
    ) # TODO take the names/cols of the happy_data df, use setdiff() to remove blacklisted columns, then populate v with those

    ### Populate the select_factor select with available factors ----
    message("Updating the choices in the select_factor dropdown...")
    updateSelectInput(
      session = session,
      inputId = "select_factor",
      choices = v
    )
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

      if (!(year() %in% c(2015, 2016))) {
        output$status <- renderText({
          "You selected a year with incomplete data. Here be dragons. You've been warned!"
        })
      } else {
        output$status <- renderText({
          ""
        })
      }
    }
  )

  ### select factor onchange
  observeEvent(input$select_factor, {
    message("Handling onchange event for select_factor...")

    req(happy_data()) # bail early if no happy data]
    req(factor()) # also needs factor too

    #freezeReactiveValue(input, "select_factor") # freezeReactiveValue : if vlue is acccesed while frozen, it throws a SILENT excpetion.... wtf

    # Updating scatter plot ----
    message("Updating scatter plot due to select_factor change...")
    output$plot_scatter <- renderPlotly({
      # TODO make more useful tooltips, check https://plotly.com/r/hover-text-and-formatting/

      plot <- happy_data() |>
        ggplot() +
        geom_point(
          mapping = aes(
            x = !!sym(factor()), # black magic voodoo, but it works
            y = `Happiness Score`,
            #fill = highlight_country, # TODO determine if I want fill, color or both for highlight country (and/or alpha below)
            color = highlight_country,
            size = if_else(highlight_country == "Y", 2, 1.3) # these sizes don't make any fucking sense ... what units are these?
            #alpha = if_else(highlight_country == "Y", 0.9, 0.8) # don't think this is needed
          )
        ) +
        guides(
          fill = "none",
          color = "none",
          alpha = "none"
        ) # TODO should I add back in a legend? Fuck no, legends are dumb.

      if (show_linear_regression() == TRUE) {
        plot <- plot +
          suppressMessages(
            # make geom_smooth stfu about what method it chooses
            geom_smooth(
              formula = y ~ x,
              mapping = aes(
                x = !!sym(factor()),
                y = `Happiness Score`
              ),
              method = "loess" # see https://ggplot2.tidyverse.org/reference/geom_smooth.html#arg-method for more info on smoothing methods
            )
          )
      }

      # TODO worth adding ggrepel labels? tbd

      return(plot)
    })

    # update the box title for the scatter plot as well
    updateBox(
      session = session,
      id = "box_plot_scatter",
      action = "update", # TODO figure out wtf is wrong with this. it updates the box title the first time, but then fails to update
      options = list(
        title = paste0(
          "Scatter Plot of: ",
          factor(),
          " ~ Happiness Score"
        )
      )
    )

    # update any additional plots that are based on selected factor

    # update anything else that doesn't depend on the value of select_factor. Should be nothing ...
  })

  ### Update DataTable ----
  output$DT_filtered <- renderDT({
    message("Updating DataTable with filtered dataframe info...")

    req(year())
    req(country())
    req(happy_data())

    #browser()
    # pass through all countries, with the selected_country col, and order by happiness score DESC

    # This doesn't fucking work...
    # Warning: Error in mutate: ℹ In argument: `Country = forcats::fct_reorder(...)`.
    # Caused by error in `lvls_reorder()`:
    #   ! `idx` must contain one integer for each level of `f`

    # a little messy, but it'll work for now

    return(
      happy_data() |>
        mutate(
          Country = forcats::fct_reorder(
            Country,
            `Happiness Score`,
            .desc = TRUE, # desc order
            .na_rm = TRUE # remove na data
          )
        ) # this should sort the df by happiness score DESC
    )

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
    req(year())
    req(happy_data())

    # first, find the idx of the country()
    idx <- which(happy_data()$Country == country())

    # Specify the number of rows before and after
    n_rows <- 15 # 15 before and after should return at maximum 31 rows, min 16 rows

    # Calculate the start and end indices
    # pmax ensures the start index is at least 1
    # pmin ensures the end index does not exceed the total number of rows
    start_idx <- pmax(1, idx - n_rows)
    end_idx <- pmin(nrow(happy_data()), idx + n_rows)

    # TODO could do better logic so that not only does it pull n_rows before and after, but ensure that the total number of rows is 30 let's say, so that we don't have a idx=1, and a ranking of only the selected country and the 15 rows after

    #browser() # use this breakpoint to investigate the index values from above
    happy_data() |>
      slice(start_idx:end_idx) |> # Select the rows using slice()
      ggplot(
        aes(
          x = `Happiness Score`,
          y = Country,
          fill = highlight_country # TODO Fix this ASAP, bullshit
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
