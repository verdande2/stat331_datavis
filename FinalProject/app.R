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
library(ggplot2)
library(plotly)
library(dplyr)
library(bs4Dash)
library(DT)
library(readr)
library(scales)
library(waiter)
library(htmltools)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(tinytex)
library(stringr)
library(bslib)
library(sf)

theme_set(theme_bw()) # default to b/w theme for ggplot2


## Global Variables ----
#default_data_path <- "world_happiness_report.csv"

## ui definition (dashboardPage) ----
ui <- dashboardPage(
  title = "World Happiness bs4Dash Page", # TODO where the hell is this title actually output? browser title?

  ## Header ----
  header = dashboardHeader(
    title = "World Happiness Dashboard",
    titleWidth = "400px", # TODO figure out how this relates to sidebar width
    status = "purple",
    fixed = TRUE,
    leftUi = tagList(
      dropdownMenu(
        icon = icon("tasks"),
        type = "tasks",
        badgeStatus = "primary",
        taskItem(
          # TODO see https://shiny.posit.co/r/components/display-messages/progress-bar/ for more info on how to update the progress on this taskItem, and this SO answer for a weird file read/write workaround ... https://stackoverflow.com/a/79118567
          # for progress bar async, try https://www.rdocumentation.org/packages/ipc/versions/0.1.4/topics/AsyncProgress
          inputId = "report_progress",
          value = 50,
          color = "orange",
          "Report Generating..."
        )
      ),
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

    ## Tabs ----
    tabItems(
      ### Main Tab ----
      tabItem(
        tabName = "main",
        fluidRow(
          ### Scatter Plot ----
          box(
            width = 6,
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
          ),
          box(
            width = 6,
            id = "summary_stats",
            title = "Summary Stats",
            fluidRow(
              valueBoxOutput(
                outputId = "value_worldwide_mean_happiness_score",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_worldwide_median_happiness_score",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_variance_worldwide_happiness_score",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_std_dev_worldwide_happiness_score",
                width = 3
              )
            ),
            fluidRow(
              valueBoxOutput(
                outputId = "value_selected_country_happiness_score",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_selected_country_happiness_rank",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_lowest_worldwide_happiness_score",
                width = 3
              ),
              valueBoxOutput(
                outputId = "value_highest_worldwide_happiness_score",
                width = 3
              )
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
              height = "600px"
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
            title = paste0("Data Table of filtered Happiness Data"),
            width = 12,
            collapsible = TRUE,
            DTOutput(
              outputId = "DT_filtered",
              width = "100%",
              height = "auto" # TODO figure out how to adjust overflow settings
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
    }
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

  ## Wrangle and Mangle dataset ----

  happy_data <- reactive({
    # happy_data will represent the filtered/mutated subset that will be used to plot

    #req(year())
    #req(country())
    #req(df())

    message("Making happy_data() more happy...")

    # starting with the full unmodified dataframe, df
    dat <- df()

    ### Mutate the data as needed ----

    # a dict map of old (dataset) country names to new (world map data) country names
    country_name_map <- c(
      "Bosnia and Herzegovina" = "Bosnia and Herz.",
      "Central African Republic" = "Central African Rep.",
      "Congo (Brazzaville)" = "Dem. Rep. Congo",
      "Congo (Kinshasa)" = "Dem. Rep. Congo",
      "Czech Republic" = "Czechia",
      "Dominican Republic" = "Dominican Rep.",
      "Ivory Coast" = "Côte d'Ivoire",
      "Macedonia" = "North Macedonia",
      "North Cyprus" = "N. Cyprus",
      "Palestinian Territories" = "Palestine",
      "Somaliland region" = "Somaliland",
      "Swaziland" = "eSwatini",
      "United States" = "United States of America"
    )

    # (ensure country names are in agreement between dataset and world map country names - standardized/normalized to best of ability
    dat <- dat |> # fix the discrepancies in the country name United States =/= United States of America, etc
      mutate(
        Country = str_replace_all(string = Country, pattern = country_name_map)
      )


    dat <- dat |>
      select(-`...1`) # remove bullshit id/pk/whatever the hell this col was, artifact from read_csv TODO research why

    # for now, looking at a subset of the data due to so much incompleteness for years past 2016
    dat <- dat |>
      filter(year %in% c(2015, 2016)) # can remove this filter, but the lack of data for years past 2016 is ... unsatisfactory

    if (!is.null(country())) {
      # if a country has been selected, add the highlight_country col to highlight it later
      dat <- dat |>
        mutate(
          # add a col representing the selected country to highlight
          highlight_country = if_else(Country == country(), "Y", "N")
        )
    }

    # we should probably do a default sort on this reactive, sort by year asc, then happiness score desc
    dat <- dat |>
      relocate(year, `Happiness Score`, Country, highlight_country, Region) |> # reorder the cols so they're easier for me to debug
      arrange(year, desc(`Happiness Score`)) # sorts by year, then by happiness score highest to lowest, ie. same as rank, but grouped (not really) by year ASC 2015-2022

    # only if the year slider has been changed, filter the dat frame further by the selected year
    if (year() != 0) {
      dat <- dat |>
        filter(
          # filter down to the selected year
          dat$year == year()
        )
    }

    # I considered adding a fct_reorder() call to resort the df by happiness score, but realized that I'll need that reorder for the world ranking and the scatter plot, bt I'll need to fct_reorder by country alphabetically for the select_country input, etc
    return(dat) # should be the exact same as original df() if no filters have been selected.
  })

  ## Update dynamic elements  ----

  ### Update various output variables ----
  output$value_worldwide_mean_happiness_score <- renderValueBox({
    req(happy_data())

    mean <- mean(happy_data()$`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0("\\( \\mu \\) = ", round(mean, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Worldwide Mean Happiness Score",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_worldwide_median_happiness_score <- renderValueBox({
    req(happy_data())

    median <- median(happy_data()$`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0("\\( M \\) = ", round(median, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Worldwide Median Happiness Score",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_variance_worldwide_happiness_score <- renderValueBox({
    req(happy_data())

    var <- var(happy_data()$`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0("\\( \\sigma^2 \\) = ", round(var, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Variance of Worldwide Happiness Scores",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_std_dev_worldwide_happiness_score <- renderValueBox({
    req(happy_data())

    sd <- sd(happy_data()$`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0("\\( \\sigma \\) = ", round(sd, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Std Dev of Worldwide Happiness Scores",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_selected_country_happiness_score <- renderValueBox({
    req(country())
    req(happy_data())

    score <- happy_data() |>
      filter(Country == country()) |>
      pull(`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0("\\( H \\) = ", round(score, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = paste0(country(), "'s Happiness Score"),
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_selected_country_happiness_rank <- renderValueBox({
    req(country())
    req(happy_data())

    rank <- happy_data() |>
      filter(Country == country()) |>
      pull(`Happiness Rank`)

    v <- renderUI(
      paste0("Rank: ", rank)
    )

    valueBox(
      value = v,
      subtitle = paste0(country(), "'s Happiness Rank"),
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })
  output$value_lowest_worldwide_happiness_score <- renderValueBox({
    req(happy_data())
    req(country())

    low_score <- happy_data() |>
      mutate(
        Country = forcats::fct_reorder(
          Country,
          `Happiness Score`,
          .na_rm = TRUE # remove na data
        )
      ) |>
      head(1) |>
      pull(`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0(country(), " has the lowest score at ", round(low_score, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Lowest Worldwide Happiness Score",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

  output$value_highest_worldwide_happiness_score <- renderValueBox({
    req(happy_data())
    req(country())

    high_score <- happy_data() |>
      mutate(
        Country = forcats::fct_reorder(
          Country,
          desc(`Happiness Score`),
          .na_rm = TRUE # remove na data
        )
      ) |>
      head(1) |>
      pull(`Happiness Score`)

    v <- renderUI(
      withMathJax(
        paste0(country(), " has the highest score at ", round(high_score, 6))
      )
    )

    valueBox(
      value = v,
      subtitle = "Highest Worldwide Happiness Score",
      icon = icon("bug"),
      color = "success", # green
      width = 3,
      footer = "footer",
      gradient = TRUE,
      elevation = 5
    )
  })

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
          "Scatter Plot of: '",
          factor(),
          " ~ Happiness Score' for all recorded countries in ",
          year()
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

    dat <- happy_data() |>
      mutate(
        Country = forcats::fct_reorder(
          Country,
          `Happiness Score`,
          .na_rm = TRUE # remove na data
        )
      ) # this should sort the df by happiness score DESC

    return(dat)
  })

  ### Update world map plot ----
  output$plot_map <- renderPlotly({
    req(country())
    message("Updating World Map Plot...")

    world <- ne_countries(scale = "medium", returnclass = "sf") # static/const vector of country shape data, to be later joined with country happiness data



    #browser()

    dataset_countries <- sort(unique(happy_data()$Country))
    world_data_countries <- sort(unique(world$name))

    #a <- setdiff(dataset_countries, world_data_countries) # most important one here, as this returns things in our dataset that are not in our world (sf geo) df

    #b <- setdiff(world_data_countries, dataset_countries)

    #browser()



    # TODO something in here is throwing a "Warning: Error in <Anonymous>: number of columns of matrices must match (see arg 15)"
    #browser()
    #dat <- left_join(happy_data(), world, by = c("Country" = "name")) # TODO THIS LOC IS FROM SATAN HIMSELF FUCK THIS CODE
    dat <- left_join(world, happy_data(), by = c("name" = "Country"))


    # # the above join should take each record in happy_data(), match it's Country to the name in world, and join the shape/spatial data, then store back in dat to use later

    # # dat is the filtered happy_data bolted onto the world data
    # dat |>
    #   ggplot() + # TODO issues with below geom_sf, complaining about geometry, tried passing geometry=geometry and it hated that too
    #   geom_sf(aes(fill = `Happiness Score`, geometry = geometry)) + #, alpha = highlight_country)) + # at this point, happy data should have the highlight_country col created
    #   scale_fill_gradient(low = "white", high = "blue") +
    #   guides(alpha = "none")

#browser()
    plot <- dat |>
      ggplot() +
      #geom_sf(mapping = aes(fill = ASDFASDFWDSFSDFSDF), color = "black", fill = "lightgreen") +
      geom_sf(
        aes(
          geometry = geometry,
          fill = highlight_country
          ),
        color = "black",
        fill = "lightgreen"
        ) + # TODO figure this line out. fill based on highlight country but default to lightgreen
      geom_sf(
        aes( # trying to add a second geomsf layer to change fill on the highlighted country, this is working so far
          geometry = geometry,
          fill = highlight_country
        ),
        color = "black"
      ) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))


    return(plot)
  })

  ### Update World Ranking plot ----
  output$plot_ranking <- renderPlotly({
    # don't try to render plotly if filters haven't been selected yet
    req(country())
    req(year())
    req(happy_data())

    message("Updating world ranking list...")

    #browser() # to verify that the happy_data() coming in is already sorted and has highlight_country set => happy_data() is arranged, but not sorted by factor Happiness Score ... must use forcats::fct_reorder() and make it work

    # right off the bat, let's sort this df by factor `Happiness Score` DESC
    sorted_happy_data <- happy_data() |>
      mutate(
        `Country` = forcats::fct_reorder(
          `Country`,
          `Happiness Score`,
          .desc = TRUE, # desc order # TODO WTF why won't you stick?
          .na_rm = TRUE # remove na data
        ),
        `Rank` = row_number(desc(`Happiness Score`)), # TODO find a way to have the y axis labeled not only by the country name, but by its rank
        `Rank Country` = paste0(
          str_pad(
            paste0("Rank ", `Rank`),
            10,
            pad = " ",
            side = "right"
          ),
          " ",
          str_pad(Country, 25, pad = " ", side = "left")
        )
      )

    ### Making the world ranking have 25 rows in it ----
    # first, find the idx of the selected country()
    idx <- which(sorted_happy_data$Country == country())

    # Specify the number of rows before and after
    n_rows <- 12 # 12 before and after should return at maximum 25 rows, min 13 rows which we'll expand later

    # max rows to display in the ranking plot
    max_rows <- 25

    # Calculate the start and end indices
    # pmax ensures the start index is at least 1
    # pmin ensures the end index does not exceed the total number of rows
    start_idx <- pmax(1, idx - n_rows)
    end_idx <- pmin(nrow(sorted_happy_data), idx + n_rows)

    # look at the initial slicing and see how it fares compared to our max_rows arg
    sliced_happy_data <- sorted_happy_data |>
      slice(start_idx:end_idx)

    current_nrows <- nrow(sliced_happy_data)

    # debug sanity check
    message(
      paste0(
        "Before adjustment: start_idx: ",
        start_idx,
        " | idx: ",
        idx,
        " | end_idx: ",
        end_idx,
        " | Total Rows: ",
        current_nrows
      )
    )

    # handle the case where we don't have enough rows selected to hit our max_rows
    if (current_nrows < max_rows) {
      # need to add more rows to hit our max_rows goal

      if (start_idx < n_rows + 1) {
        # idx < 1 + n_rows
        # we're at the start of the list, so we have start_idx<n_rows+1

        message(
          paste0(
            "At beginning of ranked list => Adding ",
            (max_rows - current_nrows),
            " to end_idx: ",
            end_idx
          )
        )

        # solution: increase end_idx so that the difference between max_rows and current_nrows is 0
        end_idx <- end_idx + (max_rows - current_nrows)
      } else if (end_idx >= current_nrows - (n_rows + 1)) {
        # the end_idx is the final chunk in the df

        message(
          paste0(
            "At end of ranked list => Subtracting ",
            (max_rows - current_nrows),
            " from start_idx: ",
            start_idx
          )
        )

        # solution, decrease the start_idx until we have our desired max_rows
        start_idx <- start_idx - (max_rows - current_nrows)
      } else {
        # both start and end idx are away from the beginning/end of the df, so let the indexes pass through normally

        message("No adjustment to indices needed!")
      }

      # reslice the data with the newly adjusted indices
      sliced_happy_data <- sorted_happy_data |>
        slice(start_idx:end_idx)

      # not really needed, but recalculating the current num of rows in sliced_happy_data(), to output later
      current_nrows <- nrow(sliced_happy_data)

      # debug sanity check
      message(
        paste0(
          "After adjustment: start_idx: ",
          start_idx,
          " | idx: ",
          idx,
          " | end_idx: ",
          end_idx,
          " | Total Rows: ",
          current_nrows
        )
      )
    } else {
      message("We already have our max_rows in the dataframe, moving on...")
    }

    #browser()
    # TODO there has to be a better way to select from a df where a column value matches, and also return the previous n rows and next n rows, so that the selected country is in the middle of the ranked listing, while still taking into consideration the min and max indices for the set of rows

    # TODO add title, subtitle, etc to plot below:
    # TODO consider adding country happiness rank to left of the country label on y axis, or perhaps (more advanced) look into a way to show the first x highest rows, a ... style skip, the selected country, another ... style skip and then the last x lowest rows, to get a better visualization where the chosen country fits in
    # TODO sliced_happy_data is already sorted by happiness score desc... why the fuck is it plotting in ascending...
    sliced_happy_data |> # pass the sliced dataset
      mutate(
        # let's see if this works... WTF WHY DOES THIS NOT WORK KASDFF LKjsdf lkasdj flksdjf lksdf jlksdf jlaksdf jksd
        `Country` = forcats::fct_reorder(
          `Country`,
          `Happiness Score`,
          .desc = TRUE, # desc order
          .na_rm = TRUE # remove na data
        )
      ) |>
      # arrange(desc(`Happiness Score`)) |> # doesn't give a fuck if I reordered factors before, or arrange them now, still showing in reverse order...
      ggplot(
        aes(
          x = `Happiness Score`,
          y = `Rank Country`,
          fill = highlight_country
        )
      ) +
      geom_col(
        #fill = "#0073C2"
      ) +
      geom_text(
        aes(label = `Happiness Score`),
        hjust = 0,
        vjust = 0,
        color = "black",
        size = 5
      ) +
      scale_x_continuous(
        labels = comma_format()
      ) +
      theme_minimal(base_family = "Consolas") +
      theme(
        legend.position = "none"
      ) +
      #   plot.title = element_text(family = "mono"), # Monospace title
      #   axis.title.x = element_text(family = "mono"), # Monospace x-axis title
      #   axis.title.y = element_text(family = "mono"), # Monospace x-axis title
      #   axis.text.x = element_text(family = "mono"), # Monospace y-axis labels
      #   axis.text.y = element_text(family = "mono") # Monospace y-axis labels
      # ) +
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
  # observe({
  #   if (input$help_switch == TRUE) {
  #     if (input$sidebar == "main") {
  #       shiny::showModal(
  #         ui = shiny::modalDialog(
  #           title = "Help",
  #           "Help Contents Here" # TODO Doc me
  #         )
  #       )
  #     } else if (input$sidebar == "report") {
  #       shiny::showModal(
  #         ui = shiny::modalDialog(
  #           title = "Help: Reports",
  #           "Help info for report settings" # TODO Doc me
  #         )
  #       )
  #     }
  #   }
  # })
}


options(shiny.reactlog = TRUE) # this seems to do jack all
### Shiny App Function call ----
shinyApp(
  ui,
  server
  #options = list(display.mode = "showcase")
)
