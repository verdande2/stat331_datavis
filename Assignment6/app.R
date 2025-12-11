# This assignment relies on the covid data and the "broken" code presented in the assignments section.
# We will be working on this in class.  Your task is to use bs4dash to make an attractive dashboard application
# out of the shell created in the code.  Upload your app.R code.
library(shiny)
library(plotly)
library(bslib)
library(DT)
library(tidyverse)
library(readr)
library(scales)
library(usmap)
library(fmsb)
library(bs4Dash)
library(shinydashboard)

# Pre app data and list prep ----
map <- us_map()

totals_vec <- c("state.name", "total_cases", "Total Deaths", "Total # Tests")
seven_vec <- c(
  "state.name",
  "Cases in Last 7 Days",
  "Deaths in Last 7 Days",
  "Total # Tests Last 7 Days",
  "case_rank"
)
per_100000_vec <- c(
  "state.name",
  "cases_per_100000",
  "Death Rate per 100000",
  "7-Day Cases Rate per 100000",
  "# Tests per 100K",
  "case_rank"
)


# UI ----------------
# Default Layout ------
ui <- dashboardPage(
  dashboardHeader(
    title = "COVID Report"
  ),
  ## Sidebar Card -------------
  dashboardSidebar(
    fluidRow(
      card(
        # TODO look into why these overflow into other divs badly, need to set overflow property?
        card_header("Settings"),
        fileInput(
          inputId = "upload",
          label = "Select Data File",
          multiple = FALSE
        ),
        selectInput(
          inputId = "myState",
          label = "Choose State",
          choices = ""
        ),
        downloadButton(
          outputId = "report",
          label = "Generate Report"
        )
      )
    )
  ),
  dashboardBody(
    style = "padding: 15px 15px !important; margin: 15px 15px !important;",
    ## Sidebar Card -------------
    fluidRow(
      tabBox(
        title = "State Highlights", # TODO need to make this spaced out more and prettier, make it extend to 100% width
        id = "highlights",
        tabPanel(
          "Per 100,000",
          fluidRow(
            infoBox(
              "Cases Per 100,000",
              textOutput(outputId = "p100Kcases")
            ),
            infoBox(
              "Tests Per 100,000",
              textOutput(outputId = "p100Ktests")
            )
          ),
          fluidRow(
            infoBox(
              "Death Rate Per 100,000",
              textOutput(outputId = "p100Kdeaths")
            ),
            infoBox(
              "Tests Per 100,000 - Last 7 Days",
              textOutput(outputId = "p100Ktests7")
            )
          )
        ),
        tabPanel(
          "Totals",
          fluidRow(
            infoBox(
              "Total Cases",
              textOutput(outputId = "totalCases")
            ),
            infoBox(
              "Total % Positive Tests",
              textOutput(outputId = "totalPPlus")
            )
          ),
          fluidRow(
            infoBox(
              "Total Deaths",
              textOutput(outputId = "totalDeaths")
            ),
            infoBox(
              "Total Tests",
              textOutput(outputId = "totalTests")
            )
          )
        ),
        tabPanel(
          "7 day",
          fluidRow(
            infoBox(
              "Cases in the last 7 Days",
              textOutput(outputId = "cases7")
            ),
            infoBox(
              "Deaths in the last 7 Days",
              textOutput(outputId = "deaths7")
            )
          ),
          fluidRow(
            infoBox(
              "Percentage of Positive Tests in Last 7 Days",
              textOutput(outputId = "PPos7")
            ),
            infoBox(
              "Total Tests in Last 7 Days",
              textOutput(outputId = "tests7")
            )
          )
        ),
        tabPanel(
          "30 day",
          fluidRow(
            infoBox(
              "Tests per 100,000 in last 30 Days",
              textOutput(outputId = "test100K30")
            ),
            infoBox(
              "Percentage of Positive Tests in Last 30 Days",
              textOutput(outputId = "PPos30")
            )
          ),
          fluidRow(
            infoBox(
              "Total Tests in Last 30 Days",
              textOutput(outputId = "tests30")
            )
          )
        )
      )
    ),
    fluidRow(
      ## Map Card --------------------
      box(
        title = "Map",
        plotlyOutput(
          outputId = "mapPlotly",
          width = "100%",
          height = "400px"
        )
      ),
      ## Radar Plot Card --------------
      box(
        title = "Multi-aspect Comparison per 100k",
        plotOutput(outputId = "plotPer100000")
      )
    ),
    fluidRow(
      ## Data Table Card --------------
      box(
        title = "Data Table",
        DTOutput(outputId = "myTable", width = "100%") # TODO figure out how to adjust overflow settings so it doesn't overflow and barf text all over
      ),
      ## plotly Card -------------
      box(
        title = "Barplot of Metrics",
        selectInput(
          inputId = "rankInputSelect",
          label = "Select Metric",
          choices = ""
        ),
        plotlyOutput(outputId = "distPlot", width = "100%")
      )
    )
  )
)


# Server ----------------
server <- function(input, output, server) {
  # Read Data from File based on user choice ----
  df <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  ## Update data ----
  COVID_data <- reactive({
    data <- df() %>%
      rename(
        total_cases = "Total Cases",
        cases_per_100000 = "Case Rate per 100000",
        state.name = "State/Territory"
      ) %>%
      mutate(case_rank = rank(-cases_per_100000, ties.method = "min"))

    data <- left_join(map, data, by = c("full" = "state.name")) %>%
      rename(state.name = full)

    return(data)
  })

  # Prepare dropdown menu values ----
  rank_vec <- reactive({
    setdiff(
      names(COVID_data()),
      c(
        "fips",
        "abbr",
        "state.name",
        "geom",
        "Confirmed Cases",
        "Probable Cases",
        "Confirmed Deaths",
        "Probable Deaths",
        "case_rank"
      )
    )
  })

  # Apply Dynamic UI elements  ----
  observeEvent(input$upload, {
    updateSelectInput(inputId = "rankInputSelect", choices = rank_vec())
    updateSelectInput(
      inputId = "myState",
      choices = unique(COVID_data()$state.name)
    )
  })

  # Generate Report Button Event ----
  observeEvent(input$report, {
    rmarkdown::render(
      input = "AutomatedReport.Rmd",
      output_file = paste0(input$myState, ".html"),
      params = list(state = input$myState)
    )
  })

  output$report <- downloadHandler(
    filename = function() {
      paste0(input$myState, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "AutomatedReport.Rmd")
      file.copy("AutomatedReport.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        state = input$myState,
        cases = COVID_data(),
        filter = input$rankInputSelect
      )
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params, # Warning: Error in knit_params_get: render params not declared in YAML: cases, filter TODO WTF Tried putting them in yaml of the report.Rmd
        envir = new.env(parent = globalenv())
      )
    }
  )

  # Output Value Boxes ----

  ## Per 100000 Value Boxes ----------------
  output$p100Kcases <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$cases_per_100000
  })

  output$p100Kdeaths <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Death Rate per 100000`
  })

  output$p100Ktests <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`# Tests per 100K`
  })

  output$p100Ktests7 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`# Tests per 100K Last 7 Days`
  })

  ## Totals Value Boxes ----------------
  output$totalCases <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$total_cases
  })

  output$totalDeaths <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Total Deaths`
  })

  output$totalPPlus <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Total % Positive`
  })

  output$totalTests <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Total # Tests`
  })

  ## 7 Value Boxes ----------------
  output$cases7 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Cases in Last 7 Days`
  })

  output$deaths7 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Deaths in Last 7 Days`
  })

  output$PPos7 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`% Positive Last 7 Days`
  })

  output$tests7 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Total # Tests Last 7 Days`
  })

  ## 30 Value Boxes ----------------
  output$test100K30 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`# Tests per 100K Last 30 Days`
  })

  output$PPos30 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`% Positive Last 30 Days`
  })

  output$tests30 <- renderText({
    r <- COVID_data() %>%
      filter(state.name == input$myState)
    r$`Total # Tests Last 30 Days`
  })

  #  Output Table -----------
  output$myTable <- renderDT({
    COVID_data()
  })

  #  Output Distribution Plot -----------
  output$distPlot <- renderPlotly({
    selection <- input$rankInputSelect
    COVID_data() %>%
      mutate(
        highlight_state = if_else(state.name == input$myState, "Y", "N")
      ) %>%
      mutate(state.name = fct_reorder(state.name, get(selection))) %>%
      ggplot(aes(
        x = get(selection),
        y = state.name,
        fill = highlight_state
      )) +
      geom_col() +
      scale_x_continuous(labels = comma_format()) +
      theme(legend.position = "none") +
      labs(
        y = NULL,
        x = selection
      )
  })

  #  Output Map Plot -----------
  output$mapPlotly <- renderPlotly({
    COVID_data() %>%
      mutate(
        highlight_state = if_else(state.name == input$myState, "Y", "N")
      ) %>%
      mutate(state.name = fct_reorder(state.name, cases_per_100000)) %>%
      ggplot() +
      geom_sf(aes(fill = cases_per_100000, alpha = highlight_state)) +
      scale_fill_gradient(low = "white", high = "blue") +
      guides(alpha = "none")
  })

  #  Output Radar Plot -----------
  output$plotPer100000 <- renderPlot({
    pcol <- c("blue", "red")
    pfcol <- alpha(pcol, 0.4)

    COVID_data() %>%
      mutate(case_rank = rank(cases_per_100000, ties.method = "min")) %>%
      bind_rows(
        COVID_data() %>%
          summarise(across(where(is.numeric), mean, na.rm = TRUE))
      ) %>%
      mutate(state.name = replace_na(state.name, "Mean")) %>%
      bind_rows(
        COVID_data() %>% summarise(across(where(is.numeric), min, na.rm = TRUE))
      ) %>%
      mutate(state.name = replace_na(state.name, "Min")) %>%
      bind_rows(
        COVID_data() %>% summarise(across(where(is.numeric), max, na.rm = TRUE))
      ) %>%
      mutate(state.name = replace_na(state.name, "Max")) %>%
      arrange(match(state.name, c("Max", "Min"))) %>%
      select(all_of(per_100000_vec)) %>%
      data.frame() %>%
      select(-c(geom)) %>%
      filter(state.name %in% c(input$myState, "Mean", "Min", "Max")) %>%
      column_to_rownames("state.name") %>%
      radarchart(
        centerzero = TRUE,
        title = "Comparison of State with the Average",
        pcol = pcol,
        pfcol = pfcol
      )
    legend(
      x = 1,
      y = 1,
      legend = c(input$myState, "Mean"),
      pch = 20,
      col = pcol
    )
  })
}

# Call Shiny App ------
shinyApp(ui, server)
